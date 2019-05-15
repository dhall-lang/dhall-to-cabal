{-# language LambdaCase #-}
{-# language NoMonomorphismRestriction #-}
{-# language NamedFieldPuns #-}
{-# language OverloadedStrings #-}
{-# language RecordWildCards #-}
{-# language ViewPatterns #-}

module Main ( main ) where

import Control.Applicative ( (<**>), optional, (<|>) )
import Control.Monad ( guard )
import Data.Char ( isAlphaNum )
import Control.Monad.Trans.State ( State, execState, modify )
import Data.Foldable ( asum, toList, traverse_ )
import Data.Function ( (&) )
import Data.List.NonEmpty ( NonEmpty(..) )
import Data.Maybe ( fromMaybe )
import Data.Text (Text)
import Data.String ( fromString )
import Data.Version ( showVersion )
import Lens.Micro ( set )
import System.Directory ( createDirectoryIfMissing )
import System.FilePath ( (</>), (<.>), addTrailingPathSeparator, normalise, takeDirectory )

import CabalToDhall ( KnownDefault, getDefault, resolvePreludeVar )
import DhallLocation ( preludeLocation, typesLocation, dhallFromGitHub )
import DhallToCabal
import DhallToCabal.Util ( relativeTo )
import qualified Paths_dhall_to_cabal as Paths

import qualified Data.Text.IO as StrictText
import qualified Data.Text.Prettyprint.Doc as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.Text as Pretty
import qualified Dhall
import qualified Dhall.Core as Dhall
import qualified Dhall.Core as Expr ( Expr(..), Var(..), Binding(..), shift )
import qualified Dhall.Lint as Lint
import qualified Dhall.Map as Map
import qualified Dhall.Parser
import qualified Dhall.TypeCheck as Dhall
import qualified Distribution.PackageDescription as Cabal
import qualified Distribution.PackageDescription.PrettyPrint as Cabal
import qualified Distribution.Types.PackageId as Cabal
import qualified Distribution.Types.PackageName as Cabal
import qualified Options.Applicative as OptParse
import qualified System.IO


data Command
  = RunDhallToCabal DhallToCabalOptions
  | PrintType PrintTypeOptions
  | PrintDefault PrintDefaultOptions
  | PrintVersion


-- Note: this needs to be in topological order of CSEability, from
-- big to small.
data KnownType
  = Package
  | Library
  | ForeignLibrary
  | Benchmark
  | Executable
  | TestSuite
  | BuildInfo
  | Config
  | SourceRepo
  | RepoType
  | RepoKind
  | Compiler
  | OS
  | Extension
  | CompilerOptions
  | Arch
  | Language
  | License
  | BuildType
  | VersionRange
  | Version
  | SPDX
  | LicenseId
  | LicenseExceptionId
  | Scope
  | ModuleRenaming
  | ForeignLibOption
  | ForeignLibType
  deriving (Bounded, Enum, Eq, Ord, Read, Show)


-- | A 'Benchmark' is a proper subrecord of an 'Executable', but we
-- don't want to write 'Executable' in terms of 'Benchmark'! Hence,
-- limit which types we will do the common-field extraction for to
-- only 'BuildInfo', for the time being.
isCandidateSubrecord :: KnownType -> Bool
isCandidateSubrecord BuildInfo = True
isCandidateSubrecord _ = False


shouldBeImported :: KnownType -> Bool
shouldBeImported Extension = True
shouldBeImported LicenseId = True
shouldBeImported LicenseExceptionId = True
shouldBeImported _ = False



data DhallToCabalOptions = DhallToCabalOptions
  { dhallFilePath :: Maybe String
  , explain :: Bool
  , outputDirection :: OutputDirection
  }


data OutputDirection
  = OutputStdout
  | OutputDir RelativeDir


data RelativeDir
  = RelativeToCWD FilePath
  | RelativeToInput FilePath


dhallToCabalOptionsParser :: OptParse.Parser DhallToCabalOptions
dhallToCabalOptionsParser =
  DhallToCabalOptions
    <$>
      optional
        ( OptParse.argument
            OptParse.str
            ( mconcat
                [ OptParse.metavar "<dhall input file>"
                , OptParse.help "The Dhall expression to convert to a Cabal file"
                ]
            )
        )
    <*>
      OptParse.flag
        False
        True
        ( mconcat
            [ OptParse.long "explain"
            , OptParse.help "Provide explanations to type Dhall syntax and type errors."
            ]
        )
    <*>
      outputDirectionParser
  where
  -- Note order: the --output-dir-input does not fail because it has a default value.
  outputDirectionParser =
        OptParse.flag'
          OutputStdout
          ( mconcat
            [ OptParse.long "output-stdout"
            , OptParse.help "Write the .cabal file to standard output."
            ]
          )

    <|> OutputDir . RelativeToCWD <$> OptParse.strOption
          ( mconcat
              [ OptParse.long "output-dir-cwd"
              , OptParse.metavar "DIR"
              , OptParse.help "Write the resulting .cabal file to DIR/NAME.cabal, where NAME is taken from the .cabal file's 'name' field, and DIR is relative to the current working directory."
              ]
          )

    <|> OutputDir . RelativeToInput <$> OptParse.strOption
          ( mconcat
              [ OptParse.long "output-dir-input"
              , OptParse.metavar "DIR"
              , OptParse.help "Write the resulting .cabal file to DIR/NAME.cabal, where NAME is taken from the .cabal file's 'name' field, and DIR is relative to the directory containing the input Dhall file (or the current working directory if the input is from standard input)."
              , OptParse.value "."
              ]
          )


data PrintTypeOptions = PrintTypeOptions
  { typeToPrint :: KnownType
  , selfContained :: Bool
  }


printTypeOptionsParser :: OptParse.Parser PrintTypeOptions
printTypeOptionsParser =
  PrintTypeOptions
    <$>
      OptParse.option OptParse.auto modifiers
    <*>
      OptParse.flag
        False
        True
        ( mconcat
            [ OptParse.long "self-contained"
            , OptParse.help "Emit self-contained types. Without this flag, some large enumerations are referenced by import."
            ]
        )

  where

    modifiers =
      mconcat
        [ OptParse.long "print-type"
        , OptParse.help "Print out the description of a type. For a full description, try --print-type Package"
        , OptParse.metavar "TYPE"
        ]



printVersionParser :: OptParse.Parser ()
printVersionParser =
  OptParse.flag'
    ()
    ( mconcat
      [ OptParse.long "version"
      , OptParse.help "Display dhall-to-cabal's version and exit."
      ]
    )


data PrintDefaultOptions =
  PrintDefaultOptions
    { defaultToPrint :: KnownDefault }


printDefaultOptionsParser :: OptParse.Parser PrintDefaultOptions
printDefaultOptionsParser =
  PrintDefaultOptions
    <$>
      OptParse.option OptParse.auto
        ( mconcat
            [ OptParse.long "print-default"
            , OptParse.help "Print out the default values for a type, as found in prelude.defaults"
            , OptParse.metavar "TYPE"
            ]
        )


runDhallToCabal :: DhallToCabalOptions -> IO ()
runDhallToCabal DhallToCabalOptions { dhallFilePath, explain, outputDirection } = do
  source <-
    case dhallFilePath of
      Nothing ->
        StrictText.getContents

      Just filePath ->
        StrictText.readFile filePath

  let
    settings = Dhall.defaultInputSettings
      & set Dhall.rootDirectory inputDir
      & set Dhall.sourceName ( fromMaybe "(STDIN)" dhallFilePath )

  pkgDesc <- explaining ( dhallToCabal settings source )
  let
    rendered =
      addWarningHeader
        ( Cabal.showGenericPackageDescription pkgDesc )

  case outputDirection of

    OutputStdout ->
      putStrLn rendered

    OutputDir _ -> do
      createDirectoryIfMissing True destDir
      let dest = destDir </> pkgDescName pkgDesc <.> "cabal"
      putStrLn ( "Writing to " ++ normalise dest ++ "." )
      writeFile dest rendered

  where
    inputDir = maybe "." takeDirectory dhallFilePath

    destDir =
      case outputDirection of
        OutputStdout ->
          "."
        OutputDir (RelativeToCWD dir) ->
          dir
        OutputDir (RelativeToInput relativeDir) ->
          inputDir </> relativeDir

    explaining =
      if explain then Dhall.detailed else id

    pkgDescName = Cabal.unPackageName . Cabal.pkgName . Cabal.package . Cabal.packageDescription

    -- Make sure that the displayed source path is copy-pasteable to
    -- the command line, and won't break the generated Cabal
    -- file. This is a somewhat conservative check.
    isAcceptableCommentChar c =
      isAlphaNum c || c == '.' || c == '/' || c == '_' || c == '-'

    -- These regeneration instructions assume that (a) if we are
    -- writing to stdout, the user is redirecting it a file in the
    -- current directory, (b) the generated file is not moved after we
    -- produce it, (c) there are no symlinks breaking .. components,
    -- and (d) they will run the regeneration instructions from the
    -- directory of the generated .cabal file.
    --
    -- These assumptions are necessarily approximations, but they're a
    -- reasonable guess at the common case and at least give the user
    -- something to go on.
    regenerationInstructions =
      case dhallFilePath of
        Just filePath | all isAcceptableCommentChar filePath ->
          [ "-- Instead, edit the source Dhall file, namely"
          , "-- '" ++ filePath ++ "', and re-generate this file by running"
          , "-- 'dhall-to-cabal -- " ++ sourcePath ++ "'."
          ]
          where
            sourcePath =
              relativeTo ( addTrailingPathSeparator destDir ) filePath
        _ ->
          [ "-- Instead, edit the source Dhall file (which may have the"
          , "-- '.dhall' extension) and re-run dhall-to-cabal, passing "
          , "-- the source file's name as its argument."
          ]

    -- Starting with Cabal 2.2, the cabal-version field *has* to be
    -- the first thing in the .cabal file. So split that off and plonk
    -- the warning header after it.
    addWarningHeader str =
      case lines str of
        [] ->
          error "addWarningHeader: no cabal-version line found?"
        cabalVersion : rest ->
          unlines $ concat
            [ [ cabalVersion
              , "-- * * * * * * * * * * * * WARNING * * * * * * * * * * * *"
              , "-- This file has been AUTO-GENERATED by dhall-to-cabal."
              , "--"
              , "-- Do not edit it by hand, because your changes will be over-written!"
              , "--"
              ]
            , regenerationInstructions
            , [ "-- * * * * * * * * * * * * WARNING * * * * * * * * * * * *" ]
            , rest
            ]


main :: IO ()
main = do
  command <-
    OptParse.execParser opts

  case command of
    RunDhallToCabal options ->
      runDhallToCabal options

    PrintType options ->
      printType options

    PrintDefault options ->
      printDefault options

    PrintVersion ->
      printVersion

  where

  parser =
    asum
      [ RunDhallToCabal <$> dhallToCabalOptionsParser
      , PrintType <$> printTypeOptionsParser
      , PrintDefault <$> printDefaultOptionsParser
      , PrintVersion <$ printVersionParser
      ]

  opts =
    OptParse.info ( parser <**> OptParse.helper ) modifiers

  modifiers =
    mconcat
      [ OptParse.progDesc "Generate Cabal files from Dhall expressions"
      , OptParse.footer
          ( "The default behaviour is to generate a .cabal file in the "
         ++ "directory of the input file with a name corresponding to the "
         ++ "'name' field of the generated package description (i.e., the "
         ++ "same behaviour as specifying '--output-dir-input .'). In the "
         ++ "case of reading from standard input, the .cabal file will be "
         ++ "placed in the current working directory."
          )
      ]



-- Shamelessly taken from dhall-format

opts :: Pretty.LayoutOptions
opts =
  Pretty.defaultLayoutOptions
    { Pretty.layoutPageWidth = Pretty.AvailablePerLine 80 1.0 }


-- Note: the expression is linted afterwards, so this can do the
-- drop-dead simple thing of adding a new `let` each time.
addBinding :: Text -> Dhall.Expr s a -> Dhall.Expr s a -> Dhall.Expr s a
addBinding name val =
  Expr.Let
    ( Expr.Binding name Nothing val :| [] )


printType :: PrintTypeOptions -> IO ()
printType PrintTypeOptions { .. } = do
  Pretty.renderIO
    System.IO.stdout
    ( Pretty.layoutSmart opts
        ( Pretty.pretty factoredType )
    )

  putStrLn ""

  where

    dhallType :: KnownType -> Dhall.Expr Dhall.Parser.Src a
    dhallType t = fmap Dhall.absurd
      ( case t of
          Config -> configRecordType
          Library -> Dhall.expected library
          ForeignLibrary -> Dhall.expected foreignLib
          Executable -> Dhall.expected executable
          Benchmark -> Dhall.expected benchmark
          TestSuite -> Dhall.expected testSuite
          BuildInfo -> buildInfoType
          SourceRepo -> Dhall.expected sourceRepo
          RepoType -> Dhall.expected repoType
          RepoKind -> Dhall.expected repoKind
          Compiler -> Dhall.expected compilerFlavor
          OS -> Dhall.expected operatingSystem
          Extension -> Dhall.expected extension
          CompilerOptions -> Dhall.expected compilerOptions
          Arch -> Dhall.expected arch
          Language -> Dhall.expected language
          License -> Dhall.expected license
          BuildType -> Dhall.expected buildType
          Package -> Dhall.expected genericPackageDescription
          VersionRange -> Dhall.expected versionRange
          Version -> Dhall.expected version
          SPDX -> Dhall.expected spdxLicense
          LicenseId -> Dhall.expected spdxLicenseId
          LicenseExceptionId -> Dhall.expected spdxLicenseExceptionId
          Scope -> Dhall.expected executableScope
          ModuleRenaming -> Dhall.expected moduleRenaming
          ForeignLibOption -> Dhall.expected foreignLibOption
          ForeignLibType -> Dhall.expected foreignLibType
      )

    makeLetOrImport t val reduced =
      let
        name = fromString ( show t )
      in if shouldBeImported t && not selfContained
         then Dhall.subst ( Expr.V name 0 ) ( Expr.Var ( Expr.V "types" 0 ) `Expr.Field` name ) reduced
         else addBinding name val reduced

    factoredType :: Expr.Expr Dhall.Parser.Src Dhall.Import
    factoredType =
      let
        body =
          execState
            ( traverse_ step [ minBound .. maxBound ] )
            ( dhallType typeToPrint )

        importing =
          addBinding
            "types"
            ( Expr.Embed ( typesLocation dhallFromGitHub ) )

      in
        Lint.lint ( importing body )

    step
      :: (Eq a)
      => KnownType
         -- ^ Name of the type we're trying to factor out
      -> State ( Expr.Expr Dhall.Parser.Src a ) ()
    step factorType =
      modify ( tryCSE factorType )

    tryCSE
      :: (Eq a)
      => KnownType -- ^ The type we're factoring out
      -> Expr.Expr Dhall.Parser.Src a
      -> Expr.Expr Dhall.Parser.Src a
    tryCSE factorType expr =
      let
        name = fromString ( show factorType )
        subrecord = isCandidateSubrecord factorType
      in
        makeLetOrImport
          factorType
          ( dhallType factorType )
          ( liftCSE subrecord name ( dhallType factorType ) expr )


-- | This will always shift the variable name provided, even if
-- nothing is lifted out.
--
-- No variables should be free in the expression to lift.
liftCSE
  :: (Eq s, Eq a)
  => Bool
     -- ^ Should we attempt to find the subexpression as a sub-record?
  -> Text
     -- ^ The name of the binding
  -> Expr.Expr s a
     -- ^ The common subexpression to lift
  -> Expr.Expr s a
     -- ^ The expression to remove a common subexpression from
  -> Expr.Expr s a
liftCSE subrecord name body expr =
  let
    v0 =
      Expr.V name 0

  in
    go True v0 ( Expr.shift 1 v0 expr )

  where

    shiftName n v | n == name =
      shiftVar 1 v

    shiftName _ v =
        v

    shiftVar delta ( Expr.V name' n ) =
      Expr.V name' ( n + delta )

    subtractRecordFields a b = do
      guard subrecord

      Expr.Record left <-
        return a

      Expr.Record right <-
        return b

      let
        intersection =
          Map.intersectionWith (==) left right

      -- The right record cannot have any fields not in left.
      guard ( null ( Map.difference right left ) )

      -- We must have at least one field with a common name
      guard ( not ( null intersection ) )

      -- All common fields must have identical types
      guard ( and intersection )

      let
        extra =
          Map.difference left right

      guard ( not ( null extra ) )

      return ( Expr.Record extra )

    -- Only lift it out if it's not the whole expression.
    go False v e | e == body =
      Expr.Var v

    go _ v ( ( `subtractRecordFields` body ) -> Just extra ) =
      Expr.CombineTypes ( Expr.Var v ) extra

    go toplevel v e =
      case e of
        Expr.Lam n t b ->
          Expr.Lam n t ( go False ( shiftName n v ) b )

        Expr.Pi n t b ->
          Expr.Pi n ( go False v t ) ( go False ( shiftName n v ) b )

        Expr.App f a ->
          Expr.App ( go False v f ) ( go False v a )

        Expr.Let bs e ->
          go' ( toList bs ) v
            where
              -- Since we lint afterwards, it's fine to transform one
              -- let with many bindings into many lets with one
              -- binding each.
              go' ( Expr.Binding n t b : bs ) v' =
                Expr.Let
                  ( pure
                    ( Expr.Binding n t
                      ( go False v' b )
                    )
                  )
                  ( go' bs ( shiftName n v' ) )
              go' [] v' =
                go toplevel v' e

        Expr.Annot a b ->
          Expr.Annot ( go False v a ) ( go False v b )

        Expr.BoolAnd a b ->
          Expr.BoolAnd ( go False v a ) ( go False v b )

        Expr.BoolOr a b ->
          Expr.BoolOr ( go False v a ) ( go False v b )

        Expr.BoolEQ a b ->
          Expr.BoolEQ ( go False v a ) ( go False v b )

        Expr.BoolNE a b ->
          Expr.BoolNE ( go False v a ) ( go False v b )

        Expr.BoolIf a b c ->
          Expr.BoolIf ( go False v a ) ( go False v b ) ( go False v c )

        Expr.NaturalPlus a b ->
          Expr.NaturalPlus ( go False v a ) ( go False v b )

        Expr.NaturalTimes a b ->
          Expr.NaturalTimes ( go False v a ) ( go False v b )

        Expr.ListAppend a b ->
          Expr.ListAppend ( go False v a ) ( go False v b )

        Expr.Combine a b ->
          Expr.Combine ( go False v a ) ( go False v b )

        Expr.Prefer a b ->
          Expr.Prefer ( go False v a ) ( go False v b )

        Expr.TextAppend a b ->
          Expr.TextAppend ( go False v a ) ( go False v b )

        Expr.ListLit t elems ->
          Expr.ListLit
            ( fmap ( go False v ) t )
            ( fmap ( go False v ) elems )

        Expr.OptionalLit t elems ->
          Expr.OptionalLit
            ( go False v t )
            ( fmap ( go False v ) elems )

        Expr.Some a ->
          Expr.Some ( go False v a )

        Expr.None ->
          Expr.None

        Expr.Record fields ->
          Expr.Record ( fmap ( go False v ) fields )

        Expr.RecordLit fields ->
          Expr.RecordLit ( fmap ( go False v ) fields )

        Expr.Union fields ->
          Expr.Union ( fmap ( fmap ( go False v ) ) fields )

        Expr.UnionLit n a fields ->
          Expr.UnionLit n ( go False v a ) ( fmap ( fmap ( go False v ) ) fields )

        Expr.Merge a b t ->
          Expr.Merge ( go False v a ) ( go False v b ) ( fmap ( go False v ) t )

        Expr.Field e f ->
          Expr.Field ( go False v e ) f

        Expr.Note s e ->
          Expr.Note s ( go False v e )

        Expr.CombineTypes a b ->
          Expr.CombineTypes ( go False v a ) ( go False v b )

        Expr.Project e fs ->
          Expr.Project ( go False v e ) fs

        Expr.ImportAlt l r ->
          Expr.ImportAlt ( go False v l ) ( go False v r )

        Expr.IntegerToDouble ->
          Expr.IntegerToDouble

        Expr.Embed{} ->
          e

        Expr.Const{} ->
          e

        Expr.Var{} ->
          e

        Expr.Bool{} ->
          e

        Expr.BoolLit{} ->
          e

        Expr.Natural{} ->
          e

        Expr.NaturalLit{} ->
          e

        Expr.NaturalFold{} ->
          e

        Expr.NaturalBuild{} ->
          e

        Expr.NaturalIsZero{} ->
          e

        Expr.NaturalEven{} ->
          e

        Expr.NaturalOdd{} ->
          e

        Expr.NaturalToInteger{} ->
          e

        Expr.NaturalShow{} ->
          e

        Expr.Integer{} ->
          e

        Expr.IntegerShow{} ->
          e

        Expr.IntegerLit{} ->
          e

        Expr.Double{} ->
          e

        Expr.DoubleShow{} ->
          e

        Expr.DoubleLit{} ->
          e

        Expr.Text{} ->
          e

        Expr.TextLit{} ->
          e

        Expr.TextShow ->
          e

        Expr.List ->
          e

        Expr.ListBuild ->
          e

        Expr.ListFold ->
          e

        Expr.ListLength ->
          e

        Expr.ListHead ->
          e

        Expr.ListLast ->
          e

        Expr.ListIndexed ->
          e

        Expr.ListReverse ->
          e

        Expr.Optional ->
          e

        Expr.OptionalFold ->
          e

        Expr.OptionalBuild ->
          e


printVersion :: IO ()
printVersion = do
  putStrLn ( "dhall-to-cabal version " ++ showVersion Paths.version )


printDefault :: PrintDefaultOptions -> IO ()
printDefault PrintDefaultOptions {..} = do
  Pretty.renderIO
    System.IO.stdout
    ( Pretty.layoutSmart opts
        ( Pretty.pretty ( Lint.lint ( withPreludeImport expr ) ) )
    )

  putStrLn ""

  where
    withPreludeImport =
      addBinding
        "prelude"
        ( Expr.Embed ( preludeLocation dhallFromGitHub ) )

    expr :: Expr.Expr Dhall.Parser.Src Dhall.Import
    expr = getDefault
             ( typesLocation dhallFromGitHub )
             resolvePreludeVar defaultToPrint
