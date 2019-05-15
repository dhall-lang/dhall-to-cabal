{-# language LambdaCase #-}
{-# language NoMonomorphismRestriction #-}
{-# language NamedFieldPuns #-}
{-# language OverloadedStrings #-}
{-# language RecordWildCards #-}
{-# language ViewPatterns #-}

module Main ( main ) where

import Control.Applicative ( (<**>), optional, (<|>) )
import Control.Monad ( guard, join )
import Data.Char ( isAlphaNum )
import Data.Foldable ( asum, foldl', toList )
import Data.Function ( (&) )
import Data.List.NonEmpty ( NonEmpty(..) )
import Data.Maybe ( fromMaybe )
import Data.Text (Text)
import Data.String ( fromString )
import Data.Version ( showVersion )
import Lens.Micro ( ASetter, over, set )
import System.Directory ( createDirectoryIfMissing )
import System.FilePath ( (</>), (<.>), addTrailingPathSeparator, normalise, takeDirectory )

import CabalToDhall ( KnownDefault, getDefault, resolvePreludeVar )
import DhallLocation ( preludeLocation, typesLocation, dhallFromGitHub )
import DhallToCabal
import DhallToCabal.Util ( relativeTo )
import qualified Paths_dhall_to_cabal as Paths

import qualified Data.Map as UnorderedMap
import qualified Data.Text.IO as StrictText
import qualified Data.Text.Prettyprint.Doc as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.Text as Pretty
import qualified Dhall
import qualified Dhall.Core as Dhall
import qualified Dhall.Core as Expr ( Expr(..), Var(..), Binding(..) )
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


factored :: KnownType -> Expr.Expr Dhall.Parser.Src KnownType
factored rootType =
  foldl' step ( dhallType rootType ) [ minBound .. maxBound ]
  where
    step expr factorType =
      fmap
        ( fromMaybe factorType )
        ( cse
          ( isCandidateSubrecord factorType )
          ( dhallType factorType )
          expr
        )


printType :: PrintTypeOptions -> IO ()
printType PrintTypeOptions { .. } = do
  Pretty.renderIO
    System.IO.stdout
    ( Pretty.layoutSmart opts
        ( Pretty.pretty ( Lint.lint result ) )
    )

  putStrLn ""

  where

    linkedType =
      join . mapWithBindings linkType . factored

    linkType var t =
      let
        name = fromString ( show t )
      in if shouldBeImported t && not selfContained
         then Expr.Var ( var "types" ) `Expr.Field` name
         else Expr.Var ( var name )

    bindTypes expr =
      foldl' bindType expr [ minBound .. maxBound ]

    bindType expr t =
      addBinding
        ( fromString ( show t ) )
        ( linkedType t )
        expr

    bindImport =
      addBinding
        "types"
        ( Expr.Embed ( typesLocation dhallFromGitHub ) )

    -- Unconditionally add everything, since lint will remove the
    -- redundant bindings.
    result =
      bindImport
        ( bindTypes ( linkedType typeToPrint ) )


transformOf :: ASetter a b a b -> (b -> b) -> a -> b
transformOf l f = go where go = f . over l go


-- | No variables should be free in the expression to lift.
cse
  :: ( Eq s, Eq a )
  => Bool
     -- ^ Should we attempt to find the subexpression as a sub-record?
  -> Expr.Expr s a
     -- ^ The common subexpression to lift.
  -> Expr.Expr s a
     -- ^ The expression to remove a common subexpression from.
  -> Expr.Expr s ( Maybe a )
     -- ^ 'Nothing' if it's representing a lifted subexpression.
cse subrecord ( fmap Just -> body ) ( fmap Just -> expr ) =
  case transformOf Dhall.subExpressions go expr of
    -- Don't lift the whole thing out - it's not a win.
    Expr.Embed Nothing ->
      body
    expr' ->
      expr'

  where

    go e | e == body =
      Expr.Embed Nothing

    go e | subrecord, Just extra <- subtractRecordFields e body =
      Expr.CombineTypes ( Expr.Embed Nothing ) extra

    go e =
      e


subtractRecordFields
  :: ( Eq s, Eq a )
  => Expr.Expr s a
  -> Expr.Expr s a
  -> Maybe ( Expr.Expr s a )
subtractRecordFields a b = do

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


chunkExprs
  :: ( Applicative f )
  => ( Expr.Expr s a -> f ( Expr.Expr t b ) )
  -> Dhall.Chunks s a -> f ( Dhall.Chunks t b )
chunkExprs f ( Dhall.Chunks chunks final ) =
  flip Dhall.Chunks final <$> traverse ( traverse f ) chunks


-- | The return value of this should be linted.
mapWithBindings
  :: ( ( Text -> Expr.Var ) -> a -> b )
  -> Expr.Expr s a
  -> Expr.Expr s b
mapWithBindings f =
  go UnorderedMap.empty

  where

    outermostVar bindings n =
      Expr.V n ( fromMaybe 0 ( UnorderedMap.lookup n bindings ) )

    shiftName =
      UnorderedMap.alter ( Just . succ . fromMaybe 0 )

    go bindings = \case
      Expr.Lam n t b ->
        Expr.Lam n
          ( go bindings t )
          ( go ( shiftName n bindings ) b )

      Expr.Pi n t b ->
        Expr.Pi n ( go bindings t ) ( go ( shiftName n bindings ) b )

      Expr.App f a ->
        Expr.App ( go bindings f ) ( go bindings a )

      Expr.Let bs e ->
        go' ( toList bs ) bindings
          where
            -- Since we lint afterwards, it's fine to transform one
            -- let with many bindings into many lets with one
            -- binding each.
            go' ( Expr.Binding n t b : bs ) bindings' =
              Expr.Let
                ( pure
                  ( Expr.Binding n
                    ( fmap ( go bindings' ) t )
                    ( go bindings' b )
                  )
                )
                ( go' bs ( shiftName n bindings' ) )
            go' [] bindings' =
              go bindings' e

      Expr.Annot a b ->
        Expr.Annot ( go bindings a ) ( go bindings b )

      Expr.BoolAnd a b ->
        Expr.BoolAnd ( go bindings a ) ( go bindings b )

      Expr.BoolOr a b ->
        Expr.BoolOr ( go bindings a ) ( go bindings b )

      Expr.BoolEQ a b ->
        Expr.BoolEQ ( go bindings a ) ( go bindings b )

      Expr.BoolNE a b ->
        Expr.BoolNE ( go bindings a ) ( go bindings b )

      Expr.BoolIf a b c ->
        Expr.BoolIf ( go bindings a ) ( go bindings b ) ( go bindings c )

      Expr.NaturalPlus a b ->
        Expr.NaturalPlus ( go bindings a ) ( go bindings b )

      Expr.NaturalTimes a b ->
        Expr.NaturalTimes ( go bindings a ) ( go bindings b )

      Expr.ListAppend a b ->
        Expr.ListAppend ( go bindings a ) ( go bindings b )

      Expr.Combine a b ->
        Expr.Combine ( go bindings a ) ( go bindings b )

      Expr.Prefer a b ->
        Expr.Prefer ( go bindings a ) ( go bindings b )

      Expr.TextAppend a b ->
        Expr.TextAppend ( go bindings a ) ( go bindings b )

      Expr.ListLit t elems ->
        Expr.ListLit
          ( fmap ( go bindings ) t )
          ( fmap ( go bindings ) elems )

      Expr.OptionalLit t elems ->
        Expr.OptionalLit
          ( go bindings t )
          ( fmap ( go bindings ) elems )

      Expr.Some a ->
        Expr.Some ( go bindings a )

      Expr.None ->
        Expr.None

      Expr.Record fields ->
        Expr.Record ( fmap ( go bindings ) fields )

      Expr.RecordLit fields ->
        Expr.RecordLit ( fmap ( go bindings ) fields )

      Expr.Union fields ->
        Expr.Union ( fmap ( fmap ( go bindings ) ) fields )

      Expr.UnionLit n a fields ->
        Expr.UnionLit n ( go bindings a ) ( fmap ( fmap ( go bindings ) ) fields )

      Expr.Merge a b t ->
        Expr.Merge ( go bindings a ) ( go bindings b ) ( fmap ( go bindings ) t )

      Expr.Field e f ->
        Expr.Field ( go bindings e ) f

      Expr.Note s e ->
        Expr.Note s ( go bindings e )

      Expr.CombineTypes a b ->
        Expr.CombineTypes ( go bindings a ) ( go bindings b )

      Expr.Project e fs ->
        Expr.Project ( go bindings e ) fs

      Expr.ImportAlt l r ->
        Expr.ImportAlt ( go bindings l ) ( go bindings r )

      Expr.IntegerToDouble ->
        Expr.IntegerToDouble

      Expr.Embed a ->
        Expr.Embed
          ( f ( outermostVar bindings ) a )

      Expr.Const c ->
        Expr.Const c

      Expr.Var v ->
        Expr.Var v

      Expr.Bool ->
        Expr.Bool

      Expr.BoolLit b ->
        Expr.BoolLit b

      Expr.Natural ->
        Expr.Natural

      Expr.NaturalLit n ->
        Expr.NaturalLit n

      Expr.NaturalFold ->
        Expr.NaturalFold

      Expr.NaturalBuild ->
        Expr.NaturalBuild

      Expr.NaturalIsZero ->
        Expr.NaturalIsZero

      Expr.NaturalEven ->
        Expr.NaturalEven

      Expr.NaturalOdd ->
        Expr.NaturalOdd

      Expr.NaturalToInteger ->
        Expr.NaturalToInteger

      Expr.NaturalShow ->
        Expr.NaturalShow

      Expr.Integer ->
        Expr.Integer

      Expr.IntegerShow ->
        Expr.IntegerShow

      Expr.IntegerLit n ->
        Expr.IntegerLit n

      Expr.Double ->
        Expr.Double

      Expr.DoubleShow ->
        Expr.DoubleShow

      Expr.DoubleLit n ->
        Expr.DoubleLit n

      Expr.Text ->
        Expr.Text

      Expr.TextLit t ->
        Expr.TextLit ( over chunkExprs ( go bindings ) t )

      Expr.TextShow ->
        Expr.TextShow

      Expr.List ->
        Expr.List

      Expr.ListBuild ->
        Expr.ListBuild

      Expr.ListFold ->
        Expr.ListFold

      Expr.ListLength ->
        Expr.ListLength

      Expr.ListHead ->
        Expr.ListHead

      Expr.ListLast ->
        Expr.ListLast

      Expr.ListIndexed ->
        Expr.ListIndexed

      Expr.ListReverse ->
        Expr.ListReverse

      Expr.Optional ->
        Expr.Optional

      Expr.OptionalFold ->
        Expr.OptionalFold

      Expr.OptionalBuild ->
        Expr.OptionalBuild


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
