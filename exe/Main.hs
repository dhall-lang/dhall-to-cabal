{-# language GeneralizedNewtypeDeriving #-}
{-# language NoMonomorphismRestriction #-}
{-# language NamedFieldPuns #-}
{-# language OverloadedStrings #-}

module Main ( main ) where

import Control.Applicative ( (<**>), (<|>), Const(..), optional )
import Data.Char ( isAlphaNum )
import Data.Foldable ( asum, foldl' )
import Data.Functor.Product ( Product(..) )
import Data.Functor.Identity ( Identity(..) )
import Data.Monoid ( Any(..), (<>) )
import Data.Function ( (&) )
import Data.Maybe ( fromMaybe )
import Data.Text.Lazy (Text)
import System.Environment ( getArgs )
import Data.String ( fromString )

import DhallToCabal

import qualified Data.Text.Lazy.IO as LazyText
import qualified Data.Text.Prettyprint.Doc as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.Text as Pretty
import qualified Data.Text.Prettyprint.Doc.Symbols.Unicode as Pretty
import qualified Dhall
import qualified Dhall.Context
import qualified Dhall.Core as Dhall
import qualified Dhall.Core as Expr ( Expr(..), Var(..), shift )
import qualified Distribution.PackageDescription as Cabal
import qualified Distribution.PackageDescription.PrettyPrint as Cabal
import qualified Distribution.Types.GenericPackageDescription as Cabal
import qualified Distribution.Types.PackageId as Cabal
import qualified Distribution.Types.PackageName as Cabal
import qualified Options.Applicative as OptParse
import qualified System.IO



data Command
  = RunDhallToCabal DhallToCabalOptions
  | PrintType KnownType



data KnownType
  = Library
  | ForeignLibrary
  | Executable
  | Benchmark
  | TestSuite
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
  | Package
  | VersionRange
  | Version
  deriving (Bounded, Enum, Eq, Ord, Read, Show)



data DhallToCabalOptions = DhallToCabalOptions
  { dhallFilePath :: Maybe String
  , explain :: Bool
  }



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



printTypeParser :: OptParse.Parser KnownType
printTypeParser =
  OptParse.option OptParse.auto modifiers

  where

    modifiers =
      mconcat
        [ OptParse.long "print-type"
        , OptParse.help "Print out the description of a type. For a full description, try --print-type Package"
        , OptParse.metavar "TYPE"
        ]



runDhallToCabal :: DhallToCabalOptions -> IO ()
runDhallToCabal DhallToCabalOptions { dhallFilePath, explain } = do
  source <-
    case dhallFilePath of
      Nothing ->
        LazyText.getContents

      Just filePath ->
        LazyText.readFile filePath

  let
    fileName = fromMaybe "(STDIN)" dhallFilePath

  explaining
    ( dhallToCabal fileName source
        & fmap ( \ pkgDesc ->
                       pkgDesc
                     & Cabal.showGenericPackageDescription
                     & addWarningHeader ( pkgDescName pkgDesc )
               )
        >>= putStrLn
    )

  where

    explaining =
      if explain then Dhall.detailed else id

    pkgDescName = Cabal.unPackageName . Cabal.pkgName . Cabal.package . Cabal.packageDescription

    -- Make sure that the displayed source path is copy-pasteable to
    -- the command line, and won't break the generated Cabal
    -- file. This is a somewhat conservative check.
    isAcceptableCommentChar c =
      isAlphaNum c || c == '.' || c == '/' || c == '_' || c == '-'

    regenerationInstructions name =
      case dhallFilePath of
        Just filePath | all isAcceptableCommentChar filePath ->
          [ "-- Instead, edit the source Dhall file, namely"
          , "-- '" ++ filePath ++ "', and re-generate this file by running"
          , "-- 'dhall-to-cabal -- " ++ filePath ++ " > " ++ name ++ ".cabal'."
          ]
        _ ->
          [ "-- Instead, edit the source Dhall file (which may have the"
          , "-- '.dhall' extension) and re-run dhall-to-cabal, passing the"
          , "-- source file's name as its argument and redirecting output to"
          , "-- '" ++ name ++ ".cabal' (this file)."
          ]

    -- Starting with Cabal 2.2, the cabal-version field *has* to be
    -- the first thing in the .cabal file. So split that off and plonk
    -- the warning header after it.
    addWarningHeader name str =
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
            , regenerationInstructions name
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

    PrintType t ->
      printType t

  where

  parser =
    asum
      [ RunDhallToCabal <$> dhallToCabalOptionsParser
      , PrintType <$> printTypeParser
      ]

  opts =
    OptParse.info ( parser <**> OptParse.helper ) modifiers

  modifiers =
    mconcat
      [ OptParse.progDesc "Generate Cabal files from Dhall expressions"
      ]



-- Shamelessly taken from dhall-format

opts :: Pretty.LayoutOptions
opts =
  Pretty.defaultLayoutOptions
    { Pretty.layoutPageWidth = Pretty.AvailablePerLine 80 1.0 }



printType :: KnownType -> IO ()
printType t = do
  Pretty.renderIO
    System.IO.stdout
    ( Pretty.layoutSmart opts
        ( Pretty.pretty factoredType )
    )

  putStrLn ""

  where

    dhallType t =
      case t of
        Config -> configRecordType
        Library -> Dhall.expected library
        ForeignLibrary -> Dhall.expected foreignLib
        Executable -> Dhall.expected executable
        Benchmark -> Dhall.expected benchmark
        TestSuite -> Dhall.expected testSuite
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

    letDhallType t =
      liftCSE ( fromString ( show t ) ) ( dhallType t )

    factoredType =
      foldl'
        ( flip letDhallType )
        ( dhallType t )
        [ minBound .. maxBound ]


liftCSE
  :: (Eq s, Eq a)
  => Text          -- ^ The name of the binding
  -> Expr.Expr s a -- ^ The common subexpression to lift
  -> Expr.Expr s a -- ^ The expression to remove a common subexpression from
  -> Expr.Expr s a
liftCSE name body expr =
  let
    v0 =
      Expr.V name 0

  in
    case go ( Expr.shift 1 v0 expr ) v0 of
      Pair ( Const ( Any False ) ) _ ->
        -- There was nothing to lift
        expr

      Pair _ ( Identity reduced ) ->
        -- We did manage to lift a CSE, so let bind it
        Expr.Let name Nothing body reduced

  where

    shiftName n v | n == name =
      shiftVar 1 v

    shiftName _ v =
        v

    shiftVar delta ( Expr.V name' n ) =
      Expr.V name' ( n + delta )

    go e v | e == body =
      Pair ( Const ( Any True ) ) ( Identity ( Expr.Var v ) )

    go e v =
      case e of
        Expr.Lam n t b ->
          Expr.Lam n t <$> go b ( shiftName n v )

        Expr.Pi n t b ->
          Expr.Pi n <$> go t v <*> go b ( shiftName n v )

        Expr.App f a ->
          Expr.App <$> go f v <*> go a v

        Expr.Let n t b e ->
          Expr.Let n t <$> go b v <*> go e ( shiftName n v )

        Expr.Annot a b ->
          Expr.Annot <$> go a v <*> go b v

        Expr.BoolAnd a b ->
          Expr.BoolAnd <$> go a v <*> go b v

        Expr.BoolOr a b ->
          Expr.BoolOr <$> go a v <*> go b v

        Expr.BoolEQ a b ->
          Expr.BoolEQ <$> go a v <*> go b v

        Expr.BoolNE a b ->
          Expr.BoolNE <$> go a v <*> go b v

        Expr.BoolIf a b c ->
          Expr.BoolIf <$> go a v <*> go b v <*> go c v

        Expr.NaturalPlus a b ->
          Expr.NaturalPlus <$> go a v <*> go b v

        Expr.NaturalTimes a b ->
          Expr.NaturalTimes <$> go a v <*> go b v

        Expr.ListAppend a b ->
          Expr.ListAppend <$> go a v <*> go b v

        Expr.Combine a b ->
          Expr.Combine <$> go a v <*> go b v

        Expr.Prefer a b ->
          Expr.Prefer <$> go a v <*> go b v

        Expr.TextAppend a b ->
          Expr.TextAppend <$> go a v <*> go b v

        Expr.ListLit t elems ->
          Expr.ListLit
            <$> ( traverse ( `go` v ) t )
            <*> ( traverse ( `go` v ) elems )

        Expr.OptionalLit t elems ->
          Expr.OptionalLit
            <$> go t v
            <*> ( traverse ( `go` v ) elems )

        Expr.Record fields ->
          Expr.Record <$> traverse ( `go` v ) fields

        Expr.RecordLit fields ->
          Expr.RecordLit <$> traverse ( `go` v ) fields

        Expr.Union fields ->
          Expr.Union <$> traverse ( `go` v ) fields

        Expr.UnionLit n a fields ->
          Expr.UnionLit n <$> go a v <*> traverse ( `go` v ) fields

        Expr.Merge a b t ->
          Expr.Merge <$> go a v <*> go b v <*> traverse ( `go` v ) t

        Expr.Constructors e ->
          Expr.Constructors <$> go e v

        Expr.Field e f ->
          Expr.Field <$> go e v <*> pure f

        Expr.Note s e ->
          Expr.Note s <$> go e v

        Expr.Embed{} ->
          pure e

        Expr.Const{} ->
          pure e

        Expr.Var{} ->
          pure e

        Expr.Bool{} ->
          pure e

        Expr.BoolLit{} ->
          pure e

        Expr.Natural{} ->
          pure e

        Expr.NaturalLit{} ->
          pure e

        Expr.NaturalFold{} ->
          pure e

        Expr.NaturalBuild{} ->
          pure e

        Expr.NaturalIsZero{} ->
          pure e

        Expr.NaturalEven{} ->
          pure e

        Expr.NaturalOdd{} ->
          pure e

        Expr.NaturalToInteger{} ->
          pure e

        Expr.NaturalShow{} ->
          pure e

        Expr.Integer{} ->
          pure e

        Expr.IntegerShow{} ->
          pure e

        Expr.IntegerLit{} ->
          pure e

        Expr.Double{} ->
          pure e

        Expr.DoubleShow{} ->
          pure e

        Expr.DoubleLit{} ->
          pure e

        Expr.Text{} ->
          pure e

        Expr.TextLit{} ->
          pure e

        Expr.List ->
          pure e

        Expr.ListBuild ->
          pure e

        Expr.ListFold ->
          pure e

        Expr.ListLength ->
          pure e

        Expr.ListHead ->
          pure e

        Expr.ListLast ->
          pure e

        Expr.ListIndexed ->
          pure e

        Expr.ListReverse ->
          pure e

        Expr.Optional ->
          pure e

        Expr.OptionalFold ->
          pure e

        Expr.OptionalBuild ->
          pure e
