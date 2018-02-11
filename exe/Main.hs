{-# language NamedFieldPuns #-}

module Main (main) where

import Data.Foldable ( asum )
import Data.Function ( (&) )
import System.Environment ( getArgs )

import Distribution.Package.Dhall

import qualified Data.Text.Lazy.IO as LazyText
import qualified Data.Text.Prettyprint.Doc as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.Text as Pretty
import qualified Dhall
import qualified Distribution.PackageDescription.PrettyPrint as Cabal
import qualified Options.Applicative as OptParse
import qualified System.IO



data Command
  = RunDhallToCabal DhallToCabalOptions
  | PrintType



data DhallToCabalOptions = DhallToCabalOptions
  { dhallFilePath :: String }



dhallToCabalOptionsParser :: OptParse.Parser DhallToCabalOptions
dhallToCabalOptionsParser =
  DhallToCabalOptions
    <$> OptParse.argument
          OptParse.str ( OptParse.metavar "<dhall input file>" )



printTypeParser :: OptParse.Parser ()
printTypeParser =
  OptParse.flag' () ( OptParse.long "print-type" )



runDhallToCabal :: DhallToCabalOptions -> IO ()
runDhallToCabal DhallToCabalOptions { dhallFilePath } =
  dhallFileToCabal dhallFilePath
    & fmap Cabal.showGenericPackageDescription
    >>= putStrLn



main :: IO ()
main = do
  command <-
    OptParse.execParser opts

  case command of
    RunDhallToCabal options ->
      runDhallToCabal options

    PrintType ->
      printType

  where

  parser =
    asum
      [ RunDhallToCabal <$> dhallToCabalOptionsParser
      , PrintType <$ printTypeParser
      ]

  opts =
    OptParse.info parser mempty



-- Shamelessly taken from dhall-format

opts :: Pretty.LayoutOptions
opts =
  Pretty.defaultLayoutOptions
    { Pretty.layoutPageWidth = Pretty.AvailablePerLine 80 1.0 }



printType :: IO ()
printType = do
  Pretty.renderIO
    System.IO.stdout
    ( Pretty.layoutSmart opts
        ( Pretty.pretty ( Dhall.expected genericPackageDescription ) )
    )

  putStrLn ""
