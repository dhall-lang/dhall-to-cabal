{-# language NamedFieldPuns #-}
{-# language OverloadedStrings #-}

module Main ( main ) where

import Control.Applicative ( (<**>), optional )
import Data.Foldable ( asum )
import Data.Version ( showVersion )

import qualified Data.ByteString as ByteString
import qualified Data.Text.Prettyprint.Doc as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.Text as Pretty
import qualified Options.Applicative as OptParse
import qualified System.IO

import CabalToDhall ( cabalToDhall, parseGenericPackageDescriptionThrows )
import DhallLocation ( dhallFromGitHub )
import Paths_dhall_to_cabal ( version )


data Command
  = RunCabalToDhall CabalToDhallOptions
  | PrintVersion


data CabalToDhallOptions = CabalToDhallOptions
  { cabalFilePath :: Maybe String
  }


cabalToDhallOptionsParser :: OptParse.Parser CabalToDhallOptions
cabalToDhallOptionsParser =
  CabalToDhallOptions
    <$>
      optional
        ( OptParse.argument
            OptParse.str
            ( mconcat
                [ OptParse.metavar "<cabal input file>"
                , OptParse.help "The Cabal file to convert to Dhall"
                ]
            )
        )


printVersionParser :: OptParse.Parser ()
printVersionParser =
  OptParse.flag'
    ()
    ( mconcat
      [ OptParse.long "version"
      , OptParse.help "Display dhall-to-cabal's version and exit."
      ]
    )


optionsParser = OptParse.info ( parser <**> OptParse.helper ) mempty
  where
    parser =
      asum
        [ RunCabalToDhall <$> cabalToDhallOptionsParser
        , PrintVersion <$ printVersionParser
        ]


main :: IO ()
main = do
  command <-
    OptParse.execParser optionsParser

  case command of
    RunCabalToDhall options ->
      runCabalToDhall options
    PrintVersion ->
      printVersion


runCabalToDhall :: CabalToDhallOptions -> IO ()
runCabalToDhall CabalToDhallOptions{ cabalFilePath } = do
  source <-
    case cabalFilePath of
      Nothing ->
        ByteString.getContents

      Just filePath ->
        ByteString.readFile filePath

  dhall <- cabalToDhall dhallFromGitHub <$>
    parseGenericPackageDescriptionThrows source

  Pretty.renderIO
    System.IO.stdout
    ( Pretty.layoutSmart opts
        ( Pretty.pretty dhall )
    )

  putStrLn ""



-- Shamelessly taken from dhall-format

-- Note: must remain in sync with the layout options in
-- golden-tests/GoldenTests.hs, so that test output is easy to generate
-- at the command line.
opts :: Pretty.LayoutOptions
opts =
  Pretty.defaultLayoutOptions
    { Pretty.layoutPageWidth = Pretty.AvailablePerLine 80 1.0 }


printVersion :: IO ()
printVersion = do
  putStrLn ( "cabal-to-dhall version " ++ showVersion version )
