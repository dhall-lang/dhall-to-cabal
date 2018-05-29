{-# language NamedFieldPuns #-}
{-# language OverloadedStrings #-}

module Main ( main ) where

import Control.Applicative ( (<**>), optional )
import GHC.Stack

import qualified Data.ByteString as ByteString
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.IO as LazyText
import qualified Data.Text.Prettyprint.Doc as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.Text as Pretty
import qualified Data.Text.Prettyprint.Doc.Symbols.Unicode as Pretty
import qualified Dhall.Core
import qualified Options.Applicative as OptParse
import qualified System.IO

import CabalToDhall ( cabalToDhall )
import DhallLocation ( dhallFromGitHub )


data Command
  = RunCabalToDhall CabalToDhallOptions


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


commandLineParser =
  RunCabalToDhall <$> ( cabalToDhallOptionsParser <**> OptParse.helper )


main :: IO ()
main = do
  command <-
    OptParse.execParser
      ( OptParse.info commandLineParser mempty )

  case command of
    RunCabalToDhall options ->
      runCabalToDhall options


runCabalToDhall :: CabalToDhallOptions -> IO ()
runCabalToDhall CabalToDhallOptions{ cabalFilePath } = do
  source <-
    case cabalFilePath of
      Nothing ->
        ByteString.getContents

      Just filePath ->
        ByteString.readFile filePath

  dhall <-
    cabalToDhall dhallFromGitHub source

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
