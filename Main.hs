{-# language NamedFieldPuns #-}

module Main (main) where

import Data.Function ( (&) )
import System.Environment ( getArgs )

import Distribution.Package.Dhall

import qualified Data.Text.Lazy.IO as LazyText
import qualified Dhall
import qualified Distribution.PackageDescription.PrettyPrint as Cabal
import qualified Options.Applicative as OptParse

data Options = Options { dhallFilePath :: String }

run :: Options -> IO ()
run Options { dhallFilePath } =
  LazyText.readFile dhallFilePath
   >>= Dhall.detailed . Dhall.input packageDescription
   & fmap Cabal.showPackageDescription
   >>= putStrLn

main :: IO ()
main =
  OptParse.execParser opts >>= run

  where

  parser =
    Options
      <$> OptParse.argument
            OptParse.str ( OptParse.metavar "<dhall input file>" )

  opts =
    OptParse.info parser mempty
