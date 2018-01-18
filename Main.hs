module Main (main) where

import Data.Function ( (&) )
import System.Environment ( getArgs )
import Options.Applicative

import Distribution.Package.Dhall

import qualified Data.Text.Lazy.IO as LazyText
import qualified Dhall
import qualified Distribution.PackageDescription.PrettyPrint as Cabal

data Options = Options { dhallFilePath :: String }

run :: Options -> IO ()
run (Options dhallFilePath) = 
    LazyText.readFile dhallFilePath
    >>= Dhall.detailed . Dhall.input packageDescription
    & fmap Cabal.showPackageDescription
    >>= putStrLn

main :: IO ()
main = execParser opts >>= run
    where
        parser = Options <$> argument str (metavar "<dhall input file>")
        opts = info parser mempty

