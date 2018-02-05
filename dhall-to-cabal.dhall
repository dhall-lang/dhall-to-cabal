    let empty-package = ./dhall/empty-package.dhall 

in  let licenses = constructors ./dhall/types/License.dhall 

in  let extensions = constructors ./dhall/types/Extension.dhall 

in  let package =
            λ(package : Text)
          → λ(version-range : VersionRange)
          → { bounds = version-range, package = package }

in  let deps =
          { Cabal =
              package "Cabal" (majorBoundVersion (v "2.0"))
          , Diff =
              package "Diff" (majorBoundVersion (v "0.3.4"))
          , base =
              package "base" (majorBoundVersion (v "4.10"))
          , bytestring =
              package "bytestring" (majorBoundVersion (v "0.10"))
          , containers =
              package "containers" (majorBoundVersion (v "0.5"))
          , dhall =
              package "dhall" (majorBoundVersion (v "1.9"))
          , dhall-to-cabal =
              package "dhall-to-cabal" anyVersion
          , filepath =
              package "filepath" (majorBoundVersion (v "1.4"))
          , optparse-applicative =
              package
              "optparse-applicative"
              ( unionVersionRanges
                (majorBoundVersion (v "0.13.2"))
                (majorBoundVersion (v "0.14"))
              )
          , tasty =
              package "tasty" (majorBoundVersion (v "0.11"))
          , tasty-golden =
              package "tasty-golden" (majorBoundVersion (v "2.3"))
          , text =
              package "text" (majorBoundVersion (v "1.2"))
          , text-format =
              package "text-format" (majorBoundVersion (v "0.3"))
          , transformers =
              package "transformers" (majorBoundVersion (v "0.5.2"))
          , trifecta =
              package "trifecta" (majorBoundVersion (v "1.7"))
          , vector =
              package "vector" (majorBoundVersion (v "0.12"))
          }

in  let gitHub-project = ./dhall/GitHub-project.dhall 

in  let OS = ./dhall/types/OS.dhall 

in  let unconditional = ./dhall/unconditional.dhall 

in    gitHub-project { owner = "ocharles", repo = "dhall-to-cabal" }
    ⫽ { executables =
          [ unconditional.executable
            "dhall-to-cabal"
            (   ./dhall/defaults/Executable.dhall 
              ⫽ { build-depends =
                    [ deps.Cabal
                    , deps.base
                    , deps.dhall
                    , deps.dhall-to-cabal
                    , deps.optparse-applicative
                    , deps.text
                    ]
                , hs-source-dirs =
                    [ "exe" ]
                , main-is =
                    "Main.hs"
                , other-extensions =
                    [ extensions.NamedFieldPuns True ]
                }
            )
          ]
      , library =
          unconditional.library
          (   ./dhall/defaults/Library.dhall 
            ⫽ { build-depends =
                  [ deps.Cabal
                  , deps.base
                  , deps.bytestring
                  , deps.containers
                  , deps.dhall
                  , deps.text
                  , deps.text-format
                  , deps.transformers
                  , deps.trifecta
                  , deps.vector
                  ]
              , compiler-options =
                    ./dhall/defaults/CompilerOptions.dhall 
                  ⫽ { GHC = [ "-Wall", "-fno-warn-name-shadowing" ] }
              , exposed-modules =
                  [ "Distribution.Package.Dhall" ]
              , hs-source-dirs =
                  [ "lib" ]
              , other-extensions =
                  [ extensions.ApplicativeDo True
                  , extensions.GADTs True
                  , extensions.GeneralizedNewtypeDeriving True
                  , extensions.LambdaCase True
                  , extensions.OverloadedStrings True
                  , extensions.RecordWildCards True
                  , extensions.TypeApplications True
                  ]
              , other-modules =
                  [ "Dhall.Extra" ]
              }
          )
      , license =
          licenses.MIT {=}
      , version =
          [ +0, +1, +0 ]
      , test-suites =
          [ unconditional.test-suite
            "golden-tests"
            (   ./dhall/defaults/TestSuite.dhall 
              ⫽ { build-depends =
                    [ deps.base
                    , deps.Cabal
                    , deps.Diff
                    , deps.bytestring
                    , deps.dhall-to-cabal
                    , deps.filepath
                    , deps.tasty
                    , deps.tasty-golden
                    , deps.text
                    ]
                , hs-source-dirs =
                    [ "golden-tests" ]
                , main-is =
                    "GoldenTests.hs"
                }
            )
          ]
      }
