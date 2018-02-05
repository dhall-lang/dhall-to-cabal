    let empty-package = ./dhall/empty-package.dhall 

in  let licenses = constructors ./dhall/types/License.dhall 

in  let extensions = constructors ./dhall/types/Extension.dhall 

in  let package =
            λ(package : Text)
          → λ(version-range : VersionRange)
          → { bounds = version-range, package = package }

in  let common-deps =
          { Cabal =
              package "Cabal" (majorBoundVersion (v "2.0"))
          , base =
              package "base" (majorBoundVersion (v "4.10"))
          , bytestring =
              package "bytestring" (majorBoundVersion (v "0.10"))
          , dhall =
              package "dhall" (majorBoundVersion (v "1.9"))
          , dhall-to-cabal =
              package "dhall-to-cabal" anyVersion
          , text =
              package "text" (majorBoundVersion (v "1.2"))
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
                    [ common-deps.base
                    , common-deps.dhall-to-cabal
                    , package
                      "optparse-applicative"
                      ( unionVersionRanges
                        (majorBoundVersion (v "0.13.2"))
                        (majorBoundVersion (v "0.14"))
                      )
                    , common-deps.text
                    , common-deps.dhall
                    , common-deps.Cabal
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
                  [ common-deps.base
                  , common-deps.Cabal
                  , common-deps.dhall
                  , common-deps.text
                  , common-deps.bytestring
                  , package "containers" (majorBoundVersion (v "0.5"))
                  , package "vector" (majorBoundVersion (v "0.12"))
                  , package "trifecta" (majorBoundVersion (v "1.7"))
                  , package "text-format" (majorBoundVersion (v "0.3"))
                  , package "transformers" (majorBoundVersion (v "0.5.2"))
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
                    [ common-deps.bytestring
                    , common-deps.base
                    , common-deps.Cabal
                    , common-deps.text
                    , package "tasty" (majorBoundVersion (v "0.11"))
                    , package "filepath" (majorBoundVersion (v "1.4"))
                    , common-deps.dhall-to-cabal
                    , package "tasty-golden" (majorBoundVersion (v "2.3"))
                    , package "Diff" (majorBoundVersion (v "0.3.4"))
                    ]
                , hs-source-dirs =
                    [ "golden-tests" ]
                , main-is =
                    "GoldenTests.hs"
                }
            )
          ]
      }
