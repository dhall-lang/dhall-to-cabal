    let stdlib = ./dhall/stdlib.dhall 

in  let gitHub-project = ./dhall/GitHub-project.dhall 

in  let OS = ./dhall/types/OS.dhall 

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
          , insert-ordered-containers =
              package
              "insert-ordered-containers"
              (majorBoundVersion (v "0.2.1.0"))
          , optparse-applicative =
              package
              "optparse-applicative"
              ( unionVersionRanges
                (majorBoundVersion (v "0.13.2"))
                (majorBoundVersion (v "0.14"))
              )
          , prettyprinter =
              package "prettyprinter" (majorBoundVersion (v "1.1.1"))
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

in    gitHub-project { owner = "ocharles", repo = "dhall-to-cabal" }
    ⫽ { license =
          stdlib.`constructors`.Licenses.MIT {=}
      , license-files =
          [ "LICENSE" ]
      , version =
          v "0.1.0"
      , library =
          stdlib.unconditional.library
          (   stdlib.default.Library
            ⫽ { build-depends =
                  [ deps.Cabal
                  , deps.base
                  , deps.bytestring
                  , deps.containers
                  , deps.insert-ordered-containers
                  , deps.dhall
                  , deps.text
                  , deps.text-format
                  , deps.transformers
                  , deps.trifecta
                  , deps.vector
                  ]
              , compiler-options =
                    stdlib.default.CompilerOptions
                  ⫽ { GHC = [ "-Wall", "-fno-warn-name-shadowing" ] }
              , exposed-modules =
                  [ "Distribution.Package.Dhall" ]
              , hs-source-dirs =
                  [ "lib" ]
              , other-extensions =
                  [ stdlib.`constructors`.Extensions.ApplicativeDo True
                  , stdlib.`constructors`.Extensions.GADTs True
                  , stdlib.`constructors`.Extensions.GeneralizedNewtypeDeriving
                    True
                  , stdlib.`constructors`.Extensions.LambdaCase True
                  , stdlib.`constructors`.Extensions.OverloadedStrings True
                  , stdlib.`constructors`.Extensions.RecordWildCards True
                  , stdlib.`constructors`.Extensions.TypeApplications True
                  ]
              , other-modules =
                  [ "Dhall.Extra" ]
              }
          )
      , executables =
          [ stdlib.unconditional.executable
            "dhall-to-cabal"
            (   stdlib.default.Executable
              ⫽ { build-depends =
                    [ deps.Cabal
                    , deps.base
                    , deps.dhall
                    , deps.dhall-to-cabal
                    , deps.optparse-applicative
                    , deps.prettyprinter
                    , deps.text
                    ]
                , hs-source-dirs =
                    [ "exe" ]
                , main-is =
                    "Main.hs"
                , other-extensions =
                    [ stdlib.`constructors`.Extensions.NamedFieldPuns True ]
                }
            )
          ]
      , test-suites =
          [ stdlib.unconditional.test-suite
            "golden-tests"
            (   stdlib.default.TestSuite
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
                , type =
                    stdlib.`constructors`.TestTypes.exitcode-stdio
                    { main-is = "GoldenTests.hs" }
                }
            )
          ]
      }
