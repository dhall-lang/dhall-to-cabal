    let stdlib = ./dhall/stdlib.dhall 

in  let v = ./dhall/types/Version/v.dhall 

in  let anyVersion = ./dhall/types/VersionRange/AnyVersion.dhall 

in  let OS = ./dhall/types/OS.dhall 

in  let package =
            λ(package : Text)
          → λ(version-range : ./dhall/types/VersionRange.dhall )
          → { bounds = version-range, package = package }

in  let majorVersions = stdlib.dependency.majorVersions

in  let deps =
          { Cabal =
              majorVersions "Cabal" [ v "2.0" ]
          , Diff =
              majorVersions "Diff" [ v "0.3.4" ]
          , base =
              majorVersions "base" [ v "4.10" ]
          , bytestring =
              majorVersions "bytestring" [ v "0.10" ]
          , containers =
              majorVersions "containers" [ v "0.5" ]
          , dhall =
              majorVersions "dhall" [ v "1.9" ]
          , dhall-to-cabal =
              package "dhall-to-cabal" anyVersion
          , filepath =
              majorVersions "filepath" [ v "1.4" ]
          , insert-ordered-containers =
              majorVersions "insert-ordered-containers" [ v "0.2.1.0" ]
          , optparse-applicative =
              majorVersions "optparse-applicative" [ v "0.13.2", v "0.14" ]
          , prettyprinter =
              majorVersions "prettyprinter" [ v "1.1.1" ]
          , tasty =
              majorVersions "tasty" [ v "0.11" ]
          , tasty-golden =
              majorVersions "tasty-golden" [ v "2.3" ]
          , text =
              majorVersions "text" [ v "1.2" ]
          , text-format =
              majorVersions "text-format" [ v "0.3" ]
          , transformers =
              majorVersions "transformers" [ v "0.5.2" ]
          , trifecta =
              majorVersions "trifecta" [ v "1.7" ]
          , vector =
              majorVersions "vector" [ v "0.12" ]
          }

in    stdlib.GitHub-project { owner = "ocharles", repo = "dhall-to-cabal" }
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
