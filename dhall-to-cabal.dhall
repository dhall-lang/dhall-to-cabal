let prelude = ./dhall/prelude.dhall

let types = ./dhall/types.dhall

let v = prelude.v

let anyVersion = prelude.anyVersion

let package =
        λ(package : Text)
      → λ(version-range : types.VersionRange)
      → { bounds = version-range, package = package }

let majorVersions = prelude.utils.majorVersions

let deps =
      { Cabal =
          majorVersions "Cabal" [ v "2.4" ]
      , Diff =
          majorVersions "Diff" [ v "0.3.4" ]
      , base =
          majorVersions "base" [ v "4.10", v "4.11", v "4.12" ]
      , bytestring =
          majorVersions "bytestring" [ v "0.10" ]
      , containers =
          majorVersions "containers" [ v "0.5", v "0.6" ]
      , directory =
          majorVersions "directory" [ v "1.3.0.2" ]
      , dhall =
          majorVersions "dhall" [ v "1.24.0" ]
      , dhall-to-cabal =
          package "dhall-to-cabal" anyVersion
      , filepath =
          majorVersions "filepath" [ v "1.4" ]
      , microlens =
          majorVersions
          "microlens"
          [ v "0.1.0.0", v "0.2.0.0", v "0.3.0.0", v "0.4.0.0" ]
      , optparse-applicative =
          majorVersions
          "optparse-applicative"
          [ v "0.13.2", v "0.14", v "0.15" ]
      , prettyprinter =
          majorVersions "prettyprinter" [ v "1.2.0.1", v "1.3.0" ]
      , contravariant =
          majorVersions "contravariant" [ v "1.4", v "1.5" ]
      , tasty =
          majorVersions
          "tasty"
          [ v "0.11", v "0.12", v "1.0", v "1.1", v "1.2" ]
      , tasty-golden =
          majorVersions "tasty-golden" [ v "2.3" ]
      , tasty-hunit =
          majorVersions "tasty-hunit" [ v "0.10.0.1" ]
      , text =
          majorVersions "text" [ v "1.2" ]
      , transformers =
          majorVersions "transformers" [ v "0.5.2" ]
      , vector =
          majorVersions "vector" [ v "0.12" ]
      }

let warning-options =
      [ "-Weverything"
      , "-Wno-safe"
      , "-Wno-unsafe"
      , "-Wno-implicit-prelude"
      , "-Wno-missed-specialisations"
      , "-Wno-all-missed-specialisations"
      , "-Wno-missing-import-lists"
      , "-Wno-missing-local-signatures"
      , "-Wno-monomorphism-restriction"
      , "-fno-warn-name-shadowing"
      ]

in    prelude.utils.GitHub-project
      { owner = "ocharles", repo = "dhall-to-cabal" }
    ⫽ { cabal-version =
          v "2.4"
      , synopsis =
          "Compile Dhall expressions to Cabal files"
      , description =
          ''
          dhall-to-cabal takes Dhall expressions and compiles them into Cabal
          files. All of the features of Dhall are supported, such as let
          bindings and imports, and all features of Cabal are supported
          (including conditional stanzas).
          ''
      , category =
          "Distribution"
      , build-type =
          Some types.BuildType.Simple
      , maintainer =
          "ollie@ocharles.org.uk"
      , author =
          "Ollie Charles <ollie@ocharles.org.uk>"
      , extra-source-files =
          [ "Changelog.md"
          , "README.md"
          , "dhall/**/*.dhall"
          , "golden-tests/dhall-to-cabal/*.dhall"
          , "golden-tests/dhall-to-cabal/*.cabal"
          , "golden-tests/cabal-to-dhall/*.dhall"
          , "golden-tests/cabal-to-dhall/*.cabal"
          ]
      , license =
          types.License.MIT
      , license-files =
          [ "LICENSE" ]
      , version =
          v "1.3.3.0"
      , library =
          prelude.unconditional.library
          (   prelude.defaults.Library
            ⫽ { build-depends =
                  [ deps.Cabal
                  , deps.base
                  , deps.bytestring
                  , deps.containers
                  , deps.contravariant
                  , deps.dhall
                  , deps.filepath
                  , deps.microlens
                  , deps.text
                  , deps.transformers
                  , deps.vector
                  ]
              , compiler-options =
                  prelude.defaults.CompilerOptions ⫽ { GHC = warning-options }
              , autogen-modules =
                  [ "Paths_dhall_to_cabal" ]
              , exposed-modules =
                  [ "CabalToDhall"
                  , "DhallLocation"
                  , "DhallToCabal"
                  , "DhallToCabal.FactorType"
                  , "DhallToCabal.Util"
                  ]
              , hs-source-dirs =
                  [ "lib" ]
              , other-extensions =
                  [ types.Extension.ApplicativeDo True
                  , types.Extension.GADTs True
                  , types.Extension.GeneralizedNewtypeDeriving True
                  , types.Extension.LambdaCase True
                  , types.Extension.OverloadedStrings True
                  , types.Extension.RecordWildCards True
                  , types.Extension.TypeApplications True
                  ]
              , other-modules =
                  [ "DhallToCabal.ConfigTree"
                  , "DhallToCabal.Diff"
                  , "Dhall.Extra"
                  , "Paths_dhall_to_cabal"
                  ]
              , default-language =
                  Some types.Language.Haskell2010
              }
          )
      , executables =
          [ prelude.unconditional.executable
            "dhall-to-cabal"
            (   prelude.defaults.Executable
              ⫽ { build-depends =
                    [ deps.Cabal
                    , deps.base
                    , deps.containers
                    , deps.dhall
                    , deps.dhall-to-cabal
                    , deps.directory
                    , deps.filepath
                    , deps.microlens
                    , deps.optparse-applicative
                    , deps.prettyprinter
                    , deps.text
                    , deps.transformers
                    ]
                , compiler-options =
                    prelude.defaults.CompilerOptions ⫽ { GHC = warning-options }
                , hs-source-dirs =
                    [ "exe" ]
                , main-is =
                    "Main.hs"
                , other-extensions =
                    [ types.Extension.NamedFieldPuns True ]
                , other-modules =
                    [ "Paths_dhall_to_cabal" ]
                , autogen-modules =
                    [ "Paths_dhall_to_cabal" ]
                , default-language =
                    Some types.Language.Haskell2010
                }
            )
          , prelude.unconditional.executable
            "cabal-to-dhall"
            (   prelude.defaults.Executable
              ⫽ { build-depends =
                    [ deps.base
                    , deps.dhall
                    , deps.bytestring
                    , deps.dhall-to-cabal
                    , deps.optparse-applicative
                    , deps.prettyprinter
                    , deps.text
                    ]
                , compiler-options =
                    prelude.defaults.CompilerOptions ⫽ { GHC = warning-options }
                , hs-source-dirs =
                    [ "cabal-to-dhall" ]
                , main-is =
                    "Main.hs"
                , other-extensions =
                    [ types.Extension.NamedFieldPuns True ]
                , other-modules =
                    [ "Paths_dhall_to_cabal" ]
                , autogen-modules =
                    [ "Paths_dhall_to_cabal" ]
                , default-language =
                    Some types.Language.Haskell2010
                }
            )
          , prelude.unconditional.executable
            "dhall-to-cabal-meta"
            (   prelude.defaults.Executable
              ⫽ { scope =
                    types.Scope.Private
                , build-depends =
                    [ deps.base
                    , deps.directory
                    , deps.dhall
                    , deps.dhall-to-cabal
                    , deps.filepath
                    , deps.optparse-applicative
                    , deps.prettyprinter
                    ]
                , hs-source-dirs =
                    [ "meta" ]
                , default-language =
                    Some types.Language.Haskell2010
                , compiler-options =
                    prelude.defaults.CompilerOptions ⫽ { GHC = warning-options }
                , main-is =
                    "Main.hs"
                }
            )
          ]
      , test-suites =
          [ prelude.unconditional.test-suite
            "golden-tests"
            (   prelude.defaults.TestSuite
              ⫽ { build-depends =
                    [ deps.base
                    , deps.Cabal
                    , deps.Diff
                    , deps.bytestring
                    , deps.dhall
                    , deps.dhall-to-cabal
                    , deps.filepath
                    , deps.microlens
                    , deps.prettyprinter
                    , deps.tasty
                    , deps.tasty-golden
                    , deps.text
                    ]
                , compiler-options =
                    prelude.defaults.CompilerOptions ⫽ { GHC = warning-options }
                , hs-source-dirs =
                    [ "golden-tests" ]
                , type =
                    types.TestType.exitcode-stdio { main-is = "GoldenTests.hs" }
                , default-language =
                    Some types.Language.Haskell2010
                }
            )
          , prelude.unconditional.test-suite
            "unit-tests"
            (   prelude.defaults.TestSuite
              ⫽ { build-depends =
                    [ deps.base
                    , deps.Cabal
                    , deps.dhall
                    , deps.dhall-to-cabal
                    , deps.tasty
                    , deps.tasty-hunit
                    , deps.text
                    ]
                , compiler-options =
                    prelude.defaults.CompilerOptions ⫽ { GHC = warning-options }
                , hs-source-dirs =
                    [ "tests" ]
                , type =
                    types.TestType.exitcode-stdio { main-is = "Tests.hs" }
                , default-language =
                    Some types.Language.Haskell2010
                , other-modules =
                    [ "DhallToCabal.Tests" ]
                }
            )
          ]
      }
