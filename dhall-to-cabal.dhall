    let prelude = ./dhall/prelude.dhall

in  let types = ./dhall/types.dhall

in  let v = prelude.v

in  let anyVersion = prelude.anyVersion

in  let OS = types.OS

in  let package =
            λ(package : Text)
          → λ(version-range : types.VersionRange)
          → { bounds = version-range, package = package }

in  let majorVersions = prelude.utils.majorVersions

in  let deps =
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
          , dhall =
              majorVersions "dhall" [ v "1.18.0" ]
          , dhall-to-cabal =
              package "dhall-to-cabal" anyVersion
          , directory =
              majorVersions "directory" [ v "1.3.0.2" ]
          , filepath =
              majorVersions "filepath" [ v "1.4" ]
          , microlens =
              majorVersions
              "microlens"
              [ v "0.1.0.0", v "0.2.0.0", v "0.3.0.0", v "0.4.0.0" ]
          , optparse-applicative =
              majorVersions "optparse-applicative" [ v "0.13.2", v "0.14" ]
          , prettyprinter =
              majorVersions "prettyprinter" [ v "1.2.0.1" ]
          , contravariant =
              majorVersions "contravariant" [ v "1.4", v "1.5" ]
          , hashable =
              majorVersions "hashable" [ v "1.2.6.1" ]
          , tasty =
              majorVersions "tasty" [ v "0.11", v "0.12", v "1.0", v "1.1" ]
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

in  let warning-options =
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
      -- build-type simple is needed to allow tools compatible with cabal < 2.2 build the package
      , build-type =
          [ prelude.types.BuildTypes.Simple {=} ] : Optional types.BuildType
      , maintainer =
          "ollie@ocharles.org.uk"
      , author =
          "Ollie Charles <ollie@ocharles.org.uk>"
      , extra-source-files =
          [ "Changelog.md"
          , "README.md"
          , "dhall/SPDX/and.dhall"
          , "dhall/SPDX/license.dhall"
          , "dhall/SPDX/licenseVersionOrLater.dhall"
          , "dhall/SPDX/or.dhall"
          , "dhall/SPDX/ref.dhall"
          , "dhall/SPDX/refWithFile.dhall"
          , "dhall/Version/v.dhall"
          , "dhall/VersionRange/anyVersion.dhall"
          , "dhall/VersionRange/differenceVersionRanges.dhall"
          , "dhall/VersionRange/earlierVersion.dhall"
          , "dhall/VersionRange/intersectVersionRanges.dhall"
          , "dhall/VersionRange/invertVersionRange.dhall"
          , "dhall/VersionRange/laterVersion.dhall"
          , "dhall/VersionRange/majorBoundVersion.dhall"
          , "dhall/VersionRange/noVersion.dhall"
          , "dhall/VersionRange/notThisVersion.dhall"
          , "dhall/VersionRange/orEarlierVersion.dhall"
          , "dhall/VersionRange/orLaterVersion.dhall"
          , "dhall/VersionRange/thisVersion.dhall"
          , "dhall/VersionRange/unionVersionRanges.dhall"
          , "dhall/VersionRange/withinVersion.dhall"
          , "dhall/defaults/Benchmark.dhall"
          , "dhall/defaults/BuildInfo.dhall"
          , "dhall/defaults/CompilerOptions.dhall"
          , "dhall/defaults/Executable.dhall"
          , "dhall/defaults/Library.dhall"
          , "dhall/defaults/Package.dhall"
          , "dhall/defaults/SourceRepo.dhall"
          , "dhall/defaults/TestSuite.dhall"
          , "dhall/prelude.dhall"
          , "dhall/types.dhall"
          , "dhall/types/Arch.dhall"
          , "dhall/types/Benchmark.dhall"
          , "dhall/types/BuildInfo.dhall"
          , "dhall/types/BuildType.dhall"
          , "dhall/types/Compiler.dhall"
          , "dhall/types/CompilerOptions.dhall"
          , "dhall/types/Config.dhall"
          , "dhall/types/CustomSetup.dhall"
          , "dhall/types/Dependency.dhall"
          , "dhall/types/Executable.dhall"
          , "dhall/types/Extension.dhall"
          , "dhall/types/Flag.dhall"
          , "dhall/types/ForeignLibrary.dhall"
          , "dhall/types/Guarded.dhall"
          , "dhall/types/Language.dhall"
          , "dhall/types/Library.dhall"
          , "dhall/types/License.dhall"
          , "dhall/types/Mixin.dhall"
          , "dhall/types/ModuleRenaming.dhall"
          , "dhall/types/OS.dhall"
          , "dhall/types/Package.dhall"
          , "dhall/types/RepoKind.dhall"
          , "dhall/types/RepoType.dhall"
          , "dhall/types/SPDX.dhall"
          , "dhall/types/SPDX/LicenseExceptionId.dhall"
          , "dhall/types/SPDX/LicenseId.dhall"
          , "dhall/types/Scope.dhall"
          , "dhall/types/SetupBuildInfo.dhall"
          , "dhall/types/SourceRepo.dhall"
          , "dhall/types/TestSuite.dhall"
          , "dhall/types/TestType.dhall"
          , "dhall/types/Version.dhall"
          , "dhall/types/VersionRange.dhall"
          , "dhall/types/builtin.dhall"
          , "dhall/unconditional.dhall"
          , "dhall/utils/GitHub-project.dhall"
          , "dhall/utils/majorVersions.dhall"
          , "dhall/utils/mapSourceRepos.dhall"
          , "dhall/utils/package.dhall"
          , "golden-tests/dhall-to-cabal/*.dhall"
          , "golden-tests/dhall-to-cabal/*.cabal"
          , "golden-tests/cabal-to-dhall/*.dhall"
          , "golden-tests/cabal-to-dhall/*.cabal"
          ]
      , license =
          prelude.types.Licenses.MIT {=}
      , license-files =
          [ "LICENSE" ]
      , version =
          v "1.3.1.0"
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
                  , deps.hashable
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
                  , "DhallToCabal.Util"
                  ]
              , hs-source-dirs =
                  [ "lib" ]
              , other-extensions =
                  [ prelude.types.Extensions.ApplicativeDo True
                  , prelude.types.Extensions.GADTs True
                  , prelude.types.Extensions.GeneralizedNewtypeDeriving True
                  , prelude.types.Extensions.LambdaCase True
                  , prelude.types.Extensions.OverloadedStrings True
                  , prelude.types.Extensions.RecordWildCards True
                  , prelude.types.Extensions.TypeApplications True
                  ]
              , other-modules =
                  [ "DhallToCabal.ConfigTree"
                  , "DhallToCabal.Diff"
                  , "Dhall.Extra"
                  , "Paths_dhall_to_cabal"
                  ]
              , default-language =
                  [ prelude.types.Languages.Haskell2010 {=} ] : Optional
                                                                types.Language
              }
          )
      , executables =
          [ prelude.unconditional.executable
            "dhall-to-cabal"
            (   prelude.defaults.Executable
              ⫽ { build-depends =
                    [ deps.Cabal
                    , deps.base
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
                    [ prelude.types.Extensions.NamedFieldPuns True ]
                , other-modules =
                    [ "Paths_dhall_to_cabal" ]
                , autogen-modules =
                    [ "Paths_dhall_to_cabal" ]
                , default-language =
                    [ prelude.types.Languages.Haskell2010 {=} ] : Optional
                                                                  types.Language
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
                    [ prelude.types.Extensions.NamedFieldPuns True ]
                , other-modules =
                    [ "Paths_dhall_to_cabal" ]
                , autogen-modules =
                    [ "Paths_dhall_to_cabal" ]
                , default-language =
                    [ prelude.types.Languages.Haskell2010 {=} ] : Optional
                                                                  types.Language
                }
            )
          , prelude.unconditional.executable
            "dhall-to-cabal-meta"
            (   prelude.defaults.Executable
              ⫽ { scope =
                    prelude.types.Scopes.Private {=}
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
                    [ prelude.types.Languages.Haskell2010 {=} ] : Optional
                                                                  types.Language
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
                    prelude.types.TestTypes.exitcode-stdio
                    { main-is = "GoldenTests.hs" }
                , default-language =
                    [ prelude.types.Languages.Haskell2010 {=} ] : Optional
                                                                  types.Language
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
                    prelude.types.TestTypes.exitcode-stdio
                    { main-is = "Tests.hs" }
                , default-language =
                    [ prelude.types.Languages.Haskell2010 {=} ] : Optional
                                                                  types.Language
                , other-modules =
                    [ "DhallToCabal.Tests" ]
                }
            )
          ]
      }
