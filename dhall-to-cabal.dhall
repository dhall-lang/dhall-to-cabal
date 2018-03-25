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
              majorVersions "dhall" [ v "1.12.0" ]
          , dhall-to-cabal =
              package "dhall-to-cabal" anyVersion
          , filepath =
              majorVersions "filepath" [ v "1.4" ]
          , insert-ordered-containers =
              majorVersions "insert-ordered-containers" [ v "0.2.1.0" ]
          , optparse-applicative =
              majorVersions "optparse-applicative" [ v "0.13.2", v "0.14" ]
          , prettyprinter =
              majorVersions "prettyprinter" [ v "1.2.0.1" ]
          , contravariant =
              majorVersions "contravariant" [ v "1.4" ]
          , hashable =
              majorVersions "hashable" [ v "1.2.6.1" ]
          , tasty =
              majorVersions "tasty" [ v "0.11" ]
          , tasty-golden =
              majorVersions "tasty-golden" [ v "2.3" ]
          , text =
              majorVersions "text" [ v "1.2" ]
          , formatting =
              majorVersions "formatting" [ v "6.3.1" ]
          , transformers =
              majorVersions "transformers" [ v "0.5.2" ]
          , trifecta =
              majorVersions "trifecta" [ v "1.7" ]
          , vector =
              majorVersions "vector" [ v "0.12" ]
          }

in    prelude.utils.GitHub-project
      { owner = "ocharles", repo = "dhall-to-cabal" }
    ⫽ { synopsis =
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
      , maintainer =
          "ollie@ocharles.org.uk"
      , extra-source-files =
          [ "Changelog.md"
          , "dhall/defaults/BuildInfo.dhall"
          , "dhall/defaults/Library.dhall"
          , "dhall/defaults/CompilerOptions.dhall"
          , "dhall/defaults/SourceRepo.dhall"
          , "dhall/defaults/TestSuite.dhall"
          , "dhall/defaults/Executable.dhall"
          , "dhall/defaults/Package.dhall"
          , "dhall/defaults/Benchmark.dhall"
          , "dhall/unconditional.dhall"
          , "dhall/GitHub-project.dhall"
          , "dhall/prelude.dhall"
          , "dhall/types/VersionRange.dhall"
          , "dhall/types/OS.dhall"
          , "dhall/types/Guarded.dhall"
          , "dhall/types/License.dhall"
          , "dhall/types/Library.dhall"
          , "dhall/types/Version.dhall"
          , "dhall/types/Language.dhall"
          , "dhall/types/Extension.dhall"
          , "dhall/types/CompilerOptions.dhall"
          , "dhall/types/SourceRepo.dhall"
          , "dhall/types/TestSuite.dhall"
          , "dhall/types/Executable.dhall"
          , "dhall/types/Dependency.dhall"
          , "dhall/types/Mixin.dhall"
          , "dhall/types/Compiler.dhall"
          , "dhall/types/Config.dhall"
          , "dhall/types/Package.dhall"
          , "dhall/types/builtin.dhall"
          , "dhall/types/BuildType.dhall"
          , "dhall/types/RepoKind.dhall"
          , "dhall/types/Version/v.dhall"
          , "dhall/types/Arch.dhall"
          , "dhall/types/Scope.dhall"
          , "dhall/types/CustomSetup.dhall"
          , "dhall/types/Benchmark.dhall"
          , "dhall/types/Flag.dhall"
          , "dhall/types/ForeignLibrary.dhall"
          , "dhall/types/ModuleRenaming.dhall"
          , "dhall/types/RepoType.dhall"
          , "dhall/types/TestType.dhall"
          , "dhall/types/VersionRange/IntersectVersionRanges.dhall"
          , "dhall/types/VersionRange/WithinVersion.dhall"
          , "dhall/types/VersionRange/InvertVersionRange.dhall"
          , "dhall/types/VersionRange/EarlierVersion.dhall"
          , "dhall/types/VersionRange/DifferenceVersionRanges.dhall"
          , "dhall/types/VersionRange/ThisVersion.dhall"
          , "dhall/types/VersionRange/OrLaterVersion.dhall"
          , "dhall/types/VersionRange/OrEarlierVersion.dhall"
          , "dhall/types/VersionRange/AnyVersion.dhall"
          , "dhall/types/VersionRange/NotThisVersion.dhall"
          , "dhall/types/VersionRange/LaterVersion.dhall"
          , "dhall/types/VersionRange/NoVersion.dhall"
          , "dhall/types/VersionRange/MajorBoundVersion.dhall"
          , "dhall/types/VersionRange/UnionVersionRanges.dhall"
          , "dhall/types/SetupBuildInfo.dhall"
          ]
      , license =
          prelude.types.Licenses.MIT {=}
      , license-files =
          [ "LICENSE" ]
      , version =
          v "1.0.0"
      , library =
          prelude.unconditional.library
          (   prelude.defaults.Library
            ⫽ { build-depends =
                  [ deps.Cabal
                  , deps.base
                  , deps.bytestring
                  , deps.containers
                  , deps.dhall
                  , deps.formatting
                  , deps.hashable
                  , deps.insert-ordered-containers
                  , deps.text
                  , deps.transformers
                  , deps.trifecta
                  , deps.vector
                  ]
              , compiler-options =
                    prelude.defaults.CompilerOptions
                  ⫽ { GHC = [ "-Wall", "-fno-warn-name-shadowing" ] }
              , exposed-modules =
                  [ "DhallToCabal" ]
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
                    , deps.optparse-applicative
                    , deps.prettyprinter
                    , deps.text
                    ]
                , hs-source-dirs =
                    [ "exe" ]
                , main-is =
                    "Main.hs"
                , other-extensions =
                    [ prelude.types.Extensions.NamedFieldPuns True ]
                , default-language =
                    [ prelude.types.Languages.Haskell2010 {=} ] : Optional
                                                                  types.Language
                }
            )
          , prelude.unconditional.executable
            "cabal-to-dhall"
            (   prelude.defaults.Executable
              ⫽ { build-depends =
                    [ deps.Cabal
                    , deps.base
                    , deps.contravariant
                    , deps.dhall
                    , deps.hashable
                    , deps.dhall-to-cabal
                    , deps.insert-ordered-containers
                    , deps.optparse-applicative
                    , deps.prettyprinter
                    , deps.text
                    ]
                , hs-source-dirs =
                    [ "cabal-to-dhall" ]
                , main-is =
                    "Main.hs"
                , other-extensions =
                    [ prelude.types.Extensions.NamedFieldPuns True ]
                , default-language =
                    [ prelude.types.Languages.Haskell2010 {=} ] : Optional
                                                                  types.Language
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
                    , deps.dhall-to-cabal
                    , deps.filepath
                    , deps.tasty
                    , deps.tasty-golden
                    , deps.text
                    ]
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
          ]
      }
