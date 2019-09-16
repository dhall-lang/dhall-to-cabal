let prelude = ../../dhall/prelude.dhall

let types = ../../dhall/types.dhall

let f =
        λ(buildInfo : types.BuildInfo)
      →   buildInfo
        ⫽ { build-depends =
                buildInfo.build-depends
              # [ prelude.utils.majorVersions "injected" [ prelude.v "1.0" ] [ types.LibraryName.main-library ] ]
          }

in  prelude.utils.mapBuildInfo
    f
    (   prelude.defaults.Package
      ⫽ { name =
            "pkg"
        , version =
            prelude.v "0"
        , library =
            Some
            (   λ(config : types.Config)
              →   prelude.defaults.MainLibrary
                ⫽ { build-depends =
                      [ prelude.utils.majorVersions
                        "library"
                        [ prelude.v "1.0" ]
                        [ types.LibraryName.main-library ]
                      ]
                  }
            )
        , custom-setup =
            Some
            { setup-depends =
                [ prelude.utils.majorVersions "setup" [ prelude.v "1.0" ] [ types.LibraryName.main-library ] ]
            }
        , benchmarks =
            [ { name =
                  "bench"
              , benchmark =
                    λ(config : types.Config)
                  →   prelude.defaults.Benchmark
                    ⫽ { main-is =
                          "Bench.hs"
                      , build-depends =
                          [ prelude.utils.majorVersions
                            "bench"
                            [ prelude.v "1.0" ]
                            [ types.LibraryName.main-library ]
                          ]
                      }
              }
            ]
        , executables =
            [ { name =
                  "exe"
              , executable =
                    λ(config : types.Config)
                  →   prelude.defaults.Executable
                    ⫽ { main-is =
                          "Exe.hs"
                      , build-depends =
                          [ prelude.utils.majorVersions
                            "exe"
                            [ prelude.v "1.0" ]
                            [ types.LibraryName.main-library ]
                          ]
                      }
              }
            ]
        , foreign-libraries =
            [ { name =
                  "flib"
              , foreign-lib =
                    λ(config : types.Config)
                  →   ../../dhall/defaults/BuildInfo.dhall
                    ⫽ { type = types.ForeignLibType.Static
                      , options = [] : List types.ForeignLibOption
                      , mod-def-files = [] : List Text
                      , lib-version-info = None { current : Natural, revision : Natural, age : Natural }
                      , lib-version-linux = None types.Version
                      , build-depends =
                          [ prelude.utils.majorVersions
                            "flib"
                            [ prelude.v "1.0" ]
                            [ types.LibraryName.main-library ]
                          ]
                      }
              }
            ]
        , sub-libraries =
            [ { name =
                  "sublib"
              , library =
                    λ(config : types.Config)
                  →   prelude.defaults.NamedLibrary
                    ⫽ { build-depends =
                          [ prelude.utils.majorVersions
                            "sublib"
                            [ prelude.v "1.0" ]
                            [ types.LibraryName.main-library ]
                          ]
                      }
              }
            ]
        , test-suites =
            [ { name =
                  "tests"
              , test-suite =
                    λ(config : types.Config)
                  →   prelude.defaults.TestSuite
                    ⫽ { type =
                          types.TestType.exitcode-stdio { main-is = "Test.hs" }
                      , build-depends =
                          [ prelude.utils.majorVersions
                            "tests"
                            [ prelude.v "1.0" ]
                            [ types.LibraryName.main-library ]
                          ]
                      }
              }
            ]
        }
    )
