let types = ../types.dhall

let mapOptional = https://prelude.dhall-lang.org/v8.0.0/Optional/map

let mapList = https://prelude.dhall-lang.org/v8.0.0/List/map

let mapGuarded =
        λ(a : Type)
      → λ(f : a → a)
      → λ(x : types.Config → a)
      → λ(config : types.Config)
      → f (x config)

let NamedGuardedExecutable =
      { name : Text, executable : types.Config → types.Executable }

let NamedGuardedLibrary =
      { name : Text, library : types.Config → types.Library }

let NamedGuardedTestSuite =
      { name : Text, test-suite : types.Config → types.TestSuite }

let NamedGuardedBenchmark =
      { name : Text, benchmark : types.Config → types.Benchmark }

let NamedGuardedForeignLibrary =
      { name : Text, foreign-lib : types.Config → types.ForeignLibrary }

in    λ(f : types.BuildInfo → types.BuildInfo)
    → λ(package : types.Package)
    →   package
      ⫽ { library =
            mapOptional
            (types.Config → types.Library)
            (types.Config → types.Library)
            ( mapGuarded
              types.Library
              (   λ(component : types.Library)
                → component ⫽ f component.(types.BuildInfo)
              )
            )
            package.library
        , executables =
            mapList
            NamedGuardedExecutable
            NamedGuardedExecutable
            (   λ(namedGuardedExecutable : NamedGuardedExecutable)
              → { name =
                    namedGuardedExecutable.name
                , executable =
                    mapGuarded
                    types.Executable
                    (   λ(component : types.Executable)
                      → component ⫽ f component.(types.BuildInfo)
                    )
                    namedGuardedExecutable.executable
                }
            )
            package.executables
        , sub-libraries =
            mapList
            NamedGuardedLibrary
            NamedGuardedLibrary
            (   λ(namedGuardedLibrary : NamedGuardedLibrary)
              → { name =
                    namedGuardedLibrary.name
                , library =
                    mapGuarded
                    types.Library
                    (   λ(component : types.Library)
                      → component ⫽ f component.(types.BuildInfo)
                    )
                    namedGuardedLibrary.library
                }
            )
            package.sub-libraries
        , test-suites =
            mapList
            NamedGuardedTestSuite
            NamedGuardedTestSuite
            (   λ(namedGuardedTestSuite : NamedGuardedTestSuite)
              → { name =
                    namedGuardedTestSuite.name
                , test-suite =
                    mapGuarded
                    types.TestSuite
                    (   λ(component : types.TestSuite)
                      → component ⫽ f component.(types.BuildInfo)
                    )
                    namedGuardedTestSuite.test-suite
                }
            )
            package.test-suites
        , benchmarks =
            mapList
            NamedGuardedBenchmark
            NamedGuardedBenchmark
            (   λ(namedGuardedBenchmark : NamedGuardedBenchmark)
              → { name =
                    namedGuardedBenchmark.name
                , benchmark =
                    mapGuarded
                    types.Benchmark
                    (   λ(component : types.Benchmark)
                      → component ⫽ f component.(types.BuildInfo)
                    )
                    namedGuardedBenchmark.benchmark
                }
            )
            package.benchmarks
        , foreign-libraries =
            mapList
            NamedGuardedForeignLibrary
            NamedGuardedForeignLibrary
            (   λ(namedGuardedForeignLibrary : NamedGuardedForeignLibrary)
              → { name =
                    namedGuardedForeignLibrary.name
                , foreign-lib =
                    mapGuarded
                    types.ForeignLibrary
                    (   λ(component : types.ForeignLibrary)
                      → component ⫽ f component.(types.BuildInfo)
                    )
                    namedGuardedForeignLibrary.foreign-lib
                }
            )
            package.foreign-libraries
        }
