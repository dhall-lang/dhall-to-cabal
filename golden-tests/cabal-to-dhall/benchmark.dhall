    let prelude = ./../../dhall/prelude.dhall

in  let types = ./../../dhall/types.dhall

in    prelude.defaults.Package
    ⫽ { name =
          "blah"
      , version =
          prelude.v "1"
      , benchmarks =
          [ { benchmark =
                  λ(config : types.Config)
                →   prelude.defaults.Benchmark
                  ⫽ { main-is =
                        "Main.hs"
                    , compiler-options =
                          prelude.defaults.CompilerOptions
                        ⫽ { GHC = [ "-O2" ] : List Text }
                    }
            , name =
                "fancy-benchmark"
            }
          ]
      , cabal-version =
          prelude.v "2.0"
      }