let prelude = ./../../dhall/prelude.dhall

let types = ./../../dhall/types.dhall

in    prelude.defaults.Package
    ⫽ { benchmarks =
          [ { benchmark =
                  λ(config : types.Config)
                →   prelude.defaults.Benchmark
                  ⫽ { compiler-options =
                          prelude.defaults.CompilerOptions
                        ⫽ { GHC = [ "-O2" ] : List Text }
                    , main-is =
                        "Main.hs"
                    }
            , name =
                "fancy-benchmark"
            }
          ]
      , cabal-version =
          prelude.v "2.0"
      , name =
          "blah"
      , version =
          prelude.v "1"
      }