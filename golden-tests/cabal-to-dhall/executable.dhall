let prelude = ./../../dhall/prelude.dhall

let types = ./../../dhall/types.dhall

in    prelude.defaults.Package
    ⫽ { cabal-version =
          prelude.v "2.0"
      , executables =
          [ { executable =
                  λ(config : types.Config)
                → prelude.defaults.Executable ⫽ { main-is = "Main.hs" }
            , name =
                "hello"
            }
          ]
      , name =
          "blah"
      , version =
          prelude.v "1"
      }