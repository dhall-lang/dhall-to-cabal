let prelude = ./../../dhall/prelude.dhall

let types = ./../../dhall/types.dhall

in    prelude.defaults.Package
    ⫽ { cabal-version =
          prelude.v "2.0"
      , library =
          Some
          (   λ(config : types.Config)
            →       if config.os (types.OS.OtherOS { _1 = "multics" })
              
              then    prelude.defaults.MainLibrary
                    ⫽ { exposed-modules = [ "A", "B" ] }
              
              else  prelude.defaults.MainLibrary ⫽ { exposed-modules = [ "A" ] }
          )
      , name =
          "test"
      , version =
          prelude.v "0"
      }