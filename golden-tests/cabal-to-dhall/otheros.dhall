    let prelude = ./../../dhall/prelude.dhall

in  let types = ./../../dhall/types.dhall

in    prelude.defaults.Package
    ⫽ { name =
          "test"
      , version =
          prelude.v "0"
      , library =
          [   λ(config : types.Config)
            →       if config.os (prelude.types.OSs.OtherOS { _1 = "multics" })
              
              then    prelude.defaults.Library
                    ⫽ { exposed-modules = [ "A", "B" ] }
              
              else  prelude.defaults.Library ⫽ { exposed-modules = [ "A" ] }
          ] : Optional (types.Config → types.Library)
      }