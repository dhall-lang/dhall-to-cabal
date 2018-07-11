    let prelude = ./../../dhall/prelude.dhall

in  let types = ./../../dhall/types.dhall

in    prelude.defaults.Package
    ⫽ { name =
          "test"
      , version =
          prelude.v "0"
      , cabal-version =
          prelude.v "2.0"
      , library =
          [   λ(config : types.Config)
            →       if config.os (prelude.types.OSs.OtherOS { _1 = "multics" })
              
              then    prelude.defaults.Library
                    ⫽ { default-language =
                          [] : Optional types.Language
                      , exposed-modules =
                          [ "A", "B" ]
                      }
              
              else    prelude.defaults.Library
                    ⫽ { default-language =
                          [] : Optional types.Language
                      , exposed-modules =
                          [ "A" ]
                      }
          ] : Optional (types.Config → types.Library)
      }