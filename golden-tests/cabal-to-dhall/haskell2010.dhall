    let prelude = ./../../dhall/prelude.dhall

in  let types = ./../../dhall/types.dhall

in    prelude.defaults.Package
    ⫽ { name =
          "test"
      , version =
          prelude.v "0"
      , library =
          [   λ(config : types.Config)
            →   prelude.defaults.Library
              ⫽ { default-language =
                    [ prelude.types.Languages.Haskell2010 {=} ] : Optional
                                                                  types.Language
                , exposed-modules =
                    [ "Foo" ]
                }
          ] : Optional (types.Config → types.Library)
      }