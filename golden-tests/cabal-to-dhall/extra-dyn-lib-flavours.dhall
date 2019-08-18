let prelude = ./../../dhall/prelude.dhall

let types = ./../../dhall/types.dhall

in    prelude.defaults.Package
    ⫽ { cabal-version =
          prelude.v "3.0"
      , library =
          Some
          (   λ(config : types.Config)
            →   prelude.defaults.MainLibrary
              ⫽ { default-language =
                    Some types.Language.Haskell2010
                , extra-dyn-lib-flavours =
                    [ "foo", "bar" ]
                }
          )
      , name =
          "foo"
      , version =
          prelude.v "0"
      }