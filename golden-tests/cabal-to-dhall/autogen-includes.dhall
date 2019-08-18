let prelude = ./../../dhall/prelude.dhall

let types = ./../../dhall/types.dhall

in    prelude.defaults.Package
    ⫽ { cabal-version =
          prelude.v "3.0"
      , library =
          Some
          (   λ(config : types.Config)
            →   prelude.defaults.MainLibrary
              ⫽ { autogen-includes =
                    [ "foo", "bar" ]
                , default-language =
                    Some types.Language.Haskell2010
                }
          )
      , name =
          "foo"
      , version =
          prelude.v "0"
      }