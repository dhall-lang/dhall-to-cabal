let prelude = ./../../dhall/prelude.dhall

let types = ./../../dhall/types.dhall

in    prelude.defaults.Package
    ⫽ { cabal-version =
          prelude.v "3.0"
      , library =
          Some
          (   λ(config : types.Config)
            →   prelude.defaults.MainLibrary
              ⫽ { default-language = Some types.Language.Haskell2010 }
          )
      , name =
          "multilib"
      , sub-libraries =
          [ { library =
                  λ(config : types.Config)
                →   prelude.defaults.NamedLibrary
                  ⫽ { default-language =
                        Some types.Language.Haskell2010
                    , visibility =
                        types.LibraryVisibility.public
                    }
            , name =
                "x"
            }
          , { library =
                  λ(config : types.Config)
                →   prelude.defaults.NamedLibrary
                  ⫽ { default-language = Some types.Language.Haskell2010 }
            , name =
                "y"
            }
          ]
      , version =
          prelude.v "0"
      }