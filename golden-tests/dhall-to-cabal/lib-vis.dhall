let prelude = ./../../dhall/prelude.dhall

let types = ./../../dhall/types.dhall

in    prelude.defaults.Package
    ⫽ { name =
          "multilib"
      , version =
          prelude.v "0"
      , cabal-version =
          prelude.v "3.0"
      , library =
          Some
          (   λ(config : types.Config)
            →   prelude.defaults.MainLibrary
              ⫽ { default-language =
                    Some types.Language.Haskell2010
                }
          )
      , sub-libraries =
          [ { library =
                  λ(config : types.Config)
                →   prelude.defaults.NamedLibrary
                  ⫽ { visibility =
                        types.LibraryVisibility.public
                    , default-language =
                        Some types.Language.Haskell2010
                    }
            , name =
                "x"
            }
          , { library =
                  λ(config : types.Config)
                →   prelude.defaults.NamedLibrary
                  ⫽ { default-language =
                        Some types.Language.Haskell2010
                    }
            , name =
                "y"
            }
          ]
      }