let prelude = ./../../dhall/prelude.dhall

let types = ./../../dhall/types.dhall

in    prelude.defaults.Package
    ⫽ { library =
          Some
          (   λ(config : types.Config)
            →   prelude.defaults.MainLibrary
              ⫽ { mixins =
                    [ { package =
                          "foo"
                      , renaming =
                          { provides =
                              types.ModuleRenaming.default
                          , requires =
                              types.ModuleRenaming.default
                          }
                      }
                    , { package =
                          "bar"
                      , renaming =
                          { provides =
                              types.ModuleRenaming.renaming
                                [ { rename = "Some.Module", to = "Some.Module" }
                                , { rename =
                                      "Some.Other.Module"
                                  , to =
                                      "Some.Other.Module"
                                  }
                                , { rename = "Third.Module", to = "Renamed" }
                                ]
                          , requires =
                              types.ModuleRenaming.default
                          }
                      }
                    , { package =
                          "baz"
                      , renaming =
                          { provides =
                              types.ModuleRenaming.hiding
                                [ "Hidden", "Also.Hidden" ]
                          , requires =
                              types.ModuleRenaming.default
                          }
                      }
                    ]
                }
          )
      , name =
          "mixins-test"
      , version =
          prelude.v "0"
      }