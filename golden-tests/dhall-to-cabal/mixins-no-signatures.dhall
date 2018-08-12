    let prelude = ./../../dhall/prelude.dhall

in  let types = ./../../dhall/types.dhall

in    prelude.defaults.Package
    ⫽ { name =
          "mixins-test"
      , version =
          prelude.v "0"
      , library =
          [   λ(config : types.Config)
            →   prelude.defaults.Library
              ⫽ { mixins =
                    [ { package =
                          "foo"
                      , renaming =
                          { provides =
                              prelude.types.ModuleRenaming.default {=}
                          , requires =
                              prelude.types.ModuleRenaming.default {=}
                          }
                      }
                    , { package =
                          "bar"
                      , renaming =
                          { provides =
                              prelude.types.ModuleRenaming.renaming
                              [ { rename = "Some.Module", to = "Some.Module" }
                              , { rename =
                                    "Some.Other.Module"
                                , to =
                                    "Some.Other.Module"
                                }
                              , { rename = "Third.Module", to = "Renamed" }
                              ]
                          , requires =
                              prelude.types.ModuleRenaming.default {=}
                          }
                      }
                    , { package =
                          "baz"
                      , renaming =
                          { provides =
                              prelude.types.ModuleRenaming.hiding
                              [ "Hidden", "Also.Hidden" ]
                          , requires =
                              prelude.types.ModuleRenaming.default {=}
                          }
                      }
                    ]
                }
          ] : Optional (types.Config → types.Library)
      }