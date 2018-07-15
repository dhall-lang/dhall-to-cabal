    let prelude = ./../../dhall/prelude.dhall

in  let types = ./../../dhall/types.dhall

in    prelude.defaults.Package
    ⫽ { name =
          "Name"
      , version =
          prelude.v "1"
      , cabal-version =
          prelude.v "2.0"
      , library =
          [   λ(config : types.Config)
            →       if config.impl
                       (prelude.types.Compilers.GHC {=})
                       ( prelude.unionVersionRanges
                         (prelude.thisVersion (prelude.v "8.2"))
                         (prelude.laterVersion (prelude.v "8.2"))
                       )
              
              then        if config.impl
                             (prelude.types.Compilers.GHC {=})
                             ( prelude.unionVersionRanges
                               (prelude.thisVersion (prelude.v "8.4"))
                               (prelude.laterVersion (prelude.v "8.4"))
                             )
                    
                    then    prelude.defaults.Library
                          ⫽ { build-depends =
                                [ { bounds = prelude.anyVersion, package = "A" }
                                , { bounds = prelude.anyVersion, package = "B" }
                                , { bounds = prelude.anyVersion, package = "C" }
                                ]
                            , default-language =
                                [] : Optional types.Language
                            }
                    
                    else    prelude.defaults.Library
                          ⫽ { build-depends =
                                [ { bounds = prelude.anyVersion, package = "A" }
                                , { bounds = prelude.anyVersion, package = "B" }
                                ]
                            , default-language =
                                [] : Optional types.Language
                            }
              
              else  if config.impl
                       (prelude.types.Compilers.GHC {=})
                       ( prelude.unionVersionRanges
                         (prelude.thisVersion (prelude.v "8.4"))
                         (prelude.laterVersion (prelude.v "8.4"))
                       )
              
              then    prelude.defaults.Library
                    ⫽ { build-depends =
                          [ { bounds = prelude.anyVersion, package = "A" }
                          , { bounds = prelude.anyVersion, package = "C" }
                          ]
                      , default-language =
                          [] : Optional types.Language
                      }
              
              else    prelude.defaults.Library
                    ⫽ { build-depends =
                          [ { bounds = prelude.anyVersion, package = "A" } ]
                      , default-language =
                          [] : Optional types.Language
                      }
          ] : Optional (types.Config → types.Library)
      , license =
          prelude.types.Licenses.Unspecified {=}
      }