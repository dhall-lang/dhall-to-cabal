let prelude = ./../../dhall/prelude.dhall

let types = ./../../dhall/types.dhall

in    prelude.defaults.Package
    ⫽ { name =
          "Name"
      , version =
          prelude.v "1"
      , cabal-version =
          prelude.v "2.0"
      , library =
          Some
          (   λ(config : types.Config)
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
                            }
                    
                    else    prelude.defaults.Library
                          ⫽ { build-depends =
                                [ { bounds = prelude.anyVersion, package = "A" }
                                , { bounds = prelude.anyVersion, package = "B" }
                                ]
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
                      }
              
              else    prelude.defaults.Library
                    ⫽ { build-depends =
                          [ { bounds = prelude.anyVersion, package = "A" } ]
                      }
          )
      , license =
          prelude.types.Licenses.Unspecified {=}
      }
