    let prelude = ./../../dhall/prelude.dhall

in  let types = ./../../dhall/types.dhall

in    prelude.defaults.Package
    ⫽ { name =
          "wai-servlet"
      , version =
          prelude.v "0.1.5.0"
      , flags =
          [ { default =
                False
            , description =
                "print debug output. not suitable for production"
            , manual =
                False
            , name =
                "wai-servlet-debug"
            }
          ]
      , cabal-version =
          prelude.v "2.0"
      , library =
          Some
          (   λ(config : types.Config)
            →       if config.impl
                       (prelude.types.Compilers.GHC {=})
                       ( prelude.unionVersionRanges
                         (prelude.thisVersion (prelude.v "0.0.9.7"))
                         (prelude.laterVersion (prelude.v "0.0.9.7"))
                       )
              
              then        if config.impl
                             (prelude.types.Compilers.GHC {=})
                             (prelude.earlierVersion (prelude.v "0.7.0.2"))
                    
                    then        if config.impl
                                   (prelude.types.Compilers.GHC {=})
                                   ( prelude.unionVersionRanges
                                     (prelude.thisVersion (prelude.v "0.0.9"))
                                     (prelude.laterVersion (prelude.v "0.0.9"))
                                   )
                          
                          then        if config.flag "wai-servlet-debug"
                                
                                then    prelude.defaults.Library
                                      ⫽ { c-sources =
                                            [ "java/Utils.java" ]
                                        , cpp-options =
                                            [ "-DINTEROP"
                                            , "-DPURE_JAVA_WITH"
                                            , "-DWAI_SERVLET_DEBUG"
                                            ]
                                        }
                                
                                else    prelude.defaults.Library
                                      ⫽ { c-sources =
                                            [ "java/Utils.java" ]
                                        , cpp-options =
                                            [ "-DINTEROP", "-DPURE_JAVA_WITH" ]
                                        }
                          
                          else  if config.flag "wai-servlet-debug"
                          
                          then    prelude.defaults.Library
                                ⫽ { cpp-options =
                                      [ "-DINTEROP"
                                      , "-DPURE_JAVA_WITH"
                                      , "-DWAI_SERVLET_DEBUG"
                                      ]
                                  }
                          
                          else    prelude.defaults.Library
                                ⫽ { cpp-options =
                                      [ "-DINTEROP", "-DPURE_JAVA_WITH" ]
                                  }
                    
                    else  if config.impl
                             (prelude.types.Compilers.GHC {=})
                             ( prelude.unionVersionRanges
                               (prelude.thisVersion (prelude.v "0.0.9"))
                               (prelude.laterVersion (prelude.v "0.0.9"))
                             )
                    
                    then        if config.flag "wai-servlet-debug"
                          
                          then    prelude.defaults.Library
                                ⫽ { c-sources =
                                      [ "java/Utils.java" ]
                                  , cpp-options =
                                      [ "-DINTEROP", "-DWAI_SERVLET_DEBUG" ]
                                  }
                          
                          else    prelude.defaults.Library
                                ⫽ { c-sources =
                                      [ "java/Utils.java" ]
                                  , cpp-options =
                                      [ "-DINTEROP" ]
                                  }
                    
                    else  if config.flag "wai-servlet-debug"
                    
                    then    prelude.defaults.Library
                          ⫽ { cpp-options =
                                [ "-DINTEROP", "-DWAI_SERVLET_DEBUG" ]
                            }
                    
                    else    prelude.defaults.Library
                          ⫽ { cpp-options = [ "-DINTEROP" ] }
              
              else  if config.impl
                       (prelude.types.Compilers.GHC {=})
                       (prelude.earlierVersion (prelude.v "0.7.0.2"))
              
              then        if config.impl
                             (prelude.types.Compilers.GHC {=})
                             ( prelude.unionVersionRanges
                               (prelude.thisVersion (prelude.v "0.0.9"))
                               (prelude.laterVersion (prelude.v "0.0.9"))
                             )
                    
                    then        if config.flag "wai-servlet-debug"
                          
                          then    prelude.defaults.Library
                                ⫽ { c-sources =
                                      [ "java/Utils.java" ]
                                  , cpp-options =
                                      [ "-DPURE_JAVA_WITH"
                                      , "-DWAI_SERVLET_DEBUG"
                                      ]
                                  }
                          
                          else    prelude.defaults.Library
                                ⫽ { c-sources =
                                      [ "java/Utils.java" ]
                                  , cpp-options =
                                      [ "-DPURE_JAVA_WITH" ]
                                  }
                    
                    else  if config.flag "wai-servlet-debug"
                    
                    then    prelude.defaults.Library
                          ⫽ { cpp-options =
                                [ "-DPURE_JAVA_WITH", "-DWAI_SERVLET_DEBUG" ]
                            }
                    
                    else    prelude.defaults.Library
                          ⫽ { cpp-options = [ "-DPURE_JAVA_WITH" ] }
              
              else  if config.impl
                       (prelude.types.Compilers.GHC {=})
                       ( prelude.unionVersionRanges
                         (prelude.thisVersion (prelude.v "0.0.9"))
                         (prelude.laterVersion (prelude.v "0.0.9"))
                       )
              
              then        if config.flag "wai-servlet-debug"
                    
                    then    prelude.defaults.Library
                          ⫽ { c-sources =
                                [ "java/Utils.java" ]
                            , cpp-options =
                                [ "-DWAI_SERVLET_DEBUG" ]
                            }
                    
                    else    prelude.defaults.Library
                          ⫽ { c-sources = [ "java/Utils.java" ] }
              
              else  if config.flag "wai-servlet-debug"
              
              then    prelude.defaults.Library
                    ⫽ { cpp-options = [ "-DWAI_SERVLET_DEBUG" ] }
              
              else  prelude.defaults.Library
          )
      }