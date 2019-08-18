let prelude = ./../../dhall/prelude.dhall

let types = ./../../dhall/types.dhall

in    prelude.defaults.Package
    ⫽ { cabal-version =
          prelude.v "2.0"
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
      , library =
          Some
          (   λ(config : types.Config)
            →       if config.impl
                         types.Compiler.GHC
                         ( prelude.unionVersionRanges
                             (prelude.thisVersion (prelude.v "0.0.9.7"))
                             (prelude.laterVersion (prelude.v "0.0.9.7"))
                         )
              
              then        if config.impl
                               types.Compiler.GHC
                               (prelude.earlierVersion (prelude.v "0.7.0.2"))
                    
                    then        if config.impl
                                     types.Compiler.GHC
                                     ( prelude.unionVersionRanges
                                         ( prelude.thisVersion
                                             (prelude.v "0.0.9")
                                         )
                                         ( prelude.laterVersion
                                             (prelude.v "0.0.9")
                                         )
                                     )
                          
                          then        if config.flag "wai-servlet-debug"
                                
                                then    prelude.defaults.MainLibrary
                                      ⫽ { c-sources =
                                            [ "java/Utils.java" ]
                                        , cpp-options =
                                            [ "-DINTEROP"
                                            , "-DPURE_JAVA_WITH"
                                            , "-DWAI_SERVLET_DEBUG"
                                            ]
                                        }
                                
                                else    prelude.defaults.MainLibrary
                                      ⫽ { c-sources =
                                            [ "java/Utils.java" ]
                                        , cpp-options =
                                            [ "-DINTEROP", "-DPURE_JAVA_WITH" ]
                                        }
                          
                          else  if config.flag "wai-servlet-debug"
                          
                          then    prelude.defaults.MainLibrary
                                ⫽ { cpp-options =
                                      [ "-DINTEROP"
                                      , "-DPURE_JAVA_WITH"
                                      , "-DWAI_SERVLET_DEBUG"
                                      ]
                                  }
                          
                          else    prelude.defaults.MainLibrary
                                ⫽ { cpp-options =
                                      [ "-DINTEROP", "-DPURE_JAVA_WITH" ]
                                  }
                    
                    else  if config.impl
                               types.Compiler.GHC
                               ( prelude.unionVersionRanges
                                   (prelude.thisVersion (prelude.v "0.0.9"))
                                   (prelude.laterVersion (prelude.v "0.0.9"))
                               )
                    
                    then        if config.flag "wai-servlet-debug"
                          
                          then    prelude.defaults.MainLibrary
                                ⫽ { c-sources =
                                      [ "java/Utils.java" ]
                                  , cpp-options =
                                      [ "-DINTEROP", "-DWAI_SERVLET_DEBUG" ]
                                  }
                          
                          else    prelude.defaults.MainLibrary
                                ⫽ { c-sources =
                                      [ "java/Utils.java" ]
                                  , cpp-options =
                                      [ "-DINTEROP" ]
                                  }
                    
                    else  if config.flag "wai-servlet-debug"
                    
                    then    prelude.defaults.MainLibrary
                          ⫽ { cpp-options =
                                [ "-DINTEROP", "-DWAI_SERVLET_DEBUG" ]
                            }
                    
                    else    prelude.defaults.MainLibrary
                          ⫽ { cpp-options = [ "-DINTEROP" ] }
              
              else  if config.impl
                         types.Compiler.GHC
                         (prelude.earlierVersion (prelude.v "0.7.0.2"))
              
              then        if config.impl
                               types.Compiler.GHC
                               ( prelude.unionVersionRanges
                                   (prelude.thisVersion (prelude.v "0.0.9"))
                                   (prelude.laterVersion (prelude.v "0.0.9"))
                               )
                    
                    then        if config.flag "wai-servlet-debug"
                          
                          then    prelude.defaults.MainLibrary
                                ⫽ { c-sources =
                                      [ "java/Utils.java" ]
                                  , cpp-options =
                                      [ "-DPURE_JAVA_WITH"
                                      , "-DWAI_SERVLET_DEBUG"
                                      ]
                                  }
                          
                          else    prelude.defaults.MainLibrary
                                ⫽ { c-sources =
                                      [ "java/Utils.java" ]
                                  , cpp-options =
                                      [ "-DPURE_JAVA_WITH" ]
                                  }
                    
                    else  if config.flag "wai-servlet-debug"
                    
                    then    prelude.defaults.MainLibrary
                          ⫽ { cpp-options =
                                [ "-DPURE_JAVA_WITH", "-DWAI_SERVLET_DEBUG" ]
                            }
                    
                    else    prelude.defaults.MainLibrary
                          ⫽ { cpp-options = [ "-DPURE_JAVA_WITH" ] }
              
              else  if config.impl
                         types.Compiler.GHC
                         ( prelude.unionVersionRanges
                             (prelude.thisVersion (prelude.v "0.0.9"))
                             (prelude.laterVersion (prelude.v "0.0.9"))
                         )
              
              then        if config.flag "wai-servlet-debug"
                    
                    then    prelude.defaults.MainLibrary
                          ⫽ { c-sources =
                                [ "java/Utils.java" ]
                            , cpp-options =
                                [ "-DWAI_SERVLET_DEBUG" ]
                            }
                    
                    else    prelude.defaults.MainLibrary
                          ⫽ { c-sources = [ "java/Utils.java" ] }
              
              else  if config.flag "wai-servlet-debug"
              
              then    prelude.defaults.MainLibrary
                    ⫽ { cpp-options = [ "-DWAI_SERVLET_DEBUG" ] }
              
              else  prelude.defaults.MainLibrary
          )
      , name =
          "wai-servlet"
      , version =
          prelude.v "0.1.5.0"
      }