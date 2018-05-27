    let prelude =
          https://raw.githubusercontent.com/dhall-lang/dhall-to-cabal/1.0.0/dhall/prelude.dhall 

in  let types =
          https://raw.githubusercontent.com/dhall-lang/dhall-to-cabal/1.0.0/dhall/types.dhall 

in  { author =
        ""
    , benchmarks =
        [] : List { benchmark : types.Config → types.Benchmark, name : Text }
    , bug-reports =
        ""
    , build-type =
        [] : Optional types.BuildType
    , cabal-version =
        prelude.v "2.0"
    , category =
        ""
    , copyright =
        ""
    , custom-setup =
        [] : Optional types.CustomSetup
    , data-dir =
        ""
    , data-files =
        [] : List Text
    , description =
        ""
    , executables =
        [] : List { executable : types.Config → types.Executable, name : Text }
    , extra-doc-files =
        [] : List Text
    , extra-source-files =
        [] : List Text
    , extra-tmp-files =
        [] : List Text
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
    , foreign-libraries =
        [] : List
             { foreign-lib : types.Config → types.ForeignLibrary, name : Text }
    , homepage =
        ""
    , library =
        [   λ ( config
              : types.Config
              )
          →       if    config.impl
                        (prelude.types.Compilers.GHC {=})
                        (prelude.orLaterVersion (prelude.v "0.0.9.7"))
                  then        if    config.impl
                                    (prelude.types.Compilers.GHC {=})
                                    ( prelude.earlierVersion
                                      (prelude.v "0.7.0.2")
                                    )
                              then        if    config.impl
                                                ( prelude.types.Compilers.GHC
                                                  {=}
                                                )
                                                ( prelude.orLaterVersion
                                                  (prelude.v "0.0.9")
                                                )
                                          then        if    config.flag
                                                            "wai-servlet-debug"
                                                      then  { autogen-modules =
                                                                [] : List Text
                                                            , build-depends =
                                                                [] : List
                                                                     { bounds :
                                                                         types.VersionRange
                                                                     , package :
                                                                         Text
                                                                     }
                                                            , build-tool-depends =
                                                                [] : List
                                                                     { component :
                                                                         Text
                                                                     , package :
                                                                         Text
                                                                     , version :
                                                                         types.VersionRange
                                                                     }
                                                            , build-tools =
                                                                [] : List
                                                                     { exe :
                                                                         Text
                                                                     , version :
                                                                         types.VersionRange
                                                                     }
                                                            , buildable =
                                                                True
                                                            , c-sources =
                                                                [ "java/Utils.java"
                                                                ]
                                                            , cc-options =
                                                                [] : List Text
                                                            , compiler-options =
                                                                prelude.defaults.CompilerOptions
                                                            , cpp-options =
                                                                [ "-DINTEROP"
                                                                , "-DPURE_JAVA_WITH"
                                                                , "-DWAI_SERVLET_DEBUG"
                                                                ]
                                                            , default-extensions =
                                                                [] : List
                                                                     types.Extension
                                                            , default-language =
                                                                [] : Optional
                                                                     types.Language
                                                            , exposed-modules =
                                                                [] : List Text
                                                            , extra-framework-dirs =
                                                                [] : List Text
                                                            , extra-ghci-libraries =
                                                                [] : List Text
                                                            , extra-lib-dirs =
                                                                [] : List Text
                                                            , extra-libraries =
                                                                [] : List Text
                                                            , frameworks =
                                                                [] : List Text
                                                            , hs-source-dirs =
                                                                [] : List Text
                                                            , include-dirs =
                                                                [] : List Text
                                                            , includes =
                                                                [] : List Text
                                                            , install-includes =
                                                                [] : List Text
                                                            , js-sources =
                                                                [] : List Text
                                                            , ld-options =
                                                                [] : List Text
                                                            , mixins =
                                                                [] : List
                                                                     types.Mixin
                                                            , other-extensions =
                                                                [] : List
                                                                     types.Extension
                                                            , other-languages =
                                                                [] : List
                                                                     types.Language
                                                            , other-modules =
                                                                [] : List Text
                                                            , pkgconfig-depends =
                                                                [] : List
                                                                     { name :
                                                                         Text
                                                                     , version :
                                                                         types.VersionRange
                                                                     }
                                                            , profiling-options =
                                                                prelude.defaults.CompilerOptions
                                                            , reexported-modules =
                                                                [] : List
                                                                     { name :
                                                                         Text
                                                                     , original :
                                                                         { name :
                                                                             Text
                                                                         , package :
                                                                             Optional
                                                                             Text
                                                                         }
                                                                     }
                                                            , shared-options =
                                                                prelude.defaults.CompilerOptions
                                                            , signatures =
                                                                [] : List Text
                                                            }
                                                
                                                else  { autogen-modules =
                                                          [] : List Text
                                                      , build-depends =
                                                          [] : List
                                                               { bounds :
                                                                   types.VersionRange
                                                               , package :
                                                                   Text
                                                               }
                                                      , build-tool-depends =
                                                          [] : List
                                                               { component :
                                                                   Text
                                                               , package :
                                                                   Text
                                                               , version :
                                                                   types.VersionRange
                                                               }
                                                      , build-tools =
                                                          [] : List
                                                               { exe :
                                                                   Text
                                                               , version :
                                                                   types.VersionRange
                                                               }
                                                      , buildable =
                                                          True
                                                      , c-sources =
                                                          [ "java/Utils.java" ]
                                                      , cc-options =
                                                          [] : List Text
                                                      , compiler-options =
                                                          prelude.defaults.CompilerOptions
                                                      , cpp-options =
                                                          [ "-DINTEROP"
                                                          , "-DPURE_JAVA_WITH"
                                                          ]
                                                      , default-extensions =
                                                          [] : List
                                                               types.Extension
                                                      , default-language =
                                                          [] : Optional
                                                               types.Language
                                                      , exposed-modules =
                                                          [] : List Text
                                                      , extra-framework-dirs =
                                                          [] : List Text
                                                      , extra-ghci-libraries =
                                                          [] : List Text
                                                      , extra-lib-dirs =
                                                          [] : List Text
                                                      , extra-libraries =
                                                          [] : List Text
                                                      , frameworks =
                                                          [] : List Text
                                                      , hs-source-dirs =
                                                          [] : List Text
                                                      , include-dirs =
                                                          [] : List Text
                                                      , includes =
                                                          [] : List Text
                                                      , install-includes =
                                                          [] : List Text
                                                      , js-sources =
                                                          [] : List Text
                                                      , ld-options =
                                                          [] : List Text
                                                      , mixins =
                                                          [] : List types.Mixin
                                                      , other-extensions =
                                                          [] : List
                                                               types.Extension
                                                      , other-languages =
                                                          [] : List
                                                               types.Language
                                                      , other-modules =
                                                          [] : List Text
                                                      , pkgconfig-depends =
                                                          [] : List
                                                               { name :
                                                                   Text
                                                               , version :
                                                                   types.VersionRange
                                                               }
                                                      , profiling-options =
                                                          prelude.defaults.CompilerOptions
                                                      , reexported-modules =
                                                          [] : List
                                                               { name :
                                                                   Text
                                                               , original :
                                                                   { name :
                                                                       Text
                                                                   , package :
                                                                       Optional
                                                                       Text
                                                                   }
                                                               }
                                                      , shared-options =
                                                          prelude.defaults.CompilerOptions
                                                      , signatures =
                                                          [] : List Text
                                                      }
                                    
                                    else  if    config.flag
                                                "wai-servlet-debug"
                                          then  { autogen-modules =
                                                    [] : List Text
                                                , build-depends =
                                                    [] : List
                                                         { bounds :
                                                             types.VersionRange
                                                         , package :
                                                             Text
                                                         }
                                                , build-tool-depends =
                                                    [] : List
                                                         { component :
                                                             Text
                                                         , package :
                                                             Text
                                                         , version :
                                                             types.VersionRange
                                                         }
                                                , build-tools =
                                                    [] : List
                                                         { exe :
                                                             Text
                                                         , version :
                                                             types.VersionRange
                                                         }
                                                , buildable =
                                                    True
                                                , c-sources =
                                                    [] : List Text
                                                , cc-options =
                                                    [] : List Text
                                                , compiler-options =
                                                    prelude.defaults.CompilerOptions
                                                , cpp-options =
                                                    [ "-DINTEROP"
                                                    , "-DPURE_JAVA_WITH"
                                                    , "-DWAI_SERVLET_DEBUG"
                                                    ]
                                                , default-extensions =
                                                    [] : List types.Extension
                                                , default-language =
                                                    [] : Optional types.Language
                                                , exposed-modules =
                                                    [] : List Text
                                                , extra-framework-dirs =
                                                    [] : List Text
                                                , extra-ghci-libraries =
                                                    [] : List Text
                                                , extra-lib-dirs =
                                                    [] : List Text
                                                , extra-libraries =
                                                    [] : List Text
                                                , frameworks =
                                                    [] : List Text
                                                , hs-source-dirs =
                                                    [] : List Text
                                                , include-dirs =
                                                    [] : List Text
                                                , includes =
                                                    [] : List Text
                                                , install-includes =
                                                    [] : List Text
                                                , js-sources =
                                                    [] : List Text
                                                , ld-options =
                                                    [] : List Text
                                                , mixins =
                                                    [] : List types.Mixin
                                                , other-extensions =
                                                    [] : List types.Extension
                                                , other-languages =
                                                    [] : List types.Language
                                                , other-modules =
                                                    [] : List Text
                                                , pkgconfig-depends =
                                                    [] : List
                                                         { name :
                                                             Text
                                                         , version :
                                                             types.VersionRange
                                                         }
                                                , profiling-options =
                                                    prelude.defaults.CompilerOptions
                                                , reexported-modules =
                                                    [] : List
                                                         { name :
                                                             Text
                                                         , original :
                                                             { name :
                                                                 Text
                                                             , package :
                                                                 Optional Text
                                                             }
                                                         }
                                                , shared-options =
                                                    prelude.defaults.CompilerOptions
                                                , signatures =
                                                    [] : List Text
                                                }
                                    
                                    else  { autogen-modules =
                                              [] : List Text
                                          , build-depends =
                                              [] : List
                                                   { bounds :
                                                       types.VersionRange
                                                   , package :
                                                       Text
                                                   }
                                          , build-tool-depends =
                                              [] : List
                                                   { component :
                                                       Text
                                                   , package :
                                                       Text
                                                   , version :
                                                       types.VersionRange
                                                   }
                                          , build-tools =
                                              [] : List
                                                   { exe :
                                                       Text
                                                   , version :
                                                       types.VersionRange
                                                   }
                                          , buildable =
                                              True
                                          , c-sources =
                                              [] : List Text
                                          , cc-options =
                                              [] : List Text
                                          , compiler-options =
                                              prelude.defaults.CompilerOptions
                                          , cpp-options =
                                              [ "-DINTEROP"
                                              , "-DPURE_JAVA_WITH"
                                              ]
                                          , default-extensions =
                                              [] : List types.Extension
                                          , default-language =
                                              [] : Optional types.Language
                                          , exposed-modules =
                                              [] : List Text
                                          , extra-framework-dirs =
                                              [] : List Text
                                          , extra-ghci-libraries =
                                              [] : List Text
                                          , extra-lib-dirs =
                                              [] : List Text
                                          , extra-libraries =
                                              [] : List Text
                                          , frameworks =
                                              [] : List Text
                                          , hs-source-dirs =
                                              [] : List Text
                                          , include-dirs =
                                              [] : List Text
                                          , includes =
                                              [] : List Text
                                          , install-includes =
                                              [] : List Text
                                          , js-sources =
                                              [] : List Text
                                          , ld-options =
                                              [] : List Text
                                          , mixins =
                                              [] : List types.Mixin
                                          , other-extensions =
                                              [] : List types.Extension
                                          , other-languages =
                                              [] : List types.Language
                                          , other-modules =
                                              [] : List Text
                                          , pkgconfig-depends =
                                              [] : List
                                                   { name :
                                                       Text
                                                   , version :
                                                       types.VersionRange
                                                   }
                                          , profiling-options =
                                              prelude.defaults.CompilerOptions
                                          , reexported-modules =
                                              [] : List
                                                   { name :
                                                       Text
                                                   , original :
                                                       { name :
                                                           Text
                                                       , package :
                                                           Optional Text
                                                       }
                                                   }
                                          , shared-options =
                                              prelude.defaults.CompilerOptions
                                          , signatures =
                                              [] : List Text
                                          }
                        
                        else  if    config.impl
                                    (prelude.types.Compilers.GHC {=})
                                    (prelude.orLaterVersion (prelude.v "0.0.9"))
                              then        if    config.flag
                                                "wai-servlet-debug"
                                          then  { autogen-modules =
                                                    [] : List Text
                                                , build-depends =
                                                    [] : List
                                                         { bounds :
                                                             types.VersionRange
                                                         , package :
                                                             Text
                                                         }
                                                , build-tool-depends =
                                                    [] : List
                                                         { component :
                                                             Text
                                                         , package :
                                                             Text
                                                         , version :
                                                             types.VersionRange
                                                         }
                                                , build-tools =
                                                    [] : List
                                                         { exe :
                                                             Text
                                                         , version :
                                                             types.VersionRange
                                                         }
                                                , buildable =
                                                    True
                                                , c-sources =
                                                    [ "java/Utils.java" ]
                                                , cc-options =
                                                    [] : List Text
                                                , compiler-options =
                                                    prelude.defaults.CompilerOptions
                                                , cpp-options =
                                                    [ "-DINTEROP"
                                                    , "-DWAI_SERVLET_DEBUG"
                                                    ]
                                                , default-extensions =
                                                    [] : List types.Extension
                                                , default-language =
                                                    [] : Optional types.Language
                                                , exposed-modules =
                                                    [] : List Text
                                                , extra-framework-dirs =
                                                    [] : List Text
                                                , extra-ghci-libraries =
                                                    [] : List Text
                                                , extra-lib-dirs =
                                                    [] : List Text
                                                , extra-libraries =
                                                    [] : List Text
                                                , frameworks =
                                                    [] : List Text
                                                , hs-source-dirs =
                                                    [] : List Text
                                                , include-dirs =
                                                    [] : List Text
                                                , includes =
                                                    [] : List Text
                                                , install-includes =
                                                    [] : List Text
                                                , js-sources =
                                                    [] : List Text
                                                , ld-options =
                                                    [] : List Text
                                                , mixins =
                                                    [] : List types.Mixin
                                                , other-extensions =
                                                    [] : List types.Extension
                                                , other-languages =
                                                    [] : List types.Language
                                                , other-modules =
                                                    [] : List Text
                                                , pkgconfig-depends =
                                                    [] : List
                                                         { name :
                                                             Text
                                                         , version :
                                                             types.VersionRange
                                                         }
                                                , profiling-options =
                                                    prelude.defaults.CompilerOptions
                                                , reexported-modules =
                                                    [] : List
                                                         { name :
                                                             Text
                                                         , original :
                                                             { name :
                                                                 Text
                                                             , package :
                                                                 Optional Text
                                                             }
                                                         }
                                                , shared-options =
                                                    prelude.defaults.CompilerOptions
                                                , signatures =
                                                    [] : List Text
                                                }
                                    
                                    else  { autogen-modules =
                                              [] : List Text
                                          , build-depends =
                                              [] : List
                                                   { bounds :
                                                       types.VersionRange
                                                   , package :
                                                       Text
                                                   }
                                          , build-tool-depends =
                                              [] : List
                                                   { component :
                                                       Text
                                                   , package :
                                                       Text
                                                   , version :
                                                       types.VersionRange
                                                   }
                                          , build-tools =
                                              [] : List
                                                   { exe :
                                                       Text
                                                   , version :
                                                       types.VersionRange
                                                   }
                                          , buildable =
                                              True
                                          , c-sources =
                                              [ "java/Utils.java" ]
                                          , cc-options =
                                              [] : List Text
                                          , compiler-options =
                                              prelude.defaults.CompilerOptions
                                          , cpp-options =
                                              [ "-DINTEROP" ]
                                          , default-extensions =
                                              [] : List types.Extension
                                          , default-language =
                                              [] : Optional types.Language
                                          , exposed-modules =
                                              [] : List Text
                                          , extra-framework-dirs =
                                              [] : List Text
                                          , extra-ghci-libraries =
                                              [] : List Text
                                          , extra-lib-dirs =
                                              [] : List Text
                                          , extra-libraries =
                                              [] : List Text
                                          , frameworks =
                                              [] : List Text
                                          , hs-source-dirs =
                                              [] : List Text
                                          , include-dirs =
                                              [] : List Text
                                          , includes =
                                              [] : List Text
                                          , install-includes =
                                              [] : List Text
                                          , js-sources =
                                              [] : List Text
                                          , ld-options =
                                              [] : List Text
                                          , mixins =
                                              [] : List types.Mixin
                                          , other-extensions =
                                              [] : List types.Extension
                                          , other-languages =
                                              [] : List types.Language
                                          , other-modules =
                                              [] : List Text
                                          , pkgconfig-depends =
                                              [] : List
                                                   { name :
                                                       Text
                                                   , version :
                                                       types.VersionRange
                                                   }
                                          , profiling-options =
                                              prelude.defaults.CompilerOptions
                                          , reexported-modules =
                                              [] : List
                                                   { name :
                                                       Text
                                                   , original :
                                                       { name :
                                                           Text
                                                       , package :
                                                           Optional Text
                                                       }
                                                   }
                                          , shared-options =
                                              prelude.defaults.CompilerOptions
                                          , signatures =
                                              [] : List Text
                                          }
                        
                        else  if    config.flag "wai-servlet-debug"
                              then  { autogen-modules =
                                        [] : List Text
                                    , build-depends =
                                        [] : List
                                             { bounds :
                                                 types.VersionRange
                                             , package :
                                                 Text
                                             }
                                    , build-tool-depends =
                                        [] : List
                                             { component :
                                                 Text
                                             , package :
                                                 Text
                                             , version :
                                                 types.VersionRange
                                             }
                                    , build-tools =
                                        [] : List
                                             { exe :
                                                 Text
                                             , version :
                                                 types.VersionRange
                                             }
                                    , buildable =
                                        True
                                    , c-sources =
                                        [] : List Text
                                    , cc-options =
                                        [] : List Text
                                    , compiler-options =
                                        prelude.defaults.CompilerOptions
                                    , cpp-options =
                                        [ "-DINTEROP", "-DWAI_SERVLET_DEBUG" ]
                                    , default-extensions =
                                        [] : List types.Extension
                                    , default-language =
                                        [] : Optional types.Language
                                    , exposed-modules =
                                        [] : List Text
                                    , extra-framework-dirs =
                                        [] : List Text
                                    , extra-ghci-libraries =
                                        [] : List Text
                                    , extra-lib-dirs =
                                        [] : List Text
                                    , extra-libraries =
                                        [] : List Text
                                    , frameworks =
                                        [] : List Text
                                    , hs-source-dirs =
                                        [] : List Text
                                    , include-dirs =
                                        [] : List Text
                                    , includes =
                                        [] : List Text
                                    , install-includes =
                                        [] : List Text
                                    , js-sources =
                                        [] : List Text
                                    , ld-options =
                                        [] : List Text
                                    , mixins =
                                        [] : List types.Mixin
                                    , other-extensions =
                                        [] : List types.Extension
                                    , other-languages =
                                        [] : List types.Language
                                    , other-modules =
                                        [] : List Text
                                    , pkgconfig-depends =
                                        [] : List
                                             { name :
                                                 Text
                                             , version :
                                                 types.VersionRange
                                             }
                                    , profiling-options =
                                        prelude.defaults.CompilerOptions
                                    , reexported-modules =
                                        [] : List
                                             { name :
                                                 Text
                                             , original :
                                                 { name :
                                                     Text
                                                 , package :
                                                     Optional Text
                                                 }
                                             }
                                    , shared-options =
                                        prelude.defaults.CompilerOptions
                                    , signatures =
                                        [] : List Text
                                    }
                        
                        else  { autogen-modules =
                                  [] : List Text
                              , build-depends =
                                  [] : List
                                       { bounds :
                                           types.VersionRange
                                       , package :
                                           Text
                                       }
                              , build-tool-depends =
                                  [] : List
                                       { component :
                                           Text
                                       , package :
                                           Text
                                       , version :
                                           types.VersionRange
                                       }
                              , build-tools =
                                  [] : List
                                       { exe :
                                           Text
                                       , version :
                                           types.VersionRange
                                       }
                              , buildable =
                                  True
                              , c-sources =
                                  [] : List Text
                              , cc-options =
                                  [] : List Text
                              , compiler-options =
                                  prelude.defaults.CompilerOptions
                              , cpp-options =
                                  [ "-DINTEROP" ]
                              , default-extensions =
                                  [] : List types.Extension
                              , default-language =
                                  [] : Optional types.Language
                              , exposed-modules =
                                  [] : List Text
                              , extra-framework-dirs =
                                  [] : List Text
                              , extra-ghci-libraries =
                                  [] : List Text
                              , extra-lib-dirs =
                                  [] : List Text
                              , extra-libraries =
                                  [] : List Text
                              , frameworks =
                                  [] : List Text
                              , hs-source-dirs =
                                  [] : List Text
                              , include-dirs =
                                  [] : List Text
                              , includes =
                                  [] : List Text
                              , install-includes =
                                  [] : List Text
                              , js-sources =
                                  [] : List Text
                              , ld-options =
                                  [] : List Text
                              , mixins =
                                  [] : List types.Mixin
                              , other-extensions =
                                  [] : List types.Extension
                              , other-languages =
                                  [] : List types.Language
                              , other-modules =
                                  [] : List Text
                              , pkgconfig-depends =
                                  [] : List
                                       { name :
                                           Text
                                       , version :
                                           types.VersionRange
                                       }
                              , profiling-options =
                                  prelude.defaults.CompilerOptions
                              , reexported-modules =
                                  [] : List
                                       { name :
                                           Text
                                       , original :
                                           { name :
                                               Text
                                           , package :
                                               Optional Text
                                           }
                                       }
                              , shared-options =
                                  prelude.defaults.CompilerOptions
                              , signatures =
                                  [] : List Text
                              }
            
            else  if    config.impl
                        (prelude.types.Compilers.GHC {=})
                        (prelude.earlierVersion (prelude.v "0.7.0.2"))
                  then        if    config.impl
                                    (prelude.types.Compilers.GHC {=})
                                    (prelude.orLaterVersion (prelude.v "0.0.9"))
                              then        if    config.flag
                                                "wai-servlet-debug"
                                          then  { autogen-modules =
                                                    [] : List Text
                                                , build-depends =
                                                    [] : List
                                                         { bounds :
                                                             types.VersionRange
                                                         , package :
                                                             Text
                                                         }
                                                , build-tool-depends =
                                                    [] : List
                                                         { component :
                                                             Text
                                                         , package :
                                                             Text
                                                         , version :
                                                             types.VersionRange
                                                         }
                                                , build-tools =
                                                    [] : List
                                                         { exe :
                                                             Text
                                                         , version :
                                                             types.VersionRange
                                                         }
                                                , buildable =
                                                    True
                                                , c-sources =
                                                    [ "java/Utils.java" ]
                                                , cc-options =
                                                    [] : List Text
                                                , compiler-options =
                                                    prelude.defaults.CompilerOptions
                                                , cpp-options =
                                                    [ "-DPURE_JAVA_WITH"
                                                    , "-DWAI_SERVLET_DEBUG"
                                                    ]
                                                , default-extensions =
                                                    [] : List types.Extension
                                                , default-language =
                                                    [] : Optional types.Language
                                                , exposed-modules =
                                                    [] : List Text
                                                , extra-framework-dirs =
                                                    [] : List Text
                                                , extra-ghci-libraries =
                                                    [] : List Text
                                                , extra-lib-dirs =
                                                    [] : List Text
                                                , extra-libraries =
                                                    [] : List Text
                                                , frameworks =
                                                    [] : List Text
                                                , hs-source-dirs =
                                                    [] : List Text
                                                , include-dirs =
                                                    [] : List Text
                                                , includes =
                                                    [] : List Text
                                                , install-includes =
                                                    [] : List Text
                                                , js-sources =
                                                    [] : List Text
                                                , ld-options =
                                                    [] : List Text
                                                , mixins =
                                                    [] : List types.Mixin
                                                , other-extensions =
                                                    [] : List types.Extension
                                                , other-languages =
                                                    [] : List types.Language
                                                , other-modules =
                                                    [] : List Text
                                                , pkgconfig-depends =
                                                    [] : List
                                                         { name :
                                                             Text
                                                         , version :
                                                             types.VersionRange
                                                         }
                                                , profiling-options =
                                                    prelude.defaults.CompilerOptions
                                                , reexported-modules =
                                                    [] : List
                                                         { name :
                                                             Text
                                                         , original :
                                                             { name :
                                                                 Text
                                                             , package :
                                                                 Optional Text
                                                             }
                                                         }
                                                , shared-options =
                                                    prelude.defaults.CompilerOptions
                                                , signatures =
                                                    [] : List Text
                                                }
                                    
                                    else  { autogen-modules =
                                              [] : List Text
                                          , build-depends =
                                              [] : List
                                                   { bounds :
                                                       types.VersionRange
                                                   , package :
                                                       Text
                                                   }
                                          , build-tool-depends =
                                              [] : List
                                                   { component :
                                                       Text
                                                   , package :
                                                       Text
                                                   , version :
                                                       types.VersionRange
                                                   }
                                          , build-tools =
                                              [] : List
                                                   { exe :
                                                       Text
                                                   , version :
                                                       types.VersionRange
                                                   }
                                          , buildable =
                                              True
                                          , c-sources =
                                              [ "java/Utils.java" ]
                                          , cc-options =
                                              [] : List Text
                                          , compiler-options =
                                              prelude.defaults.CompilerOptions
                                          , cpp-options =
                                              [ "-DPURE_JAVA_WITH" ]
                                          , default-extensions =
                                              [] : List types.Extension
                                          , default-language =
                                              [] : Optional types.Language
                                          , exposed-modules =
                                              [] : List Text
                                          , extra-framework-dirs =
                                              [] : List Text
                                          , extra-ghci-libraries =
                                              [] : List Text
                                          , extra-lib-dirs =
                                              [] : List Text
                                          , extra-libraries =
                                              [] : List Text
                                          , frameworks =
                                              [] : List Text
                                          , hs-source-dirs =
                                              [] : List Text
                                          , include-dirs =
                                              [] : List Text
                                          , includes =
                                              [] : List Text
                                          , install-includes =
                                              [] : List Text
                                          , js-sources =
                                              [] : List Text
                                          , ld-options =
                                              [] : List Text
                                          , mixins =
                                              [] : List types.Mixin
                                          , other-extensions =
                                              [] : List types.Extension
                                          , other-languages =
                                              [] : List types.Language
                                          , other-modules =
                                              [] : List Text
                                          , pkgconfig-depends =
                                              [] : List
                                                   { name :
                                                       Text
                                                   , version :
                                                       types.VersionRange
                                                   }
                                          , profiling-options =
                                              prelude.defaults.CompilerOptions
                                          , reexported-modules =
                                              [] : List
                                                   { name :
                                                       Text
                                                   , original :
                                                       { name :
                                                           Text
                                                       , package :
                                                           Optional Text
                                                       }
                                                   }
                                          , shared-options =
                                              prelude.defaults.CompilerOptions
                                          , signatures =
                                              [] : List Text
                                          }
                        
                        else  if    config.flag "wai-servlet-debug"
                              then  { autogen-modules =
                                        [] : List Text
                                    , build-depends =
                                        [] : List
                                             { bounds :
                                                 types.VersionRange
                                             , package :
                                                 Text
                                             }
                                    , build-tool-depends =
                                        [] : List
                                             { component :
                                                 Text
                                             , package :
                                                 Text
                                             , version :
                                                 types.VersionRange
                                             }
                                    , build-tools =
                                        [] : List
                                             { exe :
                                                 Text
                                             , version :
                                                 types.VersionRange
                                             }
                                    , buildable =
                                        True
                                    , c-sources =
                                        [] : List Text
                                    , cc-options =
                                        [] : List Text
                                    , compiler-options =
                                        prelude.defaults.CompilerOptions
                                    , cpp-options =
                                        [ "-DPURE_JAVA_WITH"
                                        , "-DWAI_SERVLET_DEBUG"
                                        ]
                                    , default-extensions =
                                        [] : List types.Extension
                                    , default-language =
                                        [] : Optional types.Language
                                    , exposed-modules =
                                        [] : List Text
                                    , extra-framework-dirs =
                                        [] : List Text
                                    , extra-ghci-libraries =
                                        [] : List Text
                                    , extra-lib-dirs =
                                        [] : List Text
                                    , extra-libraries =
                                        [] : List Text
                                    , frameworks =
                                        [] : List Text
                                    , hs-source-dirs =
                                        [] : List Text
                                    , include-dirs =
                                        [] : List Text
                                    , includes =
                                        [] : List Text
                                    , install-includes =
                                        [] : List Text
                                    , js-sources =
                                        [] : List Text
                                    , ld-options =
                                        [] : List Text
                                    , mixins =
                                        [] : List types.Mixin
                                    , other-extensions =
                                        [] : List types.Extension
                                    , other-languages =
                                        [] : List types.Language
                                    , other-modules =
                                        [] : List Text
                                    , pkgconfig-depends =
                                        [] : List
                                             { name :
                                                 Text
                                             , version :
                                                 types.VersionRange
                                             }
                                    , profiling-options =
                                        prelude.defaults.CompilerOptions
                                    , reexported-modules =
                                        [] : List
                                             { name :
                                                 Text
                                             , original :
                                                 { name :
                                                     Text
                                                 , package :
                                                     Optional Text
                                                 }
                                             }
                                    , shared-options =
                                        prelude.defaults.CompilerOptions
                                    , signatures =
                                        [] : List Text
                                    }
                        
                        else  { autogen-modules =
                                  [] : List Text
                              , build-depends =
                                  [] : List
                                       { bounds :
                                           types.VersionRange
                                       , package :
                                           Text
                                       }
                              , build-tool-depends =
                                  [] : List
                                       { component :
                                           Text
                                       , package :
                                           Text
                                       , version :
                                           types.VersionRange
                                       }
                              , build-tools =
                                  [] : List
                                       { exe :
                                           Text
                                       , version :
                                           types.VersionRange
                                       }
                              , buildable =
                                  True
                              , c-sources =
                                  [] : List Text
                              , cc-options =
                                  [] : List Text
                              , compiler-options =
                                  prelude.defaults.CompilerOptions
                              , cpp-options =
                                  [ "-DPURE_JAVA_WITH" ]
                              , default-extensions =
                                  [] : List types.Extension
                              , default-language =
                                  [] : Optional types.Language
                              , exposed-modules =
                                  [] : List Text
                              , extra-framework-dirs =
                                  [] : List Text
                              , extra-ghci-libraries =
                                  [] : List Text
                              , extra-lib-dirs =
                                  [] : List Text
                              , extra-libraries =
                                  [] : List Text
                              , frameworks =
                                  [] : List Text
                              , hs-source-dirs =
                                  [] : List Text
                              , include-dirs =
                                  [] : List Text
                              , includes =
                                  [] : List Text
                              , install-includes =
                                  [] : List Text
                              , js-sources =
                                  [] : List Text
                              , ld-options =
                                  [] : List Text
                              , mixins =
                                  [] : List types.Mixin
                              , other-extensions =
                                  [] : List types.Extension
                              , other-languages =
                                  [] : List types.Language
                              , other-modules =
                                  [] : List Text
                              , pkgconfig-depends =
                                  [] : List
                                       { name :
                                           Text
                                       , version :
                                           types.VersionRange
                                       }
                              , profiling-options =
                                  prelude.defaults.CompilerOptions
                              , reexported-modules =
                                  [] : List
                                       { name :
                                           Text
                                       , original :
                                           { name :
                                               Text
                                           , package :
                                               Optional Text
                                           }
                                       }
                              , shared-options =
                                  prelude.defaults.CompilerOptions
                              , signatures =
                                  [] : List Text
                              }
            
            else  if    config.impl
                        (prelude.types.Compilers.GHC {=})
                        (prelude.orLaterVersion (prelude.v "0.0.9"))
                  then        if    config.flag "wai-servlet-debug"
                              then  { autogen-modules =
                                        [] : List Text
                                    , build-depends =
                                        [] : List
                                             { bounds :
                                                 types.VersionRange
                                             , package :
                                                 Text
                                             }
                                    , build-tool-depends =
                                        [] : List
                                             { component :
                                                 Text
                                             , package :
                                                 Text
                                             , version :
                                                 types.VersionRange
                                             }
                                    , build-tools =
                                        [] : List
                                             { exe :
                                                 Text
                                             , version :
                                                 types.VersionRange
                                             }
                                    , buildable =
                                        True
                                    , c-sources =
                                        [ "java/Utils.java" ]
                                    , cc-options =
                                        [] : List Text
                                    , compiler-options =
                                        prelude.defaults.CompilerOptions
                                    , cpp-options =
                                        [ "-DWAI_SERVLET_DEBUG" ]
                                    , default-extensions =
                                        [] : List types.Extension
                                    , default-language =
                                        [] : Optional types.Language
                                    , exposed-modules =
                                        [] : List Text
                                    , extra-framework-dirs =
                                        [] : List Text
                                    , extra-ghci-libraries =
                                        [] : List Text
                                    , extra-lib-dirs =
                                        [] : List Text
                                    , extra-libraries =
                                        [] : List Text
                                    , frameworks =
                                        [] : List Text
                                    , hs-source-dirs =
                                        [] : List Text
                                    , include-dirs =
                                        [] : List Text
                                    , includes =
                                        [] : List Text
                                    , install-includes =
                                        [] : List Text
                                    , js-sources =
                                        [] : List Text
                                    , ld-options =
                                        [] : List Text
                                    , mixins =
                                        [] : List types.Mixin
                                    , other-extensions =
                                        [] : List types.Extension
                                    , other-languages =
                                        [] : List types.Language
                                    , other-modules =
                                        [] : List Text
                                    , pkgconfig-depends =
                                        [] : List
                                             { name :
                                                 Text
                                             , version :
                                                 types.VersionRange
                                             }
                                    , profiling-options =
                                        prelude.defaults.CompilerOptions
                                    , reexported-modules =
                                        [] : List
                                             { name :
                                                 Text
                                             , original :
                                                 { name :
                                                     Text
                                                 , package :
                                                     Optional Text
                                                 }
                                             }
                                    , shared-options =
                                        prelude.defaults.CompilerOptions
                                    , signatures =
                                        [] : List Text
                                    }
                        
                        else  { autogen-modules =
                                  [] : List Text
                              , build-depends =
                                  [] : List
                                       { bounds :
                                           types.VersionRange
                                       , package :
                                           Text
                                       }
                              , build-tool-depends =
                                  [] : List
                                       { component :
                                           Text
                                       , package :
                                           Text
                                       , version :
                                           types.VersionRange
                                       }
                              , build-tools =
                                  [] : List
                                       { exe :
                                           Text
                                       , version :
                                           types.VersionRange
                                       }
                              , buildable =
                                  True
                              , c-sources =
                                  [ "java/Utils.java" ]
                              , cc-options =
                                  [] : List Text
                              , compiler-options =
                                  prelude.defaults.CompilerOptions
                              , cpp-options =
                                  [] : List Text
                              , default-extensions =
                                  [] : List types.Extension
                              , default-language =
                                  [] : Optional types.Language
                              , exposed-modules =
                                  [] : List Text
                              , extra-framework-dirs =
                                  [] : List Text
                              , extra-ghci-libraries =
                                  [] : List Text
                              , extra-lib-dirs =
                                  [] : List Text
                              , extra-libraries =
                                  [] : List Text
                              , frameworks =
                                  [] : List Text
                              , hs-source-dirs =
                                  [] : List Text
                              , include-dirs =
                                  [] : List Text
                              , includes =
                                  [] : List Text
                              , install-includes =
                                  [] : List Text
                              , js-sources =
                                  [] : List Text
                              , ld-options =
                                  [] : List Text
                              , mixins =
                                  [] : List types.Mixin
                              , other-extensions =
                                  [] : List types.Extension
                              , other-languages =
                                  [] : List types.Language
                              , other-modules =
                                  [] : List Text
                              , pkgconfig-depends =
                                  [] : List
                                       { name :
                                           Text
                                       , version :
                                           types.VersionRange
                                       }
                              , profiling-options =
                                  prelude.defaults.CompilerOptions
                              , reexported-modules =
                                  [] : List
                                       { name :
                                           Text
                                       , original :
                                           { name :
                                               Text
                                           , package :
                                               Optional Text
                                           }
                                       }
                              , shared-options =
                                  prelude.defaults.CompilerOptions
                              , signatures =
                                  [] : List Text
                              }
            
            else  if    config.flag "wai-servlet-debug"
                  then  { autogen-modules =
                            [] : List Text
                        , build-depends =
                            [] : List
                                 { bounds : types.VersionRange, package : Text }
                        , build-tool-depends =
                            [] : List
                                 { component :
                                     Text
                                 , package :
                                     Text
                                 , version :
                                     types.VersionRange
                                 }
                        , build-tools =
                            [] : List
                                 { exe : Text, version : types.VersionRange }
                        , buildable =
                            True
                        , c-sources =
                            [] : List Text
                        , cc-options =
                            [] : List Text
                        , compiler-options =
                            prelude.defaults.CompilerOptions
                        , cpp-options =
                            [ "-DWAI_SERVLET_DEBUG" ]
                        , default-extensions =
                            [] : List types.Extension
                        , default-language =
                            [] : Optional types.Language
                        , exposed-modules =
                            [] : List Text
                        , extra-framework-dirs =
                            [] : List Text
                        , extra-ghci-libraries =
                            [] : List Text
                        , extra-lib-dirs =
                            [] : List Text
                        , extra-libraries =
                            [] : List Text
                        , frameworks =
                            [] : List Text
                        , hs-source-dirs =
                            [] : List Text
                        , include-dirs =
                            [] : List Text
                        , includes =
                            [] : List Text
                        , install-includes =
                            [] : List Text
                        , js-sources =
                            [] : List Text
                        , ld-options =
                            [] : List Text
                        , mixins =
                            [] : List types.Mixin
                        , other-extensions =
                            [] : List types.Extension
                        , other-languages =
                            [] : List types.Language
                        , other-modules =
                            [] : List Text
                        , pkgconfig-depends =
                            [] : List
                                 { name : Text, version : types.VersionRange }
                        , profiling-options =
                            prelude.defaults.CompilerOptions
                        , reexported-modules =
                            [] : List
                                 { name :
                                     Text
                                 , original :
                                     { name : Text, package : Optional Text }
                                 }
                        , shared-options =
                            prelude.defaults.CompilerOptions
                        , signatures =
                            [] : List Text
                        }
            
            else  { autogen-modules =
                      [] : List Text
                  , build-depends =
                      [] : List { bounds : types.VersionRange, package : Text }
                  , build-tool-depends =
                      [] : List
                           { component :
                               Text
                           , package :
                               Text
                           , version :
                               types.VersionRange
                           }
                  , build-tools =
                      [] : List { exe : Text, version : types.VersionRange }
                  , buildable =
                      True
                  , c-sources =
                      [] : List Text
                  , cc-options =
                      [] : List Text
                  , compiler-options =
                      prelude.defaults.CompilerOptions
                  , cpp-options =
                      [] : List Text
                  , default-extensions =
                      [] : List types.Extension
                  , default-language =
                      [] : Optional types.Language
                  , exposed-modules =
                      [] : List Text
                  , extra-framework-dirs =
                      [] : List Text
                  , extra-ghci-libraries =
                      [] : List Text
                  , extra-lib-dirs =
                      [] : List Text
                  , extra-libraries =
                      [] : List Text
                  , frameworks =
                      [] : List Text
                  , hs-source-dirs =
                      [] : List Text
                  , include-dirs =
                      [] : List Text
                  , includes =
                      [] : List Text
                  , install-includes =
                      [] : List Text
                  , js-sources =
                      [] : List Text
                  , ld-options =
                      [] : List Text
                  , mixins =
                      [] : List types.Mixin
                  , other-extensions =
                      [] : List types.Extension
                  , other-languages =
                      [] : List types.Language
                  , other-modules =
                      [] : List Text
                  , pkgconfig-depends =
                      [] : List { name : Text, version : types.VersionRange }
                  , profiling-options =
                      prelude.defaults.CompilerOptions
                  , reexported-modules =
                      [] : List
                           { name :
                               Text
                           , original :
                               { name : Text, package : Optional Text }
                           }
                  , shared-options =
                      prelude.defaults.CompilerOptions
                  , signatures =
                      [] : List Text
                  }
        ] : Optional (types.Config → types.Library)
    , license =
        < Unspecified =
            {=}
        | GPL :
            Optional types.Version
        | AGPL :
            Optional types.Version
        | LGPL :
            Optional types.Version
        | BSD2 :
            {}
        | BSD3 :
            {}
        | BSD4 :
            {}
        | MIT :
            {}
        | ISC :
            {}
        | MPL :
            types.Version
        | Apache :
            Optional types.Version
        | PublicDomain :
            {}
        | AllRightsReserved :
            {}
        | Other :
            {}
        >
    , license-files =
        [] : List Text
    , maintainer =
        ""
    , name =
        "wai-servlet"
    , package-url =
        ""
    , source-repos =
        [] : List
             { branch :
                 Optional Text
             , kind :
                 types.RepoKind
             , location :
                 Optional Text
             , module :
                 Optional Text
             , subdir :
                 Optional Text
             , tag :
                 Optional Text
             , type :
                 Optional types.RepoType
             }
    , stability =
        ""
    , sub-libraries =
        [] : List { library : types.Config → types.Library, name : Text }
    , synopsis =
        ""
    , test-suites =
        [] : List { name : Text, test-suite : types.Config → types.TestSuite }
    , tested-with =
        [] : List { compiler : types.Compiler, version : types.VersionRange }
    , version =
        prelude.v "0.1.5.0"
    , x-fields =
        [] : List { _1 : Text, _2 : Text }
    }