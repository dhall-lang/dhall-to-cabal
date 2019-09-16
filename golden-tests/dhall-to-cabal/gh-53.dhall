let prelude = ../../dhall/prelude.dhall

let types = ../../dhall/types.dhall

in    prelude.defaults.Package
    ⫽ { cabal-version =
          prelude.v "2.0"
      , name =
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
      , library =
          Some
          (   λ(config : types.Config)
            →       if config.impl
                       types.Compiler.GHC
                       (prelude.orLaterVersion (prelude.v "0.0.9"))
              
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
      }
