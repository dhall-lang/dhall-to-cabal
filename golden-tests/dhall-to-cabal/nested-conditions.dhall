let prelude = ../../dhall/prelude.dhall

let types = ../../dhall/types.dhall

let v = prelude.v

in    prelude.defaults.Package
    ⫽ { name =
          "foo"
      , version =
          v "0"
      , library =
          Some
          (   λ(config : types.Config)
            →   prelude.defaults.Library
              ⫽ { compiler-options =
                      prelude.defaults.CompilerOptions
                    ⫽ { GHC =
                            [ "-Weverything" ]
                          # (       if config.impl
                                       types.Compiler.GHC
                                       (prelude.orLaterVersion (v "8.2"))
                              
                              then  [ "-Wno-redundant-constraints" ] : List Text
                              
                              else  [] : List Text
                            )
                          # (       if config.impl
                                       types.Compiler.GHC
                                       (prelude.orLaterVersion (v "8.4"))
                              
                              then  [ "-Wno-missing-export-lists" ] : List Text
                              
                              else  [] : List Text
                            )
                      }
                }
          )
      }
