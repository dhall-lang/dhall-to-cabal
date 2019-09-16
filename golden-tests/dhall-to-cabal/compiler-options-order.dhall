let prelude = ../../dhall/prelude.dhall

let types = ../../dhall/types.dhall

let v = prelude.v

let ghcImpl =
        λ(cfg : types.Config)
      → λ(ver : types.VersionRange)
      → cfg.impl types.Compiler.GHC ver

in    ../../dhall/defaults/Package.dhall
    ⫽ { name =
          "Name"
      , version =
          ../../dhall/Version/v.dhall "1"
      , library =
          Some
          (   λ(config : types.Config)
            →   prelude.defaults.MainLibrary
              ⫽ { exposed-modules =
                    [ "Foo", "Bar" ]
                , compiler-options =
                      prelude.defaults.CompilerOptions
                    ⫽ { GHC =
                            [ "A" ]
                          # (       if ghcImpl
                                       config
                                       (prelude.orLaterVersion (v "8.2"))
                              
                              then  [ "B" ]
                              
                              else  [] : List Text
                            )
                          # (       if ghcImpl
                                       config
                                       (prelude.orLaterVersion (v "8.4"))
                              
                              then  [ "C" ]
                              
                              else  [] : List Text
                            )
                          # (       if ghcImpl
                                       config
                                       (prelude.orLaterVersion (v "8.2"))
                              
                              then  [ "D" ]
                              
                              else  [] : List Text
                            )
                          # (       if ghcImpl
                                       config
                                       (prelude.orLaterVersion (v "8.4"))
                              
                              then  [ "E" ]
                              
                              else  [] : List Text
                            )
                          # [ "F" ]
                      }
                }
          )
      }
