let prelude = ./../../dhall/prelude.dhall

let types = ./../../dhall/types.dhall

let v = prelude.v

let ghcImpl =
        λ(cfg : types.Config)
      → λ(ver : types.VersionRange)
      → cfg.impl (types.Compiler.GHC {=}) ver

in    ./../../dhall/defaults/Package.dhall
    ⫽ { name =
          "Name"
      , version =
          ./../../dhall/Version/v.dhall "1"
      , library =
          [   λ(config : types.Config)
            →   prelude.defaults.Library
              ⫽ { exposed-modules =
                      [ "Module1" ]
                    # ( if ghcImpl
                              config
                              (prelude.orLaterVersion (v "7.1.3"))
                        then  [ "Module2" ]
                        else  [] : List Text
                      )
                , other-modules =
                    if ghcImpl config (prelude.orLaterVersion (v "7.1.3"))
                    then  [ "OtherModule" ]
                    else  [] : List Text
                , cpp-options =
                    if ghcImpl config (prelude.orLaterVersion (v "7.1.3"))
                    then  [ "-DCOND1" ]
                    else  [] : List Text
                }
          ] : Optional (./../../dhall/types/Guarded.dhall types.Library)
      }
