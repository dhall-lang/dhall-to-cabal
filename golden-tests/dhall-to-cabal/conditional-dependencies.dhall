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
              ⫽ { build-depends =
                      [ { package = "A", bounds = prelude.anyVersion } ]
                    # ( if ghcImpl
                                 config
                                 (prelude.orLaterVersion (v "8.2"))
                        then  [ { package = "B", bounds = prelude.anyVersion } ]
                        else  [] : List types.Dependency
                      )
                    # ( if ghcImpl
                                 config
                                 (prelude.orLaterVersion (v "8.4"))
                        then  [ { package = "C", bounds = prelude.anyVersion } ]
                        else  [] : List types.Dependency
                      )
                }
          ] : Optional (types.Config → types.Library)
      }
