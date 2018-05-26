   let prelude = ./dhall/prelude.dhall

in let types = ./dhall/types.dhall

in let v = prelude.v

in let ghcImpl =
       \ ( cfg : types.Config ) -> \ ( ver : types.VersionRange )
    -> cfg.impl ( prelude.types.Compilers.GHC {=} ) ver

in ./dhall/defaults/Package.dhall
// { name =
       "Name"
   , version =
       ./dhall/types/Version/v.dhall  "1"
   , library =
        [ \ ( config : types.Config )
         -> prelude.defaults.Library
         // { build-depends =
                  [ { package = "A", bounds = prelude.anyVersion } ]
                # ( if ghcImpl config (prelude.orLaterVersion (v "8.2"))
                    then [ { package = "B", bounds = prelude.anyVersion } ]
                    else [] : List types.Dependency
                  )
                # ( if ghcImpl config (prelude.orLaterVersion (v "8.4"))
                    then [ { package = "C", bounds = prelude.anyVersion } ]
                    else [] : List types.Dependency
                  )
            }
        ] : Optional ( types.Config -> types.Library )

   }
