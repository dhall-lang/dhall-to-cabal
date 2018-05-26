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
         // { exposed-modules =
                [ "Module1" ]
              # ( if ghcImpl config ( prelude.orLaterVersion ( v "7.1.3" ) )
                  then [ "Module2" ] else [ ] : List Text )
            , other-modules =
                ( if ghcImpl config ( prelude.orLaterVersion ( v "7.1.3" ) )
                  then [ "OtherModule" ] else [ ] : List Text )
            , cpp-options =
                ( if ghcImpl config ( prelude.orLaterVersion ( v "7.1.3" ) )
                  then [ "-DCOND1" ] else [ ] : List Text )
            }
        ] : Optional ( ./dhall/types/Guarded.dhall  types.Library )

   }
