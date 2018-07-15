   let prelude = ../../dhall/prelude.dhall

in let types = ../../dhall/types.dhall

in let v = prelude.v

in let ghcImpl =
       \ ( cfg : types.Config ) -> \ ( ver : types.VersionRange )
    -> cfg.impl ( prelude.types.Compilers.GHC {=} ) ver

in ../../dhall/defaults/Package.dhall 
// { name =
       "Name"
   , version =
       ../../dhall/Version/v.dhall  "1"
   , library =
        [ \ ( config : types.Config )
         -> prelude.defaults.Library
         // { exposed-modules = [ "Foo", "Bar" ]
            , compiler-options =
                   prelude.defaults.CompilerOptions
                // { GHC = ["A"]
                     # ( if ghcImpl config (prelude.orLaterVersion (v "8.2"))
                         then ["B"]
                         else [] : List Text
                       )
                     # ( if ghcImpl config (prelude.orLaterVersion (v "8.4"))
                         then ["C"]
                         else [] : List Text
                       )
                     # ( if ghcImpl config (prelude.orLaterVersion (v "8.2"))
                         then ["D"]
                         else [] : List Text
                       )
                     # ( if ghcImpl config (prelude.orLaterVersion (v "8.4"))
                         then ["E"]
                         else [] : List Text
                       )
                     # ["F"]
                   }
            }
        ] : Optional ( types.Config -> types.Library )
        
   }
