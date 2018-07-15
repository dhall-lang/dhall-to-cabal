   let prelude = ../../dhall/prelude.dhall 
in let types = ../../dhall/types.dhall 
in let v = prelude.v
in   prelude.defaults.Package
  // { name = "foo"
     , version = v "0"
     , library =
       [ \ (config : types.Config) -> prelude.defaults.Library
           // { compiler-options = prelude.defaults.CompilerOptions
                 // { GHC = [ "-Weverything" ]
                        # (if config.impl (prelude.types.Compilers.GHC {=}) (prelude.orLaterVersion (v "8.2"))
                             then [ "-Wno-redundant-constraints" ] : List Text
                             else [] : List Text)
                        # (if config.impl (prelude.types.Compilers.GHC {=}) (prelude.orLaterVersion (v "8.4"))
                             then [ "-Wno-missing-export-lists" ] : List Text
                             else [] : List Text)
                    }
              }
       ] : Optional (types.Config -> types.Library)
     }
