   let prelude = ./dhall/prelude.dhall
in let types = ./dhall/types.dhall
in   prelude.defaults.Package
  // { name = "foo"
     , version = prelude.v "0"
     , library = prelude.unconditional.library
          ( prelude.defaults.Library
         // { default-language =
                [ prelude.types.Languages.Haskell98 {=}
                ] : Optional types.Language
            }
          )
     }
