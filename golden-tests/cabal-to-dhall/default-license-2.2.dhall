    let prelude = ./../../dhall/prelude.dhall

in  let types = ./../../dhall/types.dhall

in  prelude.defaults.Package ⫽ { name = "test", version = prelude.v "1.0" }