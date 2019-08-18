let prelude = ./../../dhall/prelude.dhall

let types = ./../../dhall/types.dhall

in    prelude.defaults.Package
    ⫽ { cabal-version =
          prelude.v "1.12"
      , license =
          types.License.Unknown "MYUnknownLicense"
      , name =
          "test"
      , version =
          prelude.v "1.0"
      }