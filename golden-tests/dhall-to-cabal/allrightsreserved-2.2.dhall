let prelude = ../../dhall/prelude.dhall

let types = ../../dhall/types.dhall

in    prelude.defaults.Package
    ⫽ { name =
          "foo"
      , version =
          prelude.v "0"
      , cabal-version =
          prelude.v "2.2"
      , license =
          types.License.AllRightsReserved
      , library =
          prelude.unconditional.library prelude.defaults.MainLibrary
      }
