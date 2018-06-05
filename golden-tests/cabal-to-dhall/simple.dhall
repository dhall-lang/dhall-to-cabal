    let prelude = ../../dhall/prelude.dhall 

in  let types = ../../dhall/types.dhall 

in    prelude.defaults.Package
    ∧ { license =
          < AllRightsReserved =
              {=}
          | GPL :
              Optional types.Version
          | AGPL :
              Optional types.Version
          | LGPL :
              Optional types.Version
          | BSD2 :
              {}
          | BSD3 :
              {}
          | BSD4 :
              {}
          | MIT :
              {}
          | ISC :
              {}
          | MPL :
              types.Version
          | Apache :
              Optional types.Version
          | PublicDomain :
              {}
          | Unspecified :
              {}
          | Other :
              {}
          | SPDX :
              types.SPDX
          >
      , name =
          "test"
      , version =
          prelude.v "1.0"
      }