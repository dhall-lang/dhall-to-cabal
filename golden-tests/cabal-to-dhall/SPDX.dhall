    let prelude = ../../dhall/prelude.dhall 

in  let types = ../../dhall/types.dhall 

in    prelude.defaults.Package
    ∧ { build-type =
          [ prelude.types.BuildTypes.Simple {=} ] : Optional types.BuildType
      , cabal-version =
          prelude.v "2.2"
      , license =
          < SPDX =
              prelude.SPDX.and
              ( prelude.SPDX.or
                ( prelude.SPDX.license
                  (prelude.types.LicenseId.AGPL_3_0_or_later {=})
                  ( [ prelude.types.LicenseExceptionId.Classpath_exception_2_0
                      {=}
                    ] : Optional types.LicenseExceptionId
                  )
                )
                ( prelude.SPDX.licenseVersionOrLater
                  (prelude.types.LicenseId.Apache_2_0 {=})
                  ([] : Optional types.LicenseExceptionId)
                )
              )
              ( prelude.SPDX.or
                ( prelude.SPDX.ref
                  "MyFancyLicense"
                  ([] : Optional types.LicenseExceptionId)
                )
                ( prelude.SPDX.refWithFile
                  "MyFancierLicense"
                  "LICENSE.txt"
                  ([] : Optional types.LicenseExceptionId)
                )
              )
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
          | AllRightsReserved :
              {}
          | Unspecified :
              {}
          | Other :
              {}
          >
      , name =
          "foo"
      , version =
          prelude.v "0"
      }