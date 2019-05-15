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
          types.License.SPDX
          ( prelude.SPDX.and
            ( prelude.SPDX.or
              ( prelude.SPDX.license
                types.LicenseId.AGPL_3_0_or_later
                prelude.SPDX.noException
              )
              ( prelude.SPDX.licenseVersionOrLater
                types.LicenseId.Apache_2_0
                (Some types.LicenseExceptionId.Classpath_exception_2_0)
              )
            )
            ( prelude.SPDX.or
              ( prelude.SPDX.ref
                "MyFancyLicense"
                (None types.LicenseExceptionId)
              )
              ( prelude.SPDX.refWithFile
                "MyFancierLicense"
                "LICENSE.txt"
                (None types.LicenseExceptionId)
              )
            )
          )
      , library =
          prelude.unconditional.library prelude.defaults.Library
      }
