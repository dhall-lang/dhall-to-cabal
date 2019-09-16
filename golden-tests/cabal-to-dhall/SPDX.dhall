let prelude = ./../../dhall/prelude.dhall

let types = ./../../dhall/types.dhall

in    prelude.defaults.Package
    ⫽ { license =
          types.License.SPDX
            ( prelude.SPDX.and
                ( prelude.SPDX.or
                    ( prelude.SPDX.license
                        types.LicenseId.AGPL_3_0_or_later
                        (Some types.LicenseExceptionId.Classpath_exception_2_0)
                    )
                    ( prelude.SPDX.licenseVersionOrLater
                        types.LicenseId.Apache_2_0
                        (None types.LicenseExceptionId)
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
      , name =
          "foo"
      , version =
          prelude.v "0"
      }