   let prelude = ../../dhall/prelude.dhall
in let types = ../../dhall/types.dhall
in   prelude.defaults.Package
  // { name = "foo"
     , version = prelude.v "0"
     , cabal-version = prelude.v "2.2"
     , license = types.License.SPDX
         ( prelude.SPDX.and
           ( prelude.SPDX.or
             (prelude.SPDX.license (types.LicenseId.AGPL_3_0_or_later {=}) prelude.SPDX.noException)
             (prelude.SPDX.licenseVersionOrLater (types.LicenseId.Apache_2_0 {=}) ([types.LicenseExceptionId.Classpath_exception_2_0 {=}] : Optional types.LicenseExceptionId))
          )
          ( prelude.SPDX.or
            (prelude.SPDX.ref "MyFancyLicense" ([] : Optional types.LicenseExceptionId))
            (prelude.SPDX.refWithFile "MyFancierLicense" "LICENSE.txt" ([] : Optional types.LicenseExceptionId))
         )
       )
     , library = prelude.unconditional.library prelude.defaults.Library
     }
