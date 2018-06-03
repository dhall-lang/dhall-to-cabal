   let LicenseId = ./LicenseId.dhall
in let LicenseExceptionId = ./LicenseExceptionId.dhall
in   λ(a : ../SPDX.dhall )
   → λ(b : ../SPDX.dhall )
   → λ(SPDX : Type)
   → λ(license : LicenseId → Optional LicenseExceptionId → SPDX)
   → λ(licenseVersionOrLater : LicenseId → Optional LicenseExceptionId → SPDX)
   → λ(ref : Text → Optional LicenseExceptionId → SPDX)
   → λ(refWithFile : Text → Text → Optional LicenseExceptionId → SPDX)
   → λ(and : SPDX → SPDX → SPDX)
   → λ(or : SPDX → SPDX → SPDX)
   → or
     ( a
       SPDX
       license
       licenseVersionOrLater
       ref
       refWithFile
       and
       or
     )
     ( b
       SPDX
       license
       licenseVersionOrLater
       ref
       refWithFile
       and
       or
     )
