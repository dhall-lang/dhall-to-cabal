   let LicenseId = ../types/SPDX/LicenseId.dhall
in let LicenseExceptionId = ../types/SPDX/LicenseExceptionId.dhall
in   λ(refName : Text)
   → λ(licenseExceptionIdOpt : Optional LicenseExceptionId )
   → λ(SPDX : Type)
   → λ(version : LicenseId → Optional LicenseExceptionId → SPDX)
   → λ(versionOrLater : LicenseId → Optional LicenseExceptionId → SPDX)
   → λ(ref : Text → Optional LicenseExceptionId → SPDX)
   → λ(refWithFile : Text → Text → Optional LicenseExceptionId → SPDX)
   → λ(and : SPDX → SPDX → SPDX)
   → λ(or : SPDX → SPDX → SPDX)
   → ref refName licenseExceptionIdOpt
