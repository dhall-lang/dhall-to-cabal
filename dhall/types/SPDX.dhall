   let LicenseExceptionId : Type = ./SPDX/LicenseExceptionId.dhall
in let LicenseId : Type = ./SPDX/LicenseId.dhall
in   ∀(SPDX : Type)
   → ∀(license : LicenseId → Optional LicenseExceptionId → SPDX)
   → ∀(licenseVersionOrLater : LicenseId → Optional LicenseExceptionId → SPDX)
   → ∀(ref : Text → Optional LicenseExceptionId → SPDX)
   → ∀(refWithFile : Text → Text → Optional LicenseExceptionId → SPDX)
   → ∀(and : SPDX → SPDX → SPDX)
   → ∀(or : SPDX → SPDX → SPDX)
   → SPDX
