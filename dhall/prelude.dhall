{ types =
    { BuildTypes =
        ./types/BuildType.dhall
    , OSs =
        ./types/OS.dhall
    , Compilers =
        ./types/Compiler.dhall
    , Extensions =
        ./types/Extension.dhall
    , Languages =
        ./types/Language.dhall
    , Licenses =
        ./types/License.dhall
    , LicenseExceptionId =
        ./types/SPDX/LicenseExceptionId.dhall
    , LicenseId =
        ./types/SPDX/LicenseId.dhall
    , ModuleRenaming =
        ./types/ModuleRenaming.dhall
    , TestTypes =
        ./types/TestType.dhall
    , RepoType =
        ./types/RepoType.dhall
    , RepoKind =
        ./types/RepoKind.dhall
    , Scopes =
        ./types/Scope.dhall
    }
, defaults =
    { CompilerOptions =
        ./defaults/CompilerOptions.dhall
    , Library =
        ./defaults/Library.dhall
    , Benchmark =
        ./defaults/Benchmark.dhall
    , Executable =
        ./defaults/Executable.dhall
    , Package =
        ./defaults/Package.dhall
    , SourceRepo =
        ./defaults/SourceRepo.dhall
    , TestSuite =
        ./defaults/TestSuite.dhall
    }
, anyVersion =
    ./VersionRange/anyVersion.dhall 
, earlierVersion =
    ./VersionRange/earlierVersion.dhall 
, orEarlierVersion =
    ./VersionRange/orEarlierVersion.dhall 
, intersectVersionRanges =
    ./VersionRange/intersectVersionRanges.dhall 
, unionVersionRanges =
    ./VersionRange/unionVersionRanges.dhall 
, majorBoundVersion =
    ./VersionRange/majorBoundVersion.dhall 
, orLaterVersion =
    ./VersionRange/orLaterVersion.dhall 
, laterVersion =
    ./VersionRange/laterVersion.dhall 
, thisVersion =
    ./VersionRange/thisVersion.dhall 
, notThisVersion =
    ./VersionRange/notThisVersion.dhall 
, withinVersion =
    ./VersionRange/withinVersion.dhall 
, v =
    ./Version/v.dhall 
, noVersion =
    ./VersionRange/noVersion.dhall 
, utils =
    ./utils/package.dhall
, unconditional =
    ./unconditional.dhall
, SPDX =
    { license =
        ./SPDX/license.dhall
    , licenseVersionOrLater =
        ./SPDX/licenseVersionOrLater.dhall
    , ref =
        ./SPDX/ref.dhall
    , refWithFile =
        ./SPDX/refWithFile.dhall
    , and =
        ./SPDX/and.dhall
    , or =
        ./SPDX/or.dhall
    , noException =
        [] : Optional ./types/SPDX/LicenseExceptionId.dhall
    }
}
