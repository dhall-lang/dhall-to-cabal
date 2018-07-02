{ types =
    { BuildTypes =
        constructors ./types/BuildType.dhall
    , OSs =
        constructors ./types/OS.dhall
    , Compilers =
        constructors ./types/Compiler.dhall
    , Extensions =
        constructors ./types/Extension.dhall
    , Languages =
        constructors ./types/Language.dhall
    , Licenses =
        constructors ./types/License.dhall
    , LicenseExceptionId =
        constructors ./types/SPDX/LicenseExceptionId.dhall
    , LicenseId =
        constructors ./types/SPDX/LicenseId.dhall
    , TestTypes =
        constructors ./types/TestType.dhall
    , RepoType =
        constructors ./types/RepoType.dhall
    , RepoKind =
        constructors ./types/RepoKind.dhall
    , Scopes =
        constructors ./types/Scope.dhall
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
