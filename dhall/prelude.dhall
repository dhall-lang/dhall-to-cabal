{ defaults =
    { CompilerOptions =
        ./defaults/CompilerOptions.dhall
    , MainLibrary =
        ./defaults/MainLibrary.dhall
    , NamedLibrary =
        ./defaults/NamedLibrary.dhall
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
, pkg-config =
    { anyVersion =
        ./PkgconfigVersionRange/anyVersion.dhall
    , thisVersion =
        ./PkgconfigVersionRange/thisVersion.dhall
    , laterVersion =
        ./PkgconfigVersionRange/laterVersion.dhall
    , earlierVersion =
        ./PkgconfigVersionRange/earlierVersion.dhall
    , orLaterVersion =
        ./PkgconfigVersionRange/orLaterVersion.dhall
    , orEarlierVersion =
        ./PkgconfigVersionRange/orEarlierVersion.dhall
    , unionVersionRanges =
        ./PkgconfigVersionRange/unionVersionRanges.dhall
    , intersectVersionRanges =
        ./PkgconfigVersionRange/intersectVersionRanges.dhall
    }
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
        None ./types/SPDX/LicenseExceptionId.dhall
    }
}
