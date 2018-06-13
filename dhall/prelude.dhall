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
    ./types/VersionRange/AnyVersion.dhall
, earlierVersion =
    ./types/VersionRange/EarlierVersion.dhall
, orEarlierVersion =
    ./types/VersionRange/OrEarlierVersion.dhall
, intersectVersionRanges =
    ./types/VersionRange/IntersectVersionRanges.dhall
, unionVersionRanges =
    ./types/VersionRange/UnionVersionRanges.dhall
, majorBoundVersion =
    ./types/VersionRange/MajorBoundVersion.dhall
, orLaterVersion =
    ./types/VersionRange/OrLaterVersion.dhall
, laterVersion =
    ./types/VersionRange/LaterVersion.dhall
, thisVersion =
    ./types/VersionRange/ThisVersion.dhall
, notThisVersion =
    ./types/VersionRange/NotThisVersion.dhall
, withinVersion =
    ./types/VersionRange/WithinVersion.dhall
, v =
    ./types/Version/v.dhall
, noVersion =
    ./types/VersionRange/NoVersion.dhall
, utils =
    ./utils/package.dhall
, unconditional =
    ./unconditional.dhall
, SPDX =
    { license =
        ./types/SPDX/License.dhall
    , licenseVersionOrLater =
        ./types/SPDX/LicenseVersionOrLater.dhall
    , ref =
        ./types/SPDX/Ref.dhall
    , refWithFile =
        ./types/SPDX/RefWithFile.dhall
    , and =
        ./types/SPDX/And.dhall
    , or =
        ./types/SPDX/Or.dhall
    , noException =
        [] : Optional ./types/SPDX/LicenseExceptionId.dhall
    }
}
