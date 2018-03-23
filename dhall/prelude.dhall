{ types =
    { Benchmark =
        ./types/Benchmark.dhall 
    , BuildType =
        ./types/BuildType.dhall 
    , BuildTypes =
        constructors ./types/BuildType.dhall 
    , OSs =
        constructors ./types/OS.dhall 
    , OS =
        ./types/OS.dhall 
    , CompilerOptions =
        ./types/CompilerOptions.dhall 
    , Compilers =
        constructors ./types/Compiler.dhall 
    , ConfigOptions =
        ./types/Config.dhall 
    , CustomSetup =
        ./types/CustomSetup.dhall 
    , Executable =
        ./types/Executable.dhall 
    , Extension =
        ./types/Extension.dhall 
    , Extensions =
        constructors ./types/Extension.dhall 
    , ForeignLibrary =
        ./types/ForeignLibrary.dhall 
    , Language =
        ./types/Language.dhall 
    , Languages =
        constructors ./types/Language.dhall 
    , Library =
        ./types/Library.dhall 
    , Mixin =
        ./types/Mixin.dhall 
    , RepoKind =
        ./types/RepoKind.dhall 
    , RepoType =
        ./types/RepoType.dhall 
    , TestSuite =
        ./types/TestSuite.dhall 
    , Version =
        ./types/Version.dhall 
    , VersionRange =
        ./types/VersionRange.dhall 
    , License =
        ./types/License.dhall 
    , Compiler =
        ./types/Compiler.dhall 
    }
, defaults =
    { compiler-options = ./defaults/CompilerOptions.dhall  }
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
}
