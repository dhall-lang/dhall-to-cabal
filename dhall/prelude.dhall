{ types =
    { Benchmark =
        ./types/Benchmark.dhall 
    , BuildType =
        ./types/BuildType.dhall 
    , BuildTypes =
        constructors ./types/BuildType.dhall 
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
    }
, defaults =
    { compiler-options = ./defaults/CompilerOptions.dhall  }
, anyVersion =
    ./types/VersionRange/AnyVersion.dhall 
, earlierVersion =
    ./types/VersionRange/EarlierVersion.dhall 
, intersectVersionRanges =
    ./types/VersionRange/IntersectVersionRanges.dhall 
, orLaterVersion =
    ./types/VersionRange/OrLaterVersion.dhall 
, thisVersion =
    ./types/VersionRange/ThisVersion.dhall 
, withinVersion =
    ./types/VersionRange/WithinVersion.dhall 
, v =
    ./types/Version/v.dhall 
}
