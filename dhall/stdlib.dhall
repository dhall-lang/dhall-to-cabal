{ default =
    { Executable =
        ./defaults/Executable.dhall 
    , Library =
        ./defaults/Library.dhall 
    , TestSuite =
        ./defaults/TestSuite.dhall 
    , SourceRepo =
        ./defaults/SourceRepo.dhall 
    , CompilerOptions =
        ./defaults/CompilerOptions.dhall 
    , Package =
        ./defaults/Package.dhall 
    }
, `constructors` =
    { Archs =
        constructors ./types/Arch.dhall 
    , BuildTypes =
        constructors ./types/BuildType.dhall 
    , Compilers =
        constructors ./types/Compiler.dhall 
    , Extensions =
        constructors ./types/Extension.dhall 
    , Licenses =
        constructors ./types/License.dhall 
    , OSs =
        constructors ./types/OS.dhall 
    , RepoKinds =
        constructors ./types/RepoKind.dhall 
    , RepoTypes =
        constructors ./types/RepoType.dhall 
    , Scopes =
        constructors ./types/Scope.dhall 
    , TestTypes =
        constructors ./types/TestType.dhall 
    }
, GitHub-project =
    ./GitHub-project.dhall 
, unconditional =
    ./unconditional.dhall 
}
