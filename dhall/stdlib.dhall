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
, dependency =
        let majorBoundVersion = λ(v : Text) → "^>= ${v}"
    
    in  let noVersion = "> 1 && < 1"
    
    in  let anyVersion = "-any"
    
    in  { majorBoundVersion =
            majorBoundVersion
        , noVersion =
            noVersion
        , anyVersion =
            anyVersion
        , majorVersions =
              λ(package : Text)
            → λ(versions : List Text)
            → { package =
                  package
              , bounds =
                  Optional/fold
                  ./types/VersionRange.dhall 
                  ( List/fold
                    Text
                    versions
                    (Optional ./types/VersionRange.dhall )
                    (   λ(v : Text)
                      → λ(r : Optional ./types/VersionRange.dhall )
                      → Optional/fold
                        ./types/VersionRange.dhall 
                        r
                        (Optional ./types/VersionRange.dhall )
                        (   λ(r : ./types/VersionRange.dhall )
                          → [ "${majorBoundVersion v} || ${r}"
                            ] : Optional ./types/VersionRange.dhall 
                        )
                        ( [ majorBoundVersion v ] : Optional
                                                    ./types/VersionRange.dhall 
                        )
                    )
                    ([] : Optional ./types/VersionRange.dhall )
                  )
                  ./types/VersionRange.dhall 
                  (λ(a : ./types/VersionRange.dhall ) → a)
                  noVersion
              }
        }
}
