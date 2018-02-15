  λ(builtin : ./types/builtin.dhall )
→ { default =
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
      { majorVersions =
            λ(package : Text)
          → λ(versions : List (List Natural))
          → { package =
                package
            , bounds =
                Optional/fold
                ./types/VersionRange.dhall 
                ( List/fold
                  (List Natural)
                  versions
                  (Optional ./types/VersionRange.dhall )
                  (   λ(v : List Natural)
                    → λ(r : Optional ./types/VersionRange.dhall )
                    → Optional/fold
                      ./types/VersionRange.dhall 
                      r
                      (Optional ./types/VersionRange.dhall )
                      (   λ(r : ./types/VersionRange.dhall )
                        → [ ./types/VersionRange/UnionVersionRanges.dhall 
                            (./types/VersionRange/MajorBoundVersion.dhall  v)
                            r
                          ] : Optional ./types/VersionRange.dhall 
                      )
                      ( [ ./types/VersionRange/MajorBoundVersion.dhall  v
                        ] : Optional ./types/VersionRange.dhall 
                      )
                  )
                  ([] : Optional ./types/VersionRange.dhall )
                )
                ./types/VersionRange.dhall 
                (λ(a : ./types/VersionRange.dhall ) → a)
                ./types/VersionRange/NoVersion.dhall 
            }
      }
  }
