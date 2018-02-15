  λ(VersionRange : Type)
→ λ(builtin : ./types/builtin.dhall  VersionRange)
→ { default =
      { Executable =
          ./defaults/Executable.dhall  VersionRange
      , Library =
          ./defaults/Library.dhall  VersionRange
      , TestSuite =
          ./defaults/TestSuite.dhall  VersionRange
      , SourceRepo =
          ./defaults/SourceRepo.dhall 
      , CompilerOptions =
          ./defaults/CompilerOptions.dhall 
      , Package =
          ./defaults/Package.dhall  VersionRange
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
      ./GitHub-project.dhall  VersionRange
  , unconditional =
      ./unconditional.dhall  VersionRange
  , dependency =
      { majorVersions =
            λ(package : Text)
          → λ(versions : List (List Natural))
          → { package =
                package
            , bounds =
                Optional/fold
                VersionRange
                ( List/fold
                  (List Natural)
                  versions
                  (Optional VersionRange)
                  (   λ(v : List Natural)
                    → λ(r : Optional VersionRange)
                    → Optional/fold
                      VersionRange
                      r
                      (Optional VersionRange)
                      (   λ(r : VersionRange)
                        → [ builtin.unionVersionRanges
                            (builtin.majorBoundVersion v)
                            r
                          ] : Optional VersionRange
                      )
                      ([ builtin.majorBoundVersion v ] : Optional VersionRange)
                  )
                  ([] : Optional VersionRange)
                )
                VersionRange
                (λ(a : VersionRange) → a)
                builtin.noVersion
            }
      }
  }
