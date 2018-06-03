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
    { majorVersions =
            let majorVersions
                :   Text
                  → List ./types/Version.dhall 
                  → { package : Text, bounds : ./types/VersionRange.dhall  }
                =   λ ( package
                      : Text
                      )
                  → λ(versions : List ./types/Version.dhall )
                  → { package =
                        package
                    , bounds =
                        Optional/fold
                        ./types/VersionRange.dhall 
                        ( List/fold
                          ./types/Version.dhall 
                          versions
                          (Optional ./types/VersionRange.dhall )
                          (   λ ( v
                                : ./types/Version.dhall 
                                )
                            → λ(r : Optional ./types/VersionRange.dhall )
                            → Optional/fold
                              ./types/VersionRange.dhall 
                              r
                              (Optional ./types/VersionRange.dhall )
                              (   λ ( r
                                    : ./types/VersionRange.dhall 
                                    )
                                → [ ./types/VersionRange/UnionVersionRanges.dhall 
                                    ( ./types/VersionRange/MajorBoundVersion.dhall 
                                      v
                                    )
                                    r
                                  ] : Optional ./types/VersionRange.dhall 
                              )
                              ( [ ./types/VersionRange/MajorBoundVersion.dhall 
                                  v
                                ] : Optional ./types/VersionRange.dhall 
                              )
                          )
                          ([] : Optional ./types/VersionRange.dhall )
                        )
                        ./types/VersionRange.dhall 
                        (λ(a : ./types/VersionRange.dhall ) → a)
                        ./types/VersionRange/NoVersion.dhall 
                    }
        
        in  majorVersions
    , GitHub-project =
        ./GitHub-project.dhall
    , GitHubWithSourceRepo-project =
        ./GitHubWithSourceRepo-project.dhall
    }
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
