  λ(VersionRange : Type)
→ (   { author =
          ""
      , flags =
          [] : List ../types/Flag.dhall 
      , benchmarks =
          [] : List
               { benchmark :
                   ../types/Guarded.dhall 
                   VersionRange
                   (../types/Benchmark.dhall  VersionRange)
               , name :
                   Text
               }
      , bug-reports =
          ""
      , build-type =
          [ (constructors ../types/BuildType.dhall ).Simple {=}
          ] : Optional ../types/BuildType.dhall 
      , cabal-version =
          [ +2, +0 ]
      , category =
          ""
      , copyright =
          ""
      , data-dir =
          ""
      , data-files =
          [] : List Text
      , description =
          ""
      , executables =
          [] : List
               { executable :
                   ../types/Guarded.dhall 
                   VersionRange
                   (../types/Executable.dhall  VersionRange)
               , name :
                   Text
               }
      , extra-doc-files =
          [] : List Text
      , extra-source-files =
          [] : List Text
      , extra-tmp-files =
          [] : List Text
      , foreign-libraries =
          [] : List
               { foreign-lib :
                   ../types/Guarded.dhall 
                   VersionRange
                   (../types/ForeignLibrary.dhall  VersionRange)
               , name :
                   Text
               }
      , homepage =
          ""
      , library =
          [] : Optional
               ( ../types/Guarded.dhall 
                 VersionRange
                 (../types/Library.dhall  VersionRange)
               )
      , license =
          (constructors ../types/License.dhall ).Unspecified {=}
      , license-files =
          [] : List Text
      , maintainer =
          ""
      , name =
          ""
      , version =
          [] : List Natural
      , package-url =
          ""
      , source-repos =
          [] : List ../types/SourceRepo.dhall 
      , stability =
          ""
      , sub-libraries =
          [] : List
               { library :
                   ../types/Guarded.dhall 
                   VersionRange
                   (../types/Library.dhall  VersionRange)
               , name :
                   Text
               }
      , synopsis =
          ""
      , test-suites =
          [] : List
               { name :
                   Text
               , test-suite :
                   ../types/Guarded.dhall 
                   VersionRange
                   (../types/TestSuite.dhall  VersionRange)
               }
      , tested-with =
          [] : List
               { compiler : ../types/Compiler.dhall , version : VersionRange }
      , x-fields =
          [] : List { _1 : Text, _2 : Text }
      , custom-setup =
          [] : Optional (../types/SetupBuildInfo.dhall  VersionRange)
      }
    : ../types/Package.dhall  VersionRange
  )
