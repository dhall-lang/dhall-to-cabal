    let prelude = ../../dhall/prelude.dhall 

in  let types = ../../dhall/types.dhall 

in  { author =
        ""
    , benchmarks =
        [] : List { benchmark : types.Config → types.Benchmark, name : Text }
    , bug-reports =
        ""
    , build-type =
        [ prelude.types.BuildTypes.Simple {=} ] : Optional types.BuildType
    , cabal-version =
        prelude.v "2.2"
    , category =
        ""
    , copyright =
        ""
    , custom-setup =
        [] : Optional types.CustomSetup
    , data-dir =
        ""
    , data-files =
        [] : List Text
    , description =
        ""
    , executables =
        [] : List { executable : types.Config → types.Executable, name : Text }
    , extra-doc-files =
        [] : List Text
    , extra-source-files =
        [] : List Text
    , extra-tmp-files =
        [] : List Text
    , flags =
        [] : List
             { default : Bool, description : Text, manual : Bool, name : Text }
    , foreign-libraries =
        [] : List
             { foreign-lib : types.Config → types.ForeignLibrary, name : Text }
    , homepage =
        ""
    , library =
        [] : Optional (types.Config → types.Library)
    , license =
        prelude.types.Licenses.SPDX
        ( prelude.SPDX.and
          ( prelude.SPDX.or
            ( prelude.SPDX.license
              (prelude.types.LicenseId.AGPL_3_0_or_later {=})
              ( [ prelude.types.LicenseExceptionId.Classpath_exception_2_0 {=}
                ] : Optional types.LicenseExceptionId
              )
            )
            ( prelude.SPDX.licenseVersionOrLater
              (prelude.types.LicenseId.Apache_2_0 {=})
              ([] : Optional types.LicenseExceptionId)
            )
          )
          ( prelude.SPDX.or
            ( prelude.SPDX.ref
              "MyFancyLicense"
              ([] : Optional types.LicenseExceptionId)
            )
            ( prelude.SPDX.refWithFile
              "MyFancierLicense"
              "LICENSE.txt"
              ([] : Optional types.LicenseExceptionId)
            )
          )
        )
    , license-files =
        [] : List Text
    , maintainer =
        ""
    , name =
        "foo"
    , package-url =
        ""
    , source-repos =
        [] : List
             { branch :
                 Optional Text
             , kind :
                 types.RepoKind
             , location :
                 Optional Text
             , module :
                 Optional Text
             , subdir :
                 Optional Text
             , tag :
                 Optional Text
             , type :
                 Optional types.RepoType
             }
    , stability =
        ""
    , sub-libraries =
        [] : List { library : types.Config → types.Library, name : Text }
    , synopsis =
        ""
    , test-suites =
        [] : List { name : Text, test-suite : types.Config → types.TestSuite }
    , tested-with =
        [] : List { compiler : types.Compiler, version : types.VersionRange }
    , version =
        prelude.v "0"
    , x-fields =
        [] : List { _1 : Text, _2 : Text }
    }