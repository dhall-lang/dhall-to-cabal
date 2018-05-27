    let prelude = ../../dhall/prelude.dhall 

in  let types = ../../dhall/types.dhall 

in  { author =
        ""
    , benchmarks =
        [] : List { benchmark : types.Config → types.Benchmark, name : Text }
    , bug-reports =
        ""
    , build-type =
        [] : Optional types.BuildType
    , cabal-version =
        prelude.v "2.0"
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
        < Unspecified =
            {=}
        | GPL :
            Optional types.Version
        | AGPL :
            Optional types.Version
        | LGPL :
            Optional types.Version
        | BSD2 :
            {}
        | BSD3 :
            {}
        | BSD4 :
            {}
        | MIT :
            {}
        | ISC :
            {}
        | MPL :
            types.Version
        | Apache :
            Optional types.Version
        | PublicDomain :
            {}
        | AllRightsReserved :
            {}
        | Other :
            {}
        >
    , license-files =
        [] : List Text
    , maintainer =
        ""
    , name =
        "test"
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
        prelude.v "1.0"
    , x-fields =
        [] : List { _1 : Text, _2 : Text }
    }