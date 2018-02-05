{ author :
    Text
, benchmarks :
    List { benchmark : ./Guarded.dhall  ./Benchmark.dhall , name : Text }
, bug-reports :
    Text
, build-type :
    Optional ./BuildType.dhall 
, cabal-version :
    List Natural
, category :
    Text
, copyright :
    Text
, custom-setup :
    Optional { setup-depends : List ./Dependency.dhall  }
, data-dir :
    Text
, data-files :
    List Text
, description :
    Text
, executables :
    List { executable : ./Guarded.dhall  ./Executable.dhall , name : Text }
, extra-doc-files :
    List Text
, extra-source-files :
    List Text
, extra-tmp-files :
    List Text
, flags :
    List { default : Bool, description : Text, manual : Bool, name : Text }
, foreign-libraries :
    List { foreign-lib : ./Guarded.dhall  ./ForeignLibrary.dhall , name : Text }
, homepage :
    Text
, library :
    Optional (./Guarded.dhall  ./Library.dhall )
, license :
    ./License.dhall 
, license-files :
    List Text
, maintainer :
    Text
, name :
    Text
, package-url :
    Text
, source-repos :
    List ./SourceRepo.dhall 
, stability :
    Text
, sub-libraries :
    List { library : ./Guarded.dhall  ./Library.dhall , name : Text }
, synopsis :
    Text
, test-suites :
    List { name : Text, test-suite : ./Guarded.dhall  ./TestSuite.dhall  }
, tested-with :
    List { compiler : ./Compiler.dhall , version : VersionRange }
, version :
    List Natural
, x-fields :
    List { _1 : Text, _2 : Text }
}
