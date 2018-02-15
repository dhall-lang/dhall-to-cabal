  λ(VersionRange : Type)
→ { author :
      Text
  , benchmarks :
      List
      { benchmark :
          ./Guarded.dhall  VersionRange (./Benchmark.dhall  VersionRange)
      , name :
          Text
      }
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
      Optional { setup-depends : List (./Dependency.dhall  VersionRange) }
  , data-dir :
      Text
  , data-files :
      List Text
  , description :
      Text
  , executables :
      List
      { executable :
          ./Guarded.dhall  VersionRange (./Executable.dhall  VersionRange)
      , name :
          Text
      }
  , extra-doc-files :
      List Text
  , extra-source-files :
      List Text
  , extra-tmp-files :
      List Text
  , flags :
      List { default : Bool, description : Text, manual : Bool, name : Text }
  , foreign-libraries :
      List
      { foreign-lib :
          ./Guarded.dhall  VersionRange (./ForeignLibrary.dhall  VersionRange)
      , name :
          Text
      }
  , homepage :
      Text
  , library :
      Optional (./Guarded.dhall  VersionRange (./Library.dhall  VersionRange))
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
      List
      { library :
          ./Guarded.dhall  VersionRange (./Library.dhall  VersionRange)
      , name :
          Text
      }
  , synopsis :
      Text
  , test-suites :
      List
      { name :
          Text
      , test-suite :
          ./Guarded.dhall  VersionRange (./TestSuite.dhall  VersionRange)
      }
  , tested-with :
      List { compiler : ./Compiler.dhall , version : VersionRange }
  , version :
      List Natural
  , x-fields :
      List { _1 : Text, _2 : Text }
  }
