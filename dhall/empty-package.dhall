{ author            = ""
, benchmarks        = [] : List { main-is : Text, name : Text }
, build-type        =
    [] : Optional < Configure : {} | Custom : {} | Make : {} | Simple : {} >
, cabal-version     = [ +2, +0 ]
, copyright         = ""
, executables       =
    [] : List
         { build-dependencies : List { bounds : VersionRange, package : Text }
         , hs-source-dirs     : List Text
         , main-is            : Text
         , name               : Text
         , other-modules      : List Text
         }
, foreign-libraries = [] : List { name : Text, type : < Shared : {} > }
, library           =
    [] : Optional
         { build-dependencies : List { bounds : VersionRange, package : Text }
         , exposed-modules    : List Text
         , hs-source-dirs     : List Text
         , name               : Optional Text
         , other-modules      : List Text
         }
, license           =
      < GPL = [ [ +3, +0 ] ] : Optional (List Natural) >
    : < GPL : Optional (List Natural) >
, license-files     = [] : List Text
, maintainer        = ""
, package           = { name = "", version = [] : List Natural }
, source-repos      = [] : List {}
, stability         = ""
, sub-libraries     =
    [] : List
         { build-dependencies : List { bounds : VersionRange, package : Text }
         , exposed-modules    : List Text
         , hs-source-dirs     : List Text
         , name               : Optional Text
         , other-modules      : List Text
         }
, tests             =
    [] : List
         { build-dependencies : List { bounds : VersionRange, package : Text }
         , hs-source-dirs     : List Text
         , main-is            : Text
         , name               : Text
         , other-modules      : List Text
         }
, x-fields          = [] : List { _1 : Text, _2 : Text }
}
