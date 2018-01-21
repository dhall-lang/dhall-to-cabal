{ author             = ""
, benchmarks         =
    [] : List
         { build-dependencies : List { bounds : VersionRange, package : Text }
         , build-tool-depends :
             List { component : Text, package : Text, version : VersionRange }
         , build-tools        : List { exe : Text, version : VersionRange }
         , buildable          : Bool
         , compiler-options   : { GHC : { build-options : List Text } }
         , hs-source-dirs     : List Text
         , main-is            : Text
         , name               : Text
         , other-modules      : List Text
         }
, bug-reports        = ""
, build-type         =
    [ < Simple = {=} | Configure : {} | Custom : {} | Make : {} >
    ] : Optional < Configure : {} | Custom : {} | Make : {} | Simple : {} >
, cabal-version      = [ +2, +0 ]
, category           = ""
, copyright          = ""
, data-directory     = ""
, data-files         = [] : List Text
, description        = ""
, executables        =
    [] : List
         { build-dependencies : List { bounds : VersionRange, package : Text }
         , build-tool-depends :
             List { component : Text, package : Text, version : VersionRange }
         , build-tools        : List { exe : Text, version : VersionRange }
         , buildable          : Bool
         , compiler-options   : { GHC : { build-options : List Text } }
         , hs-source-dirs     : List Text
         , main-is            : Text
         , name               : Text
         , other-modules      : List Text
         }
, extra-doc-files    = [] : List Text
, extra-source-files = [] : List Text
, extra-temp-files   = [] : List Text
, foreign-libraries  =
    [] : List
         { build-dependencies : List { bounds : VersionRange, package : Text }
         , build-tool-depends :
             List { component : Text, package : Text, version : VersionRange }
         , build-tools        : List { exe : Text, version : VersionRange }
         , buildable          : Bool
         , compiler-options   : { GHC : { build-options : List Text } }
         , hs-source-dirs     : List Text
         , name               : Text
         , other-modules      : List Text
         , type               : < Shared : {} >
         }
, homepage           = ""
, library            = [] : Optional ./types/Library 
, license            =
      < GPL = [ [ +3, +0 ] ] : Optional (List Natural) >
    : < GPL : Optional (List Natural) >
, license-files      = [] : List Text
, maintainer         = ""
, package            = { name = "", version = [] : List Natural }
, package-url        = ""
, source-repos       =
    [] : List { location : Optional Text, type : Optional < Git : {} > }
, stability          = ""
, sub-libraries      =
    [] : List
         { build-dependencies : List { bounds : VersionRange, package : Text }
         , build-tool-depends :
             List { component : Text, package : Text, version : VersionRange }
         , build-tools        : List { exe : Text, version : VersionRange }
         , buildable          : Bool
         , compiler-options   : { GHC : { build-options : List Text } }
         , exposed-modules    : List Text
         , hs-source-dirs     : List Text
         , name               : Optional Text
         , other-modules      : List Text
         }
, synopsis           = ""
, tested-with        =
    [] : List { compiler : < GHC : {} >, version : VersionRange }
, tests              =
    [] : List
         { build-dependencies : List { bounds : VersionRange, package : Text }
         , build-tool-depends :
             List { component : Text, package : Text, version : VersionRange }
         , build-tools        : List { exe : Text, version : VersionRange }
         , buildable          : Bool
         , compiler-options   : { GHC : { build-options : List Text } }
         , hs-source-dirs     : List Text
         , main-is            : Text
         , name               : Text
         , other-modules      : List Text
         }
, x-fields           = [] : List { _1 : Text, _2 : Text }
}