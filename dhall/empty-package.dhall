{ author             = ""
, benchmarks         =
    [] : List
         { autogen-modules      : List Text
         , build-dependencies   : List { bounds : VersionRange, package : Text }
         , build-tool-depends   :
             List { component : Text, package : Text, version : VersionRange }
         , build-tools          : List { exe : Text, version : VersionRange }
         , buildable            : Bool
         , c-sources            : List Text
         , cc-options           : List Text
         , compiler-options     : { GHC : { build-options : List Text } }
         , cpp-options          : List Text
         , default-extensions   : List <>
         , default-language     : Optional < Haskell2010 : {} | Haskell98 : {} >
         , extra-framework-dirs : List Text
         , extra-ghci-libraries : List Text
         , extra-lib-dirs       : List Text
         , extra-libraries      : List Text
         , frameworks           : List Text
         , hs-source-dirs       : List Text
         , include              : List Text
         , include-dirs         : List Text
         , install-includes     : List Text
         , js-sources           : List Text
         , ld-options           : List Text
         , main-is              : Text
         , name                 : Text
         , other-extensions     : List <>
         , other-languages      : List < Haskell2010 : {} | Haskell98 : {} >
         , other-modules        : List Text
         , pkgconfig-depends    : List { name : Text, version : VersionRange }
         , profiling-options    : { GHC : { build-options : List Text } }
         , shared-options       : { GHC : { build-options : List Text } }
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
         { autogen-modules      : List Text
         , build-dependencies   : List { bounds : VersionRange, package : Text }
         , build-tool-depends   :
             List { component : Text, package : Text, version : VersionRange }
         , build-tools          : List { exe : Text, version : VersionRange }
         , buildable            : Bool
         , c-sources            : List Text
         , cc-options           : List Text
         , compiler-options     : { GHC : { build-options : List Text } }
         , cpp-options          : List Text
         , default-extensions   : List <>
         , default-language     : Optional < Haskell2010 : {} | Haskell98 : {} >
         , extra-framework-dirs : List Text
         , extra-ghci-libraries : List Text
         , extra-lib-dirs       : List Text
         , extra-libraries      : List Text
         , frameworks           : List Text
         , hs-source-dirs       : List Text
         , include              : List Text
         , include-dirs         : List Text
         , install-includes     : List Text
         , js-sources           : List Text
         , ld-options           : List Text
         , main-is              : Text
         , name                 : Text
         , other-extensions     : List <>
         , other-languages      : List < Haskell2010 : {} | Haskell98 : {} >
         , other-modules        : List Text
         , pkgconfig-depends    : List { name : Text, version : VersionRange }
         , profiling-options    : { GHC : { build-options : List Text } }
         , shared-options       : { GHC : { build-options : List Text } }
         }
, extra-doc-files    = [] : List Text
, extra-source-files = [] : List Text
, extra-temp-files   = [] : List Text
, foreign-libraries  =
    [] : List
         { autogen-modules      : List Text
         , build-dependencies   : List { bounds : VersionRange, package : Text }
         , build-tool-depends   :
             List { component : Text, package : Text, version : VersionRange }
         , build-tools          : List { exe : Text, version : VersionRange }
         , buildable            : Bool
         , c-sources            : List Text
         , cc-options           : List Text
         , compiler-options     : { GHC : { build-options : List Text } }
         , cpp-options          : List Text
         , default-extensions   : List <>
         , default-language     : Optional < Haskell2010 : {} | Haskell98 : {} >
         , extra-framework-dirs : List Text
         , extra-ghci-libraries : List Text
         , extra-lib-dirs       : List Text
         , extra-libraries      : List Text
         , frameworks           : List Text
         , hs-source-dirs       : List Text
         , include              : List Text
         , include-dirs         : List Text
         , install-includes     : List Text
         , js-sources           : List Text
         , ld-options           : List Text
         , name                 : Text
         , other-extensions     : List <>
         , other-languages      : List < Haskell2010 : {} | Haskell98 : {} >
         , other-modules        : List Text
         , pkgconfig-depends    : List { name : Text, version : VersionRange }
         , profiling-options    : { GHC : { build-options : List Text } }
         , shared-options       : { GHC : { build-options : List Text } }
         , type                 : < Shared : {} >
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
         { autogen-modules      : List Text
         , build-dependencies   : List { bounds : VersionRange, package : Text }
         , build-tool-depends   :
             List { component : Text, package : Text, version : VersionRange }
         , build-tools          : List { exe : Text, version : VersionRange }
         , buildable            : Bool
         , c-sources            : List Text
         , cc-options           : List Text
         , compiler-options     : { GHC : { build-options : List Text } }
         , cpp-options          : List Text
         , default-extensions   : List <>
         , default-language     : Optional < Haskell2010 : {} | Haskell98 : {} >
         , exposed-modules      : List Text
         , extra-framework-dirs : List Text
         , extra-ghci-libraries : List Text
         , extra-lib-dirs       : List Text
         , extra-libraries      : List Text
         , frameworks           : List Text
         , hs-source-dirs       : List Text
         , include              : List Text
         , include-dirs         : List Text
         , install-includes     : List Text
         , js-sources           : List Text
         , ld-options           : List Text
         , name                 : Optional Text
         , other-extensions     : List <>
         , other-languages      : List < Haskell2010 : {} | Haskell98 : {} >
         , other-modules        : List Text
         , pkgconfig-depends    : List { name : Text, version : VersionRange }
         , profiling-options    : { GHC : { build-options : List Text } }
         , shared-options       : { GHC : { build-options : List Text } }
         }
, synopsis           = ""
, tested-with        =
    [] : List { compiler : < GHC : {} >, version : VersionRange }
, tests              =
    [] : List
         { autogen-modules      : List Text
         , build-dependencies   : List { bounds : VersionRange, package : Text }
         , build-tool-depends   :
             List { component : Text, package : Text, version : VersionRange }
         , build-tools          : List { exe : Text, version : VersionRange }
         , buildable            : Bool
         , c-sources            : List Text
         , cc-options           : List Text
         , compiler-options     : { GHC : { build-options : List Text } }
         , cpp-options          : List Text
         , default-extensions   : List <>
         , default-language     : Optional < Haskell2010 : {} | Haskell98 : {} >
         , extra-framework-dirs : List Text
         , extra-ghci-libraries : List Text
         , extra-lib-dirs       : List Text
         , extra-libraries      : List Text
         , frameworks           : List Text
         , hs-source-dirs       : List Text
         , include              : List Text
         , include-dirs         : List Text
         , install-includes     : List Text
         , js-sources           : List Text
         , ld-options           : List Text
         , main-is              : Text
         , name                 : Text
         , other-extensions     : List <>
         , other-languages      : List < Haskell2010 : {} | Haskell98 : {} >
         , other-modules        : List Text
         , pkgconfig-depends    : List { name : Text, version : VersionRange }
         , profiling-options    : { GHC : { build-options : List Text } }
         , shared-options       : { GHC : { build-options : List Text } }
         }
, x-fields           = [] : List { _1 : Text, _2 : Text }
}
