    let empty-package = ./dhall/empty-package.dhall 

in  let common-deps =
          { Cabal      = { bounds = majorVersion [ +2, +0 ], package = "Cabal" }
          , base       = { bounds = majorVersion [ +4, +10 ], package = "base" }
          , bytestring =
              { bounds = majorVersion [ +0, +10 ], package = "bytestring" }
          , dhall      = { bounds = majorVersion [ +1, +8 ], package = "dhall" }
          , text       = { bounds = majorVersion [ +1, +2 ], package = "text" }
          }

in  let gitHub-project = ./dhall/gitHubProject.dhall 

in    gitHub-project { owner = "ocharles", repo = "dhall-to-cabal" }
    ⫽ { executables =
          [ { autogen-modules      = [] : List Text
            , build-dependencies   =
                [ common-deps.base
                , { bounds = anyVersion, package = "dhall-to-cabal" }
                , { bounds  = majorVersion [ +0, +13, +2 ]
                  , package = "optparse-applicative"
                  }
                , common-deps.text
                , common-deps.dhall
                , common-deps.Cabal
                ]
            , build-tool-depends   =
                [] : List
                     { component : Text
                     , package   : Text
                     , version   : VersionRange
                     }
            , build-tools          =
                [] : List { exe : Text, version : VersionRange }
            , buildable            = True
            , c-sources            = [] : List Text
            , cc-options           = [] : List Text
            , compiler-options     =
                { GHC = { build-options = [] : List Text } }
            , cpp-options          = [] : List Text
            , default-extensions   = [] : List <>
            , default-language     =
                [] : Optional < Haskell2010 : {} | Haskell98 : {} >
            , extra-framework-dirs = [] : List Text
            , extra-ghci-libraries = [] : List Text
            , extra-lib-dirs       = [] : List Text
            , extra-libraries      = [] : List Text
            , frameworks           = [] : List Text
            , hs-source-dirs       = [ "exe" ]
            , include              = [] : List Text
            , include-dirs         = [] : List Text
            , install-includes     = [] : List Text
            , js-sources           = [] : List Text
            , ld-options           = [] : List Text
            , main-is              = "Main.hs"
            , name                 = "dhall-to-cabal"
            , other-extensions     = [] : List <>
            , other-languages      =
                [] : List < Haskell2010 : {} | Haskell98 : {} >
            , other-modules        = [] : List Text
            , pkgconfig-depends    =
                [] : List { name : Text, version : VersionRange }
            , profiling-options    =
                { GHC = { build-options = [] : List Text } }
            , scope                = < Public = {=} | Private : {} >
            , shared-options       =
                { GHC = { build-options = [] : List Text } }
            }
          ]
      , library     =
          [ { autogen-modules      = [] : List Text
            , build-dependencies   =
                [ common-deps.base
                , common-deps.Cabal
                , common-deps.dhall
                , common-deps.text
                , common-deps.bytestring
                , { bounds = majorVersion [ +0, +5 ], package = "containers" }
                , { bounds = majorVersion [ +0, +12 ], package = "vector" }
                , { bounds = majorVersion [ +1, +7 ], package = "trifecta" }
                , { bounds = majorVersion [ +0, +3 ], package = "text-format" }
                , { bounds  = majorVersion [ +0, +5, +2 ]
                  , package = "transformers"
                  }
                ]
            , build-tool-depends   =
                [] : List
                     { component : Text
                     , package   : Text
                     , version   : VersionRange
                     }
            , build-tools          =
                [] : List { exe : Text, version : VersionRange }
            , buildable            = True
            , c-sources            = [] : List Text
            , cc-options           = [] : List Text
            , compiler-options     =
                { GHC =
                    { build-options = [ "-Wall", "-fno-warn-name-shadowing" ] }
                }
            , cpp-options          = [] : List Text
            , default-extensions   = [] : List <>
            , default-language     =
                [] : Optional < Haskell2010 : {} | Haskell98 : {} >
            , exposed-modules      = [ "Distribution.Package.Dhall" ]
            , extra-framework-dirs = [] : List Text
            , extra-ghci-libraries = [] : List Text
            , extra-lib-dirs       = [] : List Text
            , extra-libraries      = [] : List Text
            , frameworks           = [] : List Text
            , hs-source-dirs       = [ "lib" ]
            , include              = [] : List Text
            , include-dirs         = [] : List Text
            , install-includes     = [] : List Text
            , js-sources           = [] : List Text
            , ld-options           = [] : List Text
            , name                 = [] : Optional Text
            , other-extensions     = [] : List <>
            , other-languages      =
                [] : List < Haskell2010 : {} | Haskell98 : {} >
            , other-modules        = [ "Dhall.Extra" ]
            , pkgconfig-depends    =
                [] : List { name : Text, version : VersionRange }
            , profiling-options    =
                { GHC = { build-options = [] : List Text } }
            , reexported-modules   =
                [] : List
                     { name     : Text
                     , original : { name : Text, package : Optional Text }
                     }
            , shared-options       =
                { GHC = { build-options = [] : List Text } }
            }
          ] : Optional ./dhall/types/Library 
      , license     =
            < MIT               = {=}
            | AGPL              : Optional (List Natural)
            | AllRightsReserved : {}
            | Apache            : Optional (List Natural)
            | BSD2              : {}
            | BSD3              : {}
            | BSD4              : {}
            | GPL               : Optional (List Natural)
            | ISC               : {}
            | LGPL              : Optional (List Natural)
            | MPL               : List Natural
            | Other             : {}
            | PublicDomain      : {}
            | Unspecified       : {}
            >
          : < AGPL              : Optional (List Natural)
            | AllRightsReserved : {}
            | Apache            : Optional (List Natural)
            | BSD2              : {}
            | BSD3              : {}
            | BSD4              : {}
            | GPL               : Optional (List Natural)
            | ISC               : {}
            | LGPL              : Optional (List Natural)
            | MIT               : {}
            | MPL               : List Natural
            | Other             : {}
            | PublicDomain      : {}
            | Unspecified       : {}
            >
      , package     = { name = "dhall-to-cabal", version = [ +0, +1, +0 ] }
      , tests       =
          [ { autogen-modules      = [] : List Text
            , build-dependencies   =
                [ common-deps.bytestring
                , common-deps.base
                , common-deps.Cabal
                , common-deps.text
                , { bounds = majorVersion [ +0, +11 ], package = "tasty" }
                , { bounds = majorVersion [ +1, +4 ], package = "filepath" }
                , { bounds = anyVersion, package = "dhall-to-cabal" }
                , { bounds = majorVersion [ +2, +3 ], package = "tasty-golden" }
                ]
            , build-tool-depends   =
                [] : List
                     { component : Text
                     , package   : Text
                     , version   : VersionRange
                     }
            , build-tools          =
                [] : List { exe : Text, version : VersionRange }
            , buildable            = True
            , c-sources            = [] : List Text
            , cc-options           = [] : List Text
            , compiler-options     =
                { GHC = { build-options = [] : List Text } }
            , cpp-options          = [] : List Text
            , default-extensions   = [] : List <>
            , default-language     =
                [] : Optional < Haskell2010 : {} | Haskell98 : {} >
            , extra-framework-dirs = [] : List Text
            , extra-ghci-libraries = [] : List Text
            , extra-lib-dirs       = [] : List Text
            , extra-libraries      = [] : List Text
            , frameworks           = [] : List Text
            , hs-source-dirs       = [ "golden-tests" ]
            , include              = [] : List Text
            , include-dirs         = [] : List Text
            , install-includes     = [] : List Text
            , js-sources           = [] : List Text
            , ld-options           = [] : List Text
            , main-is              = "GoldenTests.hs"
            , name                 = "golden-tests"
            , other-extensions     = [] : List <>
            , other-languages      =
                [] : List < Haskell2010 : {} | Haskell98 : {} >
            , other-modules        = [] : List Text
            , pkgconfig-depends    =
                [] : List { name : Text, version : VersionRange }
            , profiling-options    =
                { GHC = { build-options = [] : List Text } }
            , shared-options       =
                { GHC = { build-options = [] : List Text } }
            }
          ]
      }
