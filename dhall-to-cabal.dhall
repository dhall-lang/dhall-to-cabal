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
          [   ./dhall/defaults/BuildInfo 
            ⫽ { build-dependencies =
                  [ common-deps.base
                  , { bounds = anyVersion, package = "dhall-to-cabal" }
                  , { bounds  = majorVersion [ +0, +13, +2 ]
                    , package = "optparse-applicative"
                    }
                  , common-deps.text
                  , common-deps.dhall
                  , common-deps.Cabal
                  ]
              , hs-source-dirs     = [ "exe" ]
              , main-is            = "Main.hs"
              , name               = "dhall-to-cabal"
              , scope              = < Public = {=} | Private : {} >
              }
          ]
      , library     =
          [   ./dhall/defaults/BuildInfo 
            ⫽ { build-dependencies =
                  [ common-deps.base
                  , common-deps.Cabal
                  , common-deps.dhall
                  , common-deps.text
                  , common-deps.bytestring
                  , { bounds = majorVersion [ +0, +5 ], package = "containers" }
                  , { bounds = majorVersion [ +0, +12 ], package = "vector" }
                  , { bounds = majorVersion [ +1, +7 ], package = "trifecta" }
                  , { bounds  = majorVersion [ +0, +3 ]
                    , package = "text-format"
                    }
                  , { bounds  = majorVersion [ +0, +5, +2 ]
                    , package = "transformers"
                    }
                  ]
              , compiler-options   =
                    ./dhall/defaults/CompilerOptions 
                  ⫽ { GHC =
                        { build-options =
                            [ "-Wall", "-fno-warn-name-shadowing" ]
                        }
                    }
              , exposed-modules    = [ "Distribution.Package.Dhall" ]
              , hs-source-dirs     = [ "lib" ]
              , name               = [] : Optional Text
              , other-modules      = [ "Dhall.Extra" ]
              , reexported-modules =
                  [] : List
                       { name     : Text
                       , original : { name : Text, package : Optional Text }
                       }
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
          [   ./dhall/defaults/BuildInfo 
            ⫽ { build-dependencies =
                  [ common-deps.bytestring
                  , common-deps.base
                  , common-deps.Cabal
                  , common-deps.text
                  , { bounds = majorVersion [ +0, +11 ], package = "tasty" }
                  , { bounds = majorVersion [ +1, +4 ], package = "filepath" }
                  , { bounds = anyVersion, package = "dhall-to-cabal" }
                  , { bounds  = majorVersion [ +2, +3 ]
                    , package = "tasty-golden"
                    }
                  ]
              , hs-source-dirs     = [ "golden-tests" ]
              , main-is            = "GoldenTests.hs"
              , name               = "golden-tests"
              }
          ]
      }
