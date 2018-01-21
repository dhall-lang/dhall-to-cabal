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
          [ { build-dependencies =
                [ common-deps.base
                , { bounds = anyVersion, package = "dhall-to-cabal" }
                , { bounds  = majorVersion [ +0, +13, +2 ]
                  , package = "optparse-applicative"
                  }
                , common-deps.text
                , common-deps.dhall
                , common-deps.Cabal
                ]
            , build-tools        =
                [] : List { exe : Text, version : VersionRange }
            , buildable          = True
            , hs-source-dirs     = [ "exe" ]
            , main-is            = "Main.hs"
            , name               = "dhall-to-cabal"
            , other-modules      = [] : List Text
            }
          ]
      , library     =
          [ { build-dependencies =
                [ common-deps.base
                , common-deps.Cabal
                , common-deps.dhall
                , common-deps.text
                , common-deps.bytestring
                , { bounds = majorVersion [ +0, +5 ], package = "containers" }
                , { bounds = majorVersion [ +0, +12 ], package = "vector" }
                , { bounds = majorVersion [ +1, +7 ], package = "trifecta" }
                , { bounds = majorVersion [ +0, +3 ], package = "text-format" }
                ]
            , build-tools        =
                [] : List { exe : Text, version : VersionRange }
            , buildable          = True
            , exposed-modules    = [ "Distribution.Package.Dhall" ]
            , hs-source-dirs     = [ "lib" ]
            , name               = [] : Optional Text
            , other-modules      = [] : List Text
            }
          ] : Optional ./dhall/types/Library 
      , package     = { name = "dhall-to-cabal", version = [ +0, +1, +0 ] }
      , tests       =
          [ { build-dependencies =
                [ common-deps.bytestring
                , common-deps.base
                , common-deps.Cabal
                , common-deps.text
                , { bounds = majorVersion [ +0, +11 ], package = "tasty" }
                , { bounds = majorVersion [ +1, +4 ], package = "filepath" }
                , { bounds = anyVersion, package = "dhall-to-cabal" }
                , { bounds = majorVersion [ +2, +3 ], package = "tasty-golden" }
                ]
            , build-tools        =
                [] : List { exe : Text, version : VersionRange }
            , buildable          = True
            , hs-source-dirs     = [ "golden-tests" ]
            , main-is            = "GoldenTests.hs"
            , name               = "golden-tests"
            , other-modules      = [] : List Text
            }
          ]
      }
