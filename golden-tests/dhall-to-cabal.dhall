{ benchmarks        = [] : List { main-is : Text, name : Text }
, cabal-version     = [ +2, +0 ]
, executables       =
    [ { build-dependencies =
          [ { package = "base" }
          , { package = "dhall-to-cabal" }
          , { package = "optparse-applicative" }
          , { package = "text" }
          , { package = "dhall" }
          , { package = "Cabal" }
          ]
      , hs-source-dirs     = [ "exe" ]
      , main-is            = "Main.hs"
      , name               = "dhall-to-cabal"
      , other-modules      = [] : List Text
      }
    ]
, foreign-libraries = [] : List { name : Text, type : < Shared : {} > }
, library           =
    [ { build-dependencies =
          [ { package = "base" }
          , { package = "Cabal" }
          , { package = "dhall" }
          , { package = "text" }
          , { package = "containers" }
          ]
      , exposed-modules    = [ "Distribution.Package.Dhall" ]
      , hs-source-dirs     = [ "lib" ]
      , name               = [] : Optional Text
      , other-modules      = [] : List Text
      }
    ] : Optional
        { build-dependencies : List { package : Text }
        , exposed-modules    : List Text
        , hs-source-dirs     : List Text
        , name               : Optional Text
        , other-modules      : List Text
        }
, package           = { name = "dhall-to-cabal", version = [ +0, +1, +0 ] }
, source-repos      = [] : List {}
, sub-libraries     =
    [] : List
         { build-dependencies : List { package : Text }
         , exposed-modules    : List Text
         , hs-source-dirs     : List Text
         , name               : Optional Text
         , other-modules      : List Text
         }
, tests             =
    [ { build-dependencies =
          [ { package = "tasty-golden" }
          , { package = "dhall-to-cabal" }
          , { package = "bytestring" }
          , { package = "filepath" }
          , { package = "base" }
          , { package = "tasty" }
          , { package = "Cabal" }
          , { package = "text" }
          ]
      , hs-source-dirs     = [ "golden-tests" ]
      , main-is            = "GoldenTests.hs"
      , name               = "golden-tests"
      , other-modules      = [] : List Text
      }
    ]
, x-fields          = [] : List { _1 : Text, _2 : Text }
}
