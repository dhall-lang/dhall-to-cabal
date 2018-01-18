{ benchmarks        = [] : List { main-is : Text, name : Text }
, cabal-version     = [ +2, +0 ]
, executables       =
    [ { build-dependencies =
          [ { package = "base" }
          , { package = "Cabal" }
          , { package = "dhall" }
          , { package = "text" }
          , { package = "containers" }
          , { package = "optparse-applicative" }
          ]
      , main-is            = "Main.hs"
      , name               = "dhall-to-cabal"
      , other-modules      = [ "Distribution.Package.Dhall" ]
      }
    ]
, foreign-libraries = [] : List { name : Text, type : < Shared : {} > }
, library           =
    [] : Optional
         { build-dependencies : List { package : Text }
         , name               : Optional Text
         , other-modules      : List Text
         }
, package           = { name = "dhall-to-cabal", version = [ +0, +1, +0 ] }
, source-repos      = [] : List {}
, sub-libraries     =
    [] : List
         { build-dependencies : List { package : Text }
         , name               : Optional Text
         , other-modules      : List Text
         }
, tests             =
    [] : List
         { build-dependencies : List { package : Text }
         , main-is            : Text
         , name               : Text
         , other-modules      : List Text
         }
, x-fields          = [] : List { _1 : Text, _2 : Text }
}
