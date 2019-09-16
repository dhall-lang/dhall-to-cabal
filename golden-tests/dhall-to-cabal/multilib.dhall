let prelude = ./../../dhall/prelude.dhall

let types = ./../../dhall/types.dhall

in    prelude.defaults.Package
    ⫽ { name =
          "multilib"
      , version =
          prelude.v "0"
      , cabal-version =
          prelude.v "3.0"
      , library =
          Some
          (   λ(config : types.Config)
            →   prelude.defaults.MainLibrary
              ⫽ { build-depends =
                    [ { bounds =
                          prelude.anyVersion
                      , library-names =
                          [ types.LibraryName.main-library ]
                      , package =
                          "A"
                      }
                    , { bounds =
                          prelude.unionVersionRanges
                          (prelude.thisVersion (prelude.v "3"))
                          (prelude.laterVersion (prelude.v "3"))
                      , library-names =
                          [ types.LibraryName.sub-library "b1"
                          , types.LibraryName.sub-library "b2"
                          , types.LibraryName.sub-library "b3"
                          ]
                      , package =
                          "B"
                      }
                    , { bounds =
                          prelude.earlierVersion (prelude.v "3.5")
                      , library-names =
                          [ types.LibraryName.sub-library "b2" ]
                      , package =
                          "B"
                      }
                    , { bounds =
                          prelude.anyVersion
                      , library-names =
                          [ types.LibraryName.main-library
                          , types.LibraryName.sub-library "c1"
                          ]
                      , package =
                          "C"
                      }
                    , { bounds =
                          prelude.anyVersion
                      , library-names =
                          [] : List types.LibraryName
                      , package =
                          "D"
                      }
                    ]
                , default-language =
                    Some types.Language.Haskell2010
                }
          )
      }