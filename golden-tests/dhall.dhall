    let licenses = constructors ./dhall/types/License.dhall 

in  let TestTypes = constructors ./dhall/types/TestType.dhall 

in  let compilers = constructors ./dhall/types/Compiler.dhall 

in  let within =
            λ(min : List Natural)
          → λ(max : List Natural)
          → intersectVersionRanges (orLaterVersion min) (earlierVersion max)

in  let before
        : Text → List Natural → { bounds : VersionRange, package : Text }
        =   λ(package : Text)
          → λ(max : List Natural)
          → { bounds = earlierVersion max, package = package }

in  let bounded
        :   Text
          → List Natural
          → List Natural
          → { bounds : VersionRange, package : Text }
        =   λ(package : Text)
          → λ(min : List Natural)
          → λ(max : List Natural)
          → { bounds =
                intersectVersionRanges (orLaterVersion min) (earlierVersion max)
            , package =
                package
            }

in  let unguarded =
            λ(A : Type)
          → λ(a : A)
          → [ { guard =
                  λ(config : ./dhall/types/Config.dhall ) → True
              , body =
                  a
              }
            ]

in    ./dhall/defaults/Package.dhall 
    ⫽ { author =
          "Gabriel Gonzalez"
      , bug-reports =
          "https://github.com/dhall-lang/dhall-haskell/issues"
      , cabal-version =
          [ +1, +8, +0, +2 ]
      , category =
          "Compiler"
      , copyright =
          "2017 Gabriel Gonzalez"
      , description =
          ''
          Dhall is an explicitly typed configuration language that is not Turing
          complete.  Despite being Turing incomplete, Dhall is a real programming
          language with a type-checker and evaluator.
          
          Use this library to parse, type-check, evaluate, and pretty-print the Dhall
          configuration language.  This package also includes an executable which
          type-checks a Dhall file and reduces the file to a fully evaluated normal
          form.
          
          Read "Dhall.Tutorial" to learn how to use this library''
      , executables =
          [ { name =
                "dhall"
            , executable =
                unguarded
                ./dhall/types/Executable.dhall 
                (   ./dhall/defaults/Executable.dhall 
                  ⫽ { build-depends =
                        [ bounded "base" [ +4 ] [ +5 ]
                        , { bounds = anyVersion, package = "dhall" }
                        , bounded "optparse-generic" [ +1, +1, +1 ] [ +1, +3 ]
                        , { bounds = anyVersion, package = "prettyprinter" }
                        , bounded "text" [ +0, +11, +1, +0 ] [ +1, +3 ]
                        , bounded "trifecta" [ +1, +6 ] [ +1, +8 ]
                        ]
                    , compiler-options =
                          ./dhall/defaults/CompilerOptions.dhall 
                        ⫽ { GHC = [ "-Wall" ] }
                    , hs-source-dirs =
                        [ "dhall" ]
                    , main-is =
                        "Main.hs"
                    , other-modules =
                        [ "Paths_dhall" ]
                    }
                )
            }
          , { name =
                "dhall-format"
            , executable =
                unguarded
                ./dhall/types/Executable.dhall 
                (   ./dhall/defaults/Executable.dhall 
                  ⫽ { build-depends =
                        [ bounded "base" [ +4 ] [ +5 ]
                        , { bounds = anyVersion, package = "dhall" }
                        , bounded "optparse-generic" [ +1, +1, +1 ] [ +1, +3 ]
                        , bounded "prettyprinter" [ +1, +1, +1 ] [ +1, +2 ]
                        , bounded "system-filepath" [ +0, +3, +1 ] [ +0, +5 ]
                        , bounded "text" [ +0, +11, +1, +0 ] [ +1, +3 ]
                        , bounded "trifecta" [ +1, +6 ] [ +1, +8 ]
                        ]
                    , compiler-options =
                          ./dhall/defaults/CompilerOptions.dhall 
                        ⫽ { GHC = [ "-Wall" ] }
                    , hs-source-dirs =
                        [ "dhall-format" ]
                    , main-is =
                        "Main.hs"
                    , other-modules =
                        [ "Paths_dhall" ]
                    }
                )
            }
          , { name =
                "dhall-hash"
            , executable =
                unguarded
                ./dhall/types/Executable.dhall 
                (   ./dhall/defaults/Executable.dhall 
                  ⫽ { build-depends =
                        [ bounded "base" [ +4 ] [ +5 ]
                        , { bounds = anyVersion, package = "dhall" }
                        , bounded "optparse-generic" [ +1, +1, +1 ] [ +1, +3 ]
                        , bounded "text" [ +0, +11, +1, +0 ] [ +1, +3 ]
                        , bounded "trifecta" [ +1, +6 ] [ +1, +8 ]
                        ]
                    , hs-source-dirs =
                        [ "dhall-hash" ]
                    , main-is =
                        "Main.hs"
                    }
                )
            }
          ]
      , extra-source-files =
          [ "CHANGELOG.md"
          , "Prelude/Bool/and"
          , "Prelude/Bool/build"
          , "Prelude/Bool/even"
          , "Prelude/Bool/fold"
          , "Prelude/Bool/not"
          , "Prelude/Bool/odd"
          , "Prelude/Bool/or"
          , "Prelude/Bool/show"
          , "Prelude/Double/show"
          , "Prelude/Integer/show"
          , "Prelude/List/all"
          , "Prelude/List/any"
          , "Prelude/List/build"
          , "Prelude/List/concat"
          , "Prelude/List/concatMap"
          , "Prelude/List/filter"
          , "Prelude/List/fold"
          , "Prelude/List/generate"
          , "Prelude/List/head"
          , "Prelude/List/indexed"
          , "Prelude/List/iterate"
          , "Prelude/List/last"
          , "Prelude/List/length"
          , "Prelude/List/map"
          , "Prelude/List/null"
          , "Prelude/List/replicate"
          , "Prelude/List/reverse"
          , "Prelude/List/shifted"
          , "Prelude/List/unzip"
          , "Prelude/Monoid"
          , "Prelude/Natural/build"
          , "Prelude/Natural/enumerate"
          , "Prelude/Natural/even"
          , "Prelude/Natural/fold"
          , "Prelude/Natural/isZero"
          , "Prelude/Natural/odd"
          , "Prelude/Natural/product"
          , "Prelude/Natural/show"
          , "Prelude/Natural/sum"
          , "Prelude/Natural/toInteger"
          , "Prelude/Optional/all"
          , "Prelude/Optional/any"
          , "Prelude/Optional/build"
          , "Prelude/Optional/concat"
          , "Prelude/Optional/filter"
          , "Prelude/Optional/fold"
          , "Prelude/Optional/head"
          , "Prelude/Optional/last"
          , "Prelude/Optional/length"
          , "Prelude/Optional/map"
          , "Prelude/Optional/null"
          , "Prelude/Optional/toList"
          , "Prelude/Optional/unzip"
          , "Prelude/Text/concat"
          , "Prelude/Text/concatMap"
          , "Prelude/Text/concatMapSep"
          , "Prelude/Text/concatSep"
          , "tests/parser/*.dhall"
          , "tests/regression/*.dhall"
          ]
      , library =
          [ unguarded
            ./dhall/types/Library.dhall 
            (   ./dhall/defaults/Library.dhall 
              ⫽ { build-depends =
                    [ bounded "base" [ +4, +9, +0, +0 ] [ +5 ]
                    , before "ansi-wl-pprint" [ +0, +7 ]
                    , before "base16-bytestring" [ +0, +2 ]
                    , before "bytestring" [ +0, +11 ]
                    , before "case-insensitive" [ +1, +3 ]
                    , before "charset" [ +0, +4 ]
                    , bounded "containers" [ +0, +5, +0, +0 ] [ +0, +6 ]
                    , before "contravariant" [ +1, +5 ]
                    , before "cryptohash" [ +0, +12 ]
                    , bounded "exceptions" [ +0, +8, +3 ] [ +0, +9 ]
                    , bounded "http-client" [ +0, +4, +30 ] [ +0, +6 ]
                    , bounded "http-client-tls" [ +0, +2, +0 ] [ +0, +4 ]
                    , bounded "lens" [ +2, +4 ] [ +4, +16 ]
                    , bounded "parsers" [ +0, +12, +4 ] [ +0, +13 ]
                    , bounded "prettyprinter" [ +1, +1, +1 ] [ +1, +2 ]
                    , bounded "system-filepath" [ +0, +3, +1 ] [ +0, +5 ]
                    , bounded "system-fileio" [ +0, +2, +1 ] [ +0, +4 ]
                    , bounded "text" [ +0, +11, +1, +0 ] [ +1, +3 ]
                    , before "text-format" [ +0, +4 ]
                    , bounded "transformers" [ +0, +2, +0, +0 ] [ +0, +6 ]
                    , bounded "trifecta" [ +1, +6 ] [ +1, +8 ]
                    , bounded
                      "unordered-containers"
                      [ +0, +1, +3, +0 ]
                      [ +0, +3 ]
                    , bounded "vector" [ +0, +11, +0, +0 ] [ +0, +13 ]
                    ]
                , compiler-options =
                      ./dhall/defaults/CompilerOptions.dhall 
                    ⫽ { GHC = [ "-Wall" ] }
                , exposed-modules =
                    [ "Dhall"
                    , "Dhall.Context"
                    , "Dhall.Core"
                    , "Dhall.Import"
                    , "Dhall.Parser"
                    , "Dhall.Tutorial"
                    , "Dhall.TypeCheck"
                    ]
                , hs-source-dirs =
                    [ "src" ]
                }
            )
          ] : Optional
              (./dhall/types/Guarded.dhall  ./dhall/types/Library.dhall )
      , license =
          licenses.BSD3 {=}
      , license-files =
          [ "LICENSE" ]
      , maintainer =
          "Gabriel439@gmail.com"
      , name =
          "dhall"
      , version =
          [ +1, +8, +2 ]
      , source-repos =
          [   ./dhall/defaults/SourceRepo.dhall 
            ⫽ { location =
                  [ "https://github.com/dhall-lang/dhall-haskell" ] : Optional
                                                                      Text
              , type =
                  [ (constructors ./dhall/types/RepoType.dhall ).Git {=}
                  ] : Optional ./dhall/types/RepoType.dhall 
              }
          ]
      , synopsis =
          "A configuration language guaranteed to terminate"
      , tested-with =
          [ { compiler =
                compilers.GHC {=}
            , version =
                thisVersion [ +8, +0, +1 ]
            }
          ]
      , test-suites =
          [ { test-suite =
                unguarded
                ./dhall/types/TestSuite.dhall 
                (   ./dhall/defaults/TestSuite.dhall 
                  ⫽ { build-depends =
                        [ { bounds = within [ +4 ] [ +5 ], package = "base" }
                        , { bounds =
                              within [ +0, +5, +0, +0 ] [ +0, +6 ]
                          , package =
                              "containers"
                          }
                        , { bounds = anyVersion, package = "dhall" }
                        , { bounds =
                              within [ +0, +11, +2 ] [ +0, +13 ]
                          , package =
                              "tasty"
                          }
                        , { bounds =
                              within [ +0, +9, +2 ] [ +0, +11 ]
                          , package =
                              "tasty-hunit"
                          }
                        , { bounds =
                              within [ +0, +11, +1, +0 ] [ +1, +3 ]
                          , package =
                              "text"
                          }
                        , { bounds =
                              within [ +0, +11, +0, +0 ] [ +0, +13 ]
                          , package =
                              "vector"
                          }
                        ]
                    , compiler-options =
                          ./dhall/defaults/CompilerOptions.dhall 
                        ⫽ { GHC = [ "-Wall" ] }
                    , hs-source-dirs =
                        [ "tests" ]
                    , type =
                        TestTypes.exitcode-stdio { main-is = "Tests.hs" }
                    , other-modules =
                        [ "Examples"
                        , "Normalization"
                        , "Parser"
                        , "Regression"
                        , "Tutorial"
                        , "Util"
                        ]
                    }
                )
            , name =
                "test"
            }
          ]
      }
