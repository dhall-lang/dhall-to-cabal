    let empty-package = ./dhall/empty-package.dhall 

in  let licenses = constructors ./dhall/types/License 

in  let unguarded =
            λ(A : Type)
          → λ(a : A)
          → { guard = λ(config : ./dhall/types/Config ) → True, body = a }

in  let restrict-by-compiler =
            λ(A : Type)
          → λ(compiler : ./dhall/types/Compiler )
          → λ(bounds : VersionRange)
          → λ(a : A)
          → { guard =
                λ(config : ./dhall/types/Config ) → config.impl compiler bounds
            , body =
                a
            }

in  let restrict-by-flag =
            λ(A : Type)
          → λ(flag : Text)
          → λ(a : A)
          → { guard =
                λ(config : ./dhall/types/Config ) → config.flag flag
            , body =
                a
            }

in    empty-package
    ⫽ { package =
          { name = "lens", version = [ +4, +16 ] }
      , category =
          "Data, Lenses, Generics"
      , license =
          licenses.BSD2 {=}
      , author =
          "Edward A. Kmett"
      , maintainer =
          "Edward A. Kmett <ekmett@gmail.com>"
      , stability =
          "provisional"
      , license-files =
          [ "LICENSE" ]
      , homepage =
          "http://github.com/ekmett/lens/"
      , bug-reports =
          "http://github.com/ekmett/lens/issues"
      , copyright =
          "Copyright C 2012 -2016 Edward A.Kmett"
      , build-type =
          [     let BuildTypes = constructors ./dhall/types/BuildType 
            
            in  BuildTypes.Custom {=}
          ] : Optional ./dhall/types/BuildType 
      , tested-with =
              let GHC =
                      λ(version : List Natural)
                    → { compiler =
                          (constructors ./dhall/types/Compiler ).GHC {=}
                      , version =
                          thisVersion version
                      }
          
          in  [ GHC [ +7, +4, +2 ]
              , GHC [ +7, +6, +3 ]
              , GHC [ +7, +8, +4 ]
              , GHC [ +7, +10, +3 ]
              , GHC [ +8, +0, +2 ]
              , GHC [ +8, +2, +2 ]
              , GHC [ +8, +4, +1 ]
              ]
      , synopsis =
          "Lenses, Folds and Traversals"
      , description =
          ''
          This package comes \"Batteries Included\" with many useful lenses for the types
          commonly used from the Haskell Platform, and with tools for automatically
          generating lenses and isomorphisms for user-supplied data types.
          
          The combinators in @Control.Lens@ provide a highly generic toolbox for composing
          families of getters, folds, isomorphisms, traversals, setters and lenses and their
          indexed variants.
          
          An overview, with a large number of examples can be found in the <https://github.com/ekmett/lens#lens-lenses-folds-and-traversals README>.
          
          An introductory video on the style of code used in this library by Simon Peyton Jones is available from <http://skillsmatter.com/podcast/scala/lenses-compositional-data-access-and-manipulation Skills Matter>.
          
          A video on how to use lenses and how they are constructed is available on <http://youtu.be/cefnmjtAolY?hd=1 youtube>.
          
          Slides for that second talk can be obtained from <http://comonad.com/haskell/Lenses-Folds-and-Traversals-NYC.pdf comonad.com>.
          
          More information on the care and feeding of lenses, including a brief tutorial and motivation
          for their types can be found on the <https://github.com/ekmett/lens/wiki lens wiki>.
          
          A small game of @pong@ and other more complex examples that manage their state using lenses can be found in the <https://github.com/ekmett/lens/blob/master/examples/ example folder>.
          
          /Lenses, Folds and Traversals/
          
          With some signatures simplified, the core of the hierarchy of lens-like constructions looks like:
          
          
          <<http://i.imgur.com/ALlbPRa.png>>
          
          <Hierarchy.png (Local Copy)>
          
          You can compose any two elements of the hierarchy above using @(.)@ from the @Prelude@, and you can
          use any element of the hierarchy as any type it linked to above it.
          
          The result is their lowest upper bound in the hierarchy (or an error if that bound doesn't exist).
          
          For instance:
          
          * You can use any 'Traversal' as a 'Fold' or as a 'Setter'.
          
          * The composition of a 'Traversal' and a 'Getter' yields a 'Fold'.
          
          /Minimizing Dependencies/
          
          If you want to provide lenses and traversals for your own types in your own libraries, then you
          can do so without incurring a dependency on this (or any other) lens package at all.
          
          /e.g./ for a data type:
          
          > data Foo a = Foo Int Int a
          
          You can define lenses such as
          
          > -- bar :: Lens' (Foo a) Int
          > bar :: Functor f => (Int -> f Int) -> Foo a -> f (Foo a)
          > bar f (Foo a b c) = fmap (\a' -> Foo a' b c) (f a)
          
          > -- quux :: Lens (Foo a) (Foo b) a b
          > quux :: Functor f => (a -> f b) -> Foo a -> f (Foo b)
          > quux f (Foo a b c) = fmap (Foo a b) (f c)
          
          without the need to use any type that isn't already defined in the @Prelude@.
          
          And you can define a traversal of multiple fields with 'Control.Applicative.Applicative':
          
          > -- traverseBarAndBaz :: Traversal' (Foo a) Int
          > traverseBarAndBaz :: Applicative f => (Int -> f Int) -> Foo a -> f (Foo a)
          > traverseBarAndBaz f (Foo a b c) = Foo <$> f a <*> f b <*> pure c
          
          What is provided in this library is a number of stock lenses and traversals for
          common haskell types, a wide array of combinators for working them, and more
          exotic functionality, (/e.g./ getters, setters, indexed folds, isomorphisms).
          ''
      , extra-source-files =
          [ ".travis.yml"
          , ".gitignore"
          , ".vim.custom"
          , "cabal.project"
          , "examples/LICENSE"
          , "examples/lens-examples.cabal"
          , "examples/*.hs"
          , "examples/*.lhs"
          , "images/*.png"
          , "lens-properties/CHANGELOG.markdown"
          , "lens-properties/LICENSE"
          , "lens-properties/Setup.hs"
          , "travis/cabal-apt-install"
          , "travis/config"
          , "HLint.hs"
          , "Warning.hs"
          , "AUTHORS.markdown"
          , "CHANGELOG.markdown"
          , "README.markdown"
          , "SUPPORT.markdown"
          ]
      , source-repos =
          [ { type =
                [ (constructors ./dhall/types/RepoType ).Git {=}
                ] : Optional ./dhall/types/RepoType 
            , location =
                [ "https://github.com/ekmett/lens.git" ] : Optional Text
            }
          ]
      , flags =
          [ { name =
                "benchmark-uniplate"
            , default =
                False
            , manual =
                True
            , description =
                ''
                Enable benchmarking against Neil Mitchell's uniplate library for comparative performance analysis. Defaults to being turned off to avoid
                the extra dependency.
                
                > cabal configure --enable-benchmarks -fbenchmark-uniplate && cabal build && cabal bench''
            }
          , { name =
                "safe"
            , manual =
                True
            , default =
                False
            , description =
                "Disallow unsafeCoerce"
            }
          ]
      , library =
          [ [ unguarded
              ./dhall/types/Library 
              (   ./dhall/defaults/Library.dhall 
                ⫽ { build-dependencies =
                      [ { package = "ghc-prim", bounds = anyVersion } ]
                  , exposed-modules =
                      [ "Control.Exception.Lens"
                      , "Control.Lens"
                      , "Control.Lens.At"
                      , "Control.Lens.Combinators"
                      , "Control.Lens.Cons"
                      , "Control.Lens.Each"
                      , "Control.Lens.Empty"
                      , "Control.Lens.Equality"
                      , "Control.Lens.Extras"
                      , "Control.Lens.Fold"
                      , "Control.Lens.Getter"
                      , "Control.Lens.Indexed"
                      , "Control.Lens.Internal"
                      , "Control.Lens.Internal.Bazaar"
                      , "Control.Lens.Internal.ByteString"
                      , "Control.Lens.Internal.Coerce"
                      , "Control.Lens.Internal.Context"
                      , "Control.Lens.Internal.CTypes"
                      , "Control.Lens.Internal.Deque"
                      , "Control.Lens.Internal.Exception"
                      , "Control.Lens.Internal.FieldTH"
                      , "Control.Lens.Internal.PrismTH"
                      , "Control.Lens.Internal.Fold"
                      , "Control.Lens.Internal.Getter"
                      , "Control.Lens.Internal.Indexed"
                      , "Control.Lens.Internal.Instances"
                      , "Control.Lens.Internal.Iso"
                      , "Control.Lens.Internal.Level"
                      , "Control.Lens.Internal.List"
                      , "Control.Lens.Internal.Magma"
                      , "Control.Lens.Internal.Prism"
                      , "Control.Lens.Internal.Review"
                      , "Control.Lens.Internal.Setter"
                      , "Control.Lens.Internal.TH"
                      , "Control.Lens.Internal.Zoom"
                      , "Control.Lens.Iso"
                      , "Control.Lens.Lens"
                      , "Control.Lens.Level"
                      , "Control.Lens.Operators"
                      , "Control.Lens.Plated"
                      , "Control.Lens.Prism"
                      , "Control.Lens.Reified"
                      , "Control.Lens.Review"
                      , "Control.Lens.Setter"
                      , "Control.Lens.TH"
                      , "Control.Lens.Traversal"
                      , "Control.Lens.Tuple"
                      , "Control.Lens.Type"
                      , "Control.Lens.Unsound"
                      , "Control.Lens.Wrapped"
                      , "Control.Lens.Zoom"
                      , "Control.Monad.Error.Lens"
                      , "Control.Parallel.Strategies.Lens"
                      , "Control.Seq.Lens"
                      , "Data.Array.Lens"
                      , "Data.Bits.Lens"
                      , "Data.ByteString.Lens"
                      , "Data.ByteString.Strict.Lens"
                      , "Data.ByteString.Lazy.Lens"
                      , "Data.Complex.Lens"
                      , "Data.Data.Lens"
                      , "Data.Dynamic.Lens"
                      , "Data.HashSet.Lens"
                      , "Data.IntSet.Lens"
                      , "Data.List.Lens"
                      , "Data.Map.Lens"
                      , "Data.Sequence.Lens"
                      , "Data.Set.Lens"
                      , "Data.Text.Lens"
                      , "Data.Text.Strict.Lens"
                      , "Data.Text.Lazy.Lens"
                      , "Data.Tree.Lens"
                      , "Data.Typeable.Lens"
                      , "Data.Vector.Lens"
                      , "Data.Vector.Generic.Lens"
                      , "GHC.Generics.Lens"
                      , "System.Exit.Lens"
                      , "System.FilePath.Lens"
                      , "System.IO.Error.Lens"
                      , "Language.Haskell.TH.Lens"
                      , "Numeric.Lens"
                      , "Numeric.Natural.Lens"
                      ]
                  , other-modules =
                      [ "Paths_lens" ]
                  , cpp-options =
                      [ "-traditional" ]
                  , compiler-options =
                      ./dhall/defaults/CompilerOptions  ⫽ { GHC = [ "-Wall" ] }
                  , hs-source-dirs =
                      [ "src" ]
                  }
              )
            , restrict-by-compiler
              ./dhall/types/Library 
              ((constructors ./dhall/types/Compiler ).GHC {=})
              (earlierVersion [ +8 ])
              (   ./dhall/defaults/Library.dhall 
                ⫽ { build-dependencies =
                      [ { package =
                            "generic-deriving"
                        , bounds =
                            orLaterVersion [ +1, +10 ]
                        }
                      ]
                  }
              )
            , restrict-by-compiler
              ./dhall/types/Library 
              ((constructors ./dhall/types/Compiler ).GHC {=})
              (earlierVersion [ +7, +9 ])
              (   ./dhall/defaults/Library.dhall 
                ⫽ { build-dependencies =
                      [ { package = "nats", bounds = orLaterVersion [ +0, +1 ] }
                      ]
                  }
              )
            , restrict-by-flag
              ./dhall/types/Library 
              "safe"
              (   ./dhall/defaults/Library.dhall 
                ⫽ { cpp-options = [ "-DSAFE=1" ] }
              )
            ]
          ] : Optional (./dhall/types/Guarded  ./dhall/types/Library )
      }
