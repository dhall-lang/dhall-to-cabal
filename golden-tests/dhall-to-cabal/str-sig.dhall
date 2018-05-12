  ./dhall/defaults/Package.dhall 
⫽ { name =
      "str-sig"
  , version =
      [ +0, +1, +0, +2 ]
  , synopsis =
      "Signature package for String-like types."
  , description =
      ''
      This package provides a Str signature which defines
      common namespace of functionality provided by
      string packages.  Implementations of subsets of
      this signature can be found in str-string,
      str-bytestring, str-text and str-foundation.
      
      Once this signature is stabilized, this package
      will never introduce a backwards incompatible
      change in an update; however, we may release new
      versions of this signature which add methods. To
      ensure that your code continues working, please
      locally declare which methods from this signature
      you are using; you can find instructions for how to
      do this in <https://wiki.haskell.org/Module_signature>
      ''
  , license =
      (constructors ./dhall/types/License.dhall ).BSD3 {=}
  , license-files =
      [ "LICENSE" ]
  , author =
      "Edward Z. Yang"
  , maintainer =
      "ezyang@cs.stanford.edu"
  , category =
      "Signature"
  , build-type =
      [ (constructors ./dhall/types/BuildType.dhall ).Simple {=}
      ] : Optional ./dhall/types/BuildType.dhall 
  , extra-source-files =
      [ "ChangeLog.md", "README.md" ]
  , cabal-version =
      [ +1, +25 ]
  , library =
      [ [ { guard =
              λ(_ : ./dhall/types/Config.dhall ) → True
          , body =
                ./dhall/defaults/Library.dhall 
              ⫽ { signatures =
                    [ "Str" ]
                , default-language =
                    [ (constructors ./dhall/types/Language.dhall ).Haskell2010
                      {=}
                    ] : Optional ./dhall/types/Language.dhall 
                , build-depends =
                    [ { package =
                          "base"
                      , bounds =
                          intersectVersionRanges
                          (orLaterVersion [ +4, +10 ])
                          (earlierVersion [ +4, +11 ])
                      }
                    ]
                }
          }
        ]
      ] : Optional (./dhall/types/Guarded.dhall  ./dhall/types/Library.dhall )
  }
