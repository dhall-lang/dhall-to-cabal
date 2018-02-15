  λ(VersionRange : Type)
→ { autogen-modules :
      List Text
  , build-depends :
      List (./Dependency.dhall  VersionRange)
  , build-tool-depends :
      List { component : Text, package : Text, version : VersionRange }
  , build-tools :
      List { exe : Text, version : VersionRange }
  , buildable :
      Bool
  , c-sources :
      List Text
  , cc-options :
      List Text
  , compiler-options :
      ./CompilerOptions.dhall 
  , cpp-options :
      List Text
  , default-extensions :
      List ./Extension.dhall 
  , default-language :
      Optional ./Language.dhall 
  , exposed-modules :
      List Text
  , extra-framework-dirs :
      List Text
  , extra-ghci-libraries :
      List Text
  , extra-lib-dirs :
      List Text
  , extra-libraries :
      List Text
  , frameworks :
      List Text
  , hs-source-dirs :
      List Text
  , includes :
      List Text
  , include-dirs :
      List Text
  , install-includes :
      List Text
  , js-sources :
      List Text
  , ld-options :
      List Text
  , other-extensions :
      List ./Extension.dhall 
  , other-languages :
      List ./Language.dhall 
  , other-modules :
      List Text
  , pkgconfig-depends :
      List { name : Text, version : VersionRange }
  , profiling-options :
      ./CompilerOptions.dhall 
  , reexported-modules :
      List { name : Text, original : { name : Text, package : Optional Text } }
  , shared-options :
      ./CompilerOptions.dhall 
  , mixins :
      List ./Mixin.dhall 
  , signatures :
      List Text
  }
