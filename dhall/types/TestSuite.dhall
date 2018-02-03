{ autogen-modules :
    List Text
, build-depends :
    List { bounds : VersionRange, package : Text }
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
    ./CompilerOptions 
, cpp-options :
    List Text
, default-extensions :
    List ./Extension 
, default-language :
    Optional < Haskell2010 : {} | Haskell98 : {} >
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
    List ./Extension 
, other-languages :
    List < Haskell2010 : {} | Haskell98 : {} >
, other-modules :
    List Text
, pkgconfig-depends :
    List { name : Text, version : VersionRange }
, profiling-options :
    ./CompilerOptions 
, shared-options :
    ./CompilerOptions 
, main-is :
    Text
}
