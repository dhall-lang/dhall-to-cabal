{ autogen-modules :
    List Text
, build-depends :
    List ./Dependency.dhall 
, build-tool-depends :
    List { component : Text, package : Text, version : ./VersionRange.dhall  }
, build-tools :
    List { exe : Text, version : ./VersionRange.dhall  }
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
    List { name : Text, version : ./VersionRange.dhall  }
, profiling-options :
    ./CompilerOptions.dhall 
, shared-options :
    ./CompilerOptions.dhall 
, static-options :
    ./CompilerOptions.dhall 
, mixins :
    List ./Mixin.dhall 
, asm-options :
    List Text
, asm-sources :
    List Text
, cmm-options :
    List Text
, cmm-sources :
    List Text
, cxx-options :
    List Text
, cxx-sources :
    List Text
, virtual-modules :
    List Text
, extra-lib-flavours :
    List Text
, extra-bundled-libs :
    List Text
}
