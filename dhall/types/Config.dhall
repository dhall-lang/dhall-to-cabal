{ os :
    ./OS.dhall  → Bool
, arch :
    ./Arch.dhall  → Bool
, impl :
    ./Compiler.dhall  → ./VersionRange.dhall  → Bool
, flag :
    Text → Bool
}
