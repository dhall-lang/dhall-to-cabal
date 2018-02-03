{ os :
    ./OS.dhall  → Bool
, arch :
    ./Arch.dhall  → Bool
, impl :
    ./Compiler.dhall  → VersionRange → Bool
, flag :
    Text → Bool
}
