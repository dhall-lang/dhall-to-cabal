    let v
        : ∀(str : Text) → ../types/Version.dhall 
        = λ(str : Text) → λ(Version : Type) → λ(v : Text → Version) → v str

in  v
