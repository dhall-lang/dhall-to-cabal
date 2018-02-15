  λ(VersionRange : Type)
→ λ(A : Type)
→ List { body : A, guard : ./Config.dhall  VersionRange → Bool }
