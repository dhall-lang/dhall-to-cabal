let SourceRepo = ../types/SourceRepo.dhall

let Package = ../types/Package.dhall

let mapSourceRepos
    : (SourceRepo → SourceRepo) → Package → Package
    =   λ(f : SourceRepo → SourceRepo)
      → λ(pkg : Package)
      →   pkg
        ⫽ { source-repos =
              List/build
              SourceRepo
              (   λ(list : Type)
                → λ(cons : SourceRepo → list → list)
                → List/fold
                  SourceRepo
                  pkg.source-repos
                  list
                  (λ(x : SourceRepo) → cons (f x))
              )
          }

in  mapSourceRepos
