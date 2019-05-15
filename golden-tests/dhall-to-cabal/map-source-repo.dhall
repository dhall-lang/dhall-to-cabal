let prelude = ../../dhall/prelude.dhall

let types = ../../dhall/types.dhall

let updateRepo =
      prelude.utils.mapSourceRepos
      (   λ(srcRepo : types.SourceRepo)
        → srcRepo ⫽ { tag = Some "1.0.0", kind = types.RepoKind.RepoThis }
      )

let project = prelude.utils.GitHub-project { owner = "owner", repo = "repo" }

in  updateRepo
    (   project
      ⫽ { version =
            prelude.v "1.0.0"
        , executables =
            [ { name =
                  "foo"
              , executable =
                    λ(config : ../../dhall/types/Config.dhall)
                  →   ../../dhall/defaults/Executable.dhall
                    ⫽ { main-is = "Main.hs" }
              }
            ]
        }
    )
