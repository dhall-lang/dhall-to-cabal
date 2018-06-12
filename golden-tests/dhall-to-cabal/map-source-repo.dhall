    let prelude = ./dhall/prelude.dhall

in  let types = ./dhall/types.dhall

in  let updateRepo =
          prelude.utils.mapSourceRepos
          (   λ(srcRepo : types.SourceRepo)
            →   srcRepo
              ⫽ { tag =
                    [ "1.0.0" ] : Optional Text
                , kind =
                    prelude.types.RepoKind.RepoThis {=}
                }
          )

in  let project =
          prelude.utils.GitHub-project { owner = "owner", repo = "repo" }

in  updateRepo
    (   project
      ⫽ { version =
            prelude.v "1.0.0"
        , executables =
            [ { name =
                  "foo"
              , executable =
                    λ(config : ./dhall/types/Config.dhall)
                  → ./dhall/defaults/Executable.dhall
				  ⫽ { main-is = "Main.hs" }
              }
            ]
        }
    )
