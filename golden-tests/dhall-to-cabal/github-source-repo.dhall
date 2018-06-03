   let prelude = ./dhall/prelude.dhall

in let types = ./dhall/types.dhall

in let RepoKind = constructors types.RepoKind

in let GitHub-project = prelude.utils.GitHubWithSourceRepo-project
                        ( prelude.defaults.SourceRepo
                       // { tag = ["1.0.0"] : Optional Text
                          , kind = RepoKind.RepoThis {=}
                          }
                         )
                         { owner = "owner" , repo = "repo" }
in  GitHub-project
 // { version =
        prelude.v "1.0.0"
    , executables =
      [ { name =
            "foo"
        , executable =
            λ(config : ./dhall/types/Config.dhall)
          → ./dhall/defaults/Executable.dhall
        }
      ]
    }