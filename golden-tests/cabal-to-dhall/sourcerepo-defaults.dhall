let prelude = ./../../dhall/prelude.dhall

let types = ./../../dhall/types.dhall

in    prelude.defaults.Package
    ⫽ { name =
          "test"
      , version =
          prelude.v "0"
      , source-repos =
          [   prelude.defaults.SourceRepo
            ⫽ { type = Some types.RepoType.Git, location = Some "example.com" }
          ,   prelude.defaults.SourceRepo
            ⫽ { type =
                  Some types.RepoType.Darcs
              , location =
                  Some "example.org"
              , kind =
                  types.RepoKind.RepoThis
              }
          ]
      }
