let prelude = ./../../dhall/prelude.dhall

let types = ./../../dhall/types.dhall

in    prelude.defaults.Package
    ⫽ { name =
          "test"
      , source-repos =
          [   prelude.defaults.SourceRepo
            ⫽ { location = Some "example.com", type = Some types.RepoType.Git }
          ,   prelude.defaults.SourceRepo
            ⫽ { kind =
                  types.RepoKind.RepoThis
              , location =
                  Some "example.org"
              , type =
                  Some types.RepoType.Darcs
              }
          ]
      , version =
          prelude.v "0"
      }