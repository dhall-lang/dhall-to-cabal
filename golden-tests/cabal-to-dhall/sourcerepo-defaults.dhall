    let prelude = ./../../dhall/prelude.dhall

in  let types = ./../../dhall/types.dhall

in    prelude.defaults.Package
    ⫽ { name =
          "test"
      , version =
          prelude.v "0"
      , source-repos =
          [   prelude.defaults.SourceRepo
            ⫽ { type =
                  Some (prelude.types.RepoType.Git {=})
              , location =
                  Some "example.com"
              }
          ,   prelude.defaults.SourceRepo
            ⫽ { type =
                  Some (prelude.types.RepoType.Darcs {=})
              , location =
                  Some "example.org"
              , kind =
                  prelude.types.RepoKind.RepoThis {=}
              }
          ]
      }