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
                  [ prelude.types.RepoType.Git {=} ] : Optional types.RepoType
              , location =
                  [ "example.com" ] : Optional Text
              }
          ,   prelude.defaults.SourceRepo
            ⫽ { type =
                  [ prelude.types.RepoType.Darcs {=} ] : Optional types.RepoType
              , location =
                  [ "example.org" ] : Optional Text
              , kind =
                  prelude.types.RepoKind.RepoThis {=}
              }
          ]
      }