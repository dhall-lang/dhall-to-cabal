let prelude = ./../../dhall/prelude.dhall

let types = ./../../dhall/types.dhall

in    prelude.defaults.Package
    ⫽ { name =
          "foo"
      , version =
          prelude.v "2"
      , cabal-version =
          prelude.v "2.4"
      , source-repos =
          [   prelude.defaults.SourceRepo
            ⫽ { type =
                  Some types.RepoType.Git
              , location =
                  Some "https://example.com"
              , kind =
                  types.RepoKind.RepoKindUnknown { _1 = "blargh" }
              }
          ]
      }