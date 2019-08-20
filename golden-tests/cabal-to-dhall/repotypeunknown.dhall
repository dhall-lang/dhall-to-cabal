let prelude = ./../../dhall/prelude.dhall

let types = ./../../dhall/types.dhall

in    prelude.defaults.Package
    ⫽ { cabal-version =
          prelude.v "2.4"
      , name =
          "foo"
      , source-repos =
          [   prelude.defaults.SourceRepo
            ⫽ { kind =
                  types.RepoKind.RepoKindUnknown { _1 = "blargh" }
              , location =
                  Some "https://example.com"
              , type =
                  Some types.RepoType.Git
              }
          ]
      , version =
          prelude.v "2"
      }