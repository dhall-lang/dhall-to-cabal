let GitHubProject : Type = { owner : Text, repo : Text }

let gitHubProject =
        λ(github : GitHubProject)
      → let gitHubRoot = "https://github.com/${github.owner}/${github.repo}"
        
        in    ./../defaults/Package.dhall
            ⫽ { name =
                  github.repo
              , bug-reports =
                  "${gitHubRoot}/issues"
              , homepage =
                  gitHubRoot
              , source-repos =
                  [   ./../defaults/SourceRepo.dhall
                    ⫽ { location =
                          [ gitHubRoot ] : Optional Text
                      , type =
                          [ (./../types/RepoType.dhall).Git {=}
                          ] : Optional ./../types/RepoType.dhall
                      }
                  ]
              }

in  gitHubProject
