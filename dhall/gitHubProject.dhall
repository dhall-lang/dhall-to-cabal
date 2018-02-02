    let GitHubProject : Type = { owner : Text, repo : Text }

in  let gitHubProject =
            λ(github : GitHubProject)
          →     let gitHubRoot =
                      "https://github.com/${github.owner}/${github.repo}"
            
            in    ./empty-package.dhall 
                ⫽ { bug-reports =
                      "${gitHubRoot}/issues"
                  , homepage =
                      gitHubRoot
                  , source-repos =
                      [ { location =
                            [ gitHubRoot ] : Optional Text
                        , type =
                            [ (constructors ./types/RepoType ).Git {=}
                            ] : Optional ./types/RepoType 
                        }
                      ]
                  }

in  gitHubProject
