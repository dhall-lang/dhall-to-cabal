    let GitHubProject : Type = { owner : Text, repo : Text }
in  let gitHubWithSourceRepoProject =
            λ(repoData : ./types/SourceRepo.dhall )
          → λ(github : GitHubProject)
          →     let gitHubRoot =
                      "https://github.com/${github.owner}/${github.repo}"
            
            in    ./defaults/Package.dhall 
                ⫽ { name =
                      github.repo
                  , bug-reports =
                      "${gitHubRoot}/issues"
                  , homepage =
                      gitHubRoot
                  , source-repos =
                      [   repoData
                        ⫽ { location =
                              [ gitHubRoot ] : Optional Text
                          , type =
                              [ (constructors ./types/RepoType.dhall ).Git {=}
                              ] : Optional ./types/RepoType.dhall 
                          }
                      ]
                  }

in  gitHubWithSourceRepoProject
