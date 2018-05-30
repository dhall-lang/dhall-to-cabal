    let gitHubWithSourceRepo =
            ./GitHubWithSourceRepo-project.dhall
in  let gitHubProject = 
            gitHubWithSourceRepo ./defaults/SourceRepo.dhall
in  gitHubProject