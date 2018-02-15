  λ(VersionRange : Type)
→   ./BuildInfo.dhall  VersionRange
  ⫽ { exposed-modules =
        [] : List Text
    , other-modules =
        [] : List Text
    , reexported-modules =
        [] : List
             { name :
                 Text
             , original :
                 { name : Text, package : Optional Text }
             }
    , signatures =
        [] : List Text
    }
