   let Version =
        ../types/Version.dhall
in let VersionRange =
        ../types/VersionRange.dhall
in let Versions =  
        { UnionVersionRanges =
            ../types/VersionRange/UnionVersionRanges.dhall
        , MajorBoundVersion =
            ../types/VersionRange/MajorBoundVersion.dhall
        , NoVersion =
            ../types/VersionRange/NoVersion.dhall
        }

in let majorVersions
        :   Text
          → List Version 
          → { package : Text, bounds : VersionRange  }
        =   λ(package : Text)
          → λ(versions : List Version )
          → { package =
                package
            , bounds =
                Optional/fold
                VersionRange 
                ( List/fold
                  Version 
                  versions
                  (Optional VersionRange )
                  (   λ(v : Version )
                    → λ(r : Optional VersionRange )
                    → Optional/fold
                      VersionRange 
                      r
                      (Optional VersionRange )
                      (   λ(r : VersionRange )
                        → [ Versions.UnionVersionRanges 
                            ( Versions.MajorBoundVersion  v )
                            r
                          ] : Optional VersionRange 
                      )
                      ( [ Versions.MajorBoundVersion  v
                        ] : Optional VersionRange 
                      )
                  )
                  ([] : Optional VersionRange )
                )
                VersionRange 
                (λ(a : VersionRange ) → a)
                Versions.NoVersion 
            }

in  majorVersions
