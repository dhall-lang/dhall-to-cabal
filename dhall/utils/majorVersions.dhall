   let Version =
        ../types/Version.dhall
in let VersionRange =
        ../types/VersionRange.dhall
in let Versions =  
        { unionVersionRanges =
            ../VersionRange/unionVersionRanges.dhall
        , majorBoundVersion =
            ../VersionRange/majorBoundVersion.dhall
        , noVersion =
            ../VersionRange/noVersion.dhall
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
                        → [ Versions.unionVersionRanges 
                            ( Versions.majorBoundVersion  v )
                            r
                          ] : Optional VersionRange 
                      )
                      ( [ Versions.majorBoundVersion  v
                        ] : Optional VersionRange 
                      )
                  )
                  ([] : Optional VersionRange )
                )
                VersionRange 
                (λ(a : VersionRange ) → a)
                Versions.noVersion 
            }

in  majorVersions
