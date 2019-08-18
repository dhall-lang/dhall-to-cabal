let Dependency = ../types/Dependency.dhall

let LibraryName = ../types/LibraryName.dhall

let Version = ../types/Version.dhall

let VersionRange = ../types/VersionRange.dhall

let Versions =
      { unionVersionRanges =
          ../VersionRange/unionVersionRanges.dhall
      , majorBoundVersion =
          ../VersionRange/majorBoundVersion.dhall
      , noVersion =
          ../VersionRange/noVersion.dhall
      }

let majorVersions
    : Text → List Version → List LibraryName → Dependency
    =   λ(package : Text)
      → λ(versions : List Version)
      → λ(library-names : List LibraryName)
      → { package =
            package
        , bounds =
            Optional/fold
            VersionRange
            ( List/fold
              Version
              versions
              (Optional VersionRange)
              (   λ(v : Version)
                → λ(r : Optional VersionRange)
                → Optional/fold
                  VersionRange
                  r
                  (Optional VersionRange)
                  (   λ(r : VersionRange)
                    → Some
                      ( Versions.unionVersionRanges
                        (Versions.majorBoundVersion v)
                        r
                      )
                  )
                  (Some (Versions.majorBoundVersion v))
              )
              (None VersionRange)
            )
            VersionRange
            (λ(a : VersionRange) → a)
            Versions.noVersion
        , library-names =
            library-names
        }

in  majorVersions
