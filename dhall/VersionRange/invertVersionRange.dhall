  λ(a : ../types/VersionRange.dhall )
→ λ(VersionRange : Type)
→ λ(anyVersion : VersionRange)
→ λ(noVersion : VersionRange)
→ λ(thisVersion : ../types/Version.dhall  → VersionRange)
→ λ(notThisVersion : ../types/Version.dhall  → VersionRange)
→ λ(laterVersion : ../types/Version.dhall  → VersionRange)
→ λ(earlierVersion : ../types/Version.dhall  → VersionRange)
→ λ(orLaterVersion : ../types/Version.dhall  → VersionRange)
→ λ(orEarlierVersion : ../types/Version.dhall  → VersionRange)
→ λ(withinVersion : ../types/Version.dhall  → VersionRange)
→ λ(majorBoundVersion : ../types/Version.dhall  → VersionRange)
→ λ(unionVersionRanges : VersionRange → VersionRange → VersionRange)
→ λ(intersectVersionRanges : VersionRange → VersionRange → VersionRange)
→ λ(differenceVersionRanges : VersionRange → VersionRange → VersionRange)
→ λ(invertVersionRange : VersionRange → VersionRange)
→ invertVersionRange
  ( a
    VersionRange
    anyVersion
    noVersion
    thisVersion
    notThisVersion
    laterVersion
    earlierVersion
    orLaterVersion
    orEarlierVersion
    withinVersion
    majorBoundVersion
    unionVersionRanges
    intersectVersionRanges
    differenceVersionRanges
    invertVersionRange
  )
