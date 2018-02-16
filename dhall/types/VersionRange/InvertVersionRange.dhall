  λ(a : ../VersionRange.dhall )
→ λ(VersionRange : Type)
→ λ(anyVersion : VersionRange)
→ λ(noVersion : VersionRange)
→ λ(thisVersion : ../Version.dhall  → VersionRange)
→ λ(notThisVersion : ../Version.dhall  → VersionRange)
→ λ(laterVersion : ../Version.dhall  → VersionRange)
→ λ(earlierVersion : ../Version.dhall  → VersionRange)
→ λ(orLaterVersion : ../Version.dhall  → VersionRange)
→ λ(orEarlierVersion : ../Version.dhall  → VersionRange)
→ λ(withinVersion : ../Version.dhall  → VersionRange)
→ λ(majorBoundVersion : ../Version.dhall  → VersionRange)
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
