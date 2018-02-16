  λ(a : ../VersionRange.dhall )
→ λ(b : ../VersionRange.dhall )
→ λ(VersionRange : Type)
→ λ(anyVersion : VersionRange)
→ λ(noVersion : VersionRange)
→ λ(thisVersion : List Natural → VersionRange)
→ λ(notThisVersion : List Natural → VersionRange)
→ λ(laterVersion : List Natural → VersionRange)
→ λ(earlierVersion : List Natural → VersionRange)
→ λ(orLaterVersion : List Natural → VersionRange)
→ λ(orEarlierVersion : List Natural → VersionRange)
→ λ(withinVersion : List Natural → VersionRange)
→ λ(majorBoundVersion : List Natural → VersionRange)
→ λ(unionVersionRanges : VersionRange → VersionRange → VersionRange)
→ λ(intersectVersionRanges : VersionRange → VersionRange → VersionRange)
→ λ(differenceVersionRanges : VersionRange → VersionRange → VersionRange)
→ λ(invertVersionRange : VersionRange → VersionRange)
→ intersectVersionRanges
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
  ( b
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
