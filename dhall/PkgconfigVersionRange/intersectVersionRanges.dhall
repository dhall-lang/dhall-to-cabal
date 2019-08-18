  λ(a : ../types/PkgconfigVersionRange.dhall)
→ λ(b : ../types/PkgconfigVersionRange.dhall)
→ λ(PkgconfigVersionRange : Type)
→ λ(anyVersion : PkgconfigVersionRange)
→ λ(thisVersion : Text → PkgconfigVersionRange)
→ λ(laterVersion : Text → PkgconfigVersionRange)
→ λ(earlierVersion : Text → PkgconfigVersionRange)
→ λ(orLaterVersion : Text → PkgconfigVersionRange)
→ λ(orEarlierVersion : Text → PkgconfigVersionRange)
→ λ ( unionVersionRanges
    : PkgconfigVersionRange → PkgconfigVersionRange → PkgconfigVersionRange
    )
→ λ ( intersectVersionRanges
    : PkgconfigVersionRange → PkgconfigVersionRange → PkgconfigVersionRange
    )
→ intersectVersionRanges
  ( a
    PkgconfigVersionRange
    anyVersion
    thisVersion
    laterVersion
    earlierVersion
    orLaterVersion
    orEarlierVersion
    unionVersionRanges
    intersectVersionRanges
  )
  ( b
    PkgconfigVersionRange
    anyVersion
    thisVersion
    laterVersion
    earlierVersion
    orLaterVersion
    orEarlierVersion
    unionVersionRanges
    intersectVersionRanges
  )
