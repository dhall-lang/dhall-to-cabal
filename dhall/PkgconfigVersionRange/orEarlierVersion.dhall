  λ(v : Text)
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
→ orEarlierVersion v
