# dhall-to-cabal change log

## 1.3.0.0 -- 2018-07-28

### Breaking API Changes

* `DhallToCabal.dhallToCabal` now takes an `InputSettings` from
  `dhall` as its first argument.

### Functional Changes

* `dhall-to-cabal` has a new `--print-default TYPE` flag.

* When reading from a file, `dhall-to-cabal` now interprets imports as
  being relative to that file, rather than the current working
  directory. (#114)


## 1.2.0.0 -- 2018-07-05

### Breaking API Changes

* Remove orphan `Dhall.Core.Inject` instances for `[Char]` and
  `CompilerFlavor`.

* `CabalToDhall.cabalToDhall` is now a pure function that accepts a
  `GenericPackageDescription`. A new convenience function has been
  added to `CabalToDhall`, `parseGenericPackageDescriptionThrows`.

### Functional Changes

* `os` conditions where the operating system's name was not recognised
  (e.g., `os(multics)`) were crashing cabal-to-dhall. They now work as
  expected.

* `dhall-to-cabal` and `cabal-to-dhall` now respond to `--version`.

* The `dhall` subdirectory has been reorganised so that things that
  are not types are not in the `types` subdirectory. Specifically,
  `dhall/types/Version/v.dhall`, all of
  `dhall/types/VersionRange/*.dhall` and the operations (i.e., the
  enumerations `LicenseId.dhall` and `LicenseExceptionId.dhall`) from
  `dhall/types/SPDX/*.dhall` have been moved to, respectively,
  `dhall/Version/v.dhall`, `dhall/VersionRange/*.dhall`, and
  `dhall/SPDX/*.dhall`. In addition, the files have been renamed as
  appropriate to reflect the name that they are exported from the
  prelude as; in practice, this means that they have gone from
  TitleCase to camelCase.

  Code that only imports `prelude.dhall` and `types.dhall` is
  unaffected by this change.

* `prelude.defaults.Package.license` is now `AllRightsReserved`.

* `dhall-to-cabal` now maps `AllRightsReserved` to `SPDX.NONE` when
  `cabal-version` is at least 2.2.

* `cabal-to-dhall` will now generate more compact `.dhall` files by
  using defaults.

* The default `build-type` is now omission, to use Cabal 2.2's
  inference, and the default `cabal-version` has been bumped to 2.2.

* Export `prelude.types.Scopes`.

## Other Changes

* Bump upper-bounds for `base`, `containers` and `contravariant`. This project
  can build on GHC 8.6 (though will need `--allow-newer` for `Cabal` until this
  is official released).


## 1.1.0.0 -- 2018-06-03

### Breaking Changes

* The type of DhallToCabal.license has changed to
  `Dhall.Type (Either SPDX.License Cabal.License)` to accomodate Cabal 2.2.

### Other Changes

* Increase upper-bound of base to allow 4.11.

* Increase upper-bound of tasty to allow 1.1.

* Switch to Dhall 1.14.0.

* dhall-to-cabal: Fix tracking which branches are already true or false in
  conditionals. Dhall expressions with lots of conditions previously produced
  Cabal files that did not correctly match the requested conditions. See
  https://github.com/dhall-lang/dhall-to-cabal/pull/56,
  https://github.com/dhall-lang/dhall-to-cabal/issues/53 and
  https://github.com/dhall-lang/dhall-to-cabal/issues/55 for more information.

  Thank you to @jneira and @quasicomputational for helping identify and fix this
  bug.

* cabal-to-dhall: Rewrite conditional handling to avoid hangs with complicated ones.
  See https://github.com/dhall-lang/dhall-to-cabal/pull/54 and linked issues.

* Added a warning to generated `.cabal` files against hand-editing.

* `cabal-to-dhall` now pretty prints the resulting Dhall.

* The signature of `CabalToDhall.cabalToDhall` has changed: it now takes the location
  of the `prelude.dhall` and `types.dhall` to import as a parameter.

* Upgrade to Cabal 2.2. This introduces SPDX license identifiers and Dhall
  functionality to manipulate them; see <golden-tests/dhall-to-cabal/SPDX.dhall>
  for a (convoluted) demonstration.

* `prelude.defaults.Executable` has lost its `main-is` field, as it
  makes little sense to have an executable without it.

* `--print-type` now omits the lengthy definition of `Extension`, instead importing
  it from the prelude. `--self-contained` is a new switch to disable this behaviour.


## 1.0.0.1 -- 2018-03-25

Small packaging only tweaks:

* Missing README.md
* Missing author

The irony of this change is not lost on me.

## 1.0.0 -- 2018-03-25

First release!
