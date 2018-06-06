# dhall-to-cabal change log

## NEXT

* `os` conditions where the operating system's name was not recognised
  (e.g., `os(multics)`) were crashing cabal-to-dhall. They now work as
  expected.

* Remove orphan `Dhall.Core.Inject` instances for `[Char]` and
  `CompilerFlavor`.

* `dhall-to-cabal` and `cabal-to-dhall` now respond to `--version`.

* `CabalToDhall.cabalToDhall` is now a pure function that accepts a
  `GenericPackageDescription`. A new convenience function has been
  added to `CabalToDhall`, `parseGenericPackageDescriptionThrows`.

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
