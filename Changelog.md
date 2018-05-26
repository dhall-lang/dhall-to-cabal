# dhall-to-cabal change log

## 1.0.1.0 -- UNRELEASED

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


## 1.0.0.1 -- 2018-03-25

Small packaging only tweaks:

* Missing README.md
* Missing author

The irony of this change is not lost on me.

## 1.0.0 -- 2018-03-25

First release!
