# `dhall-to-cabal` -- generate Cabal files from Dhall expressions

*Work in progress* - there's plenty more to do here. Feel free to get stuck in
and submit pull requests!

`dhall-to-cabal` takes a Dhall expression that evaluates to a record containing
fields fo a `.cabal` file. For example, the following Dhall expression...

``` dhall
{ benchmarks        = [ { main-is = "hello", name = "a-benchmark" } ]
, executables       = [ { main-is = "Main", name = "exe" } ]
, foreign-libraries = [ { name = "foo", type = < Shared = {=} > } ]
, library           =
    [ { name = [ "Hello" ] : Optional Text } ] : Optional
                                                 { name : Optional Text }
, package           = { name = "test-package", version = [ +1, +0, +3 ] }
, tests             = [ { main-is = "Main.hs", name = "tests" } ]
, x-fields          = [ { _1 = "x-foo", _2 = "bar" } ]
, source-repos = [ {=} ]
} 
```

produces

``` cabal
name: test-package
version: 1.0.3
cabal-version: 2.0
build-type: Simple
license: OtherLicense
x-foo: bar

source-repository head

library
    exposed: True
    buildable: True

foreign library foo
    type: native-shared
    buildable: True

executable exe
    main-is: Main
    scope: public
    buildable: True

test-suite tests
    type: exitcode-stdio-1.0
    main-is: Main.hs
    buildable: True

benchmark a-benchmark
    type: exitcode-stdio-1.0
    main-is: hello
    buildable: True
```
