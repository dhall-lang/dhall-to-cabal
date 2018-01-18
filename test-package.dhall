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
