# `dhall-to-cabal` -- generate Cabal files from Dhall expressions

*Work in progress* - there's plenty more to do here. Feel free to get stuck in
and submit pull requests!

`dhall-to-cabal` takes a Dhall expression that evaluates to a record containing
fields fo a `.cabal` file and produces a valid `.cabal` file. For a large
example, this very project has a
[`dhall-to-cabal.dhall`](./dhall-to-cabal.dhall) file which generates
[`dhall-to-cabal.cabal`](./dhall-to-cabal.cabal). Don't be put off by the
verbosity, [issue #3](https://github.com/ocharles/dhall-to-cabal/issues/3)
tracks providing a much nicer standard library (in Dhall-land) to generate these
Dhall expressions. Such is the beauty of having a powerful language behind us!

## y tho?

I love Cabal, and `.cabal` files aren't *bad*, but they are hardly great either.
A major limitation is the lack of a way to import things. This leads to code
duplication and repetition, and we all know this is a bad thing. The means of
abstraction in a cabal file a pretty rudimentary too - why not just give us
proper variables and let us figure out how best to abstract things?

If you accept the above argument, then Dhall-to-Cabal might be of interest to
you. This approach enables a few nice things:

* The ability to share your Cabal configuration files inside the configuration
  of other services without having to write a bunch of Haskell code. It's just
  Dhall, so you can just import it and manipulate it as you'd like.
  
* The ability for projects to provide a template that can be included by users.
  Large frameworks such as Yesod could provide a `yesod.dhall` that is imported
  by users. One might imagine:
  
  ```dhall
  let yesodProject = http://.../yesod in
  yesodProject {
    name = "my-cool-website",
    handlers = [ "Handler.Index", "Handler.CoolThing" ],
    models = [ "Model.Cat", "Model.Dog" ]
  } 
  ```
  
  and have `yesodProject` turn this into a valid Cabal plan, bringing down
  Yesod, enabling type extensions, turning on specific warnings, all that
  goodness.

  We can also see the desire to have this import/reuse functionality in core
  libraries. The [`network`](https://hackage.haskell.org/package/network) opens
  with a stanza about how you have to add the library to your cabal file which
  is totally non-standard. Just give me an expression to import, and then I
  don't have to worry about the details!
