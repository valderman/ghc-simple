ghc-simple
==========

The GHC API is a great tool for working with Haskell code. Unfortunately, it's
also fairly opaque and hard to get started with. This library abstracts away
the intricacies of working with the GHC API, giving a general, no-nonsense way
to extract highly optimized (or not, depending on your use case) Core, STG,
custom intermediate code, and other information from Haskell code.


TODO
----

* Caching and loading generated code on demand based on symbol (and/or module)
  dependencies.
