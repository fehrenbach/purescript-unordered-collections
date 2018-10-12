# purescript-unordered-collections
[![Build Status](https://travis-ci.org/fehrenbach/purescript-unordered-collections.svg?branch=master)](https://travis-ci.org/fehrenbach/purescript-unordered-collections)
<a href="https://pursuit.purescript.org/packages/purescript-unordered-collections">
  <img src="https://pursuit.purescript.org/packages/purescript-unordered-collections/badge"
       alt="purescript-unordered-collections on Pursuit">
  </img>
</a>

Hash-based, immutable collection types for PureScript.

The two main goals are good performance and API compatibility with the `Ord`-based collections wherever reasonable.

The implementation of `HashMap` is based on "Optimizing Hash-Array
Mapped Tries for Fast and Lean Immutable JVM Collections" (Steindorfer
and Vinju, OOPSLA 2015, https://doi.org/10.1145/2814270.2814312).

Performance
-----------

HashMaps are fast. Most common operations are 1.5x-3x faster than the
`Ord`-based map. Very limited testing puts `insert` and `lookup` at
around 10% faster than immutable.js.

Related
-------

- [purescript-ordered-collections](https://github.com/purescript/purescript-ordered-collections): ordered collections in PureScript
- [unordered-containers](http://hackage.haskell.org/package/unordered-containers): efficient hashing-based container types in Haskell
- [immutable.js](https://facebook.github.io/immutable-js/): immutable collections for JavaScript and TypeScript
- [io.lacuna.bifurcan](https://github.com/lacuna/bifurcan): high-quality Java implementations of mutable and immutable data structures
- [im-rs](http://immutable.rs): blazing fast immutable collection datatypes for Rust

Documentation
-------------

Module documentation is [published on Pursuit](https://pursuit.purescript.org/packages/purescript-unordered-collections).
