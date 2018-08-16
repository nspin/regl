# Fast, applicative, streaming regular expressions for Haskell

This repository contains a set of libraries for constructing functions on regular languages.

The core type, `Data.Regex.Poly.RE s a`, is function from a regular language over `s` to `a`.
`RE`'s are composed in the applicative style.
Primitives include what you would expect from a regular expression library, plus some uncommon but useful ones, such as `complement` and `intersect`.

A slow but easy to use API is implemented using regular expression derivatives.
A fast but memory-hungry API is implemented using DFA's.
An in-progress API will use an NFA-like "virtual machine" [1] for a balance of speed, memory efficiency, and convenience.

I'm working on implementing a memory-efficient streaming approach to matching.
Work towards that goal can be found in `./regl-conduit`.

See `./regl-demo` for some examples.

### Credit

Some ideas for functionality are taken from [regex-applicative](https://github.com/feuerbach/regex-applicative).

Some combinators are taken from [attoparsec](https://github.com/bos/attoparsec) and [parsers](https://github.com/ekmett/parsers/).

### References

[1] https://swtch.com/~rsc/regexp/regexp2.html
