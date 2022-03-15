# purescript-unlift

[![Latest release](http://img.shields.io/github/release/purescript/purescript-unlift.svg)](https://github.com/purescript/purescript-unlift/releases)
[![Build status](https://github.com/purescript/purescript-unlift/workflows/CI/badge.svg?branch=main)](https://github.com/purescript/purescript-unlift/actions?query=workflow%3ACI+branch%3Amain)
[![Pursuit](https://pursuit.purescript.org/packages/purescript-unlift/badge)](https://pursuit.purescript.org/packages/purescript-unlift)

Standard library for working with monads at the base of a transformer stack.
`MonadBase` allow any base monad to be lifted into a transformed monad à la
`MonadEffect` and `MonadAff`. `MonadUnliftEffect`, `MonadUnliftAff`, and
`MonadUnlift` allow running a stateless transformed monad inside of a base
monad. Based on:

- [transformers-base](http://hackage.haskell.org/package/transformers-base)
- [unliftio](http://hackage.haskell.org/package/unliftio)
- [unlift](http://hackage.haskell.org/package/unlift)

## Installation

```
spago install unlift
```

## Documentation

Module documentation is [published on Pursuit](http://pursuit.purescript.org/packages/purescript-unlift).

