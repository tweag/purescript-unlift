# purescript-unlift

[![Latest release](http://img.shields.io/github/release/purescript/purescript-unlift.svg)](https://github.com/purescript/purescript-unlift/releases)
[![Build status](https://github.com/purescript/purescript-unlift/workflows/CI/badge.svg?branch=main)](https://github.com/purescript/purescript-unlift/actions?query=workflow%3ACI+branch%3Amain)
[![Pursuit](https://pursuit.purescript.org/packages/purescript-unlift/badge)](https://pursuit.purescript.org/packages/purescript-unlift)

Classes that allow stateless monads to be "unlifted" into a base monad.

- `MonadUnliftEffect`: Run any compatible monad in `Effect`
- `MonadUnliftAff`: Run any compatible monad in `Aff`
- `MonadUnlift`: Run any compatible monad in any base monad.

Based on:

- [unliftio](http://hackage.haskell.org/package/unliftio)
- [unlift](http://hackage.haskell.org/package/unlift)

## Installation

```
spago install unlift
```

## Documentation

Module documentation is [published on Pursuit](http://pursuit.purescript.org/packages/purescript-unlift).
