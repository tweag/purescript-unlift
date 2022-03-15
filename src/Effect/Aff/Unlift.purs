module Effect.Aff.Unlift where

import Prelude

import Control.Monad.Reader (ReaderT(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)

-- | Monads which allow their actions to be run in `Aff`.
-- |
-- | `MonadUnliftAff` captures the opposite notion of `MonadAff` - while
-- | `MonadAff` allows an `Aff` to be lifted into another monad,
-- | `MonadUnliftAff` allows a monad other than `Aff` to be run in an
-- | `Aff` context.
-- |
-- | Note that the laws given below require that a monad have no "monadic
-- | state", which essentially limits instances to `ReaderT` stacks with a
-- | base of `Aff`.
-- |
-- | Instances should satisfy the following laws, which state that
-- | `unliftAff` is a transformer of monads for any given `u` returned by
-- | `askUnliftAff`:.
-- |
-- | ```purescript
-- | unliftAff u <<< pure = pure
-- | unliftAff u (f =<< m) = unliftAff u <<< f =<< unliftAff u m
-- | ```
class MonadAff m <= MonadUnliftAff m where
  -- | Run an `Aff` with access to a runner function that is capable of
  -- | running a monadic action `m` in `Aff`.
  withRunInAff :: forall b. ((m ~> Aff) -> Aff b) -> m b

instance MonadUnliftAff Aff where
  withRunInAff runAction = runAction identity

instance MonadUnliftAff m => MonadUnliftAff (ReaderT r m) where
  withRunInAff runAction = ReaderT \context ->
    withRunInAff \runMInAff ->
      runAction \(ReaderT reader) ->
        runMInAff $ reader context

-- | A newtype wrapper around a natural transformation from `m` to `Aff`.
newtype UnliftAff m = UnliftAff (m ~> Aff)

-- | Run an action directly in `Aff`. Use `askUnliftAff` or
-- | `withUnliftAff` to obtain an `UnliftAff m` value.
unliftAff :: forall m. UnliftAff m -> m ~> Aff
unliftAff (UnliftAff run) = run

-- | Returns a natural transformation from `m` to `Aff` within an `m` context.
-- | This can subsequently be used to run `m` actions directly in `Aff`.
askUnliftAff :: forall m. MonadUnliftAff m => m (UnliftAff m)
askUnliftAff = withRunInAff \run -> pure $ UnliftAff run

-- | A monomorphic version of askUnliftAff which can be more convenient when
-- | you only want to use the resulting runner function once with a concrete
-- | type.
-- |
-- | If you run into type issues using this, try using `askUnlitAff` instead.
askRunInAff :: forall m a. MonadUnliftAff m => m (m a -> Aff a)
askRunInAff = withRunInAff pure

-- | A version of `withRunInAff` that provides an `UnliftAff` wrapper
-- | instead of a rank-2 polymorphic function.
withUnliftAff
  :: forall m a. MonadUnliftAff m => (UnliftAff m -> Aff a) -> m a
withUnliftAff runAction =
  withRunInAff \run -> runAction $ UnliftAff run

-- | Run the given action inside the `Aff` monad.
toAff :: forall m a. MonadUnliftAff m => m a -> m (Aff a)
toAff m = withRunInAff \run -> pure $ run m
