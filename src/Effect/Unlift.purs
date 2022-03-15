module Effect.Unlift where

import Prelude

import Control.Monad.Identity.Trans (IdentityT(..))
import Control.Monad.Reader (ReaderT(..))
import Effect (Effect)
import Effect.Class (class MonadEffect)

-- | Monads which allow their actions to be run in `Effect`.
-- |
-- | `MonadUnliftEffect` captures the opposite notion of `MonadEffect` - while
-- | `MonadEffect` allows an `Effect` to be lifted into another monad,
-- | `MonadUnliftEffect` allows a monad other than `Effect` to be run in an
-- | `Effect` context.
-- |
-- | Note that the laws given below require that a monad have no "monadic
-- | state", which essentially limits instances to `ReaderT` and `IdentityT`
-- | stacks with a base of `Effect`.
-- |
-- | Instances should satisfy the following laws, which state that
-- | `unliftEffect` is a transformer of monads for any given `u` returned by
-- | `askUnliftEffect`:
-- |
-- | ```purescript
-- | unliftEffect u <<< pure = pure
-- | unliftEffect u (f =<< m) = unliftEffect u <<< f =<< unliftEffect u m
-- | ```
class MonadEffect m <= MonadUnliftEffect m where
  -- | Run an `Effect` with access to a runner function that is capable of
  -- | running a monadic action `m` in `Effect`.
  withRunInEffect :: forall b. ((m ~> Effect) -> Effect b) -> m b

instance MonadUnliftEffect Effect where
  withRunInEffect runAction = runAction identity

instance MonadUnliftEffect m => MonadUnliftEffect (ReaderT r m) where
  withRunInEffect runAction = ReaderT \context ->
    withRunInEffect \runMInEffect ->
      runAction \(ReaderT reader) ->
        runMInEffect $ reader context

instance MonadUnliftEffect m => MonadUnliftEffect (IdentityT m) where
  withRunInEffect runAction = IdentityT $
    withRunInEffect \runMInEffect ->
      runAction \(IdentityT a) ->
        runMInEffect a

-- | A newtype wrapper around a natural transformation from `m` to `Effect`.
newtype UnliftEffect m = UnliftEffect (m ~> Effect)

-- | Run an action directly in `Effect`. Use `askUnliftEffect` or
-- | `withUnliftEffect` to obtain an `UnliftEffect m` value.
unliftEffect :: forall m. UnliftEffect m -> m ~> Effect
unliftEffect (UnliftEffect run) = run

-- | Returns a natural transformation from `m` to `Effect` within an `m` context.
-- | This can subsequently be used to run `m` actions directly in `Effect`.
askUnliftEffect :: forall m. MonadUnliftEffect m => m (UnliftEffect m)
askUnliftEffect = withRunInEffect \run -> pure $ UnliftEffect run

-- | A monomorphic version of askUnliftEffect which can be more convenient when
-- | you only want to use the resulting runner function once with a concrete
-- | type.
-- |
-- | If you run into type issues using this, try using `askUnlitEffect` instead.
askRunInEffect :: forall m a. MonadUnliftEffect m => m (m a -> Effect a)
askRunInEffect = withRunInEffect pure

-- | A version of `withRunInEffect` that provides an `UnliftEffect` wrapper
-- | instead of a rank-2 polymorphic function.
withUnliftEffect
  :: forall m a. MonadUnliftEffect m => (UnliftEffect m -> Effect a) -> m a
withUnliftEffect runAction =
  withRunInEffect \run -> runAction $ UnliftEffect run

-- | Run the given action inside the `Effect` monad.
toEffect :: forall m a. MonadUnliftEffect m => m a -> m (Effect a)
toEffect m = withRunInEffect \run -> pure $ run m
