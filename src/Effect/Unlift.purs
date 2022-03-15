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

newtype UnliftEffect m = UnliftEffect (m ~> Effect)

askUnliftEffect :: forall m. MonadUnliftEffect m => m (UnliftEffect m)
askUnliftEffect = withRunInEffect \run -> pure $ UnliftEffect run

askRunInEffect :: forall m a. MonadUnliftEffect m => m (m a -> Effect a)
askRunInEffect = withRunInEffect pure

withUnliftEffect
  :: forall m a. MonadUnliftEffect m => (UnliftEffect m -> Effect a) -> m a
withUnliftEffect runAction =
  withRunInEffect \run -> runAction $ UnliftEffect run

toEffect :: forall m a. MonadUnliftEffect m => m a -> m (Effect a)
toEffect m = withRunInEffect \run -> pure $ run m
