module Control.Monad.Unlift where

import Prelude

import Control.Monad.Base (class MonadBase)
import Control.Monad.Identity.Trans (IdentityT(..))
import Control.Monad.Reader (ReaderT(..))
import Control.Monad.ST (ST)
import Data.Either (Either)
import Data.Identity (Identity)
import Data.List (List)
import Data.List.Lazy as LL
import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Aff (Aff)

-- | Monads which allow their actions to be run in their base monad.
-- |
-- | `MonadUnlift` captures the opposite notion of `MonadBase` - while
-- | `MonadBase` allows an base monad `b` to be lifted into another monad `m`,
-- | `MonadUnlift` allows a `m` to be run in `b`, as long as contained in an
-- | outer `m` context.
-- |
-- | Note that the laws given below require that a monad have no "monadic
-- | state", which essentially limits instances to `ReaderT` and `IdentityT`
-- | stacks with a base of `b`.
-- |
-- | Instances should satisfy the following laws, which state that
-- | `unlift` is a transformer of monads for any given `u` returned by
-- | `askUnlift`:
-- |
-- | ```purescript
-- | unlift u <<< pure = pure
-- | unlift u (f =<< m) = unlift u <<< f =<< unlift u m
-- | ```
class MonadBase b m <= MonadUnlift b m | m -> b where
  -- | Run a base monad action with access to a runner function that is capable
  -- | of running a monadic action `m` in `b` (strictly speaking, a natural
  -- | transformation from `m` to `b`).
  withRunInBase :: forall x. ((m ~> b) -> b x) -> m x

instance MonadUnlift Array Array where
  withRunInBase runAction = runAction identity

instance MonadUnlift List List where
  withRunInBase runAction = runAction identity

instance MonadUnlift LL.List LL.List where
  withRunInBase runAction = runAction identity

instance MonadUnlift Maybe Maybe where
  withRunInBase runAction = runAction identity

instance MonadUnlift Effect Effect where
  withRunInBase runAction = runAction identity

instance MonadUnlift Aff Aff where
  withRunInBase runAction = runAction identity

instance MonadUnlift Identity Identity where
  withRunInBase runAction = runAction identity

instance MonadUnlift (Either e) (Either e) where
  withRunInBase runAction = runAction identity

instance MonadUnlift (ST s) (ST s) where
  withRunInBase runAction = runAction identity

instance MonadUnlift b m => MonadUnlift b (ReaderT r m) where
  withRunInBase runAction = ReaderT \context ->
    withRunInBase \runMInBase ->
      runAction \(ReaderT reader) ->
        runMInBase $ reader context

instance MonadUnlift b m => MonadUnlift b (IdentityT m) where
  withRunInBase runAction = IdentityT $
    withRunInBase \runMInBase ->
      runAction \(IdentityT a) ->
        runMInBase a

newtype Unlift :: forall k. (k -> Type) -> (k -> Type) -> Type
newtype Unlift b m = Unlift (m ~> b)

askUnlift :: forall b m. MonadUnlift b m => m (Unlift b m)
askUnlift = withRunInBase \run -> pure $ Unlift run

askRunInBase :: forall b m a. MonadUnlift b m => m (m a -> b a)
askRunInBase = withRunInBase pure

withUnlift :: forall b m a. MonadUnlift b m => (Unlift b m -> b a) -> m a
withUnlift runAction = withRunInBase \run -> runAction $ Unlift run

toBase :: forall b m a. MonadUnlift b m => m a -> m (b a)
toBase m = withRunInBase \run -> pure $ run m
