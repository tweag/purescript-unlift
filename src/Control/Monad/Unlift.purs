module Control.Monad.Unlift where

import Prelude

import Control.Monad.Base (class MonadBase)
import Control.Monad.Reader (ReaderT(..))
import Data.Either (Either)
import Data.Identity (Identity)
import Data.List (List)
import Data.Maybe (Maybe)
import Data.Tuple (Tuple)
import Effect (Effect)
import Effect.Aff (Aff)

-- | Monads which allow their actions to be run in a base monad.
-- |
-- | `MonadUnlift` captures the opposite notion of `MonadBase` - while
-- | `MonadBase` allows any base monad `b` to be lifted into a transformed monad
-- | `m`, `MonadUnlift` allows `m` to be run in `b`, as long as the outer
-- | context is in `m`.
-- |
-- | Note that the laws given below require that a monad have no "monadic
-- | state", which essentially limits instances to `ReaderT` and `IdentityT`
-- | stacks.
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

instance Monoid a => MonadUnlift (Tuple a) (Tuple a) where
  withRunInBase runAction = runAction identity

instance MonadUnlift b m => MonadUnlift b (ReaderT r m) where
  withRunInBase runAction = ReaderT \context ->
    withRunInBase \runMInBase ->
      runAction \(ReaderT reader) ->
        runMInBase $ reader context

-- TODO add MonadBase instance for IdentityT
-- instance MonadUnlift b m => MonadUnlift b (IdentityT m) where
--   withRunInBase runAction = IdentityT $
--     withRunInBase \runMInBase ->
--       runAction \(IdentityT a) ->
--         runMInBase a

-- | A newtype wrapper around a natural transformation from `m` to `b`.
newtype Unlift :: forall k. (k -> Type) -> (k -> Type) -> Type
newtype Unlift b m = Unlift (m ~> b)

-- | Run an action directly in a base monad `b`. Use `askUnlift` or `withUnlift`
-- | to obtain an `Unlift b m` value.
unlift :: forall b m. Unlift b m -> m ~> b
unlift (Unlift run) = run

-- | Returns a natural transformation from `m` to `b` within an `m` context.
-- | This can subsequently be used to run `m` actions in the base monad `b`.
askUnlift :: forall b m. MonadUnlift b m => m (Unlift b m)
askUnlift = withRunInBase \run -> pure $ Unlift run

-- | A monomorphic version of askUnlift which can be more convenient when you
-- | only want to use the resulting runner function once with a concrete type.
-- |
-- | If you run into type issues using this, try using `askUnlit` instead.
askRunInBase :: forall b m a. MonadUnlift b m => m (m a -> b a)
askRunInBase = withRunInBase pure

-- | A version of `withRunInBase` that provides an `Unlift` wrapper instead of
-- | a rank-2 polymorphic function.
withUnlift :: forall b m a. MonadUnlift b m => (Unlift b m -> b a) -> m a
withUnlift runAction = withRunInBase \run -> runAction $ Unlift run

-- | Run the given action inside the base monad `b`.
toBase :: forall b m a. MonadUnlift b m => m a -> m (b a)
toBase m = withRunInBase \run -> pure $ run m
