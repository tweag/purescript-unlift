module Control.Monad.Base where

import Prelude

import Control.Monad.Cont (ContT)
import Control.Monad.Except (ExceptT)
import Control.Monad.Identity.Trans (IdentityT)
import Control.Monad.List.Trans (ListT)
import Control.Monad.Maybe.Trans (MaybeT)
import Control.Monad.RWS (RWST)
import Control.Monad.Reader (ReaderT)
import Control.Monad.ST (ST)
import Control.Monad.State (StateT)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Control.Monad.Writer (WriterT)
import Data.Either (Either)
import Data.Identity (Identity)
import Data.List (List)
import Data.List.Lazy as LL
import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Aff (Aff)

-- | A class for lifting computations in a monad at the bottom of a transformer
-- | stack.
-- |
-- | `MonadBase` is a generalization of the concept represented by classes like
-- | `MonadEffect` and `MonadAff`.
-- |
-- | Instances should satisfy the following laws, which state that `liftBase`
-- | is a transformer of monads:
-- |
-- | ```purescript
-- | liftBase <<< pure = pure
-- | liftBase (f =<< m) = liftBase <<< f =<< liftBase m
-- | ```
class (Monad b, Monad m) <= MonadBase b m | m -> b where
  -- | Lift a computation from the base monad.
  liftBase :: b ~> m

instance MonadBase Array Array where
  liftBase = identity

instance MonadBase List List where
  liftBase = identity

instance MonadBase LL.List LL.List where
  liftBase = identity

instance MonadBase Maybe Maybe where
  liftBase = identity

instance MonadBase Effect Effect where
  liftBase = identity

instance MonadBase Aff Aff where
  liftBase = identity

instance MonadBase Identity Identity where
  liftBase = identity

instance MonadBase (Either e) (Either e) where
  liftBase = identity

instance MonadBase (ST s) (ST s) where
  liftBase = identity

instance MonadBase (Function r) (Function r) where
  liftBase = identity

instance MonadBase b m => MonadBase b (ListT m) where
  liftBase = liftBaseDefault

instance MonadBase b m => MonadBase b (MaybeT m) where
  liftBase = liftBaseDefault

instance (Monoid w, MonadBase b m) => MonadBase b (WriterT w m) where
  liftBase = liftBaseDefault

instance MonadBase b m => MonadBase b (ExceptT e m) where
  liftBase = liftBaseDefault

instance MonadBase b m => MonadBase b (StateT s m) where
  liftBase = liftBaseDefault

instance MonadBase b m => MonadBase b (ReaderT r m) where
  liftBase = liftBaseDefault

instance MonadBase b m => MonadBase b (IdentityT m) where
  liftBase = liftBaseDefault

instance MonadBase b m => MonadBase b (ContT r m) where
  liftBase = liftBaseDefault

instance (Monoid w, MonadBase b m) => MonadBase b (RWST r w s m) where
  liftBase = liftBaseDefault

-- | A default implementation of `liftBase` which is defined as 
-- |
-- | ```purescript
-- | lift <<< liftBase
-- | ```
liftBaseDefault
  :: forall t m b. MonadTrans t => Monad m => MonadBase b m => b ~> t m
liftBaseDefault = lift <<< liftBase
