{-# LANGUAGE InstanceSigs #-}

import Data.Functor.Sum
import Control.Monad.Reader
import qualified Control.Monad.Fail as Fail
import Writer

newtype StateT s m a = StateT { runStateT :: s -> m (a,s) }

class MonadTrans t where
    lift :: Monad m => m a -> t m a

state :: Monad m => (s -> (a, s)) -> StateT s m a
state f = StateT (return . f)


instance Functor m => Functor (StateT s m) where
  fmap :: (a -> b) -> StateT s m a -> StateT s m b
  fmap f m = StateT $ \st -> fmap updater $ runStateT m st
    where updater ~(x, s) = (f x, s)

instance Monad m => Applicative (StateT s m) where
  pure :: a -> StateT s m a
  pure x = StateT $ \s -> return (x, s)

  (<*>) :: StateT s m (a -> b) -> StateT s m a -> StateT s m b
  f <*> v = StateT $ \s -> do
      ~(g, s') <- runStateT f s
      ~(x, s'') <- runStateT v s'
      return (g x, s'')

instance Monad m => Monad (StateT s m) where
  (>>=) :: StateT s m a -> (a -> StateT s m b) -> StateT s m b
  m >>= k  = StateT $ \s -> do
    ~(x, s') <- runStateT m s
    runStateT (k x) s'


-- instance MonadTrans (StateT s) where
--   lift :: Monad m => m a -> StateT s m a
--   lift m = StateT $ \st -> do
--     a <- m
--     return (a, st)


get :: Monad m => StateT s m s
get = state $ \s -> (s, s)

getAndSet :: Monad m => (s -> s) -> StateT s m s
getAndSet modify = state $ \s -> (s, modify s)

put :: Monad m => s -> StateT s m ()
put s = state $ const ((), s)

modify :: Monad m => (s -> s) -> StateT s m ()
modify f = getAndSet $ const ()

evalStateT :: Monad m => StateT s m a -> s -> m a
evalStateT m s = fst <$> runStateT m s

execStateT :: Monad m => StateT s m a -> s -> m s
execStateT m s = snd <$> runStateT m s

readerToStateT :: Monad m => ReaderT r m a -> StateT r m a
readerToStateT rdr = StateT $ \r -> do
  a <- runReaderT rdr r
  return (a, r)

instance MonadFail m => Fail.MonadFail (StateT s m) where
  fail :: String -> StateT s m a
  fail msg = StateT $ \s -> do fail msg


data Tree a = Leaf a | Fork (Tree a) a (Tree a)

numberAndCount :: Tree () -> (Tree Integer, Integer)
numberAndCount t = getSum <$> runWriter (evalStateT (go t) 1)
  where
    go :: Tree () -> StateT Integer (Writer (Sum Integer)) (Tree Integer)
    go (Leaf _) = do
      lift $ tell 1
      n <- getAndSet succ
      return $ Leaf n

    go (Fork left _ right) = do
      newLeft <- go left
      n <- getAndSet succ
      newRight <- go right
      return $ Fork newLeft n newRight

-- go :: Tree () -> StateT Integer (Writer (Sum Integer)) (Tree Integer)
-- go (Leaf _)            = liftM  Leaf step <* lift (tell $ Sum 1)
-- go (Fork left _ right) = liftM3 Fork (go left) step (go right)
-- step                   = get <* modify succ
