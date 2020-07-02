{-# LANGUAGE InstanceSigs #-}

import Control.Applicative
import Writer

newtype Reader r a = Reader { runReader :: r -> a }

newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

ask :: Monad m => ReaderT r m r
ask = ReaderT return

asks :: Monad m => (r -> a) -> ReaderT r m a
asks f = ReaderT $ return . f

local :: Monad m => (r -> r) -> ReaderT r m a -> ReaderT r m a
local f rdr = ReaderT $ (runReaderT rdr) . f

reader :: Monad m => (r -> a) -> ReaderT r m a
reader f = ReaderT (return . f)

instance Functor (Reader r) where
    fmap :: (a -> b) -> Reader r a -> Reader r b
    fmap f rdr = Reader $ f . runReader rdr

instance Functor m => Functor (ReaderT r m) where
    fmap :: (a -> b) -> ReaderT r m a -> ReaderT r m b
    fmap f rdr = ReaderT $ fmap f . runReaderT rdr


instance Applicative (Reader r) where
    pure :: a -> Reader r a
    pure = Reader . const

    (<*>) :: Reader r (a->b) -> Reader r a -> Reader r b
    rf <*> rv = Reader $ \r -> let
        f = runReader rf r
        v = runReader rv r
        in f v

instance (Applicative m) => Applicative (ReaderT r m) where
    pure :: a -> ReaderT r m a
    pure = ReaderT . const . pure

    (<*>) :: ReaderT r m (a->b) -> ReaderT r m a -> ReaderT r m b
    -- rf <*> rv = ReaderT $ \r -> let
    --     mf = runReaderT rf r
    --     mv = runReaderT rv r
    --     in mf <*> mv

    rf <*> rv = ReaderT $ liftA2 (<*>) (runReaderT rf) (runReaderT rv)


instance Monad (Reader r) where
    (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
    rdr >>= k = Reader $ \r -> runReader (k $ runReader rdr r) r

instance (Monad m) => Monad (ReaderT r m) where
{-
    (>>=) :: ReaderT r m a -> (a -> ReaderT r m b) -> ReaderT r m b
    rdr >>= k = ReaderT $ \r -> let
        ma = runReaderT rdr r
        in ma >>= \a -> runReaderT (k a) r
-}
    (>>=) :: ReaderT r m a -> (a -> ReaderT r m b) -> ReaderT r m b
    rdr >>= k = ReaderT $ \r -> do
        a <- runReaderT rdr r
        runReaderT (k a) r


newtype Arr2T e1 e2 m a = Arr2T { getArr2T :: e1 -> e2 -> m a }
newtype Arr3T e1 e2 e3 m a = Arr3T { getArr3T :: e1 -> e2 -> e3 -> m a }

arr2 :: Monad m => (e1 -> e2 -> a) -> Arr2T e1 e2 m a
arr2 f = Arr2T $ \e1 e2 -> return $ f e1 e2

arr3 :: Monad m => (e1 -> e2 -> e3 -> a) -> Arr3T e1 e2 e3 m a
arr3 f = Arr3T $ \e1 e2 e3 -> return $ f e1 e2 e3

-- GHCi> (getArr2T $ arr2 (+)) 33 9 :: [Integer]
-- [42]
-- GHCi> (getArr3T $ arr3 foldr) (*) 1 [1..5] :: Either String Integer
-- Right 120
-- GHCi> import Data.Functor.Identity
-- GHCi> runIdentity $ (getArr2T $ arr2 (+)) 33 9
-- 42

instance (Functor m) => Functor (Arr2T e1 e2 m) where
    fmap :: (a -> b) -> Arr2T e1 e2 m a -> Arr2T e1 e2 m b
    fmap f arr =  Arr2T $ (fmap . fmap . fmap $ f) $ getArr2T arr

instance (Functor m) => Functor (Arr3T e1 e2 e3 m) where
    fmap :: (a -> b) -> Arr3T e1 e2 e3 m a -> Arr3T e1 e2 e3 m b
    fmap f arr =  Arr3T $ \e1 e2 e3 -> f <$> getArr3T arr e1 e2 e3

instance (Applicative m) => Applicative (Arr2T e1 e2 m) where
    pure :: a -> Arr2T e1 e2 m a
    pure a =  Arr2T $ \_ _ -> pure a

    (<*>) :: Arr2T e1 e2 m (a -> b) -> Arr2T e1 e2 m a -> Arr2T e1 e2 m b
    af <*> av = Arr2T $ \e1 e2 -> (getArr2T af e1 e2) <*> (getArr2T av e1 e2)

instance (Applicative m) => Applicative (Arr3T e1 e2 e3 m) where
    pure :: a -> Arr3T e1 e2 e3 m a
    pure a =  Arr3T $ \_ _ _ -> pure a

    (<*>) :: Arr3T e1 e2 e3 m (a -> b) -> Arr3T e1 e2 e3 m a -> Arr3T e1 e2 e3 m b
    af <*> av = Arr3T $ \e1 e2 e3 -> (getArr3T af e1 e2 e3) <*> (getArr3T av e1 e2 e3)

    -- pure = Arr3T . const . const . const . pure
    -- f <*> x = Arr3T $ (liftA2 . liftA2 . liftA2) (<*>) (getArr3T f) (getArr3T x)

instance (Monad m) => Monad (Arr2T e1 e2 m) where
    (>>=) :: Arr2T e1 e2 m a -> (a -> Arr2T e1 e2 m b) -> Arr2T e1 e2 m b
    arr >>= k = Arr2T $ \e1 e2 -> do
        a <- getArr2T arr e1 e2
        getArr2T (k a) e1 e2

instance (Monad m) => Monad (Arr3T e1 e2 e3 m) where
    (>>=) :: Arr3T e1 e2 e3 m a -> (a -> Arr3T e1 e2 e3 m b) -> Arr3T e1 e2 e3 m b
    arr >>= k = Arr3T $ \e1 e2 e3 -> do
        a <- getArr3T arr e1 e2 e3
        getArr3T (k a) e1 e2 e3


class MonadTrans t where
    lift :: Monad m => m a -> t m a

instance MonadTrans (Arr2T e1 e2) where
    lift :: m a -> Arr2T e1 e2 m a
    lift m =  Arr2T $ \_ _ -> m

asks2 :: Monad m => (e1 -> e2 -> a) -> Arr2T e1 e2 m a
asks2 f = Arr2T $ \e1 e2 -> return $ f e1 e2

-- GHCi> a2l = Arr2T $ \e1 e2 -> [e1,e2]
-- GHCi> getArr2T (do {x <- a2l; y <- a2l; return (x + y)}) 3 5
-- [6,8,8,10]
-- GHCi> a3m = Arr3T $ \e1 e2 e3 -> Just (e1 + e2 + e3)
-- GHCi> getArr3T (do {x <- a3m; y <- a3m; return (x * y)}) 2 3 4
-- Just 81

-- GHCi> a2l = Arr2T $ \e1 e2 -> [e1,e2,e1+e2]
-- GHCi> (getArr2T $ succ <$> a2l) 10 100
-- [11,101,111]
-- GHCi> a3e = Arr3T $ \e1 e2 e3 -> Right (e1+e2+e3)
-- GHCi> (getArr3T $ sqrt <$> a3e) 2 3 4
-- Right 3.0

data Tree a = Leaf a | Fork (Tree a) a (Tree a)

numberAndCount :: Tree () -> (Tree Integer, Integer)
numberAndCount t = getSum <$> runWriter (evalStateT (go t) 1)
  where
    go :: Tree () -> StateT Integer (Writer (Sum Integer)) (Tree Integer)
    go = undefined
