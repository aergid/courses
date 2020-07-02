{-# LANGUAGE InstanceSigs #-}
module Writer where

import Data.Tuple
import Control.Applicative
import Control.Monad.Identity
import Control.Monad
import qualified Control.Monad.Fail as Fail

class MonadTrans t where
    lift :: Monad m => m a -> t m a

newtype Writer w a = Writer { runWriter :: (a, w) }

newtype WriterT w m a = WriterT { runWriterT :: m (a, w) }

writer :: Monad m => (a, w) -> WriterT w m a
writer = WriterT . return

execWriter :: Writer w a -> w
execWriter = snd . runWriter

execWriterT :: Monad m => WriterT w m a -> m w
execWriterT = fmap snd . runWriterT

tell :: Monad m => w -> WriterT w m ()
tell w = writer ((), w)

listen :: Monad m => WriterT w m a -> WriterT w m (a, w)
listen m = WriterT $ do
    ~(a, w) <- runWriterT m
    return ((a, w), w)

censor :: Monad m => (w -> w) -> WriterT w m a -> WriterT w m a
censor f m = WriterT $ do
    ~(a, w) <- runWriterT m
    return (a, f w)

instance Functor (Writer w) where
    fmap :: (a -> b) -> Writer w a -> Writer w b
    fmap f w = Writer $ swap . fmap f . swap . runWriter $ w

instance (Monad m) => Functor (WriterT w m) where
    fmap :: (a -> b) -> WriterT w m a -> WriterT w m b
    fmap f w = WriterT $ do
        (v, w) <- runWriterT w
        return (f v, w)

instance (Monoid w) => Applicative (Writer w) where
    pure :: a -> Writer w a
    pure a = Writer (a, mempty)

    (<*>) :: Writer w (a->b) -> Writer w a -> Writer w b
    wf <*> wv = Writer $ updater (runWriter wf) (runWriter wv)
        where updater ~(f, w) ~(v, w') = (f v, w `mappend` w')

instance (Monoid w, Monad m) => Applicative (WriterT w m) where
    pure :: a -> WriterT w m a
    pure a = WriterT $ return (a, mempty)

    (<*>) :: WriterT w m (a->b) -> WriterT w m a -> WriterT w m b
    wf <*> wv = WriterT $ liftA2 updater (runWriterT wf) (runWriterT wv)
        where updater ~(f, w) ~(v, w') = (f v, w `mappend` w')

instance (Monoid w) => Monad (Writer w) where
    (>>=) :: Writer w a -> (a -> Writer w b) -> Writer w b
    wrt >>= k = Writer $ let
        (v, w) = runWriter wrt
        (v', w') = runWriter (k v)
        in (v', w `mappend` w')

instance (Monoid w, Monad m) => Monad (WriterT w m) where
    (>>=) :: WriterT w m a -> (a -> WriterT w m b) -> WriterT w m b
    wrt >>= k = WriterT $ do
        (v, w) <- runWriterT wrt
        (v', w') <- runWriterT (k v)
        return (v', w `mappend` w')

instance (Monoid w) => MonadTrans (WriterT w) where
    lift :: Monad m => m a -> WriterT w m a
    lift m = WriterT $ do
        a <- m
        return (a, mempty)

-- fail :: String -> WriterT w m a
-- fail = WriterT . fail

data Logged a = Logged String a deriving (Eq,Show)

newtype LoggT m a = LoggT { runLoggT :: m (Logged a) }

write2log :: Monad m => String -> LoggT m ()
write2log s = LoggT $ return (Logged s ())

type Logg = LoggT Identity

runLogg :: Logg a -> Logged a
runLogg = runIdentity . runLoggT


instance Functor Logged where
    fmap f (Logged s a) = Logged s (f a)
    -- fmap = liftM

instance Applicative Logged where
    pure = Logged ""
    (Logged s1 f) <*> (Logged s2 v) = Logged (s1 ++ s2) $ f v
    -- (<*>) = ap

instance Monad Logged where
    return = Logged ""
    (Logged s1 a) >>= f = Logged (s1 ++ s2) v where
        (Logged s2 v) = f a

instance Functor m => Functor (LoggT m) where
    fmap f lg = LoggT $ (fmap f) <$> runLoggT lg

instance Applicative m => Applicative (LoggT m) where
    pure = LoggT . pure . pure
    f <*> v = LoggT $ liftA2 (<*>) (runLoggT f) (runLoggT v)

instance Monad m => Monad (LoggT m) where
    return = pure

    m >>= k  = LoggT $ do
        (Logged s1 a) <- runLoggT m
        (Logged s2 b) <- runLoggT $ k a
        return $ Logged (s1 ++ s2) b

instance MonadFail m => Fail.MonadFail (LoggT m) where
    fail :: String -> LoggT m a
    fail = LoggT . fail

instance MonadTrans LoggT where
    lift :: Monad m => m a -> LoggT m a
    lift m = LoggT $ Logged "" <$> m

logTst :: LoggT Identity Integer
logTst = do
  x <- LoggT $ Identity $ Logged "AAA" 30
  y <- return 10
  z <- LoggT $ Identity $ Logged "BBB" 2
  return $ x + y + z

failTst :: [Integer] -> LoggT [] Integer
failTst xs = do
  5 <- LoggT $ fmap (Logged "") xs
  LoggT [Logged "A" ()]
  return 42

-- GHCi> runIdentity (runLoggT logTst)
-- Logged "AAABBB" 42
-- GHCi> runLoggT $ failTst [5,5]
-- [Logged "A" 42,Logged "A" 42]
-- GHCi> runLoggT $ failTst [5,6]
-- [Logged "A" 42]
-- GHCi> runLoggT $ failTst [7,6]
-- []
