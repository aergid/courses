{-# LANGUAGE InstanceSigs #-}

module Except where

import Control.Applicative (liftA2)


newtype Except e a = Except { runExcept :: Either e a }

newtype ExceptT e m a = ExceptT { runExceptT :: m (Either e a) }

except :: Monad m  => Either e a -> ExceptT e m a
except = ExceptT . return

instance Functor (Except r) where
    fmap f ex = Except $ f <$> runExcept ex

instance Applicative (Except r) where
    pure = Except . Right
    f <*> v = Except $ runExcept f <*> runExcept v

instance Monad (Except r) where
    return = pure
    ma >>= k = Except $ runExcept ma >>= \a -> runExcept $ k a

instance Functor m => Functor (ExceptT r m) where
    fmap f ex = ExceptT $ fmap f <$> runExceptT ex

-- instance Applicative m => Applicative (ExceptT r m) where
--     pure = ExceptT . pure . Right
--     f <*> v = ExceptT $ liftA2 (<*>) (runExceptT f) (runExceptT v)

instance Monad m => Applicative (ExceptT r m) where
    pure = ExceptT . pure . Right

    ExceptT mef <*> ExceptT mea = ExceptT $ do
        ef <- mef
        case ef of
            (Left e) -> return (Left e)
            (Right f) -> fmap f <$> mea

instance Monad m => Monad (ExceptT r m) where
    return = pure
    ma >>= k = ExceptT $ do
        ea <- runExceptT ma
        case ea of
            (Left e) -> return $ Left e
            (Right a) -> runExceptT $ k a

    -- fail = ExceptT . fail

class MonadTrans t where
    lift :: Monad m => m a -> t m a

instance MonadTrans (ExceptT e) where
    lift :: Monad m => m a -> ExceptT e m a
    lift m = ExceptT $ Right <$> m

throwE :: Monad m => e -> ExceptT e m a
throwE = ExceptT . return . Left

catchE :: Monad m => ExceptT e m a -> (e -> ExceptT e1 m a) -> ExceptT e1 m a
m `catchE` h = ExceptT $ do
    a <- runExceptT m
    case a of
        (Left e) -> runExceptT (h e)
        (Right r) -> return $ Right r

data Tile = Floor | Chasm | Snake
  deriving Show

data DeathReason = Fallen | Poisoned
  deriving (Eq, Show)

type Point = (Integer, Integer)
type GameMap = Point -> Tile


nextx (x, y) = (,) (succ x) y
prevx (x, y) = (,) (pred x) y
nexty (x, y) = (,) x (succ y)
prevy (x, y) = (,) x (pred y)


go :: GameMap -> Point -> Either DeathReason Point
go gm p = case gm p of
        Floor -> Right p
        Chasm -> Left Fallen
        Snake -> Left Poisoned


move ::  GameMap -> Point -> ExceptT DeathReason [] Point
move gm pt = do
    current <- except $ go gm pt
    moveFrom <- lift [nextx, nexty, prevx, prevy]
    except $ go gm $ moveFrom current


moves :: GameMap -> Int -> Point -> [Either DeathReason Point]
moves gm n start = runExceptT $ foldl (\m _ -> m >>= move gm) (return start) $ replicate n ()

waysToDie :: DeathReason -> GameMap -> Int -> Point -> Int
waysToDie reason gm n start = length $ filter sameReason $ moves gm n start where
    sameReason st = case st of
        (Left e) -> e == reason
        _ -> False

map1 :: GameMap
map1 (2, 2) = Snake
map1 (4, 1) = Snake
map1 (x, y)
  | 0 < x && x < 5 && 0 < y && y < 5 = Floor
  | otherwise                        = Chasm

--  | 0 1 2 3 4 5
-- --------------
-- 0| o o o o o o
-- 1| o       s o
-- 2| o   s     o
-- 3| o         o
-- 4| o         o
-- 5| o o o o o o


-- moves :: GameMap -> Int -> Point -> [Either DeathReason Point]
-- moves gmap n p = runExceptT (repeatM (move1 gmap) n p)

-- waysToDie :: DeathReason -> GameMap -> Int -> Point -> Int
-- waysToDie reason gmap n p = length $ filter (== Left reason) (moves gmap n p)

-- move1 :: GameMap -> Point -> ExceptT DeathReason [] Point
-- move1 gmap (x,y) = ExceptT $ fmap f [(x-1,y),(x,y-1),(x+1,y),(x,y+1)] where
--     f p1 = case gmap p1 of
--         Snake -> Left Poisoned
--         Chasm -> Left Fallen
--         Floor -> Right p1

-- repeatM :: (Monad m) => (a -> m a) -> Int -> a -> m a
-- repeatM f n a0 = foldM (const . f) a0 [1..n]


-- import Control.Monad.Trans.Class

-- moves :: GameMap -> Int -> Point -> [Either DeathReason Point]
-- moves m n p = runExceptT $ move n p
--   where
--     move 0 p = pure p
--     move i (x, y) = do
--       p' <- lift $ do
--         dx <- [-1, 0, 1]
--         dy <- [-1, 0, 1]
--         guard $ abs dx /= abs dy
--         pure (x+dx, y+dy)
--       case m p' of
--         Snake -> throwE Poisoned
--         Chasm -> throwE Fallen
--         Floor -> move (i-1) p'

-- instance MonadTrans (ExceptT e) where
--   lift m = ExceptT $ do
--     a <- m
--     pure $ pure a

-- throwE :: Monad m => e -> ExceptT e m a
-- throwE = ExceptT . pure . Left

-- waysToDie :: DeathReason -> GameMap -> Int -> Point -> Int
-- waysToDie r m i p = length . filter (== Left r) $ moves m i p
