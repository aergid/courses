import Data.List
import Control.Monad.Trans.Except
import Data.Time.Clock
import Data.Time.Format

import Prelude hiding (lookup)
import qualified Data.List as L


main' :: IO ()
main' = meet'n'greet

meet'n'greet :: IO ()
meet'n'greet = do
    putStr "What is your name?"
    name <- getLine
    putStrLn $ "Name: " ++ name
    if (name == "") then meet'n'greet else putStrLn $ "Hi, " ++ name ++ "!"

type User = String
type Password = String
type UsersTable = [(User, Password)]

--GHCi> runReader usersWithBadPasswords [("user", "123456"), ("x", "hi"), ("root", "123456")]
--["user","root"]




groupElems :: Eq a => [a] -> [[a]]
groupElems [] = []
groupElems (x : xs) = case groupElems xs of
                        [] -> [[x]]
                        (g : gs) | x == head g -> (x : g) : gs
                                 | otherwise   -> [x] : g : gs

perms :: [a] -> [[a]]
perms [] = [[]]
perms [x] = [[x]]
perms (x:xs) = concatMap (insertAtAllPositions x) $ perms xs where
    insertAtAllPositions :: t -> [t] -> [[t]]
    insertAtAllPositions x [] = [[x]]
    insertAtAllPositions x ys@(y:yy) = (x:ys) : map (y:) (insertAtAllPositions x yy)

perms2 :: [a] -> [[a]]
perms2 []  = [[]]
perms2 x   = concatMap (\n -> map ((x !! n) :) (perms2 (take n x ++ drop (n + 1) x))) [0..(length x - 1)]

revRange :: (Char,Char) -> [Char]
revRange (a, b) = unfoldr g b
  where g x = if x < a then Nothing else Just (x, pred x)

timeToString :: UTCTime -> String
timeToString = formatTime defaultTimeLocale "%a %d %T"

data LogLevel = Error | Warning | Info deriving Show

data LogEntry = LogEntry {
        timestamp  :: UTCTime,
        logLevel :: LogLevel,
        message :: String
    }

logLevelToString :: LogLevel -> String
logLevelToString = show

logEntryToString :: LogEntry -> String
logEntryToString = do
    time <- timeToString . timestamp
    lvl <- logLevelToString . logLevel
    msg <- message
    return $ "<" ++ time ++ ">: <" ++ lvl ++ ">: <" ++ msg ++ ">"

class MapLike m where
    empty :: m k v
    lookup :: Ord k => k -> m k v -> Maybe v
    insert :: Ord k => k -> v -> m k v -> m k v
    delete :: Ord k => k -> m k v -> m k v
    fromList :: Ord k => [(k,v)] -> m k v
    -- fromList [] = empty
    -- fromList ((k,v):xs) = insert k v (fromList xs)

newtype ListMap k v = ListMap { getListMap :: [(k,v)] }
    deriving (Eq,Show)

instance MapLike ListMap where
    -- empty :: ListMap k v
    empty = ListMap []

    -- lookup :: Ord k => k -> ListMap k v -> Maybe v
    lookup k (ListMap m) = L.find (\(mk, mv) -> mk == k) m >>= Just . snd

    -- insert :: Ord k => k -> v -> ListMap k v -> ListMap k v
    insert k v (ListMap m) = ListMap $ left ++ (k, v) : right' where
        (left, right) = span (\(mk, mv) -> mk /= k) m
        right' = case right of
            [] -> []
            x:xs -> xs

    -- delete :: Ord k => k -> ListMap k v -> ListMap k v
    delete k (ListMap m) = ListMap $ filter (\(mk, mv) -> mk /= k) m

    -- fromList :: Ord k => [(k, v)] -> ListMap k v
    fromList = ListMap


-- newtype ArrowMap k v = ArrowMap { getArrowMap :: k -> Maybe v }
-- instance MapLike ArrowMap where
--     empty = ArrowMap $ const Nothing

--     lookup k am = getArrowMap am $ k

--     insert k v am = ArrowMap f' where
--         f' k' | k' == k = Just v
--               | otherwise = getArrowMap am $ k'

--     delete k am = ArrowMap f' where
--         f' k' | k' == k =  Nothing
--               | otherwise = getArrowMap am $ k'

--     fromList = foldl (\am (k, v) -> insert k v am) empty


-- numberTree :: Tree () -> Tree Integer
-- numberTree tree = evalState (number tree) 1
--   where
--     number :: Tree () -> State Integer (Tree Integer)
--     number (Leaf ()) = get >>= \i -> modify (+1) >> return (Leaf i)
--     number (Fork l () r) = do
--       la <- number l
--       i <- get
--       modify (+1)
--       ra <- number r
--       return $ Fork la i ra

-- numberTree :: Tree () -> Tree Integer
-- numberTree tree = evalState (helper tree) 0 where
--     helper (Leaf _)            = liftM Leaf step
--     helper (Fork left _ right) = liftM3 Fork (helper left) step (helper right)
--     step = modify succ >> get

newtype Cont r a = Cont { runCont :: (a -> r) -> r }

-- instance Monad (Cont r) where
--     return a = Cont ($ a)
--     m >>= k  = Cont $ \c -> runCont m $ \a -> runCont (k a) c

-- ((a -> r) -> r) -> (a -> (b -> r) -> r) ->  (b -> r) -> r

type Checkpointed a = (a -> Cont a a) -> Cont a a


-- addTens :: Int -> Checkpointed Int
-- addTens :: a -> (a -> m a) -> ma

-- addTens :: Int -> (Int -> Cont Int Int) -> Cont Int Int

-- addTens x1 = \checkpoint -> do
--   checkpoint x1
--   let x2 = x1 + 10
--   checkpoint x2     {- x2 = x1 + 10 -}
--   let x3 = x2 + 10
--   checkpoint x3     {- x3 = x1 + 20 -}
--   let x4 = x3 + 10
--   return x4         {- x4 = x1 + 30 -}

-- runCheckpointed :: (a -> Bool) -> Checkpointed a -> a
runCheckpointed :: (a -> Bool) -> ((a -> Cont a a) -> Cont a a) -> a
-- runCheckpointed (< 100) $ addTens 1
-- runCheckpointed p kc = runCont (checker >>= kc) id where
-- m >>= k  = Cont $ \c -> runCont m $ \a -> runCont (k a) c
runCheckpointed p kc = runCont (kc . check $ p) id where
    check :: (a -> Bool) -> (a -> Cont a a)
    check pr = \a -> Cont $ \c -> let
        future = c a
        condition = pr future
        in if condition then c a else a

-- Если в реальной жизни напишите без "let .. in", будет очень плохо, потому что продолжение будет вызвано 2 раза.

-- ﻿Год использую CPS в продакшене

-- -- type Cont r a = { runCont :: (a -> r) -> r }
-- type Checkpointed a = (a -> Cont a a) -> Cont a a

-- runCheckpointed :: (a -> Bool) -> Checkpointed a -> a
-- runCheckpointed p checkpoint = runCont (checkpoint $ \x -> Cont $ \k -> let y = k x in if p y then y else x) id

newtype FailCont r e a = FailCont { runFailCont :: (a -> r) -> (e -> r) -> r }

toFailCont :: Except e a -> FailCont r e a
toFailCont exec = case (runExcept exec) of
    (Left e) -> FailCont $ \ok -> \bad -> bad e
    (Right a) -> FailCont $ \ok -> \bad -> ok a


evalFailCont :: FailCont (Either e a) e a -> Either e a
evalFailCont m = runFailCont m Right Left

instance Functor (FailCont r e) where
    fmap f m = FailCont $ \ok -> \bad -> runFailCont m (ok . f) bad

instance Applicative (FailCont r e) where
    pure a = FailCont $ \ok -> \bad -> ok a

-- f(a -> b) -> f a -> f b
-- ((a -> b) -> r) -> (e -> r) -> r
-- (a -> r) -> (e -> r) -> r
-- (b -> r) -> (e -> r) -> r
-- m >>= k  = Cont $ \c -> runCont m $ \a -> runCont (k a) c
-- Cont cf <*> Cont cx = Cont $ \k -> cf (\fn -> cx (\x -> k (fn x)))
    (<*>) cf cv =  FailCont $ \ok -> \bad -> let
        innerOk = \f -> runFailCont cv (ok . f) bad
        innerBad = \e -> bad e
        in runFailCont cf innerOk innerBad

instance Monad (FailCont r e) where
    return = pure

    m >>= k = FailCont $ \ok -> \bad -> let
        innerOk = \a -> runFailCont (k a) ok bad
        innerBad = \e -> bad e
            in runFailCont m innerOk innerBad


callCC :: ((a -> Cont r b) -> Cont r a) -> Cont r a
callCC f = Cont $ \h -> runCont (f (\a -> Cont $ \_ -> h a)) h


-- 1. Как вы помните, если мы, при вычислении в монаде Cont, в какой-то момент игнорируем продолжение, то дальнейшие вычисления прерываются, и возвращаемым значением является то, что мы напишем. Однако из-за этого возвращаемое значение перестаёт быть полиморфным. Если бы мы могли применить к нашему значению функцию-продолжение - тогда возвращаемое значение осталось бы полиморфным, т.к. зависело бы от её сигнатуры. Но в этом случае у нас бы продолжились вычисления в монаде. Получается тупик: чтобы сохранить полиморфность возвращаемого значения при остановке вычислений, нам нужно применить к нему продолжение, но тогда наши вычисления не останавливаются.

-- 2. Для того, чтобы выйти из этого тупика, мы "завели" в нашу основную монаду Cont дополнительную монаду Cont. У основной монады тип продолжения (a -> r) -> r, а у дополнительной - тип продолжения (b -> r) -> r.

--   2a. Когда нам не нужно прерывать вычисления, мы просто игнорируем дополнительную монаду и все вычисления проходят так, будто бы её не существует.

--   2b. Но когда нам нужно прервать вычисления, мы "заходим" в дополнительную монаду, игнорируя при этом её продолжение, а к возвращаемому значению применяем функцию-продолжение основной монады. Поскольку всё это мы делаем в дополнительной монаде, то, игнорируя продолжение этой монады, мы прерываем вычисления. Но так как мы при этом мы применяем к возвращаемому значению продолжение основной монады, то оно остаётся полиморфным.

-- Таким образом, при помощи функции callCC, мы можем и прерывать вычисления, и сохранять полиморфность возвращаемого значения монады Cont.

callCFC :: ((a -> FailCont r e b) -> FailCont r e a) -> FailCont r e a
callCFC k = FailCont $ \ca -> \ce -> runFailCont (k (\a -> FailCont $ \_ _ -> (ca a))) ca ce




