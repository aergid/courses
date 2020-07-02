import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer
import Control.Monad.Trans.State
import Control.Monad.Trans.Except
import Control.Monad.Trans
import Control.Monad
import Data.Char
import Data.List

logFirstAndRetSecond :: WriterT String (Reader [String]) String
logFirstAndRetSecond = do
    first <- lift $ asks head
    second <- lift $ asks (map toUpper . head . tail)
    tell first
    return second


-- Реализуйте функцию separate :: (a -> Bool) -> (a -> Bool) -> [a] -> WriterT [a] (Writer [a]) [a].

-- Эта функция принимает два предиката и список и записывает в один лог элементы списка, удовлетворяющие первому предикату, в другой лог — второму предикату, а возвращающает список элементов, ни одному из них не удовлетворяющих.

-- GHCi> (runWriter . runWriterT) $ separate (<3) (>7) [0..10]
-- (([3,4,5,6,7],[0,1,2]),[8,9,10])

separate :: (a -> Bool) -> (a -> Bool) -> [a] -> WriterT [a] (Writer [a]) [a]
separate pi po xs = do
    let inner = filter pi xs
    let outer = filter po xs
    let res = filter (\x -> not (pi x) && not (po x)) xs
    lift $ tell outer
    tell inner
    return res

-- separate :: (a -> Bool) -> (a -> Bool) -> [a] -> WriterT [a] (Writer [a]) [a]
-- separate pred1 pred2 = filterM $ \x -> do
--     when (pred1 x) $        tell [x]
--     when (pred2 x) $ lift $ tell [x]
--     return $ not (pred1 x) && not (pred2 x)

-- separate :: (a -> Bool) -> (a -> Bool) -> [a] -> WriterT [a] (Writer [a]) [a]
-- separate p1 p2 xs = do
--     tell        (filter p1 xs)
--     lift $ tell (filter p2 xs)
--     return      (filter (not . liftM2 (||) p1 p2) xs)

-- Превратите монаду MyRW в трансформер монад MyRWT:


-- GHCi> runMyRWT logFirstAndRetSecond ["abc","defg","hij"]
-- First is "abc"
-- Second is "DEFG"
-- ("DEFG","abc")

type MyRWT m a = WriterT String (ReaderT [String] m) a

runMyRWT :: MyRWT m a -> [String] -> m (a, String)
runMyRWT m = runReaderT (runWriterT m)

myAsks :: (Monad m) => ([String] -> a) -> MyRWT m a
myAsks = lift . asks

myTell :: Monad m => String -> MyRWT m ()
myTell = tell

myLift :: Monad m => m a -> MyRWT m a
myLift = lift . lift

logFirstAndRetSecond2 :: MyRWT IO String
logFirstAndRetSecond2 = do
  el1 <- myAsks head
  myLift $ putStrLn $ "First is " ++ show el1
  el2 <- myAsks (map toUpper . head . tail)
  myLift $ putStrLn $ "Second is " ++ show el2
  myTell el1
  return el2


-- GHCi> runMyRWT veryComplexComputation ["abc","defg","hij"]
-- Nothing
-- GHCi> runMyRWT veryComplexComputation ["abc","defg","hij","kl"]
-- Just (("KL","HIJ"),"defg,abc")

veryComplexComputation :: MyRWT Maybe (String, String)
veryComplexComputation = do
    (odds, evens) <- myAsks $ partition (odd . length)
    case (odds, evens) of
        (o:o1:_, e:e1:_) -> do
            myTell $ e ++ "," ++ o
            myLift $ Just (map toUpper e1, map toUpper o1)

        _ -> myLift Nothing

-- Так как мы в монаде Maybe, мы можем смело сиспользовать фейлящиеся вычисления и получать в результате Nothing.

-- veryComplexComputation :: MyRWT Maybe (String, String)
-- veryComplexComputation = do
--   (e1:e2:_) <- myAsks $ take 2 . filter (even . length)
--   (o1:o2:_) <- myAsks $ take 2 . filter (odd . length)
--   myTell (e1 ++ "," ++ o1) >> pure (map toUpper e2, map toUpper o2)

-- на библиотечных функциях




-- veryComplexComputation :: MyRWT Maybe (String, String)
-- veryComplexComputation = do
--     a1 <- local (filter $ even . length) logFirstAndRetSecond
--     lift $ tell ","
--     a2 <- local (filter $ odd . length) logFirstAndRetSecond
--     return (a1, a2)

tickCollatz :: State Integer Integer
tickCollatz = do
  n <- get
  let res = if odd n then 3 * n + 1 else n `div` 2
  put res
  return n


type EsSi = ExceptT String (State Integer)

-- GHCi> runEsSi (go 1 85 tickCollatz) 27
-- (Right (),82)
-- GHCi> runEsSi (go 1 80 tickCollatz) 27
-- (Left "Upper bound",82)
-- GHCi> runEsSi (forever $ go 1 1000 tickCollatz) 27
-- (Left "Upper bound",1186)
-- GHCi> runEsSi (forever $ go 1 10000 tickCollatz) 27
-- (Left "Lower bound",1)

runEsSi :: EsSi a -> Integer -> (Either String a, Integer)
runEsSi es = runState (runExceptT es)

go :: Integer -> Integer -> State Integer Integer -> EsSi ()
go lower upper ms = do
    s <- lift get
    let (n, s') = runState ms s
    lift $ put s'
    when (s' <= lower) $ throwE "Lower bound"
    when (s' >= upper) $ throwE "Upper bound"

-- go lower upper next = do
--   lift next
--   n <- lift get


type RiiEsSiT m = ReaderT (Integer,Integer) (ExceptT String (StateT Integer m))

-- GHCi> runRiiEsSiT (forever $ go tickCollatz') (1,200) 27
-- 82
-- 41
-- 124
-- 62
-- 31
-- 94
-- 47
-- 142
-- 71
-- 214
-- (Left "Upper bound",214)

runRiiEsSiT :: ReaderT (Integer,Integer) (ExceptT String (StateT Integer m)) a
                -> (Integer,Integer)
                -> Integer
                -> m (Either String a, Integer)

runRiiEsSiT mr bounds = runStateT . runExceptT $ runReaderT mr bounds

go' :: Monad m => StateT Integer m Integer -> RiiEsSiT m ()
go' ms = do
    lift . lift $ ms
    (lower, upper) <- ask
    n <- lift . lift $ get
    when (n <= lower) $ lift . throwE $ "Lower bound"
    when (n >= upper) $ lift . throwE $ "Upper bound"


tickCollatz' :: StateT Integer IO Integer
tickCollatz' = do
  n <- get
  let res = if odd n then 3 * n + 1 else n `div` 2
  lift $ putStrLn $ show res
  put res
  return n
