import Data.Char
import Data.Function

main = putStrLn "Hello, world!"

seqA :: Integer -> Integer
seqA n = let
        seqHelp a b c n | n == 0 = a
                        | n == 1 = b
                        | n == 2 = c
                        | otherwise = seqHelp b c (b + c - 2 * a) $ n - 1
        in seqHelp 1 2 3 n


sum'n'count :: Integer -> (Integer, Integer)
sum'n'count n = (s,c) where
    absolute = abs n
    literal = show absolute
    c = toInteger $ length literal
    s = toInteger $ sum $ map digitToInt literal


-- import Data.List (foldl')

integration :: (Double -> Double) -> Double -> Double -> Double
integration f a b
    | a <= b = integration' f a b
    | otherwise = -integration' f b a

integration' f a b = h * ((f a + f b)/2 + intervalSum) where
    n = 1000
    h = (b - a) / n
    interval = [a + h, a + 2 *h.. b -h]
    intervalSum = foldl (\acc x -> acc + f x) 0 interval


on3 :: (b -> b -> b -> c) -> (a -> b) -> a -> a -> a -> c
on3 op f x y z = op (f x) (f y) (f z)

class KnownToGork a where
    stomp :: a -> a
    doesEnrageGork :: a -> Bool

class KnownToMork a where
    stab :: a -> a
    doesEnrageMork :: a -> Bool

class (KnownToGork a, KnownToMork a) => KnownToGorkAndMork a where
    stompOrStab :: a -> a
    stompOrStab a
        | doesEnrageGork a && doesEnrageMork a = stomp . stab $ a
        | doesEnrageGork a = stab a
        | doesEnrageMork a = stomp a
        | otherwise = a

data Tree a = Leaf (Maybe a) | Branch (Tree a) (Maybe a) (Tree a) deriving Show

instance Functor Tree where
    fmap f (Leaf x) = Leaf $ f <$> x
    fmap f (Branch left root right) = Branch (f <$> left) (f <$> root) (f <$> right)

data Entry k1 k2 v = Entry (k1, k2) v  deriving Show
data Map k1 k2 v = Map [Entry k1 k2 v]  deriving Show


instance Functor (Entry k1 k2) where
    fmap f (Entry kp v) = Entry kp $ f v

instance Functor (Map k1 k2) where
    fmap f (Map es) = Map $ map (fmap f) es


data Log a = Log [String] a deriving Show
toLogger :: (a -> b) -> String -> (a -> Log b)
toLogger f msg a = Log [msg] $ f a

execLoggers :: a -> (a -> Log b) -> (b -> Log c) -> Log c
execLoggers a runLogA runLogB = Log msgs c where
    msgs = msgA ++ msgB
    Log msgA b = runLogA a
    Log msgB c = runLogB b

returnLog :: a -> Log a
returnLog = Log []

bindLog :: Log a -> (a -> Log b) -> Log b
bindLog logA runLogB = Log msgs b where
    Log msgA a = logA
    Log msgB b = runLogB a
    msgs = msgA ++ msgB

instance Functor Log where
    fmap f (Log msg a) = Log msg $ f a

instance Applicative Log where
    pure = Log []
    (<*>) (Log msg1 fa) (Log msg2 a) = Log (msg1 ++ msg2) $ fa a

instance Monad Log where
    return = returnLog
    (>>=) (Log msga a) f = Log (msga ++ msgb) c where
        Log msgb c = f a


execLoggersList :: a -> [a -> Log a] -> Log a
execLoggersList = foldl (>>=) . return

-- import Control.Monad

-- execLoggersList :: a -> [a -> Log a] -> Log a
-- execLoggersList = flip (foldl1 (>=>))
--}

data Token = Number Int | Plus | Minus | LeftBrace | RightBrace    
     deriving (Eq, Show)
-- Тип Token уже объявлен, его писать не нужно

asToken :: String -> Maybe Token
asToken str | str == "+" = Just Plus
            | str == "-" = Just Minus
            | str == "(" = Just LeftBrace
            | str == ")" = Just RightBrace
            | all isDigit str = Just . Number $ read str
            | otherwise = Nothing

tokenize :: String -> Maybe [Token]
tokenize input = foldl accumulate (Just []) lst where
    lst = asToken <$> words input
    accumulate (Just lst) (Just token) = Just $ lst ++ [token]
    accumulate _ _ = Nothing

tokenizeR :: String -> Maybe [Token]
tokenizeR input = foldr f (return []) (words input) where
    f word list = do
        token <- asToken word
        tokens <- list
        return $ token : tokens
