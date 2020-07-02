import Data.Functor
import Data.Char
import Data.List

data Point3D a = Point3D a a a deriving Show

data GeomPrimitive a = Point (Point3D a) | LineSegment (Point3D a) (Point3D a)

instance Functor Point3D where
    fmap f (Point3D x y z) = Point3D (f x) (f y) (f z)

instance Functor GeomPrimitive where
    fmap f (Point pt) = Point $ fmap f pt
    fmap f (LineSegment pt1 pt2) = LineSegment (fmap f pt1) (fmap f pt2)


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


data Log a = Log [String] a

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

execLoggersList :: a -> [a -> Log a] -> Log a
execLoggersList a = foldl (>>=) (return a)

data SomeType a = List[a]

instance Functor SomeType where
    fmap f x = x >>= f' where
        f' = return . f

data Token = Number Int | Plus | Minus | LeftBrace | RightBrace    
    deriving (Eq, Show)

asToken :: String -> Maybe Token
asToken str | str == "+" = Just Plus
            | str == "-" = Just Minus
            | str == "(" = Just LeftBrace
            | str == ")" = Just RightBrace
            | all isDigit str = Just . Number $ read str
            | otherwise = Nothing

tokenize :: String -> Maybe [Token]
tokenize input = foldl accumulate (Just []) lst where
    lst = fmap asToken $ words input
    accumulate (Just lst) (Just token) = Just $ lst ++ [token]
    accumulate _ _ = Nothing


data Board = Id
nextPositions :: Board -> [Board]
nextPositions = undefined

nextPositionsN :: Board -> Int -> (Board -> Bool) -> [Board]
nextPositionsN b 0 pred = if (pred b) then [b] else []
nextPositionsN b n pred | n < 0 = []

nextPositionsN b n pred = nextPositions b >>= \pos -> nextPositionsN pos (n-1) pred


pythagoreanTriple :: Int -> [(Int, Int, Int)]
pythagoreanTriple x = do
 a <- [1..x]
 b <- [a+1..x]
 c <- [b+1..x]
 True <- return $ a^2 + b^2 == c^2
 return (a,b,c)


meet'n'greet :: IO ()
meet'n'greet = do
    putStrLn "What is your name?"
    putStr "Name: "
    name <- getLine
    if (name == "") then meet'n'greet else putStrLn $ "Hi, " ++ name ++ "!"

fsclean :: IO ()
fsclean = do
    putStr "Substring: "
    pat <- getLine
    files <- getDirectoryContents "."
    if (pat == "")
    then
        putStrLn "Canceled"
    else
        mapM_ (\f -> putStrLn ("Removing file: " ++ f)  >> removeFile f) $ filter (pat `isInfixOf`) files


local' :: (r -> r') -> Reader r' a -> Reader r a
local' f m = Reader $ \r -> runReader m (f r)


type User = String
type Password = String
type UsersTable = [(User, Password)]

usersWithBadPasswords :: Reader UsersTable [User]
usersWithBadPasswords = do
    bad <- asks $ filter ((== "123456") . snd)
    return $ map fst bad


evalWriter :: Writer w a -> a
evalWriter = fst . runWriter


type Shopping = Writer (Sum Integer) ()

shopping1 :: Shopping
shopping1 = do
  purchase "Jeans"   19200
  purchase "Water"     180
  purchase "Lettuce"   328

purchase :: String -> Integer -> Shopping
purchase _ = tell . Sum

total :: Shopping -> Integer
total = getSum . execWriter

purchase' :: String -> Integer -> Shopping
purchase' item cost = tell ([item], Sum cost)

total' :: Shopping -> Integer
total' = getSum .snd . execWriter

items' :: Shopping -> [String]
items' = fst . execWriter


readerToState :: Reader r a -> State r a
readerToState m = State $ \s -> (runReader m s, s)

writerToState :: Monoid w => Writer w a -> State w a
writerToState m = State $ \st ->
     let (a, s') = runWriter m
         st' = st `mappend` s'
     in (a, st')

fibStep :: State (Integer, Integer) ()
fibStep = do
    (a, b) <- get
    put (b, a+b)

execStateN :: Int -> State s a -> s -> s
execStateN n m = execState (replicateM n m)


data Tree a = Leaf a | Fork (Tree a) a (Tree a)

numberTree :: Tree () -> Tree Integer
numberTree tree = evalState (traverseInOrder tree) 1

traverseInOrder :: Tree a -> State Integer (Tree Integer)
traverseInOrder (Leaf _) = do
    n <- get
    put (n + 1)
    return $ Leaf n

traverseInOrder (Fork left _ right ) = do
    left' <- traverseInOrder left
    n <- get
    put (n + 1)
    let v = n
    right' <- traverseInOrder right
    return $ Fork left' v right'
