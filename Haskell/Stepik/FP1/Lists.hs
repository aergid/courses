import Data.Char
import Data.List

addTwoElements :: a -> a -> [a] -> [a]
addTwoElements a b xs = a : b : xs

nTimes:: a -> Int -> [a]
nTimes c 0 = []
nTimes c n = c : nTimes c (n - 1)

oddsOnly :: Integral a => [a] -> [a]
oddsOnly [] = []
oddsOnly (x:xs) | odd x = x : oddsOnly xs
                | otherwise = oddsOnly xs

isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == (rev xs [])  where
    rev [] ys = ys
    rev (x:xs) ys = rev xs (x:ys)

sum3 :: Num a => [a] -> [a] -> [a] -> [a]
sum3 as bs cs = sum2 as $ sum2 bs cs where
    sum2 [] cs = cs
    sum2 bs [] = bs
    sum2 (b:bs) (c:cs) = (b + c): sum2 bs cs

groupElems :: Eq a => [a] -> [[a]]
groupElems [] = []
groupElems (x:xs) = let
    (same, remains) = takeWhileEq x xs []
    takeWhileEq x [] acc = (acc, [])
    takeWhileEq x ys@(y:xs) acc | y == x = takeWhileEq x xs $ x:acc
                             | otherwise = (acc, ys)
    in (x:same) : groupElems remains


readDigits :: String -> (String, String)
readDigits = span isDigit

filterDisj :: (a -> Bool) -> (a -> Bool) -> [a] -> [a]
filterDisj _ _ [] = []
filterDisj p1 p2 (x:xs) | p1 x || p2 x = x : filterDisj p1 p2 xs
                        | otherwise = filterDisj p1 p2 xs

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort lesser ++ (x : qsort greater) where
    (lesser, greater) = partition (< x) xs
    partition p xs = (filter p xs, filter (not . p) xs)


squares'n'cubes :: Num a => [a] -> [a]
squares'n'cubes = concatMap (\x -> [x^2, x^3])


perms :: [a] -> [[a]]
perms [] = [[]]
perms [x] = [[x]]
perms (x:xs) = concatMap (insertAtAllPositions x) $ perms xs where
    insertAtAllPositions :: t -> [t] -> [[t]]
    insertAtAllPositions x [] = [[x]]
    insertAtAllPositions x ys@(y:yy) = (x:ys) : map (y:) (insertAtAllPositions x yy)

delAllUpper :: String -> String
delAllUpper = unwords . filter notUpper . words where
    notUpper = not . all isUpper

max3 :: Ord a => [a] -> [a] -> [a] -> [a]
max3 = zipWith3 (\a b c -> a `max` b `max` c)

fibStream :: [Integer]
fibStream = 0:1: zipWith (+) fibStream (tail fibStream)


coins :: [a]
coins = undefined

change :: (Ord a, Num a) => a -> [[a]]
change 0 = [[]]
change n = [x:y | x <- filter (<= n) coins, y <- change (n - x)]


concatList :: [[a]] -> [a]
concatList = foldr (++) []

lengthList :: [a] -> Int
lengthList = foldr (\x a -> succ a) 0

sumOdd :: [Integer] -> Integer
sumOdd = foldr (\x s -> s + if odd x then x else 0) 0

meanList :: [Double] -> Double
meanList xs = let
    (s, l) = foldr (\x (as, al) -> (as + x, succ al)) (0, 0) xs
    in s / l

-- drop elements at odd positions
evenOnly :: [a] -> [a]
evenOnly xs = reverse . snd $ foldl (\(p,acc) x -> (not p, if p then x : acc else acc)) (False,[]) xs

-- infinite
evenOnly' :: [a] -> [a]
evenOnly' xs = foldr (\xy s -> if even $ snd xy then (fst xy) : s else s) [] (zip xs [1..])

lastElem :: [a] -> a
lastElem = foldl1 $ flip const

revRange :: (Char,Char) -> [Char]
revRange (a, b) = unfoldr g (a, succ b) where
  g (x, y) | y == a = Nothing
           | fromEnum x > fromEnum y = Nothing
           | otherwise = Just (pred y, (x, pred y))
