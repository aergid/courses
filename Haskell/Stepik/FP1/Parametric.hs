import Data.Char(isDigit)
import Data.List


data Coord a = Coord a a

distance :: Coord Double -> Coord Double -> Double
distance (Coord x y) (Coord x' y') = sqrt $ (x - x')^2 + (y-y')^2

manhDistance :: Coord Int -> Coord Int -> Int
manhDistance (Coord x y) (Coord x' y') = abs(x - x') + abs(y -y')

findDigit :: [Char] -> Maybe Char
findDigit [] = Nothing
findDigit (x:xs) | isDigit x = Just x
                 | otherwise = findDigit xs

findDigitOrX :: [Char] -> Char
findDigitOrX xs = case findDigit xs of
    Just x -> x
    Nothing -> 'X'

maybeToList :: Maybe a -> [a]
maybeToList (Just x) = [x]
maybeToList _ = []

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:_) = Just x

eitherToMaybe :: Either a b -> Maybe a
eitherToMaybe (Left a) = Just a
eitherToMaybe (Right _) = Nothing


data List a = Nil | Cons a (List a)

fromList :: List a -> [a]
fromList Nil = []
fromList (Cons a lst) = a: fromList lst

toList :: [a] -> List a
toList = foldr Cons Nil


data Nat = Zero | Suc Nat

fromNat :: Nat -> Integer
fromNat Zero = 0
fromNat (Suc n) = fromNat n + 1

toNat :: Integer -> Nat
toNat 0 = Zero
toNat n = foldl (\a b -> Suc a) Zero $ replicate (fromInteger n) 1

add :: Nat -> Nat -> Nat
add a b = toNat $ fromNat a + fromNat b

mul :: Nat -> Nat -> Nat
mul a b = toNat $ fromNat a * fromNat b

fac :: Nat -> Nat
fac Zero = Suc Zero
fac n = toNat $ product [1..(fromNat n)]


data Tree a = Leaf a | Node (Tree a) (Tree a)

height :: Tree a -> Int
height (Leaf _) = 0
height (Node left right) = 1 + max (height left) (height right)

size :: Tree a -> Int
size (Leaf _) =  1
size (Node left right) = 1 + size left + size right

avg :: Tree Int -> Int
avg t =
    let (c,s) = go t
    in s `div` c
  where
    go :: Tree Int -> (Int,Int)
    go (Leaf a) = (1, a)
    go (Node left right) = let
        (cl, sl) = go left
        (cr, sr) = go right
        in (cl + cr, sl + sr)

newtype Xor = Xor { getXor :: Bool }
    deriving (Eq,Show)


instance Monoid Xor where
    mempty = Xor False
    mappend a b | a == b = Xor False
                | otherwise = Xor True


newtype Maybe' a = Maybe' { getMaybe :: Maybe a }
    deriving (Eq,Show)

instance Monoid a => Monoid (Maybe' a) where
    mempty = Maybe' $ Just mempty
    mappend (Maybe' Nothing) _ = Maybe' Nothing
    mappend _ (Maybe' Nothing) = Maybe' Nothing
    mappend (Maybe' (Just l)) (Maybe' (Just r)) = Maybe' . Just $ l `mappend` r
