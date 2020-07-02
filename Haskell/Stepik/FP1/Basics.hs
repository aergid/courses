import Data.Function

getSecondFrom :: t1 -> t2 -> t3 -> t2
getSecondFrom a = const

multSecond = g `on` h where
    g = (*)
    h = snd

on3 :: (b -> b -> b -> c) -> (a -> b) -> a -> a -> a -> c
on3 op f x y z = op (f x) (f y) (f z)


class Printable a where
    toString :: a -> String

instance Printable Bool where
    toString True = "true"
    toString False = "false"

instance Printable () where
    toString () = "unit type"

instance (Printable a, Printable b) => Printable (a, b) where
    toString (a,b) = "(" ++ toString a ++ "," ++ toString b ++ ")"


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

class (Enum a, Bounded a, Eq a) => SafeEnum a where
  ssucc :: a -> a
  ssucc a = if a == maxBound then minBound else succ a

  spred :: a -> a
  spred a = if a == minBound then maxBound else pred a


avg :: Int -> Int -> Int -> Double
avg x y z = (sum $ map fromIntegral [x,y,z]) / 3.0
