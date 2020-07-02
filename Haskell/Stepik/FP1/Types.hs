import Data.Char

data Color = Red | Green | Blue

instance Show Color where
    show Red = "Red"
    show Green = "Green"
    show Blue = "Blue"

stringToColor :: String -> Color
stringToColor "Red" = Red
stringToColor "Green" = Green
stringToColor "Blue" = Blue

charToInt :: Char -> Int
charToInt c | isDigit c = read [c]


data LogLevel = Error | Warning | Info

levelToInt Info = 0
levelToInt Warning = 1
levelToInt Error = 2

cmp :: LogLevel -> LogLevel -> Ordering
cmp a b | levelToInt a > levelToInt b = GT
        | levelToInt a < levelToInt b = LT
        | otherwise = EQ

data Result = Fail | Success

doSomeWork :: a -> (Result, Int)
doSomeWork = undefined

processData :: a -> String
processData d = case doSomeWork d of
    (_, 0) -> "Success"
    (_, n) -> "Fail: " ++ show n


data Point = Point Double Double

origin :: Point
origin = Point 0.0 0.0

distanceToOrigin :: Point -> Double
distanceToOrigin (Point x y) = sqrt (x ^ 2 + y ^ 2)

distance :: Point -> Point -> Double
distance (Point x1 y1) (Point x2 y2) = sqrt $ (x1-x2)^2 + (y1-y2)^2


data Shape = Circle Double | Rectangle Double Double

area :: Shape -> Double
area (Circle r) = pi * r * r
area (Rectangle a b) = a * b


data Result' = Successed | Failure Int

instance Show Result' where
    show Successed = "Success"
    show (Failure n) = "Fail: " ++ show n

doSomeWork' :: a -> Result'
doSomeWork' d = case doSomeWork d of
    (_, 0) -> Successed
    (_, n) -> Failure n


square :: Double -> Shape
square a = Rectangle a a

isSquare :: Shape -> Bool
isSquare (Rectangle a b ) | a == b = True
                          | otherwise = False
isSquare _ = False

