import Data.Char

doubleFact :: Integer -> Integer
doubleFact 0 = 1
doubleFact 1 = 1
doubleFact n = n * doubleFact (n - 2)

naivefib :: Integer -> Integer
naivefib 1 = 1
naivefib 0 = 0
naivefib n | n < 0 = naivefib (n + 2) - naivefib (n + 1)
            | otherwise = naivefib (n - 1) + naivefib (n - 2)


linearfib :: Integer -> Integer
linearfib n = fib 0 1 n where
    fib a b n | n == 1 = b
              | n == 0 = a
              | n > 0 = fib b (a + b) $ n-1
              | n < 0 = fib (b - a) a $ n+1

-- a0 = 0, a1 = 1, a2 = 2; a[k+3] = a[k+2] + a[k+1] - 2*a[k]
seqA :: Integer -> Integer
seqA n = let
        seqHelp a b c n | n == 0 = a
                        | n == 1 = b
                        | n == 2 = c
                        | otherwise = seqHelp b c (b + c - 2 * a) $ n - 1
        in seqHelp 1 2 3 n

-- sum and count digits base 10
sum'n'count n = (s,c) where
    absolute = abs n
    literal = show absolute
    c = toInteger $ length literal
    s = toInteger $ sum $ map digitToInt literal
