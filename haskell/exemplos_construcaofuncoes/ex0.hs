abs' :: Int -> Int
abs' n = if n >= 0 then n else -n

signum' :: Int -> Int
signum' n = if n < 0 then -1 else if n == 0 then 0 else 1

abs'' :: Int -> Int
abs'' n | n >= 0 = n
       | otherwise = -n

signum'' :: Int -> Int
signum'' n | n < 0 = -1
           | n == 0 = 0
           | otherwise = 1

nao :: Bool -> Bool
nao False = True
nao True = False

e :: Bool -> Bool -> Bool
e True True = True
e True False = False
e False True = False
e False False = False

e' :: Bool -> Bool -> Bool
e' True True = True
e' _ _ = False

e'' :: Bool -> Bool -> Bool
e'' True b = b
e'' False _ = False

soma = \x -> (\y -> x + y)

