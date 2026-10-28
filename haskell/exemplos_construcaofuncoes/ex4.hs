somar :: Int -> Int -> Int
somar x y = x + y

somarv2 :: Int -> (Int -> Int)
somarv2 = \x -> (\y -> x + y)

