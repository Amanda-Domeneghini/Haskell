somar :: Int -> Int -> Int
somar x y = x + y

somav2 :: Int -> (Int -> Int)
somav2 = \x ->(\y -> x + y)

twice2 f x 

---map f (x:xs) = f x : map f xs
map (+1) [1,2,3]
map.even [1,2,3,4]
--[2,4]