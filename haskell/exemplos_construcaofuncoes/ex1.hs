--Calcular o maior entre dois números inteiros
--Com expressão condicional
maior :: Integral a => a -> a -> a
maior x y = if x >= y then x else y

--Com equações guardadas
maiorv2 :: Integral a => a -> a -> a
maiorv2 x y
    | x >=y = x
    | otherwise = y
