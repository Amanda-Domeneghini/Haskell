--implementar uma função usando compreensão de listas 
--NOME: count
--RECEBE: uma caractere e uma String
--RETORNA: a quantidade de vezes que o caractere aparece na String
--EXEMPLO: cont a "banana" --3
--DICA: usar o lengt

count :: Char -> String -> Int
count c ss = length[ x | x <- ss, x == c]

zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys

-- FAZER 1 E 2 

-- dado um numero x se ele for iguas a soma de todos os seus divizores exeto ele mesmo, e um numero perfeito
perfect :: Int -> [Int]
perfect n = [x | x <- [1..n],
             sum [divizores | 
                  divizores <- [1 .. x-1],
                  x `mod` divizores == 0] == x]