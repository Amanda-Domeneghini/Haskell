--add :: (int,int) -> int -- definição de uma função
--nome :: add
-- tipo de entrada: (x,y), onde sao inteiros
-- retorno : z -- inteiro

add :: (Int, Int) -> Int
add (x,y) = x + y  -- no terminal: add (2,4)

-- faca o calculo da fotorial de forma recursiva
--usando as sequencias n * fatorial(-1)
fatorial2 :: Int -> Int
fatorial2 0 = 1
fatorial2 n = n * fatorial2 (n - 1)

--Implemente em Haskell uma função chamada pair que receba dois valores e
--retorne uma tupla contendo esses dois valores, mantendo a mesma ordem em
--que foram recebidos.
pair :: a -> b -> (a, b)
pair x y = (x, y)

-- Implemente em Haskell uma função que receba uma lista e retorne o segundo
-- elemento dessa lista.
-- Considere que a lista recebida sempre possuirá pelo menos dois elementos.

segundo :: [a] -> a
segundo xs = head(tail xs)

-- ou

segundo2 :: [a] -> a
segundo2 xs = xs !!1

--Implemente em Haskell uma função chamada `palindromo` que receba uma lista
-- e produza uma nova lista formada pela lista original seguida dos seus elementos
-- em ordem inversa.

palindromo :: [a] -> [a]
palindromo xs = xs ++ reverse(xs)

-- ou
 -- REVER
--palindromo2 :: [a] -> [a]
--palindromo2 xs = reverse xs == xs