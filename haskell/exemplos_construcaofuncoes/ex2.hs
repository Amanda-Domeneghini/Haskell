--Calcular fatorial
--Com expressão condicional
fatorial :: Integer -> Integer
fatorial x = if x == 0 then 1 else x * fatorial (x-1)

--Com equações guardadas
fatorialv2 :: Integer -> Integer
fatorialv2 x
    | x == 0 = 1
    | otherwise = x * fatorial (x-1)

--Com casamento de padrões
fatorialv3 :: Integer -> Integer
fatorialv3 0 = 1
fatorialv3 x = x * fatorial (x-1)

--recursão múltipla
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-2) + fib (n-1)

--recursão mútua
par :: Int -> Bool
par 0 = True
par n = impar (n-1)

impar :: Int -> Bool
impar 0 = False
impar n = par (n-1)

--recursão interessante (função de Ackermann)
ack :: Integer -> Integer -> Integer
ack 0 n = n+1
ack m 0 = ack (m-1) 1
ack m n = ack (m-1) (ack m (n-1))