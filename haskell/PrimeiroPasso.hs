{-# OPTIONS_GHC -Wno-x-partial #-}

-- ============================================
-- COMO RODAR ESTE ARQUIVO
-- ============================================
-- 1. Abra o terminal na pasta onde este arquivo está salvo
-- 2. Digite: ghci PrimeiroPasso.hs
--    (isso abre o GHCi já carregando o arquivo)
-- 3. Para testar uma função, digite o nome dela, ex: factorial 5
-- 4. Se você editar e salvar o arquivo, NÃO feche o GHCi.
--    Basta digitar :reload (ou :r) para recarregar as mudanças
-- 5. Se quiser carregar o arquivo de dentro do GHCi (sem ter
--    aberto com o nome do arquivo), use :load PrimeiroPasso.hs
--    (ou :l PrimeiroPasso.hs)
-- 6. Para sair do GHCi, digite :quit (ou :q)
-- ============================================

-- Dobra o valor de x (soma x com ele mesmo)
double x = x + x

-- Quadruplica x aplicando double duas vezes
quadruple x = double (double x)

-- Calcula o fatorial de n, multiplicando todos os números de 1 até n
factorial n = product [1..n]

-- Calcula a média de uma lista de números (soma dividida pela quantidade)
average ns = sum ns `div` length ns

--add :: (int,int) -> int -- definição de uma função
--nome :: add
-- tipo de entrada: (x,y), onde sao inteiros
-- retorno : z -- inteiro

add :: (Int, Int) -> Int
add (x,y) = x + y  -- no terminal: add (2,4)


-- Calcula a divisão inteira de "a" pelo tamanho da lista xs
--corrija o erro 
--n = a `div` length xs
    --where
        --a =10
        --xs = [1,2,3,4,5]

n = a `div` length xs
    where
        a =10
        xs = [1,2,3,4,5]


--slide primeiros passos
--3 
last2 :: [Int] -> Int
last2 xs = head (reverse xs)

--4
last3 :: [Int] -> Int
last3 xs = xs !! (length xs - 1)

{-length xs: Calcula a quantidade total de elementos na lista. Se a lista for [10, 20, 30], o tamanho é 3.

- 1: Subtrai 1 do tamanho total porque os índices (posições) em Haskell começam a contar do zero. Uma lista de tamanho 3 possui os índices 0, 1 e 2.
 O índice do último elemento sempre será o tamanho total menos um.

!!: É o operador de busca por índice em Haskell. A sintaxe é sempre lista !! indice.

xs !! (length xs - 1): Pede ao Haskell para olhar a lista xs e extrair o elemento que está na exata posição calculada.-}

--5
init2 :: [Int] -> [Int]
init2 xs = reverse (tail (reverse xs))

-- outra opção

init3 :: [Int] -> [Int]
init3 xs = take (length xs - 1) xs