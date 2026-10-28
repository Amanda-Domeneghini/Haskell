module Main where

-- COMO TESTAR
--
-- Modo normal (GHCi):
--   ghci trabalho.hs
--   getMem 251 (simular prog1)    -- 13
--   getMem 251 (simular prog2)    -- 12
--   getMem 251 (simular prog3)    -- 11
--   getMem 240 (simular prog3)    -- 5
--   tela (simular prog1)          -- mostra a memoria de video (251 a 255)

-- Modo interativo (menu):
--   runghc trabalho.hs
--   (ou: ghc trabalho.hs  e depois  ./trabalho)
--   opcoes 1, 2 e 3: executam os programas de teste
--   opcao 4: le a memoria de um arquivo texto (ex.: memoria.txt)
--   opcao 0: sai

-- Modo interativo (menu):
--   dentro do GHCi:   main
--   pelo terminal:    runghc trabalho.hs
--   compilado:        ghc trabalho.hs  e depois  .\trabalho.exe
--   opcoes 1, 2 e 3: executam os programas de teste
--   opcao 4: le a memoria de um arquivo texto (ex.: memoria.txt)
--   opcao 0: sai

import System.IO (hFlush, stdout)

-- 1. TIPOS DE DADOS

type Endereco = Int
type Valor    = Int
type Memoria  = [(Endereco, Valor)]

data CPU = CPU
  { pc      :: Int
  , acc     :: Int
  , eqz     :: Bool
  , mem     :: Memoria
  , rodando :: Bool
  } deriving Show

-- 2. MEMORIA

getMem :: Endereco -> Memoria -> Valor
getMem _ [] = 0
getMem addr ((a, v):xs)
  | a == addr = v
  | otherwise = getMem addr xs

setMem :: Endereco -> Valor -> Memoria -> Memoria
setMem addr val [] = [(addr, val)]
setMem addr val ((a, v):xs)
  | a == addr = (addr, val) : xs
  | otherwise = (a, v) : setMem addr val xs

truncar :: Int -> Int
truncar x = x `mod` 256

-- 3. CICLO DE EXECUCAO

cpuInicial :: Memoria -> CPU
cpuInicial m = CPU { pc = 0, acc = 0, eqz = True, mem = m, rodando = True }

escreveACC :: Int -> CPU -> CPU
escreveACC v c = c { acc = v', eqz = v' == 0 }
  where v' = truncar v

avanca :: CPU -> CPU
avanca c = c { pc = truncar (pc c + 2) }

passo :: CPU -> CPU
passo c
  | not (rodando c) = c
  | otherwise =
      case opcode of
        2  -> avanca (escreveACC (getMem arg m) c)
        4  -> avanca c { mem = setMem arg (acc c) m }
        6  -> c { pc = truncar arg }
        8  -> if eqz c then c { pc = truncar arg } else avanca c
        10 -> avanca (escreveACC (acc c + getMem arg m) c)
        12 -> avanca (escreveACC (acc c - getMem arg m) c)
        14 -> avanca c
        _  -> c { rodando = False }
  where
    m      = mem c
    opcode = getMem (pc c) m
    arg    = getMem (pc c + 1) m

executar :: CPU -> CPU
executar c
  | rodando c = executar (passo c)
  | otherwise = c

simular :: Memoria -> Memoria
simular m = mem (executar (cpuInicial m))

tela :: Memoria -> [(Endereco, Valor)]
tela m = [ (a, getMem a m) | a <- [251 .. 255] ]

-- 4. PROGRAMAS DE TESTE

prog1 :: Memoria
prog1 =
  [ (0, 2),  (1, 240)
  , (2, 10), (3, 241)
  , (4, 12), (5, 245)
  , (6, 4),  (7, 251)
  , (8, 20), (9, 0)
  , (240, 10)
  , (241, 5)
  , (245, 2)
  ]
--Resp = A + B - 2;
--getMem 251 (simular prog1) == 13

prog2 :: Memoria
prog2 =
  [ (0, 2),   (1, 245)
  , (2, 4),   (3, 251)
  , (4, 2),   (5, 241)
  , (6, 4),   (7, 250)
  , (8, 2),   (9, 250)
  , (10, 8),  (11, 26)
  , (12, 2),  (13, 251)
  , (14, 10), (15, 240)
  , (16, 4),  (17, 251)
  , (18, 2),  (19, 250)
  , (20, 12), (21, 246)
  , (22, 4),  (23, 250)
  , (24, 6),  (25, 8)
  , (26, 20), (27, 0)
  , (240, 4)
  , (241, 3)
  , (245, 0)
  , (246, 1)
  ]
--Resp = A * B
--getMem 251 (simular prog2) == 12

prog3 :: Memoria
prog3 =
  [ (0, 2),   (1, 245)
  , (2, 4),   (3, 240)
  , (4, 2),   (5, 246)
  , (6, 4),   (7, 251)
  , (8, 2),   (9, 247)
  , (10, 12), (11, 240)
  , (12, 8),  (13, 28)
  , (14, 2),  (15, 240)
  , (16, 10), (17, 246)
  , (18, 4),  (19, 240)
  , (20, 2),  (21, 251)
  , (22, 10), (23, 248)
  , (24, 4),  (25, 251)
  , (26, 6),  (27, 8)
  , (28, 20), (29, 0)
  , (245, 0)
  , (246, 1)
  , (247, 5)
  , (248, 2)
  ]
--A = 0; Resp = 1; while(A < 5) { A = A + 1; Resp = Resp + 2; }
--getMem 251 (simular prog3) == 11
--getMem 240 (simular prog3) == 5

-- 5. PROGRAMA INTERATIVO

executa :: Memoria -> IO ()
executa m = do
  let final = simular m
  putStrLn "\nMemoria final:"
  print final
  putStrLn "\nTela (251 a 255):"
  mapM_ (\(a, v) -> putStrLn ("  [" ++ show a ++ "] = " ++ show v)) (tela final)

main :: IO ()
main = do
  putStrLn "=== Simulador de computador hipotetico ==="
  putStrLn "1 - Programa 1"
  putStrLn "2 - Programa 2"
  putStrLn "3 - Programa 3"
  putStrLn "4 - Ler memoria de arquivo"
  putStrLn "0 - Sair"
  putStr "Opcao: "
  hFlush stdout
  op <- getLine
  case op of
    "1" -> executa prog1 >> main
    "2" -> executa prog2 >> main
    "3" -> executa prog3 >> main
    "4" -> do
      putStr "Nome do arquivo: "
      hFlush stdout
      nome <- getLine
      s <- readFile nome
      case reads s :: [(Memoria, String)] of
        [(m, _)] -> executa m >> main
        _        -> putStrLn "Arquivo invalido." >> main
    "0" -> putStrLn "Ate logo."
    _   -> putStrLn "Opcao invalida." >> main