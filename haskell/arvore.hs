--qual a principal diferenca entre os tipos de dados Tree e ArvBin?
--Refaça com ArvBin as funcoes occur e occurV2

-- ocrrs 1 arv > True
-- ocrrs 4 arv > False

data ArvBin a = Vazia | Nodo (ArvBin a) a (ArvBin a) deriving Show

-- Busca em árvore qualquer: O(n)
occurs :: Eq a => a -> ArvBin a -> Bool
occurs _ Vazia        = False
occurs x (Nodo l y r) = x == y || occurs x l || occurs x r

-- Busca em ABP: O(h), onde h é a altura
occursV2 :: Ord a => a -> ArvBin a -> Bool
occursV2 _ Vazia = False
occursV2 x (Nodo l y r)
  | x == y    = True
  | x < y     = occursV2 x l
  | otherwise = occursV2 x r

-- ABP de teste
arv :: ArvBin Int
arv = Nodo (Nodo Vazia 1 Vazia) 2 (Nodo Vazia 3 Vazia)

main :: IO ()
main = do
  print (occurs   1 arv)  -- True
  print (occurs   4 arv)  -- False
  print (occursV2 1 arv)  -- True
  print (occursV2 4 arv)  -- False