--Retornar o primeiro elemento da lista
--como uma função total
cabeca :: [a] -> a
cabeca [] = error "Não definido para lista vazia"
cabeca (x:_) = x

--Verificar se a lista está vazia
vazia :: [a] -> Bool
vazia [] = True
vazia (_:_) = False