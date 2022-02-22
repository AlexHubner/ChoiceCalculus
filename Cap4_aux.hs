module Cap4_aux where

import Capitulo4

--Função para definir dimensões atômicas
atomic :: Dim -> [Tag] -> [V a] -> V a
atomic d ts cs = Dim d ts $ Chc d cs

--Função que declara uma dimensão A com as tags a1 e a2
dimA :: V a -> V a
dimA = Dim "A" ["a1", "a2"]

--Função que constrói uma escolha na dimensão A
chcA :: [V a] -> V a
chcA = Chc "A"

--Função que declara uma dimensão B com as tags b1 e b2
dimB :: V a -> V a
dimB = Dim "B" ["b1", "b2"]

--Função que constrói uma escolha na dimensão B
chcB :: [V a] -> V a
chcB = Chc "B"

-- Função que declara uma dimensão A, então insere uma escolha em A em alguma 
--expressão, de acordo com a função argumento.

insertA :: (V Int -> V Int) -> V Int
insertA f = dimA (f ( chcA [Obj 1, Obj 2]))