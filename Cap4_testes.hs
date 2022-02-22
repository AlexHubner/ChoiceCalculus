module Cap4_testes where
import Capitulo4
import Cap4_aux

{-- Página 66 --}
inserir = insertA (\v -> Dim "A" ["a3","a4"] v)

{-- Página 67 --}
--Expressão variacional inteira ab, onde dimB e chcB são constutores similares a dimA e chcA.
ab = dimA $ chcA [dimB $ chcB [Obj 1, Obj 2], Obj 3]

--  Usando fmap podemos incrementar os valores de todos os objetos em uma expressão variacional inteira;
--  Ou podemos fazer um map com a função odd sobre a estrutura o que produzirá uma expressão variacional
--booleana do tipo V bool.

{-- Página 68 --}
dis = Dim "S" ["n","y"] $ ab >>= (\i -> Chc "S" [Obj i, Obj (i*i)])

--  Cada valor da expressão ab é expandida em uma escolha na dimensão S.

