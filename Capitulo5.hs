module Capitulo5 where
import Capitulo4

type VList a = V (List a) --Listas variacionais

-- Definição de listas
data List a = Cons a (List a)
            | Empty
            | VList (VList a)
            deriving (Eq,Show)

--Data type representando as comidas
data Food = Steak | Pasta | Fries | Cake | Sherry deriving (Show,Eq)