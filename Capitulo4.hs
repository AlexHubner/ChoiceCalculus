module Capitulo4 where

--Definindo os tipos Dim e Tag
type Dim = String
type Tag = String

--Definindo V como tipo de dado para representação de variação genérica
data V a = Obj a
    |Dim Dim [Tag] (V a)
    |Chc Dim [V a]
    deriving (Eq,Show)

--Para a solução de várias das próximas questões vamos definir um Functor e um Monad(applicative)
instance Functor V where
    fmap f (Obj x)       = Obj $ f x
    fmap f (Dim d ts vs) = Dim d ts $ fmap f vs
    fmap f (Chc d vs)    = Chc d $ fmap (fmap f) vs


--Implementação do Monad como está no tutorial:
{--instance Monad V where
  return = Obj
  Obj a >>= f = f a
  Dim d t v >>= f = Dim d t (v >>= f)
  Chc d vs >>= f = Chc d (map (>>= f) vs) --}

{--Devido a mudanças introduzidas no GHC é necessário escrevermos uma instância de Applicative de V. 
Aqui iremos seguir as orientações do Breno* que indica que como um Monad é um Applicative ele introduz
a função bindV que é identica a >>= --}

bindV :: V a -> (a -> V b) -> V b
Obj a     `bindV` f = f a
Dim d t v `bindV` f = Dim d t (v >>= f)
Chc d vs  `bindV` f = Chc d (map (>>= f) vs)

instance Applicative V where
  pure = Obj
  mf <*> mx = mf `bindV` (\f -> mx `bindV` (\x -> pure $ f x))

instance Monad V where
  (>>=) = bindV