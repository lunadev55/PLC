-- Sinonimo
type Par = (Int, Int)

--nomeMaiusculo :: Nome -> Nome
primeiroInteiro :: Par -> Int
primeiroInteiro (x,y) = x


-- Tipos Algebricos
-- Palavra reservada : data

-- Enumeracao
--data MBool = True | False
data Temperatura = Quente | Frio
         deriving (Show)
data Estacao = Verao | Outono 
               | Inverno | Primavera  
{-
clima :: Estacao -> Temperatura
clima Verao = Quente
clima Outono = Quente
clima Inverno = Frio
clima Primavera = Quente
-}

clima :: Estacao -> Temperatura
clima Inverno = Frio
clima _ = Quente

type Nome = String 
type Idade = Int
data Pessoas = Pessoa Nome Idade 
     --  deriving (Show)

showPerson :: Pessoas -> String
showPerson (Pessoa n i) = 
    n ++ " tem " ++ (show i) ++ " anos de idade" 

data Figura = Circulo Float
          | Retangulo Float Float
          deriving (Show)
  
ehCircular :: Figura -> Bool
ehCircular (Circulo _) = True
ehCircular _ = False

--ehCircular (Retangulo _ _) = False

area :: Figura -> Float
area (Circulo r) = pi * r * r
area (Retangulo l a) = l * a

-- Tipo algebrico recursivo

data Expr = Lit Int
          | Add Expr Expr
          | Sub Expr Expr
          --deriving (Show)
exp1 = Lit 2
exp2 = Add (Lit 1) (Lit 2)
exp3 = Add (Lit 5) (Sub (Lit 7) (Lit 4) )

eval :: Expr -> Int
eval (Lit n) = n
eval (Add e1 e2) = (eval e1) + (eval e2)
eval (Sub e1 e2) = (eval e1) - (eval e2)

{-
eval Add (Lit 5) (Sub (Lit 7) (Lit 4) )
=
eval (Lit 5) + eval (Sub (Lit 7) (Lit 4))
=
eval (Lit 5) + (eval (Lit 7) - eval (Lit 4)) 
=
5 + ( 7 - 4)
-}

mshow :: Expr -> String
mshow (Lit n) = show n
mshow (Add e1 e2) = "( " ++ (mshow e1) 
                   ++ " + " ++
                    (mshow e2) ++ " )"
mshow (Sub e1 e2) = "( " ++(mshow e1) 
                    ++ " - " ++
                     (mshow e2) ++ " )"

data TreeInt = NilT
             | No TreeInt Int TreeInt
        
arv1 = NilT
arv2 = No (NilT) 4 (NilT)
arv3 = No (No (NilT) 5 (NilT) ) 7 (No (NilT) 9 (NilT))
arv4 = No (No (NilT) 5 (NilT) ) 7 (No (No (No (NilT) 15 (NilT)) 3 (NilT)) 9 (NilT))

somaArv :: TreeInt -> Int
somaArv (NilT) = 0
somaArv (No arvEsq n arvDir) =
     somaArv arvEsq  
     + n +
     somaArv arvDir

profundidade :: TreeInt -> Int
profundidade (NilT) = 0 
profundidade (No arvEsq n arvDir) =
    1 + 
    max (profundidade (arvEsq))
    (profundidade (arvDir))

-- Lista polimorfica

data Lista t = Nil | Cons t (Lista t) 

l1 = Nil
l2 = Cons 4 (Nil)
l3 = Cons 5 (Cons 7 (Cons 9 Nil))

toList :: Lista t -> [t]
toList Nil = []


-- Arvore Polimorfica
data Tree t = Niltree
             | Node (Tree t) t (Tree t)
