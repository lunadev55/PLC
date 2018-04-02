
type Pessoa = String
type Livro = String
type BancoDados = [(Pessoa, Livro)]


baseExemplo :: BancoDados
baseExemplo = [("Joao","Software Abstractions"), ("Andre","Programming in Haskell"), 
   ("Fernando" ,"Introduction to Programming with Python"), 
   ("Fernando","Programming in Haskell")]

f bd lv = [ True | (n,l) <- bd, lv == l ]


emprestado :: BancoDados -> Livro -> Bool
emprestado bd lv = length([ 1 | (n, l) <- bd, lv == l ]) /= 0

emprestimos :: BancoDados -> Livro -> [Pessoa]
emprestimos bd lv = [ p | (p,l)  <- bd , lv == l ]

-- Funcoes como valores

ehPar :: Int -> Bool
ehPar x = x `mod`2 == 0

ehImpar :: Int -> Bool
ehImpar x = not (ehPar x)

listaFuncoes = [ehPar, ehImpar]

inc :: Int -> Int 
inc n = n + 1

negacao :: Bool -> Bool
negacao b = not b

applyTwice :: ( a -> a) -> a -> a 
applyTwice f x = f (f x)

vendas 0 = 5
vendas 1 = 0
vendas 2 = 3

totalVendas :: Int -> Int
totalVendas 0 = vendas 0
totalVendas n = vendas n + totalVendas (n-1)

quadrado :: Int -> Int
quadrado n = n * n

total :: (Int -> Int) -> Int -> Int
total f 0 = f 0
total f n = f n + total f  (n-1)

maxi :: Int -> Int -> Int
maxi m n
 | m >= n = m
 |otherwise = n

maxFun :: (Int -> Int) -> Int -> Int
maxFun f 0 = f 0
maxFun f n = maxi (f n) (maxFun f (n-1))

zeroInRange :: (Int -> Int) -> Int -> Bool
zeroInRange f 0 = (f 0 == 0)
zeroInRange f n = (f n == 0) || zeroInRange f (n-1)

-- Sobre listas

somaLista :: [Int] -> Int
somaLista [] = 0
somaLista (x:xs) = x + somaLista xs


dobroLista :: [Int] -> [Int]
dobroLista [] = []
dobroLista (x:xs) = (2*x) : dobroLista xs

quadradoLista :: [Int] -> [Int]
quadradoLista [] = []
quadradoLista (x:xs) = (x*x): quadradoLista xs

dobro :: Int -> Int
dobro x = 2 * x

dobroLista2 :: [Int] -> [Int]
dobroLista2 [] = []
dobroLista2 (x:xs) = (dobro x ) : dobroLista2 xs

quadradoLista2 :: [Int] -> [Int]
quadradoLista2 [] = []
quadradoLista2 (x:xs) = (quadrado x): quadradoLista2 xs

mapeamento :: (Int -> Int) -> [Int] -> [Int]
mapeamento f [] = []
mapeamento f (x:xs) = (f x ): mapeamento f xs


ehDigito ch =  (ch >= '0') && (ch <= '9')

map2 :: (a -> b) -> [a] -> [b]
map2 f [] = []
map2 f (x:xs) = (f x ): map2 f xs

map3 :: (a -> b) -> [a] -> [b]
map3 f l = [ f x | x <- l ]


qs [] = []
qs (x:xs) = qs [y |  y <- xs, y < x ]  
            ++ [x] ++ 
            qs [y |  y <- xs, y > x ]