
-- Q1, ou-exclusivo
exor :: Bool -> Bool -> Bool
exor x y
	| (x == False && y == True) = True
	| (x == True && y == False) = True
	| otherwise = False 

-- Q2, ou-exclusivo (usando literais no lado esquerdo)
exOr :: Bool -> Bool -> Bool
exOr True True = False
exOr True False = True
exOr False True = True
exOr False False = False

-- Q3 (duas definições da função nAnd)
nAnd :: Bool -> Bool -> Bool
nAnd True True = True
nAnd True False = False
nAnd False True = False
nAnd False False = False

nAnd2 :: Bool -> Bool -> Bool
nAnd2 True True = True
nAnd2 _ _ = False

-- Q4 (letras minusculas em máiusculas)
offset :: Int
offset = fromEnum 'A' - fromEnum 'a'

maiuscula :: Char -> Char
maiuscula ch
	| (fromEnum ch >= 65 && fromEnum ch <= 90) = 
		toEnum (fromEnum ch + 0)
	| otherwise = toEnum (fromEnum ch + offset)

-- Q5 (converte um dígito como '8' no inteiro 8)
charToNum :: Char -> Int
charToNum x = fromEnum x - fromEnum '0'

-- Q6 (atividades slides)

-- a) produz string com n espacos
addEspacos :: Int -> String
addEspacos n = replicate n ' '

-- b) adiciona n espacos a esquerda de uma string
paraDireita :: Int -> String -> String
paraDireita n t = addEspacos(n) ++ t

-- d) recebe 3 ints e retorna tupla (menor, maior) BUGADO
menorMaior :: Int -> Int -> Int -> (Int, Int)
menorMaior a b c
	| a > (b && c) && b > c = (c,a)
	| a > (b && c) && c > b = (b,a)
	| b > (a && c) && a > c = (c,b)
	| b > (a && c) && a < c = (a,b)
	| c > (a && b) && a > b = (b,c)
	| otherwise = (a,c)





























