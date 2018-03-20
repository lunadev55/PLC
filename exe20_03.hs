--funcoes sobre listas
-- PART 1 

-- doubles each elem in a list
double :: [Int] -> [Int]
double xs = [x^2 | x <- xs]

-- checks if a give element is part of the list
member :: [Int] -> Int -> Bool
member [] _ = False
member (l:ls) x
    | x == l = True
    | otherwise = member ls x
    
-- results in a list with just the nums of a given list
digits :: String -> String
digits xs = [x | x <- xs, x `elem` ['0'..'9']]

-- sum pairs
sumPairs :: [(Int,Int)] -> [Int]
sumPairs l = [ x+y | (x,y) <- l]


-- PART 2
type Pessoa = String
type Livro = String
type BancoDados = [(Pessoa, Livro)]

baseExemplo :: BancoDados
baseExemplo = [("Joao","Software Abstractions"), ("Andre","Programming in Haskell"), ("Fernando", "Introduction to Programming with Python"), ("Fernando","Programming in Haskell")]

livros :: BancoDados -> Pessoa -> [Livro]
livros [] _ = []
livros ((a,b):ls) x 
    | a == x = [b] ++ (livros ls x)
    | otherwise = livros ls x


