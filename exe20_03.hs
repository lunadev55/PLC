-- FUNCTIONS OVER LISTS
-- PART 1 

-- doubles each elem in a list
double :: [Int] -> [Int]
double xs = [x^2 | x <- xs]

-- checks if a given element is part of the list
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

-- NOT USING LIST COMPREHENSION

livros :: BancoDados -> Pessoa -> [Livro]
livros [] _ = []
livros ((a,b):ls) x 
    | a == x = [b] ++ (livros ls x)
    | otherwise = livros ls x

emprestimos :: BancoDados -> Livro -> [Pessoa]
emprestimos [] _ = []
emprestimos ((a,b):ls) x
	| b == x = [a] ++ (emprestimos ls x)
	| otherwise = emprestimos ls x

emprestado :: BancoDados -> Livro -> Bool
emprestado [] _ = False
emprestado ((a,b):ls) x
	| b == x = True
	| otherwise = emprestado ls x

qtdEmprestimos :: BancoDados -> Pessoa -> Int
qtdEmprestimos [] _ = 0
qtdEmprestimos ((a,b):ls) x
	| a == x = 1 + qtdEmprestimos ls x
	| otherwise = qtdEmprestimos ls x

-- having trouble
emprestar :: BancoDados -> Pessoa -> Livro -> BancoDados
emprestar [] _ = []
emprestar ((a,b):ls) = ls ++ [(a,b)]











