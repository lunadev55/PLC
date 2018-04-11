-- factorial
fatorial :: Int -> Int
fatorial 0 = 1
fatorial n = n * fatorial (n-1)

-- checks if all 4 numbers are equal
all4Equal :: Int -> Int -> Int -> Int -> Bool
all4Equal a b c d = (a == b) && (b == c) && (c == d)

-- check if all 4 numbers are equal using 'allEqual'

-- calculates how many of the 3 nums area equal
equalCount :: Int -> Int -> Int -> Int
equalCount a b c
    | (a == b) && (b == c) = 3
    | (a == b) || (b == c) = 2
    | otherwise = 0
    
-- calculates the number of sales equal to 's' in 'n' weeks
vendas 0 = 0
vendas 1 = 3
vendas 2 = 6
vendas 3 = 3
vendas 4 = 2
vendas 5 = 3

equals' :: Int -> Int -> Int
equals' s n
    | (n == 0) = 0
    | ((vendas n) == s) = 1 + equals' s (n-1)
    | otherwise = equals' s (n-1)

    
