{- the value size is an integer (Int), defined to be
   the sum of twelve and thirteen -}

size :: Int
size = 12+13

-- the function to square an integer
square :: Int -> Int
square n = n*n

-- the function to double an integer
double :: Int -> Int
double n = 2*n

-- an example using double, square and size
example :: Int
example = double (size - square (2+2))

-- the function doubles its input and squares the result
dbl_sqr :: Int -> Int
dbl_sqr n = square (double (n))

-- the function squares its input and doubles the result
sqr_dbl :: Int -> Int
sqr_dbl n = double (square (n))

-- the function calculates the biggest n between a and b
maxi :: Int -> Int -> Int
maxi a b
	|a > b = a
	|otherwise = b