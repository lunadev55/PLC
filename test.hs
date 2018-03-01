-- basic haskell and test to compile and run

--basic function to add 2 numbers
soma :: Int -> Int -> Int
soma a b = (a+b)

--basic factorial function
fatorial :: Int -> Int
fatorial 0 = 1
fatorial n = n * fatorial(n-1)