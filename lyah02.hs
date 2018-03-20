
--doubleMe :: Int -> Int
doubleMe x = x + x

--doubleUs :: Int -> Int -> Int
doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x = if x > 100
						then x
						else x*2

doubleList :: [Int] -> [Int]
doubleList x = [x | x <- [50..100], x `mod` 7 == 3] 

boomBangs xs = [if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

-- removes lower case letters from a string
rNonuppercase st = [c | c <- st, c `elem` ['A'..'Z']]

triangles' = [(a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2, a+b+c == 24]