 
data A = A {a :: Int, b :: Int, c :: Int} deriving (Show)

l = A 1 2 3

main = putStrLn $ show l{a=5,b=3}