--Recursive functions and sequences 

fib:: Int -> Int

fib x | x == 0 = 0
      | x == 1 = 1
      | x > 1 = fib (x-1) + fib (x-2) 


fa1 :: Int -> Int
fa1 x | x == 1 = 2
      | x > 1 = 2*(x-1)* fa1 (x-1) + (2^x)*factorial (x-1)

fa2 :: Int -> Int

fa2 n | n == 1 = 1
      | n == 2 = 2
      | n > 2 = (n-2)*fa2 (n-1) + 2* (n-1)*fa2 (n-2) 
 
fa3 :: Int -> Int

fa3 x | x == 1 = -3
      | x == 2 = 6
      | x > 2 && mod x 2 /= 0 = (-1)*(fa3 (x-1) - 3)
      | x > 2 && mod x 2 == 0 = (fa3 (x-1) + 2*fa3 (x-2) + 9)


     

factorial x | x == 0 = 1
            | x > 0 = x * factorial (x-1)

suma:: Int -> Int
suma 1 = 0
suma n = n + suma (n-1)

func1 :: Int -> Int

func1 x | x == 0 = 1 
        | x > 0 = 2^x + func1 (x-1)

func2 :: Integer -> Float -> Float
func2 n q 
  | q == 0 = 0
  | n > 0 && q > 0 = q^(n) + func2 (n-1) q


-- func2pro :: (Num a) => a -> a -> a
func2pro :: Integer -> Integer -> Integer
func2pro n 0 = 0  
func2pro 0 q = 1
-- func2pro n q = q**(n) + func2 (n-1) q

func3 :: Integer -> Float -> Float
func3 n q 
  | n == 0 = 1
  | n >0 && q > 0 = q^(2*n) + func3 (n-1) q

func4 :: Integer -> Float -> Float
func4 n q 
  | n == 0 = 1
  | n >0 && q > 0 = func2 (2*n) q

-- q^(2*n) + func3 (n-1) q 

