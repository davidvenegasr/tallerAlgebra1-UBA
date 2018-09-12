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
      | x > 2 && mod x 2 == 0 = (-1)*(fa3 (x-1) - 3)
      | x > 2 && mod x 2 /= 0 = (fa3 (x-1) + 2*fa3 (x-2) + 9)


     

factorial x | x == 0 = 1
            | x > 0 = x * factorial (x-1)
