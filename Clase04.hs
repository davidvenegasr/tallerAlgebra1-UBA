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
--Implementar la funci´on esPar :: Integer -> Bool que determine si un n´umero natural es
--par. No est´a permitido utilizar mod ni div.

esPar :: Integer -> Bool
esPar x 
  | x == 1 = False 
  | x == 2 = True
  | x > 2 = esPar (x-2)
  
-- para este ejericio se considera que los numeros naturaes no incluyen al cero


esPar2 :: Integer -> Bool
esPar2 x
  | x == 1 = False
  | x > 1 = not (esPar2 (x-1))

-- esta funcion alternativa se basa en considerar que los numeros naturales se encuentran de forma ordenada de par e impar
-- por lo que si el numero anterior es impar, el numero siguiente sera par.

{-|
   Escribir una funci´on para determinar si un n´umero natural es m´ultiplo de 3. No est´a
permitido utilizar mod ni div.
-}

esMult3 :: Integer-> Bool
esMult3 x
   | x == 3 = True
   | x > 3 = esMult3 (x-3)
   | otherwise = False
   
{-|Implementar la funci´on sumaImpares :: Integer -> Integer que dado n ∈ N sume los
primeros n n´umeros impares. Ej: sumaImpares 3 1+3+5 9.
-}
sumaImpares :: Integer -> Integer
sumaImpares x
  | x == 1 = 1
  | x > 1 = (2*x -1) + sumaImpares (n - 1)

{-|
Escribir una funci´on doblefact que dado n ∈ N calcula n!! = n (n − 2)(n − 4) · · · . Por
ejemplo:
doblefact 10 10 ∗ 8 ∗ 6 ∗ 4 ∗ 2 3840.
doblefact 9 9 ∗ 7 ∗ 5 ∗ 3 ∗ 1 945.
-}

doblefact :: Integer -> Integer
doblefact  x
  | x == 0 = 1
  | x == 1 = 1
  | x > 1 = x * doblefact (x-2) 

{-| Escribir una funci´on recursiva que no termine si se la ejecuta con enteros negativos (y en
cambio s´ı termine para el resto de los enteros).
-}
 noTermina x
  | x == 0 = 0 
  | otherwise = noTermina (x-1)
