listar :: a -> a -> a-> [a]
listar a b c = a:b:c:[]

-- cien es una expresion
cien = [1,0..(-100)]

suma :: [Integer] -> Integer
suma xs 
  | length xs == 0 = 0
  | otherwise = (head xs) + sum (tail xs)
 
pertenece :: Integer -> [Integer] -> Bool
pertenece x ys 
  | length ys == 0 = False
  | otherwise = x == head ys || pertenece x (tail ys)
  
sumPro :: [Integer] -> Integer
sumPro [] = 0
sumPro (x:xs) = x + sum xs

pertenece2 :: Integer -> [Integer] -> Bool
pertenece2 x [] = False
pertenece2 x ys = x == head ys || pertenece x (tail ys)
--para este ejercicio se pidio representar la funcion pertenece usando pattern matching

productoria :: [Integer] -> Integer
productoria [] = 1
productoria (x:xs) = x * productoria xs

sumarN :: Integer -> [Integer] -> [Integer]
sumarN x [] = []
sumarN x (y:ys) = (y+x):(sumarN x ys) 

sumarNUltimo :: [Integer] -> [Integer]
sumarNUltimo ys = sumarN x ys 
                where x = ultimo ys
                
ultimo :: [Integer] -> Integer
ultimo (x:xs)
 | length xs == 0 = x
 | otherwise = ultimo xs 
 
sumarElPrimero :: [Integer] -> [Integer] 
sumarElPrimero ys = sumarN x ys 
                     where x = head ys
               
pares :: [Integer] -> [Integer] 
pares [] = []
pares (y:ys) 
 | y `mod` 2 == 0 = y: (pares ys)
 | otherwise = pares ys 
 
multiplosDeN :: Integer -> [Integer] -> [Integer]
multiplosDeN n [] = []
multiplosDeN n (y:ys) 
 | y `mod` n == 0 = y: (multiplosDeN n ys)
 | otherwise = multiplosDeN n ys 
            
quitar :: Integer -> [Integer] -> [Integer]
quitar n [] = []
quitar n (x:xs) 
 | n == x = quitar n xs
 | otherwise = x:(quitar n xs)
 
hayRepetidos :: [Integer] -> Bool
hayRepetidos [] = False


maximo :: [Integer] -> Integer      
maximo [] = 0
maximo (x:xs) 
 | (maximo xs) > x = maximo xs
 | otherwise = x
 
maximum :: [Integer] -> Integer
maximum [] = error "empty list"
maximum [x] = x
maximum (x:xs) = maximum x (maximum xs)
    
ordenar:: [Integer] -> [Integer]
ordenar [] = []
ordenar (x:xs) = undefined
