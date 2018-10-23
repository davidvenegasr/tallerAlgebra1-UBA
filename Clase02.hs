-- Clase02.hs 08/29/18 CABA 

signo :: Integer -> Integer
signo n 
  | n > 0 = 1
  | n == 0 = 0
  | otherwise = -1

absoluto :: Integer -> Integer 
absoluto n 
  | signo n == -1 = (-n)
  | otherwise = n
   
--absoluto2 es una version mas corta de la funcion absoluto y que se define en una sola expresion sin la necesidad de usar guardas

absoluto2 :: Integer -> Integer
absoluto2 n = (signo n)*n 

maximo :: Integer -> Integer -> Integer 
maximo a b 
 | a >= b = a 
 | otherwise = b

--maximo2 es una funcion que utiliza la definicion matematica de maximo para calcular el mayor de 2 numeros
maximo2 :: Integer -> Integer -> Float
maximo2 a b = fromIntegral (absoluto (a+b) - absoluto (a-b)) / 2 

maximoEntre3 :: Integer -> Integer -> Integer -> Integer 
maximoEntre3 a b c =  maximo (maximo a b) c

esPar:: Int -> Bool 
esPar n = mod n 2 == 0

--esPar es una funcion que evalua un expresion, como esta puede ser True or False no es necesario usar otherwise

esMultiploDe :: Int -> Int -> Bool
esMultiploDe a b = mod b a == 0

--esMultiploDe: dados dos numeros naturales, determina si el primero es multiplo del segundo
 
esMultiploDe2:: (Integral a, Eq b) => a -> a -> b -> Bool
esMultiploDe2 x y z = mod y x == 0     
--esMultiplo2 era simplemente una prueba con variables de tipos

crearPar :: a -> b -> (a, b)
crearPar a b = (a, b)

invertir :: (a, b) -> (b, a) 
invertir (a, b) = (b, a)

-- este metodo a pesar de su simplicidad utiliza patter matching, el lector deberia utlizar las funciones fst y snd para trabajas con guardas en esta clase

distanciaPuntos :: (Float, Float) -> (Float, Float) -> Float
distanciaPuntos (a,b) (c,d) = sqrt ((c-a)^2+(d-b)^2)

f1 :: Float -> (Float, Float, Float)
f1 x = (2*x, x^2, x-7)

f2 :: Integer -> Integer
f2 x 
 | mod x 2 == 0 = div x 2
 | otherwise = x+1
 
f :: Integer -> Integer
f x
 | mod x 6 == 0 = div (x^2) 2
 | otherwise = 3*x+1
 
g :: (Integer, Integer) -> Integer
g (a,b) = a*(b+1)

h :: (Integer, Integer) -> Integer
h (a,b) = f (g (a,b))
