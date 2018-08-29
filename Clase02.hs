Clase02.hs 08/29/18 CABA 
signo:: Int -> Int

signo n | n > 0 = 1
        | n == 0 = 0
        | otherwise = -1

absoluto:: Int -> Int 

absoluto n | signo n == -1 = (-n)
           | otherwise = n 

absoluto3 n = (signo n)*n 


maximo a b | a >= b = a 
           | otherwise = b

maximo2 a b = fromIntegral (absoluto (a+b) - absoluto (a-b)) / 2 

maximo3 a b c =  maximo (maximo a b) c

esPar:: Int -> Bool 
esPar n = mod n 2 == 0
 
esMultiploDe:: Int -> Int -> Bool

esMultiploDe a b = mod b a == 0
 
esMultiploDe2:: (Integral a, Eq b) => a -> a -> b -> Bool

esMultiploDe2 x y z = mod y x == 0     
