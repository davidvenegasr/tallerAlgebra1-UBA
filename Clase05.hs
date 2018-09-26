factorial :: Integer -> Integer
factorial x 
  | x == 0 = 1
  | x == 1 = 1 
  | x > 1 = x * factorial (x-1)

eAprox :: Integer -> Float
eAprox x 
  | x == 1 = 1
  | x > 1 = (1/fromInteger(factorial x)) + eAprox (x-1)

e :: Float
e = eAprox 100

parteEntera :: Float -> Integer
parteEntera x 
  | x > -1  && x <= 1 = 0
  | x > 1 = parteEntera (x - 1) + 1
  | x < 0 = parteEntera (x + 1) - 1

division :: Integer -> Integer -> (Integer, Integer)
-- Debe funcionar para a≥0,d>0 y no se pueden usar div, mod ni/.

division a d 
  | a < d = (0,a)
  | otherwise = (fst qr' + 1 , snd qr')  
   where qr' = division (a-d) d
---ahora la modificamos para los negativos 

divisionCompleta :: Integer -> Integer -> (Integer, Integer)
divisionCompleta a d 
  | a >= 0 && a < d  = (0, a) 
  | a >= d = (fst qr' + 1 , snd qr') 
  | otherwise = (fst qrn - 1 , snd qrn)  
   where qr' = division (a-d) d
         qrn = division2 (a + d) d


sumaDivisoresHasta :: Integer -> Integer -> Integer
sumaDivisoresHasta x y 
 | y == 1 =  1
 | y > 1 = (sumaDivisoresHasta x (y-1)) + (sumoSiEsDivisor x y)

sumoSiEsDivisor :: Integer-> Integer -> Integer
sumoSiEsDivisor x y 
 | mod x y /= 0 = 0 
 | mod x y == 0 = y


menorDivisor :: Integer -> Integer
menorDivisor x 
 | x mod k == 0 = k
 where k = menorDivisor x + 1 

--esPrimo :: Integer -> Bool
--esPrimo = Undefined


