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
         qrn = division (a + d) d


sumaDivisoresHasta :: Integer -> Integer -> Integer
sumaDivisoresHasta x y 
  | y == 1 =  1
  | y > 1 = (sumaDivisoresHasta x (y-1)) + (sumoSiEsDivisor x y)

sumoSiEsDivisor :: Integer-> Integer -> Integer
sumoSiEsDivisor x y 
  | mod x y /= 0 = 0 
  | mod x y == 0 = y
 
sumaDivisores :: Integer -> Integer
sumaDivisores x
  | x == 0 =  0
  | x > 1 = sumaDivisoresHasta x x

menorDivisor :: Integer -> Integer
menorDivisor x 
  | sumaDivisores x == (x+1) = x 
  | otherwise = auxMenorDivisor x 2

auxMenorDivisor :: Integer -> Integer -> Integer
auxMenorDivisor x b 
  | mod x b == 0 = b
  | otherwise = auxMenorDivisor x (b+1)

 
esPrimo :: Integer -> Bool
esPrimo x = menorDivisor x == x 

func :: Integer -> Float -> Float 
func n m 
  | m == 1 = suma (fromIntegral n)
  | otherwise = func2 n m + func n (m-1)
----problema solo funciona cuando n = m  
suma :: Float -> Float
suma 1 = 1
suma x = x + suma (x-1)

func2 :: Integer -> Float -> Float
func2 o p  
  | o == 1 = 1
  | o > 1 = p^(o) + func2 (o-1) p
  
