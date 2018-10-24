ologico :: Bool -> Bool -> Bool 
ologico  False False = False
ologico _ _ = True 

ylogico :: Bool -> Bool-> Bool
ylogico True True = True
ylogico _ _ = False

implica :: Bool -> Bool-> Bool
implica True False = False
implica _ _ = True

sumaGaussiana :: Integer -> Integer
sumaGuassiana 0 = 0 
sumaGaussiana n = n + sumaGaussiana (n-1)

algunoEsCero :: (Integer, Integer, Integer) -> Bool
algunoEsCero (0,_,_) = True
algunoEsCero (_,0,_) = True
algunoEsCero (_,_,0) = True
algunoEsCero (_,_,_) = False

productoInterno :: (Float, Float) -> (Float, Float) -> Float
productoInterno (x1,y1) (x2,y2) = x1 * x2 + y1 * y2

sumaPrimos :: Integer -> Bool
sumaPrimos 0 = False
sumaPrimos 1 = False
sumaPrimos _ = True

conjeturaGoldbach :: Integer -> Bool
conjeturaGoldbach x = sumaPrimos x 

sumaDigitos :: Integer -> Integer
sumaDigitos 0 = 0
sumaDigitos n = n `mod` 10 + sumaDigitos (n `div` 10)

digitosIguales :: Integer -> Bool
digitosIguales 0 = True
--digitosIguales n = restoDivision10 n == digitosIguales 10
digitosIguales n = comparar n == comparar (n `div` 10)

comparar :: Integer -> Bool
comparar n = restoDivision10 n == restoDivision10 (n `div` 10) 

restoDivision10 :: Integer -> Integer
restoDivision10 n = mod n 10 

mayorSequenciaCollatz :: Integer -> (Integer, Integer)
mayorSequenciaCollatz 1 = (1,1)
mayorSequenciaCollatz x = contadorCollatz x 1 `chequearMayor` mayorSequenciaCollatz (x-1)
 where chequearMayor (a,b) (c,d) | d > b = (c,d)
                                 | otherwise = (a,b) 

contadorCollatz:: Integer -> Integer -> (Integer, Integer)
contadorCollatz 1 y = (1, y)
contadorCollatz x y = (x, y) `sumaa` contadorCollatz (collatz x) (y + 1)
  where sumaa (a,b) (c,d) = (a, d)   
