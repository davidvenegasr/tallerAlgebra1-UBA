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
digitosIguales n
	| n < 0 = digitosIguales ((-1)*n)
	| n <10 = True
	| otherwise = ((mod n 10) == mod (div n 10) 10) && (digitosIguales (div n 10) )


comparar :: Integer -> Bool
comparar n = restoDivision10 n == restoDivision10 (n `div` 10)

restoDivision10 :: Integer -> Integer
restoDivision10 n = mod n 10

collatz :: Integer -> Integer
collatz n
  | mod n 2 == 0 = div n 2
  | otherwise = 3*n+1

numeroMayorSeqCollatz :: Integer -> Integer
numeroMayorSeqCollatz x = numeroMayorSeqCollatzAux x 1

numeroMayorSeqCollatzAux :: Integer -> Integer -> Integer
numeroMayorSeqCollatzAux 1 b = b
numeroMayorSeqCollatzAux x b --b es el contador del max largo de la sequencia
  | largoSeqC1 > largoSeqC2 = numeroMayorSeqCollatzAux (x-1) x --si el largo del primer termino es mayor que el largo de su termino anterior, ese es el nuevo max largo
	| otherwise = numeroMayorSeqCollatzAux (x-1) b --sino se evalua recursivamene el termino anterior
    where largoSeqC1 = largoSeqC x
          largoSeqC2 = largoSeqC b

largoSeqC :: Integer -> Integer --cuenta el largo de la sequencia de collatz un dado numero
largoSeqC 1 = 1
largoSeqC x = 1 + largoSeqC (collatz x)

-- A partir de aqui empieza una nueva funcion que busca un alternativa al problema de collatz
numeroMayorSeqCollatzRap :: Integer -> Integer  ----esta funcion es un alternativa mas rapida a
numeroMayorSeqCollatzRap x = fst (mayorSequenciaCollatz x)

mayorSequenciaCollatz :: Integer -> (Integer, Integer) 
mayorSequenciaCollatz 1 = (1,1)
mayorSequenciaCollatz x = contadorCollatz x 1 `chequeoMayorSeq` mayorSequenciaCollatz (x-1)

chequeoMayorSeq :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
chequeoMayorSeq (a,largoSeq1) (c,largoSeq2)
  | largoSeq2 > largoSeq1 = (c,largoSeq2)
  | otherwise = (a,largoSeq1)
-- chequeoMayorSeq busca comparar cual el el numero con la mayor sequencia

contadorCollatz:: Integer -> Integer -> (Integer, Integer) --cuenta el largo de la sequencia
contadorCollatz 1 y = (1, y)
contadorCollatz x y = (x, y) `aumentoSeq` contadorCollatz (collatz x) (y + 1)

aumentoSeq :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
aumentoSeq (a,b) (c,d) = (a, d)
-- aumentoSeq busca contar el largo de la sequencia hasta que llegas a 1
