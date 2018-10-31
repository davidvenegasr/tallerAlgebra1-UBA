type Set a = [a]

vacio :: Set Integer
vacio = []

agregar :: Integer -> Set Integer ->  Set Integer
agregar n xs
 | elem n xs = xs
 | otherwise = n:xs

incluido :: Set Integer -> Set Integer -> Bool
incluido [] b = True
incluido (a:as) b
  | length (a:as) > length b = False
  | elem a b == True = incluido as b
  | otherwise = False

iguales :: Set Integer -> Set Integer -> Bool
iguales a b
  | length a /= length b = False
  | otherwise = incluido a b

agregarATodos :: Integer -> Set (Set Integer) -> Set (Set Integer)
agregarATodos n [] = []
agregarATodos n (c:cls)  = (agregar n c):agregarATodos n cls

partes :: Integer -> Set (Set Integer)
partes 0 = [[]]
partes n = agregarATodos n (partes (n-1)) ++ (partes (n-1))

productoCartesiano :: Set Integer -> Set Integer -> Set (Integer, Integer)
productoCartesiano [] _ = []
productoCartesiano _ [] = []
productoCartesiano (a:as) (b:bs) = [(a,b)] ++ productoCartesiano [a] bs ++ productoCartesiano as (b:bs)

--variaciones (x:xs) b = [variacionesAux (x:xs) x ++ variaciones xs]

agregarATodos' :: Integer -> Set ([Integer]) -> Set [Integer]
agregarATodos' n [] = []
agregarATodos' n (c:cls)  = (n:c): agregarATodos' n cls

agregarCadaUnoATodos :: Set Integer -> Set [Integer] -> Set [Integer]
agregarCadaUnoATodos [] ys = []
agregarCadaUnoATodos (x:xs) ys = (agregarATodos' x ys ) ++ (agregarCadaUnoATodos xs ys)

variaciones :: Set Integer -> Integer -> Set [Integer]
variaciones xs 0 = [[]]
variaciones xs n = agregarCadaUnoATodos xs (variaciones xs (n-1))

{-
bolitasEnCajas :: Integer -> Integer -> Integer
bolitasEnCajas _ 0 = 0
bolitasEnCajas 0 _ = 0
-bolitasEnCajas bolitas cajas = factorial (bolitas+cajas-1) -}
