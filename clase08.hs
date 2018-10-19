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

variaciones :: Set Integer -> Integer -> Set [Integer]
variaciones _ 0 = []
variaciones x [a:as] = []

variacionesAux :: Integer -> Set Integer -> Set [Integer]
variacionesAux 0 _ = []
variacionesAux 0 _ = []
variacionesAux x [a:as] = [a] ++ variaciones (x-1) [as]  
