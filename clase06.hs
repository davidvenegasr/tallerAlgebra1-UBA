
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

sumaDigitos :: Integer -> Integer

sumaDigitos 0 = 0
sumaDigitos n = n `mod` 10 + sumaDigitos (n `div` 10)

digitosIguales :: Integer -> Bool

digitosIguales 0 = True
digitosIguales n = undefined
