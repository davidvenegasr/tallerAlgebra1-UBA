-- TP TALLER ALGEBRA 1 - GRUPO 18

-- 632/18 Pérez Reiter Sebastián Lautaro MN
-- 669/18 Carreira Leandro Emmanuel MN
-- 783/18 Venegas Ramirez David Alejandro MM 


type Conjunto a = [a]
type Tablero = [[Integer]]
type Posicion = (Integer,Integer)
type Camino = [Posicion]

sopa1 :: Tablero
sopa1 = [[13, 12, 19, 6], [7, 13, 32, 6], [22, 20, 14, 7], [7, 33,53, 16], [27, 2, 8, 18]]

sopa2 :: Tablero
sopa2 = [[(-20), (-20), (-20)], [0, 10, 20], [(-10), (-10), 0], [10, 20,(-10)]]

sopa3 :: Tablero
sopa3 = [[10,5,15],[-1,7,2],[2,12,3]]

camino1 :: Camino 
camino1 = [(1,1),(1,2),(2,2),(2,3)]

camino2 :: Camino 
camino2 = [(2,1),(2,2),(2,3),(3,3),(4,3)]

camino3 :: Camino 
camino3 = [(1,2),(2,2),(3,2)]

-- Dado la cantidad filas de un tablero.
cantidadFilas :: Tablero -> Integer
cantidadFilas t = fromIntegral (length t)

-- Dado la cantidad columnas de un tablero.
cantidadColumnas :: Tablero -> Integer
cantidadColumnas (t:ts) = fromIntegral (length t)

-- Devuelve el valor de una posicion de un tablero
valor :: Tablero -> Posicion -> Integer
valor (t:ts) (1,y) = valorY t y
valor (t:ts) (x,y) = valor ts (x-1,y)

valorY :: [Integer] -> Integer -> Integer
valorY (c:cs) 1 = c
valorY (c:cs) n = valorY cs (n-1)  

-- Determina si una posicion esta dentro de los limites de un tablero
posValida :: Tablero -> Posicion -> Bool
posValida t (x,y) = x >= 1 && x <= (cantidadFilas t) && y >= 1 && y <= (cantidadColumnas t) 




---TP INICIA ACA

--maximo es una funcion que devuelve el número más grande de un tablero dado

maximo :: Tablero -> Integer 
maximo [] = error "empty list"
maximo [x] = maximoDeUnaFila x
maximo (x:xs) = max (maximoDeUnaFila x) (maximo xs)

--maximoDeUnaFila es una funcion que devuelve el número más grande de una lista (en este caso una fila del tablero)
maximoDeUnaFila :: [Integer] -> Integer
maximoDeUnaFila [] = error "empty list"
maximoDeUnaFila [x] = x
maximoDeUnaFila (x:xs) = max x (maximoDeUnaFila xs) 

-- masRepetido es una funcion que devuelve el número que más veces aparece en un tablero dado. 
-- si hay empate devuelve cualquiera de ellos.

masRepetido :: Tablero -> Integer
masRepetido t = escanearTab t 0 0 []

-- escanearTab es una funcion escanea el tablero comparando las repeticiones de cada uno de sus valores
-- con el siguiente, solo quedandose con el mayor
-- m: valor que mas aparece
-- i: cantidad de apariciones de m 
-- historial: lista elementos ya contados, para no contar 2 veces el mismo
escanearTab :: Tablero -> Integer -> Integer -> [Integer] -> Integer
escanearTab [] m i historial = m
escanearTab ([]:ts) m i historial = escanearTab ts m i historial
escanearTab ((n:ns):ts) m i historial 
  | elem n historial = escanearTab (ns:ts) m i historial
  | (contar tablero n) < i = escanearTab (ns:ts) m i historial
  | otherwise  = escanearTab (ns:ts) n (contar tablero n) (n:historial)
    where tablero = ((n:ns):ts)      
                                    
-- contar es una funcion que cuenta las ocurrencias del valor v en un tablero

contar :: Tablero -> Integer -> Integer
contar [] v = 0
contar ([]:ts) v = contar ts v
contar ((n:ns):ts) v | v == n = 1 + contar (ns:ts) v
                     | otherwise = contar (ns:ts) v
----------------------------------------------------------
-- fin de masRepetido

-- numerosDeCamino es una funcion que devuelve los números de los casilleros de un camino.

numerosDeCamino :: Tablero -> Camino -> [Integer]
numerosDeCamino _ [] = []
numerosDeCamino t (c:cs) = (casillero):(numerosDeCamino t cs)
                           where casillero = valor t c

-- caminoSinRepetidos es una funcion que devuleve True si y solo si en un camino no aparecen números repetidos

caminoSinRepetidos :: Tablero -> Camino -> Bool
caminoSinRepetidos t [] = True
caminoSinRepetidos t (x:xs) 
  | noHayRepeticionesDe t (x:xs) = noHayRepeticionesDe t xs
  | otherwise = False
  
-- noHayRepeticionesDe es una funcion que evalua si el primer elemento de una lsita coincide con algun otro elemento de ese lista

noHayRepeticionesDe :: Tablero -> Camino -> Bool
noHayRepeticionesDe t [x] = True
noHayRepeticionesDe t (x1:x2:xs)
  | primerValor /= segundoValor = noHayRepeticionesDe t (x1:xs)
  | otherwise = False
  where primerValor = valor t x1
        segundoValor = valor t x2
  
-- caminoDeFibonacci es una funcion que determina si los números de los casilleros de un camino forman un camino de Fibonacci.
-- caminoDeFibonacci  compara si la suma de las dos primeras posiciones de un camino es igual al valor de la tercera posicion  
caminoDeFibonacci :: Tablero -> Camino -> Bool
caminoDeFibonacci _ [] = True
caminoDeFibonacci _ (c1:[]) = True
caminoDeFibonacci _ (c1:c2:[]) = True
caminoDeFibonacci t (c1:c2:c3:c) 
  | (valor t c1 + valor t c2) == (valor t c3) = caminoDeFibonacci t (c2:c3:c) 
  | otherwise = False
  
  
  
  
  
  
  
  
  
  
  
  
  
