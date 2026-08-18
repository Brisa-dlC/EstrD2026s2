{-Trabajo Prático 3
de la Cerda Dominique-}

--1. Tipos recursivos simples
{-1.1. Celdas con b olitas
Representaremos una celda con b olitas de colores ro jas y azules, de la siguiente manera:-}

data Color = Azul | Rojo
 deriving (Show, Eq)

data Celda = Bolita Color Celda | CeldaVacia
 deriving Show

{-En dicha representación, la cantidad de apariciones de un determinado color denota la cantidad
de bolitas de ese color en la celda. Por ejemplo, una celda con 2 bolitas azules y 2 rojas, podría
ser la siguiente:
Bolita Rojo (Bolita Azul (Bolita Rojo (Bolita Azul CeldaVacia)))
Implementar las siguientes funciones sobre celdas:
nroBolitas :: Color -> Celda -> Int
Dados un color y una celda, indica la cantidad de bolitas de ese color. Nota: pensar si ya
existe una operación sobre listas que ayude a resolver el problema.-}
nroBolitas :: Color -> Celda -> Int
nroBolitas _ (CeldaVacia) = 0
nroBolitas c (Bolita c1 ce) = unoSiCeroSino (c == c1) + nroBolitas c ce

unoSiCeroSino :: Bool -> Int
unoSiCeroSino True = 1
unoSiCeroSino False = 0

{-poner :: Color -> Celda -> Celda
Dado un color y una celda, agrega una bolita de dicho color a la celda.-}
poner :: Color -> Celda -> Celda
poner c ce = (Bolita c ce)

{-sacar :: Color -> Celda -> Celda
Dado un color y una celda, quita una bolita de dicho color de la celda. Nota: a diferencia de
Gobstones, esta función es total.-}
sacar :: Color -> Celda -> Celda
sacar _ (CeldaVacia) = CeldaVacia
sacar c (Bolita co ce) = if c == co
                            then ce
                            else (Bolita co (sacar c ce))

{-ponerN :: Int -> Color -> Celda -> Celda
Dado un número n, un color c, y una celda, agrega n b olitas de color c a la celda.-}
ponerN :: Int -> Color -> Celda -> Celda
ponerN 0 _ ce = ce
ponerN n c ce = ponerN (n - 1) c (poner c ce)

{-1.2. Camino hacia el tesoro
Tenemos los siguientes tipos de datos-}

data Objeto = Cacharro | Tesoro
 deriving (Show, Eq)

data Camino = Fin | Cofre [Objeto] Camino | Nada Camino
 deriving Show

camino1 :: Camino
camino1 = (Nada (Cofre [Cacharro] (Nada Fin)))

camino2 :: Camino
camino2 = (Nada (Cofre [Cacharro] (Nada (Cofre [Cacharro, Tesoro, Tesoro] Fin))))

{-Definir las siguientes funciones:
hayTesoro :: Camino -> Bool
Indica si hay un cofre con un tesoro en el camino.-}
hayTesoro :: Camino -> Bool
hayTesoro (Fin) = False
hayTesoro (Nada c) = hayTesoro c
hayTesoro (Cofre obs c) = hayTesoroEnCofre obs || hayTesoro c

hayTesoroEnCofre :: [Objeto] -> Bool
hayTesoroEnCofre [] = False
hayTesoroEnCofre (o:obs) = o == Tesoro || hayTesoroEnCofre obs

{-pasosHastaTesoro :: Camino -> Int
Indica la cantidad de pasos que hay que recorrer hasta llegar al primer cofre con un tesoro.
Si un cofre con un tesoro está al principio del camino, la cantidad de pasos a recorrer es 0.
Precondición: tiene que haber al menos un tesoro.-}
pasosHastaTesoro :: Camino -> Int
-- Precondición: hay al menos un tesoro en el camino.
pasosHastaTesoro (Fin) = 0
pasosHastaTesoro (Nada c) = 1 + pasosHastaTesoro c
pasosHastaTesoro (Cofre obs c) = if hayTesoroEnCofre obs
                                    then 0
                                    else 1 + pasosHastaTesoro c

{-hayTesoroEn :: Int -> Camino -> Bool
Indica si hay un tesoro en una cierta cantidad exacta de pasos. Por ejemplo, si el número de
pasos es 5, indica si hay un tesoro en 5 pasos.-}
hayTesoroEn :: Int -> Camino -> Bool
hayTesoroEn 0 (Cofre obs _) = hayTesoroEnCofre obs
hayTesoroEn n (Nada c) = hayTesoroEn (n - 1) c
hayTesoroEn n (Cofre _ c) = hayTesoroEn (n - 1) c
hayTesoroEn 0 _ = False
hayTesoroEn _ (Fin) = False

{-alMenosNTesoros :: Int -> Camino -> Bool
Indica si hay al menos "n" tesoros en el camino.-}
alMenosNTesoros :: Int -> Camino -> Bool
alMenosNTesoros 0 _ = True
alMenosNTesoros n (Nada c) = alMenosNTesoros n c
alMenosNTesoros n (Cofre obs c) = n <= cantTesorosEn obs || alMenosNTesoros (n - (cantTesorosEn obs)) c
alMenosNTesoros _ _ = False

cantTesorosEn :: [Objeto] -> Int
cantTesorosEn [] = 0
cantTesorosEn (o:obs) = unoSiCeroSino (o == Tesoro) + cantTesorosEn obs

{-(desafío) cantTesorosEntre :: Int -> Int -> Camino -> Int
Dado un rango de pasos, indica la cantidad de tesoros que hay en ese rango. Por ejemplo, si
el rango es 3 y 5, indica la cantidad de tesoros que hay entre hacer 3 pasos y hacer 5. Están
incluidos tanto 3 como 5 en el resultado.-}
cantTesorosEntre :: Int -> Int -> Camino -> Int
cantTesorosEntre 0 n c = 

--2. Tipos arbóreos
{-2.1. Árboles binarios
Dada esta definición para árboles binarios
data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
defina las siguientes funciones utilizando recursión estructural según corresponda:
1. sumarT :: Tree Int -> Int
Dado un árbol binario de enteros devuelve la suma entre sus elementos.
2. sizeT :: Tree a -> Int
Dado un árbol binario devuelve su cantidad de elementos, es decir, el tamaño del árbol (size
en inglés).
3. mapDobleT :: Tree Int -> Tree Int
Dado un árbol de enteros devuelve un árbol con el doble de cada número.
4. perteneceT :: Eq a => a -> Tree a -> Bool
Dados un elemento y un árbol binario devuelve True si existe un elemento igual a ese en el
árbol.
5. aparicionesT :: Eq a => a -> Tree a -> Int
Dados un elemento e y un árbol binario devuelve la cantidad de elementos del árbol que son
iguales a e.
6. leaves :: Tree a -> [a]
Dado un árbol devuelve los elementos que se encuentran en sus hojas.
NOTA: en este tipo se define como hoja a un nodo con dos hijos vacíos.
7. heightT :: Tree a -> Int
Dado un árbol devuelve su altura.
Nota: la altura de un árbol (height en inglés), también llamada profundidad, es
la cantidad de niveles del árbol1
. La altura para EmptyT es 0, y para una hoja
es 1.
8. mirrorT :: Tree a -> Tree a
Dado un árbol devuelve el árbol resultante de intercambiar el hijo izquierdo con
el derecho, en cada nodo del árbol.
9. toList :: Tree a -> [a]
Dado un árbol devuelve una lista que representa el resultado de recorrerlo en
modo in-order.
Nota: En el modo in-order primero se procesan los elementos del hijo izquierdo,
luego la raiz y luego los elementos del hijo derecho.
10. levelN :: Int -> Tree a -> [a]
Dados un número n y un árbol devuelve una lista con los nodos de nivel n. El
nivel de un nodo es la distancia que hay de la raíz hasta él. La distancia de la
raiz a sí misma es 0, y la distancia de la raiz a uno de sus hijos es 1.
Nota: El primer nivel de un árbol (su raíz) es 0.
11. listPerLevel :: Tree a -> [[a]]
Dado un árbol devuelve una lista de listas en la que cada elemento representa
un nivel de dicho árbol.
12. ramaMasLarga :: Tree a -> [a]
Devuelve los elementos de la rama más larga del árbol
13. todosLosCaminos :: Tree a -> [[a]]
Dado un árbol devuelve todos los caminos, es decir, los caminos desde la raíz
hasta cualquiera de los nodos.
ATENCIÓN: se trata de todos los caminos, y no solamente de los maximales (o
sea, de la raíz hasta la hoja), o sea, por ejemplo
todosLosCaminos (NodeT 1 (NodeT 2 (NodeT 3 EmptyT EmptyT)
EmptyT)
(NodeT 4 (NodeT 5 EmptyT EmptyT)
EmptyT))
= [ [1], [1,2], [1,2,3], [1,4], [1,4,5] ]
OBSERVACIÓN: puede resultar interesante plantear otra función, variación de
ésta para devolver solamente los caminos maximales.
2.2. Expresiones Aritméticas
El tipo algebraico ExpA modela expresiones aritméticas de la siguiente manera:
data ExpA = Valor Int
| Sum ExpA ExpA
| Prod ExpA ExpA
| Neg ExpA
Implementar las siguientes funciones utilizando el esquema de recursión estructural sobre Exp:
1. eval :: ExpA -> Int
Dada una expresión aritmética devuelve el resultado evaluarla.
2. simplificar :: ExpA -> ExpA
Dada una expresión aritmética, la simplifica según los siguientes criterios (descritos utilizando
notación matemática convencional):
a) 0 + x = x + 0 = x
b) 0 * x = x * 0 = 0
c) 1 * x = x * 1 = x
d) - (- x) = x-}