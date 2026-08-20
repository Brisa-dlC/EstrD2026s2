{-Trabajo Prático 4
de la Cerda Dominique-}

{-1. Pizzas
Tenemos los siguientes tipos de datos:-}

data Pizza = Prepizza | Capa Ingrediente Pizza
 deriving Show

data Ingrediente = Salsa | Queso | Jamon | Aceitunas Int
 deriving Show

pizza :: Pizza
pizza = Capa (Aceitunas 2) (Capa Salsa (Capa Queso (Capa Salsa Prepizza)))

{-Definir las siguientes funciones:
cantidadDeCapas :: Pizza -> Int
Dada una pizza devuelve la cantidad de ingredientes-}
cantidadDeCapas :: Pizza -> Int
cantidadDeCapas Prepizza = 0
cantidadDeCapas (Capa i p) = 1 + cantidadDeCapas p

{-armarPizza :: [Ingrediente] -> Pizza
Dada una lista de ingredientes construye una pizza-}
armarPizza :: [Ingrediente] -> Pizza
armarPizza [] = Prepizza
armarPizza (i:is) = Capa i (armarPizza is)

{-sacarJamon :: Pizza -> Pizza
Le saca los ingredientes que sean jamón a la pizza-}
sacarJamon :: Pizza -> Pizza
sacarJamon Prepizza = Prepizza
sacarJamon (Capa i p) = if esXIngrediente Jamon i
                            then sacarJamon p
                            else Capa i (sacarJamon p)

esXIngrediente :: Ingrediente -> Ingrediente -> Bool
esXIngrediente Salsa Salsa = True
esXIngrediente Queso Queso = True
esXIngrediente Jamon Jamon = True
esXIngrediente (Aceitunas _) (Aceitunas _) = True
esXIngrediente _ _ = False 

{-tieneSoloSalsaYQueso :: Pizza -> Bool
Dice si una pizza tiene solamente salsa y queso (o sea, no tiene de otros ingredientes. En
particular, la prepizza, al no tener ningún ingrediente, debería dar verdadero.)-}
tieneSoloSalsaYQueso :: Pizza -> Bool
tieneSoloSalsaYQueso Prepizza = True
tieneSoloSalsaYQueso (Capa i p) = esSalsaOQueso i && tieneSoloSalsaYQueso p

esSalsaOQueso :: Ingrediente -> Bool
esSalsaOQueso i = esXIngrediente Salsa i || esXIngrediente Queso i

{-duplicarAceitunas :: Pizza -> Pizza
Recorre cada ingrediente y si es aceitunas duplica su cantidad-}
duplicarAceitunas :: Pizza -> Pizza
duplicarAceitunas Prepizza = Prepizza
duplicarAceitunas (Capa i p) = Capa (duplicarSiEsAceituna i) (duplicarAceitunas p)

duplicarSiEsAceituna :: Ingrediente -> Ingrediente
duplicarSiEsAceituna (Aceitunas n) = Aceitunas (n * 2)
duplicarSiEsAceituna x = x

{-cantCapasPorPizza :: [Pizza] -> [(Int, Pizza)]
Dada una lista de pizzas devuelve un par donde la primera componente es la cantidad de
ingredientes de la pizza, y la respectiva pizza como segunda componente.-}
cantCapasPorPizza :: [Pizza] -> [(Int, Pizza)]
cantCapasPorPizza [] = []
cantCapasPorPizza (p:ps) = (cantidadDeCapas p, p) : cantCapasPorPizza ps

{-2. Mapa de tesoros (con bifurcaciones)
Un mapa de tesoros es un árbol con bifurcaciones que terminan en cofres. Cada bifurcación y
cada cofre tiene un objeto, que puede ser chatarra o un tesoro.-}

data Dir = Izq | Der
 deriving (Show, Eq)

data Objeto = Tesoro | Chatarra
 deriving (Show, Eq)

data Cofre = Cofre [Objeto]
 deriving Show

data Mapa = Fin Cofre | Bifurcacion Cofre Mapa Mapa
 deriving Show

mapa :: Mapa
mapa = Bifurcacion cofreT (Bifurcacion cofreT (Fin cofreC) (Fin cofreC)) (Fin cofreC)

cofreC :: Cofre 
cofreC = Cofre [Chatarra]

cofreT :: Cofre 
cofreT = Cofre [Tesoro]

{-Definir las siguientes operaciones:
1. hayTesoro :: Mapa -> Bool
Indica si hay un tesoro en alguna parte del mapa.-}
hayTesoro :: Mapa -> Bool
hayTesoro (Fin c) = tieneTesoro c
hayTesoro (Bifurcacion c mi md) = tieneTesoro c || hayTesoro mi || hayTesoro md

tieneTesoro :: Cofre -> Bool
tieneTesoro (Cofre obs) = hayAlgunTesoro obs

hayAlgunTesoro :: [Objeto] -> Bool
hayAlgunTesoro [] = False
hayAlgunTesoro (o:obs) = o == Tesoro || hayAlgunTesoro obs


{-2. hayTesoroEn :: [Dir] -> Mapa -> Bool
Indica si al final del camino hay un tesoro. Nota: el final de un camino se representa con una
lista vacía de direcciones.-}
hayTesoroEn :: [Dir] -> Mapa -> Bool
hayTesoroEn [] m = hayTesoro m 
hayTesoroEn (d:ds) (Bifurcacion _ mi md) = if d == Izq
                                            then hayTesoroEn ds mi
                                            else hayTesoroEn ds md
hayTesoroEn ds (Fin _) = error "El camino es mas largo que el mapa"

{-3. caminoAlTesoro :: Mapa -> [Dir]
Indica el camino al tesoro. Precondición: existe un tesoro y es único.-}
caminoAlTesoro :: Mapa -> [Dir]
--Precondición: existe un tesoro y es único.
caminoAlTesoro (Fin c) = if tieneUnSoloTesoro c
                            then []
                            else error "No hay tesoros o hay mas de uno"
caminoAlTesoro (Bifurcacion c mi md) = if tieneUnSoloTesoro c && (not (hayTesoro mi || hayTesoro md))
                                            then []
                                            else if hayTesoro mi
                                                    then Izq: caminoAlTesoro mi
                                                    else Der: caminoAlTesoro md

tieneUnSoloTesoro :: Cofre -> Bool
tieneUnSoloTesoro (Cofre obs) = unSoloTesoro obs

unSoloTesoro :: [Objeto] -> Bool
unSoloTesoro [] = False
unSoloTesoro (o:obs) = (o == Tesoro && not (unSoloTesoro obs)) || (not (o == Tesoro) && unSoloTesoro obs )

{-4. caminoDeLaRamaMasLarga :: Mapa -> [Dir]
Indica el camino de la rama más larga.
5. tesorosPorNivel :: Mapa -> [[Objeto]]
Devuelve los tesoros separados por nivel en el árbol.
6. todosLosCaminos :: Mapa -> [[Dir]]
Devuelve todos lo caminos en el mapa.
3. Nave Espacial
modelaremos una Nave como un tipo algebraico, el cual nos permite construir una nave espacial,
dividida en sectores, a los cuales podemos asignar tripulantes y componentes. La representación
es la siguiente:
data Componente = LanzaTorpedos | Motor Int | Almacen [Barril]
data Barril = Comida | Oxigeno | Torpedo | Combustible
data Sector = S SectorId [Componente] [Tripulante]
type SectorId = String
type Tripulante = String
data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
data Nave = N (Tree Sector)
Implementar las siguientes funciones utilizando recursión estructural:
1. sectores :: Nave -> [SectorId]
Propósito: Devuelve todos los sectores de la nave.
2. poderDePropulsion :: Nave -> Int
Propósito: Devuelve la suma de poder de propulsión de todos los motores de la nave. Nota:
el poder de propulsión es el número que acompaña al constructor de motores.
3. barriles :: Nave -> [Barril]
Propósito: Devuelve todos los barriles de la nave.
4. agregarASector :: [Componente] -> SectorId -> Nave -> Nave
Propósito: Añade una lista de componentes a un sector de la nave.
Nota: ese sector puede no existir, en cuyo caso no añade componentes.
5. asignarTripulanteA :: Tripulante -> [SectorId] -> Nave -> Nave
Propósito: Incorpora un tripulante a una lista de sectores de la nave.
Precondición: Todos los id de la lista existen en la nave.
6. sectoresAsignados :: Tripulante -> Nave -> [SectorId]
Propósito: Devuelve los sectores en donde aparece un tripulante dado.
7. tripulantes :: Nave -> [Tripulante]
Propósito: Devuelve la lista de tripulantes, sin elementos repetidos.
4. Manada de lobos
Modelaremos una manada de lobos, como un tipo Manada, que es un simple registro compuesto
de una estructura llamada Lobo, que representa una jerarquía entre estos animales.
Los diferentes casos de lobos que forman la jerarquía son los siguientes:
Los cazadores poseen nombre, una lista de especies de presas cazadas y 3 lobos a cargo.
Los exploradores poseen nombre, una lista de nombres de territorio explorado (nombres de
bosques, ríos, etc.), y poseen 2 lobos a cargo.
Las crías poseen sólo un nombre y no poseen lobos a cargo.
La estructura es la siguiente:
type Presa = String -- nombre de presa
type Territorio = String -- nombre de territorio
type Nombre = String -- nombre de lobo
data Lobo = Cazador Nombre [Presa] Lobo Lobo Lobo
| Explorador Nombre [Territorio] Lobo Lobo
| Cría Nombre
data Manada = M Lobo
1. Construir un valor de tipo Manada que posea 1 cazador, 2 exploradores y que el resto sean
crías. Resolver las siguientes funciones utilizando recursión estructural sobre la estructura
que corresponda en cada caso:
2. buenaCaza :: Manada -> Bool
Propósito: dada una manada, indica si la cantidad de alimento cazado es mayor a la cantidad
de crías.
3. elAlfa :: Manada -> (Nombre, Int)
Propósito: dada una manada, devuelve el nombre del lobo con más presas cazadas, junto
con su cantidad de presas. Nota: se considera que los exploradores y crías tienen cero presas
cazadas, y que podrían formar parte del resultado si es que no existen cazadores con más de
cero presas.
4. losQueExploraron :: Territorio -> Manada -> [Nombre]
Propósito: dado un territorio y una manada, devuelve los nombres de los exploradores que
pasaron por dicho territorio.
5. exploradoresPorTerritorio :: Manada -> [(Territorio, [Nombre])]
Propósito: dada una manada, denota la lista de los pares cuyo primer elemento es un territorio
y cuyo segundo elemento es la lista de los nombres de los exploradores que exploraron
dicho territorio. Los territorios no deben repetirse.
6. cazadoresSuperioresDe :: Nombre -> Manada -> [Nombre]
Propósito: dado el nombre de un lobo y una manada, indica el nombre de todos los cazadores
que tienen como subordinado al lobo dado (puede ser un subordinado directo, o el
subordinado de un subordinado).
Precondición: hay un lobo con dicho nombre y es único.
Suponiendo la siguiente manada de ejemplo:
manadaEj =
Cazador "DienteFiloso" ["Búfalos", "Antílopes"]
(Cría "Hopito")
(Explorador "Incansable" ["Oeste hasta el río"]
(Cría "MechónGris")
(Cría "Rabito")
)
(Cazador "Garras" ["Antílopes", "Ciervos"]
(Explorador "Zarpado" ["Bosque este"]
(Cría "Osado")
(Cazador "Mandíbulas" ["Cerdos", "Pavos"]
(Cría "Desgreñado")
(Cría "Malcriado")
(Cazador "TrituraHuesos" ["Conejos"]
(Cría "Peludo")
(Cría "Largo")
(Cría "Menudo")
)
)
)
(Cría "Garrita")
(Cría "Manchas")
)
la función cazadoresSuperioresDe debería dar lo siguiente:
cazadoresSuperioresDe "Mandíbulas" manadaEj = ["DienteFiloso", "Garras"]
cazadoresSuperioresDe "Rabito" manadaEj = ["DienteFiloso"]
cazadoresSuperioresDe "DienteFiloso" manadaEj = []
cazadoresSuperioresDe "Peludo" manadaEj =
["DienteFiloso", "Garras", "Mandíbulas", "TrituraHuesos"]-}
