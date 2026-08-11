{-Trabajo Prático 1
de la Cerda Dominique-}

-- 2. Números enteros --
{-1. Defina las siguientes funciones:
a) sucesor :: Int -> Int
Dado un número devuelve su sucesor-}
sucesor :: Int -> Int
sucesor x = x + 1

{-b) sumar :: Int -> Int -> Int
Dados dos números devuelve su suma utilizando la operación +.-}
sumar :: Int -> Int -> Int
sumar x y = x + y

{-c) divisionYResto :: Int -> Int -> (Int, Int)
Dado dos números, devuelve un par donde la primera componente es la división del
primero por el segundo, y la segunda componente es el resto de dicha división. Nota:
para obtener el resto de la división utilizar la función mod :: Int -> Int -> Int,
provista por Haskell.-}
divisionYResto :: Int -> Int -> (Int, Int)
divisionYResto x y = (div x y, mod x y)


{-d) maxDelPar :: (Int,Int) -> Int
Dado un par de números devuelve el mayor de estos.-}
maxDelPar :: (Int,Int) -> Int
maxDelPar (x,y) = if x > y 
                    then x
                    else y

{-2. De 4 ejemplos de expresiones diferentes que denoten el número 10, utilizando en cada expresión a todas las funciones del punto anterior.
Ejemplo: maxDePar (divisionYResto (suma 5 5) (sucesor 0))-}

--3. Tipos enumerativos--
{-1. Definir el tipo de dato Dir, con las alternativas Norte, Sur, Este y Oeste. Luego implementar
las siguientes funciones:-}
data Dir = Norte | Sur | Este | Oeste
 deriving Show

{-a) opuesto :: Dir -> Dir
Dada una dirección devuelve su opuesta.-}
opuesto :: Dir -> Dir
opuesto Norte = Sur
opuesto Sur = Norte
opuesto Este = Oeste
opuesto Oeste = Este

{-b) iguales :: Dir -> Dir -> Bool
Dadas dos direcciones, indica si son la misma. Nota: utilizar pattern matching y no ==.-}
iguales :: Dir -> Dir -> Bool
iguales Norte Norte = True
iguales Sur Sur = True
iguales Oeste Oeste = True
iguales Este Este = True
iguales _ _ = False

{-c) siguiente :: Dir -> Dir
Dada una dirección devuelve su siguiente, en sentido horario, y suponiendo que no existe
la siguiente dirección a Oeste. 
¿Posee una precondición esta función? Sí
¿Es una función total o parcial? Parcial
¿Por qué? Porque en caso de que se escriba siguiente Oeste, falla ya que no tener siguiente-}
siguiente :: Dir -> Dir
--Precondición: En este modelo, Oeste no tiene siguiente.
siguiente Norte = Este
siguiente Este = Sur
siguiente Sur = Oeste

{-2. Definir el tipo de dato DiaDeSemana, con las alternativas Lunes, Martes, Miércoles, Jueves,
Viernes, Sabado y Domingo. Supongamos que el primer día de la semana es lunes, y el último
es domingo. Luego implementar las siguientes funciones:-}
data DiaDeSemana = Lunes | Martes | Miercoles | Jueves | Viernes | Sabado | Domingo
 deriving Show 

{-a) primeroYUltimoDia :: (DiaDeSemana, DiaDeSemana)
Devuelve un par donde la primera componente es el primer día de la semana, y la
segunda componente es el último día de la semana. Considerar definir subtareas útiles
que puedan servir después.-}
primeroYUltimoDia :: (DiaDeSemana, DiaDeSemana)
primeroYUltimoDia = (primerDia, ultimoDia)

primerDia :: DiaDeSemana
primerDia = Lunes

ultimoDia :: DiaDeSemana
ultimoDia = Domingo

{-b) empiezaConM :: DiaDeSemana -> Bool
Dado un día de la semana indica si comienza con la letra M.-}
empiezaConM :: DiaDeSemana -> Bool
empiezaConM Martes = True
empiezaConM Miercoles = True
empiezaConM _ = False

{-c) vieneDespues :: DiaDeSemana -> DiaDeSemana -> Bool
Dado dos días de semana, indica si el primero viene después que el segundo. Analizar
la calidad de la solución respecto de la cantidad de casos analizados (entre los casos
analizados en esta y cualquier subtarea, deberían ser no más de 9 casos).
Ejemplo: vieneDespues Jueves Lunes = True-}
vieneDespues :: DiaDeSemana -> DiaDeSemana -> Bool
vieneDespues dia1 dia2 = (numeroAsignado dia1) < numeroAsignado dia2

numeroAsignado :: DiaDeSemana -> Int
numeroAsignado Lunes = 0
numeroAsignado Martes = 1
numeroAsignado Miercoles = 2
numeroAsignado Jueves = 3
numeroAsignado Viernes = 4
numeroAsignado Sabado = 5
numeroAsignado Domingo = 6

{-d) estaEnElMedio :: DiaDeSemana -> Bool
Dado un día de la semana indica si no es ni el primer ni el ultimo dia.-}
estaEnElMedio :: DiaDeSemana -> Bool
estaEnElMedio Lunes = False
estaEnElMedio Domingo = False
estaEnElMedio _ = True

{-3. Los booleanos también son un tipo de enumerativo. Un booleano es True o False. Defina
las siguientes funciones utilizando pattern matching (no usar las funciones sobre booleanos
ya definidas en Haskell):
a) negar :: Bool -> Bool
Dado un booleano, si es True devuelve False, y si es False devuelve True.
En Haskell ya está definida como not.-}
negar :: Bool -> Bool
negar True = False
negar False = True

{-b) implica :: Bool -> Bool -> Bool
Dados dos booleanos, si el primero es True y el segundo es False, devuelve False, sino
devuelve True.
Esta función NO debe realizar doble pattern matching.
Nota: no viene implementada en Haskell.-}
implica :: Bool -> Bool -> Bool
implica True b = b
implica _ _ = True

{-c) yTambien :: Bool -> Bool -> Bool
Dados dos booleanos si ambos son True devuelve True, sino devuelve False.
Esta función NO debe realizar doble pattern matching.
En Haskell ya está definida como \&\&.-}
yTambien :: Bool -> Bool -> Bool
yTambien True b = b
yTambien _ _ = False

{-d) oBien :: Bool -> Bool -> Bool
Dados dos booleanos si alguno de ellos es True devuelve True, sino devuelve False.
Esta función NO debe realizar doble pattern matching.
En Haskell ya está definida como ||.-}
oBien :: Bool -> Bool -> Bool
oBien True _ = True
oBien _ b = b

{-4. Registros
1. Definir el tipo de dato Persona, como un nombre y la edad de la persona. Realizar las
siguientes funciones:-}
data Persona = P String Int
            --   Nombre | Edad
 deriving Show

{-nombre :: Persona -> String
Devuelve el nombre de una persona-}
nombre :: Persona -> String
nombre (P n _) = n

{-edad :: Persona -> Int
Devuelve la edad de una persona-}
edad :: Persona -> String
edad (P _ e) = e

{-crecer :: Persona -> Persona
Aumenta en uno la edad de la persona.-}
crecer :: Persona -> Persona
crecer (P n e) = (P n (e + 1))

{-cambioDeNombre :: String -> Persona -> Persona
Dados un nombre y una persona, devuelve una persona con la edad de la persona y el
nuevo nombre.-}
cambioDeNombre :: String -> Persona -> Persona
cambioDeNombre m (P n e) = (P m e)

{-esMayorQueLaOtra :: Persona -> Persona -> Bool
Dadas dos personas indica si la primera es mayor que la segunda.-}
esMayorQueLaOtra :: Persona -> Persona -> Bool
esMayorQueLaOtra (P _ e1) (P _ e2) = e1 > e2

{-laQueEsMayor :: Persona -> Persona -> Persona
Dadas dos personas devuelve a la persona que sea mayor.-}
laQueEsMayor :: Persona -> Persona -> Persona
laQueEsMayor p1 p2 = if esMayorQueLaOtra p1 p2
                        then p1
                        else p2

{-2. Definir los tipos de datos Pokemon, como un TipoDePokemon (agua, fuego o planta) y un
porcentaje de energía; y Entrenador, como un nombre y dos Pokémon. Luego definir las
siguientes funciones:-}
data Pokemon = Po TipoDePokemon Int 
                -- TipoDePokemon Porcentaje de energía
 deriving Show

data Entrenador = E String Pokemon Pokemon
                --  Nombre Pokemon1 Pokemon2
 deriving Show

data TipoDePokemon = Agua | Fuego | Tierra

{-superaA :: Pokemon -> Pokemon -> Bool
Dados dos Pokémon indica si el primero, en base al tipo, es superior al segundo. Agua
supera a fuego, fuego a planta y planta a agua. Y cualquier otro caso es falso.-}
superaA :: Pokemon -> Pokemon -> Bool
superaA p1 p2 = superaTipo (tipo p1) (tipo p2)

superaTipo :: TipoDePokemon -> TipoDePokemon -> Bool



{-cantidadDePokemonDe :: TipoDePokemon -> Entrenador -> Int
Devuelve la cantidad de Pokémon de determinado tipo que posee el entrenador.
juntarPokemon :: (Entrenador, Entrenador) -> [Pokemon]
Dado un par de entrenadores, devuelve a sus Pokémon en una lista.-}
