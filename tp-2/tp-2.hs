{-Trabajo Prático 2
de la Cerda Dominique-}

-- 1. Recursión sobre listas
{-Defina las siguientes funciones utilizando recursión estructural sobre listas, salvo que se indique
lo contrario:
1. sumatoria :: [Int] -> Int
Dada una lista de enteros devuelve la suma de todos sus elementos.-}
sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs

{-2. longitud :: [a] -> Int
Dada una lista de elementos de algún tipo devuelve el largo de esa lista, es decir, la cantidad
de elementos que posee.-}
longitud :: [a] -> Int
longitud [] = 0
longitud (_:xs) = 1 + longitud xs

{-3. sucesores :: [Int] -> [Int]
Dada una lista de enteros, devuelve la lista de los sucesores de cada entero.-}
sucesores :: [Int] -> [Int]
sucesores [] = []
sucesores (x:xs) = (x + 1) : sucesores xs

{-4. conjuncion :: [Bool] -> Bool
Dada una lista de booleanos devuelve True si todos sus elementos son True.-}
conjuncion :: [Bool] -> Bool
conjuncion [] = True
conjuncion (x:xs) = x && conjuncion xs

{-5. disyuncion :: [Bool] -> Bool
Dada una lista de booleanos devuelve True si alguno de sus elementos es True.-}
disyuncion :: [Bool] -> Bool
disyuncion [] = False
disyuncion (x:xs) = x || disyuncion xs

{-6. aplanar :: [[a]] -> [a]
Dada una lista de listas, devuelve una única lista con todos sus elementos.-}
aplanar :: [[a]] -> [a]
aplanar [] = []
aplanar (x:xs) = x ++ aplanar xs

{-7. pertenece :: Eq a => a -> [a] -> Bool
Dados un elemento e y una lista xs devuelve True si existe un elemento en xs que sea igual a e.-}
pertenece :: Eq a => a -> [a] -> Bool
pertenece _ [] = False 
pertenece e (x:xs) = (x == e) || pertenece e xs

{-8. apariciones :: Eq a => a -> [a] -> Int
Dados un elemento e y una lista xs cuenta la cantidad de apariciones de e en xs.-}
apariciones :: Eq a => a -> [a] -> Int
apariciones _ [] = 0
apariciones e (x:xs) = unoSiEsIgual e x + apariciones e xs

unoSiEsIgual :: Eq a => a -> a -> Int
unoSiEsIgual a b = if a == b
                        then 1
                        else 0

{-9. losMenoresA :: Int -> [Int] -> [Int]
Dados un número n y una lista xs, devuelve todos los elementos de xs que son menores a n.-}
losMenoresA :: Int -> [Int] -> [Int]
losMenoresA _ [] = []
losMenoresA n (x:xs) = if x < n 
                            then x : losMenoresA n xs
                            else losMenoresA n xs

{-10. lasDeLongitudMayorA :: Int -> [[a]] -> [[a]]
Dados un número n y una lista de listas, devuelve la lista de aquellas listas que tienen más
de n elementos.-}
lasDeLongitudMayorA :: Int -> [[a]] -> [[a]]
lasDeLongitudMayorA _ [] = []
lasDeLongitudMayorA n (x:xs) = if longitud x > n
                                    then x : lasDeLongitudMayorA n xs
                                    else lasDeLongitudMayorA n xs

{-11. agregarAlFinal :: [a] -> a -> [a]
Dados una lista y un elemento, devuelve una lista con ese elemento agregado al final de la
lista.-}
agregarAlFinal :: [a] -> a -> [a]
agregarAlFinal [] e = [e]
agregarAlFinal (x:xs) e = x : agregarAlFinal xs e

{-12. agregar :: [a] -> [a] -> [a]
Dadas dos listas devuelve la lista con todos los elementos de la primera lista y todos los
elementos de la segunda a continuación. Definida en Haskell como (++).-}
agregar :: [a] -> [a] -> [a]
agregar [] ys = ys
agregar (x:xs) ys = x :  agregar xs ys

{-13. reversa :: [a] -> [a]
Dada una lista devuelve la lista con los mismos elementos de atrás para adelante. Definida
en Haskell como reverse.-}
reversa :: [a] -> [a]
reversa [] = []
reversa (x:xs) = reversa xs ++ [x]

{-14. zipMaximos :: [Int] -> [Int] -> [Int]
Dadas dos listas de enteros, devuelve una lista donde el elemento en la posición n es el
máximo entre el elemento n de la primera lista y de la segunda lista, teniendo en cuenta que
las listas no necesariamente tienen la misma longitud.-}
zipMaximos :: [Int] -> [Int] -> [Int]
zipMaximos [] ys = ys
zipMaximos xs [] = xs
zipMaximos (x:xs) (y:ys) = if x > y
                                then x : zipMaximos xs ys
                                else y : zipMaximos xs ys

{-15. elMinimo :: Ord a => [a] -> a
Dada una lista devuelve el mínimo-}
elMinimo :: Ord a => [a] -> a
-- Precondición: La lista no puede ser vacia
elMinimo [] = error "La lista está vacia"
elMinimo (x:xs) = if x < elMinimo xs
                        then x
                        else elMinimo xs

--2. Recursión sobre números
{-Defina las siguientes funciones utilizando recursión sobre números enteros, salvo que se indique
lo contrario:
1. factorial :: Int -> Int
Dado un número n se devuelve la multiplicación de este número y todos sus anteriores hasta
llegar a 0. Si n es 0 devuelve 1. La función es parcial si n es negativo.-}
factorial :: Int -> Int
-- Precondicción: n No puede ser negativo 
factorial 0 = 1
factorial n = n * factorial (n - 1)

{-2. cuentaRegresiva :: Int -> [Int]
Dado un número n devuelve una lista cuyos elementos sean los números comprendidos entre
n y 1 (incluidos). Si el número es inferior a 1, devuelve la lista vacía.-}
cuentaRegresiva :: Int -> [Int]
cuentaRegresiva 0 = []
cuentaRegresiva n = n : cuentaRegresiva (n - 1)

{-3. repetir :: Int -> a -> [a]
Dado un número n y un elemento e devuelve una lista en la que el elemento e repite n veces.-}
repetir :: Int -> a -> [a]
repetir 0 _ = [] 
repetir n e = e : repetir (n - 1) e

{-4. losPrimeros :: Int -> [a] -> [a]
Dados un número n y una lista xs, devuelve una lista con los n primeros elementos de xs.
Si la lista es vacía, devuelve una lista vacía.-}
losPrimeros :: Int -> [a] -> [a]
losPrimeros _ [] = []
losPrimeros 0 _ = []
losPrimeros n (x:xs) = x : losPrimeros (n - 1) xs

{-5. sinLosPrimeros :: Int -> [a] -> [a]
Dados un número n y una lista xs, devuelve una lista sin los primeros n elementos de lista
recibida. Si n es cero, devuelve la lista completa.-}
sinLosPrimeros :: Int -> [a] -> [a]
sinLosPrimeros _ [] = []
sinLosPrimeros 0 xs = xs
sinLosPrimeros n (x:xs) = sinLosPrimeros (n - 1) xs 

--3. Registros
{-1. Definir el tipo de dato Persona, como un nombre y la edad de la persona. Realizar las
siguientes funciones:-}
data Persona = P String Int
--               Nombre Edad
 deriving Show

{-mayoresA :: Int -> [Persona] -> [Persona]
Dados una edad y una lista de personas devuelve a las personas mayores a esa edad.-}
mayoresA :: Int -> [Persona] -> [Persona]
mayoresA n [] = []
mayoresA n (x:xs) = if n > edad x
                        then x : mayoresA n xs
                        else mayoresA n xs

edad :: Persona -> Int
edad (P _ e) = e

{-promedioEdad :: [Persona] -> Int
Dada una lista de personas devuelve el promedio de edad entre esas personas. Precondición: la lista al menos posee una persona.-}
promedioEdad :: [Persona] -> Int
-- Precondicion: La lista no puede estar vacia
promedioEdad [] = error "La lista está vacia"
promedioEdad xs = div (edadesSumadas xs) (longitud xs)

edadesSumadas :: [Persona] -> Int
edadesSumadas [] = 0
edadesSumadas (x:xs) = edad x + edadesSumadas xs

{-elMasViejo :: [Persona] -> Persona
Dada una lista de personas devuelve la persona más vieja de la lista. Precondición: la
lista al menos posee una persona.-}
elMasViejo :: [Persona] -> Persona
-- Precondicion: La lista no puede estar vacia
elMasViejo [] = error "La lista está vacia"
elMasViejo (x:xs) = if edad x > edad (elMasViejo xs)
                        then x
                        else elMasViejo xs

{-2. Modificaremos la representación de Entreador y Pokemon de la práctica anterior de la siguiente manera:-}
data TipoDePokemon = Agua | Fuego | Planta
 deriving Show

data Pokemon = Po TipoDePokemon Int
--                TipoDePokemon Energía
 deriving Show

data Entrenador = E String [Pokemon]
--                  Nombre Pokemones 
 deriving Show

{-Como puede observarse, ahora los entrenadores tienen una cantidad de Pokemon arbitraria.
Definir en base a esa representación las siguientes funciones:
cantPokemon :: Entrenador -> Int
Devuelve la cantidad de Pokémon que posee el entrenador.-}
cantPokemon :: Entrenador -> Int
cantPokemon (E _ ps) = longitud ps

{-cantPokemonDe :: TipoDePokemon -> Entrenador -> Int
Devuelve la cantidad de Pokémon de determinado tipo que posee el entrenador.-}
cantPokemonDe :: TipoDePokemon -> Entrenador -> Int
cantPokemonDe t (E _ ps) = cantidadDePokesDe t ps

cantidadDePokesDe :: TipoDePokemon -> [Pokemon] -> Int
cantidadDePokesDe t [] = 0
cantidadDePokesDe t (p:ps) = unoSiCoinciden t (tipo p) + cantidadDePokesDe t ps  

unoSiCoinciden :: TipoDePokemon -> TipoDePokemon -> Int
unoSiCoinciden Agua Agua = 1
unoSiCoinciden Fuego Fuego = 1
unoSiCoinciden Planta Planta = 1
unoSiCoinciden _ _ = 0

tipo :: Pokemon -> TipoDePokemon
tipo (Po p _) = p

{-cuantosDeTipo_De_LeGananATodosLosDe_ :: TipoDePokemon -> Entrenador -> Entrenador -> Int
Dados dos entrenadores, indica la cantidad de Pokemon de cierto tipo pertenecientes al
primer entrenador, que le ganarían a todos los Pokemon del segundo entrenador.-}
cuantosDeTipo_De_LeGananATodosLosDe_ :: TipoDePokemon -> Entrenador -> Entrenador -> Int
cuantosDeTipo_De_LeGananATodosLosDe_ t e1 e2 = cantDe_QueLeGananATodosLosDe_ (pokemonesDeTipo t (pokemones e1)) (pokemones e2)

cantDe_QueLeGananATodosLosDe_ :: [Pokemon] -> [Pokemon] -> Int
cantDe_QueLeGananATodosLosDe_ [] ys = 0
cantDe_QueLeGananATodosLosDe_ ps [] = length ps
cantDe_QueLeGananATodosLosDe_ (p:ps) ys = if superaATodos p ys
                                            then 1 + cantDe_QueLeGananATodosLosDe_ ps ys
                                            else cantDe_QueLeGananATodosLosDe_ ps ys

pokemones :: Entrenador -> [Pokemon]
pokemones (E _ ps) = ps

pokemonesDeTipo :: TipoDePokemon -> [Pokemon] -> [Pokemon]
pokemonesDeTipo t [] = []
pokemonesDeTipo t (p:ps) = if esMismoTipo t (tipo p)
                                then p : pokemonesDeTipo t ps
                                else pokemonesDeTipo t ps

esMismoTipo :: TipoDePokemon -> TipoDePokemon -> Bool
esMismoTipo Agua Agua = True
esMismoTipo Fuego Fuego = True
esMismoTipo Planta Planta = True
esMismoTipo _ _ = False

superaATodos :: Pokemon -> [Pokemon] -> Bool
superaATodos p [] = False
superaATodos p (y:ys) = superaA p y && superaATodos p ys

superaA :: Pokemon -> Pokemon -> Bool
superaA p1 p2 = superaTipo (tipo p1) (tipo p2)

superaTipo :: TipoDePokemon -> TipoDePokemon -> Bool
superaTipo Agua Fuego = True
superaTipo Fuego Planta = True
superaTipo Planta Agua = True
superaTipo _ _ = False

{-esMaestroPokemon :: Entrenador -> Bool
Dado un entrenador, devuelve True si posee al menos un Pokémon de cada tipo posible.-}
esMaestroPokemon :: Entrenador -> Bool
esMaestroPokemon (E _ ps) = hayAlMenosUn_ Agua ps && hayAlMenosUn_ Fuego ps && hayAlMenosUn_ Planta ps

hayAlMenosUn_ :: TipoDePokemon -> [Pokemon] -> Bool
hayAlMenosUn_ t (p:ps) = esMismoTipo t (tipo p) || hayAlMenosUn_ t ps

{-3. El tipo de dato Rol representa los roles (desarollo o management) de empleados IT dentro
de una empresa de software, junto al proyecto en el que se encuentran. Así, una empresa es
una lista de personas con diferente rol. La definición es la siguiente:-}
data Seniority = Junior | SemiSenior | Senior
 deriving Show

data Proyecto = Pr String
 deriving (Show, Eq)

data Rol = Developer Seniority Proyecto | Management Seniority Proyecto
 deriving Show

data Empresa = Em [Rol]
 deriving Show

{-Definir las siguientes funciones sobre el tipo Empresa:
proyectos :: Empresa -> [Proyecto]
Dada una empresa denota la lista de proyectos en los que trabaja, sin elementos repetidos.-}
proyectos :: Empresa -> [Proyecto]
proyectos (Em rs) = proyectosDe rs

proyectosDe :: [Rol] -> [Proyecto]
proyectosDe [] = []
proyectosDe (r:rs) = proyecto r : proyectosDe rs

proyecto :: Rol -> Proyecto
proyecto (Developer _ p) = p 
proyecto (Management _ p) = p 

{-losDevSenior :: Empresa -> [Proyecto] -> Int
Dada una empresa indica la cantidad de desarrolladores senior que posee, que pertecen
además a los proyectos dados por parámetro.-}
losDevSenior :: Empresa -> [Proyecto] -> Int
losDevSenior (Em rs) [] = 0
losDevSenior (Em rs) ps = losDe_QuePertenecenA (losDevSeniorDe rs) ps

losDe_QuePertenecenA :: [Rol] -> [Proyecto] -> Int
losDe_QuePertenecenA [] _ = 0
losDe_QuePertenecenA (r:rs) ps = if elem (proyecto r) ps
                                    then 1 + losDe_QuePertenecenA rs ps
                                    else losDe_QuePertenecenA rs ps

losDevSeniorDe :: [Rol] -> [Rol]
losDevSeniorDe [] = []
losDevSeniorDe (r:rs) = if esDevSenior r 
                            then r : losDevSeniorDe rs
                            else losDevSeniorDe rs

esDevSenior :: Rol -> Bool
esDevSenior (Developer Senior _) = True
esDevSenior _ = False

{-cantQueTrabajanEn :: [Proyecto] -> Empresa -> Int
Indica la cantidad de empleados que trabajan en alguno de los proyectos dados.-}
cantQueTrabajanEn :: [Proyecto] -> Empresa -> Int
cantQueTrabajanEn ps (E rs) = losDe_QuePertenecenA rs ps

{-asignadosPorProyecto :: Empresa -> [(Proyecto, Int)]
Devuelve una lista de pares que representa a los proyectos (sin repetir) junto con su
cantidad de personas involucradas.-}
asignadosPorProyecto :: Empresa -> [(Proyecto, Int)]
asignadosPorProyecto (E rs) = asignadosPorProyectoDe_