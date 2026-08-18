{-Trabajo Prático 3
de la Cerda Dominique-}

--1. Tipos recursivos simples
{-1.1. Celdas con b olitas
Representaremos una celda con b olitas de colores ro jas y azules, de la siguiente manera:-}

data Color = Azul | Rojo
 deriving Show

data Celda = Bolita Color Celda | CeldaVacia
 deriving Show

{-En dicha representación, la cantidad de apariciones de un determinado color denota la cantidad
de bolitas de ese color en la celda. Por ejemplo, una celda con 2 b olitas azules y 2 ro jas, p o dría
ser la siguiente:
Bolita Rojo (Bolita Azul (Bolita Rojo (Bolita Azul CeldaVacia)))
Implementar las siguientes funciones sobre celdas:
nroBolitas :: Color -> Celda -> Int
Dados un color y una celda, indica la cantidad de b olitas de ese color. Nota: p ensar si ya
existe una op eración sobre listas que ayude a resolver el problema.-}
nroBolitas :: Color -> Celda -> Int
nroBolitas 

{-poner :: Color -> Celda -> Celda
Dado un color y una celda, agrega una b olita de dicho color a la celda.
sacar :: Color -> Celda -> Celda
Dado un color y una celda, quita una b olita de dicho color de la celda. Nota: a diferencia de
Gobstones, esta función es total.
ponerN :: Int -> Color -> Celda -> Celda
Dado un número n, un color c, y una celda, agrega n b olitas de color c a la celda.-}
