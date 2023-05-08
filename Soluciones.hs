module Soluciones where
-- Completar con los datos del grupo
-- Nombre de Grupo: Overflow 'em all
-- Integrante 1: Lucia Silva, lucia.silva.alberto@gmail.com , 209/22
-- Integrante 2: Horacio Garcia Crespo, horaciogarciacr@gmail.com, 203/20
-- Integrante 3: Antonella Manzoni Bascoy, antonellapilar23@yahoo.com, 1603/21
-- Integrante 4: Ludmila Krasnozhon, ludkra2@gmail.com, 252/22
{-# OPTIONS_GHC -Wno-incomplete-patterns #-} -- borrar esto antes de entregar
import System.Posix (UserID)
import Data.Time.Format.ISO8601 (yearFormat)
type Usuario = (Integer, String) -- (id, nombre)
type Relacion = (Usuario, Usuario) -- usuarios que se relacionan
type Publicacion = (Usuario, String, [Usuario]) -- (usuario que publica, texto publicacion, likes)
type RedSocial = ([Usuario], [Relacion], [Publicacion])

-- Funciones basicas

usuarios :: RedSocial -> [Usuario]
usuarios (us, _, _) = us

relaciones :: RedSocial -> [Relacion]
relaciones (_, rs, _) = rs

publicaciones :: RedSocial -> [Publicacion]
publicaciones (_, _, ps) = ps

idDeUsuario :: Usuario -> Integer
idDeUsuario (id, _) = id 

nombreDeUsuario :: Usuario -> String
nombreDeUsuario (_, nombre) = nombre 

usuarioDePublicacion :: Publicacion -> Usuario
usuarioDePublicacion (u, _, _) = u

likesDePublicacion :: Publicacion -> [Usuario]
likesDePublicacion (_, _, us) = us

-- Ejercicios:

--Ejercicio 1 

nombresDeUsuarios :: RedSocial -> [String]
nombresDeUsuarios red = eliminarRepetidos ( proyectarNombres (usuarios red))

--Funciones auxiliares 
proyectarNombres:: [Usuario] -> [String]
proyectarNombres [] = []
proyectarNombres (x:xs) = nombreDeUsuario x : proyectarNombres xs


sinRepetidos :: (Eq t) => [t] -> Bool
sinRepetidos [] = True
sinRepetidos (x:xs) | pertenece x xs = False
                    | otherwise = sinRepetidos xs


eliminarRepetidos :: (Eq t) => [t] -> [t]
eliminarRepetidos [] = []
eliminarRepetidos (x:xs) | pertenece x xs = [x] ++ quitarTodos x (eliminarRepetidos xs)
                         | otherwise = [x] ++ eliminarRepetidos xs

quitarTodos :: (Eq t) => t -> [t] -> [t]
quitarTodos x xs | xs == [] = []
            | x == head xs = [] ++ quitarTodos x (tail xs)
            | otherwise = [head xs] ++ quitarTodos x (tail xs)

pertenece :: (Eq t) => t -> [t] -> Bool
pertenece e l | longitud l == 0 = False
              | e == head l = True
              | otherwise = pertenece e (tail l)
   
longitud :: [t] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs


--Ejercicio 2 

amigosDe :: RedSocial -> Usuario -> [Usuario]
amigosDe red n = amigos n (relaciones red)

--FUncion auxiliar
amigos :: Usuario -> [Relacion] -> [Usuario]
amigos _ [] = []
amigos n (x:xs) | n == fst x = [snd x] ++ amigos n xs 
                 | n == snd x = [fst x] ++ amigos n xs 
                 | otherwise = amigos n xs

--Ejercicio 3 

cantidadDeAmigos :: RedSocial -> Usuario -> Int
cantidadDeAmigos red n = longitud (amigosDe red n)


-- describir qué hace la función: .....
usuarioConMasAmigos :: RedSocial -> Usuario
usuarioConMasAmigos = undefined

-- describir qué hace la función: .....
estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos = undefined

-- describir qué hace la función: .....
publicacionesDe :: RedSocial -> Usuario -> [Publicacion]
publicacionesDe = undefined

-- describir qué hace la función: .....
publicacionesQueLeGustanA :: RedSocial -> Usuario -> [Publicacion]
publicacionesQueLeGustanA = undefined

-- describir qué hace la función: .....
lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
lesGustanLasMismasPublicaciones = undefined

-- describir qué hace la función: .....
tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel = undefined

-- describir qué hace la función: .....
existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos = undefined

-- ([(1,"horax64"),(2,"luloide"),(3,"antobascoy"),(4,"mila")],[((4,"mila"),(3,"antobascoy")),((2,"luloide"),(3,"antobascoy")),((1,"horax64"),(2,"luloide"))],[((1,"horax64"),"somos todos montiel",[(1,"horax64"),(2,"luloide")])])
