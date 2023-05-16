module Soluciones where
import GHC.RTS.Flags (GCFlags(pcFreeHeap))
-- Completar con los datos del grupo
-- Nombre de Grupo: Overflow 'em all
-- Integrante 1: Lucia Silva, lucia.silva.alberto@gmail.com , 209/22
-- Integrante 2: Horacio Garcia Crespo, horaciogarciacr@gmail.com, 203/20
-- Integrante 3: Antonella Manzoni Bascoy, antonellapilar23@yahoo.com, 1603/21
-- Integrante 4: Ludmila Krasnozhon, ludkra2@gmail.com, 252/22
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
usuarioConMasAmigos red = elQueTieneMasAmigos red (usuarios red) (head (usuarios red))


elQueTieneMasAmigos :: RedSocial -> [Usuario] -> Usuario -> Usuario
elQueTieneMasAmigos red (x:xs) n | null xs && cantidadDeAmigos red x > cantidadDeAmigos red n = x
                                 | null xs = n 
                                 | cantidadDeAmigos red x >= cantidadDeAmigos red n = elQueTieneMasAmigos red xs x
                                 | otherwise = elQueTieneMasAmigos red xs n
                 
-- describir qué hace la función: .....
estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos red = if (cantidadDeAmigos red (usuarioConMasAmigos red)) > 10 then True else False

-- describir qué hace la función: .....
publicacionesDe :: RedSocial -> Usuario -> [Publicacion]
publicacionesDe red u = postsDe (publicaciones red) u

postsDe :: [Publicacion] -> Usuario -> [Publicacion]
postsDe [] u = []
postsDe (x:xs) u | usuarioDePublicacion x == u = x : postsDe xs u
                 | otherwise = postsDe xs u

-- describir qué hace la función: .....
publicacionesQueLeGustanA :: RedSocial -> Usuario -> [Publicacion]
publicacionesQueLeGustanA red u = likesDeU (publicaciones red) u

likesDeU :: [Publicacion] -> Usuario -> [Publicacion]
likesDeU [] _ = []
likesDeU (x:xs) u | elem u (likesDePublicacion x) = x : likesDeU xs u 
                  | otherwise = likesDeU xs u 

-- describir qué hace la función: .....
lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
lesGustanLasMismasPublicaciones red a b = if publicacionesQueLeGustanA red a == publicacionesQueLeGustanA red b then True else False

-- describir qué hace la función: .....
tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel red u = esSeguidorFielDeU red u (usuarios red)

esSeguidorFielDeU ::RedSocial -> Usuario -> [Usuario] -> Bool
esSeguidorFielDeU red u [] = False  
esSeguidorFielDeU red u (x:xs) | pertenecePubDeUALosLikesDeU2 (publicacionesDe red u) (publicacionesQueLeGustanA red x) && u /= x = True
                               | otherwise = esSeguidorFielDeU red u xs
                               
pertenecePubDeUALosLikesDeU2 :: [Publicacion] -> [Publicacion] -> Bool
pertenecePubDeUALosLikesDeU2 [] l = False
pertenecePubDeUALosLikesDeU2 (x:xs) l | pertenece x l = True
                                      | otherwise = pertenecePubDeUALosLikesDeU2 xs l

-- describir qué hace la función: .....
existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos red u1 u2 = pertenece u2 (comunidad red [u1] [u1])


comunidad :: RedSocial -> [Usuario] -> [Usuario] -> [Usuario]
comunidad red pc [] = pc 
comunidad red pc (x:xs) = comunidad red ((elementosNoEnComun (amigosDe red x) pc) ++ pc) (xs ++ (elementosNoEnComun (amigosDe red x) pc))


elementosNoEnComun :: [Usuario] -> [Usuario] -> [Usuario]
elementosNoEnComun [] xs = []
elementosNoEnComun (x:xs) ls | pertenece x ls = elementosNoEnComun xs ls 
                             | otherwise = x : elementosNoEnComun xs ls
                 
