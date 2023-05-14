module Pruebas where
import Soluciones

usuarioValido :: Usuario -> Bool
usuarioValido u | idDeUsuario u > 0 && length (nombreDeUsuario u ) > 0 = True
                | otherwise = False

noHayIdsRepetidos :: [Usuario] -> Bool
noHayIdsRepetidos [] = True
noHayIdsRepetidos (x:xs) | elem (idDeUsuario x) (listaDeIds xs) = False
                         | otherwise = noHayIdsRepetidos xs

listaDeIds :: [Usuario] -> [Integer]
listaDeIds [] = []
listaDeIds (x:xs) = idDeUsuario x : listaDeIds xs

usuariosValidos :: [Usuario] -> Bool
usuariosValidos [] = True
usuariosValidos (x:xs) | not (usuarioValido x) ||  not (noHayIdsRepetidos (x:xs)) = False
                       | otherwise = usuariosValidos xs

