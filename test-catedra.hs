module Test where
    import Soluciones

    import Test.HUnit
    --import Solucion descomentar al terminar

    main = runTestTT tests

    tests = test [
        " nombresDeUsuarios 1" ~: (nombresDeUsuarios redA) ~?= ["Juan","Natalia","Pedro","Mariela"],

        " amigosDe 1" ~: (amigosDe redA usuario1) ~?= [usuario2, usuario4],

        " cantidadDeAmigos 1" ~: (cantidadDeAmigos redA usuario1) ~?= 2,

        " usuarioConMasAmigos 1" ~: expectAny (usuarioConMasAmigos redA) [usuario2, usuario4],

        " estaRobertoCarlos 1" ~: (estaRobertoCarlos redA) ~?= False,

        " publicacionesDe 1" ~: (publicacionesDe redA usuario2) ~?= [publicacion2_1, publicacion2_2],

        " publicacionesQueLeGustanA 1" ~: (publicacionesQueLeGustanA redA usuario1) ~?= [publicacion2_2, publicacion4_1],

        " lesGustanLasMismasPublicaciones 2" ~: (lesGustanLasMismasPublicaciones redB usuario1 usuario3) ~?= True,

        " tieneUnSeguidorFiel 1" ~: (tieneUnSeguidorFiel redA usuario1) ~?= True,

        " existeSecuenciaDeAmigos 1" ~: (existeSecuenciaDeAmigos redA usuario1 usuario3) ~?= True
        ]

    expectAny actual expected = elem actual expected ~? ("expected any of: " ++ show expected ++ "\n but got: " ++ show actual)

    -- Ejemplos

    usuario1 = (1, "Juan")
    usuario2 = (2, "Natalia")
    usuario3 = (3, "Pedro")
    usuario4 = (4, "Mariela")
    usuario5 = (5, "Natalia")

    relacion1_2 = (usuario1, usuario2)
    relacion1_3 = (usuario1, usuario3)
    relacion1_4 = (usuario4, usuario1) -- Notar que el orden en el que aparecen los usuarios es indistinto
    relacion2_3 = (usuario3, usuario2)
    relacion2_4 = (usuario2, usuario4)
    relacion3_4 = (usuario4, usuario3)

    publicacion1_1 = (usuario1, "Este es mi primer post", [usuario2, usuario4])
    publicacion1_2 = (usuario1, "Este es mi segundo post", [usuario4])
    publicacion1_3 = (usuario1, "Este es mi tercer post", [usuario2, usuario5])
    publicacion1_4 = (usuario1, "Este es mi cuarto post", [])
    publicacion1_5 = (usuario1, "Este es como mi quinto post", [usuario5])

    publicacion2_1 = (usuario2, "Hello World", [usuario4])
    publicacion2_2 = (usuario2, "Good Bye World", [usuario1, usuario4])

    publicacion3_1 = (usuario3, "Lorem Ipsum", [])
    publicacion3_2 = (usuario3, "dolor sit amet", [usuario2])
    publicacion3_3 = (usuario3, "consectetur adipiscing elit", [usuario2, usuario5])

    publicacion4_1 = (usuario4, "I am Alice. Not", [usuario1, usuario2])
    publicacion4_2 = (usuario4, "I am Bob", [])
    publicacion4_3 = (usuario4, "Just kidding, i am Mariela", [usuario1, usuario3])


    usuariosA = [usuario1, usuario2, usuario3, usuario4]
    relacionesA = [relacion1_2, relacion1_4, relacion2_3, relacion2_4, relacion3_4]
    publicacionesA = [publicacion1_1, publicacion1_2, publicacion2_1, publicacion2_2, publicacion3_1, publicacion3_2, publicacion4_1, publicacion4_2]
    redA = (usuariosA, relacionesA, publicacionesA)

    usuariosB = [usuario1, usuario2, usuario3, usuario5]
    relacionesB = [relacion1_2, relacion2_3]
    publicacionesB = [publicacion1_3, publicacion1_4, publicacion1_5, publicacion3_1, publicacion3_2, publicacion3_3]
    redB = (usuariosB, relacionesB, publicacionesB)

-- testsuits, test cases , usuarios, relaciones y publicaciones creadas por el grupo
    pruebasNuestras = runTestTT testsuit1 -- hay que poner todo en main ojo!!
    testsuit1 = test [
        " Ej1: dos usuarios tienen el mismo nombre" ~: (nombresDeUsuarios red1) ~?= ["horax64","luloide","antobascoy","mila"],
        " Ej1: todos los usuarios tienen distinto nombre" ~: (nombresDeUsuarios red1_2) ~?= ["horax64","luloide","antobascoy","mila"],
        " Ej1: caso en el cual la red no tiene usuarios" ~: (nombresDeUsuarios red_vacia) ~?= []
        ]  
    testsuit2 = test [
        " Ej2: usuario u no tiene amigos" ~: (amigosDe red2 usuario8) ~?= [],
        " Ej2: usuario u tiene mas de un amigo " ~: (amigosDe red2 usuario6) ~?= [(7,"luloide"),(9,"mila")],
        " Ej2: usuario u tiene un solo amigo"  ~: (amigosDe red2_1 usuario8) ~?= [(11,"robertcharles")]
        ]
--usuarios
-- ([(1,"horax64"),(2,"luloide"),(3,"antobascoy"),(4,"mila")],[((4,"mila"),(3,"antobascoy")),((2,"luloide"),(3,"antobascoy")),((1,"horax64"),(2,"luloide"))],[((1,"horax64"),"somos todos montiel",[(1,"horax64"),(2,"luloide")]),((1,"horax64"),"bokita el mas grande",[(2,"antobascoy"),(2,"luloide")])])
    usuario6 = (6, "horax64")
    usuario7 = (7, "luloide")
    usuario8 = (8, "antobascoy")
    usuario9 = (9, "mila")
    usuario10 = (10, "horax64") -- horax se registro 2 veces
    usuario11 = (11,"robertcharles")
    usuario12 = (12,"fede")

    --relaciones
    relacion6_7 = (usuario6, usuario7)
    relacion8_9 = (usuario8, usuario9)
    relacion6_8 = (usuario6, usuario8)
    relacion7_8 = (usuario7, usuario8)
    relacion7_9 = (usuario7, usuario9)
    relacion6_9 = (usuario6, usuario9)
    relacion8_11 = (usuario8, usuario11)
    relacion9_12 = (usuario9, usuario12)

    --publicaciones
    publicacion6_1 = (usuario6, "somos todos montiel", [usuario6, usuario7])
    publicacion6_2 = (usuario6, "bokita el mas grande", [usuario7, usuario8])

    --redes
    --redes ejercicio 1
    usuarios1 = [usuario6, usuario7, usuario8, usuario9, usuario10]
    relaciones1 = [relacion6_7, relacion8_9, relacion7_8]
    publicaciones1 = [publicacion6_1, publicacion6_2]
    red_vacia = ([],[],[])
    red1 = (usuarios1,relaciones1,publicaciones1)
    usuarios1_2 = [usuario6, usuario7, usuario8, usuario9]
    red1_2 = (usuarios1_2, relaciones1, publicaciones1)
    -- redes ejercicio 2
    usuarios2 = [usuario6, usuario7, usuario8, usuario9,usuario11]
    relaciones2 = [relacion6_7,relacion6_9,relacion7_9]
    relaciones2_1 = [relacion6_7,relacion6_9,relacion7_9, relacion8_11]
    red2 = (usuarios2,relaciones2, publicaciones1)
    red2_1 = (usuarios2,relaciones2_1, publicaciones1)

    --Pruebas ej 10
    relaciones10 = [relacion6_7,relacion6_8,relacion7_8,relacion7_9,relacion9_12]
    usuarios10 = [usuario6, usuario7,usuario8, usuario9, usuario12]
    red10 = (usuarios10, relaciones10, publicaciones1)

    relaciones10_2 = [relacion6_7, relacion6_8 , relacion7_8, relacion9_12]
    red10_2 = (usuarios10, relaciones10_2, publicaciones1)
    relaciones10_3 = []
    red10_3 = (usuarios10, relaciones10_3, publicaciones1)