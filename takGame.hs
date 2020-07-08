import Data.Maybe (fromJust, listToMaybe)
import Data.List (elemIndex)
import Data.List
import System.Random (randomRIO)
import Data.Char

type TakGame = ([Casillero], TakPlayer)

type Casillero = ([Char], (Integer, Integer))

type Camino = [(Integer, Integer)]

data Direccion = Arriba | Abajo | Izquierda | Derecha deriving(Show)

data TakAction = Insertar (Integer, Integer) Bool | Mover (Integer, Integer) (Integer, Integer) | Desapilar (Integer, Integer) [Integer] Direccion
               
-- bool insertar True = Pared
data TakPlayer = WhitePlayer | BlackPlayer deriving (Eq, Show, Enum)

coordenadasCasillero3x3 = (map (\n -> ("",(divMod n 3)))  [0..8])
coordenadasCasillero4x4 = map (\n -> ("",(divMod n 4)))  [0..15]

-- ejemplo de TakGame
juego3x3 = ([("oxoxo",(0,0)) ,("O",(0,1)),("O",(0,2)),("X",(1,0)),("x",(1,1)),("",(1,2)),("o",(2,0)),("o",(2,1)),("",(2,2))],BlackPlayer)

casillero3X3 = (fst juego3x3)

juego3x32 = ([("o",(0,0)) ,("O",(0,1)),("O",(0,2)),("o",(1,0)),("x",(1,1)),("",(1,2)),("o",(2,0)),("o",(2,1)),("",(2,2))],BlackPlayer)

cCerc = [("",(0,0)),("X",(0,1)),("",(0,2)),("",(1,0)),("",(1,1)),("",(1,2)),("",(2,0)),("",(2,1)),("",(2,2))]

juego4x4 = (map (\n -> ("x",(divMod n 4)))  [0..15], WhitePlayer)

juegoVacio = ([],BlackPlayer)

casilleroVacio = [("",(0,0)),("",(0,1)),("",(0,2)),("",(1,0)),("",(1,1)),("",(1,2)),("",(2,0)),("",(2,1)),("",(2,2))]

beginning3x3 :: TakGame
beginning3x3 = (coordenadasCasillero3x3, BlackPlayer)

beginning4x4 :: TakGame
beginning4x4 = (coordenadasCasillero4x4, BlackPlayer)


-- juegoValido determina si el juego es un juego valido, ejemplo: que en el tablero 3x3 no tenga mas de 9
juegoValido :: TakGame -> Bool
juegoValido juego
    | length (fst juego) == 9 = True
    | length (fst juego) == 16 = True
    | otherwise = False

-- caracterPosicion devuelve el caracter en una posicion dada
caracterPosicion :: TakGame -> Int -> String
caracterPosicion juego posicion
    | (juegoValido juego) == False = error "juego no valido"
    | posicion < 0 || posicion > 16 = error "posicion no valida"
    | otherwise = (fst ((fst juego) !! posicion))

obtenerCasillero :: TakGame -> [Casillero]
obtenerCasillero juego
    | (fst juego) == [] = []
    | otherwise = (fst juego)

juegoSinComenzar :: TakGame -> Bool
juegoSinComenzar ((x:xs),y)
    | (x:xs) == [] = True
    | (fst x ) == "x" || (fst x ) == "X" || (fst x ) == "o"  || (fst x ) == "O" = False
    | otherwise = True

contenidoCasillero :: [Casillero] -> (Int, Int) -> [Char]
contenidoCasillero ((caracteres, (a,b)):xs) (x,y)
    | a == x && b == y = caracteres
    | otherwise = contenidoCasillero xs (x,y)
contenidoCasillero [] _ = error "casillero no encontrado"

actions :: TakGame -> [(TakPlayer, [TakAction])]
actions juego = [(activePlayer juego, (generarAccionesInsertar (obtenerCasillero juego)) ++ generarAccionesMover (borrarCasosNoPosibles (casillasParaMover (obtenerCasillero juego)) )), (nonActivePlayer juego, [])]

 

--[(activePlayer juego, (generarAccionesMover ((casillasParaMover (obtenerCasillero juego))) ++ (generarAccionesInsertar (obtenerCasillero juego)))) , (nonActivePlayer juego, [])]
    
--[(activePlayer juego, (generarAccionesInsertar (obtenerCasillero juego)) ++ generarAccionesMover (filtrarMovimientos (activePlayer juego) (casillasParaMover (obtenerCasillero juego)))), (nonActivePlayer juego, [])]


          
--     | (juegoSinComenzar juego) = [(activePlayer juego, generarAccionesInsertar (obtenerCasillero juego)), (nonActivePlayer juego, [])]  
--zip players [if f then [] else [TakAction], []] --TODO

borrarCasosNoPosibles :: [(Casillero, Casillero)] -> [(Casillero, Casillero)]
borrarCasosNoPosibles [] = []
borrarCasosNoPosibles (((caracteres, (x,y)), ((caracteres2, (x2,y2)))):xs)
    | caracteres == "" || caracteres2 == "X" || caracteres2 == "O" = borrarCasosNoPosibles xs
    | otherwise = ((caracteres, (x,y)), ((caracteres2, (x2,y2)))): borrarCasosNoPosibles xs

generarAccionesInsertar :: [Casillero] -> [TakAction]
generarAccionesInsertar [] = []
generarAccionesInsertar ((caracteres, (a,b)):xs)
    | caracteres == "" = (Insertar (a,b) True) : (Insertar (a,b) False) : generarAccionesInsertar xs
    | otherwise = generarAccionesInsertar xs

generarAccionesMover :: [(Casillero, Casillero)] -> [TakAction]
generarAccionesMover (((_, (a,b)),(_, (a2,b2))):xs)  = (Mover (a,b) (a2,b2)) : generarAccionesMover xs
generarAccionesMover [] = []

generarAccionesDesapilar :: [Casillero] -> Casillero -> TakAction
generarAccionesDesapilar juego (caracteres, (a,b)) = 
    | contenidoCasillero juego (a+1, b) /= 'X' && 

generarAccionesDesapilarDerecha :: [Casillero] -> Casillero -> TakAction
generarAccionesDesapilarDerecha [] _ = error "juego vacio"
generarAccionesDesapilarDerecha juego (caracteres2,(x,y))
    | contenidoCasillero juego (x,y+1) /= 'X' || contenidoCasillero juego (x,y+1) /= 'O' = 

casillasDireccionIzquierda :: [Casillero] -> Casillero -> [Casillero]
casillasDireccionIzquierda juego (caracteres, (x,y)) = 
    | 



f2 :: a -> [a] ->[(a,a)]
f2 a [] = []
f2 a (h:t) = (a,h):(f2 a t)

f3 :: [a] -> a -> [(a,a)]
f3 [] a = []
f3 (x:xs) a = (x,a): f3 xs a

filtrarMovimientos :: TakPlayer -> [(Casillero, Casillero)] -> [(Casillero, Casillero)]
filtrarMovimientos WhitePlayer (((caracteres, (x,y)), ((caracteres2, (x2,y2)))):xs) = filter (\x -> last caracteres == 'x' && caracteres /= "" && last caracteres2 /= 'X' && last caracteres2 /= 'O') (((caracteres, (x,y)), (caracteres2, (x2,y2))):xs)
filtrarMovimientos BlackPlayer (((caracteres, (x,y)), ((caracteres2, (x2,y2)))):xs) = filter (\x -> last caracteres == 'o' && caracteres /= "" && last caracteres2 /= 'X' && last caracteres2 /= 'O') (((caracteres, (x,y)), (caracteres2, (x2,y2))):xs) 

casillasParaMover :: [Casillero] -> [(Casillero, Casillero)]
casillasParaMover [] = []
casillasParaMover ((caracteres, (a, b)):xs) = (f2 (caracteres, (a, b)) (casillasCercanas  ((caracteres, (a, b)):xs) (caracteres, (a,b)))) ++ (f3 ((casillasCercanas  ((caracteres, (a, b)):xs) (caracteres, (a,b)))) (caracteres, (a, b))) ++ casillasParaMover xs

casillasPosibles :: [Casillero] -> [Casillero]
casillasPosibles [] = []
casillasPosibles ((caracteres, (a, b)):xs) 
    | (caracteres == "") || (last caracteres == 'x') || (last caracteres == 'o')  = (caracteres,(a,b)) : (casillasPosibles xs)
    | (last caracteres == 'X') || (last caracteres == 'O') = casillasPosibles xs
    | otherwise = []

casillasCercanas :: [Casillero] -> Casillero -> [Casillero]
casillasCercanas [] _ = []
casillasCercanas ((caracteres1, (a, b)):xs) (caracteres2, (x, y)) = 
                                                                        if (x - 1 == a && y == b) || (x + 1 == a && y == b) || (x == a && y + 1 == b) || (x == a && y - 1 == b) then
                                                                            [(caracteres1, (a, b))] ++ (casillasCercanas xs (caracteres2, (x, y)))
                                                                        else
                                                                            casillasCercanas xs (caracteres2, (x, y))



controlDeCaracteres :: Casillero -> Bool
controlDeCaracteres (caracteres,(x,y))
    | (length caracteres) > 5 = error "cantidad de caracteres en una pila no valido"
    | (length caracteres) < 0 = error "cantidad de caracteres en una pila no valido"
    | otherwise = True



direccion :: Casillero -> Casillero -> Direccion
direccion (caracteres,(x,y)) (caracteres2,(x2,y2))
    | x > x2 && y == y2 = Arriba
    | x < x2 && y == y2 = Abajo
    | x == x2 && y > y2 = Derecha
    | x == x2 && y < y2 = Izquierda 

-- movimiento vertical
seMueveEnX :: Casillero -> [Casillero] -> Bool
seMueveEnX (caracteres,(x,y)) ((caracteres2,(a,b)):xs) = if  ((x+1,y) == (a,b) || (x-1,y) == (a,b)) && seMueveEnX (caracteres2,(a,b)) xs

-- se mueve hacia la derecha
seMueveDerecha :: Casillero -> [Casillero] -> Bool
seMueveDerecha (caracteres,(x,y)) ((caracteres2,(a,b)):xs) = ((x,y+1) == (a,b) || (x,y-1) == (a,b)) && seMueveDerecha (caracteres2,(a,b)) xs

-- movimiento horizontal
seMueveEnY :: Casillero -> [Casillero] -> Bool
seMueveEnY (caracteres,(x,y)) ((caracteres2,(a,b)):xs) = ((x,y+1) == (a,b) || (x,y-1) == (a,b)) && seMueveEnY (caracteres2,(a,b)) xs


{--
moverPilaEnX :: Casillero -> [Casillero] ->  
moverPilaEnX (caracteres,(x,y)) ((caracteres2,(a,b)):xs) = if 

intersectCercaYPosible :: Casillero -> [Casillero] -> [Casillero]
intersectCercaYPosible casillero (x:xs) = intersectCercaYPosible (casillasPosibles casillero (x:xs)) (casillasCercanas)
--}

activePlayer :: TakGame -> TakPlayer
activePlayer (g, WhitePlayer) = WhitePlayer 
activePlayer (g, BlackPlayer) = BlackPlayer 

nonActivePlayer :: TakGame -> TakPlayer
nonActivePlayer (g, WhitePlayer) = BlackPlayer
nonActivePlayer (g, BlackPlayer) = WhitePlayer

   
-- HAY QUE CAMBIAR EL JUGADOR POR EL OTRO CUANDO SE EJECUTA EL NEXT, SINO SE 
-- VA A ROMPER TODO AL CARAJO
-- TAMBIEN HAY QUE PREGUNTAR SI EL JUEGO FINALIZA LUEGO DE ESA ACCION

{--
next :: TakGame -> (TakPlayer, TakAction) -> TakGame
next _ _ = TakGame True --TODO
result :: TakGame -> [(TakPlayer, Int)]
result f = zip players (if f then [] else [1, -1]) --TODO
score :: TakGame -> [(TakPlayer, Int)]
score _ = zip players [0, 0] --TODO
readAction :: String -> TakAction
readAction = read --TODO
activePlayer :: TakGame -> Maybe TakPlayer
activePlayer g = listToMaybe [p | (p, as) <- actions g, not (null as)]
players :: [TakPlayer]
players = [minBound..maxBound]
--}


next :: TakGame -> (TakPlayer, TakAction) -> TakGame
next juego (jugador,accion)
    | (activePlayer juego) /= jugador = error "jugador de la accion distinto al jugador que le toca jugar"
    | juegoTerminado juego == True = error "juego terminado"
    | elem accion (snd (head (actions juego))) == False = error "acción no posible"
    | let acc1 = (Insertar (0,0) False) in acc1 == accion = realizarAccionInsertar (obtenerCasillero juego) (jugador,accion)
    | let acc2 = (Mover (0,0) (0,0)) in acc2 == accion = realizarAccionMover (obtenerCasillero juego) (jugador,accion)
 --   | let acc3 = (Desapilar (0,0), [] Abajo) in acc3 == accion = realizarAccionDesapilar (obtenerCasillero juego) (jugador, accion)

realizarAccionInsertar :: [Casillero] -> (TakPlayer,TakAction) -> TakGame
realizarAccionInsertar ((caracteres,(x,y)):xs) (jugadorAct, (Insertar (a,b) bool)) = 
        if (controlDeCaracteres (caracteres,(x,y))) then
            if jugadorAct == WhitePlayer then
                if bool then ((buscarEnCasillero ((caracteres,(x,y)):xs) ("X",(a,b))),BlackPlayer)
                else ((buscarEnCasillero ((caracteres,(x,y)):xs) ("x",(a,b))),BlackPlayer)
            else 
                if bool then ((buscarEnCasillero ((caracteres,(x,y)):xs) ("O",(a,b))),WhitePlayer)
                else ((buscarEnCasillero ((caracteres,(x,y)):xs) ("o",(a,b))),WhitePlayer)
        else error "No se puede insertar a una pila"

realizarAccionMover :: [Casillero] -> (TakPlayer, TakAction) -> TakGame
realizarAccionMover _ (_, (Insertar (x, y) parada)) = error "!"
realizarAccionMover _ (_, (Desapilar (x, y) xoy direccion)) = error "!"
realizarAccionMover tablero (WhitePlayer, (Mover (xOrigen, yOrigen) (xDestino, yDestino))) = (eliminarUltimaPosicion (buscarEnCasillero tablero (topeDePila tablero (xOrigen, yOrigen), (xDestino, yDestino))) (xOrigen, yOrigen), BlackPlayer)
realizarAccionMover tablero (BlackPlayer, (Mover (xOrigen, yOrigen) (xDestino, yDestino))) = (eliminarUltimaPosicion (buscarEnCasillero tablero (topeDePila tablero (xOrigen, yOrigen), (xDestino, yDestino))) (xOrigen, yOrigen), WhitePlayer)

--realizarAccionDesapilar :: [Casillero] -> (TakPlayer, TakAction) -> TakGame
--realizarAccionDesapilar

buscarEnCasillero :: [Casillero] -> Casillero -> [Casillero]
buscarEnCasillero ((cad1,(x,y)):xs) (cad2,(a,b))
    | x == a && y == b = ((cad1++cad2,(a,b)):xs)
    | otherwise = (cad1,(x,y)):(buscarEnCasillero xs (cad2,(a,b)))

eliminarUltimaPosicion :: [Casillero] -> (Integer, Integer) -> [Casillero]
eliminarUltimaPosicion ((cad1,(x,y)):xs) (a,b)
    | x == a && y == b = ((drop 1 cad1,(a,b)):xs)
    | otherwise = (cad1,(x,y)):(eliminarUltimaPosicion xs (a,b))

topeDePila :: [Casillero] -> (Integer, Integer) -> [Char]
topeDePila ((cad1,(x,y)):xs) (a,b)
    | x == a && y == b && cad1 /= "" = [last cad1] 
    | otherwise = topeDePila xs (a,b)
topeDePila [] (_,_) = error "!"

impresionJuego3x3 :: TakGame -> String
impresionJuego3x3 juego = unlines $ [(caracterPosicion juego 0) ++  (caracterPosicion juego 1) ++ (caracterPosicion juego 2) ++'\n': 
    (caracterPosicion juego 3) ++ (caracterPosicion juego 4) ++ (caracterPosicion juego 5) ++ '\n':
    (caracterPosicion juego 6) ++ (caracterPosicion juego 7) ++ (caracterPosicion juego 8)]

impresionJuego4x4 :: TakGame -> String
impresionJuego4x4 juego = unlines $ [(caracterPosicion juego 0) ++  (caracterPosicion juego 1) ++ (caracterPosicion juego 2) ++ (caracterPosicion juego 3) ++'\n': 
     (caracterPosicion juego 4) ++ (caracterPosicion juego 5) ++ (caracterPosicion juego 6) ++ (caracterPosicion juego 7) ++'\n':
     (caracterPosicion juego 8) ++ (caracterPosicion juego 9) ++ (caracterPosicion juego 10) ++ (caracterPosicion juego 11) ++ '\n':
     (caracterPosicion juego 12) ++ (caracterPosicion juego 13) ++ (caracterPosicion juego 14) ++ (caracterPosicion juego 15)]


showGame :: TakGame -> String
showGame juego 
    | fst juego == [] = error "juego vacio"
    --juego 3x3
    | length (fst juego) == 9 =  impresionJuego3x3 juego
    --juego 4x4
    | length (fst juego) == 16 = impresionJuego4x4 juego
    | otherwise = error "juego no valido para mostrar"    

showAction :: TakAction -> String
showAction (Insertar (x, y) pared) = if (pared) then
                                        "||Se inserta una pared en " ++ (show (x,y))
                                    else
                                        "||Se inserta una ficha plana en " ++ (show (x,y))

showAction (Mover (x, y) (x2, y2)) = "||Se mueve de: " ++ (show (x,y)) ++ "hacia: " ++ (show (x2, y2))

showAction (Desapilar (x,y) cantidad direccion) = "Se desapila desde : " ++ (show (x,y)) ++ "en :" ++ (show cantidad) ++ " en direccion : " ++ (show direccion)

showAction2 :: [TakAction] -> String
showAction2 [] = "no hay mas acciones para mostrar "
showAction2 (x:xs) = (showAction x) ++ (showAction2 xs)

showAction3 :: [(TakPlayer, [TakAction])] -> String
showAction3 [] = " juego vacio"
showAction3 (x:y:xs) = (showAction2 (snd x)) ++ (showAction2 (snd y)) ++ showAction3 xs

score :: TakGame -> [(TakPlayer, Int)]
score juego = [(activePlayer juego, puntajeJugador (activePlayer juego) juego), (nonActivePlayer juego, puntajeJugador (nonActivePlayer juego) juego)]

puntajeJugador :: TakPlayer -> TakGame -> Int
puntajeJugador _ ([], _) = 0
puntajeJugador WhitePlayer (((tablero):xs), p)
    | fst tablero == "" = puntajeJugador WhitePlayer (xs, p)
    | last (fst tablero) == 'x' = 1 + puntajeJugador WhitePlayer (xs, p)
    | otherwise = puntajeJugador WhitePlayer (xs, p)

puntajeJugador BlackPlayer (((tablero):xs), p)
    | fst tablero == "" = puntajeJugador BlackPlayer (xs, p)
    | last (fst tablero) == 'o' = 1 + puntajeJugador BlackPlayer (xs, p)
    | otherwise = puntajeJugador BlackPlayer (xs, p)

-- Solo tuve en cuenta el caso de un empate donde la unica forma de determinar un ganador
-- es a partir de los puntos de los jugadores

result :: TakGame -> [(TakPlayer, Int)]
result juego =  if (juegoTerminado juego) then
                    [(activePlayer juego, 1), (nonActivePlayer juego, (-1))]
                else
                    []

-- Aca hay que ver los caminos
-- el que haga el next deberia usar esto antes de hacer cualquier cosa
juegoTerminado :: TakGame -> Bool
juegoTerminado (tablero, j)
    | length tablero == 9 = cumpleCaminos (tablero, j) caminosPosibles3x3
    | length tablero == 16 = cumpleCaminos (tablero, j) caminosPosibles4x4

caminosPosibles3x3 = [[(0,0),(0,1),(0,2)], [(1,0),(1,1),(1,2)], [(2,0),(2,1),(2,2)], [(0,0),(1,0),(2,0)], [(0,1),(1,1),(2,1)], [(0,2),(1,2),(2,2)], [(0,0), (0,1), (1,1), (1,2)], [(0,0), (1,0), (1,1), (2,1)], [(0,1), (1,0), (1,1), (2,0)], [(0,1), (1,1), (1,2), (2,2)], [(0,1), (0,2), (1,0), (1,1)], [(0,2), (1,1), (1,2), (2,1)], [(1,0), (1,1), (2,1), (2,2)], [(1,1), (1,2), (2,0), (2,1)]]
caminosPosibles4x4 = [[(0,0), (0,1), (0,2), (0,3)], [(1,0), (1,1), (1,2), (1,3)], [(2,0), (2,1), (2,2), (2,3)], [(3,0), (3,1), (3,2), (3,3)], [(0,0), (1,0), (2,0), (3,0)], [(0,1), (1,1), (2,1), (3,1)], [(0,2), (1,2), (2,2), (3,2)], [(0,3), (1,3), (2,3), (3,3)], 
                    [(0,0), (1,0), (2,0), (2,1), (3,1)], [(0,0), (1,0), (1,1), (2,1), (3,1)], [(0,0), (1,0), (1,1), (1,2), (2,2), (3,2)], [(0,0), (1,0), (2,0), (2,1), (2,2), (3,2)], [(0,0), (1,0), (1,1), (2,1), (2,2), (3,2)], [(0,0), (0,1), (0,2), (1,2), (1,3)], [(0,0), (0,1), (1,1), (1,2), (1,3)], [(0,0), (0,1), (1,1), (2,1), (2,2), (2,3)], [(0,0), (0,1), (0,2), (1,2), (2,2), (2,3)], [(0,0), (0,1), (1,1), (1,2), (2,2), (2,3)],
                    [(0,1), (0,2), (0,3), (1,0), (1,1)], [(0,2), (0,3), (1,0), (1,1), (1,2)], [(0,2), (0,3), (1,2), (2,0), (2,1), (2,2)], [(0,1), (0,2), (0,3), (1,1), (2,0), (2,1)], [(0,2), (0,3), (1,1), (1,2), (2,0), (2,1)], [(0,3), (1,3), (2,2), (2,3), (3,2)], [(0,3), (1,2), (1,3), (2,2), (3,2)], [(0,3), (1,1), (1,2), (1,3), (2,1), (3,1)], [(0,3), (1,3), (2,1), (2,2), (2,3), (3,1)], [(0,3), (1,2), (1,3), (2,1), (2,2), (3,1)],
                    [(0,1), (1,0), (1,1), (2,0), (3,0)], [(0,1), (1,1), (2,0), (2,1), (3,0)], [(0,2), (1,2), (2,0), (2,1), (2,2), (3,0)], [(0,2), (1,0), (1,1), (1,2), (2,0), (3,0)], [(0,2), (1,1), (1,2), (2,0), (2,1), (3,0)], [(2,2), (2,3), (3,0), (3,1), (3,2)], [(2,1), (2,2), (2,3), (3,0), (3,1)], [(1,1), (1,2), (1,3), (2,1), (3,0), (3,1)], [(1,2), (1,3), (2,2), (3,0), (3,1), (3,2)], [(1,2), (1,3), (2,1), (2,2), (3,0), (3,1)],
                    [(2,0), (2,1), (3,1), (3,2), (3,3)], [(2,0), (2,1), (2,2), (3,2), (3,3)], [(1,0), (1,1), (1,2), (2,2), (3,2), (3,3)], [(1,0), (1,1), (2,1), (3,1), (3,2), (3,3)], [(1,0), (1,1), (2,1), (2,2), (3,2), (3,3)], [(0,2), (1,2), (1,3), (2,3), (3,3)], [(0,2), (1,2), (2,2), (2,3), (3,3)], [(0,1), (1,1), (2,1), (2,2), (2,3), (3,3)], [(0,1), (1,1), (1,2), (1,3), (2,3), (3,3)], [(0,1), (1,1), (1,2), (2,2), (2,3), (3,3)],
                    [(0,1), (1,1), (1,2), (2,2), (3,2)], [(0,2), (1,1), (1,2), (2,1), (3,1)], [(1,2), (1,3), (2,0), (2,1), (2,2)], [(1,0), (1,1), (1,2), (2,2), (2,3)], [(0,1), (1,1), (2,1), (2,2), (3,2)], [(0,2), (1,2), (2,1), (2,2), (3,1)], [(1,1), (1,2), (1,3), (2,0), (2,1)], [(1,0), (1,1), (2,1), (2,2), (2,3)]]

cumpleCaminos :: TakGame -> [Camino] -> Bool
cumpleCaminos _ [] = False
cumpleCaminos juego (camino:ys) = (cumpleCamino juego camino) || cumpleCaminos juego ys

cumpleCamino :: TakGame -> Camino -> Bool
cumpleCamino _ [] = True
cumpleCamino ((caracteres, (x,y)):xs, WhitePlayer) ((xBuscado, yBuscado):ys)
    | x == xBuscado && y == yBuscado = if caracteres == "" then False else (last caracteres == 'x') && (cumpleCamino ((caracteres, (x,y)):xs, WhitePlayer) ys)
    | otherwise = cumpleCamino (xs, WhitePlayer) ((xBuscado, yBuscado):ys)

cumpleCamino ((caracteres, (x,y)):xs, BlackPlayer) ((xBuscado, yBuscado):ys)
    | x == xBuscado && y == yBuscado = if caracteres == "" then False else (last caracteres == 'o') && (cumpleCamino ((caracteres, (x,y)):xs, BlackPlayer) ys)
    | otherwise = cumpleCamino (xs, BlackPlayer) ((xBuscado, yBuscado):ys)


instance Eq TakAction where
    (Insertar (_,_) _) == (Insertar (_,_) _) = True
    (Insertar (_,_) _) == (Mover (_,_) (_,_)) = False
    (Insertar (_,_) _) == (Desapilar (_,_) [_] _) = False

    (Mover (_,_) (_,_)) == (Mover (_,_) (_,_)) = True
    (Mover (_,_) (_,_)) == (Insertar (_,_) _) = False
    (Mover (_,_) (_,_)) == (Desapilar (_,_) [_] _) = False

    (Desapilar (_,_) [_] _) == (Desapilar (_,_) [_] _) = True
    (Desapilar (_,_) [_] _) == (Insertar (_,_) _) = False   
    (Desapilar (_,_) [_] _) == (Mover (_,_) (_,_)) = False
    

    

{-- Match controller -------------------------------------------------------------------------------
Código de prueba. Incluye una función para correr las partidas y dos agentes: consola y aleatorio.
type TakAgent = TakGame -> IO (Maybe TakAction)
{- La función ´runMatch´ corre la partida completa a partir del estado de juego dado, usando los dos 
agentes dados. Retorna una tupla con los puntajes (score) finales del juego.
-}
runMatch :: (TakAgent, TakAgent) -> TakGame -> IO [(TakPlayer, Int)]
runMatch ags@(ag1, ag2) g = do
   putStrLn (showBoard g)
   case (activePlayer g) of
      Nothing -> return $ result g
      Just p -> do
         let ag = [ag1, ag2] !! (fromJust (elemIndex p players))
         move <- ag g
         runMatch ags (Tak.next g (p, fromJust move))
{- La función ´runOnConsole´ ejecuta toda la partida a partir del estado inicial usando dos agentes
de consola.
-}
runOnConsole :: TakGame -> IO [(TakPlayer, Int)]
runOnConsole g = do
   runMatch (consoleAgent WhitePlayer, consoleAgent BlackPlayer) g
run3x3OnConsole :: IO [(TakPlayer, Int)]
run3x3OnConsole = runOnConsole beginning3x3
run4x4OnConsole :: IO [(TakPlayer, Int)]
run4x4OnConsole = runOnConsole beginning4x4
{- El agente de consola ´consoleAgent´ muestra el estado de juego y los movimientos disponibles por
consola, y espera una acción por entrada de texto.
-}
consoleAgent :: TakPlayer -> TakAgent
consoleAgent player state = do
   let moves = fromJust (lookup player (actions state))
   if null moves then do
      putStrLn "No moves!"
      getLine
      return Nothing
   else do
      putStrLn ("Select one move:" ++ concat [" "++ show m | m <- moves])
      line <- getLine
      let input = readAction line
      if elem input moves then return (Just input) else do 
         putStrLn "Invalid move!"
         consoleAgent player state
randomAgent :: TakPlayer -> TakAgent
randomAgent player state = do
    let moves = fromJust (lookup player (actions state))
    if null moves then do
       putStrLn "No moves!"
       return Nothing
    else do
       i <- randomRIO (0, (length moves) - 1)
       return (Just (moves !! i))
-- Fin

-}
{--
zip (fst beginning3x3) coordenadasCasillero3x3
[("",(0,0)),("",(0,1)),("",(0,2)),("",(1,0)),("",(1,1)),("",(1,2)),("",(2,0)),("",(2,1)),("",(2,2))]
insertarX tablero (xy)
where algo = zip tabler coordenadas3x3
activePlayer :: TakGame -> TakPlayer
activePlayer juego 
    | contarRepeticiones juego WhitePlayer > contarRepeticiones juego BlackPlayer = BlackPlayer
    | contarRepeticiones juego WhitePlayer <= contarRepeticiones juego BlackPlayer = WhitePlayer
nonActivePlayer :: TakGame -> TakPlayer
nonActivePlayer juego 
    | activePlayer juego == WhitePlayer = BlackPlayer
    | otherwise = WhitePlayer
contarRepeticiones :: TakGame -> TakPlayer -> Int
contarRepeticiones ((caracteres, _, _):xs) WhitePlayer
    | elem 'x' caracteres || elem 'X' caracteres  = (fichasDeLaPila caracteres 'x') + (fichasDeLaPila caracteres 'X') + (contarRepeticiones(xs) WhitePlayer)
    | otherwise = (contarRepeticiones(xs) WhitePlayer)
contarRepeticiones ((caracteres, _, _):xs) BlackPlayer
    | elem 'o' caracteres || elem 'O' caracteres = (fichasDeLaPila caracteres 'o') + (fichasDeLaPila caracteres 'o') + (contarRepeticiones(xs) BlackPlayer)
    | otherwise = (contarRepeticiones(xs) BlackPlayer)
contarRepeticiones [] _ = 0
fichasDeLaPila :: [Char] -> Char -> Int
fichasDeLaPila [] _ = 0
fichasDeLaPila (x:xs) caracter
    | x == caracter = 1 + fichasDeLaPila xs caracter
    | otherwise = fichasDeLaPila xs caracter
actions :: TakGame -> [(TakPlayer, [TakAction])]
actions juegoActual 
        | juegoActual == beginning3x3 = [(activePlayer juegoActual, generarAccionesInsertar juegoActual),(nonActivePlayer juegoActual, []) ]
        | juegoActual == beginning4x4 = [(activePlayer juegoActual, generarAccionesInsertar juegoActual),(nonActivePlayer juegoActual, [])]
        | contarRepeticiones juegoActual (activePlayer juegoActual) == 10 && (length juegoActual == 9) =  [(activePlayer juegoActual, (generarAccionesMover  (casillasParaMover juegoActual))), (nonActivePlayer juegoActual, [])]
        | contarRepeticiones juegoActual (activePlayer juegoActual) == 15 && (length juegoActual == 16) = [(activePlayer juegoActual, (generarAccionesMover  (casillasParaMover juegoActual))), (nonActivePlayer juegoActual, [])]
        | otherwise = [(activePlayer juegoActual, (generarAccionesInsertar juegoActual) ++ (generarAccionesMover  (casillasParaMover juegoActual))), (nonActivePlayer juegoActual, [])]
{-
    CASO INSERTAR FICHA NUEVA
-}
casillasLibresParaInsertar :: TakGame -> [Casillero]
casillasLibresParaInsertar ((caracteres, a, b):xs) = if (caracteres == ".") then
                                                    [(caracteres, a, b)] ++ (casillasLibresParaInsertar xs)
                                                else
                                                    casillasLibresParaInsertar xs
casillasLibresParaInsertar [] = []
generarAccionesInsertar :: [Casillero] -> [TakAction]
generarAccionesInsertar [] = []
generarAccionesInsertar (x:xs) = [(x, True, False, x,0)] ++ [(x, True, True, x, 0)] ++ generarAccionesInsertar xs
enJuego3x3 :: TakGame -> Int -> Bool
enJuego3x3 ((caracteres, a, b):xs) contador = case (contador == 9) of {
    True -> False;
    False -> case (caracteres == ".") of {
        True -> enJuego3x3 xs (contador + 1);
        False -> case (contador < 9) of {
            True -> True;
            False -> False
        }
    }
}
todasCasillasLlenas :: TakGame -> Bool
todasCasillasLlenas [] = True
todasCasillasLlenas ((caracteres, a, b):xs) = if (last caracteres == '.')  && todasCasillasLlenas xs then True else False
{-
    CASO MOVER FICHA EXISTENTE
-}
f2 :: a -> [a] ->[(a,a)]
f2 a [] = []
f2 a (h:t) = (a,h):(f2 a t)
casillasParaMover :: TakGame -> [(Casillero, Casillero)]
casillasParaMover [] = []
casillasParaMover ((caracteres, a, b):xs) = (f2 (caracteres, a, b) (casillasCercanasPosibles (casillasCercanas  ((caracteres, a, b):xs) (caracteres, a,b)))++ casillasParaMover xs)
-- contar largo de la pila, que sea menor igual a 5 y hacer tuplas posibles 
--
generarAccionesMover :: [(Casillero, Casillero)] -> [TakAction]
generarAccionesMover (((caracteres,a,b),y):xs) = (movimientosParaUnaPila ((caracteres,a,b),y)) ++ generarAccionesMover xs
movimientosParaUnaPila :: (Casillero, Casillero) -> [TakAction]
movimientosParaUnaPila ((caracteres,a,b),y) = [((caracteres,a,b), False, False, y, contador) | contador <- [1.. length caracteres]]
m = [(("X", 0, 0), ("X", 0, 1)), (("X", 0, 2), ("X", 1, 0)),( ("X", 1, 1), ("X", 1, 2)), (("X", 2, 0), ("X", 2, 1))]
{-
    CASO MANIPULAR PILA
-}
-- a partir del estado del juego
casillasCercanas :: TakGame -> Casillero -> [Casillero]
casillasCercanas [] _ = []
casillasCercanas ((caracteres1, a, b):xs) (caracteres2, x, y) = if (x - 1 == a && y == b) || (x + 1 == a && y == b) || (x == a && y + 1 == b) || (x == a && y - 1 == b) then
                                                                    [(caracteres1, a, b)] ++ (casillasCercanas xs (caracteres2, x, y))
                                                                else
                                                                    casillasCercanas xs (caracteres2, x, y)
-- a partir del metodo "casillasCercanas"
casillasCercanasVacias :: [Casillero] -> [Casillero]
casillasCercanasVacias ((caracteres, a, b):xs) = if (caracteres == ".") then
                                                    [(caracteres, a, b)] ++ (casillasCercanasVacias xs)
                                                else
                                                    casillasCercanasVacias xs
casillasCercanasVacias [] = []
-- a partir del metodo "casillasCercanas"
casillasCercanasPropias :: TakPlayer -> [Casillero] -> [Casillero]
casillasCercanasPropias WhitePlayer ((caracteres, a, b):xs) = if (last caracteres == 'x') then
                                                            [(caracteres, a, b)] ++ (casillasCercanasPropias WhitePlayer xs)
                                                        else
                                                            casillasCercanasPropias WhitePlayer xs
casillasCercanasPropias BlackPlayer ((caracteres, a, b):xs) = if (last caracteres == 'o') then
                                                            [(caracteres, a, b)] ++ (casillasCercanasPropias BlackPlayer xs)
                                                        else
                                                            casillasCercanasPropias BlackPlayer xs
casillasCercanasPropias _ [] = []
-- a partir del metodo "casillasCercanas"
casillasCercanasPosibles :: [Casillero] -> [Casillero]
casillasCercanasPosibles ((caracteres, a, b):xs) = if (last caracteres /= 'X' && last caracteres /= 'O') then
                                                        [(caracteres, a, b)] ++ (casillasCercanasPosibles xs)
                                                    else
                                                        casillasCercanasPosibles xs
casillasCercanasPosibles [] = []
next :: TakGame -> (TakPlayer, TakAction) -> TakGame
next juegoActual (jugador, accion)
    | result juegoActual /= [] = error "juego terminado"
    | accion `elem` (snd (head(actions juegoActual))) && jugador == (fst (head(actions juegoActual))) = 
    | otherwise = error "No se puede realizar"
-- caso de  mover, necesitamos dado la cantidad de elementos que queremos mover dejarlos en la posicion inicial y mover los restantes
-- a la posicion final 
realizarAccion :: TakGame -> (TakPlayer,TakAction) -> Int -> TakGame
realizarAccion [] _ = []
realizarAccion ((caracteres1,x,y):xs) (WhitePlayer, ((caracteres2,x2,y2), insertaMueve, paradaPlana, (caracteres3,x3,y3))) contador = 
            if insertaMueve then
                if ( x == x3 && y == y3) then
                    if (paradaPlana) then
                      take (contador -1)++("X",x3,y3):drop (contador + 1) 
                    else 
                        take (contador -1)++("x",x3,y3):drop (contador + 1)       
                else realizarAccion xs (WhitePlayer, ((caracteres2,x2,y2),insertaMueve, (caracteres3,x3,y3))) (contador + 1)
            else
                if ( x == x2 && y == y2) then
                   if (paradaPlana) then
--result :: TakGame -> [(TakPlayer, Int)]
score :: TakGame -> [(TakPlayer, Int)]
score ((caracteres, a, b):xs) = if last caracteres == 'x' || last caracteres == 'X' then puntaje (BlackPlayer,0) ++ score xs
                                    
puntaje :: (TakPlayer, Int) -> (TakPlayer, Int)
puntaje (BlackPlayer, x) = (BlackPlayer, x+1)
pintaje (WhitePlayer, x) = (WhitePlayer, x+1)
-- tenemos que poner al jugador activo en el TakGame
-- map (\n -> divmod n 3) [0..8] para las coordenadas de un 3x3
-- map (\n -> divmod n 4) [0..15] para las coordenadas de un 4x4
-- cuantos elementos de la pila original dejo en cada casilla
fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x
fst4 :: (a, b, c, d) -> a
fst4 (a,_,_,_) = a
fst5 :: (a, b, c, d, e) -> a
fst5 (a,_,_,_,_) = a
showGame :: TakGame -> String
showGame juego 
    | length juego == 9 = show( fst3 (juego!!0) ++ fst3 (juego!!1) ++ fst3 (juego!!2) ++  '\n': fst3 (juego!!3)  ++ fst3 (juego!!4) ++ fst3 (juego!!5) ++  '\n' : fst3 (juego!!6) ++ fst3 (juego!!7) ++ fst3 (juego!!8) )
    | length juego == 16 = show (fst3 (juego!!0) ++ fst3 (juego!!1) ++ fst3 (juego!!2) ++ fst3 (juego!!3) ++  '\n' : fst3 (juego!!4) ++ fst3 (juego!!5) ++ fst3 (juego!!6) ++ fst3 (juego!!7) ++  '\n' : fst3 (juego!!8) ++ fst3 (juego!!9) ++ fst3 (juego!!10) ++ fst3 (juego!!11) ++  '\n' : fst3 (juego!!12) ++ fst3 (juego!!13) ++ fst3 (juego!!14) ++ fst3 (juego!!15) )
showAction :: TakAction -> String
showAction ((_,x,y),b,c,(_,x2,y2),e) = 
    if (b) then 
        if (c) 
            then "Inserta desde afuera la pieza parada en la posicion (" ++ show x2 ++","++ show y2 ++")"
        else
            "Inserta desde afuera la pieza plana en la posicion (" ++ show x2 ++","++ show y2 ++")"
    else
        "mueve "++ show e ++ " fichas desde la posicion (" ++  show x ++","++ show y ++ ")" ++ " hacia la posicion (" ++ show x2 ++","++ show y2 ++")"
--}

--readAction :: String -> TakAction