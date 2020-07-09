import Data.Maybe (fromJust, listToMaybe)
import Data.List (elemIndex)
import Data.List
import System.Random (randomRIO)
import Data.Char
import Data.List (tails)

type TakGame = ([Casillero], TakPlayer)
type Casillero = ([Char], (Integer, Integer))
type Camino = [(Integer, Integer)]
data Direccion = Arriba | Abajo | Izquierda | Derecha deriving(Eq, Show, Read)
data TakAction = Insertar (Integer, Integer) Bool | Mover (Integer, Integer) (Integer, Integer) | Desapilar (Integer, Integer) [Integer] Direccion deriving (Eq)       -- bool insertar True = Pared
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


--- METODOS SOLICITADOS

beginning3x3 :: TakGame
beginning3x3 = (coordenadasCasillero3x3, BlackPlayer)

beginning4x4 :: TakGame
beginning4x4 = (coordenadasCasillero4x4, BlackPlayer)

actions :: TakGame -> [(TakPlayer, [TakAction])]
actions juego = [(activePlayer juego, (generarAccionesInsertar (obtenerCasillero juego)) ++ generarAccionesMover (borrarCasosNoPosibles (activePlayer juego ) (casillasParaMover (obtenerCasillero juego)) )), (nonActivePlayer juego, [])]

activePlayer :: TakGame -> TakPlayer
activePlayer (g, jugador) = jugador 

result :: TakGame -> [(TakPlayer, Int)]
result juego =  if (juegoTerminado juego) then
                    [(activePlayer juego, 1), (nonActivePlayer juego, (-1))]
                else
                    []
{-
next :: TakGame -> (TakPlayer, TakAction) -> TakGame
next juego (jugador,accion)
    | (activePlayer juego) /= jugador = error "jugador de la accion distinto al jugador que le toca jugar"
    | juegoTerminado juego = error "juego terminado"
    | elem accion (snd (head (actions juego))) == False = error "acción no posible"
    | let acc1 = (Insertar (0,0) False) in acc1 == accion = realizarAccionInsertar (obtenerCasillero juego) (jugador,accion)
    | let acc2 = (Mover (0,0) (0,0)) in acc2 == accion = realizarAccionMover (obtenerCasillero juego) (jugador,accion)
--    | let acc3 = (Desapilar (0,0), [] Abajo) in acc3 == accion = realizarAccionDesapilar (obtenerCasillero juego) (jugador, accion)
-}

next :: TakGame -> (TakPlayer, TakAction) -> TakGame
next juego (jugador, (Insertar (x,y) pared))
    | elem (Insertar (x,y) pared) (snd (head (actions juego))) = realizarAccionInsertar (obtenerCasillero juego) (jugador, (Insertar (x,y) pared))
    | otherwise = error "accion invalida"

next juego (jugador, (Mover (x,y) (x2, y2)))
    | elem (Mover (x,y) (x2, y2)) (snd (head (actions juego))) = realizarAccionMover (obtenerCasillero juego) (jugador, (Mover (x,y) (x2, y2)))
    | otherwise = error "accion invalida"

{-
next juego (jugador, (Desapilar (x,y) cantidades direccion))
    | elem (Desapilar (x,y) cantidades direccion) (snd (head (actions juego))) = realizarAccionDesapilar (obtenerCasillero juego) (jugador, (Desapilar (x,y) cantidades direccion))
    | otherwise = error "accion invalida"
-}

score :: TakGame -> [(TakPlayer, Int)]
score juego = [(activePlayer juego, puntajeJugador (activePlayer juego) juego), (nonActivePlayer juego, puntajeJugador (nonActivePlayer juego) juego)]

showAction :: TakAction -> String
showAction (Insertar (x, y) pared) = if (pared) then
                                        "||Insertar una pared en " ++ (show (x,y))
                                    else
                                        "||Insertar una ficha plana en " ++ (show (x,y))

showAction (Mover (x, y) (x2, y2)) = "||Mover de: " ++ (show (x,y)) ++ "hacia: " ++ (show (x2, y2))
showAction (Desapilar (x,y) cantidad direccion) = "||Desapilar desde : " ++ (show (x,y)) ++ "en :" ++ (show cantidad) ++ " en direccion : " ++ (show direccion)

showAction2 :: [TakAction] -> String
showAction2 [] = "no hay mas acciones para mostrar "
showAction2 (x:xs) = (showAction x) ++ (showAction2 xs)

showAction3 :: [(TakPlayer, [TakAction])] -> String
showAction3 [] = " juego vacio"
showAction3 (x:y:xs) = (showAction2 (snd x)) ++ (showAction2 (snd y)) ++ showAction3 xs

readAction :: String -> TakAction
readAction mensaje = 
    if (head mensaje == '3') then
        if (mensaje !! 1 == 'i') then
            if (isDigit (mensaje!!2)) then
                if (mensaje !! 3 == 'p') then
                    (Insertar (posicionACoordenadas3x3 (digitToInt (mensaje!!2))) False) 
                else if (mensaje !! 3 == 'P') then
                    (Insertar (posicionACoordenadas3x3 (digitToInt (mensaje!!2))) True) 
                else
                    error "formato invalido"
            else
                error "formato invalido"
        else if (mensaje !! 1 == 'm') then
            if (isDigit (mensaje!!2)) && (isDigit (mensaje!!3)) then
                (Mover (posicionACoordenadas3x3 (digitToInt (mensaje!!2))) (posicionACoordenadas3x3 (digitToInt (mensaje!!3))))
            else
                error "formato invalido"
        --else if (mensaje !! 1 == 'a') then
                -- ACA SE COMPLICA
        else
            error "formato invalido"
    else if (head mensaje == '4') then
        if (mensaje !! 1 == 'i') then
            if (isDigit (mensaje!!2) && isDigit (mensaje!!3)) then
                if (mensaje !! 4 == 'p') then
                    (Insertar (posicionACoordenadas4x4 (digitToInt (mensaje!!2)*10 + digitToInt (mensaje!!3))) False) 
                else if (mensaje !! 3 == 'P') then
                    (Insertar (posicionACoordenadas4x4 (digitToInt (mensaje!!2)*10 + digitToInt (mensaje!!3))) True)
                else
                    error "formato invalido"
            else
                error "formato invalido"
        else if (mensaje !! 1 == 'm') then
            if (isDigit (mensaje!!2) && isDigit (mensaje!!3) && isDigit (mensaje!!4) && isDigit (mensaje!!5)) then
                (Mover (posicionACoordenadas4x4 (digitToInt (mensaje!!2)*10 + digitToInt (mensaje!!3))) (posicionACoordenadas4x4 (digitToInt (mensaje!!4)*10 + digitToInt (mensaje!!5))))
            else 
                error "formato invalido"
        --else if (mensaje !! 1 == 'a') then
                -- ACA SE COMPLICA
        else
            error "formato invalido"
    else
        error "formato invalido"

showBoard :: TakGame -> String
showBoard juego 
    | fst juego == [] = error "juego vacio"
    --juego 3x3
    | length (fst juego) == 9 =  impresionJuego3x3 juego
    --juego 4x4
    | length (fst juego) == 16 = impresionJuego4x4 juego
    | otherwise = error "juego no valido para mostrar" 

activePlayer2 :: TakGame -> Maybe TakPlayer
activePlayer2 g = listToMaybe [p | (p, as) <- actions g, not (null as)]

players = [WhitePlayer, BlackPlayer]

-- METODOS AUXILIARES

nonActivePlayer :: TakGame -> TakPlayer
nonActivePlayer (g, WhitePlayer) = BlackPlayer
nonActivePlayer (g, BlackPlayer) = WhitePlayer

posicionACoordenadas3x3 :: Int -> (Integer, Integer)
posicionACoordenadas3x3 num
    | num <= 8 = snd ((fst beginning3x3) !! num)
    | otherwise = error "!"

posicionACoordenadas4x4 :: Int -> (Integer, Integer)
posicionACoordenadas4x4 num
    | num <= 15 = snd ((fst beginning4x4) !! num)
    | otherwise = error "!"

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

contenidoCasillero :: [Casillero] -> (Integer, Integer) -> [Char]
contenidoCasillero ((caracteres, (a,b)):xs) (x,y)
    | (a == x && b == y) = caracteres
    | otherwise = contenidoCasillero xs (x,y)
contenidoCasillero [] _ = error "casillero no encontrado"

borrarCasosNoPosibles :: TakPlayer -> [(Casillero, Casillero)] -> [(Casillero, Casillero)]
borrarCasosNoPosibles _ [] = []
borrarCasosNoPosibles WhitePlayer (((caracteres, (x,y)), (([], (x2,y2)))):xs) = 
    if caracteres /= "" && (last caracteres == 'X' || last caracteres == 'x') then 
        ((caracteres, (x,y)), (([], (x2,y2)))): borrarCasosNoPosibles WhitePlayer xs
    else 
        borrarCasosNoPosibles WhitePlayer xs      
borrarCasosNoPosibles BlackPlayer (((caracteres, (x,y)), (([], (x2,y2)))):xs) = 
    if caracteres /= "" && (last caracteres == 'O' || last caracteres == 'o') then 
        ((caracteres, (x,y)), (([], (x2,y2)))): borrarCasosNoPosibles BlackPlayer xs
    else 
        borrarCasosNoPosibles BlackPlayer xs                                        
borrarCasosNoPosibles WhitePlayer (((caracteres, (x,y)), ((caracteres2, (x2,y2)))):xs)
    | caracteres == "" || last caracteres2 == 'X' || last caracteres2 == 'O' || last caracteres2 == 'o' = borrarCasosNoPosibles WhitePlayer xs
    | otherwise = ((caracteres, (x,y)), ((caracteres2, (x2,y2)))): borrarCasosNoPosibles WhitePlayer xs
borrarCasosNoPosibles BlackPlayer (((caracteres, (x,y)), ((caracteres2, (x2,y2)))):xs)
    | caracteres == "" || last caracteres2 == 'X' || last caracteres2 == 'O' || last caracteres2 == 'x' = borrarCasosNoPosibles BlackPlayer xs
    | otherwise = ((caracteres, (x,y)), ((caracteres2, (x2,y2)))): borrarCasosNoPosibles BlackPlayer xs

generarAccionesInsertar :: [Casillero] -> [TakAction]
generarAccionesInsertar [] = []
generarAccionesInsertar ((caracteres, (a,b)):xs)
    | caracteres == "" = (Insertar (a,b) True) : (Insertar (a,b) False) : generarAccionesInsertar xs
    | otherwise = generarAccionesInsertar xs

generarAccionesMover :: [(Casillero, Casillero)] -> [TakAction]
generarAccionesMover (((_, (a,b)),(_, (a2,b2))):xs)  = (Mover (a,b) (a2,b2)) : generarAccionesMover xs
generarAccionesMover [] = []
{--
generarAccionesDesapilar :: [Casillero] -> Casillero -> TakAction
generarAccionesDesapilar juego (caracteres, (a,b)) = 
    | contenidoCasillero juego (a+1, b) /= 'X' && 



casillasDireccionIzquierda :: [Casillero] -> Casillero -> [Casillero]
casillasDireccionIzquierda juego (caracteres, (x,y)) = 
    | 



casillasDireccionDerecha :: [Casillero] -> [Casillero]
casillasDireccionDerecha [] = []
casillasDireccionDerecha((((caracteres, (x,y)),(caracteres2, (x2,y2)))):xs)  = (filter (x==x2 & y+1 ==y2) casillero)

    | length caracteres == 3 = combinationsOf 3 caracteres
    | length caracteres == 4 = combinationsOf 4 caracteres
    | length caracteres == 5 = combinationsOf 5 caracteres
--}

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

sumaNumero :: Int -> [[Int]]
sumaNumero 0 = [[0]]
sumaNumero 1 = [[1]]
sumaNumero x = quitarDuplicados ([w : z | y<-[1..(x - 1)], w<-[1..(x - 1)], z<-(sumaNumero y), x == w + (sum z)]
     ++ [z ++ [w] | y<-[1..(x - 1)], w<-[1..(x - 1)], z<-(sumaNumero y), x == w + (sum z)]
     ++ [[y] ++ [z] | y<-[1..(x - 1)], z<-[1..(x - 1)] ,y + z == x])

quitarDuplicados :: (Eq a) => [a] -> [a]
quitarDuplicados [] = []
quitarDuplicados (x:xs)
    | elem x xs = quitarDuplicados xs
    | otherwise = x : quitarDuplicados xs

{--
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
--seMueveDerecha :: Casillero -> [Casillero] -> Bool
--seMueveDerecha (caracteres,(x,y)) ((caracteres2,(a,b)):xs) = ((x,y+1) == (a,b) || (x,y-1) == (a,b)) && seMueveDerecha (caracteres2,(a,b)) xs

-- movimiento horizontal
seMueveEnY :: Casillero -> [Casillero] -> Bool
seMueveEnY (caracteres,(x,y)) ((caracteres2,(a,b)):xs) = ((x,y+1) == (a,b) || (x,y-1) == (a,b)) && seMueveEnY (caracteres2,(a,b)) xs
--}

{--
moverPilaEnX :: Casillero -> [Casillero] ->  
moverPilaEnX (caracteres,(x,y)) ((caracteres2,(a,b)):xs) = if 
--}

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
realizarAccionMover tablero (WhitePlayer, (Mover (xOrigen, yOrigen) (xDestino, yDestino))) = (eliminarUltimaPosicion (buscarEnCasillero tablero (topeDePila tablero (xOrigen, yOrigen), (xDestino, yDestino))) (xOrigen, yOrigen), BlackPlayer)
realizarAccionMover tablero (BlackPlayer, (Mover (xOrigen, yOrigen) (xDestino, yDestino))) = (eliminarUltimaPosicion (buscarEnCasillero tablero (topeDePila tablero (xOrigen, yOrigen), (xDestino, yDestino))) (xOrigen, yOrigen), WhitePlayer)

{-
realizarAccionDesapilar :: [Casillero] -> (TakPlayer, TakAction) -> TakGame
realizarAccionDesapilar ((caracteres,(x,y)):xs) (jugadorAct, (Desapilar (a,b) (z:zs) direccion))
    | direccion == Arriba = 
    | direccion == Izquierda =
    | direccion == Abajo = 
    | direccion == Derecha = 

-- disminuyen las x
desapilarArriba :: [Casillero] -> (TakPlayer, TakAction) -> [Casillero]
desapilarArriba ((caracteres,(x,y)):xs) (jugadorAct, (Desapilar (a,b) (z:zs) Arriba)) =
        if x == a && y == b then 
            if (seMueveArriba ("",(a,b)) x | x <- (intersectCercaYPosible ((caracteres,(x,y)):xs) ("",(1,1)))) then 
            
            --(((take (foldr (+) 0 (z:zs)) caracteres), (x,y)) : xs)
        else
            (((caracteres,(x,y)):xs) : desapilarArriba xs (jugadorAct, (Desapilar (a,b) (z:zs) Arriba)))

-- aumentan las x
desapilarAbajo :: [Casillero] -> (TakPlayer, TakAction) -> TakGame

-- disminuyen las y
desapilarIzquierda :: [Casillero] -> (TakPlayer, TakAction) -> TakGame

-- aumentan las y
desapilarDerecha :: [Casillero] -> (TakPlayer, TakAction) -> TakGame         
-}

-- (integer,Integer) pos inicial
crearCaminoArriba :: Direccion -> [Casillero]  -> (Integer,Integer) -> [Casillero]
crearCaminoArriba _ [] _ = []
crearCaminoArriba direccion (x:xs) (a,b)
    | ((a,b) == (0,0) || (a,b) == (0,1) || (a,b) == (0,2)) && direccion == Arriba = (esPosible (x:xs) (obtenerCasilla (x:xs) (a,b)))
    | otherwise = (obtenerCasilla (x:xs) (a,b)) : [casillasSiguienteDireccion direccion (x:xs) (a,b)] ++ (esPosible (crearCaminoArriba direccion (xs) (a-1,b)) (obtenerCasilla (crearCaminoArriba direccion (xs) (a-1,b)) (a-1,b)))

crearCaminoAbajo :: Direccion -> [Casillero]  -> (Integer,Integer) -> [Casillero]
crearCaminoAbajo _ [] _ = []
crearCaminoAbajo direccion (x:xs) (a,b)
    | ((a,b) == (2,0) || (a,b) == (2,1) || (a,b) == (2,2)) && direccion == Abajo = (esPosible (x:xs) (obtenerCasilla (x:xs) (a,b)))
    | otherwise = (obtenerCasilla (x:xs) (a,b)) : [casillasSiguienteDireccion direccion (x:xs) (a,b)] ++ (esPosible (crearCaminoArriba direccion (xs) (a+1,b)) (obtenerCasilla (crearCaminoArriba direccion (xs) (a+1,b)) (a+1,b)))



crearCaminoDerecha :: Direccion -> [Casillero]  -> (Integer,Integer) -> [Casillero]
crearCaminoDerecha _ [] _ = []
crearCaminoDerecha direccion (x:xs) (a,b)
    | ((a,b) == (0,2) || (a,b) == (1,2) || (a,b) == (2,2)) && direccion == Derecha = [(obtenerCasilla (x:xs) (a,b)) ] 
    | otherwise = (obtenerCasilla (x:xs) (a,b)) : [casillasSiguienteDireccion direccion (x:xs) (a,b)] ++ (esPosible (crearCaminoArriba direccion (xs) (a,b+1)) (obtenerCasilla (crearCaminoArriba direccion (xs) (a,b+1)) (a,b+1)))

crearCaminoIzquierda :: Direccion -> [Casillero]  -> (Integer,Integer) -> [Casillero]
crearCaminoIzquierda _ [] _ = []
crearCaminoIzquierda direccion (x:xs) (a,b)
    | ((a,b) == (0,0) || (a,b) == (0,1) || (a,b) == (0,2)) && direccion == Arriba = [(obtenerCasilla (x:xs) (a,b))] 
    | otherwise = (obtenerCasilla (x:xs) (a,b)) : [casillasSiguienteDireccion direccion (x:xs) (a,b)] ++ (esPosible (crearCaminoArriba direccion (xs) (a,b-1)) (obtenerCasilla (crearCaminoArriba direccion (xs) (a,b-1)) (a,b-1)))

esPosible :: [Casillero] -> Casillero -> [Casillero]
esPosible [] _ = []
esPosible (x:xs) casilla = if elem casilla (casillasPosibles (x:xs)) then [casilla] else []

crearCamino :: Direccion -> [Casillero] -> (Integer,Integer) -> [Casillero]
crearCamino _ [] _ = []
crearCamino direccion (x:xs) (a,b)
    | direccion == Arriba = let casillero = (quitarDuplicados (crearCaminoArriba direccion (x:xs) (a,b))) in (quitarError casillero (10,10) 10)
    | direccion == Abajo = let casillero = (quitarDuplicados (crearCaminoAbajo direccion (x:xs) (a,b))) in (quitarError casillero (10,10) 10)
    | direccion == Derecha = let casillero = (quitarDuplicados (crearCaminoDerecha direccion (x:xs) (a,b))) in (quitarError casillero (10,10) 10)
    | direccion == Izquierda = let casillero = (quitarDuplicados (crearCaminoIzquierda direccion (x:xs) (a,b))) in (quitarError casillero (10,10) 10)
    | otherwise = []





casillasSiguienteDireccion :: Direccion -> [Casillero] -> (Integer,Integer) -> Casillero
casillasSiguienteDireccion dir (x:xs) (a,b) 
    | dir == Arriba = (devolverCamino [((seMueveArriba (obtenerCasilla (x:xs) (a,b)) x),x) | x <- ((intersectCercaYPosible (obtenerCasilla (x:xs) (a,b))  (x:xs)))])
    | dir == Abajo = (devolverCamino [((seMueveAbajo (obtenerCasilla (x:xs) (a,b)) x),x) | x <- ((intersectCercaYPosible (obtenerCasilla (x:xs) (a,b))  (x:xs)))])
    | dir == Derecha = (devolverCamino [((seMueveDerecha (obtenerCasilla (x:xs) (a,b)) x),x) | x <- ((intersectCercaYPosible (obtenerCasilla (x:xs) (a,b))  (x:xs)))])
    | dir == Izquierda = (devolverCamino [((seMueveIzquierda (obtenerCasilla (x:xs) (a,b)) x),x) | x <- (intersectCercaYPosible (obtenerCasilla (x:xs) (a,b))  (x:xs))])


obtenerCasilla :: [Casillero] -> (Integer,Integer) -> Casillero
obtenerCasilla [] _ = ("z",(10,10))
obtenerCasilla ((caracteres,(x,y)):xs) (a,b)
    | x == a && y == b = (caracteres,(x,y))
    | otherwise = obtenerCasilla xs (a,b)


quitarError :: [Casillero] -> (Integer,Integer) -> Int -> [Casillero]
quitarError [] _ _= []
quitarError ((caracteres,(x,y)):xs) (a,b) contador
    | x == a && y == b = (take contador xs)
    | otherwise = (caracteres,(x,y)) : quitarError xs (a,b) (contador+1)

devolverBool :: [(Bool,Casillero)] -> Bool
devolverBool [] = False
devolverBool ((bool,casillero):xs)
    | bool == True = True
    | otherwise = devolverBool xs

devolverCamino :: [(Bool, Casillero)] -> Casillero
devolverCamino [] = ("z",(10,10))
devolverCamino ((bool,casillero):xs)
    | bool == True = casillero
    | otherwise = devolverCamino xs


desapilarDeCasilla :: Casillero -> Int -> (Casillero, [Char])
desapilarDeCasilla (caracteres,(a,b)) cantidad = (((take ((length caracteres) - cantidad) caracteres),(a,b)),(drop ((length caracteres)-cantidad) caracteres))


intersectCercaYPosible :: Casillero -> [Casillero] -> [Casillero]
intersectCercaYPosible casilla (x:xs) = intersect (casillasPosibles (x:xs)) (casillasCercanas (x:xs) casilla)

posibilidadesCasillero :: Casillero -> [Char]
posibilidadesCasillero (caracteres,(a,b))
    | length caracteres >= 4 = drop 4 caracteres
    | otherwise = take (length caracteres ) caracteres

seMueveDerecha :: Casillero -> Casillero -> Bool
seMueveDerecha (caracteres,(x,y)) (caracteres2,(a,b)) = (x == a && y+1 == b)

seMueveIzquierda :: Casillero -> Casillero -> Bool
seMueveIzquierda (caracteres,(x,y)) (caracteres2,(a,b)) = (x == a && y-1 == b)

seMueveArriba :: Casillero -> Casillero -> Bool
seMueveArriba (caracteres,(x,y)) (caracteres2,(a,b)) = (x-1 == a && y == b)

seMueveAbajo :: Casillero -> Casillero -> Bool
seMueveAbajo (caracteres,(x,y)) (caracteres2,(a,b)) = (x+1 == a && y == b)

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
impresionJuego3x3 juego = unlines $ [(caracterPosicion juego 0) ++ "      " ++  (caracterPosicion juego 1) ++ "      " ++ (caracterPosicion juego 2) ++'\n': 
    (caracterPosicion juego 3) ++ "      " ++ (caracterPosicion juego 4) ++ "      " ++ (caracterPosicion juego 5) ++ '\n':
    (caracterPosicion juego 6) ++ "      " ++ (caracterPosicion juego 7) ++ "      " ++ (caracterPosicion juego 8)]

impresionJuego4x4 :: TakGame -> String
impresionJuego4x4 juego = unlines $ [(caracterPosicion juego 0) ++ "      " ++  (caracterPosicion juego 1) ++ "      " ++ (caracterPosicion juego 2) ++ "      " ++ (caracterPosicion juego 3) ++'\n': 
     (caracterPosicion juego 4) ++ "      " ++ (caracterPosicion juego 5) ++ "      " ++ (caracterPosicion juego 6) ++ "      " ++ (caracterPosicion juego 7) ++'\n':
     (caracterPosicion juego 8) ++ "      " ++ (caracterPosicion juego 9) ++ "      " ++ (caracterPosicion juego 10) ++ "      " ++ (caracterPosicion juego 11) ++ '\n':
     (caracterPosicion juego 12) ++ "      " ++ (caracterPosicion juego 13) ++ "      " ++ (caracterPosicion juego 14) ++ "      " ++ (caracterPosicion juego 15)]

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

{-
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
-}

instance Show TakAction where
    show (Insertar (x, y) pared) = if (pared) then
                                        "| Insertar una pared en " ++ (show (x,y)) ++ " |"
                                    else
                                        "| Insertar una ficha plana en " ++ (show (x,y)) ++ " |"

    show (Mover (x, y) (x2, y2)) = "| Mover de: " ++ (show (x,y)) ++ "hacia: " ++ (show (x2, y2)) ++ " |"
    show (Desapilar (x,y) cantidad direccion) = "| Desapilar desde : " ++ (show (x,y)) ++ " en :" ++ (show cantidad) ++ " en direccion: " ++ (show direccion) ++ " |"


-- Match controller -------------------------------------------------------------------------------
-- Código de prueba. Incluye una función para correr las partidas y dos agentes: consola y aleatorio.
type TakAgent = TakGame -> IO (Maybe TakAction)
{- La función ´runMatch´ corre la partida completa a partir del estado de juego dado, usando los dos 
agentes dados. Retorna una tupla con los puntajes (score) finales del juego.
-}
runMatch :: (TakAgent, TakAgent) -> TakGame -> IO [(TakPlayer, Int)]
runMatch ags@(ag1, ag2) g = do
   putStrLn (showBoard g)
   case (activePlayer2 g) of
      Nothing -> return $ result g
      Just p -> do
         let ag = [ag1, ag2] !! (fromJust (elemIndex p players))
         move <- ag g
         runMatch ags (next g (p, fromJust move))
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