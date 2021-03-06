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
data TakAction = Insertar (Integer, Integer) Bool | Mover (Integer, Integer) (Integer, Integer) | Desapilar (Integer,Integer) [Integer] Direccion deriving (Eq)       -- bool insertar True = Pared
data TakPlayer = WhitePlayer | BlackPlayer deriving (Eq, Show, Enum)

coordenadasCasillero3x3 = (map (\n -> ("",(divMod n 3)))  [0..8])
coordenadasCasillero4x4 = map (\n -> ("",(divMod n 4)))  [0..15]

-- ejemplo de TakGame
juego3x3 = ([("o",(0,0)) ,("O",(0,1)),("O",(0,2)),("oxox",(1,0)),("X",(1,1)),("",(1,2)),("x",(2,0)),("o",(2,1)),("",(2,2))],BlackPlayer)

casillero3X3 = (fst juego3x3)
juego3x32 = ([("o",(0,0)) ,("O",(0,1)),("O",(0,2)),("o",(1,0)),("x",(1,1)),("",(1,2)),("o",(2,0)),("o",(2,1)),("",(2,2))],BlackPlayer)
cCerc = [("",(0,0)),("X",(0,1)),("",(0,2)),("",(1,0)),("",(1,1)),("",(1,2)),("",(2,0)),("",(2,1)),("",(2,2))]
juego4x4 = (map (\n -> ("x",(divMod n 4)))  [0..15], WhitePlayer)
juegoVacio = ([],BlackPlayer)
casilleroVacio = [("",(0,0)),("",(0,1)),("",(0,2)),("",(1,0)),("",(1,1)),("",(1,2)),("",(2,0)),("",(2,1)),("",(2,2))]


----------------------------------------------------------------- METODOS SOLICITADOS -------------------------------------------------------------

beginning3x3 :: TakGame
beginning3x3 = (coordenadasCasillero3x3, BlackPlayer)

beginning4x4 :: TakGame
beginning4x4 = (coordenadasCasillero4x4, BlackPlayer)

actions :: TakGame -> [(TakPlayer, [TakAction])]
actions juego = [(activePlayer2 juego, (generarAccionesInsertar (obtenerCasillero juego)) ++ 
                generarAccionesMover (borrarCasosNoPosibles (activePlayer2 juego) (casillasParaMover (obtenerCasillero juego)) )
                ++ filtrarDesapilados juego (generarAccionesDesapilarJuego juego (obtenerCasillero juego))), (nonActivePlayer2 juego, [])]
                

activePlayer2 :: TakGame -> TakPlayer
activePlayer2 (g, jugador) = jugador

result :: TakGame -> [(TakPlayer, Int)]
result juego =  if (juegoTerminado juego) then
                    [(activePlayer2 juego, 1), (nonActivePlayer2 juego, (-1))]
                else
                    []


next :: TakGame -> (TakPlayer, TakAction) -> TakGame
next juego (jugador, (Insertar (x,y) pared))
    | elem (Insertar (x,y) pared) (snd (head (actions juego))) = realizarAccionInsertar (obtenerCasillero juego) (jugador, (Insertar (x,y) pared))
    | otherwise = error "accion invalida"

next juego (jugador, (Mover (x,y) (x2, y2)))
    | elem (Mover (x,y) (x2, y2)) (snd (head (actions juego))) = realizarAccionMover (obtenerCasillero juego) (jugador, (Mover (x,y) (x2, y2)))
    | otherwise = error "accion invalida"

next juego (jugador, (Desapilar (x,y) cantidades direccion))
    | elem (Desapilar (x,y) cantidades direccion) (snd (head (actions juego))) = realizarAccionDesapilarManager juego (jugador, (Desapilar (x,y) cantidades direccion))
    | otherwise = error "accion invalida"


score :: TakGame -> [(TakPlayer, Int)]
score juego = [(activePlayer2 juego, puntajeJugador (activePlayer2 juego) juego), (nonActivePlayer2 juego, puntajeJugador (nonActivePlayer2 juego) juego)]

showAction :: TakAction -> String
showAction (Insertar (x, y) pared) = if (pared) then
                                        "||Insertar una pared en " ++ (show (x,y))
                                    else
                                        "||Insertar una ficha plana en " ++ (show (x,y))

showAction (Mover (x, y) (x2, y2)) = "||Mover de: " ++ (show (x,y)) ++ "hacia: " ++ (show (x2, y2))

showAction (Desapilar (x,y) cantidad direccion) =  "||Desapilar desde : " ++ (show (x,y)) ++ "en :" ++ (show cantidad) ++ " en direccion : " ++ (show direccion)

showAction2 :: [TakAction] -> String
showAction2 [] = "no hay mas acciones para mostrar "
showAction2 (x:xs) = (showAction x) ++ (showAction2 xs)

showAction3 :: [(TakPlayer, [TakAction])] -> String
showAction3 [] = " juego vacio"
showAction3 (x:y:xs) = (showAction2 (snd x)) ++ (showAction2 (snd y)) ++ showAction3 xs

readAction :: String -> TakAction
readAction mensaje = 
    if (head mensaje == '3') then -- tablero
        if (mensaje !! 1 == 'i') then -- inserta mueve desapila
            if (isDigit (mensaje!!2)) then 
                if (mensaje !! 3 == 'p') then 
                    (Insertar (posicionACoordenadas3x3 (fromIntegral (digitToInt (mensaje!!2)))) False) 
                else if (mensaje !! 3 == 'P') then
                    (Insertar (posicionACoordenadas3x3 (fromIntegral (digitToInt (mensaje!!2)))) True) 
                else
                    error "formato invalido"
            else
                error "formato invalido"
        else if (mensaje !! 1 == 'm') then
            if (isDigit (mensaje!!2)) && (isDigit (mensaje!!3)) then
                (Mover (posicionACoordenadas3x3 (fromIntegral (digitToInt (mensaje!!2)))) (posicionACoordenadas3x3 (fromIntegral (digitToInt (mensaje!!3)))))
            else
                error "formato invalido"
        else if (mensaje !! 1 == 'd') then -- desapila
            if (isDigit (mensaje !! 2)) then -- origen
                    -- cantidad 1               -- cantidad 2
                if (isDigit (mensaje !! 3)) && (isDigit (mensaje !! 4)) then -- la lista de los que dejas por casilla
                    if (mensaje !! 5 == 'u') then
                        (Desapilar (posicionACoordenadas3x3 (fromIntegral ((digitToInt (mensaje!!2))))) [(fromIntegral (digitToInt (mensaje!!3))), (fromIntegral (digitToInt (mensaje!!4)))] Arriba) 
                    else if (mensaje !! 5 == 'd') then
                        (Desapilar (posicionACoordenadas3x3 (fromIntegral (digitToInt (mensaje!!2)))) [(fromIntegral (digitToInt (mensaje!!3))), (fromIntegral (digitToInt (mensaje!!4)))] Abajo) 
                    else if (mensaje !! 5 == 'l') then 
                        (Desapilar (posicionACoordenadas3x3 (fromIntegral (digitToInt (mensaje!!2)))) [(fromIntegral (digitToInt (mensaje!!3))), (fromIntegral (digitToInt (mensaje!!4)))] Izquierda)
                    else if (mensaje !! 5 == 'r') then 
                        (Desapilar (posicionACoordenadas3x3 (fromIntegral (digitToInt (mensaje!!2)))) [(fromIntegral (digitToInt (mensaje!!3))), (fromIntegral (digitToInt (mensaje!!4)))] Derecha)
                    else 
                        error "formato invalido"
                else 
                    error "formato invalido"
            else 
                error "formato invalido"
        else
            error "formato invalido"
            
            
    else if (head mensaje == '4') then
            if (mensaje !! 1 == 'i') then
                if (isDigit (mensaje!!2) && isDigit (mensaje!!3)) then
                    if (mensaje !! 4 == 'p') then
                        (Insertar (posicionACoordenadas4x4 (fromIntegral (digitToInt (mensaje!!2))*10 + fromIntegral (digitToInt (mensaje!!3)))) False) 
                    else if (mensaje !! 3 == 'P') then
                            (Insertar (posicionACoordenadas4x4 (fromIntegral (digitToInt (mensaje!!2))*10 + fromIntegral (digitToInt (mensaje!!3)))) True)
                    else
                        error "formato invalido"
            else
                error "formato invalido"
        else if (mensaje !! 1 == 'm') then
            if (isDigit (mensaje!!2) && isDigit (mensaje!!3) && isDigit (mensaje!!4) && isDigit (mensaje!!5)) then
                (Mover (posicionACoordenadas4x4 (fromIntegral (digitToInt (mensaje!!2))*10 + fromIntegral (digitToInt (mensaje!!3)))) (posicionACoordenadas4x4 (fromIntegral (digitToInt (mensaje!!4))*10 + fromIntegral (digitToInt (mensaje!!5)))))
            else 
                error "formato invalido"
        else if (mensaje !! 1 == 'd') then -- desapila
            if (isDigit (mensaje !! 2)) then -- origen
                    -- cantidad 1               -- cantidad 2               -- cantidad 3
                if (isDigit (mensaje !! 3)) && (isDigit (mensaje !! 4)) && (isDigit (mensaje !! 5)) then -- la lista de los que dejas por casilla
                    if (mensaje !! 6 == 'u') then
                        (Desapilar (posicionACoordenadas4x4 (fromIntegral (digitToInt (mensaje!!2)))) [(fromIntegral (digitToInt (mensaje!!3))), (fromIntegral (digitToInt (mensaje!!4))), (fromIntegral (digitToInt (mensaje!!5)))] Arriba) 
                    else if (mensaje !! 6 == 'd') then
                        (Desapilar (posicionACoordenadas4x4 (fromIntegral (digitToInt (mensaje!!2)))) [(fromIntegral (digitToInt (mensaje!!3))), (fromIntegral (digitToInt (mensaje!!4))), (fromIntegral (digitToInt (mensaje!!5)))] Abajo) 
                    else if (mensaje !! 6 == 'l') then 
                        (Desapilar (posicionACoordenadas4x4 (fromIntegral (digitToInt (mensaje!!2)))) [(fromIntegral (digitToInt (mensaje!!3))), (fromIntegral (digitToInt (mensaje!!4))), (fromIntegral (digitToInt (mensaje!!5)))] Izquierda)
                    else if (mensaje !! 6 == 'r') then 
                        (Desapilar (posicionACoordenadas4x4 (fromIntegral (digitToInt (mensaje!!2)))) [(fromIntegral (digitToInt (mensaje!!3))), (fromIntegral (digitToInt (mensaje!!4))), (fromIntegral (digitToInt (mensaje!!5)))] Derecha)
                    else 
                        error "formato invalido"
                else
                    error "formato invalido"
            else 
                error "formato invalido"
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

activePlayer :: TakGame -> Maybe TakPlayer
activePlayer g = listToMaybe [p | (p, as) <- actions g, not (null as)]

players = [WhitePlayer, BlackPlayer]

----------------------------------------------------------------- METODOS AUXILIARES -------------------------------------------------------------


outerProduct xs ys =
   do
       x <- xs
       y <- ys
       return (x,y)


nonActivePlayer2 :: TakGame -> TakPlayer
nonActivePlayer2 (g, WhitePlayer) = BlackPlayer
nonActivePlayer2 (g, BlackPlayer) = WhitePlayer


juegoSinComenzar :: TakGame -> Bool
juegoSinComenzar ((x:xs),y)
    | (x:xs) == [] = True
    | (fst x ) == "x" || (fst x ) == "X" || (fst x ) == "o"  || (fst x ) == "O" = False
    | otherwise = True


listaIntToInteger :: [Int] -> [Integer]
listaIntToInteger [] = []
listaIntToInteger (x:xs) = fromIntegral x : listaIntToInteger xs

listaIntegerToInt :: [Integer] -> [Int]
listaIntegerToInt [] = []
listaIntegerToInt (x:xs) = fromInteger x : listaIntegerToInt xs

f2 :: a -> [a] ->[(a,a)]
f2 a [] = []
f2 a (h:t) = (a,h):(f2 a t)

f3 :: [a] -> a -> [(a,a)]
f3 [] a = []
f3 (x:xs) a = (x,a): f3 xs a


quitarDuplicados :: (Eq a) => [a] -> [a]
quitarDuplicados [] = []
quitarDuplicados (x:xs)
    | elem x xs = quitarDuplicados xs
    | otherwise = x : quitarDuplicados xs
    
quitarDuplicados2 :: (Eq a) => [[a]] -> [[a]]
quitarDuplicados2 [[]] = [[]]
quitarDuplicados2 (x:xs)
    | elem x xs = quitarDuplicados2 xs
    | otherwise = x : quitarDuplicados2 xs

generarPosibilidades :: [Integer] -> Integer -> [[Integer]]
generarPosibilidades pos objetivo = hacerPosibilidad' [] pos (fromIntegral (length (listaIntegerToInt pos))) objetivo
  where
    hacerPosibilidad' :: [Integer] -> [Integer] -> Integer -> Integer -> [[Integer]]
    hacerPosibilidad' hacer pos indice objetivo
      | objetivo <  0 = []
      | objetivo == 0 = [hacer]
      | indice == 0 && objetivo >= 1 = []
      | otherwise = (hacerPosibilidad' hacer pos (fromInteger indice - 1) objetivo) ++ (hacerPosibilidad' (hacer  ++ [pos !! (fromInteger indice - 1)]) pos indice (objetivo - (pos !! (fromInteger indice - 1))))

permutaciones :: [a] -> [[a]]
permutaciones []     = [[]]
permutaciones (x:xs) = 
    concat [intercala x ys | ys <- permutaciones xs]

intercala :: a -> [a] -> [[a]]
intercala x [] = [[x]]
intercala x (y:ys) = (x:y:ys) : [y:zs | zs <- intercala x ys]

posibilidades2 :: [Integer] -> Integer -> [[Integer]]
posibilidades2 [] _ = []
posibilidades2  list num =  init (init([tail x | x <- (quitarDuplicados2 (posibilidades (generarPosibilidades list num)))]))

posibilidades :: [[Integer]] -> [[Integer]]
posibilidades [] = [[]]
posibilidades (x:xs) = permutaciones x ++ posibilidades xs


direccion :: Casillero -> Casillero -> Direccion
direccion (caracteres,(x,y)) (caracteres2,(x2,y2))
    | x > x2 && y == y2 = Arriba
    | x < x2 && y == y2 = Abajo
    | x == x2 && y > y2 = Derecha
    | x == x2 && y < y2 = Izquierda 


cadenaConLista :: [Char] -> [Int] -> [([Char])]
cadenaConLista [] _ = []
cadenaConLista _ [] = []
cadenaConLista cadena (x:xs) = (take  x cadena) : cadenaConLista (drop x cadena) xs



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


-------------------------------------------------------Sobre Casilleros -------------------------------------------------------------------
buscarEnCasillero :: [Casillero] -> Casillero -> [Casillero]
buscarEnCasillero [] _ = []
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


contenidoCasillero :: [Casillero] -> (Integer, Integer) -> [Char]
contenidoCasillero ((caracteres, (a,b)):xs) (x,y)
    | (a == x && b == y) = caracteres
    | otherwise = contenidoCasillero xs (x,y)
contenidoCasillero [] _ = error "casillero no encontrado"

contenidoCasillero2 :: Casillero -> [Char]
contenidoCasillero2 (caracteres,(x,y)) = caracteres


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


insertarCaracteres :: Casillero -> [Char] -> Casillero  --concatena una cadena de caracteres en un casillero
insertarCaracteres cas newCaracter = ((fst cas ++ newCaracter),snd cas)



cambiarCasilla :: Casillero -> [Casillero] -> [Casillero]
cambiarCasilla (caracteres,(x,y)) ((caracteres2,(a,b)):xs)
    | (x,y) == (a,b) = ((caracteres,(a,b)):xs)
    | otherwise = (caracteres2,(a,b)) : cambiarCasilla (caracteres,(x,y)) xs

esPosible :: [Casillero] -> Casillero -> [Casillero]
esPosible [] _ = []
esPosible (x:xs) casilla = if elem casilla (casillasPosibles (x:xs)) then [casilla] else []


obtenerCasilla :: [Casillero] -> (Integer,Integer) -> Casillero
obtenerCasilla [] _ = ("z",(10,10))
obtenerCasilla ((caracteres,(x,y)):xs) (a,b)
    | x == a && y == b = (caracteres,(x,y))
    | otherwise = obtenerCasilla xs (a,b)


---------------------------------------- Accion Insertar -------------------------------------------------------


generarAccionesInsertar :: [Casillero] -> [TakAction]
generarAccionesInsertar [] = []
generarAccionesInsertar ((caracteres, (a,b)):xs)
    | caracteres == "" = (Insertar (a,b) True) : (Insertar (a,b) False) : generarAccionesInsertar xs
    | otherwise = generarAccionesInsertar xs


realizarAccionInsertar :: [Casillero] -> (TakPlayer,TakAction) -> TakGame
realizarAccionInsertar ((caracteres,(x,y)):xs) (jugadorAct, (Insertar (a,b) bool)) = 
        if (controlDeCaracteres (caracteres,(x,y))) then
            if jugadorAct == WhitePlayer then
                if bool then ((buscarEnCasillero ((caracteres,(x,y)):xs) ("X",(a,b))),BlackPlayer)
                else ((buscarEnCasillero ((caracteres,(x,y)):xs) ("x",(a,b))),BlackPlayer)
            else 
                if bool then ((buscarEnCasillero ((caracteres,(x,y)):xs) ("O",(a,b))), WhitePlayer)
                else ((buscarEnCasillero ((caracteres,(x,y)):xs) ("o",(a,b))), WhitePlayer)
        else error "No se puede insertar a una pila"


intersectCercaYPosible :: Casillero -> [Casillero] -> [Casillero]
intersectCercaYPosible casilla (x:xs) = intersect (casillasPosibles (x:xs)) (casillasCercanas (x:xs) casilla)

posibilidadesCasillero :: Casillero -> [Char]
posibilidadesCasillero (caracteres,(a,b))
    | length caracteres >= 4 = drop 4 caracteres
    | otherwise = take (length caracteres ) caracteres



------------------------------------------------------------------------------------------


------------------------------- Accion Mover-------------------------------------------

generarAccionesMover :: [(Casillero, Casillero)] -> [TakAction]
generarAccionesMover (((_, (a,b)),(_, (a2,b2))):xs)  = (Mover (a,b) (a2,b2)) : generarAccionesMover xs
generarAccionesMover [] = []

realizarAccionMover :: [Casillero] -> (TakPlayer, TakAction) -> TakGame
realizarAccionMover tablero (WhitePlayer, (Mover (xOrigen, yOrigen) (xDestino, yDestino))) = (eliminarUltimaPosicion (buscarEnCasillero tablero (topeDePila tablero (xOrigen, yOrigen), (xDestino, yDestino))) (xOrigen, yOrigen), BlackPlayer)
realizarAccionMover tablero (BlackPlayer, (Mover (xOrigen, yOrigen) (xDestino, yDestino))) = (eliminarUltimaPosicion (buscarEnCasillero tablero (topeDePila tablero (xOrigen, yOrigen), (xDestino, yDestino))) (xOrigen, yOrigen), WhitePlayer)

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





--------------------------------- Accion Desapilar ---------------------


desapiladosPosiblesArriba :: [Casillero] -> (Integer,Integer) -> Int -> [(Casillero, [Integer])]
desapiladosPosiblesArriba juego (x, y) cantidad
    | length (juego) == 9 = filter (\z -> length (snd z) <= length (crearCamino3X3 Arriba juego (snd (fst z)))) (outerProduct (crearCamino3X3 Arriba juego (x,y))  (posibilidades2 (listaIntToInteger [1..cantidad]) (fromIntegral cantidad)))
    | length (juego) == 16 = filter (\z -> length (snd z) <= length (crearCamino4X4 Arriba juego (snd (fst z)))) (outerProduct (crearCamino4X4 Arriba juego (x,y))  (posibilidades2 (listaIntToInteger [1..cantidad]) (fromIntegral cantidad)))


desapiladosPosiblesAbajo :: [Casillero] -> (Integer,Integer) -> Int -> [(Casillero, [Integer])]
desapiladosPosiblesAbajo juego (x, y) cantidad
    | length (juego) == 9 = filter (\z -> length (snd z) <= length (crearCamino3X3 Abajo juego (snd (fst z)))) (outerProduct (crearCamino3X3 Abajo juego (x,y)) (posibilidades2 (listaIntToInteger [1..cantidad]) (fromIntegral cantidad))) 
    | length (juego) == 16 = filter (\z -> length (snd z) <= length (crearCamino4X4 Abajo juego (snd (fst z)))) (outerProduct (crearCamino4X4 Abajo juego (x,y)) (posibilidades2 (listaIntToInteger [1..cantidad]) (fromIntegral cantidad))) 


desapiladosPosiblesIzquierda :: [Casillero] -> (Integer,Integer) -> Int -> [(Casillero, [Integer])]
desapiladosPosiblesIzquierda juego (x, y) cantidad
    | length (juego) == 9 = filter (\z -> length (snd z) <= length (crearCamino3X3 Izquierda juego (snd (fst z)))) (outerProduct (crearCamino3X3 Izquierda juego (x,y)) (posibilidades2 (listaIntToInteger [1..cantidad]) (fromIntegral cantidad))) 
    | length (juego) == 16 = filter (\z -> length (snd z) <= length (crearCamino4X4 Izquierda juego (snd (fst z)))) (outerProduct (crearCamino4X4 Izquierda juego (x,y)) (posibilidades2 (listaIntToInteger [1..cantidad]) (fromIntegral cantidad))) 

desapiladosPosiblesDerecha :: [Casillero] -> (Integer,Integer) -> Int -> [(Casillero, [Integer])]
desapiladosPosiblesDerecha juego (x, y) cantidad
    | length (juego) == 9 = filter (\z -> length (snd z) <= length (crearCamino3X3 Derecha juego (snd (fst z)))) (outerProduct (crearCamino3X3 Derecha juego (x,y)) (posibilidades2 (listaIntToInteger [1..cantidad]) (fromIntegral cantidad))) 
    | length (juego) == 16 = filter (\z -> length (snd z) <= length (crearCamino4X4 Derecha juego (snd (fst z)))) (outerProduct (crearCamino4X4 Derecha juego (x,y)) (posibilidades2 (listaIntToInteger [1..cantidad]) (fromIntegral cantidad))) 

accionesDesapilados :: [(Casillero, [Integer])] -> Direccion -> [TakAction]
accionesDesapilados [] _ = []
accionesDesapilados (((caracteres, (x,y)), lista):xs) direccion = (Desapilar (x, y) (lista) direccion) : accionesDesapilados xs direccion

realizarAccionDesapilarManager :: TakGame -> (TakPlayer, TakAction) -> TakGame
realizarAccionDesapilarManager juego (jugador, accion)
    | length (obtenerCasillero juego) == 9 = realizarAccionDesapilar3x3 juego (jugador, accion)
    | length (obtenerCasillero juego) == 16 = realizarAccionDesapilar4x4 juego (jugador, accion)




generarAccionesDesapilarParaUnCasillero :: TakGame -> Casillero -> Int -> [TakAction]
generarAccionesDesapilarParaUnCasillero ([],_) _ _ = []
generarAccionesDesapilarParaUnCasillero (juego, jugador) (caracteres, (x,y)) num = 
    if (num >= 1) then (accionesDesapilados (desapiladosPosiblesAbajo (juego) (x,y) num) Abajo) ++ 
                        (accionesDesapilados (desapiladosPosiblesArriba (juego) (x,y) num) Arriba) ++ 
                        (accionesDesapilados (desapiladosPosiblesDerecha (juego) (x,y) num) Derecha) ++ 
                        (accionesDesapilados (desapiladosPosiblesIzquierda (juego) (x,y) num) Arriba) ++ 
                        generarAccionesDesapilarParaUnCasillero (juego, jugador) (caracteres, (x,y)) (num-1)
    else []

generarAccionesDesapilarJuego :: TakGame -> [Casillero] -> [TakAction]
generarAccionesDesapilarJuego _ [] = []
generarAccionesDesapilarJuego juego (x:xs)  = (generarAccionesDesapilarParaUnCasillero juego x (length (contenidoCasillero2 x))) ++ generarAccionesDesapilarJuego juego xs 

filtrarDesapilados :: TakGame -> [TakAction] -> [TakAction]
filtrarDesapilados ([],_) [] = []
filtrarDesapilados _ [] = []
filtrarDesapilados (juego,jugador) ((Desapilar (x,y) (z:zs) direccion):ds) =
    if (length juego == 9) then
        if length (contenidoCasillero juego (x,y)) >= (foldr (+) 0 (listaIntegerToInt (z:zs))) &&
            (length (contenidoCasillero juego (x,y)) >= 2) &&
            (length (crearCamino3X3 direccion juego (x,y)) - 1) >= length (z:zs) then
  
            (Desapilar (x,y) (z:zs) direccion) : (filtrarDesapilados (juego,jugador) ds)

        else 
            filtrarDesapilados (juego,jugador) ds
    else if (length juego == 16) then
        if length (contenidoCasillero juego (x,y)) >= (foldr (+) 0 (listaIntegerToInt (z:zs)))  &&
            length (contenidoCasillero juego (x,y)) >= 2 &&
            length (crearCamino4X4 direccion juego (x,y)) - 1 >= length (z:zs) then
  
            (Desapilar (x,y) (z:zs) direccion) : (filtrarDesapilados (juego,jugador) ds)

        else 
            filtrarDesapilados (juego,jugador) ds
    else
        error "!"

realizarAccionDesapilar3x3 :: TakGame -> (TakPlayer, TakAction) -> TakGame
realizarAccionDesapilar3x3 (juego, jugador) (jugadorAct, (Desapilar (a,b) (z:zs) direccion))
    | direccion == Arriba = (desapilarArriba3x3 juego  juego (jugadorAct, (Desapilar (a,b) (z:zs) direccion)), nonActivePlayer2 (juego, jugador))
    | direccion == Izquierda = (desapilarIzquierda3x3 juego juego (jugadorAct, (Desapilar (a,b) (z:zs) direccion)), nonActivePlayer2 (juego, jugador))
    | direccion == Abajo = ((desapilarAbajo3x3 juego juego (jugadorAct, (Desapilar (a,b) (z:zs) direccion)), nonActivePlayer2 (juego, jugador)))
    | direccion == Derecha = ((desapilarDerecha3x3 juego juego (jugadorAct, (Desapilar (a,b) (z:zs) direccion)), nonActivePlayer2 (juego, jugador)))


realizarAccionDesapilar4x4 :: TakGame -> (TakPlayer, TakAction) -> TakGame
realizarAccionDesapilar4x4 (juego, jugador) (jugadorAct, (Desapilar (a,b) (z:zs) direccion))
    | direccion == Arriba = (desapilarArriba4x4 juego  juego (jugadorAct, (Desapilar (a,b) (z:zs) direccion)), nonActivePlayer2 (juego, jugador))
    | direccion == Izquierda = (desapilarIzquierda4x4 juego juego (jugadorAct, (Desapilar (a,b) (z:zs) direccion)), nonActivePlayer2 (juego, jugador))
    | direccion == Abajo = ((desapilarAbajo4x4 juego juego (jugadorAct, (Desapilar (a,b) (z:zs) direccion)), nonActivePlayer2 (juego, jugador)))
    | direccion == Derecha = ((desapilarDerecha4x4 juego juego (jugadorAct, (Desapilar (a,b) (z:zs) direccion)), nonActivePlayer2 (juego, jugador)))



insertarEnCasilleroSigPos :: [Casillero] -> Direccion -> (Integer,Integer) -> [Char] -> [Casillero]
insertarEnCasilleroSigPos [] _ _ _ = []
insertarEnCasilleroSigPos ((caracteres,(x,y)):xs) direccion (a,b) cad = 
    let casSig = (casillaSiguienteDireccion direccion ((caracteres,(x,y)):xs) (a,b)) in buscarEnCasillero ((caracteres,(x,y)):xs) (cad,(a,b))


desapilarDeCasilla :: Casillero -> Int -> (Casillero, ([Char]))
desapilarDeCasilla (caracteres,(a,b)) cantidad = (((take ((length caracteres) - cantidad) caracteres),(a,b)),(drop ((length caracteres)-cantidad) caracteres))

desapilarDeCasilla2 :: Casillero -> [Integer] -> (Casillero,[[Char]])
desapilarDeCasilla2 (caracteres,(a,b)) (z:zs) = (((take ((length caracteres) - (foldr (+) 0 (listaIntegerToInt (z:zs)))) caracteres),(a,b)), cadenaConLista (drop ((length caracteres) - (foldr (+) 0 (listaIntegerToInt (z:zs))))  caracteres) (listaIntegerToInt (z:zs)))


desapilarArriba3x3 :: [Casillero] -> [Casillero] -> (TakPlayer, TakAction) -> [Casillero]
desapilarArriba3x3 _ _ (_ ,(Desapilar _ [] _)) = error "Lista vacia"
desapilarArriba3x3 [] _ _ = error "Casillero Vacio 1"
desapilarArriba3x3 _ [] _ = error "Casillero Vacio 2" 
desapilarArriba3x3 juego ((caracteres,(x,y)):xs) (jugadorAct, (Desapilar (a,b) (z:zs) Arriba)) =
    if (a,b) == (0,0) || (a,b) == (0,1) || (a,b) == (0,2) then error "accion invalida"
    else
        if x == a && y == b then 
            if (length caracteres) >= (foldr (+) 0 (listaIntegerToInt (z:zs))) then 
                -- por ej tenemos 3 casillas y [1,2] se debe separar en las 2 casillas,
                if  length (z:zs) == 0 then error "no se puede realizar esta accion"       --si tenemos [1,2,1] se separa en las 3 casillas 
                else    --- lo que hace la linea de abajo es toma los caracteres que se quieren sacar y se los quita a los que estan en la posicion (a,b). Despues busca en la sig posicion en la direccion y le agrega la cadena 
                    if length (z:zs) == 1 then cambiarCasilla (fst (desapilarDeCasilla (caracteres,(x,y)) (fromInteger z)))  (insertarEnCasilleroSigPos juego Arriba (a-1,b) (snd (desapilarDeCasilla (caracteres,(x,y)) (fromInteger z))))
                    else  -- [1,2]
                        if length (z:zs) == 2 then cambiarCasilla (fst (desapilarDeCasilla2 (caracteres,(x,y)) (z:zs)))  (insertarEnCasilleroSigPos (insertarEnCasilleroSigPos juego Arriba (a-1,b) ((snd (desapilarDeCasilla2 (caracteres,(x,y)) (z:zs))) !! 0)) Arriba (a-2,b) ((snd (desapilarDeCasilla2 (caracteres,(x,y)) (z:zs))) !! 1)) 
                        else error "No se puede desapila fuera del tablero"
                
            else error "No hay fichas suficientes para desapilar"
    
        else    (desapilarArriba3x3 juego xs (jugadorAct, (Desapilar (a,b) (z:zs) Arriba)))
      
desapilarAbajo3x3 :: [Casillero] -> [Casillero] -> (TakPlayer, TakAction) -> [Casillero]
desapilarAbajo3x3 _ _ (_ ,(Desapilar _ [] _)) = error "Lista vacia"
desapilarAbajo3x3 [] _ _ = error "Casillero Vacio 1"
desapilarAbajo3x3 _ [] _ = error "Casillero Vacio 2" 
desapilarAbajo3x3 juego ((caracteres,(x,y)):xs) (jugadorAct, (Desapilar (a,b) (z:zs) Abajo)) =
    if (a,b) == (2,0) || (a,b) == (2,1) || (a,b) == (2,2) then error "accion invalida"
    else
        if x == a && y == b then 
            if (length caracteres) > (foldr (+) 0 (listaIntegerToInt (z:zs))) then 
                -- por ej tenemos 3 casillas y [1,2] se debe separar en las 2 casillas,
                    if  length (z:zs) == 0 then error "no se puede realizar esta accion"       --si tenemos [1,2,1] se separa en las 3 casillas 
                    else    --- lo que hace la linea de abajo es toma los caracteres que se quieren sacar y se los quita a los que estan en la posicion (a,b). Despues busca en la sig posicion en la direccion y le agrega la cadena 
                        if length (z:zs) == 1 then cambiarCasilla (fst (desapilarDeCasilla (caracteres,(x,y)) (fromInteger z)))  (insertarEnCasilleroSigPos juego Abajo (a+1,b) (snd (desapilarDeCasilla (caracteres,(x,y)) (fromInteger z))))
                        else  -- [1,2]
                            if length (z:zs) == 2 then cambiarCasilla (fst (desapilarDeCasilla2 (caracteres,(x,y)) (z:zs)))  (insertarEnCasilleroSigPos (insertarEnCasilleroSigPos juego Abajo (a+1,b) ((snd (desapilarDeCasilla2 (caracteres,(x,y)) (z:zs))) !! 0)) Abajo (a+2,b) ((snd (desapilarDeCasilla2 (caracteres,(x,y)) (z:zs))) !! 1)) 
                            else error "No se puede desapila fuera del tablero"
                
            else error "No hay fichas suficientes para desapilar"
        
        else    (desapilarAbajo3x3 juego xs (jugadorAct, (Desapilar (a,b) (z:zs) Abajo)))


desapilarDerecha3x3 :: [Casillero] -> [Casillero] -> (TakPlayer, TakAction) -> [Casillero]
desapilarDerecha3x3 _ _ (_ ,(Desapilar _ [] _)) = error "Lista vacia"
desapilarDerecha3x3 [] _ _ = error "Casillero Vacio 1"
desapilarDerecha3x3 _ [] _ = error "Casillero Vacio 2" 
desapilarDerecha3x3 juego ((caracteres,(x,y)):xs) (jugadorAct, (Desapilar (a,b) (z:zs) Derecha)) =
    if (a,b) == (0,2) || (a,b) == (1,2) || (a,b) == (2,2) then error "accion invalida"
    else
        if x == a && y == b then 
            if (length caracteres) > (foldr (+) 0 (listaIntegerToInt (z:zs))) then 
                -- por ej tenemos 3 casillas y [1,2] se debe separar en las 2 casillas,
                    if  length (z:zs) == 0 then error "no se puede realizar esta accion"       --si tenemos [1,2,1] se separa en las 3 casillas 
                    else    --- lo que hace la linea de abajo es toma los caracteres que se quieren sacar y se los quita a los que estan en la posicion (a,b). Despues busca en la sig posicion en la direccion y le agrega la cadena 
                        if length (z:zs) == 1 then cambiarCasilla (fst (desapilarDeCasilla (caracteres,(x,y)) (fromInteger z)))  (insertarEnCasilleroSigPos juego Derecha (a,b+1) (snd (desapilarDeCasilla (caracteres,(x,y)) (fromInteger z))))
                        else  -- [1,2]
                            if length (z:zs) == 2 then cambiarCasilla (fst (desapilarDeCasilla2 (caracteres,(x,y)) (z:zs)))  (insertarEnCasilleroSigPos (insertarEnCasilleroSigPos juego Derecha (a,b+1) ((snd (desapilarDeCasilla2 (caracteres,(x,y)) (z:zs))) !! 0)) Derecha (a,b+2) ((snd (desapilarDeCasilla2 (caracteres,(x,y)) (z:zs))) !! 1)) 
                            else error "No se puede desapila fuera del tablero"
                
            else error "No hay fichas suficientes para desapilar"
        
        else    (desapilarDerecha3x3 juego xs (jugadorAct, (Desapilar (a,b) (z:zs) Derecha)))


desapilarIzquierda3x3 :: [Casillero] -> [Casillero] -> (TakPlayer, TakAction) -> [Casillero]
desapilarIzquierda3x3 _ _ (_ ,(Desapilar _ [] _)) = error "Lista vacia"
desapilarIzquierda3x3 [] _ _ = error "Casillero Vacio 1"
desapilarIzquierda3x3 _ [] _ = error "Casillero Vacio 2" 
desapilarIzquierda3x3 juego ((caracteres,(x,y)):xs) (jugadorAct, (Desapilar (a,b) (z:zs) Izquierda)) =
    if (a,b) == (0,0) || (a,b) == (1,0) || (a,b) == (2,0) then error "accion invalida"
    else
        if x == a && y == b then 
            if (length caracteres) > (foldr (+) 0 (listaIntegerToInt (z:zs))) then 
                -- por ej tenemos 3 casillas y [1,2] se debe separar en las 2 casillas,
                    if  length (z:zs) == 0 then error "no se puede realizar esta accion"       --si tenemos [1,2,1] se separa en las 3 casillas 
                    else    --- lo que hace la linea de Izquierda es toma los caracteres que se quieren sacar y se los quita a los que estan en la posicion (a,b). Despues busca en la sig posicion en la direccion y le agrega la cadena 
                        if length (z:zs) == 1 then cambiarCasilla (fst (desapilarDeCasilla (caracteres,(x,y)) (fromInteger z)))  (insertarEnCasilleroSigPos juego Izquierda (a,b-1) (snd (desapilarDeCasilla (caracteres,(x,y)) (fromInteger z))))
                        else  -- [1,2]
                            if length (z:zs) == 2 then cambiarCasilla (fst (desapilarDeCasilla2 (caracteres,(x,y)) (z:zs)))  (insertarEnCasilleroSigPos (insertarEnCasilleroSigPos juego Izquierda (a,b-1) ((snd (desapilarDeCasilla2 (caracteres,(x,y)) (z:zs))) !! 0)) Izquierda (a,b-2) ((snd (desapilarDeCasilla2 (caracteres,(x,y)) (z:zs))) !! 1)) 
                            else error "No se puede desapila fuera del tablero"
                
            else error "No hay fichas suficientes para desapilar"
        
        else    (desapilarIzquierda3x3 juego xs (jugadorAct, (Desapilar (a,b) (z:zs) Izquierda)))


desapilarArriba4x4 :: [Casillero] -> [Casillero] -> (TakPlayer, TakAction) -> [Casillero]
desapilarArriba4x4 _ _ (_ ,(Desapilar _ [] _)) = error "Lista vacia"
desapilarArriba4x4 [] _ _ = error "Casillero Vacio 1"
desapilarArriba4x4 _ [] _ = error "Casillero Vacio 2" 
desapilarArriba4x4 juego ((caracteres,(x,y)):xs) (jugadorAct, (Desapilar (a,b) (z:zs) Arriba)) =
    if (a,b) == (0,0) || (a,b) == (0,1) || (a,b) == (0,2) || (a,b) == (0,3) then error "accion invalida"
    else
        if x == a && y == b then 
            if (length caracteres) >= (foldr (+) 0 (listaIntegerToInt (z:zs))) then 
                -- por ej tenemos 3 casillas y [1,2] se debe separar en las 2 casillas,
                if  length (z:zs) == 0 then error "no se puede realizar esta accion"       --si tenemos [1,2,1] se separa en las 3 casillas 
                else    --- lo que hace la linea de abajo es toma los caracteres que se quieren sacar y se los quita a los que estan en la posicion (a,b). Despues busca en la sig posicion en la direccion y le agrega la cadena 
                    if length (z:zs) == 1 then cambiarCasilla (fst (desapilarDeCasilla (caracteres,(x,y)) (fromInteger z)))  (insertarEnCasilleroSigPos juego Arriba (a-1,b) (snd (desapilarDeCasilla (caracteres,(x,y)) (fromInteger z))))
                    else  -- [1,2]
                        if length (z:zs) == 2 then cambiarCasilla (fst (desapilarDeCasilla2 (caracteres,(x,y)) (z:zs)))  (insertarEnCasilleroSigPos (insertarEnCasilleroSigPos juego Arriba (a-1,b) ((snd (desapilarDeCasilla2 (caracteres,(x,y)) (z:zs))) !! 0)) Arriba (a-2,b) ((snd (desapilarDeCasilla2 (caracteres,(x,y)) (z:zs))) !! 1)) 
                        else 
                            if length (z:zs) == 3 then cambiarCasilla (fst (desapilarDeCasilla2 (caracteres,(x,y)) (z:zs)))  (insertarEnCasilleroSigPos (insertarEnCasilleroSigPos (insertarEnCasilleroSigPos juego Arriba (a-1,b) ((snd (desapilarDeCasilla2 (caracteres,(x,y)) (z:zs))) !! 0)) Arriba (a-2,b) ((snd (desapilarDeCasilla2 (caracteres,(x,y)) (z:zs))) !! 1)) Arriba (a-3,b) ((snd (desapilarDeCasilla2 (caracteres,(x,y)) (z:zs))) !! 2)) 
                            else error "No se puede desapila fuera del tablero"
            else error "No hay fichas suficientes para desapilar"
    
        else    (desapilarArriba4x4 juego xs (jugadorAct, (Desapilar (a,b) (z:zs) Arriba)))


desapilarAbajo4x4 :: [Casillero] -> [Casillero] -> (TakPlayer, TakAction) -> [Casillero]
desapilarAbajo4x4 _ _ (_ ,(Desapilar _ [] _)) = error "Lista vacia"
desapilarAbajo4x4 [] _ _ = error "Casillero Vacio 1"
desapilarAbajo4x4 _ [] _ = error "Casillero Vacio 2" 
desapilarAbajo4x4 juego ((caracteres,(x,y)):xs) (jugadorAct, (Desapilar (a,b) (z:zs) Abajo)) =
    if (a,b) == (3,0) || (a,b) == (3,1) || (a,b) == (3,2) || (a,b) == (3,3) then error "accion invalida"
    else
        if x == a && y == b then 
            if (length caracteres) > (foldr (+) 0 (listaIntegerToInt (z:zs))) then 
                -- por ej tenemos 3 casillas y [1,2] se debe separar en las 2 casillas,
                    if  length (z:zs) == 0 then error "no se puede realizar esta accion"       --si tenemos [1,2,1] se separa en las 3 casillas 
                    else    --- lo que hace la linea de abajo es toma los caracteres que se quieren sacar y se los quita a los que estan en la posicion (a,b). Despues busca en la sig posicion en la direccion y le agrega la cadena 
                        if length (z:zs) == 1 then cambiarCasilla (fst (desapilarDeCasilla (caracteres,(x,y)) (fromInteger z)))  (insertarEnCasilleroSigPos juego Abajo (a+1,b) (snd (desapilarDeCasilla (caracteres,(x,y)) (fromInteger z))))
                        else  -- [1,2]
                            if length (z:zs) == 3 then cambiarCasilla (fst (desapilarDeCasilla2 (caracteres,(x,y)) (z:zs)))  (insertarEnCasilleroSigPos (insertarEnCasilleroSigPos (insertarEnCasilleroSigPos juego Abajo (a+1,b) ((snd (desapilarDeCasilla2 (caracteres,(x,y)) (z:zs))) !! 0)) Abajo (a+2,b) ((snd (desapilarDeCasilla2 (caracteres,(x,y)) (z:zs))) !! 1)) Abajo (a+3,b) ((snd (desapilarDeCasilla2 (caracteres,(x,y)) (z:zs))) !! 2)) 
                            else error "No se puede desapila fuera del tablero"
                
            else error "No hay fichas suficientes para desapilar"
        
        else    (desapilarAbajo4x4 juego xs (jugadorAct, (Desapilar (a,b) (z:zs) Abajo)))

desapilarIzquierda4x4 :: [Casillero] -> [Casillero] -> (TakPlayer, TakAction) -> [Casillero]
desapilarIzquierda4x4 _ _ (_ ,(Desapilar _ [] _)) = error "Lista vacia"
desapilarIzquierda4x4 [] _ _ = error "Casillero Vacio 1"
desapilarIzquierda4x4 _ [] _ = error "Casillero Vacio 2" 
desapilarIzquierda4x4 juego ((caracteres,(x,y)):xs) (jugadorAct, (Desapilar (a,b) (z:zs) Izquierda)) =
    if (a,b) == (0,0) || (a,b) == (1,0) || (a,b) == (2,0) || (a,b) == (3,0) then error "accion invalida"
    else
        if x == a && y == b then 
            if (length caracteres) >= (foldr (+) 0 (listaIntegerToInt (z:zs))) then 
                -- por ej tenemos 3 casillas y [1,2] se debe separar en las 2 casillas,
                if  length (z:zs) == 0 then error "no se puede realizar esta accion"       --si tenemos [1,2,1] se separa en las 3 casillas 
                else    --- lo que hace la linea de abajo es toma los caracteres que se quieren sacar y se los quita a los que estan en la posicion (a,b). Despues busca en la sig posicion en la direccion y le agrega la cadena 
                    if length (z:zs) == 1 then cambiarCasilla (fst (desapilarDeCasilla (caracteres,(x,y)) (fromInteger z)))  (insertarEnCasilleroSigPos juego Izquierda (a,b-1) (snd (desapilarDeCasilla (caracteres,(x,y)) (fromInteger z))))
                    else  -- [1,2]
                        if length (z:zs) == 2 then cambiarCasilla (fst (desapilarDeCasilla2 (caracteres,(x,y)) (z:zs)))  (insertarEnCasilleroSigPos (insertarEnCasilleroSigPos juego Izquierda (a,b-1) ((snd (desapilarDeCasilla2 (caracteres,(x,y)) (z:zs))) !! 0)) Izquierda (a,b-2) ((snd (desapilarDeCasilla2 (caracteres,(x,y)) (z:zs))) !! 1)) 
                        else 
                            if length (z:zs) == 3 then cambiarCasilla (fst (desapilarDeCasilla2 (caracteres,(x,y)) (z:zs)))  (insertarEnCasilleroSigPos (insertarEnCasilleroSigPos (insertarEnCasilleroSigPos juego Izquierda (a,b-1) ((snd (desapilarDeCasilla2 (caracteres,(x,y)) (z:zs))) !! 0)) Izquierda (a,b-2) ((snd (desapilarDeCasilla2 (caracteres,(x,y)) (z:zs))) !! 1)) Izquierda (a,b-3) ((snd (desapilarDeCasilla2 (caracteres,(x,y)) (z:zs))) !! 2)) 
                            else error "No se puede desapila fuera del tablero"
            else error "No hay fichas suficientes para desapilar"
    
        else    (desapilarIzquierda4x4 juego xs (jugadorAct, (Desapilar (a,b) (z:zs) Izquierda)))


desapilarDerecha4x4 :: [Casillero] -> [Casillero] -> (TakPlayer, TakAction) -> [Casillero]
desapilarDerecha4x4 _ _ (_ ,(Desapilar _ [] _)) = error "Lista vacia"
desapilarDerecha4x4 [] _ _ = error "Casillero Vacio 1"
desapilarDerecha4x4 _ [] _ = error "Casillero Vacio 2" 
desapilarDerecha4x4 juego ((caracteres,(x,y)):xs) (jugadorAct, (Desapilar (a,b) (z:zs) Derecha)) =
    if (a,b) == (0,3) || (a,b) == (1,3) || (a,b) == (2,3) || (a,b) == (3,3) then error "accion invalida"
    else
        if x == a && y == b then 
            if (length caracteres) >= (foldr (+) 0 (listaIntegerToInt (z:zs))) then 
                -- por ej tenemos 3 casillas y [1,2] se debe separar en las 2 casillas,
                if  length (z:zs) == 0 then error "no se puede realizar esta accion"       --si tenemos [1,2,1] se separa en las 3 casillas 
                else    --- lo que hace la linea de abajo es toma los caracteres que se quieren sacar y se los quita a los que estan en la posicion (a,b). Despues busca en la sig posicion en la direccion y le agrega la cadena 
                    if length (z:zs) == 1 then cambiarCasilla (fst (desapilarDeCasilla (caracteres,(x,y)) (fromInteger z)))  (insertarEnCasilleroSigPos juego Derecha (a,b+1) (snd (desapilarDeCasilla (caracteres,(x,y)) (fromInteger z))))
                    else  -- [1,2]
                        if length (z:zs) == 2 then cambiarCasilla (fst (desapilarDeCasilla2 (caracteres,(x,y)) (z:zs)))  (insertarEnCasilleroSigPos (insertarEnCasilleroSigPos juego Derecha (a,b+1) ((snd (desapilarDeCasilla2 (caracteres,(x,y)) (z:zs))) !! 0)) Derecha (a,b+2) ((snd (desapilarDeCasilla2 (caracteres,(x,y)) (z:zs))) !! 1)) 
                        else 
                            if length (z:zs) == 3 then cambiarCasilla (fst (desapilarDeCasilla2 (caracteres,(x,y)) (z:zs)))  (insertarEnCasilleroSigPos (insertarEnCasilleroSigPos (insertarEnCasilleroSigPos juego Derecha (a,b+1) ((snd (desapilarDeCasilla2 (caracteres,(x,y)) (z:zs))) !! 0)) Derecha (a,b-2) ((snd (desapilarDeCasilla2 (caracteres,(x,y)) (z:zs))) !! 1)) Derecha (a,b+3) ((snd (desapilarDeCasilla2 (caracteres,(x,y)) (z:zs))) !! 2)) 
                            else error "No se puede desapila fuera del tablero"
            else error "No hay fichas suficientes para desapilar"
    
        else    (desapilarDerecha4x4 juego xs (jugadorAct, (Desapilar (a,b) (z:zs) Derecha)))

        

--- crear Camino 3X3
crearCaminoArriba3x3 :: Direccion -> [Casillero]  -> (Integer,Integer) -> [Casillero]
crearCaminoArriba3x3 _ [] _ = []
crearCaminoArriba3x3 direccion (x:xs) (a,b)
    | ((a,b) == (0,0) || (a,b) == (0,1) || (a,b) == (0,2) || (elem 'X' (contenidoCasillero2 x)) || (elem 'O' (contenidoCasillero2 x))) && direccion == Arriba = [(obtenerCasilla (x:xs) (a,b))]
    | otherwise = (obtenerCasilla (x:xs) (a,b)) : (esPosible (crearCaminoArriba3x3 direccion (xs) (a-1,b)) (obtenerCasilla (crearCaminoArriba3x3 direccion (xs) (a-1,b)) (a-1,b))) ++ [casillaSiguienteDireccion direccion (x:xs) (a-1,b)]



crearCaminoAbajo3x3 :: Direccion -> [Casillero]  -> (Integer,Integer) -> [Casillero]
crearCaminoAbajo3x3 _ [] _ = []
crearCaminoAbajo3x3 direccion (x:xs) (a,b)
    | ((a,b) == (2,0) || (a,b) == (2,1) || (a,b) == (2,2) || (elem 'X' (contenidoCasillero2 x)) || (elem 'O' (contenidoCasillero2 x))) && direccion == Abajo = [(obtenerCasilla (x:xs) (a,b))]
    | otherwise = (obtenerCasilla (x:xs) (a,b)) : (esPosible (crearCaminoAbajo3x3 direccion (xs) (a+1,b)) (obtenerCasilla (crearCaminoAbajo3x3 direccion (xs) (a+1,b)) (a+1,b))) ++ [casillaSiguienteDireccion direccion (x:xs) (a+1,b)]

crearCaminoDerecha3x3 :: Direccion -> [Casillero]  -> (Integer,Integer) -> [Casillero]
crearCaminoDerecha3x3 _ [] _ = []
crearCaminoDerecha3x3 direccion (x:xs) (a,b)
    | ((a,b) == (0,2) || (a,b) == (1,2) || (a,b) == (2,2) || (elem 'X' (contenidoCasillero2 x)) || (elem 'O' (contenidoCasillero2 x))) && direccion == Derecha = [(obtenerCasilla (x:xs) (a,b)) ] 
    | otherwise = (obtenerCasilla (x:xs) (a,b)) : (esPosible (crearCaminoDerecha3x3 direccion (xs) (a,b+1)) (obtenerCasilla (crearCaminoDerecha3x3 direccion (xs) (a,b+1)) (a,b+1))) ++ [casillaSiguienteDireccion direccion (x:xs) (a,b+1)]


crearCaminoIzquierda3x3 :: Direccion -> [Casillero]  -> (Integer,Integer) -> [Casillero]
crearCaminoIzquierda3x3 _ [] _ = []
crearCaminoIzquierda3x3 direccion (x:xs) (a,b)
    | ((a,b) == (0,0) || (a,b) == (1,0) || (a,b) == (2,0) || (elem 'X' (contenidoCasillero2 x)) || (elem 'O' (contenidoCasillero2 x))) && direccion == Izquierda = [(obtenerCasilla (x:xs) (a,b))] 
    | otherwise = (obtenerCasilla (x:xs) (a,b)) : (esPosible (crearCaminoIzquierda3x3 direccion (xs) (a,b-1)) (obtenerCasilla (crearCaminoIzquierda3x3 direccion (xs) (a,b-1)) (a,b-1))) ++ [casillaSiguienteDireccion direccion (x:xs) (a,b-1)]

crearCamino3X3 :: Direccion -> [Casillero] -> (Integer,Integer) -> [Casillero]
crearCamino3X3 _ [] _ = []
crearCamino3X3 direccion (x:xs) (a,b)
    | direccion == Arriba = let casillero = (quitarDuplicados (crearCaminoArriba3x3 direccion (x:xs) (a,b))) in (quitarError casillero (10,10) 10)
    | direccion == Abajo = let casillero = (quitarDuplicados (crearCaminoAbajo3x3  direccion (x:xs) (a,b))) in (quitarError casillero (10,10) 10)
    | direccion == Derecha = let casillero = (quitarDuplicados (crearCaminoDerecha3x3  direccion (x:xs) (a,b))) in (quitarError casillero (10,10) 10)
    | direccion == Izquierda = let casillero = (quitarDuplicados (crearCaminoIzquierda3x3  direccion (x:xs) (a,b))) in (quitarError casillero (10,10) 10)
    | otherwise = []

--- crear Camino 4x4  
crearCaminoArriba4x4 :: Direccion -> [Casillero]  -> (Integer,Integer) -> [Casillero]
crearCaminoArriba4x4 _ [] _ = []
crearCaminoArriba4x4 direccion (x:xs) (a,b)
    | ((a,b) == (0,0) || (a,b) == (0,1) || (a,b) == (0,2) || (a,b) == (0,3) || (elem 'X' (contenidoCasillero2 x)) || (elem 'O' (contenidoCasillero2 x))) && direccion == Arriba = [(obtenerCasilla (x:xs) (a,b))]
    | otherwise = (obtenerCasilla (x:xs) (a,b)) : (esPosible (crearCaminoArriba4x4 direccion (xs) (a-1,b)) (obtenerCasilla (crearCaminoArriba4x4 direccion (xs) (a-1,b)) (a-1,b))) ++ [casillaSiguienteDireccion direccion (x:xs) (a-1,b)]

crearCaminoAbajo4x4 :: Direccion -> [Casillero]  -> (Integer,Integer) -> [Casillero]
crearCaminoAbajo4x4 _ [] _ = []
crearCaminoAbajo4x4 direccion (x:xs) (a,b)
    | ((a,b) == (3,0) || (a,b) == (3,1) || (a,b) == (3,2) || (a,b) == (3,3) || (elem 'X' (contenidoCasillero2 x)) || (elem 'O' (contenidoCasillero2 x)))  && direccion == Abajo = [(obtenerCasilla (x:xs) (a,b))]
    | otherwise = (obtenerCasilla (x:xs) (a,b)) : (esPosible (crearCaminoAbajo4x4 direccion (xs) (a+1,b)) (obtenerCasilla (crearCaminoAbajo4x4 direccion (xs) (a+1,b)) (a+1,b))) ++ [casillaSiguienteDireccion direccion (x:xs) (a+1,b)]

crearCaminoDerecha4x4 :: Direccion -> [Casillero]  -> (Integer,Integer) -> [Casillero]
crearCaminoDerecha4x4 _ [] _ = []
crearCaminoDerecha4x4 direccion (x:xs) (a,b)
    | ((a,b) == (0,3) || (a,b) == (1,3) || (a,b) == (2,3) || (a,b) == (3,3) || (elem 'X' (contenidoCasillero2 x)) || (elem 'O' (contenidoCasillero2 x))) && direccion == Derecha = [(obtenerCasilla (x:xs) (a,b)) ] 
    | otherwise = (obtenerCasilla (x:xs) (a,b)) : (esPosible (crearCaminoDerecha4x4 direccion (xs) (a,b+1)) (obtenerCasilla (crearCaminoDerecha4x4 direccion (xs) (a,b+1)) (a,b+1))) ++ [casillaSiguienteDireccion direccion (x:xs) (a,b+1)]

crearCaminoIzquierda4x4 :: Direccion -> [Casillero]  -> (Integer,Integer) -> [Casillero]
crearCaminoIzquierda4x4 _ [] _ = []
crearCaminoIzquierda4x4 direccion (x:xs) (a,b)
    | ((a,b) == (0,0) || (a,b) == (1,0) || (a,b) == (2,0) || (a,b) == (3,0) || (elem 'X' (contenidoCasillero2 x)) || (elem 'O' (contenidoCasillero2 x)))  && direccion == Izquierda = [(obtenerCasilla (x:xs) (a,b))] 
    | otherwise = (obtenerCasilla (x:xs) (a,b)) : (esPosible (crearCaminoIzquierda4x4 direccion (xs) (a,b-1)) (obtenerCasilla (crearCaminoIzquierda4x4 direccion (xs) (a,b-1)) (a,b-1))) ++ [casillaSiguienteDireccion direccion (x:xs) (a,b-1)]

crearCamino4X4 :: Direccion -> [Casillero] -> (Integer,Integer) -> [Casillero]
crearCamino4X4 _ [] _ = []
crearCamino4X4 direccion (x:xs) (a,b)
    | direccion == Arriba = let casillero = (quitarDuplicados (crearCaminoArriba4x4 direccion (x:xs) (a,b))) in (quitarError casillero (10,10) 10)
    | direccion == Abajo = let casillero = (quitarDuplicados (crearCaminoAbajo4x4  direccion (x:xs) (a,b))) in (quitarError casillero (10,10) 10)
    | direccion == Derecha = let casillero = (quitarDuplicados (crearCaminoDerecha4x4  direccion (x:xs) (a,b))) in (quitarError casillero (10,10) 10)
    | direccion == Izquierda = let casillero = (quitarDuplicados (crearCaminoIzquierda4x4  direccion (x:xs) (a,b))) in (quitarError casillero (10,10) 10)
    | otherwise = []

seMueveDerecha :: Casillero -> Casillero -> Bool
seMueveDerecha (caracteres,(x,y)) (caracteres2,(a,b)) = (x == a && y+1 == b)

seMueveIzquierda :: Casillero -> Casillero -> Bool
seMueveIzquierda (caracteres,(x,y)) (caracteres2,(a,b)) = (x == a && y-1 == b)

seMueveArriba :: Casillero -> Casillero -> Bool
seMueveArriba (caracteres,(x,y)) (caracteres2,(a,b)) = (x-1 == a && y == b)

seMueveAbajo :: Casillero -> Casillero -> Bool
seMueveAbajo (caracteres,(x,y)) (caracteres2,(a,b)) = (x+1 == a && y == b)

casillaSiguienteDireccion :: Direccion -> [Casillero] -> (Integer,Integer) -> Casillero
casillaSiguienteDireccion dir (x:xs) (a,b) 
    | dir == Arriba = (devolverCamino [((seMueveArriba (obtenerCasilla (x:xs) (a,b)) x),x) | x <- ((intersectCercaYPosible (obtenerCasilla (x:xs) (a,b))  (x:xs)))])
    | dir == Abajo = (devolverCamino [((seMueveAbajo (obtenerCasilla (x:xs) (a,b)) x),x) | x <- ((intersectCercaYPosible (obtenerCasilla (x:xs) (a,b))  (x:xs)))])
    | dir == Derecha = (devolverCamino [((seMueveDerecha (obtenerCasilla (x:xs) (a,b)) x),x) | x <- ((intersectCercaYPosible (obtenerCasilla (x:xs) (a,b))  (x:xs)))])
    | dir == Izquierda = (devolverCamino [((seMueveIzquierda (obtenerCasilla (x:xs) (a,b)) x),x) | x <- (intersectCercaYPosible (obtenerCasilla (x:xs) (a,b))  (x:xs))])


------------------------------------------Imprimir --------------------------

impresionJuego3x3 :: TakGame -> String
impresionJuego3x3 juego = unlines $ [(caracterPosicion juego 0) ++ "      " ++  (caracterPosicion juego 1) ++ "      " ++ (caracterPosicion juego 2) ++'\n': 
    (caracterPosicion juego 3) ++ "      " ++ (caracterPosicion juego 4) ++ "      " ++ (caracterPosicion juego 5) ++ '\n':
    (caracterPosicion juego 6) ++ "      " ++ (caracterPosicion juego 7) ++ "      " ++ (caracterPosicion juego 8)]

impresionJuego4x4 :: TakGame -> String
impresionJuego4x4 juego = unlines $ [(caracterPosicion juego 0) ++ "      " ++  (caracterPosicion juego 1) ++ "      " ++ (caracterPosicion juego 2) ++ "      " ++ (caracterPosicion juego 3) ++'\n': 
     (caracterPosicion juego 4) ++ "      " ++ (caracterPosicion juego 5) ++ "      " ++ (caracterPosicion juego 6) ++ "      " ++ (caracterPosicion juego 7) ++'\n':
     (caracterPosicion juego 8) ++ "      " ++ (caracterPosicion juego 9) ++ "      " ++ (caracterPosicion juego 10) ++ "      " ++ (caracterPosicion juego 11) ++ '\n':
     (caracterPosicion juego 12) ++ "      " ++ (caracterPosicion juego 13) ++ "      " ++ (caracterPosicion juego 14) ++ "      " ++ (caracterPosicion juego 15)]


----------------- Puntaje y Resultado -----------------------------------
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
   case (activePlayer g) of
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