import Data.Maybe (fromJust, listToMaybe)
import Data.List (elemIndex)
import Data.List
import System.Random (randomRIO)
import Data.Char





type TakGame = ([Casillero], TakPlayer)
type Casillero = ([Char], (Integer, Integer))

data Direccion = Arriba | Abajo | Izquierda | Derecha

data TakAction = Insertar (Integer, Integer) Bool | Mover (Integer, Integer) (Integer, Integer) | Desapilar (Integer, Integer) [Integer] Direccion

data TakPlayer = WhitePlayer | BlackPlayer deriving (Eq, Show, Enum)

coordenadasCasillero3x3 = map (\n -> ("",divMod n 3)) [0..8]
coordenadasCasillero4x4 = map (\n -> ("",(divMod n 4)))  [0..15]

-- ejemplo de takGame
juego3x3 = ([("o",(0,0)) ,("o",(0,1)),("o",(0,2)),("o",(1,0)),("o",(1,1)),("o",(1,2)),("o",(2,0)),("o",(2,1)),("o",(2,2))],WhitePlayer)

juego4x4 = (map (\n -> ("x",(divMod n 4)))  [0..15], BlackPlayer)

juegoVacio = ([],BlackPlayer)


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


impresionJuego3x3 :: TakGame -> String
impresionJuego3x3 juego = unlines $ [(caracterPosicion juego 0) ++  (caracterPosicion juego 1) ++ (caracterPosicion juego 2) ++'\n': 
    (caracterPosicion juego 3) ++ (caracterPosicion juego 4) ++ (caracterPosicion juego 5) ++ '\n':
    (caracterPosicion juego 6) ++ (caracterPosicion juego 7) ++ (caracterPosicion juego 8)]

impresionJuego4x4 :: TakGame -> String
impresionJuego4x4 juego = unlines $ [(caracterPosicion juego 0) ++  (caracterPosicion juego 1) ++ (caracterPosicion juego 2) ++ (caracterPosicion juego 3) ++'\n': 
     (caracterPosicion juego 4) ++ (caracterPosicion juego 5) ++ (caracterPosicion juego 6) ++ (caracterPosicion juego 7) ++'\n':
     (caracterPosicion juego 8) ++ (caracterPosicion juego 9) ++ (caracterPosicion juego 10) ++ (caracterPosicion juego 11) ++ '\n':
     (caracterPosicion juego 12) ++ (caracterPosicion juego 13) ++ (caracterPosicion juego 14) ++ (caracterPosicion juego 15)]

printStrings :: [[String]] -> IO ()
printStrings = mapM_ (mapM_ putStrLn)

showGame :: TakGame -> IO()
showGame juego 
    | fst juego == [] = error "juego vacio"
    --juego 3x3
    | length (fst juego) == 9 =  printStrings [[impresionJuego3x3 juego]]
    --juego 4x4
    | length (fst juego) == 16 = printStrings [[impresionJuego4x4 juego]]
    
    | otherwise = error "juego no valido para mostrar"
    
    


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