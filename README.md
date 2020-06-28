# TakGame
TakGame obligatorio Programación Funcional


• activePlayer :: TakGame -> TakPlayer: Esta función determina a cuál jugador le toca mover, dado
un estado de juego.

• actions :: TakGame -> [(TakPlayer, [TakAction])]: La lista debe incluir una y solo una tupla para
cada jugador. Si el jugador está activo, la lista asociada debe incluir todos sus posibles movimientos para
el estado de juego dado. Sino la lista debe estar vacía.

• next :: TakGame -> (TakPlayer, TakAction) -> TakGame: Esta función aplica una acción sobre un
estado de juego dado, y retorna el estado resultante. Se debe levantar un error si el jugador dado no es el
jugador activo, si el juego está terminado, o si la acción no es realizable.

• result :: TakGame -> [(TakPlayer, Int)]: Si el juego está terminado retorna el resultado de juego
para cada jugador. Este valor es 1 si el jugador ganó, -1 si perdió y 0 si se empató. Si el juego no está
terminado, se debe retornar una lista vacía.

• score :: TakGame -> [(TakPlayer, Int)]: Retorna el puntaje para todos los jugadores en el estado
de juego dado. Esto es independiente de si el juego está terminado o no.

• showGame :: TakGame -> String: Convierte el estado de juego a un texto que puede ser impreso en la
consola para mostrar el tablero y demás información de la partida. (Terminado)

• showAction :: TakAction -> String: Convierte una acción a un texto que puede ser impreso en la
consola para mostrarla.

• readAction :: String -> TakAction: Obtiene una acción a partir de un texto que puede haber sido
introducido por el usuario en la consola.














