# Proyecto final de programación declarativa.

El comesolo consiste en comer bolas en un tablero, parecido al juego de damas pero con 1 solo jugador.
Las reglas son las siguientes:
- La bola a mover debe pasar por encima de otra(solo una) bola.
- La bola que se saltó debe ser removida del tablero.
- Una bola solo se puede desplazar hacia un espacio vacío y este debe estar a 2 posiciones de la bola.

El objetivo del juego es dejar el tablero con solo una bola.

---------------------------------------------------------------------------------

Para ejecutar el programa se ingresa a la carpeta ProyectoFinal y se hace lo siguiente:
- Se compila el programa desde la terminal con el comando "ghc ProyectoFinal.hs" (sin comillas).
- Se ejecuta el archivo con el comando "./ProyectoFinal.hs"
- Posteriormente el programa irá dando instrucciones sobre cómo poner un ejemplar.
- Cuando termine la ejecución se verá en consola una solución en forma de pares y además se mostrará la evolución del tablero.
- También se crea un archivo de texto que se guarda en la carpeta Comesolo y Comesolo/dist.
- Opcionalmente se puede visualizar una animación sobre el juego en el proyecto de Java llamado Comesolo (solo después de haber ejecutado ProyectoFinal.hs).

-------------------------------------------------------------------------------

Para ejecutar la animación (el proyecto de java) debes entrar a la carpeta Comesolo/dist y ejecutar el jar con el comando:
"java -jar Comesolo.jar"
O bien abrir el proyecto desde NetBeans y ejecutarlo ahí.

El botón "reiniciar" vuelve a ejecutar la animación, y si se modificó el archivo solucion.sol se modifica la animación.
