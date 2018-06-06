import System.Exit
import System.Random
import Control.Monad(when)
import Data.List
import Data.Char


type Pos_ini = Int
type Pos_fin = Int
type Pos_mid = Int
type Tablero = [Int] -- la lista representa las posiciones donde hay una pelota en el come solo trianuglar.
type Jugada = (Pos_ini , Pos_mid , Pos_fin , Tablero) -- En la jugada (a,b,c,T) a es de donde proviene la bola y c el destino de esta. b es la bola que se va a comer.
type Solucion = [(Pos_ini , Pos_mid , Pos_fin)] -- la solucion es una lista de jugadas que haga que en el tablero quede solo una bola.
type NodoSolucion = (Solucion , [Jugada])

-- Toma un tablero y devuelve la solución de este siempre que exista.
solucion :: Tablero -> Solucion
solucion tablero =  solucion_aux [([],(jugadas tablero))]

-- Recibe una lista de jugadas, y una solución parcial. Si en la lista hay un nodo final lo agrega al conjunto solución.
verifica_jugadas :: [Jugada] -> Solucion -> Solucion
verifica_jugadas [] _ = []
verifica_jugadas (actual@(pi,pm,pf,_):xs) sol = if esFinal actual then sol++[(pi,pm,pf)] else verifica_jugadas xs sol


solucion_aux :: [NodoSolucion] -> Solucion
solucion_aux [] = []
solucion_aux ((_,[]):xs) = solucion_aux xs
solucion_aux ((sol,jugadas):xs) = if (null solucion) then  solucion_aux ((sucesor_jugadas jugadas sol)++xs) else solucion
                                  where solucion = (verifica_jugadas jugadas sol)


-- Funcion que nos regresa el centro
centro :: String -> Int -> String
centro s n = espacios ++ s ++ espacios
             where espacios = replicate ((n - length s) `div` 2) ' '


tablero = [[1] , [2,3] , [4,5,6] , [7,8,9,10] , [11,12,13,14,15]]



imprime_tablero x = mapM_ putStrLn $ ((flip centro (((15*70)`div`10)) . unwords) . map show) <$> x

imprime_solucion ::  Solucion -> String
imprime_solucion [] = ""
imprime_solucion ((x,_,y):xs) = (show x) ++ " -> " ++ (show y) ++  " , " ++ (imprime_solucion xs)

--Función que simula el juego de come solo. Se llama en la función principal.
comeSolo = do
              putStrLn "Ingresa una de las siguinetes opciones :";
              putStrLn "1 .- Si quieres generar un tablero aleatorio";
              putStrLn "2 .- Si quieres dar tu el tablero";
              putStrLn "3 .- Salir"
              opcion1 <- getLine;
              let opcion = (trim [opcion1])!!0
              if (opcion == "1")
              then do
                    putStrLn "Ingresa la cantidad de bolas que va a tener el tablero";
                    num <- getLine 
                    when ((read num :: Int) > 15 ) 
                         (do putStrLn "Error: La cantidad de bolas no debe de exceder el numero de casillas del tablero (15)."; comeSolo; exitSuccess);
                    lis <- (randomlist (read num :: Int))
                    putStrLn "Este es el tablero generado"
                    imprime_tablero (crea_tablero lis)
                    let sol = (solucion lis)
                    if (null sol)
                        then do putStrLn "Ese tablero no tiene solución"
				escribe_archivo lis sol
                                continuar;

                        else do despliega_solucion lis sol

              else do
                if (opcion == "2")
                then do
                    putStrLn "Ingresa una configuracion de tablero para el tablero ";
                    putStrLn "EL tablero es tiene la siguiente forma";
                    imprime_tablero tablero
                    putStrLn "Indique las posiciones ocupadas por las bolas segun el esquema anterior con los numeros separados por comas";
                    putStrLn "por ejemplo: 1,2,3,4,5,6"
                    tablero <- getLine
                    let tablero_c = construye_tablero  $ trim $ split_c tablero ','
                    when (elem (-1) tablero_c)
                         (do putStrLn "Error: caracter no encontrado"; comeSolo; exitSuccess);
                    putStrLn "El tablero elegido es el siguienete:"
                    imprime_tablero (crea_tablero tablero_c)
                    let sol = (solucion tablero_c)
                    if (null sol)
                        then do putStrLn "Ese tablero no tiene solución"
				escribe_archivo tablero_c sol
                                continuar;

                        else do despliega_solucion tablero_c sol
                               
                else do
                    if (opcion == "3")
                        then do 
                            exitSuccess;
                        else do 
                            putStrLn "Error: opcion invalida";
                            comeSolo;
                            exitSuccess;
                            

escribe_archivo :: Tablero -> Solucion -> IO()
escribe_archivo tablero sol = do 
                                writeFile "Comesolo/soluciones.sol" ((show sol) ++ "\n" ++ (soluciona_tablero tablero sol))
                                writeFile "Comesolo/dist/soluciones.sol" ((show sol) ++ "\n" ++ (soluciona_tablero tablero sol))



soluciona_tablero :: Tablero -> Solucion -> String
soluciona_tablero  tablero [] = show tablero ++ "\n"
soluciona_tablero  tablero ((ini, mid, fin):xs) = (show tablero) ++ "\n" ++ (soluciona_tablero (delete ini (delete mid (fin:tablero))) xs)

imprime_solucion2 :: Tablero -> Solucion -> IO()
imprime_solucion2 tablero [] = do
                               imprime_tablero $ crea_tablero tablero 
                               continuar
imprime_solucion2 tablero ((ini , mid , fin):ys) = do
                                                    imprime_tablero (crea_tablero tablero);
                                                    let tab_actual = delete ini (delete mid (fin:tablero))
                                                    putStrLn $ centro (take 75  (repeat '-')) ((15*70)`div`10);
                                                    imprime_solucion2 tab_actual ys


despliega_solucion tablero sol  = do
                                 putStrLn "Los 0 indican en donde no hay bolas"
                                 putStrLn "La solucion es la siguiente:"
                                 putStrLn (imprime_solucion sol)
                                 putStrLn "la pareja x->y indica que la bola en el lugar x se mueve a la posicion y"
                                 escribe_archivo tablero sol
                                 imprime_solucion2 tablero sol
                                 

construye_tablero :: [String] -> [Int]
construye_tablero [] = []
construye_tablero (x:xs) =  if (elem x lista_cadenas) then (read x :: Int):construye_tablero xs else [-1]

continuar = do 
             putStrLn "Desea probar con otro tablero : \n1.- Si \n2 .- No"
             opcion1 <- getLine
             let opcion = (trim [opcion1])!!0
             if (opcion == "1")
             then do comeSolo;
             else do if (opcion == "2") then do exitSuccess else do putStrLn "Error: opcion invalida"; continuar; exitSuccess

trim :: [String] -> [String] 
trim [] = []
trim (x:xs) = (dropWhile isSpace (dropWhileEnd isSpace x)):(trim xs)

split_c :: String -> Char -> [String]
split_c [] delim = [""]
split_c (c:cs) delim
   | c == delim = "" : rest
   | otherwise = (c : head rest) : tail rest
   where
       rest = split_c cs delim

lista = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15]
lista_cadenas = ["1" , "2" , "3" , "4" , "5" , "6" , "7" , "8" , "9" , "10" , "11" , "12", "13", "14" , "15"]

swapElts i j ls = [get k x | (k, x) <- zip [0..length ls - 1] ls]
    where get k x | k == i = ls !! j
                  | k == j = ls !! i
                  | otherwise = x
randomlist x =  do 
                lis <- (randomlist1 x lista)
                return (take x lis)
               
randomlist1 x t = do 
        if (x == 0) 
        then do return t
        else do
            i1 <- randomRIO (0, 14 :: Int)
            i2 <- randomRIO (0, 14 :: Int)
            randomlist1 (x-1) (swapElts i1 i2 t) 


crea_tablero :: [Int] -> [[Int]]
crea_tablero x = crea_tablero_aux x tablero
               where crea_tablero_aux _ [] = []
                     crea_tablero_aux y (x:xs) = [crea_tablero_aux2 y x] ++ crea_tablero_aux y xs
                                               where  crea_tablero_aux2 _ [] =[]
                                                      crea_tablero_aux2 x (y:ys) = if (elem y x) then y:(crea_tablero_aux2 x ys) else 0:(crea_tablero_aux2 x ys)          


-- Función que verifica si un estado del tablero es final. Es decir, si solo tiene una bola.
esFinal :: Jugada -> Bool
esFinal (_,_,_,tablero) = (length tablero) == 1


sucesor_jugadas :: [Jugada] -> Solucion -> [NodoSolucion]
sucesor_jugadas [] _ = []
sucesor_jugadas ((pi,pm,pf,tablero):xs) solucion = [(solucion++[(pi,pm ,pf)], (jugadas tablero))] ++ sucesor_jugadas xs solucion

-- Dado un estado del tablero, devuelve un conjunto de las jugadas posibles.
jugadas :: Tablero -> [Jugada]
jugadas tablero = jugadas_aux tablero tablero
                where jugadas_aux [] _ = []
                      jugadas_aux (x:xs) tablero = jugadas_posibles 0 x tablero ++ jugadas_aux xs tablero


jugadas_posibles  :: Int -> Int -> Tablero -> [Jugada]
jugadas_posibles _ _ [] = []
jugadas_posibles it i tablero | (i == 1) = let  c1 = 2
                                                c2 = 3
                                                c3 = 4
                                                c4 = 6
                                            in case it of 
                                                0 -> if (elem c1 tablero) && (not (elem c3 tablero)) then [(i,c1,c3,[ x | x<-tablero , x/=c1 , x/= i]++[c3] )] ++ (jugadas_posibles 1 i tablero) else (jugadas_posibles 1 i tablero)
                                                1 -> if (elem c2 tablero) && (not (elem c4 tablero)) then [(i,c2,c4,[ x | x<-tablero , x/=c2 , x/= i]++[c4] )] else []
                              | (i == 2) = let  c1 = 4
                                                c2 = 5
                                                c3 = 7
                                                c4 = 9
                                            in case it of 
                                                0 -> if (elem c1 tablero) && (not (elem c3 tablero)) then [(i,c1,c3,[ x | x<-tablero , x/=c1 , x/= i]++[c3] )] ++ (jugadas_posibles 1 i tablero) else (jugadas_posibles 1 i tablero)
                                                1 -> if (elem c2 tablero) && (not (elem c4 tablero)) then [(i,c2,c4,[ x | x<-tablero , x/=c2 , x/= i]++[c4] )] else [] 
                              | (i == 3) = let  c1 = 5
                                                c2 = 6
                                                c3 = 8
                                                c4 = 10
                                            in case it of 
                                                0 -> if (elem c1 tablero) && (not (elem c3 tablero)) then [(i,c1,c3,[ x | x<-tablero , x/=c1 , x/= i]++[c3] )] ++ (jugadas_posibles 1 i tablero) else (jugadas_posibles 1 i tablero)
                                                1 -> if (elem c2 tablero) && (not (elem c4 tablero)) then [(i,c2,c4,[ x | x<-tablero , x/=c2 , x/= i]++[c4] )] else [] 
                              | (i == 5) = let  c1 = 8
                                                c2 = 9
                                                c3 = 12
                                                c4 = 14
                                            in case it of 
                                                0 -> if (elem c1 tablero) && (not (elem c3 tablero)) then [(i,c1,c3,[ x | x<-tablero , x/=c1 , x/= i]++[c3] )] ++ (jugadas_posibles 1 i tablero) else (jugadas_posibles 1 i tablero)
                                                1 -> if (elem c2 tablero) && (not (elem c4 tablero)) then [(i,c2,c4,[ x | x<-tablero , x/=c2 , x/= i]++[c4] )] else [] 
                              | (i == 7) = let  c1 = 8
                                                c2 = 4
                                                c3 = 9
                                                c4 = 2
                                            in case it of 
                                                0 -> if (elem c1 tablero) && (not (elem c3 tablero)) then [(i,c1,c3,[ x | x<-tablero , x/=c1 , x/= i]++[c3] )] ++ (jugadas_posibles 1 i tablero) else (jugadas_posibles 1 i tablero)
                                                1 -> if (elem c2 tablero) && (not (elem c4 tablero)) then [(i,c2,c4,[ x | x<-tablero , x/=c2 , x/= i]++[c4] )] else [] 
                              | (i == 8) = let  c1 = 5
                                                c2 = 9
                                                c3 = 3
                                                c4 = 10
                                            in case it of 
                                                0 -> if (elem c1 tablero) && (not (elem c3 tablero)) then [(i,c1,c3,[ x | x<-tablero , x/=c1 , x/= i]++[c3] )] ++ (jugadas_posibles 1 i tablero) else (jugadas_posibles 1 i tablero)
                                                1 -> if (elem c2 tablero) && (not (elem c4 tablero)) then [(i,c2,c4,[ x | x<-tablero , x/=c2 , x/= i]++[c4] )] else [] 
                              | (i == 9) = let  c1 = 8
                                                c2 = 5
                                                c3 = 7
                                                c4 = 2
                                            in case it of 
                                                0 -> if (elem c1 tablero) && (not (elem c3 tablero)) then [(i,c1,c3,[ x | x<-tablero , x/=c1 , x/= i]++[c3] )] ++ (jugadas_posibles 1 i tablero) else (jugadas_posibles 1 i tablero)
                                                1 -> if (elem c2 tablero) && (not (elem c4 tablero)) then [(i,c2,c4,[ x | x<-tablero , x/=c2 , x/= i]++[c4] )] else [] 
                              | (i == 10) = let c1 = 6
                                                c2 = 9
                                                c3 = 3
                                                c4 = 8
                                            in case it of 
                                                0 -> if (elem c1 tablero) && (not (elem c3 tablero)) then [(i,c1,c3,[ x | x<-tablero , x/=c1 , x/= i]++[c3] )] ++ (jugadas_posibles 1 i tablero) else (jugadas_posibles 1 i tablero)
                                                1 -> if (elem c2 tablero) && (not (elem c4 tablero)) then [(i,c2,c4,[ x | x<-tablero , x/=c2 , x/= i]++[c4] )] else [] 
                              | (i == 11) = let c1 = 7
                                                c2 = 12
                                                c3 = 4
                                                c4 = 13
                                            in case it of 
                                                0 -> if (elem c1 tablero) && (not (elem c3 tablero)) then [(i,c1,c3,[ x | x<-tablero , x/=c1 , x/= i]++[c3] )] ++ (jugadas_posibles 1 i tablero) else (jugadas_posibles 1 i tablero)
                                                1 -> if (elem c2 tablero) && (not (elem c4 tablero)) then [(i,c2,c4,[ x | x<-tablero , x/=c2 , x/= i]++[c4] )] else [] 
                              | (i == 12) = let c1 = 13
                                                c2 = 8
                                                c3 = 14
                                                c4 = 5
                                            in case it of 
                                                0 -> if (elem c1 tablero) && (not (elem c3 tablero)) then [(i,c1,c3,[ x | x<-tablero , x/=c1 , x/= i]++[c3] )] ++ (jugadas_posibles 1 i tablero) else (jugadas_posibles 1 i tablero)
                                                1 -> if (elem c2 tablero) && (not (elem c4 tablero)) then [(i,c2,c4,[ x | x<-tablero , x/=c2 , x/= i]++[c4] )] else [] 
                              | (i == 14) = let c1 = 9
                                                c2 = 13
                                                c3 = 5
                                                c4 = 12
                                            in case it of 
                                                0 -> if (elem c1 tablero) && (not (elem c3 tablero)) then [(i,c1,c3,[ x | x<-tablero , x/=c1 , x/= i]++[c3] )] ++ (jugadas_posibles 1 i tablero) else (jugadas_posibles 1 i tablero)
                                                1 -> if (elem c2 tablero) && (not (elem c4 tablero)) then [(i,c2,c4,[ x | x<-tablero , x/=c2 , x/= i]++[c4] )] else [] 
                              | (i == 15) = let c1 = 10
                                                c2 = 14
                                                c3 = 6
                                                c4 = 13
                                            in case it of 
                                                0 -> if (elem c1 tablero) && (not (elem c3 tablero)) then [(i,c1,c3,[ x | x<-tablero , x/=c1 , x/= i]++[c3] )] ++ (jugadas_posibles 1 i tablero) else (jugadas_posibles 1 i tablero)
                                                1 -> if (elem c2 tablero) && (not (elem c4 tablero)) then [(i,c2,c4,[ x | x<-tablero , x/=c2 , x/= i]++[c4] )] else [] 
                              | (i == 4) = let  c1 = 8
                                                c2 = 7
                                                c3 = 5
                                                c4 = 2
                                                c5 = 13
                                                c6 = 11
                                                c7 = 6
                                                c8 = 1
                                            in case it of
                                                0 -> if (elem c1 tablero) && (not (elem c5 tablero)) then [(i,c1,c5,[ x | x<-tablero , x/=c1 , x/= i]++[c5] )] ++ (jugadas_posibles 1 i tablero) else (jugadas_posibles 1 i tablero)
                                                1 -> if (elem c2 tablero) && (not (elem c6 tablero)) then [(i,c2,c6,[ x | x<-tablero , x/=c2 , x/= i]++[c6] )] ++ (jugadas_posibles 2 i tablero) else (jugadas_posibles 2 i tablero)
                                                2 -> if (elem c3 tablero) && (not (elem c7 tablero)) then [(i,c3,c7,[ x | x<-tablero , x/=c3 , x/= i]++[c7] )] ++ (jugadas_posibles 3 i tablero) else (jugadas_posibles 3 i tablero)
                                                3 -> if (elem c4 tablero) && (not (elem c8 tablero)) then [(i,c4,c8,[ x | x<-tablero , x/=c4 , x/= i]++[c8] )] else [] 
                              | (i == 6) = let  c1 = 9
                                                c2 = 10
                                                c3 = 5
                                                c4 = 3
                                                c5 = 13
                                                c6 = 15
                                                c7 = 4
                                                c8 = 1
                                            in case it of
                                                0 -> if (elem c1 tablero) && (not (elem c5 tablero)) then [(i,c1,c5,[ x | x<-tablero , x/=c1 , x/= i]++[c5] )] ++ (jugadas_posibles 1 i tablero) else (jugadas_posibles 1 i tablero)
                                                1 -> if (elem c2 tablero) && (not (elem c6 tablero)) then [(i,c2,c6,[ x | x<-tablero , x/=c2 , x/= i]++[c6] )] ++ (jugadas_posibles 2 i tablero) else (jugadas_posibles 2 i tablero)
                                                2 -> if (elem c3 tablero) && (not (elem c7 tablero)) then [(i,c3,c7,[ x | x<-tablero , x/=c3 , x/= i]++[c7] )] ++ (jugadas_posibles 3 i tablero) else (jugadas_posibles 3 i tablero)
                                                3 -> if (elem c4 tablero) && (not (elem c8 tablero)) then [(i,c4,c8,[ x | x<-tablero , x/=c4 , x/= i]++[c8] )] else [] 
                              | (i == 13) = let c1 = 12
                                                c2 = 14
                                                c3 = 8
                                                c4 = 9
                                                c5 = 11
                                                c6 = 15
                                                c7 = 4
                                                c8 = 6
                                            in case it of
                                                0 -> if (elem c1 tablero) && (not (elem c5 tablero)) then [(i,c1,c5,[ x | x<-tablero , x/=c1 , x/= i]++[c5] )] ++ (jugadas_posibles 1 i tablero) else (jugadas_posibles 1 i tablero)
                                                1 -> if (elem c2 tablero) && (not (elem c6 tablero)) then [(i,c2,c6,[ x | x<-tablero , x/=c2 , x/= i]++[c6] )] ++ (jugadas_posibles 2 i tablero) else (jugadas_posibles 2 i tablero)
                                                2 -> if (elem c3 tablero) && (not (elem c7 tablero)) then [(i,c3,c7,[ x | x<-tablero , x/=c3 , x/= i]++[c7] )] ++ (jugadas_posibles 3 i tablero) else (jugadas_posibles 3 i tablero)
                                                3 -> if (elem c4 tablero) && (not (elem c8 tablero)) then [(i,c4,c8,[ x | x<-tablero , x/=c4 , x/= i]++[c8] )] else [] 
                              | otherwise = []

--Función principal            
main :: IO ()
main = do comeSolo
