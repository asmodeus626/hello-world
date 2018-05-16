import Control.Parallel.Strategies
import Control.Monad(when,guard,forM,mapM)
import System.Directory(doesFileExist) 
import System.Environment(getArgs,getProgName)   
import System.IO.Error(catchIOError,isDoesNotExistError)
import System.Random
import System.Exit





--Un arreglo es una función de los enteros en un tipo 'a'; la segunda componente es para conocer su longitud.
data Arreglo a = Arr (Int->a) Int


--Para pintar los arreglos de forma bonita
instance (Show a) => Show (Arreglo a) where
      show (Arr f n) =  "{"++ pinta 0 n f where 
                              pinta i n f | n==0 = "}" 
                                          | i==0 = show (f i)++pinta (i+1) n f 
                                          | i==n = "}"
                                          | otherwise = ","++show (f i)++pinta (i+1) n f

--Ejemplo de un arreglo
arr = Arr f 11 where 
          f n = case n of
                 0 -> 2
                 1 -> 1
                 2 -> 23
                 3 -> 3
                 4 -> 0
                 5 -> 4
                 6 -> 10
                 7 -> 11
                 8 -> 22
                 9 -> 9
                 10 -> 50
                 _-> error "fuera de índice"

instance Functor Arreglo where  
    fmap f (Arr t n) = Arr (\x->f (t x)) n


type R = Double
data C = Punto(R , R) deriving(Eq,Show)

instance Num C where
    Punto (x1 , x2) + Punto (y1 , y2) = Punto (x1+y1 , x2+y2)
    Punto (x1 , x2) * Punto (y1 , y2) = Punto ((x1*y1)+(y2*x2*(-1)) , (x2*y1)+(y2*x1))

-- Definicion de raiz con el algoritmo babilonico
raiz :: (Eq a) => Floating a => a -> a 
raiz x = raiz_aux x x 0
        where raiz_aux x r t = if (t  == r) then r else raiz_aux x ((0.5)*((x/r)+r)) r 

--norma sin usar la raiz cuadrada de haskell 
norma :: C -> R
norma (Punto(a,b)) = raiz ((a**2)+(b**2))


plano :: R -> [C]
plano x =  plano_aux desplazamiento (desplazamiento' -1) (1-desplazamiento') 
        where desplazamiento = 2.0/x
              desplazamiento' = desplazamiento/2.0
              plano_aux desplazamiento x y | x > (1-desplazamiento') = plano_aux desplazamiento (desplazamiento' -1) (y-desplazamiento)
                                           | y < (desplazamiento' -1) = []
                                           | otherwise = [Punto(x,y)] ++ (plano_aux desplazamiento (x+desplazamiento) y )
                                           where desplazamiento' = desplazamiento/2


esMandelbrot :: C -> Bool
esMandelbrot x = conjuntoMandelbrot (Punto(0,0)) x 200
               where  conjuntoMandelbrot  ultimo x i | i == 0 = True
                                                     | otherwise = if (norma ultimo) <= 2 then conjuntoMandelbrot ((ultimo*ultimo)+x) x (i-1) else False

mandelbrot_set::[C]->String 
mandelbrot_set [] = ""
mandelbrot_set (x:xs) = if (esMandelbrot x) then "1 " ++ (mandelbrot_set xs) else "0 " ++ (mandelbrot_set xs) 

prueba = [Punto(1,1) , Punto(0.5,0) , Punto(0,0)]


eratostenes :: [Int] -> [Int] -- Criba de Eratóstenes (de una lista dada [2..n] te deja sólo los números primos)
eratostenes [] = []
eratostenes (x:xs) | x == 1 = eratostenes xs 
                   | not (null xs) && x^2 > last xs = (x:xs)
                   | otherwise = x: eratostenes [y | y <- xs, y `mod` x /= 0]


mandelbrot_set_parallel::[C]->String
mandelbrot_set_parallel [] = ""
mandelbrot_set_parallel (x:xs) = runEval $ do 
                                            condicion <- rpar $ esMandelbrot x
                                            conjunto <- rpar $ mandelbrot_set_parallel xs
                                            if (condicion)
                                            then return ("1 " ++ conjunto)
                                            else return ("0 " ++ conjunto)

escribir_imagen = do
                   putStrLn "Cual es el tamaño de la imagen"
                   putStrLn "Digite Ancho"
                   ancho <- getLine
                   putStrLn "Digite Alto"
                   alto <- getLine
                   putStrLn "Elija el modo \nseq : para secuencial \npar: para paralelo"
                   modo <- getLine
                   if (igual modo "seq") 
                   then writeFile "imagen.ppm" ("P2 \n" ++  ancho ++ " " ++ alto ++ "\n1\n" ++ (mandelbrot_set (plano (raiz ((m ancho) * (m alto))))))
                   else writeFile "imagen.ppm" ("P2 \n" ++  ancho ++ " " ++ alto ++ "\n1\n" ++ (mandelbrot_set_parallel (plano (raiz ((m ancho) * (m alto))))))
                   where m s = read s ::R
                         igual [] [] = True
                         igual _ [] = False
                         igual [] _ = False
                         igual (x:xs) (y:ys) = if x == y then igual xs ys else False

goldbach::Int->[(Int , Int, Int)]
goldbach n = do guard (n > 5) >> return (eratostenes [1..n]) >>= (\t ->[(x,y,z) | x <- [1..n]  , y <- [x..n]  , z <- [y..n] , elem x t  ,  elem y t, elem z t , (x+y+z == n) ]) 


main = do
       args <- getArgs
       if (igual  (args !! 1) "seq") 
            then writeFile "imagen_fractal.ppm" ("P2 \n" ++  (args !! 0) ++ " " ++ (args !! 0) ++ "\n1\n" ++ (mandelbrot_set (plano (m (args !! 0)))))
       else writeFile "imagen_fractal_paralelo.ppm" ("P2 \n" ++  (args !! 0) ++ " " ++ (args !! 0)++ "\n1\n" ++ (mandelbrot_set_parallel (plano (m (args !! 0)))))
       where m s = read s ::R
             igual [] [] = True
             igual _ [] = False
             igual [] _ = False
             igual (x:xs) (y:ys) = if x == y then igual xs ys else False

newtype State s a = ST (s->(a,s))
type Stack a = [a]

runState::State s a->s->(a,s)
runState (ST st) s = st s

instance Functor (State s) where
   fmap = error "te toca"

instance Applicative (State s) where
   pure = error "te toca"
   (<*>) = error "te toca"
   
instance Monad (State s) where
   return x = ST (\s -> (x,s))
   st >>= f = ST (\s -> let (a,s1) = runState st s in 
                        runState (f a) s1)
         


incrementa :: State Int ()
incrementa = ST (\n -> ( (), n+1))

length1::[Int]->State Int Int
length1 a = length2 a (length a)
          where length2 [] a = ST (\n -> (a , n))
                length2 (x:xs) a = do 
                                    if ( x == 1) 
                                    then do incrementa;
                                            length2 xs a
                                    else do length2 xs a

-- tiene longitud 12 y tiene 4 unos
ejemplo4 = runState (length1 [1,2,1,4,1,6,7,8,1,10,11,12]) 0




minArrState::Ord a=>Arreglo a->Int->Int 
minArrState a i = aux1 (aux2 a i)
                where aux1 (ST st) = snd (st (-1))
                      aux2 a i= do 
                                   if (i >= size a) 
                                    then getState
                                    else do
                                      updState i
                                      aux3 a (i+1)
                                where aux3 a i = do 
                                                 if (i >= size a) 
                                                 then getState
                                                 else do
                                                    minimo a i;
                                                    aux3 a (i+1)
                                                 where minimo a i = ST $ \n -> if (get a i) < (get a n) then (() , i) else (() , n)


fib_par::Int->Int
fib_par 0 = 1
fib_par 1 = 1
fib_par n = runEval $ do
            n1 <- rpar $ fib_par (n-1);
            n2 <- rpar $ fib_par (n-2);
            return $ n1+n2
            

selectionSortState::Ord a=>Arreglo a->Arreglo a 
selectionSortState arr = aux1 (aux2 0 arr) where
                         aux1 (ST st) = snd (st arr)
                         aux2 i arr  =  do 
                                        updState arr 
                                        ordena i (size arr) 
                         ordena i termino = do 
                                        if (i < termino)
                                        then do 
                                             swap i;
                                             ordena (i+1) termino
                                        else getState
                                        where   swap  i   = ST (\n -> let j = (minArrState n i)
                                                                          xi = get n i
                                                                          xj = get n j
                                                                          f' t | (t == i) = xj
                                                                               | (t == j) = xi
                                                                               | otherwise = get n t
                                                                       in ((), (Arr f' (size arr)) ))

getState = ST $ \s -> (s,s) 

updState s = ST $ \_ -> ((),s)

get::Arreglo a->Int->a
get (Arr f n) i | i>=0 && i < n = f i
                | otherwise = error ("no :v "  ++ (show i))

--Para sobreescribir el elemento en la i-ésima posición
upd::Arreglo a->Int->a->Arreglo a
upd (Arr f n) i x = Arr (\m -> if m==i then x else f m) n


--Para obtener el tamaño de un arreglo
size::Arreglo a->Int
size (Arr _ n) = n

ejemplo5 =  selectionSortState arr


--El tipo para hacer cómputos CPS
newtype Continuation r a = Cont ((a->r)->r)

--Para correr un cómputo CPS.
runCont::Continuation r a->(a->r)->r
runCont (Cont cont) k = cont k 


instance Functor (Continuation r) where
   fmap = error "Te toca"


instance Applicative (Continuation r) where
   pure = error "Te toca"
   (<*>) = error "Te toca"


--Los cómputos CPS son una mónada.
instance Monad (Continuation r) where
   return x = Cont $ \k -> k x
   ka >>= f = Cont $ \k -> runCont ka $ \a -> runCont (f a) k


--Multiplicación de los elementos en una lista con la mónada CPS.
cpsmultM::[Int]->Continuation r Int
cpsmultM [] = return 1
cpsmultM (n:ns) = do 
                   m <- cpsmultM ns;
                   return $ n*m


lengthcps::[a]->Continuation r Int 
lengthcps [] = return 0
lengthcps (x:xs) = do
                   m <- lengthcps xs;
                   return $ 1+m




takecps::Int->[a]->Continuation r [a] 
takecps 0 x = return []
takecps _ [] = return []
takecps n (x:xs) = do
                    m <- takecps (n-1) xs
                    return $[x]++m

dropcps::Int->[a]->Continuation r [a] 
dropcps 0 x = return x
dropcps _ [] = return []
dropcps n (x:xs) = do 
                    m <- dropcps (n-1) xs
                    return m
getPass2 = do
              putStrLn "Ingresa un password (debe contener un número, una mayúscula, y tener mínimo 8 símbolos)";
              pass <- getLine;
              when (length pass < 8) 
                   (do putStrLn "user error La contraseña debe contener 8 símbolos."; getPass2; exitSuccess);
              when (not $ any (\c -> elem c ['0'..'9']) pass) 
                   (do putStrLn "user error La contraseña debe contener un número."; getPass2; exitSuccess);
              when (not $ any (\c -> elem c ['A'..'Z']) pass) 
                   (do putStrLn "user error La contraseña debe contener una letra mayúscula."; getPass2; exitSuccess);
              putStrLn "Contraseña almacenada"


--Hacemos uso de las funciones 'forM' y 'mapM'.
dieta = do   
    putStrLn "\tORGANIZA TU DIETA";  
    let dias = ["lunes","martes","miércoles","jueves","viernes"];
        platillos = ["Tostadas de pollo" , "Pozole" , "Verduras al vapor" , "Quesadillas" , "Ensalada Cesar",
                    "Tamales" , "Pollo con Mole" , "Pechuga Empanizada con papas", "Chilaquiles Verdes" , "Pambazos",
                    "Tacos al pastor" , "Sopes" , "Pancita" , "Chiles Rellenos" , "Picadillo" , "Torta de milanesa" , 
                    "Barbacoa" , "Torta ahogada" , "Carnitas" , "Pescado zarandeado"];
    plan <- forM dias (\dia -> do  
                                putStrLn $ "¿Qué vas a comer el "++dia++"?";  
                                indice <- getStdRandom (randomR (0, 19))
                                return (platillos!!indice) )  
    putStrLn "Tu dieta esta semana va a ser:";  
    mapM putStrLn $ zipWith (\dia comida-> "\t"++ m dia++": "++comida) dias plan
      where 
         m (x:xs) = (cambia x):xs 
         cambia c = ['A'..'Z'] !! (pos c ['a'..'z'])
         pos c (y:ys) = if y==c then 0 else 1+(pos c ys)