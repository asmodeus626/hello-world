import System.Random
import Data.List(nub)


type Semilla = Int


numeroRandom::Semilla->Int
numeroRandom s = fst $ random (mkStdGen s) 



numeroRandom_::Semilla->(Int,Int)->Int
numeroRandom_ s (a,b) = fst $ randomR (a,b) (mkStdGen s)


numerosRandom::Semilla->[Int]
numerosRandom s = randoms (mkStdGen s)



numerosRandom_::Semilla->(Int,Int)->[Int]
numerosRandom_ s (a,b) = randomRs (a,b) (mkStdGen s) 


inscripcion = do
               putStrLn "Ingresa tu nombre:";
               nombre <- getLine;
               grupo <- randomRIO ('A','D');
               putStrLn $ "Te tocó en el grupo "++[grupo]


pareja_futuro = do
                 putStrLn "¿Qué estás buscando?\n1)Novio\n2)Novia\n3)No sé";
                 opc <- readLn;
                 case opc of
                  1 -> do
                        i <- randomRIO (0,9);
                        let nombre_novio = novios !! i;
                        putStrLn $ "Tu próximo novio se llamará "++nombre_novio;
                  2 -> do
                        i <- randomRIO (0,9);
                        let nombre_novia = novias !! i;
                        putStrLn $ "Tu próxima novia se llamará "++nombre_novia; 
                  3 -> do
                        i <- randomRIO (0,9);
                        opc_pareja <- randomIO;
                        let parejas = if opc_pareja then novios else novias;
                        let nombre_pareja = parejas !! i;
                        putStrLn $ "Vas a tener" ++ (if opc_pareja then " novio " else " novia ") ++ "y se va a llamar "++nombre_pareja;    
                  _ -> putStrLn "Te vas a quedar solo/a."          
                 where 
                   novios = ["Pancrasio","Bufarreti","Tiburcio","Cástulo","Moy","Kike","Leónidas","Juan Camacho","Nicolás","Ramón"]
                   novias = ["Nicolasa","Tomasa","Emma","Florinda","Ana","Nina","Cristina","Ana Lisa","Mireya","Carmen"]   
                 









