import Control.Monad
--Tipo para construir gráficas polimórficas.
data Graph a = Graph [(a,[a])] deriving Show

--Dos gráficas de ejemplo.
g1 = Graph [(1,[2,3]),
            (2,[1,4]),
            (3,[1,4]),
            (4,[2,3,5]),
            (5,[4]),
            (6,[7]),
            (7,[6])]
            
g2 = Graph [(1,[2,3,4]),
            (2,[]),
            (3,[6]),
            (4,[]),
            (5,[4]),
            (6,[2,5])]
            

--El tipo 'Graph' es un funtor
instance Functor Graph where
   fmap f = \(Graph vs) -> Graph [(f w, map f ws) | (w,ws)<-vs]        
   

--Ejemplo para hacer un fmap sobre las gráficas de arriba.
f 1 = "Lalo"
f 2 = "Lelo"
f 3 = "Lilo"
f 4 = "Lolo"
f 5 = "Lulu"
f 6 = "Lala"  
f _ = "Lero Lero"


--Tipo isomórfico a 'Maybe' para poner de ejemplo las instancias de clase.
data Chance a = Nada | Cajita a deriving Show

--'Chance' es un funtor
instance Functor Chance where
   fmap f = \chance -> case chance of
                        Nada -> Nada
                        Cajita x -> Cajita $ f x   
 
      
--'Chance' es un funtor aplicativo, es decir, puede aplicar funciones dentro de una caja a un argumento dentro de una caja.  
instance Applicative Chance where
   pure x = Cajita x
   (Cajita f) <*> (Cajita x) = Cajita $ f x
   _ <*> _ = Nada

--Combinando el poder de 'fmap' con '<*>' es posible aplicar una función binaria a dos argumentos dentro de una caja.
sumaChances::Chance Int->Chance Int->Chance Int
sumaChances chance_1 chance_2 = fmap (+) (chance_1) <*> (chance_2) 
   

--Para jugar a las bases de datos, una persona es una cadena de texto. 
type Persona = String  
   
--Hace una consulta a la base preguntando por la mamá de una persona.
madre::Persona->Chance Persona
madre p = case p of
           "Lola" -> Cajita "Pita"
           "Carmela" -> Cajita "Ramona" 
           "Ana Lisa" -> Cajita  "Leidi"
           "Patricio" -> Cajita "Margarita"
           "Pita" -> Cajita "Petaca"
           _ -> Nada

--Hace una consulta a la base preguntando por el papá de una persona.
padre::Persona->Chance Persona
padre p = case p of
           "Carmela" -> Cajita "Ramón" 
           "Ana Lisa" -> Cajita  "Leidi"
           "Patricio" -> Cajita "Luis"
           "Israel" -> Cajita "Enrique"
           "Margarita" -> Cajita "Vicente"
           "Ramona" -> Cajita "Roberto"
           "Ramón" -> Cajita "Mauricio"
           _ -> Nada
           
--Haciendo que 'Chance' forme parte de la clase Monad, es posible encadenar cajas como si fuera notación imperativa.
instance Monad Chance where
   m_x >>= f = case m_x of
                Nada -> Nada
                Cajita x -> f x

--Se usa el poder de la función 'bind' (>>=).
abueloMaterno::Persona->Chance Persona
abueloMaterno p = madre p >>= padre 

{-
La notación 'do' se define como sigue:

* do {m} = m                       (El 'do' de una caja 'm' es devolver 'm')
* do {x<-m1;m2} = m1 >>= \x -> m2  (Se extrae el valor de la caja 'm1' en 'x' y se liga a la caja 'm2')
* do {m1;m2} = m1 >>= \_ -> m2     (Si el valor dentro de 'm1' no figura en 'm2', el valor se omite)

-}
--Gracias a la notación 'do' es posible hacer esta maravilla.
ambosAbuelosPapás::Persona->Chance (Persona,Persona)
ambosAbuelosPapás p = do 
                       abM <- abueloMaterno p;
                       papP <- padre p;
                       pap_papP <- padre papP;
                       return (abM,pap_papP) 
                        
                       


{-Ternas pitagóricas de tres maneras diferentes.-}      
--Listas por comprensión.
pitagoras n = [(a,b,c)| a<-[1..n],b<-[a..n],c<-[b..n],a*a + b*b == c*c]

--Notación 'do'.
pitagoras1 n = do 
                a<-[1..n];
                b<-[a..n];
                c<-[b..n];
                guard $ a*a + b*b == c*c;
                return (a,b,c) 
                
--Operadores monádicos.
pitagoras2 n = [1..n] >>= \a ->
               [a..n] >>= \b ->
               [b..n] >>= \c ->
               guard (a*a + b*b == c*c) >>
               return (a,b,c)
               

esPrimo::Int->Bool
esPrimo n | n < 2 = False
          | n == 2 = True
          | even n = False
          | otherwise = null [ m | m <- imparesHasta n, mod n m == 0]  
          where imparesHasta n = [m | m<-[3,5..(div n 2)]] 

--¿Será que este polinomio genera números primos?       
g n = n^2 + n + 41     

--Esta función lo comprueba.
estafa = and $ do
         x <- [1..]
         return $ esPrimo $ g x
   
      
--Para leer desde un archivo.
leerArchivo = do
            file <- readFile "ejemplo"
            putStrLn $ reverse file


pintaNum n = pinta 0 n where
             pinta i n | i==n = show i
                       | otherwise =  show i ++ "\n" ++
                                      pinta (i+1) n    

--Para escribir en un archivo               
pintaInter = do 
            putStrLn $ "Teclea un entero:"
            n <- readLn;
            writeFile "salida" $ pintaNum n
  
  
  
  
