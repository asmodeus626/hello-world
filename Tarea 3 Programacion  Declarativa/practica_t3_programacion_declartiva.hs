import Arreglos
import Graphs_clase
import Data.List


-- funcion que hace el HeapSort
heapSort :: Ord c => Arreglo c -> Arreglo c
heapSort (Arr f n) = construyeArreglo 0 (Arr f n) a1
                    where  a1 = construyeHeap  0 (Arr f 0) (Arr f n)
                           construyeArreglo i a b | (size b > 0) = construyeArreglo (i+1) (upd a i (get b 0)) (eliminaRaizHeap b)
                                                  | otherwise = a
                           construyeHeap i a b = if ( i < size b) then construyeHeap (i+1) (mete (size a) a (get b i)) b else a
                                                 where mete i (Arr f n) x | (i > 0) && ((get (Arr f n) padre) > x) = mete padre (upd (Arr f n) i (get (Arr f n) padre)) x
                                                                          | otherwise = upd (Arr f (n+1)) i x
												                          where padre = if (odd i) then i `div` 2 else  (i `div` 2) -1
                           eliminaRaizHeap (Arr f n)  = revalancea 0 (swap (Arr f n) (n-1) 0) 
                                                        where revalancea  i (Arr f n) | (hijo_izq < (n-1)) && (hijo_der < (n-1)) && (elem_der <= elem_izq) = if  ( i < n) && (elem_der < elem )then  revalancea hijo_der (swap (Arr f n) i hijo_der) else (Arr f (n-1))
                                                                                      | (hijo_izq < (n-1)) && (hijo_der < (n-1)) = if ( i < n) && (elem_izq < elem) then revalancea hijo_izq (swap (Arr f n) i hijo_izq) else (Arr f (n-1))
						                                                              | (hijo_izq < (n-1)) = if ( i < n) && (elem_izq < elem) then revalancea hijo_izq (swap (Arr f n) i hijo_izq) else (Arr f (n-1))
					                                                                  | (hijo_der < (n-1)) = if  ( i < n) && (elem_der < elem )then  revalancea hijo_der (swap (Arr f n) i hijo_der) else (Arr f (n-1))
                                                                                      | otherwise = (Arr f (n-1))
                                                                                      where hijo_izq = (2*i) +1 
                                                                                            hijo_der = hijo_izq +1
                                                                                            elem_der = get (Arr f n) hijo_der
                                                                                            elem_izq = get (Arr f n) hijo_izq
                                                                                            elem = get (Arr f n) i


-- Funcion que hace  BuscketSrot 
bucketSort :: Arreglo Float -> Arreglo Float 
bucketSort a = bucketSort_aux  0 a (creaBuckets 0 (Arr (\m -> (Arr (\m' -> 2.0) 0)) (size a)))
               where bucketSort_aux i a buckets | (i == size a) = vaciaBuckets 0 0 0 (Arr (\m -> 0.0) (size a)) (ordenaBuckets 0 buckets)
                                                | otherwise =  bucketSort_aux (i+1) a (mete buckets 0 (get a i))
                                                where vaciaBuckets i j k a b | (i < size b) = if (j < size bucket) then vaciaBuckets i (j+1) (k+1) (upd a k (get bucket j)) b else vaciaBuckets (i+1) 0 k a b
	                                                                         | otherwise = a
                                                                             where bucket = (get b i) 
                                                      ordenaBuckets i a | (i < size a) =  ordenaBuckets (i+1) (upd a i (heapSort (get a i)))
                                                                        | otherwise = a  
                                                      mete buckets indice elem = if (indice_elem <= elem) && (elem <= (indice_elem +1.0)) then upd  buckets indice (mete_aux (get buckets indice) elem) else mete buckets (indice+1) elem 
                                                                                 where indice_elem = fromIntegral  indice :: Float
                                                                                       mete_aux (Arr f n) elem  = upd (Arr f (n+1)) n elem																			
                     creaBuckets i a | (i == size a) = a
                                     | otherwise = creaBuckets (i+1) (upd a i (Arr (\m -> 0.0) 0))	

busquedaBinaria :: (Ord a) => Arreglo a->a->Int 
busquedaBinaria a x = busquedaBinaria_aux a x 0 ((size a)-1)
                      where busquedaBinaria_aux a t ini fin | (ini > fin) = (-1)
                                                            | (get a mitad) == t = mitad
                                                            | otherwise = if ((get a mitad) < t) then busquedaBinaria_aux a t (mitad+1) fin else busquedaBinaria_aux a t ini (mitad-1)
                                                            where mitad = (ini + fin) `div` 2
									 								 
	
--Algoritmo de la pregunta 1            
alg_pregunta1 :: (Integral a) => Arreglo a -> Arreglo a
alg_pregunta1 a = regresa 0  (Arr (\m -> 0) (size a)) (bucketSort (creaArreglo 0 a (Arr (\m -> 0.0) (size a))))
                  where creaArreglo i a b | (i == size a) = b
                                              | otherwise = creaArreglo (i+1) a (upd b i ((fromIntegral (get a i) :: Float) / (fromIntegral (size a) :: Float)))
                        regresa i a b | (i < size a) = regresa (i+1) (upd a i (truncate ((get b i)  * (fromIntegral (size a) :: Float)))) b
                                            | otherwise = a
												  
												  

															
														
-- Algoritmo de la pregunta 2											
alg_pregunta2 :: (Ord a)=>(Num a)=>Arreglo a -> Arreglo a -> a -> Maybe (a , a)
alg_pregunta2 a b x = busca 0 a b' x 
					   where b' = heapSort b
					         busca i a b x | i < (size a) = if indice_elem == (-1) then busca (i+1) a b x else Just (get a i , get b indice_elem ) 
					                       | otherwise = Nothing 
										   where indice_elem = busquedaBinaria b (x-(get a i))

-- Algoritmo de la pregunta 3
alg_pregunta3 :: (Integral a) => Arreglo a-> Int -> Maybe (Int,Int)
alg_pregunta3 a x= encuentra_maximo 0 (-1,-1) a' a' (fromIntegral x)
                   where a' = transforma (heapSort a)
                         encuentra_maximo i tupla@(ia_max , ib_max) a b x | (i < size a) = encuentra_maximo (i+1) (compara_maximo tupla (i , j) a b) a b x
                                                                          | otherwise = if ((ia_max == -1) && (ib_max == -1)) then Nothing else Just (get a ia_max , get b ib_max)	
                                                                          where j = busca_maximo b (x-(get a i))
                                                                                compara_maximo t1@(i,j) t2@(h,k) a b | (j == -1) && (k == -1) = t1
                                                                                                                     | (j == -1) || (k == -1) = if (j == -1) then t2 else t1 
                                                                                                                     | otherwise = if (x > y) then t1 else t2 
                                                                                                                     where x = (get a i) + (get b j)
                                                                                                                           y = (get a h) + (get b k)
                                                                                busca_maximo a x = busqueda a x 0 ((size a)-1) (-1)
                                                                                                   where busqueda a t ini fin maximo | (ini > fin) = maximo 
                                                                                                                                     | (get a mitad) == t = mitad
                                                                                                                                     | otherwise = if ((get a mitad) < t) then busqueda a t (mitad+1) fin max' else busqueda a t ini (mitad-1) maximo
                                                                                                                                     where mitad = (ini + fin) `div` 2
                                                                                                                                           max' = if (maximo == -1) then mitad else if ((t-(get a mitad)) == (min (t-(get a mitad)) (t-(get a maximo)))) then mitad else maximo

transforma :: (Integral a) => Arreglo a -> Arreglo Int
transforma a = transforma_aux 0 a (Arr (\m -> 0) (size a))
                where transforma_aux i a b | i < size a = transforma_aux (i+1) a (upd b i (fromIntegral (get a i)))
                                           |  otherwise = b 


alg_pregunta4 ::  Arreglo [Char] -> Arreglo [Char]
alg_pregunta4 a = mueveVerdes 0 0 a
                where mueveVerdes i j a | j == (size a) = mueveRojo i i a
				                        | (color a j) == "verde" = mueveVerdes (i+1) (j+1) (swap a i j)
										| otherwise = mueveVerdes i (j+1) a
                                        where mueveRojo i j a | j == (size a) = a
                                                              | (color a j) == "rojo" = mueveRojo (i+1) (j+1) (swap a i j)
                                                              | otherwise = mueveRojo i (j+1) a
                                              color a i = get a i																  
															  
															  
alg_pregunta5 :: (Int , Int) -> (Int , Int) -> (Int , Int , Int)
alg_pregunta5 (a , b) (c, d) = let a' = a*c
                                   b' = b*d
                                   c' = (a+b)*(c+d) in
                                   (a',c'-a'-b',b')	
				

--Funcion que calcula el triangulo de pascal 
pascal :: Int -> IO()
pascal x =  mapM_ putStrLn $ ((flip centro (((x*70)`div`10)) . unwords) . map show) <$> pascal_calcula x

-- Funcion que nos regresa el centro
centro :: String -> Int -> String
centro s n = espacios ++ s ++ espacios
             where espacios = replicate ((n - length s) `div` 2) ' '


-- Funcion que cacula el triangulo de pascal en forma de listas de listas de enteros
pascal_calcula :: Int -> [[Int]]
pascal_calcula 1 = [[1]]
pascal_calcula n = pascal_calcula (n-1) ++ [pascal_calcula_aux n]
                   where pascal_calcula_aux 1 = [1]
                         pascal_calcula_aux n = [1] ++ [x+y | (x,y) <- pares (pascal_calcula_aux (n-1))] ++[1]
                         pares (x:y:ys) = [(x,y)] ++ pares(y:ys)
                         pares _ = []
					   

-- Funcion que nos regresa los digitos  de un numero por ejemplo digitos 123 = [1,2,3]
digitos :: Int -> [Int]
digitos n = map (\x -> read [x] :: Int) (show n)



acarreos :: Int -> Int -> Int
acarreos x y = acarreos_aux 0 (reverse (digitos x)) (reverse (digitos y))
               where acarreos_aux i [] [] = i
                     acarreos_aux i [x] [y] = if (x+y) >= 10 then (i+1) else i
                     acarreos_aux i x@(x':x'':xs) y@(y':y'':ys) | lx > ly = acarreos_aux i y x
									                            | lx < ly = acarreos_aux i (agrega_ceros (ly - lx) x) y
                                                                | otherwise = if (x' + y') >= 10 then acarreos_aux (i+1) ((x''+1):xs) (y'':ys) else acarreos_aux i (x'':xs) (y'':ys)
                                                                where lx = length x
                                                                      ly = length y
                                                                      agrega_ceros 0 x = x
                                                                      agrega_ceros n x = agrega_ceros (n-1) x++[0]											
														
																		
						 
collatz :: Int -> [Int]
collatz 1 = [1]
collatz x = if (odd x) then [x] ++ collatz ((3*x)+1) else [x]++ collatz (x `div` 2)

intercala :: a -> [a] -> [[a]]
intercala x [] = [[x]]
intercala x (y:ys) = (x:y:ys):[y:zs | zs <- intercala x ys]

permutations :: [a] -> [[a]]
permutations [] = [[]]
permutations (x:xs) =  concat [intercala x ys | ys <- permutations xs]
 
primos :: Int ->  Int -> [Int]
primos n m  | (n == 0) || (n ==1) = criba [2..m]
            | otherwise = criba [n..m]
              where criba [] = []
                    criba (n:ns) = n : criba [ x | x <- ns, x `mod` n /= 0 ]




golbach :: Int -> Maybe [(Int , Int, Int)]
golbach n | ( n <= 5 ) = Nothing
          | otherwise = if (null lista_terna) then Nothing else Just lista_terna
          where lista_primos = primos 2 (n-1)
                lista_terna = [(x,y,z) | x <- [1..n]  , y <- [x..n]  , z <- [y..n]  , elem x lista_primos , elem y lista_primos ,  elem z lista_primos , (x+y+z == n) ]
				
				
type Moneda = Int 

type NodoMonedas = (Int , [Moneda])

sucesoresMonedas :: [Moneda] -> NodoMonedas -> [NodoMonedas]
sucesoresMonedas monedas (r,p) = [(r-c, c:p) | c<-monedas , r-c >= 0]

esFinalMonedas :: NodoMonedas -> Bool
esFinalMonedas (v,_) = v==0

cambio :: [Int] -> Int -> [[Int]]
cambio monedas n = elimina_permutaciones (cambio_monedas monedas (sucesoresMonedas monedas (n,[])))
                   where cambio_monedas _ [] = []
                         cambio_monedas monedas (x:xs)  = if esFinalMonedas x then [snd x] ++ cambio_monedas monedas xs 
                         	                                                  else cambio_monedas monedas ((sucesoresMonedas monedas x) ++ xs)
																			  
																			  
																			  
elimina_permutaciones :: (Eq a) => [[a]] -> [[a]]
elimina_permutaciones [] = []
elimina_permutaciones (x:xs) =  [x] ++ elimina_permutaciones [z | z <- xs , not (elem z (permutations x))]

--Funciones auxiliares-------------------------------------------------------------------------------------------------------

--Función que recibe una gráfica y devuelve la misma pero sin la palabra Graph
sacaGraf::Graph->[(V,[V])]
sacaGraf (Graph l) = l

--Función que recibe un vértice y lo elimina de la gráfica. Pero no lo elimina de las vecindades
eliminaVertice::Graph->V->Graph
eliminaVertice (Graph ((a,b):ls)) vertice = if vertice == a then Graph ls
                                            else Graph ((a,b):(sacaGraf (eliminaVertice (Graph ls) vertice)))

--Recibe un vértice y lo elimina de todas las vecindades.
eliminaVecino::Graph->V->Graph
eliminaVecino graf vertice = Graph (map (elimVec vertice) (sacaGraf graf)) where
                             elimVec v (a,b) = (a, delete v b)

--Borra un vértice de la gráfica y de las vecindades.
borraVertice::Graph->V->Graph
borraVertice graf v = eliminaVecino (eliminaVertice graf v) v

--Borra una lista de vértices de una gráfica.
borraVertices::Graph->[V]->Graph
borraVertices graf [] = graf
borraVertices graf (x:xs) = borraVertices (borraVertice graf x) xs

--Función que borra una arista de la gráfica
borraArista::Graph->(V,V)->Graph
borraArista (Graph []) _ = Graph []
borraArista (Graph ((a,b):xs)) (v1,v2) = if a == v1
                                         then Graph ((a,delete v2 b):xs)
                                         else Graph ((a,b):(sacaGraf (borraArista (Graph xs) (v1,v2))))

--Función que divide la gráfica en componentes conexas. Recibe una gráfica, una lista de todos los vértices y devuelve la partición.
compConex::Graph->[V]->[[V]]
compConex graf [] = []
compConex graf l = let listaRes = (dfs (head l) graf)
                   in [listaRes]++(compConex graf (l \\ listaRes))


--Recibe una gráfica, un vérice y decide si es un vértice de corte
esvertDeCorte::Graph->V->Bool
esvertDeCorte graf v = let grafRed = borraVertice graf v
                       in (length (compConex grafRed (vert grafRed))) > (length (compConex graf (vert graf)))


--Recibe una gráfica, una arista y decide si es una arista de corte
esarDeCorte::Graph->(V,V)->Bool
esarDeCorte graf ar = let grafRed = borraArista graf ar
                      in (length (compConex grafRed (vert grafRed))) > (length (compConex graf (vert graf)))


--Recibe una gráfica, un conjunto de vértices y decide si el conjunto es independiente.
esIndependiente::Graph->[V]->Bool
esIndependiente (Graph l) conj = let residuo = filter (\x-> elem (fst x) conj) l
                                 in all (\x->(intersect (snd x) conj)==[]) residuo

--Verifica si una gráfica G es completa
esCompleta::Graph->Bool
esCompleta (Graph l) = let vertices = vert (Graph l)
                       in all (\x->(sort (snd x))==(sort (delete (fst x) vertices))) l

--Función que recibe una gráfica, un conjunto de vértices y decide si dicho conjunto es un clan.
esClan::Graph->[V]->Bool
esClan g conj = esCompleta (borraVertices g ((vert g) \\ conj))

--Función que recibe una lista de aristas y verifica si un vértice v está en la lista.
estaEnLista::V->[(V,V)]->Bool
estaEnLista _ [] = False
estaEnLista v (x:xs) = if v==(fst x) || v==(snd x) then True else estaEnLista v xs

--Función que recibe una lista de aristas, un vértice y saca todas las aristas incidentes en el vértice.
incidentes::V->[(V,V)]->[(V,V)]
incidentes _ [] = []
incidentes v (x:xs) = if v==(fst x) || v==(snd x) then x:(incidentes v xs) else incidentes v xs

--Función que recibe un vértice, una lista de aristas y devuelve la primera arista que contenga a ese vértice.
primerArista::V->[(V,V)]->(V,V)
primerArista v l = if estaEnLista v l then primAr v l else (0,0) where
                   primAr ve (x:xs) = if ve==(fst x) || ve==(snd x) then x else primAr ve xs

--Función que recibe un vértice v, una arista y devuelve el vértice adyacente.
compl::V->(V,V)->V
compl v (a,b) = if a==v then b else a

{-Función que verifica, mediante una lista de aristas, si una gráfica tiene un ciclo.
Utiliza recursión de cola, la primera lista es la que recibe (de aristas), la segunda es donde va guardando
las aristas que ha visitado y la tercera es una pila de los vértices que va visitando-}
tieneCiclo_cola::[(V,V)]->[(V,V)]->[V]->Bool
tieneCiclo_cola [] _ _ = False
tieneCiclo_cola _ _ [] = False
tieneCiclo_cola input current ver = if (estaEnLista (head ver) input) then
                                    let temp = primerArista (head ver) input
                                        input2 = delete temp input
                                        v2 = compl (head ver) temp
                                    in  if estaEnLista v2 current then True
                                        else tieneCiclo_cola input2 (temp:current) (v2:ver)
                                    else tieneCiclo_cola input current (tail ver)
                                    

--Función que recibe una permutación de vértices de una gráfica y decide si vi es adyacente a vi+1.
sonAdyacentes::[(V,V)]->[V]->Bool
sonAdyacentes ar [] = True
sonAdyacentes ar ver = (elem (head ver,last ver) ar || elem (last ver,head ver) ar) && sonAd ar ver where
                       sonAd arist [] = True
                       sonAd arist [e] = True
                       sonAd arist (x:xs) = let v2 = head xs
                                            in if (elem (x,v2) arist) || (elem (v2,x) arist) then
                                                  sonAd arist xs
                                            else False

--Funciones de la tarea------------------------------------------------------------------------------------------------

--Vertices de corte
vertices_corte::Graph->[V]
vertices_corte graf = filter (esvertDeCorte graf) (vert graf)

--Aristas de corte
aristas_corte::Graph->[(V,V)]
aristas_corte graf = filter (esarDeCorte graf) (edges graf)

--Función que recibe una gráfica, obtiene un conjunto independiente maximal.
independienteMax::Graph->[V]
independienteMax graf = let potenciaVert = reverse $ sortOn length (subsequences $ vert graf) --conjunto potencia de los vértices de la gráfica
                        in  sacaInd graf potenciaVert where
                            sacaInd g [] = []
                            sacaInd g (x:xs) = if esIndependiente g x then x else sacaInd g xs

--Función que recibe una gráfica y devuelve un clan maximal
clanMax::Graph->[V]
clanMax graf = let potenciaVert = reverse $ sortOn length (subsequences $ vert graf) --conjunto potencia de los vértices de la gráfica
                        in  sacaClan graf potenciaVert where
                            sacaClan g [] = []
                            sacaClan g (x:xs) = if esClan g x then x else sacaClan g xs

--Función que recibe una gráfica y decide si tiene un ciclo.
tieneCiclo::Graph->Bool
tieneCiclo graf = tieneCiclo_cola (edges graf) [] [(head (vert graf))]


--Función que decide si una gráfica tiene un ciclo hamiltoniano.
hamilton::Graph->Bool
hamilton g = if not (tieneCiclo g) then False else
             ham (permutations (vert g)) (edges g) where
             ham [] _ = False
             ham (x:xs) ar = if sonAdyacentes ar x then True else
                             ham xs ar
