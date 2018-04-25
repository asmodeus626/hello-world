type V = Int
data Graph = Graph [(V,[V])]


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
            
            
            
vert::Graph->[V]
vert (Graph l) = map fst l


edges::Graph->[(V,V)]
edges (Graph l) = quitaRep $ concat $ map edge l where
                      edge (v,vs) = [(v,v') | v'<-vs]
                      quitaRep [] = []
                      quitaRep ((x,y):es) = (x,y):(filter (/=(y,x)) $ quitaRep es)       
                  
                
neighbors::V->Graph->[V]
neighbors v (Graph l) = head [snd e | e<-l, fst e==v]
                  
   
dfs::V->Graph->[V]
dfs v g = recorre [v] [] where
          recorre [] vis = vis
          recorre (v:vs) vis | elem v vis = recorre vs vis
                             | otherwise = recorre (neighbors v g ++ vs) (vis++[v])    

bfs::V->Graph->[V]
bfs v g = recorre [v] [] where
          recorre [] vis = vis
          recorre (v:vs) vis | elem v vis = recorre vs vis
                             | otherwise = recorre (vs ++ neighbors v g) (vis++[v])
                             
esConexa::Graph->Bool
esConexa g@(Graph l) = (length $ vert g) == (length $ dfs (fst $ head l) g)




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
                 _ -> error "fuera de índice"
				 
--Ejemplo de un arreglo
arr_alg1 = Arr f 10 where 
          f n = case n of
                 0 -> 100
                 1 -> 70
                 2 -> 80
                 3 -> 60
                 4 -> 90
                 5 -> 20
                 6 -> 10
                 7 -> 17
                 8 -> 2
                 9 -> 85
                 _ -> error "fuera de índice"
				 
--Ejemplo de un arreglo
arr1 = Arr f 4 where 
          f n = case n of
                 0 -> 21
                 1 -> 22
                 2 -> 24
                 3 -> 27
                 _ -> error "fuera de índice"
				 
arr_colores = Arr f 21 where 
                     f n = case n of 
                         0 -> "azul"
                         1 -> "azul"
                         2 -> "verde"
                         3 -> "verde"
                         4 -> "verde"
                         5 -> "verde"
                         6 -> "rojo"
                         7 -> "rojo"
                         8 -> "azul"
                         9 -> "rojo"
                         10 -> "rojo"
                         11 -> "verde"
                         12 -> "verde"
                         13 -> "rojo"
                         14 -> "rojo"
                         15 -> "azul"
                         16 -> "rojo"
                         17 -> "azul"
                         18 -> "azul"
                         19 -> "verde"
                         20 -> "azul"
                         _ -> error "fuera de indice"
						 
                 
--Para obtener el elemento en la i-ésima posición de un arreglo
get::Arreglo a->Int->a
get (Arr f n) i | i>=0 && i < n = f i
                | otherwise = error ("no :v "  ++ (show i))

--Para sobreescribir el elemento en la i-ésima posición
upd::Arreglo a->Int->a->Arreglo a
upd (Arr f n) i x = Arr (\m -> if m==i then x else f m) n


--Para obtener el tamaño de un arreglo
size::Arreglo a->Int
size (Arr _ n) = n

--Para saber si un elemento pertenece a un arreglo
elemArr::Eq a=>a->Arreglo a->Int
elemArr x arr = busca x 0 arr where
                busca x i arr | i < size arr = if x == get arr i then i else busca x (i+1) arr      
                              | otherwise = -1


--Para obtener la posición del mínimo en un arreglo que está en la posición 'i' en adelante.
minArr::Ord a=>Arreglo a->Int->Int
minArr arr i = buscaMin i (i+1) arr where
               buscaMin m_i j arr | j==size arr = m_i
                                  | otherwise = if get arr m_i < get arr j then buscaMin m_i (j+1) arr else buscaMin j (j+1) arr   

maxArr::Ord a=>Arreglo a->Int->Int
maxArr arr i = buscaMax i (i+1) arr where
               buscaMax m_i j arr | j==size arr = m_i
                                  | otherwise = if get arr m_i > get arr j then buscaMax m_i (j+1) arr else buscaMax j (j+1) arr  								  

--Intercambia los elementos en la posición 'i' y 'j' de un arreglo.
swap::Arreglo a->Int->Int->Arreglo a
swap arr i j = let xi = get arr i
                   xj = get arr j
                   f' n | n == i = xj
                        | n == j = xi
                        | otherwise = get arr n in
               Arr f' (size arr)              
                                
swap' :: Arreglo a -> Int -> Int -> Arreglo a
swap' (Arr f n) i j = (Arr (\m -> if(m==i) then f j else (if m == j then f i else f m )) n) 
                                
--Ordena un arreglo con selectionSort
selectionSort::Ord a=>Arreglo a->Arreglo a
selectionSort arr = ordena 0 arr where
                    ordena i arr | i<size arr = let m = minArr arr i 
                                                    arr' = swap arr i m in 
                                                ordena (i+1) arr'              
                                 | otherwise = arr    



bubbleSort::Ord a=>Arreglo a->Arreglo a
bubbleSort arr = ordena arr 0 where
                 ordena arr i | i == size arr-1 = arr 
                              | otherwise = ordena (bubble arr i 0) (i+1) 
                 bubble arr i j | j == (size arr)-i-1 = arr 
                                | otherwise = if get arr j > get arr (j+1)  
                                              then bubble (swap arr j (j+1)) i (j+1) 
                                              else bubble arr i (j+1)                        


insertionSort::Ord a=>[a]->[a]
insertionSort xs = case xs of
                    [] -> []
                    x:xs -> inserta x $ insertionSort xs where
                            inserta x [] = [x]
                            inserta x (y:ys) = if x<y then (x:(y:ys)) 
                                                      else (y:(inserta x ys))  

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
			
alg_pregunta1 :: (Integral a) => Arreglo a -> Arreglo a
alg_pregunta1 a = regresa 0  (Arr (\m -> 0) (size a)) (bucketSort (creaArreglo 0 a (Arr (\m -> 0.0) (size a))))
                  where creaArreglo i a b | (i == size a) = b
				                          | otherwise = creaArreglo (i+1) a (upd b i ((fromIntegral (get a i) :: Float) / (fromIntegral (size a) :: Float)))
                        regresa i a b | (i < size a) = regresa (i+1) (upd a i (truncate ((get b i)  * (fromIntegral (size a) :: Float)))) b
						              | otherwise = a


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
									 								 
	

	
 
mergeSort::Ord a=>[a]->[a]
mergeSort xs = case xs of
                [] -> []
                [x] -> [x]
                xs -> merge (mergeSort x1) (mergeSort x2) where
                      x1 = take k xs
                      x2 = drop k xs	
                      k = div (length xs) 2
                      merge [] ys = ys
                      merge xs [] = xs
                      merge l1@(x:xs) l2@(y:ys) = if x<y then x:(merge xs l2)
                                                  else y:(merge l1 ys) 
												  
												  
busquedaBinaria :: (Ord a) => Arreglo a->a->Int 
busquedaBinaria a x = busquedaBinaria_aux a x 0 ((size a)-1)
                      where busquedaBinaria_aux a t ini fin | (ini > fin) = (-1)
					                                        | (get a mitad) == t = mitad
				                                             | otherwise = if ((get a mitad) < x) then busquedaBinaria_aux a x (mitad+1) fin else busquedaBinaria_aux a x ini (mitad-1)
								                            where mitad = (ini + fin) `div` 2
															
															
reversa :: 	Arreglo a -> Arreglo a 
reversa a = reversa_aux 0 a 
            where reversa_aux i (Arr f n) | (i < (n `div` 2)) = reversa_aux (i+1) (swap (Arr f n) i (n-1-i))
                                          | otherwise = (Arr f n)
											
alg_pregunta2 :: (Ord a)=>(Num a)=>Arreglo a -> Arreglo a -> a -> Maybe (a , a)
alg_pregunta2 a b x = busca 0 a b' x 
					   where b' = heapSort b
					         busca i a b x | i < (size a) = if indice_elem == (-1) then busca (i+1) a b x else Just (get a i , get b indice_elem ) 
					                       | otherwise = Nothing 
										   where indice_elem = busquedaBinaria b (x-(get a i))

alg_pregunta3 :: (Integral a) => Arreglo a-> Int -> Maybe (Int,Int)
alg_pregunta3 a x= encuentra_maximo 0 (-1,-1) a' a' (fromIntegral x)
                   where a' = (heapSort a)
                         encuentra_maximo i tupla@(ia_max , ib_max) a b x | (i < size a) = encuentra_maximo (i+1) (compara_maximo tupla (i , j) a b) a b x
                                                                          | otherwise = if ((ia_max == -1) && (ib_max == -1)) then Nothing else Just (ia_max , ib_max)	
                                                                          where j = busca_maximo b (x-(get a i))
                                                                                compara_maximo t1@(i,j) t2@(h,k) a b | (j == -1) && (k == -1) = t1
                                                                                                                     | (j == -1) || (k == -1) = if (j == -1) then t2 else t1 
                                                                                                                     | otherwise = if (x > y) then t1 else t2 
                                                                                                                     where x = (get a i) + (get b j)
                                                                                                                           y = (get a h) + (get b k)
                                                                                busca_maximo a n = -1					   

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

centro :: String -> Int -> String
centro s n = espacios ++ s ++ espacios
             where espacios = replicate ((n - length s) `div` 2) ' '


pascal_calcula :: Int -> [[Int]]
pascal_calcula 1 = [[1]]
pascal_calcula n = pascal_calcula (n-1) ++ [pascal_calcula_aux n]
                   where pascal_calcula_aux 1 = [1]
                         pascal_calcula_aux n = [1] ++ [x+y | (x,y) <- pares (pascal_calcula_aux (n-1))] ++[1]
                         pares (x:y:ys) = [(x,y)] ++ pares(y:ys)
                         pares _ = []
					   

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


