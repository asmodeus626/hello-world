{-Facultad de Ciencias UNAM - Programación Declarativa 2018-2 
      Profesor: C. Moisés Vázquez Reyes
      Ayudante: Enrique Antonio Bernal Cedillo
-}

import System.IO
codifica = hSetEncoding stdout utf8
--Los átomos son cadenas.
type At = String
--Fórmulas de la lógica proposicional en forma normal negativa.
data F = Var At | Neg At | Conj F F | Disy F F deriving Eq 

--Para pintar fórmulas de forma especial.
instance Show F where
   show f = case f of
             Var p -> p
             Neg p -> "(no "++p ++ ")"
             Conj f1 f2 -> case f1 of
                            Var _ -> show f1 ++ " y " ++ case f2 of
                                                        Var _ -> show f2
                                                        Neg _ -> show f2
                                                        _ -> "("++show f2++")"
                            Neg _ -> show f1 ++ " y " ++ case f2 of
                                                        Var _ -> show f2
                                                        Neg _ -> show f2
                                                        _ -> "("++show f2++")"
                            _ -> "("++show f1 ++ ") y " ++ case f2 of
                                                        Var _ -> show f2
                                                        Neg _ -> show f2
                                                        _ -> "("++show f2++")"  
             Disy f1 f2 -> case f1 of
                            Var _ -> show f1 ++ " o " ++ case f2 of
                                                        Var _ -> show f2
                                                        Neg _ -> show f2
                                                        _ -> "("++show f2++")"
                            Neg _ -> show f1 ++ " o " ++ case f2 of
                                                        Var _ -> show f2
                                                        Neg _ -> show f2
                                                        _ -> "("++show f2++")"
                            _ -> "("++show f1 ++ ") o " ++ case f2 of
                                                        Var _ -> show f2
                                                        Neg _ -> show f2
                                                        _ -> "("++show f2++")"                                                                                                      

--Para negar fórmulas en general.
neg::F->F
neg f = case f of
         Var p -> Neg p
         Neg p -> Var p
         Conj f1 f2 -> Disy (neg f1) (neg f2)
         Disy f1 f2 -> Conj (neg f1) (neg f2)


--La implicación es un caso particular de la disyunción.
imp::F->F->F
imp f1 f2 = Disy (neg f1) f2

{-=============================================================-}
--Una literal es una fórmula atómica o la negación de una fórmula atómica.
--Se considera que una 'Literal' únicamente es de la forma 'Var _' o 'Neg _'.
type Literal = F
--Una cláusula es una literal o una disyunción de literales.
--La lista [l1,l2,..,lk] significa (l1 ⋁ l2 ⋁..⋁ lk)
type Clausula = [Literal]

type Regla = String 
data ArbolDPLL = Satisfacible | Insatisfacible | R1 [Clausula] ArbolDPLL Regla | R2 [Clausula] ArbolDPLL [Clausula] ArbolDPLL Regla 

instance Show ArbolDPLL where 
    show t = pinta t 0 where 
	         pinta t n = case t of 
                         Satisfacible -> (replicate n ' ')++"Satisfacible"
                         Insatisfacible -> (replicate n ' ')++"Insatisfacible"
                         R1 cl t' r -> (replicate n ' ')++show cl ++ show r++"\n"++ pinta t' n
                         R2 c1 a1 c2 a2 r -> (replicate n ' ')++show r ++"\n"++ 
						                     (replicate n ' ')++ show c1 ++ "\n" ++  
											 pinta a1 (n+3) ++ "\n" ++ 
											 (replicate n ' ') ++ show c2 ++ "\n" ++
											 pinta a2 (n+3) 
						 
--Transforma una fórmula a FNC.
fnc::F->F
fnc f = case f of 
			Var p -> Var p
			Neg p -> Neg p
			Conj f1 f2 -> Conj (fnc f1) (fnc f2)
			Disy (Conj f1 f2) f3 -> Conj (fnc (Disy f1 f3)) (fnc (Disy f2 f3))
			Disy f1 (Conj f2 f3) -> Conj (fnc (Disy f1 f2)) (fnc (Disy f1 f3))
			Disy (Disy f1 f2) f3 -> fnc (Disy f1 (fnc (Disy f2 f3)))
			f-> f


--Obtiene las cláusulas de una fórmula.
clausulas::F->[Clausula]
clausulas f = case f of 
               Var p -> [[Var p]]
               Neg p -> [[Neg p]]
               Conj (Conj f1 f2) (Conj f3 f4) -> clausulas (Conj f1 f2) ++ clausulas (Conj f3 f4)
               Conj (Conj f1 f2) f3 -> clausulas (Conj f1 f2) ++ [clausulas_aux f3]
               Conj f1 (Conj f2 f3) -> clausulas (Conj (Conj f2 f3) f1)
               Conj f1 f2 -> clausulas f1 ++ clausulas f2
               Disy f1 f2 -> [clausulas_aux f1 ++ clausulas_aux f2]



clausulas_aux :: F -> Clausula
clausulas_aux f = case f of 
               Var p -> [Var p]
               Neg p -> [Neg p]
               Disy f1 f2 -> clausulas_aux f1 ++ clausulas_aux f2 
			   
dpll :: [Clausula] -> ArbolDPLL
dpll [] = Satisfacible
dpll ([]:xs) = Insatisfacible
dpll x | (tiene_tautologia x) = let y = (elimina_tautologia x)
                                          in R1 y (dpll y) "Elimina Tautologia"
			| (literal_unica /= Nothing) = let y = borra_unitaria lit_unit x
                                in R1 y (dpll y) ("Clausula Unitaria " ++ show lit_unit)
			| (literal_puro	/= Nothing) = let y = borra_puro lit_puro x
                                       in R1 y 	(dpll y) ("Literal Puro " ++ show lit_puro)		
		    | otherwise = let { y =(head (head x))
                              ; x' = x ++ [[y]]
                              ; x'' = x ++ [[(neg y)]]
                              }
                           in R2 x' (dpll x') x'' (dpll x'') ("Regla de Bifurcacion con " ++ show y)
            where literal_unica = (tiene_unitaria x);
			      literal_puro = (tiene_literal_puro x);
				  lit_puro = case literal_puro of
				              Just x -> x
                  lit_unit = case literal_unica of 
				              Just x -> x
							  
borra_puro :: Literal -> [Clausula] -> [Clausula]
borra_puro _ [] = []
borra_puro x (y:ys) = if elem x y then borra_puro x ys else [y] ++ borra_puro x ys

tiene_literal_puro :: [Clausula] -> Maybe Literal
tiene_literal_puro [] = Nothing
tiene_literal_puro (x:xs) =  lit_puro_aux x xs (x:xs)
                            where lit_puro_aux [] [] _=  Nothing
                                  lit_puro_aux [] (x:xs) y = lit_puro_aux x xs y
                                  lit_puro_aux (x:xs) t y = if (es_puro x y) then Just x else  (lit_puro_aux xs t y)
  
es_puro :: Literal -> [Clausula] -> Bool
es_puro _ [] = True
es_puro x (y:ys) =  if (elem (neg x) y) then False else es_puro x ys


tiene_unitaria :: [Clausula] -> Maybe Literal
tiene_unitaria [] = Nothing 
tiene_unitaria (x:xs) = if length x == 1 then Just (head x) else tiene_unitaria xs

borra_unitaria :: Literal -> [Clausula] -> [Clausula]
borra_unitaria _ [] = []
borra_unitaria y (x:xs) | elem y x = borra_unitaria y xs
                        | elem (neg y) x = [filter (/= (neg y)) x] ++ borra_unitaria y xs 
						| otherwise = [x] ++ (borra_unitaria y xs)
						
tiene_tautologia :: [Clausula] -> Bool
tiene_tautologia [] = False
tiene_tautologia (x:xs) = if tiene_var_comp x then True else tiene_tautologia xs  

tiene_var_comp :: Clausula -> Bool
tiene_var_comp [] = False
tiene_var_comp ((Var x):xs) =  if elem (Neg x) xs then True else tiene_var_comp xs
tiene_var_comp ((Neg x):xs) =  if elem (Var x) xs then True else tiene_var_comp xs

elimina_tautologia :: [Clausula] -> [Clausula]
elimina_tautologia [] = []
elimina_tautologia (x:xs) = if tiene_var_comp x then xs else [x] ++ elimina_tautologia xs

prueba1 = neg ( Conj (Var "p") (imp (Var "q") (Var "r")))
prueba2 = imp (Conj (imp (Var "p") (Var "q")) (imp (Var "q") (Var "p"))) (Var "r")
prueba3 =  [[Neg "a", Var "b"] ,[Var "a"] ,[Neg "a"]]
prueba4 = [[Var "p" , Var "q"],[Var "p" , Neg "q"] , [Var "r" , Var "q"] , [Var "r" , Neg "q"]]
prueba5 = [[Var "p" , Neg "q"] , [Var "q" , Neg "p"]]
prueba6 = concat$ map clausulas $map fnc  $[Disy (Var "a") (Var "b"),imp (neg $ Var "c") (neg $ Var "a")]++[neg $ imp (Var "b") (neg $ Var "c")]
prueba8 = [Disy (imp (Var "s") (Var "p")) (imp (Var "t") (Var "q"))] ++ [neg (Disy (imp (Var "s") (Var "q")) (imp (Var "t") (Var "p")))]
prueba8_1 = concat $ map clausulas $ map fnc $ [Disy (imp (Var "s") (Var "p")) (imp (Var "t") (Var "q"))] ++ [neg (Disy (imp (Var "s") (Var "q")) (imp (Var "t") (Var "p")))]
prueba9 = [Conj (Var "p") (Var "q") , Conj (Var "r") (neg (Var "s")) , imp (imp (Var "q") (Var "p")) (Var "t") , imp (imp (Var "t") (Var "r")) (Disy (Var "s") (Var "w"))] ++ [neg (Var "w")]
prueba9_1 = concat $ map clausulas $ map fnc $ [Conj (Var "p") (Var "q") , Conj (Var "r") (neg (Var "s")) , imp (imp (Var "q") (Var "p")) (Var "t") , imp (imp (Var "t") (Var "r")) (Disy (Var "s") (Var "w"))] ++ [neg (Var "w")]
prueba7 = concat $ map clausulas $ map fnc $[Disy (imp (Var "p") (Var "r")) (Conj (neg (Var "s")) (Var "p")) , imp (Var "s") (neg (Conj (Var "p") (Var "r")))] ++ [neg (neg (Disy (Var "r") (neg (Var "s"))))]

ejer1_1 = dpll $ concat $ map clausulas $ [Disy (Var "a") (Var "b"),imp (neg $ Var "c") (neg $ Var "a")]++[neg $ imp (Var "b") (neg $ Var "c")]
ejer1_2 = dpll $ concat $ map clausulas $ map fnc [Disy (imp (Var "p") (Var "r")) (Conj (neg (Var "s")) (Var "p")) , imp (Var "s") (neg (Conj (Var "p") (Var "r")))] ++ [neg (neg (Disy (Var "r") (neg (Var "s"))))]
ejer1_3 = dpll $ concat $ map clausulas $ map fnc [Disy (imp (Var "s") (Var "p")) (imp (Var "t") (Var "q"))] ++ [neg (Disy (imp (Var "s") (Var "q")) (imp (Var "t") (Var "p")))]
ejer1_4 = dpll $ concat $ map clausulas $ map fnc [Conj (Var "p") (Var "q") , Conj (Var "r") (neg (Var "s")) , imp (imp (Var "q") (Var "p")) (Var "t") , imp (imp (Var "t") (Var "r")) (Disy (Var "s") (Var "w"))] ++ [neg (Var "w")]


ejer2_a = error "Te toca"
ejer2_b = error "Te toca"

