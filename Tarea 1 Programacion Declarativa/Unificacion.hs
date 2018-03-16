{-Facultad de Ciencias UNAM - Programación Declarativa 2018-2 
      Profesor: C. Moisés Vázquez Reyes
      Ayudante: Enrique Antonio Bernal Cedillo
-}

module Unificacion where

infixr :-> {- Así, el poderador ':->' asocia a la derecha. -}
type Nombre = String

-- Categoría de tipos.
data Tipo = TNat | TBool | X Nombre | Tipo :-> Tipo deriving Eq


instance Show Tipo where
     show t = case t of
            TNat -> "ℕ"
            TBool -> "𝔹"
            X name -> name 
            TNat:->TNat -> "ℕ" ++"->"++"ℕ"
            TNat:->TBool -> "ℕ" ++"->"++"𝔹"
            TNat:->(X name) -> "ℕ"++"->"++name
            TNat:->(t1:->t2) -> "ℕ"++"->("++show t1++"->"++show t2++")"
            TBool:->TBool -> "𝔹" ++"->"++"𝔹"
            TBool:->TNat -> "𝔹" ++"->"++"ℕ"
            TBool:->(X name) -> "Bool"++"->"++name
            TBool:->(t1:->t2) -> "𝔹"++"->("++show t1++"->"++show t2++")"
            (X name):->TNat -> name++"->"++"ℕ"
            (X name):->TBool -> name++"->"++"𝔹"
            (X name1):->(X name2) -> name1++"->"++name2
            (X name):->(t1:->t2) -> name++"->("++show t1++"->"++show t2++")"
            (t1:->t2):->TNat -> "("++show t1++"->"++show t2++")"++"->"++"ℕ"
            (t1:->t2):->TBool -> "("++show t1++"->"++show t2++")"++"->"++"𝔹"
            (t1:->t2):->(X name) -> "("++show t1++"->"++show t2++")"++"->"++name
            (t1:->t2):->(t3:->t4) -> "("++show t1++"->"++show t2++")"++"->("++show t3++"->"++show t4++")"


--Una sustitución es un conjunto de la forma [(xi, Ti)]
type Sust = [(Nombre, Tipo)]


--Elimina sustituciones de la forma [X:=X] en una sustitución.
simpSust::Sust->Sust
simpSust [] = []
simpSust ((nombre , tipo):xs) = case tipo of 
				X name -> if (nombre  == name) then simpSust xs else [(nombre , tipo)]  ++ simpSust xs
				TNat -> [(nombre , tipo)] ++ simpSust xs
				TBool -> [(nombre , tipo)]  ++ simpSust xs 
				t1 :-> t2 -> [(nombre , tipo)]  ++ simpSust xs 

--Realiza la composición de dos sustituciones.
compSust::Sust->Sust->Sust
compSust s1 s2 = [(nombre , apSustT tipo s2) | (name , tipo) <- s1] ++ [(n,t) | (n,t) <- g , not (elem n (map fst s1))]


--Aplica una sustitución a un tipo.
apSustT::Tipo->Sust->Tipo 
apSustT TNat sust = TNat
apSustT TBool sust = TBool
apSustT (X name) [] = (X name)
apSustT (X name) ((nombre, tipo):xs) = if name == nombre then apSustT tipo xs else apSustT (X name) xs
apSustT (t1:->t2) sust = (apSustT t1 sust) :-> (apSustT t2 sust) 


--Unifica dos tipos.
unifica::Tipo->Tipo->[Sust]
unifica (X name_x) (X name_y) =  if name_x=name_y then [[]] else [[(name_x , (X name_y)]]
unifica t (X name ) =  unifica (X name) t
unifica (X name) t = if (esta_variable name t) then [] else [(name,t)]
unifica (t1 :-> t2) (t3 :-> t4) = [compSust s1 s2 | s1 <- (unifica t1 t3) , s2 <- (unifica (apSustT t2 s1) (apSustT t4 s2))]

esta_variable :: Nombre -> Tipo -> Boolean
esta_variable x (X name) = (x == name)
esta_variable x (t1 :-> t2) = (esta_variable x t1) || (esta_variable x t2)
esta_variable x t = False 


--Unifica una lista de tipos.
unificaConj::[(Tipo,Tipo)]->[Sust]
unificaConj [] = [[]]
unificaConj ((t1,t2):ts) = [compSust s1 s2 | s1 <- unifica t1 t2, s2 <- unificaConj [(apSustT (fst t) s1,apSustT (snd t) s1) | t <- ts]]



	