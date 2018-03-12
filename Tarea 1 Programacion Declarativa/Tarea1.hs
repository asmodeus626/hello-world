{-Facultad de Ciencias UNAM - Programación Declarativa 2018-2 
      Profesor: C. Moisés Vázquez Reyes
      Ayudante: Enrique Antonio Bernal Cedillo
-}

import Unificacion
import System.IO
codifica = hSetEncoding stdout utf8
{-----------------------}
--CÁLCULO LAMBDA PURO--
{-----------------------}
data Lam_U = VarU Nombre | LamU Nombre Lam_U | AppU Lam_U Lam_U 

instance Show Lam_U where
         show t = case t of
                   VarU x -> x
                   LamU x e -> "λ"++x++"."++show e
                   AppU (VarU x) (VarU y) -> x++y 
                   AppU (VarU x) e2 -> x++"("++show e2++")"
                   AppU e1 (VarU y) -> "("++show e1++")"++y

--Aplica la β-reducción de dos términos
betaR :: Lam_U->Lam_U-> Lam_U
betaR (LamU parametro expresion) e2 = formaNormal $ sustituye parametro e2 expresion
betaR t1 t2 = AppU t1 t2

-- Llamada general
formaNormal :: Lam_U-> Lam_U
formaNormal (VarU x) = VarU x
formaNormal (LamU parametro expresion) = LamU parametro (formaNormal expresion)
formaNormal (AppU e1 e2) = betaR (formaNormal e1) (formaNormal e2)

-- Sustituye en todas las apariciones de 'parametro' dentro de 'e1' lo que diga 'e2'
sustituye :: Nombre->Lam_U->Lam_U-> Lam_U
sustituye parametro e1 e2 = fmap' remplaza e2
                            where remplaza = \(VarU x) -> if x == parametro then e1 else (VarU x)
							
-- Aplica la funcion 'f' en toda la expresion
fmap' :: (Lam_U->Lam_U)->Lam_U-> Lam_U
fmap' f (VarU v) = f (VarU v)
fmap' f (LamU v t) = LamU v (fmap' f t)
fmap' f (AppU e1 e2) = AppU (fmap' f e1) (fmap' f e2)

-- (\x.x) y => Deberia dar: y
ejemplo1 = formaNormal $ AppU (LamU "x" (VarU "x")) (VarU "y")

-- (λx.xz) a => Deberia dar: az
ejemplo2 = formaNormal $ AppU (LamU "x" (AppU (VarU "x") (VarU "z"))) (VarU "a")

-- (λx.λy.y x)(λz.u) => Deberia dar: λy.y(λz.u)
ejemplo3 = formaNormal $ AppU (LamU "x" (LamU "y" (AppU (VarU "y") (VarU "x")))) (LamU "z" (VarU "u"))

-- (λy.y a) ((λx.x) (λz.(λu.u) z)) => Deberia dar: a~
ejemplo4 = formaNormal $ AppU (LamU "y" (AppU (VarU "y") (VarU "a"))) (AppU (LamU "x" (VarU "x")) (LamU "z" (AppU (LamU "u" (VarU "u")) (VarU "z"))))

--Codifica los incisos de la pregunta 1

--Codifica los incisos de la pregunta 2
cero = LamU "z" (LamU "w" (VarU "z"))
uno = LamU "x" (LamU "y" (AppU (VarU "y") cero))
dos = LamU "x" (LamU "y" (AppU (VarU "y") uno))
tres = LamU "x" (LamU "y" (AppU (VarU "y") dos))
cuatro = LamU "x" (LamU "y" (AppU (VarU "y") tres))
cinco =  LamU "x" (LamU "y" (AppU (VarU "y") cuatro))
verdadero = LamU "x" (LamU "y" (VarU "x"))
falso = LamU "x" (LamU "y" (VarU "y"))
p2_f2 = LamU "n" (LamU "x" (LamU "y" (AppU (VarU "y") (VarU "n"))))
p2_g2 = LamU "n" (AppU (AppU (VarU "n") cero) (LamU "x" (VarU "x")))
p2_h2 = LamU "n" (AppU (AppU (VarU "n") verdadero) (LamU "x" falso))
ejemplo2a1 = formaNormal $ AppU p2_f2 cero
ejemplo2a2 = formaNormal $ AppU p2_f2 tres
ejemplo2b1 = formaNormal $ AppU p2_g2 uno
ejemplo2b2 = formaNormal $ AppU p2_g2 cuatro
ejemplo2c1 = formaNormal $ AppU p2_h2 cero
ejemplo2c2 = formaNormal $ AppU p2_h2 cinco

--Codifica los incisos de la pregunta 3

{-----------------------}
--INFERENCIA DE TIPOS--
{-----------------------}
--Expresiones LamAB sin anotaciones de tipos.
data LamAB = VNum Int
     | VBool Bool
     | Var Nombre
     | Suma LamAB LamAB
     | Prod LamAB LamAB
     | Ifte LamAB LamAB LamAB
     | Iszero LamAB
     | Lam Nombre LamAB
     | App LamAB LamAB
     deriving Show
     
        
--Expresiones LamAB con anotaciones de tipos.
data LamABT = VNumT Int
     | VBoolT Bool
     | VarT Nombre
     | SumaT LamABT LamABT
     | ProdT LamABT LamABT
     | IfteT LamABT LamABT LamABT
     | IszeroT LamABT
     | LamT Nombre Tipo LamABT
     | AppT LamABT LamABT
     deriving Show


--Para representar un contexto de variables [(x1,T1),...,(xn,Tn)].
type Ctx = [(Nombre,Tipo)]

--Para representar juicios de tipado.
data Juicio = Deriv (Ctx,LamABT,Tipo)

instance Show Juicio where
    show (Deriv (ctx, e, t)) = show ctx++" ⊢ "++show e++" : "++show t




--Realiza la inferencia de tipos de una expresión LamABT
algoritmoW :: LamAB->Juicio
algoritmoW e = let (Deriv (ctx,e',t),_) = w e [] in Deriv (elimRep ctx,e',t) where
                                                           elimRep [] = []
                                                           elimRep (x:xs) = x:(filter (x/=) $ elimRep xs)      


--Realiza el algoritmo W en una expresión LamAB utilizando una lista de nombres que ya están ocupados. 
w::LamAB->[Nombre]->(Juicio,[Nombre])                
w e vars = error "Te toca"


{-PRUEBAS:-}
-- []|-LamT "x" X1 (LamT "y" X0 (VarT "y")):X1->(X0->X0)
prueba1 = algoritmoW $ Lam "x" $ Lam "y" $ Var "y"

-- [("x",X3->(X4->X0)),("y",X3),("z",X4)]|-AppT (AppT (VarT "x") (VarT "y")) (VarT "z"):X0
prueba2 = algoritmoW $ App (App (Var "x") (Var "y")) (Var "z")

-- *** Exception: No se pudo unificar.
prueba3 = algoritmoW $ App (Var "x") (Var "x")

-- []|-LamT "s" X2->X0 (LamT "z" X2 (AppT (VarT "s") (VarT "z"))):(X2->X0)->(X2->X0)
prueba4 = algoritmoW $ Lam "s" $ Lam "z" $ App (Var "s") (Var "z")

-- [("x",X6->(X4->X0)),("z",X6),("y",X6->X4),("z",X6)]|-AppT (AppT (VarT "x") (VarT "z")) (AppT (VarT "y") (VarT "z")):X0
prueba5 = algoritmoW $ App (App (Var "x") (Var "z")) (App (Var "y") (Var "z"))

-- []|-LamT "f" Nat->X0 (LamT "x" Nat (LamT "y" Nat (AppT (VarT "f") (SumaT (VarT "x") (VarT "y"))))):(Nat->X0)->(Nat->Nat->X0)
prueba6 = algoritmoW $ Lam "f" $ Lam "x" $ Lam "y" $ App (Var "f") (Suma (Var "x") (Var "y")) 

-- [("g",X2->X0),("f",Nat->X2),("z",Nat)]|-AppT (VarT "g") (AppT (VarT "f") (ProdT (VNumT 3) (VarT "z"))):X0
prueba7 = algoritmoW $ App (Var "g") (App (Var "f") (Prod (VNum 3) (Var "z")))

-- [("f",X2->Bool),("y",X2)]|-IfteT (IszeroT (SumaT (VNumT 2) (VNumT 0))) (AppT (VarT "f") (VarT "y")) (VBoolT False):Bool
prueba8 = algoritmoW $ Ifte (Iszero $ Suma (VNum 2) (VNum 0)) (App (Var "f") (Var "y")) (VBool False)

-- [("f",X2->X3)]|-LamT "x" X2 (LamT "y" X3 (IfteT (VBoolT True) (AppT (VarT "f") (VarT "x")) (VarT "y"))):X2->(X3->X3)
prueba9 = algoritmoW $ Lam "x" $ Lam "y" $ Ifte (VBool True) (App (Var "f") (Var "x")) (Var "y")
 
-- *** Exception: No se pudo unificar.
prueba10 = algoritmoW $ App (Suma (VNum 1) (Var "n")) (Var "w")






