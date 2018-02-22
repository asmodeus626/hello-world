
--Los átomos son cadenas.
type At = String
--Fórmulas de la lógica proposicional en forma normal negativa.
data F = Var At | Neg At | Conj F F | Disy F F deriving Eq 

--Para pintar fórmulas de forma especial.
instance Show F where
   show f = case f of
             Var p -> p
             Neg p -> "¬"++p
             Conj f1 f2 -> case f1 of
                            Var _ -> show f1 ++ "∧" ++ case f2 of
                                                        Var _ -> show f2
                                                        Neg _ -> show f2
                                                        _ -> "("++show f2++")"
                            Neg _ -> show f1 ++ "∧" ++ case f2 of
                                                        Var _ -> show f2
                                                        Neg _ -> show f2
                                                        _ -> "("++show f2++")"
                            _ -> "("++show f1 ++ ")∧" ++ case f2 of
                                                        Var _ -> show f2
                                                        Neg _ -> show f2
                                                        _ -> "("++show f2++")"  
             Disy f1 f2 -> case f1 of
                            Var _ -> show f1 ++ "∨" ++ case f2 of
                                                        Var _ -> show f2
                                                        Neg _ -> show f2
                                                        _ -> "("++show f2++")"
                            Neg _ -> show f1 ++ "∨" ++ case f2 of
                                                        Var _ -> show f2
                                                        Neg _ -> show f2
                                                        _ -> "("++show f2++")"
                            _ -> "("++show f1 ++ ")∨" ++ case f2 of
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

--Definición recursiva de un tableaux semántico.
data Tableaux = Tache | Bolita | 
                R1 [F] Tableaux |
                R2 Tableaux Tableaux 

--Para pintar los tableaux de forma especial.
instance Show Tableaux where
   show t = pinta t 0 where
            pinta t n = case t of
                         Tache -> (replicate n ' ')++"ⓧ"
                         Bolita -> (replicate n ' ')++"ⓥ"
                         R1 fs t' -> (replicate n ' ')++show fs++"\n"++pinta t' n
                         R2 t1 t2 -> pinta t1 (n+3)++"\n"++pinta t2 (n+3)

--Definición de un tableaux cerrado.
cerrado::Tableaux->Bool
cerrado t = case t of
             Tache -> True
             Bolita -> False
             R1 _ t' -> cerrado t'
             R2 t1 t2 -> cerrado t1 && cerrado t2  

--Nos dice si en una rama hay literales complementarias.
hayLitComp::[F]->Bool
hayLitComp fs = case fs of
                 [] -> False
                 (Var p):gs -> elem (Neg p) gs || hayLitComp gs   
                 (Neg p):gs -> elem (Var p) gs || hayLitComp gs
                 _:gs -> hayLitComp gs

--Nos dice si en una rama hay una conjunción, de ser así, la devuelve.
hayConj::[F]->Maybe F
hayConj fs = case fs of
              [] -> Nothing
              f@(Conj _ _):_ -> Just f
              _:gs -> hayConj gs 

--Nos dice si en una rama hay una disyunción, de ser así, la devuelve.
hayDisy::[F]->Maybe F
hayDisy fs = case fs of
              [] -> Nothing
              f@(Disy _ _):_ -> Just f
              _:gs -> hayDisy gs

--Dada una lista de fórmulas proposicionales, se crea un tableaux.
creaTableaux::[F]->Tableaux
creaTableaux fs = expande fs where
                  expande fs | hayLitComp fs = R1 fs Tache
                             | otherwise = case hayConj fs of
                                            Just f@(Conj f1 f2) -> R1 fs $ expande (f1:(f2:[f'|f'<-fs,f'/=f])) 
                                            Nothing -> case hayDisy fs of
                                                        Just f@(Disy f1 f2) -> R1 fs $ R2 (expande (f1:[f'|f'<-fs,f'/=f])) (expande (f2:[f'|f'<-fs,f'/=f]))
                                                        Nothing -> R1 fs Bolita   


ejem1 = [imp (Var "p") (Var "q"), Var "p"] ++ [Neg "q"]
ejem2 = [imp (Var "p") (Var "q"), Var "q"] ++ [Neg "p"]
ejem3 = [imp (Var "p") (Var "r"), imp (Var "q") (Var "r")] ++ [neg $ imp (Conj (Var "p") (Var "q")) (Var "r")]
ejem4 = [imp (Var "p") (Var "q"),imp (Var "p") (Var "r")] ++ [neg $ imp (Var "p") (Conj (Var "q") (Var "r"))]    
ejem5 = [imp (Var "p") (Var "q"),imp (Var "q") (Var "r")] ++ [neg $ imp (Var "p") (Var "r")]         
