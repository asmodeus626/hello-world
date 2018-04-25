{-PROGRAMACIÓN CPS: pasar del tipo "a->b" al tipo "a->(b->r)->r"-}

--La función identidad cotidiana.
id1::a->a
id1 x = x


--La función identidad versión CPS
cpsid::a->(a->r)->r
cpsid x k = k x


--La multiplicación de los elementos en una lista.
mult::[Int]->Int
mult [] = 1
mult (n:ns) = n * mult ns


--La multiplicación de los elementos en una lista versión CPS.
cpsmult::[Int]->(Int->r)->r
cpsmult [] k = k 1
cpsmult (n:ns) k = let k1 = cpsmult ns in 
                   k1 $ \m -> k $ n*m    


data Exp = Num Int | Div Exp Exp


--Evaluador versión CPS.
evalcps::Exp->(Int->r)->r
evalcps (Num n) k = k n
evalcps (Div e1 e2) k = let k1 = evalcps e1
                            k2 = evalcps e2 in  
                        k1 $ \v1 -> k2 $ \v2 -> k $ div v1 v2    


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

--Función identidad con la mónada CPS.
cpsidM::a->Continuation r a
cpsidM x = return x


--Multiplicación de los elementos en una lista con la mónada CPS.
cpsmultM::[Int]->Continuation r Int
cpsmultM [] = return 1
cpsmultM (n:ns) = do 
                   m <- cpsmultM ns;
                   return $ n*m


--Evaluador con la mónada CPS.
evalContM::Exp->Continuation r Int
evalContM (Num n) = return n
evalContM (Div e1 e2) = do
                         v1 <- evalContM e1;
                         v2 <- evalContM e2;
                         return $ div v1 v2


--Clase para implementar la función 'callCC'.
class (Monad m) => MonadCont m where 
    callCC :: ((a -> m b) -> m a) -> m a
    
    
instance MonadCont (Continuation r) where 
    callCC f = Cont $ \k -> runCont (f (\a -> Cont $ \_ -> k a)) k    


--Para cambiar el flujo de un programa
when::Monad m=>Bool->m ()->m ()
when b v = if b then v else return ()


--Para multiplicar de forma eficiente. 
cpsprod0::[Int]->Continuation r Int
cpsprod0 [] = return 1
cpsprod0 (n:ns) = callCC $ \k -> do
                                  when (n==0) $ k 0;
                                  m <- cpsprod0 ns; 
                                  return $ n*m


type Handler r a = Int->Continuation r a 


--Evaluador con la mónada CPS para manejar el caso cuando el denominador es 0.
evalCont::Exp->Handler r Int->Continuation r Int
evalCont (Num n) _ = return n
evalCont (Div e1 e2) handler = callCC $ \ok -> do
                                                v2 <- evalCont e2 handler
                                                err <- callCC $ \notOk -> do
                                                                           when (v2==0) $ notOk $ -1
                                                                           v1 <- evalCont e1 handler
                                                                           ok $ div v1 v2
                                                handler err
                           
                           
                           
                           



