data Exp = Num Int | Div Exp Exp

eval::Exp->Int
eval (Num n) = n
eval (Div e1 e2) = div (eval e1) (eval e2)


eval1::Exp->(Int->(Int,Int))
eval1 (Num n) = \s -> (n,s)
eval1 (Div e1 e2) = 
         \s -> let (v1,s1) = eval1 e1 s in
               let (v2,s2) = eval1 e2 s1 in
               (div v1 v2,s2+1)    


type Stack a = [a]


pop::Stack a->(a,Stack a)
pop (x:xs) = (x,xs)

push::a->Stack a->((),Stack a)
push x xs = ((),x:xs)


king::Stack Int->((),Stack Int)
king ns = let (n1,ns1) = pop ns in
          let (n2,ns2) = pop ns1 in 
          let (n3,ns3) = pop ns2 in
          if n1+n2+n3==19 then 
            let ((),m1) = push n1 ns3 in
            let ((),m2) = push n3 m1 in
            ((),m2)
          else let ((),m1) = push 7 ns3 in
               let ((),m2) = push 2 m1 in
               let ((),m3) = push 10 m2 in
               ((),m3)  
            
newtype State s a = ST (s->(a,s))

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
   
tick::State Int ()
tick = ST (\n -> ((),n+1))  
   
evalState::Exp->State Int Int
evalState (Num n) = return n
evalState (Div e1 e2) = do 
                         v1 <- evalState e1;
                         v2 <- evalState e2;
                         tick;
                         return $ div v1 v2

ejemplo = runState (evalState $ Div (Num 20) (Div (Num 16) (Num 4))) 0

   
   
popState::State (Stack a) a
popState = ST (\(x:xs) -> (x,xs))

pushState::a->State (Stack a) ()
pushState x = ST (\xs -> ((),x:xs))
   
   
kingState::State (Stack Int) ()
kingState = do
             n1 <- popState;
             n2 <- popState;
             n3 <- popState;
             if (n1+n2+n3==19) then
               do 
                pushState n1;
                pushState n3
             else do 
                   pushState 7;
                   pushState 2;
                   pushState 10  

ejemplo2 = runState kingState [1,2,3,4,5,6]
   
ejemplo3 = runState kingState [1,2,16,4,5,6]
   
