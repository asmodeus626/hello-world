data Nat = Cero | S Nat deriving(Eq,Show)

suma :: Nat -> Nat -> Nat 
suma Cero n = n
suma (S m) n = S(suma m n)  

producto :: Nat -> Nat -> Nat
producto  Cero m =  Cero
producto  (S n) m = suma ( producto n m ) m


iguales :: Nat -> Nat -> Bool
iguales Cero Cero = True
iguales Cero _ = False
iguales _ Cero = False
iguales (S m) (S n) = iguales m n

h :: Int -> Int -> Int 
h _ 0 = 1
h n m  =  if (even m) then  h (n * n) (div m 2) else n * (h n (m-1)) 

pyu :: [a] -> (a,a)
pyu [] = error "La lista esta vacia"
pyu [x] = (x,x)
pyu (x:xs) = (x , ultimo xs)

ultimo [x] = x
ultimo (_:xs) = ultimo xs

clona :: [Int] -> [Int]
clona [] = []
clona [0] = []
clona (x:xs) = if (x < 0) then [] ++ (clona xs) else [x |_<-[1..x]] ++ (clona xs)

frec :: [Int] -> [(Int , Int)]
frec [] = []
frec (x:xs) =  [(x, frecuencia_de x (x:xs))] ++ frec (elimina_de xs x)

elimina_de :: [Int] -> Int -> [Int]
elimina_de [] _ = []
elimina_de (x:xs) y = if (x == y) then (elimina_de xs y) else x:(elimina_de xs y)

frecuencia_de :: Int -> [Int] -> Int
frecuencia_de _ [] = 0
frecuencia_de x (y:ys) = if (x == y) then 1+ (frecuencia_de x ys) else frecuencia_de x ys

agrupa :: [Int] -> [[Int]]
agrupa [] = []
agrupa (x:xs) = [takeWhile (== x) (x:xs)] ++ agrupa (dropWhile (== x) (x:xs))