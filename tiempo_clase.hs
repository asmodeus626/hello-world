import Data.Time



insertionSort::Ord a=>[a]->[a]
insertionSort xs = case xs of
                    [] -> []
                    x:xs -> inserta x $ insertionSort xs where
                            inserta x [] = [x]
                            inserta x (y:ys) = if x<y then (x:(y:ys)) 
                                                      else (y:(inserta x ys))
                                                      
                                                      
quickS::Ord a=>[a]->[a]
quickS [] = []
quickS (x:xs) = quickS [y|y<-xs, y<= x] ++ x:(quickS [y|y<-xs, y>x]) 



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
                                                  

main = do 
        putStrLn "\tCOMPARANDO ORDENAMIENTOS";
        putStrLn "Teclea el tamaño de la lista a ordenar:"
        n <- readLn;
        let ns = [n,n-1..1];
        start1 <- getCurrentTime; 
        m1 <- return $ mergeSort ns;
        putStrLn $ "Merge sort: " ++ let [k1,k2,k3] = take 3 m1 in "["++show k1++","++show k2++","++show k3++"..."++(show $ last m1)++"]"
        end1 <- getCurrentTime;
        putStrLn (show $ diffUTCTime end1 start1);
        putStrLn ""        
        start2 <- getCurrentTime; 
        m2 <- return $ insertionSort ns;
        putStrLn $ "Insertion sort: " ++ let [n1,n2,n3] = take 3 m2 in "["++show n1++","++show n2++","++show n3++"..."++(show $ last m2)++"]"
        end2 <- getCurrentTime;
        putStrLn (show $ diffUTCTime end2 start2);
        putStrLn ""
        start3 <- getCurrentTime; 
        m3 <- return $ quickS [1..n];
        putStrLn $ "Quick sort: " ++ let [r1,r2,r3] = take 3 m3 in "["++show r1++","++show r2++","++show r3++"..."++(show $ last m3)++"]"
        end3 <- getCurrentTime;
        putStrLn (show $ diffUTCTime end3 start3);
        
        
        
        
        
        
        
         



