
module Graphs_clase where

type V = Int
data Graph = Graph [(V,[V])] deriving Show


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

gcompleta = Graph [(1,[2,3,4,5]),
                    (2,[1,3,4,5]),
                    (3,[1,2,4,5]),
                    (4,[1,2,3,5]),
                    (5,[1,2,3,4])]

gciclo = Graph [(1,[2,5]),
                    (2,[3,1]),
                    (3,[4,2]),
                    (4,[5,3]),
                    (5,[1,4])]

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



