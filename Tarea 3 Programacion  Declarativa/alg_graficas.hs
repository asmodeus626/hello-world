
import Graphs_clase
import Data.List

--Funciones auxiliares-------------------------------------------------------------------------------------------------------

--Función que recibe una gráfica y devuelve la misma pero sin la palabra Graph
sacaGraf::Graph->[(V,[V])]
sacaGraf (Graph l) = l

--Función que recibe un vértice y lo elimina de la gráfica. Pero no lo elimina de las vecindades
eliminaVertice::Graph->V->Graph
eliminaVertice (Graph ((a,b):ls)) vertice = if vertice == a then Graph ls
                                            else Graph ((a,b):(sacaGraf (eliminaVertice (Graph ls) vertice)))

--Recibe un vértice y lo elimina de todas las vecindades.
eliminaVecino::Graph->V->Graph
eliminaVecino graf vertice = Graph (map (elimVec vertice) (sacaGraf graf)) where
                             elimVec v (a,b) = (a, delete v b)

--Borra un vértice de la gráfica y de las vecindades.
borraVertice::Graph->V->Graph
borraVertice graf v = eliminaVecino (eliminaVertice graf v) v

--Borra una lista de vértices de una gráfica.
borraVertices::Graph->[V]->Graph
borraVertices graf [] = graf
borraVertices graf (x:xs) = borraVertices (borraVertice graf x) xs

--Función que borra una arista de la gráfica
borraArista::Graph->(V,V)->Graph
borraArista (Graph []) _ = Graph []
borraArista (Graph ((a,b):xs)) (v1,v2) = if a == v1
                                         then Graph ((a,delete v2 b):xs)
                                         else Graph ((a,b):(sacaGraf (borraArista (Graph xs) (v1,v2))))

--Función que divide la gráfica en componentes conexas. Recibe una gráfica, una lista de todos los vértices y devuelve la partición.
compConex::Graph->[V]->[[V]]
compConex graf [] = []
compConex graf l = let listaRes = (dfs (head l) graf)
                   in [listaRes]++(compConex graf (l \\ listaRes))


--Recibe una gráfica, un vérice y decide si es un vértice de corte
esvertDeCorte::Graph->V->Bool
esvertDeCorte graf v = let grafRed = borraVertice graf v
                       in (length (compConex grafRed (vert grafRed))) > (length (compConex graf (vert graf)))


--Recibe una gráfica, una arista y decide si es una arista de corte
esarDeCorte::Graph->(V,V)->Bool
esarDeCorte graf ar = let grafRed = borraArista graf ar
                      in (length (compConex grafRed (vert grafRed))) > (length (compConex graf (vert graf)))


--Recibe una gráfica, un conjunto de vértices y decide si el conjunto es independiente.
esIndependiente::Graph->[V]->Bool
esIndependiente (Graph l) conj = let residuo = filter (\x-> elem (fst x) conj) l
                                 in all (\x->(intersect (snd x) conj)==[]) residuo

--Verifica si una gráfica G es completa
esCompleta::Graph->Bool
esCompleta (Graph l) = let vertices = vert (Graph l)
                       in all (\x->(sort (snd x))==(sort (delete (fst x) vertices))) l

--Función que recibe una gráfica, un conjunto de vértices y decide si dicho conjunto es un clan.
esClan::Graph->[V]->Bool
esClan g conj = esCompleta (borraVertices g ((vert g) \\ conj))

--Función que recibe una lista de aristas y verifica si un vértice v está en la lista.
estaEnLista::V->[(V,V)]->Bool
estaEnLista _ [] = False
estaEnLista v (x:xs) = if v==(fst x) || v==(snd x) then True else estaEnLista v xs

--Función que recibe una lista de aristas, un vértice y saca todas las aristas incidentes en el vértice.
incidentes::V->[(V,V)]->[(V,V)]
incidentes _ [] = []
incidentes v (x:xs) = if v==(fst x) || v==(snd x) then x:(incidentes v xs) else incidentes v xs

--Función que recibe un vértice, una lista de aristas y devuelve la primera arista que contenga a ese vértice.
primerArista::V->[(V,V)]->(V,V)
primerArista v l = if estaEnLista v l then primAr v l else (0,0) where
                   primAr ve (x:xs) = if ve==(fst x) || ve==(snd x) then x else primAr ve xs

--Función que recibe un vértice v, una arista y devuelve el vértice adyacente.
compl::V->(V,V)->V
compl v (a,b) = if a==v then b else a

{-Función que verifica, mediante una lista de aristas, si una gráfica tiene un ciclo.
Utiliza recursión de cola, la primera lista es la que recibe (de aristas), la segunda es donde va guardando
las aristas que ha visitado y la tercera es una pila de los vértices que va visitando-}
tieneCiclo_cola::[(V,V)]->[(V,V)]->[V]->Bool
tieneCiclo_cola [] _ _ = False
tieneCiclo_cola _ _ [] = False
tieneCiclo_cola input current ver = if (estaEnLista (head ver) input) then
                                    let temp = primerArista (head ver) input
                                        input2 = delete temp input
                                        v2 = compl (head ver) temp
                                    in  if estaEnLista v2 current then True
                                        else tieneCiclo_cola input2 (temp:current) (v2:ver)
                                    else tieneCiclo_cola input current (tail ver)
                                    

--Función que recibe una permutación de vértices de una gráfica y decide si vi es adyacente a vi+1.
sonAdyacentes::[(V,V)]->[V]->Bool
sonAdyacentes ar [] = True
sonAdyacentes ar ver = (elem (head ver,last ver) ar || elem (last ver,head ver) ar) && sonAd ar ver where
                       sonAd arist [] = True
                       sonAd arist [e] = True
                       sonAd arist (x:xs) = let v2 = head xs
                                            in if (elem (x,v2) arist) || (elem (v2,x) arist) then
                                                  sonAd arist xs
                                            else False

--Funciones de la tarea------------------------------------------------------------------------------------------------

--Vertices de corte
vertices_corte::Graph->[V]
vertices_corte graf = filter (esvertDeCorte graf) (vert graf)

--Aristas de corte
aristas_corte::Graph->[(V,V)]
aristas_corte graf = filter (esarDeCorte graf) (edges graf)

--Función que recibe una gráfica, obtiene un conjunto independiente maximal.
independienteMax::Graph->[V]
independienteMax graf = let potenciaVert = reverse $ sortOn length (subsequences $ vert graf) --conjunto potencia de los vértices de la gráfica
                        in  sacaInd graf potenciaVert where
                            sacaInd g [] = []
                            sacaInd g (x:xs) = if esIndependiente g x then x else sacaInd g xs

--Función que recibe una gráfica y devuelve un clan maximal
clanMax::Graph->[V]
clanMax graf = let potenciaVert = reverse $ sortOn length (subsequences $ vert graf) --conjunto potencia de los vértices de la gráfica
                        in  sacaClan graf potenciaVert where
                            sacaClan g [] = []
                            sacaClan g (x:xs) = if esClan g x then x else sacaClan g xs

--Función que recibe una gráfica y decide si tiene un ciclo.
tieneCiclo::Graph->Bool
tieneCiclo graf = tieneCiclo_cola (edges graf) [] [(head (vert graf))]


--Función que decide si una gráfica tiene un ciclo hamiltoniano.
hamilton::Graph->Bool
hamilton g = if not (tieneCiclo g) then False else
             ham (permutations (vert g)) (edges g) where
             ham [] _ = False
             ham (x:xs) ar = if sonAdyacentes ar x then True else
                             ham xs ar

