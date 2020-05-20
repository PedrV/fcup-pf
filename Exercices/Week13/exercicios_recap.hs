--82

data Graph = Graph ( [Int] , [(Int, Int)] )

grafo :: Graph
grafo = Graph ([1..6], [(1,2), (1,4), (2,3), (2,5), (2,6), (3,6), (4,5), (6,5)])

destinos :: Graph -> Int -> [Int]
destinos (Graph (v, a)) o = [snd x | x<- a, fst x == o]

caminhos :: Graph -> Int -> Int -> Int
caminhos (Graph (v,a)) o d | (o == d)   = 1
                           | otherwise  = sum [ caminhos (Graph (v,a)) x d | x <- dest  ]
                           where dest = destinos (Graph (v,a)) o


-- verificar grafo bem construido
-- G (V, A) todos os vertices estao em A

-- Num grafo com ciclos


-------------------
-- Ordem superior
-- Folds