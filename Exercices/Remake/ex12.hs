{- 

-- Propriedades

-0 
Todas as distribuiçoes e associaçoes da matemica estao presentes

-1
reverse [] = []                         (rev1)
reverse (x:xs) = reverse xs ++ [x]      (rev2)

-2
[] ++ ys = ys                           (++1)    
(x:xs) ++ ys = x:(xs++ys)               (++2)    

-3
Zero + m = m                            (+1)
(Succ n) + m = Succ (n+m)               (+2)    


-4
length [] = 0                           (l1)
length (x:xs) = 1 + length xs           (l2)


-5
replicate 0 x = []                      (rep1)
replicate n x = x : replicate (n-1) x   (rep2)

 -}


--89

Objetivo: x + (y+z) = (x+y) + z

Caso Base:
         
        Zero + (x+z)   (Assoc +)
        (Zero + y) + z  (+1)
        y + z           (+1)
        (Zero + y) + z

Caso de Inducao:

        x + (y+z) = (x+y) + z => (Succ x) + (y+z) = ((Succ x)+y) + z

        Hipotese de Inducao:  x + (y+z) = (x+y) + z

        (Succ x) + (y+z)   (+2) 
        Succ (x + (y+z))    H.I
        Succ ((x+y) + z)   (+2)
        (Succ (x+y)) + z   (+2)       
        ((Succ x) + y) + z

---------------------------------------------------------------------------------------------------


--90

Objetivo: (xs ++ ys) ++ zs = xs ++ (ys ++ zs) (Assoc ++)

Caso Base:
        ([] ++ ys) ++ zs    (++1)
        ys ++ zs            (++1)
        [] ++ (ys ++ zs)

Caso de Inducao:

        (xs ++ ys) ++ zs = xs ++ (ys ++ zs) => ((x:xs) ++ ys) ++ zs = (x:xs) ++ (ys ++ zs)

        Hipotese de Inducao: (xs ++ ys) ++ zs = xs ++ (ys ++ zs)

        ((x:xs) ++ ys) ++ zs    (++2)
        (x:(xs++ys)) ++ zs      (++2)
        x:((xs++ys) ++ zs)      H.I
        x:(xs ++ (ys ++ zs))    (++2)
        (x:xs) ++ (ys ++ zs)

---------------------------------------------------------------------------------------------------

--91

-- Lemma (++3) --

Objetivo: ys ++ [] = ys

        [] ++ [] (++1)
        []

Caso de Inducao: 
     
        ys ++ [] = ys => (y:ys) ++ [] = (y:ys)

        Hipotese de Inducao: ys ++ [] = ys

        (y:ys) ++ []   (++2)
        y:(ys ++ [])   H.I 
        (y:ys)

--------------------------------

Objetivo: reverse (xs++ys) = reverse ys ++ reverse xs

Caso Base: 
        reverse ([]++ys)        (++1)
        reverse (ys)            (++3)
        reverse ys ++ []        (rev1)
        reverse ys ++ reverse []


Caso de Inducao: 
        reverse (xs++ys) = reverse ys ++ reverse xs => reverse ((x:xs)++ys) = reverse ys ++ reverse (x:xs)

        Hipotese de Inducao: reverse (xs++ys) = reverse ys ++ reverse xs

        reverse ((x:xs)++ys)                (++2)
        reverse (x:(xs++ys))                (rev2)
        reverse (xs++ys) ++ [x]             H.I
        (reverse ys ++ reverse xs) ++ [x]   (Assoc ++)
        reverse ys ++ (reverse xs ++ [xs])  (rev2)
        reverse ys ++ reverse (x:xs)

---------------------------------------------------------------------------------------------------


--92

map f [] = []                       (m1)
map f (x:xs) = f x : map f xs       (m2)

(f . g) x = f (g x)                 (comp)


Objetivo: map f (map g xs) = map (f . g) xs

Caso Base: 
        map f (map g [])        (m1)
        map f []                (m1)
        []                      (m1)
        map (f . g) []          

Caso de Inducao: 
        map f (map g xs) = map (f . g) xs => map f (map g (x:xs)) = map (f . g) (x:xs)


Hipotese Inducao: map f (map g xs) = map (f . g) xs

        map f (map g (x:xs))            (m2)
        map f ((g x) : map g xs)        (m2)
        (f (g x)) : (map f (map g xs))  H.I
        (f (g x)) : map (f . g) xs     (comp)
        (f . g) x : map (f . g) xs     (m2) 
        map (f . g) (x:xs)

---------------------------------------------------------------------------------------------------


--93

take 0 xs = []                              (t1)
take n [] | n > 0 = []                      (t2)
take n (x:xs) | n > 0 =  x: take (n-1) xs   (t3)

drop 0 xs = xs                              (d1)
drop n []| n > 0 = []                       (d2)
drop n (x:xs) |n > 0 = drop (n−1) xs        (d3)    


Objetivo: take n xs ++ drop n xs = xs


Caso Base:  

        take 0 xs ++ drop 0 xs    (t1) 
        [] ++ drop 0 xs           (d1)
        [] ++ xs                  (++1) 
        xs

Caso Inducao: 

        take n xs ++ drop n xs = xs => take (n+1) xs ++ drop (n+1) xs = xs

Hipotese de Inducao: take n xs ++ drop n xs = xs

-- xs = (x:xs)
take (n+1) (x:xs) ++ drop (n+1) (x:xs)          (t3)
(x : (take n xs)) ++ drop (n+1) (x:xs)          (d3)
(x : (take n xs)) ++ drop n xs                  (++2) 
x : (take n xs ++ drop n xs)                    H.I
x:xs

-- xs = []
take (n+1) [] ++ drop (n+1) []                  (t2)
[] ++ drop (n+1) []                             (d2)
[] ++ []                                        [++1]
[]

---------------------------------------------------------------------------------------------------

--94

Objetivo: length (map f xs) = length xs

Caso base:

        length (map f [])       (m1)
        length []

Caso Inducao: 

        length (map f xs) = length xs => length (map f (x:xs)) = length (x:xs)

Hipotese: length (map f xs) = length xs

length (map f (x:xs))           (m2)
length ((f x) : (map f xs))     (l2)
1 + length (map f xs)           H.I
1 + length xs                   (l2)
length (x:xs)

---------------------------------------------------------------------------------------------------

--95

sum [] = 0               (s1)
sum (x:xs) = x + sum xs  (s2)


Objetivo: sum (map (1+) xs) = length xs + sum xs

Caso Base: 

        sum (map (1+) [])       (m1)
        sum []                  (s1)
        0                       (Elm neutro)   
        0 + 0                   (l1) (s1)
        length [] + sum []

Caso de Inducao: 

        sum (map (1+) xs) = length xs + sum xs => sum (map (1+) (x:xs)) = length (x:xs) + sum (x:xs)

Hipotese de Inducao: sum (map (1+) xs) = length xs + sum xs 

        sum (map (1+) (x:xs))               (m2)
        sum ((1+x) : (map (1+) xs))         (s2)
        (1+x) + sum (map (1+) xs)           H.I
        (1+x) + (length xs + sum xs)        (Rearr)
        1 + length xs + x + sum xs          (l2) (s2)
        length (x:xs) + sum (x:xs)

---------------------------------------------------------------------------------------------------

--96

Objetivo: map f (xs++ys) = map f xs ++ map f ys     (96)

Caso Base: 
        map f ([]++ys)              (++1)
        map f ys                    (++1)
        [] ++ map f ys              (m1)
        map f [] ++ map f ys

Caso de Inducao: 
        map f (xs++ys) = map f xs ++ map f ys => map f ((x:xs)++ys) = map f (x:xs) ++ map f ys

Hipotese de Inducao: map f (xs++ys) = map f xs ++ map f ys

        map f ((x:xs)++ys)              (++2)
        map f x:(xs++ys)                (m2)
        (f x) : map f (xs++ys)          H.I
        (f x) : (map f xs ++ map f ys)  (++2)
        (f x : map f xs) ++ map f ys    (m2)
        map f (x:xs) ++ map f ys

---------------------------------------------------------------------------------------------------

--97

Objetivo: map f (reverse xs) = reverse (map f xs)

Caso base:
        map f (reverse [])      (rev1)
        map f []                (m1)
        []                      (rev1)
        reverse []              (m1)      
        reverse (map f [])

Caso Inducao: 
        map f (reverse xs) = reverse (map f xs) => map f (reverse (x:xs)) = reverse (map f (x:xs))

Hipotese de Inducao: map f (reverse xs) = reverse (map f xs)

        map f (reverse (x:xs))                  (rev1)
        map f (reverse xs ++ [x])               (96)
        map f (reverse xs) ++ map f [x]         H.I
        reverse (map f xs) ++ map f [x]         (m2)
        reverse (map f xs) ++ (f x) : map f []  (m1)
        reverse (map f xs) ++ (f x) : []        (:1)
        reverse (map f xs) ++ [f x]             (rev2)
        reverse ((f x) : map f xs)              (m2)
        reverse (map f (x:xs))

---------------------------------------------------------------------------------------------------

--98

inserir x [] = [x]                                      (i1)
inserir x (y:ys) | x <= y = x:y:ys                      (i2)
inserir x (y:ys) | x > y  = y : inserir x ys            (i3)   

Objetivo: length (insert x xs) = 1 + length xs

Caso base: 
        length (insert x [])        (i1)
        length [x]                  (l2)               
        1 + length []

Caso de Inducao:
        length (insert x xs) = 1 + length xs => length (insert x (x:xs)) = 1 + length (x:xs)

Hipotese de Inducao: length (insert x xs) = 1 + length xs

    -- z <= x
        length (insert z (x:xs))        (i2)
        length (z:x:xs)                 (l2)            
        1 + length (x:xs)

    -- z > x
        length (insert z (x:xs))       (i3) 
        length (x : insert z xs)       (l2) 
        1 + length (insert z xs)       H.I
        1 + 1 + length xs              (l2) 
        1 + length (x:xs)

---------------------------------------------------------------------------------------------------

--99

data Arv a = Folha | No a (Arv a) (Arv a)

intermedios :: Arv a -> Int
intermedios Folha = 0                                                                   (int1)
intermedios (No _ left right) = 1 + intermedios left + intermedios right                (int2)

folhas :: Arv a -> Int
folhas Folha = 1                                                                        (fol1)
folhas (No _ left right) = folhas left + folhas right                                   (fol2)


Objetivo: intermedios t < folhas t

Caso Base:
        intermedios Folha < folhas Folha        (int1) (fol1)
        0 < 1

Caso de Inducao: intermedios t < folhas t => intermedios (No x left right) < folhas (No x left right)

Hipotese de Inducao: intermedios t < folhas t

        intermedios (No x left right) < folhas (No x left right)                        (int2) (fol2)
        1 + intermedios left + intermedios right  <  folhas left + folhas right         H.I
        1 + 2*intermedios t < 2*folhas t
        

        
---------------------------------------------------------------------------------------------------

--100 (duvida)

---------------------------------------------------------------------------------------------------

--101

data Arv a = Vazia | No a (Arv a) (Arv a)

--a)
soma :: Arv a -> a
soma Vazia = 0                                                          (sA1)
soma (No a left right) = a + (soma left) + (soma right)                 (sA2)

--b)
valorArv :: Arv a -> a
valorArv Vazia = 0                                                      (vA1)
valorArv (No x _ _) = x                                                 (vA2)

--c)
somaTree :: Arv a -> Arv a
somaTree Vazia = Vazia                                                                                          (sT1)
somaTree (No x Vazia Vazia) = No x Vazia Vazia                                                                  (sT2)
somaTree (No x (No y Vazia Vazia) (No z Vazia Vazia)) = No (x+y+z) (No y Vazia Vazia) (No z Vazia Vazia)
somaTree (No x left right) =  



--d)

--- Duvida ---


Objetivo: soma t = valorArv (somaTree t)

Caso Base:
        soma Vazia                              (sA1)
        0                                       (vA1)                              
        valorArv 0                              (sT1)
        valorArv (somaTree Vazia)

Caso de Inducao: soma t = valorArv (somaTree t) => soma (No x t1 t2) = valorArv (somaTree (No x t1 t2))

Hipotese de Inducao: soma t = valorArv (somaTree t)

        -- (No x t1 t2) = (No x Vazia Vazia) 
        soma (No x Vazia Vazia)                                                               (sA2)
        x + (soma Vazia) + (soma Vazia)                                                       H.I 
        x + (valorArv (somaTree Vazia)) + valorArv (somaTree Vazia)                           (sT1)  
        x + (valorArv Vazia) + (valorArv Vazia)                                               (vA1)
        x + 0 + 0                                                                             (Elem neutro)
        x                                                                                     (vA2)   
        valorArv (No x Vazia Vazia)                                                           (sT2)  
        valorArv (somaTree (No x Vazia Vazia))


---------------------------------------------------------------------------------------------------

--102

data Arv a = No a (Arv a) (Arv a) | Vazia

simetrica :: Arv a -> Arv a
simetrica Vazia = Vazia                                                                 (sim1)
simetrica (No x left right) = (No x (simetrica right) (simetrica left))                 (sim2)

listar Vazia = []                                                                       (list1)
listar (No x left right) = listar left ++ [x] ++ listar right                           (list2)

reverse [] = []                                                                         (rev1)
reverse (x:xs) = reverse xs ++ [x]                                                      (rev2)

Objetivo: listar t = reverse (listar (simetrica t))

Caso Base: 
        listar Vazia                                    (list1)
        []                                              (rev1)
        reverse ([])                                    (sim1)
        reverse (listar [])                             (sim1)
        reverse (listar (simetrica Vazia))

Caso de Inducao: 
        listar t = reverse (listar (simetrica t)) => listar (No x t1 t2) = reverse (listar (simetrica (No x t1 t2)))

Hipotese de Inducao: listar t = reverse (listar (simetrica t))


        listar (No x t1 t2)                                                                     (list2)
        listar t1 ++ [x] ++ listar t2                                                           H.I
        reverse (listar (simetrica t1)) ++ [x] ++ reverse (listar (simetrica t2))               (Identidade reverse sobre [x])
        reverse (listar (simetrica t1)) ++ reverse [x] ++ reverse (listar (simetrica t2))       (Distributividade do reverse sobre ++) -- Provado 91
        reverse (listar (simetrica t1) ++ [x] ++ listar (simetrica t2))                         (list2)
        reverse (listar (No x (simetrica right) (simetrica left)))                              (sim2)
        reverse (listar (simetrica (No x t1 t2)))

---------------------------------------------------------------------------------------------------

--103

--a)

foldTree :: (a -> b -> b -> b) -> b -> Arv a -> b
foldTree f n Vazia = n                                                          (fold1)
foldTree f n (No x t1 t2) = f x (foldTree f n t1) (foldTree f n t2)             (fold2)

soma3 :: a -> b -> b -> b
soma3 x y z = x + y + z         (s3)

soma :: Arv a -> a
soma Vazia = 0                                                          (sA1)
soma (No a left right) = a + (soma left) + (soma right)                 (sA2)

--b)
Objetivo: soma t = foldtree soma3 0 t

Caso Base: 
        soma Vazia                              (sA1)                  
        0
        0
        foldtree soma3 0 Vazia                  (fold1)


Caso de Inducao: 
        soma t = foldtree soma3 0 t => soma (No x t1 t2) = foldtree soma3 0 (No x t1 t2)

Hipotese de Inducao:  soma t = foldtree soma3 0 t


        soma (No x t1 t2)                                               (sA2)
        x + soma t1 + soma t2                                           H.I
        x + foldtree soma3 0 t1 + foldtree soma3 0 t2                   
        x + foldtree soma3 0 t1 + foldtree soma3 0 t2                   (s3)
        soma3 x (foldtree soma3 0 t1) (foldtree soma3 0 t2)             (fold2)
        foldtree soma3 0 (No x t1 t2)

---------------------------------------------------------------------------------------------------            