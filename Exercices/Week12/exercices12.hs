--89

Zero + y = y                        (+.1)
Succ x + y = Succ ( x + y )         (+.2)  

Objetivo: x + (y + z) = (x + y) + z


-> Caso Base: x = Zero 

    -- Chegar da esq à direita ou direita à esq --
    Zero + (y + z) = (Zero + y) + z

    Zero + (y + z)      (+.1)
<=> (y + z)             (+.1) 
<=> (Zero + y) + z   

[!] Caso base está provado


-> Caso de Indução: -- Supondo que isto se mantém entre 0 e n, isto é verdade para n então é para n+1 --

        x + (y + z) = (x + y) + z  =>  (Succ x) + (y + z) = ((Succ x) + y) + z


-> Hipotese de Indução: x + (y + z) = (x + y) + z

<=>    (Succ x) + (y + z)        (+.2)
<=>    Succ ( x + (y+x) )        (H.I) -- Dentro do parentises --  
<=>    Succ ( (x+y) + z )        (+.2)
<=>    (Succ (x + y) ) + z       (+.2)
<=>    ((Succ x) + y ) + z

---------------------------------------------------------------------------------------------------

--90

[]      ++ ys = ys              (++.1)
(x:xs)  ++ ys = x:(xs++ys)      (++.2)


Objetivo: (xs ++ ys) ++ zs = xs ++ (ys ++ zs)


-> Caso Base: xs = []

    ([] ++ ys) ++ zs = [] ++ (ys ++ zs)


       ([] ++ ys) ++ zs       (++.1)
<=>    ys ++ zs               (++.1)
<=>    [] ++ (ys ++ zs)

[!] Caso base está provado


-> Caso de Indução: -- Supondo que isto se mantém entre 0 e n, isto é verdade para n então é pata n+1 --

        (xs ++ ys) ++ zs = xs ++ (ys ++ zs) => ((x:xs) ++ ys) ++ zs = (x:xs) ++ (ys ++ zs)


-> Hipotese de Indução: (xs ++ ys) ++ zs = xs ++ (ys ++ zs)


        ((x:xs) ++ ys) ++ zs      (++.2)
<=>     (x:(xs ++ ys)) ++ zs      (++.2)
<=>     x : ((xs ++ ys) ++ zs)    (H.I)
<=>     x : (xs ++ (ys ++ zs))    (++.2)
<=>     (x:xs) ++ (ys ++ zs)

---------------------------------------------------------------------------------------------------

--91

-- Lemma (++.3) -- 

Objetivo: ys ++ [] = ys 


-> Caso Base: ys = []

      [] ++ [] = []

      [] ++ []    (++.1)
<=>   []

[!] Caso base está provado


-> Caso de Indução : -- Supondo que isto se mantém entre 0 e n, isto é verdade para n então é pata n+1 --

        ys ++ [] = ys => y:ys ++ [] = y:ys


-> Hipotese de Indução: ys ++ [] = ys

<=>     (y:ys) ++ []        (++.2)   
<=>     y : (ys ++ [])      (H.I)
<=>     y:ys

-------------------

[]      ++ ys = ys              (++.1)
(x:xs)  ++ ys = x:(xs++ys)      (++.2)

reverse [] = []                     (r.1)
reverse (x:xs) = reverse xs ++ [x]  (r.2)  


Objetivo: reverse (xs ++ ys) = reverse ys ++ reverse xs


-> Caso Base: xs = []

       reverse ([] ++ ys) = reverse ys ++ reverse []

       reverse ([] ++ ys)          (++.1)
<=>    reverse ys                  (++.3)
<=>    reverse ys ++ []            (r.1)
<=>    reverse ys ++ reverse []

[!] Caso base está provado


-> Caso de Indução : -- Supondo que isto se mantém entre 0 e n, isto é verdade para n então é pata n+1 --

       reverse (xs ++ ys) = reverse ys ++ reverse xs => reverse ((x:xs) ++ ys) = reverse ys ++ reverse (x:xs)


-> Hipotese de Induçao: reverse (xs ++ ys) = reverse ys ++ reverse xs

               reverse (x:xs ++ ys)                 (++.2)
<=>            reverse (x:(xs ++ ys)                (r.2)
<=>            reverse (xs ++ ys) ++ [x]            (H.I)
<=>            (reverse ys ++ reverse xs) ++ [x]    (Associativedade de (++)) -- Provado em 90
<=>            reverse ys ++ (reverse xs ++ [x])    (r.2)
<=>            reverse ys ++ reverse (x:xs)


---------------------------------------------------------------------------------------------------


--92

map f [] = []                         (m.1)
map f (x:xs) = f x : (map f xs)       (m.2)
(f . g) x = f (g x)                   (.1)


Objetivo: map f (map g xs) = map (f . g) xs


-> Caso Base: xs = []

        map f (map g []) = map (f . g) []

       map f (map g [])        (m.1)
<=>    map f []                (m.1)
<=>    []                      (m.1)
<=>    map (f . g) []

[!] Caso base está provado


-> Caso de Indução : -- Supondo que isto se mantém entre 0 e n, isto é verdade para n então é pata n+1 --

        map f (map g xs) = map (f . g) xs => map f (map g (x:xs)) = map (f . g) (x:xs)


-> Hipotese de Induçao:  map f (map g xs) = map (f . g) xs
            
           map f (map g (x:xs))             (m.2)
<=>        map f (g x : (map g xs)          (m.2)
<=>        f (g x) : map f (g xs)           (.1)
<=>        (f . g) x : map f (g xs)         (H.I)
<=>        (f . g) x : map (f . g) xs       (m.2)
<=>        map (f . g) (x:xs)

---------------------------------------------------------------------------------------------------


--93 

take 0 xs             = []                      (t.1)
take n [] | n > 0     = []                      (t.2)
take n (x:xs) | n > 0 = x: take (n-1) xs        (t.3)

drop 0 xs             = xs                      (d.1)
drop n [] | n > 0     = []                      (d.2)
drop n (x:xs) | n > 0 = drop (n-1) xs        (d.3)


Objetivo: take n xs ++ drop n xs = xs


-> Caso Base: 
                                  
           take 0 xs ++ drop 0 xs        (t.1)                              
<=>        [] ++ drop 0 xs               (d.1)        
<=>        [] ++ xs                      (++.1)           
<=>        xs                                            
            
[!] Caso base está provado


-> Caso de Indução : -- Supondo que isto se mantém entre 0 e n, isto é verdade para n então é pata n+1 --

        take n xs ++ drop n xs = xs => take (n+1) xs ++ drop (n+1) xs = xs


-> Hipotese de Induçao: take n xs ++ drop n xs = xs

            -- 1. xs = [] 

            take (n+1) [] ++ drop (n+1) []          (t.2)
<=>         [] ++ drop (n+1) []                     (d.2)
<=>         [] ++ []                                (++.1)
<=>         []

            -- 2. xs = (y:ys)

            take (n+1) (y:ys) ++ drop (n+1) (y:ys)      (t.3)
<=>         y : take n ys ++ drop (n+1) (y:ys)          (d.3)
<=>         (y : take n ys) ++ drop n ys               (++.2)
<=>         y : (take n ys ++ drop n ys)                (H.I)
<=>         (y:ys)


---------------------------------------------------------------------------------------------------

--94

length [] = 0                       (l.1)
length (x:xs) = 1 + length xs       (l.2)


Objetivo: length (map f xs) = length xs


-> Caso Base: xs = []

        length (map f [])       (m.1)
<=>     length []

[!] Caso base está provado


-> Caso de Indução : -- Supondo que isto se mantém entre 0 e n, isto é verdade para n então é pata n+1 --

            length (map f xs) = length xs => length (map f (x:xs)) = length (x:xs)


-> Hipotese de Induçao:  length (map f xs) = length xs

            length (map f (x:xs))           (m.2)
<=>         length ((f x) : (map f xs))     (l.2)
<=>         1 + length (map f xs)           (H.I)
<=>         1 + length xs                   (l.2)
<=>         length (x:xs)

---------------------------------------------------------------------------------------------------

--95 

sum [] = 0                      (s.1)
sum (x:xs) = x + sum xs         (s.2)


Objetivo: sum (map (1+) xs) = length xs + (sum xs)

-> Caso Base: xs = []

         sum (map (1+) [])     (m.1)
         sum []                (s.1) 
         0                     (elementro neutro da addi)
         0 + 0                 (l.1)
         length [] + 0         (s.1)
         length [] + sum [] 


-> Caso de Induçao: 

                sum (map (1+) (x:xs)) => length (x:xs) + (sum (x:xs))

-> Hipotese: sum (map (1+) xs) = length xs + (sum xs)


                sum (map (1+) (x:xs))                   (m.2)
                sum ((1+) x): (map (1+) xs)             (s.2)
                (1+) x + sum (map (1+) xs)              H.I       
                1 + x + length xs + (sum xs)            (Associativedade de add)
                x + 1 + length xs + sum xs              (l.2)
                length (x:xs) + x + sum xs              (s.2)
                length (x:xs) + (sum (x:xs))

---------------------------------------------------------------------------------------------------

--96

Objetivo: map f (xs ++ ys) = map f xs ++ map f ys

-> Caso Base: 
                map f ([] ++ ys)        (++.1)
                map f ys                (++.1)
                [] ++ map f ys          (m.1)
                map f [] ++ map f ys    

-> Caso de Indução: map f ((x:xs) ++ ys) = map f (x:xs) ++ map f ys

   Hipotese: map f (xs ++ ys) = map f xs ++ map f ys

        map f ((x:xs) ++ ys)            (++.2)
        map f (x : (xs++ys))            (m.2)
        f x : (map f (xs++ys)           H.I      
        f x : (map f xs ++ map f ys)    (++.2)
        (f x : map f xs) ++ map f ys    (m.2)
        map f (x:xs) ++ map f ys

---------------------------------------------------------------------------------------------------

--97 

        Objetivo: map f (reverse xs) = reverse (map f xs)

-> Caso base: 

        map f (reverse [])             (r.1)
        map f []                       (m.1)
        []                             (r.1)
        reverse ([])                   (m.1) 
        reverse (map f [])


-> Caso de Indução: map f (reverse (x:xs)) = reverse (map f (x:xs))

        Hipotese: map f (reverse xs) = reverse (map f xs)

        map f (reverse (x:xs))                          (r.2)
        map f (reverse xs ++ [x])                       (96)  
        map f (reverse xs) ++ map f [x]                 (H.I)
        reverse (map f xs) ++ (map f [x])               (m.2)      
        reverse (map f xs) ++ (f x) : map f []          (m.1)
        reverse (map f xs) ++  (f x) : []           
        reverse (map f xs) ++  [(f x)]                  (r.2)
        reverse (f x : map f xs)                        (m.2)                                             
        reverse (map f (x:xs))
