2. Aufgabe 
(mod (n^3 + (n+1)^3 + (n+2)^3) 9) == 0    für alle   n>=0

I.A. n=0
(mod (0^3 + (1)^3 + (2)^3) 9) =? 0
(mod (0 + 1 + 8) 9) = (mod 9 9) = 0

I.V. n=n'
(mod (n'^3 + (n'+1)^3 + (n'+2)^3) 9) == 0

I.S. n=n'+1
(mod ((n'+1)^3 + ((n'+1)+1)^3 + ((n'+1)+2)^3) 9) == 0
(mod ((n'+1)^3 + (n'+2)^3 + ((n'+3)*(n'(n'+3)+3(n'+3)))) 9)
n'*n'*n'
3*3*3

(n'+3) * (n'+3)

--

3. Aufgabe

length (replicate n x) == n

I.A. n=0
     length(replicate 0 x) =? 0

I.V. für n=n' gilt length(replicate n' x) = n'

I.S. n=n'+1
     length (replicate (n'+1) x) =? (n'+1)
    
     length(replicate (n'+1) x) =? length(x:replicate n' x) -- replicate.2
                                =? 1+length(replicate n' x) -- length.2
                                =  1+n' -- I.V.
                                =  n'+1
--


4. Aufgabe

map f xs ++ map f ys == map f (xs++ys)


map f  []    = [] -- map.1
map f (x:xs) = (f x) : map f xs -- map.2

(++) [] ys     = ys -- (++).1
(++) (x:xs) ys = x:(xs++ys) -- (++).2

I.A. xs=[]
[] ++ map f ys == map f ([]++ys) -- map.1 und (++).1
      map f ys == map f ys

I.V. für xs = xs'

I.S. xs = x:xs'
  map f (x:xs') ++ map f ys     =? map f ((x:xs') ++ ys)
  map f (x:xs') ++ map f ys     =? map f (x:((xs') ++ ys)) -- (++).2
(f x) : map f xs' ++ map f ys   =? (f x) : map f (xs' ++ ys) -- map.2
                                 = (f x) : map f xs' ++ map f ys -- I.V.
  
  
  
  
  
  
  
  
  
(f x):map f xs' ++ map f ys == (f x) :  map f (xs' ++ ys) -- map.2
