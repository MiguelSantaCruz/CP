import Cp
import Nat
   
c' = prj.for loop inic where
    inic = (1,1,6,6)
    loop (c,f,s,l) = (f*c, 6/s + f,l+s,2+l)
    prj (c,f,s,l) = c
    
{-

l :: Integer -> Integer 
l 0 = 6
l (n+1) = 2 + l n

s :: Integer -> Integer  
s 0 = 6
s (n+1) = l n + s n

f :: Integer -> Double 
f 0 = 1
f (n+1) = 6/(s n) + f n

c :: Integer -> Double 
c 0 = 1
c (n+1) = f n * c n
-}
