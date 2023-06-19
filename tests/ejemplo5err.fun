foo :: (Int,Int,Bool) -> Int
foo(x,y,z) = if (3==True) 
                then z
                else if z then x+y else x==y 

main = foo(2,4)

Expected: Int Actual: Bool -- el if es de tipo Bool y foo retorna Int
Expected: Bool Actual: Int -- el if interno es de tipo Int   
Expected: Int Actual: Bool -- True en 3==True
Expected: Int Actual: Bool -- x==y 
The number of arguments in the application of: foo doesn't match the signature (2 vs 3)
