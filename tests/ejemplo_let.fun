f :: (Int, Int) -> Int
f (x,y) = (let x :: Int = ((let y :: Int) = 3 in y))

main = f (1,1)