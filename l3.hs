--map1 []=[]
--map1 (x:xs) = (head x):(map1 xs)

--map2 :: a -> a -> [[a]] -> a
--map2 _ _ [[]]=[]
--map2 i j  [x:xs] 
--  | i==0  = x !! j
--  | otherwise   = map2 (i-1) j [xs]


--f3 = zipWith(zipWith (+))

--f5 = map(map (*2)) (*2))

--lab04stud

type Matrix = [[Integer]]

--parsem1 :: String -> Matrix
parsem [] l=l
parsem (x:xs) l= if x/='\n' then parsem xs (x:l) else parsem xs l 

pars (l:xs) [y]=if l/=' ' then pars xs [l:y] else pars xs [y]



toString :: Matrix -> String
{-
toString (m:xs) = foldr (++) m xs
 -}
 toString (x:xs)= foldr (x++"/n"++) xs 

vprod x [m:xs] = (map (*x) m ) : vprod x [xs]

hjoin :: Matrix -> Matrix -> Matrix
hjoin (m1:x) (m2:y) = (m1 ++ m2):hjoin x y

tr :: [[a]] -> [[a]]

tr x = (map head x) : tr (map tail x)



--map(\li ( map(\cj -> foldr (*) . (zipwith (*) li cj)) (tr m2)) m1
--linia li col cj


type Image = [[Char]]
toStringImg :: Image -> String
toStringImg []=[]
toStringImg (x:xs) = x ++ toStringImg xs


--flipH :: Image -> Image

flipH (x:y) = (flipH y) ++ x


flipV = map reverse

rotate90r x =  (map head x) : (tr (map tail x))


--rotate90l x = rotate90r.rotate90r.rotate90r x



--new lab3

f58  gr ((x,y):xs) = if gr == x then fst y:(f58  gr xs) 

app :: [[Integer]] -> [Integer] 
cate (x:xs) = 1 + cate num xs
f60 (x:xs)=(cate x) : (f60 xs)


foldr1 f z []     = z
foldr1 f z (x:xs) = let z' = z `f` xs 
                    in foldl f z' x 
foldl1 f z []     = z
foldl1 f z (x:xs) = let z' = z `f` x 
                   in foldl f z' xs

f52 lista = \x -> x `elem` lista