type Image = [String] 
type Image1 = [[Char]]
toStringImg :: Image1 -> String
toStringImg []=[]
toStringImg (x:xs) = x ++"\n"++ toStringImg xs
displaym = putStrLn . toStringImg
l1="        ***** **            ***** **    "
l2="     ******  ****        ******  ****   "
l3="    **   *  *  ***      **   *  *  ***  "
l4="   *    *  *    ***    *    *  *    *** "
l5="       *  *      **        *  *      ** "
l6="      ** **      **       ** **      ** "
l7="      ** **      **       ** **      ** "
l8="    **** **      *      **** **      *  "
l9="   * *** **     *      * *** **     *   "
l10="      ** *******          ** *******    "
l11="      ** ******           ** ******     "
l12="      ** **               ** **         "
l13="      ** **               ** **         "
l14="      ** **               ** **         "
l15=" **   ** **          **   ** **         "
l16="***   *  *          ***   *  *          "
l17=" ***    *            ***    *           "
l18="  ******              ******            "
l19="    ***                 ***             "
 
logo = [l1,l2,l3,l4,l5,l6,l7,l8,l9,l10,l11,l12,l13,l14,l15,l16,l17,l18,l19]

tr :: [[a]] -> [[a]]

tr [[]] = [[]]
tr x = (map head x) : (tr (map tail x))



flipH = map (reverse) 

flipV = reverse 

{-}
poz [] = 0
poz (x:xs) = 1 + (poz xs)
poz1 (x:xs) = if (poz x) < 3 then x=0 

f17 = map (poz1)
-}


--funct (x:xs) i = if i<3 then 0:(funct xs (i-1)) else x:(funct xs i-1)
--f17 (x:xs) = map (funct x 3 ) (x:xs)
for1 1="*"
for1 x= "*" ++ for1 (x-1) ++ "*"
space1 0=[]
space1 x = " " ++ space1 (x-1)

diamond 0=[]
diamond x= for1 x ++ "\n" ++ (space1 (x-1)) ++ diamond (x-1)

diamonda x =  flipV(diamond x) ++"\n" ++ (diamond (x-1))
displayd = putStrLn . diamonda


{-}
overlay = zipWith (\ x y -> if x==y then "*" else " ") 
f20 = map overlay 
-}
vprod m1 m2 = 
  map (\line -> map (\col -> foldr (+) 0 (zipWith (*) line col) ) (tr m2) ) m1
char1 "*" "*" = "*"
char1 _ _ = " "

overlay [] [] =[]
overlay (x:xs) (y:ys) = (zipWith char1 x y) : ( overlay xs ys)

poz [] i j = []
poz  (x:xs) i j = if i<=j then x:(poz xs (i+1) j) else 0:(poz xs (i+1) j)

--poz1 [] = 0
--poz1 (x:xs) = 1 + poz1 xs
--f17 [] = []
size (x:xs)= 1+ size xs
f17 [] j = []
f17 (x:y) j  = (poz x 1 j ) : (f17 y (j-1))

poz1 [] i j = []
poz1  (x:xs) i j = if i>=j then x:(poz1 xs (i+1) j) else 0:(poz1 xs (i+1) j)
f18 [] j = []
f18 (x:y) j  = (poz1 x 1 j ) : (f18 y (j+1))

zero1 0 = [] 
zero1 x = 0:(zero1 (x-1)) 

f19 [] i = []
f19 (x:y) i =( (take i x )++ zero1 ( 3 - i) ) : (f19 y (i+1))

 --f6 (m:m1) (z:zs) = (m:m1:z):(f6 m zs)

-- f6 m1 = \m -> m ++ m1

poz2 [] i j = []
poz2  (x:xs) i j = if i==j then x:(poz1 xs (i+1) j) else 0:(poz2 xs (i+1) j)

f20 [] j = []
f20 (x:y) j  = (poz2 x 1 j ) : (f20 y (j+1))