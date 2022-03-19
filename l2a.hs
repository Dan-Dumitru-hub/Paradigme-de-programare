import Data.List
f1 s (x:xs) y= if y==3 then s else f1 x xs (y+1)

--f2 s [] y = if y > 0 then 0 else s
--f2 s (x:xs) y = if y > 3 then f2 x xs (y-1) 


f (x:y:z:w:l) = w
--f _ = 0


--f4 = filter (>0) [-1,-2,3,4,5,6,7,8]

f4 [] s=s
f4 (x:xs) s= if x > 0 then f4 xs (x:s) else f4 xs s

f4ne (x:xs) 
 | x>0 = x:(f4ne (tail xs))
 | otherwise = f4ne (tail xs)
 


f4a s = (head s) + f4a (tail s) 

f4b [] s=s
f4b (x:xs) s = f4b xs (s+x)

f6 (x:xs) s= if (head x)=="M" then f6 xs (x:s) else f6 xs s

--f7 :: String -> [(String ,[String])] -> [String]


--f7 gr [y:(a,[x:b])] = if gr == a then x:(f7 gr [y:(a,[b])] ) else f7 gr [(a,[x:b])] 

--f9 xs = (sort xs)

f8 xs  = if head(tail(tail(sort xs))) > 0 then True else False

 

