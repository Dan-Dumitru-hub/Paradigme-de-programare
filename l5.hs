

--import Data.Tree

data IntList  = Empty | Cons Integer IntList 
isum :: IntList -> Integer
isum Empty =  0
isum ( Cons x Empty) = x
isum (Cons x xs) = x + isum xs

y=4 `Cons` (5 `Cons` Empty)  --intlist


data List a = Empty1 | Cons1 a (List a)

to_poly_list :: IntList -> List Integer
to_poly_list Empty = Empty1
to_poly_list ( Cons x Empty) = x `Cons1` Empty1
to_poly_list ( Cons x xs) = x `Cons1` (to_poly_list xs)



z=4 `Cons1` (5 `Cons1` Empty1)  --listInteger

--show_list :: List Integer -> String

show_list Empty1 =[]
show_list (Cons1 x xs) =(show  x) ++ show_list xs



data Tree a = Void | Node (Tree a) a (Tree a) deriving Show

t= Node(Node ( Node Void 1 Void ) 2 (Node Void 4 Void)) 3 (Node Void 0 Void)
--flatten :: Tree a -> List a
flatten (Void ) = []
flatten ( Node a b c)= (flatten a) ++( show b) ++ (flatten c)



app :: (List a) -> (List a) -> (List a) 
app   x Empty1   = x  
app  z (x `Cons1` y)   = app  (x `Cons1` z) y


tmap :: (a -> b) -> (Tree a) -> (Tree b)
tmap f Void = Void

tmap f (Node a b c) = Node  (tmap f a) (f b) (tmap f c) 




tzipWith :: (a -> b -> c) -> (Tree a) -> (Tree b) -> (Tree c)
tzipWith f Void Void = Void
tzipWith f Void (Node a b c) = Void
tzipWith f (Node a b c) (Node d e f1) = Node (tzipWith f a d) (f b e) (tzipWith f c f1)

tfoldr :: (a -> b -> b) -> b -> Tree a -> b
tfoldr f acc Void = acc
tfoldr f acc (Node a b c) =  tfoldr f (tfoldr f (f b acc)  a ) c

tflatten = tfoldr (\x -> case x of (Node a b c) ->  b ) [] 





data Extended = Infinity | Value Integer

extsum1 x = case x of
     Value x -> x

extSum :: Extended -> Extended -> Extended
extSum x y = case (x,y) of
		(Infinity,_) -> Infinity
		(_,Infinity) -> Infinity
		(Value a ,Value b) -> Value (a+b)



equal :: Extended -> Extended -> Bool
equal x y =  case (x,y) of
		(Infinity,Infinity) -> True
		
		(Value a ,Value b) -> a==b

data Maybe1 a = Nothing1 | Just1 a
lhead :: List a -> Maybe1 a

lhead Empty1  = Nothing1
lhead (a `Cons1` b) = Just1 (a)


ltail :: List a -> Maybe1 a

ltail (a `Cons1` Empty1) = Just1 a
ltail (a `Cons1` b) = ltail b
	