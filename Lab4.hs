{-|
 - Paradigme de Programare CB
 - Laborator 4
 -}
module Lab4 where

import Control.Exception (assert)
import Data.List (sort)

-- 1. Binary Trees

-- Puteți ignora partea cu "deriving Eq". Pe scurt, ne permite să comparăm doi
-- arbori cu operatorul "==" (util pentru asserturi).
data BTree a = Nil | Node a (BTree a) (BTree a) deriving Eq

-- 1.a
foldrT :: (a -> b -> b) -> b -> BTree a -> b
foldrT = undefined


-- 1.b
mapT :: (a -> b) -> BTree a -> BTree b
mapT = undefined

-- 1.c
zipWithT :: (a -> b -> c) -> BTree a -> BTree b -> BTree c
zipWithT = undefined


-- 2. Associative Lists
type Assoc k v = [(k, v)]

-- 2.a
insert :: (Eq k) => k -> v -> Assoc k v -> Assoc k v
insert = undefined

-- 2.b
getValue :: (Eq k) => k -> Assoc k v -> Maybe v
getValue = undefined

-- 2.c
removeKey :: (Eq k) => k -> Assoc k v -> Assoc k v
removeKey = undefined
-- 2.d
getKeys :: Assoc k v -> [k]
getKeys = undefined
-- 3. Extended natural numbers

data Extended = Infinity | Value Integer deriving Eq

infin = 100000000000000000000000000000000000000000
-- 3.a

tip :: Extended -> Integer
tip x = case x of
    Value x -> x
    Infinity -> infin

extSum :: Extended -> Extended -> Extended
extSum Infinity _ = Infinity
extSum x (Value 0) = x
extSum x y = extSum ((Value x) + (Value y)) 0

extSum1 :: Extended -> Extended -> Integer
extSum1 x y = tip x + tip y


-- 3.b
extDiv :: Extended -> Extended -> Extended
extDiv = undefined


extDiv1 :: Extended -> Extended -> Integer
extDiv1 x y = if (tip y)==0 then infin else
                                        if (tip x) == infin then infin 
                                            else (tip x) `div` (tip y)

-- 3.c
extLess :: Extended -> Extended -> Bool
extLess = undefined



-- test trees
singleNode = Node 1349 Nil Nil
dSingleNode = Node 2698 Nil Nil
simpleTree = Node 2 (Node 3 Nil Nil) (Node 2 Nil Nil)
sqSimpleTree = Node 4 (Node 9 Nil Nil) (Node 4 Nil Nil)
stringTree = Node "root" (Node "left" Nil Nil) (Node "right" Nil Nil)

-- to allow free order in the associative array implementation
(=?=) :: (Ord a) => [a] -> [a] -> Bool
l =?= l' = sort l == sort l'
infix 4 =?=

test1a = [
    assert (foldrT (+) 20 Nil == 20) "Success for foldrT (+) 20 Nil",
    assert (foldrT (+) 0 singleNode == 1349) "Success for foldrT (+) 0 singleNode",
    assert (foldrT (+) 20 singleNode == 1369) "Success for foldrT (+) 20 singleNode",
    assert (foldrT (+) 0 simpleTree == 7) "Success for foldrT (+) 0 simpleTree",
    assert (foldrT (^) 1 simpleTree == 512) "Success for foldrT (^) 1 simpleTree",
    assert (foldrT (++) "" stringTree == "rootleftright") "Success for foldrT (++) \"\" stringTree"
    ]

test1b = [
    assert (mapT (==3) Nil == Nil) "Success for mapT (*2) Nil",
    assert (mapT (*2) singleNode == dSingleNode) "Success for mapT (*2) singleNode",
    assert (mapT (length) stringTree == Node 4 (Node 4 Nil Nil) (Node 5 Nil Nil)) "Success for mapT (length) stringTree"
    ]

test1c = [
   assert (zipWithT (^^) Nil Nil == Nil) "Success for zipWithT (^^) Nil Nil",
   assert (zipWithT (const) singleNode Nil == Nil) "Success for zipWithT (const) singleNode Nil",
   assert (zipWithT (div) Nil singleNode == Nil) "Success for zipWithT (div) Nil singleNode",
   assert (zipWithT (+) singleNode singleNode == dSingleNode) "Success for zipWithT (+) singleNode singleNode",
   assert (zipWithT (*) simpleTree simpleTree == sqSimpleTree) "Success for zipWithT (*) simpleTree simpleTree",
   assert (zipWithT (*) singleNode simpleTree == dSingleNode) "Success for zipWithT (*) singleNode simpleTree"
   ]

test2a = [
    assert (insert 1 2 [] == [(1, 2)]) "Success for insert 1 2 []",
    assert (insert 1 "two" [] == [(1, "two")]) "Success for insert 1 \"two\" []",
    assert (insert "one" 2 [] == [("one", 2)]) "Success for insert \"one\" 2 []",
    assert (insert 1 2 [(3, 4), (5, 6)] =?= [(1, 2), (3, 4), (5, 6)]) "Success for insert 1 2 [(3, 4), (5, 6)]",
    assert (insert 1 2 [(1, 4), (5, 6)] =?= [(1, 2), (5, 6)]) "Success for insert 1 2 [(1, 4), (5, 6)]"
    ]

test2b = [
    assert (getValue 1 ([] :: Assoc Int Int) == Nothing) "Success for getValue 1 []",
    assert (getValue 1 [(2, 4), (3, 9)] == Nothing) "Success for getValue 1 [(2, 4), (3, 9)]",
    assert (getValue 1 [(1, 1), (2, 4), (3, 9)] == Just 1) "Success for getValue 1 [(1, 1), (2, 4), (3, 9)]"
    ]

test2c = [
    assert (removeKey 1 ([] :: Assoc Int Int) == []) "Success for removeKey 1 []",
    assert (removeKey 1 [(2, 4), (3, 9)] =?= [(2, 4), (3, 9)]) "Success for removeKey 1 [(2, 4), (3, 9)]",
    assert (removeKey 1 [(1, 1), (2, 4), (3, 9)] =?= [(2, 4), (3, 9)]) "Success for removeKey 1 [(1, 1), (2, 4), (3, 9)]"
    ]

test2d = [
    assert (getKeys ([] :: Assoc Int Int) == []) "Success for getKeys []",
    assert (getKeys [(1, 1)] == [1]) "Success for getKeys [(1, 1)]",
    assert (getKeys [(1, 1), (2, 4), (3, 9)] =?= [1, 2, 3]) "Success for getKeys [(1, 1), (2, 4), (3, 9)]"
    ]


test3a = [
    assert (extSum (Value 3) (Value 2) == Value 5) "Success for extSum (Value 3) (Value 2)",
    assert (extSum (Value 3) Infinity == Infinity) "Success for extSum (Value 3) Infinity",
    assert (extSum Infinity (Value 2) == Infinity) "Success for extSum Infinity (Value 2)"
    ]

test3b = [
    assert (extDiv (Value 8) (Value 2) == Value 4) "Success for extSum (Value 8) (Value 2)",
    assert (extDiv (Value 8) (Value 3) == Value 2) "Success for extSum (Value 8) (Value 2)",
    assert (extDiv (Value 8) (Value 0) == Infinity) "Success for extSum (Value 8) (Value 0)",
    assert (extDiv (Value 8) Infinity == Value 0) "Success for extSum (Value 8) Infinity",
    assert (extDiv Infinity (Value 3) == Infinity) "Success for extSum Infinity (Value 3)"
    ]

test3c = [
    assert (extLess (Value 3) (Value 9) == True) "Success for extLess (Value 3) (Value 9)",
    assert (extLess (Value 9) (Value 3) == False) "Success for extLess (Value 9) (Value 3)",
    assert (extLess (Value 101) Infinity == True) "Success for extLess (Value 101) Infinity",
    assert (extLess Infinity (Value 2999) == False) "Success for extLess Infinity (Value 2999)",
    assert (extLess Infinity Infinity == False) "Success for extLess Infinity Infinity"
    ]

allTests = [test1a, test1b, test1c, test2a, test2b, test2c, test2d,
            test3a, test3b, test3c]

runAll = mapM_ (mapM_ putStrLn) allTests
runTest test = mapM_ putStrLn test
