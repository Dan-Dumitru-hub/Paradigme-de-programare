type Dict = [(String,Integer)]

valueOf :: String -> Dict -> Integer 

valueOf str ((d1,d2):xs) = if str == d1 then d2 else valueOf str xs


ins :: String -> Integer -> Dict -> Dict

ins str val (ss : (d1,d2) :xs) =  if str == d1 then ss:(d1,val):xs else 
	if  xs == [] then (str,val) : ((d1,d2):xs)
		else ins str val xs

data PExpr = Val Integer |
       		  Var String  |
       		  PExpr :+: PExpr deriving Show
 
show_pexpr :: PExpr -> String
show_pexpr (Var x) = x
show_pexpr (Val x)	= show x	 
show_pexpr (a:+:b) = show_pexpr a ++ "+" ++ show_pexpr b



eval_pexpr :: Dict -> PExpr -> Integer
val ((x,a):xs) y = if x==y then a else val xs y

eval_pexpr  ((x,a):xs) (z:+:y) = (val  ((x,a):xs) (show_pexpr z) )+ ( val ((x,a):xs) (show_pexpr y ))

data BExpr = PExpr :==: PExpr | 
            PExpr :<: PExpr |
            Not BExpr |
            BExpr :&&: BExpr deriving Show
 
show_bexpr :: BExpr -> String
show_bexpr (a:<:b) = show_pexpr a ++ "<" ++ show_pexpr b
show_bexpr (a:&&:b) = show_bexpr a ++ "&&" ++ show_bexpr b
show_bexpr (a:==:b) = show_pexpr a ++ "==" ++ show_pexpr b
show_bexpr (Not a) = "not " ++ show_bexpr a 

val1 ((x,a):xs) (Not (y:==:z)) = if (val ((x,a):xs) y) ==  (val ((x,a):xs) z) then False else True

val1 ((x,a):xs) (Not (y:<:z)) = (val ((x,a):xs) y) >  (val ((x,a):xs) z)
--val1 ((x,a):xs) (y:&&:z) = (val ((x,a):xs) y) &&  (val ((x,a):xs) z)
--val1 ((x,a):xs) (y:&&:z) = (val ((x,a):xs) y) &&  (val ((x,a):xs) z)

eval_bexpr :: Dict -> BExpr -> Bool
eval_bexpr  ((x,a):xs) (z:<:y) =  (val  ((x,a):xs) (show_pexpr z) ) < ( val ((x,a):xs) (show_pexpr y ))
eval_bexpr  ((x,a):xs) (z:==:y) =  (val  ((x,a):xs) (show_pexpr z) ) == ( val ((x,a):xs) (show_pexpr y ))
eval_bexpr  ((x,a):xs) (Not z) =  val1 ((x,a):xs) (z) 
--eval_bexpr  ((x,a):xs) (z:&&:y) =  (val  ((x,a):xs) (show_bexpr z) ) < ( val ((x,a):xs) (show_bexpr y ))





{-}

data Prog = PlusPlus Var |        -- x++;
            Var :=: PExpr |     -- x = <expr>;
            DeclareInt Var |      -- int x;
            Begin Prog Prog |     -- <p> <p'>
            While BExpr Prog |     -- while (<expr>) { <p> 
            If BExpr Prog Prog      -- if (<expr>) { <p> } else { <p'> }   
 
show_p :: Prog -> String
show_p (PlusPlus v) = v++"++;\n"
show_p (x :=: e) = x++"="++(show_pexpr e)++";\n"
show_p (DeclareInt x) = "int "++x++";\n"
show_p (Begin p p') = (show_p p)++(show_p p')
show_p (While e p) = "while ("++(show_bexpr e)++") {\n"++(show_p p)++"}\n"
show_p (If e p p') = "if ("++(show_bexpr e)++") {\n"++(show_p p)++"}\n else {\n"++(show_p p')++"}\n"
 
instance Show Prog where
  show = show_p



Begin (DeclareInt x) (Begin (PlusPlus x) ( While (x:<:100) (x:=: x+1 )))
-}