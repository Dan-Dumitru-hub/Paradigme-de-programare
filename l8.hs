data LExpr = Var Char | Lambda Char LExpr | App LExpr LExpr | Lambda3 Char Char Char LExpr | App3 LExpr LExpr LExpr | Lambdan [Char] LExpr | Appn  [LExpr]  deriving Show

vars :: LExpr -> [Char]

vars (Var a) = show(a)
vars (Lambda a b) = a:(vars b)
vars (App a b) = vars a ++ vars b

reducible :: LExpr -> Bool
reducible (Var a) = False
reducible (Lambda a b) = if reducible b then False else True
reducible (App a b) = reducible a || reducible b

rename :: Char -> Char -> LExpr -> LExpr
rename a b (Var c) = if a == c then Var b else Var c
rename a b (Lambda d c) = if a==d then Lambda b (rename a b c) else Lambda d (rename a b c)
rename a b (App c d) = App (rename a b c) (rename a b d)

replace :: Char -> LExpr -> LExpr -> LExpr
replace a (Var b) c = rename a b c

replace a (Lambda c d) (Var e) = if a==e then Lambda c d else Var e
replace a  (Lambda c d)  (Lambda e f) = if a==e then App (Lambda c d) f else Lambda e (replace a (Lambda c d) f)
replace a (Lambda c d)  (App e f) = App (replace a (Lambda c d) e ) (replace a (Lambda c d) f )

f4 :: LExpr -> LExpr

--f4 (Lambda c d) =-- if c==d then Var c
--f4 (Lambda c d) --=-- --Lambda c (f4 d)
--f4 (Lambda c (Var e)) --=-- --if c==e then c else e
f4 (Var a)= Var a
f4 (Lambda c d) = case  d of
		Var e ->Lambda c d
		Lambda f g -> Lambda c (f4 (Lambda f g))
		App x y -> Lambda c (App (f4 x) (f4 y))





f4 (App c d) = case (c,d) of
	((Lambda x y ), (Var a))->  rename x a y
	((Lambda x y ), (Lambda a b))-> replace x (Lambda a b) y


display :: LExpr -> LExpr

display ( Lambda3 a b c d) = Lambda c(Lambda b  (Lambda a d))
display (App3 a b c ) =   App (App a b) c 
display (Lambdan [x] y) =Lambda x y 
display (Lambdan (x:xs) y) =  Lambda  x (display (Lambdan xs y)) 
display (Appn  [x,y]) = App  x y
display (Appn (x:xs)) = App (display (Appn  xs) )   x

lcurry :: LExpr->LExpr
lcurry (Lambda3 a b c d ) =  Lambda c (Lambda b  (Lambda a d) )
lcurry (Lambdan (x:xs) y) = display (Lambdan (x:xs) y)

luncurry :: LExpr->LExpr
luncurry  (Lambda c (Lambda b  (Lambda a d) )) = (Lambda3 a b c d )
luncurry (Lambda x y) = case y of
		Lambda a b -> Lambdan (x:a:[]) b


fv :: LExpr -> [Char]
fv (Var a) = []
fv  (App  a b )= (fv a) ++ (fv b)
fv (Lambda a b) = case b of
		Var x ->if x==a then [] else a:[]
		Lambda y z -> fv (Lambda y z)
bv :: LExpr -> [Char]
bv (Var a) = []
bv  (App  a b )= (bv a) ++ (bv b)
bv (Lambda a b) = case b of
		Var x ->if x==a then a:[] else []
		Lambda y z -> fv (Lambda y z)


libera :: LExpr -> Char -> Bool
libera (Var a) b = a==b
libera (Lambda a b) c = if a==c then False else (libera b c)
	--libera (App a b) = 

subst :: Char -> LExpr -> LExpr -> LExpr
subst a (Var b) (Lambda x y) =undefined
-- if (libera y b )== True then rename a b (Lambda x y) else Lambda x y