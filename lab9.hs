f1 n [] = [n]
f1 n  s =  if (n >0  ) then (f1 (n+1) (n:s)) else s

f2 n s k = f2 (n+1) (k:s) (n*k)


f3 (a:a1) n  k = f3 a1 (k:n) (k+a)