nat(zero).
nat(X) :- X = succ(Y), nat(Y).

add(X) :-  S is X+3,write(S).

minus(X):-  S is X-3,write(S).


toNat(0,zero).
toNat(X,succ(Res)) :- X>0, Xp is X-1, toNat(Xp,Res). 
 
fromNat(zero,0).
fromNat(succ(X),R) :- fromNat(X,Rp), R is Rp + 1. 

min3(X,Y,R):- X<Y , R is X, write(R).
min3(X,Y,R):- X>Y , R is Y, write(R).
max(X,Y,R):- X>Y , R is X, write(R).
max(X,Y,R):- X<Y , R is Y, write(R).

max(X,Y):-
(  
 X=Y -> 
  write('both are equal')
 ;
 X>Y -> 
  (
  Z is X, 
  write(Z)
  )
  ;
  (
  Z is Y, 
  write(Z)
  ) 
).

gt2(X,Y):-(
X>Y

	).

leq2(X,Y):-(X  = Y).

div3(X,Y,R):-  R is (X div Y).
mod3(X,Y,R):- (R is (X mod Y)).

gcd(0, X, X):- X > 0.
gcd(X, Y, Z):- X >= Y, X1 is X-Y, gcd(X1,Y,Z).
gcd(X, Y, Z):- X < Y, X1 is Y- X, gcd(X1,X,Z).


isList(void).
isList(cons(_,T)) :- isList(T).

p([H|T], H, T).

head([X|_],R):- (R is X).
tail([X|Y],R):- p([X|Y], _, R),write(R).

size([],0).
size([_|Y],R):- size(Y,L) , R is L+1.

concat([],Z):- write(Z).
concat([X|Y],Z):-concat(Y,cons(X,Z)) , write(Z).

reverse(Z):- concat(Z,[]).


fromList(void,[]).
fromList(cons(H,T),[H|R]) :- fromList(T,R).

toList([],void).
toList([H|R],cons(H,T)):- toList(R,T).


kelem([X|_],1,X).
kelem([_|R],K,Y):- K>1, K1 is K-1 ,kelem(R,K1,Y).


rem2(A,B):-(sort(A, B)).


head1(cons(X,_),R):- R = X.
tail1(cons(_,T),R):- R = T.

size1(void,0).
size1(cons(_,B),R):- size1(B,R1) , R = R1+1.

concat1(void,Z):-write(Z).
concat1(cons(A,B),C):-concat1(B,cons(A,C)).

reverse1(A):- concat1(A,void).
concat2(void,Z):-write(Z).
concat2(reverse1(cons(A,B)),C):-concat2(B,cons(A,C)).


add1(zero,zero,zero).
add1(succ(X),succ(Y),R):- add1(X,Y,R1),R=succ(succ(R1)).
add1(zero,succ(Y),R):- add1(zero,Y,R1),R=succ(R1).
add1(succ(Y),zero,R):- add1(Y,zero,R1),R=succ(R1).


minus1(succ(X),succ(Y),R):- minus1(X,Y,R).
minus1(zero,X,R):- add1(zero,X,R).
minus1(X,zero,R):- add1(X,zero,R).
minus1(zero,zero,zero).


min1(succ(X),succ(Y),R):- min1(X,Y,succ(R)).
min1(zero,A,A).
min1(_,zero,_):- write("al doilea").
min1(zero,zero,zero).

max1(succ(X),succ(Y)):- max1(X,Y).
max1(zero,_):- write("al doilea").
max1(_,zero):- write("primul").
max1(zero,zero).


concat3(void,A,A).
concat3(cons(A,B),C,R):-concat3(B,cons(A,C) ,R).

reverse3(A,R):-concat3(A,void,R).

reverse2(A):- concat1(A,void).
concat4(void,Z):-write(Z).
concat4(reverse1(cons(A,B)),C):-concat4(B,cons(A,C)).

greater(succ(_),zero).
greater(succ(X),succ(Y)):- greater(X,Y).





toList1([],void).
toList1([A|B],cons(R,T)):- toList1(B,T).

rem([X|H],[]):-rem(H,X).
rem([],[]).
rem([],_).
rem([H|T],[X|R]):- rem(T,[X|R]).


remove([], []).

remove([X | T], R) :- member(X, T), remove(T, R).

remove([X | T], [X | R]) :- not(member(X, T)), remove(T, R).


flatten2([], []) :- !.
flatten2([L|Ls], FlatL) :-
    !,
    flatten2(L, NewL),
    flatten2(Ls, NewLs),
    append(NewL, NewLs, FlatL).
flatten2(L, [L]).


slice(_,1,1,[]).
slice([_|T],A,B,R):- A1 is A-1 , B1 is B-1 ,slice(T,A1,B1,R).
slice([H|T],1,B,[H|R]):- B1 is B-1,slice(T,1,B1,R).