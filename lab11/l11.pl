
sublist1([], []).
sublist1([_|Xs], Ys) :-
    sublist1(Xs, Ys).
sublist1([X|Xs], [X|Ys]) :-
    prefix_sublist(Xs, Ys).

prefix_sublist(_, []).
prefix_sublist([X|Xs], [X|Ys]) :-
    prefix_sublist(Xs, Ys).


    sublist2(L, R) :-  append([_,R, _], L).

    sublist(L,R):-append(Sub1,_,L),append(R,_,Sub1).

sublist(_,[]).


ascending([_]).
ascending([X | [Y | T]]) :- Y is X + 1, ascending([Y | T]).

%natlist([H | T]) :- H = 0, ascending([H | T]).

create(_,_).
create(X,H):-  X1 is X+1 , H1 is [X1|H], create(X1,H1).


natlist(R):- do_list1(0, [], R).

do_list1(_, L, L).
do_list1(N, R, L) :- N <10, N1 is N+1, do_list1(N1, [N|R], L).




oddonly([],[]).
oddonly([H|T], R)   :- H mod 2 =:= 0, oddonly(T, R).
oddonly([H|T1], [H|T2]) :- H  mod 2 =\= 0, oddonly(T1, T2).


oddlist(R):- do_list2(1, [], R).

do_list2(_, L, L).
do_list2(N, R, L) :- N <10, N1 is N+2, do_list1(N1, [N|R], L).


%eqelem(L), length(L,3), L=[0|_].

%eqelem(_).

%eqelem([A,B,T]) :- A = B, eqelem(T).

eqelem([]).
eqelem([_,[]]).
eqelem([H|T]):-(member(H,T)),eqelem(T).


repeat(X,K,R):-  R=[X|_],length(R,K),eqelem(R).


build(X,_,X).

build(X,N1,[X]):- N1>0, N is N1 - 1, build(X,N,[]).  
%pal(L):- append(L,_,C),append(_,L,C) ,reverse(C,C).


compare([X|Y]):-help(Y,X).
compare([]).

help([],_).
help([Y|X],Y) :- help(X,Y).



palind(R):- append(L,L1,R),append(L1,L,R).


%ksubset(C,K,V):-  length(C,K), append(C,_,V).

subset1([],[]).
subset1([X|L],[X|S]) :-
            subset1(L,S).
subset1(L, [_|S]) :-
            subset1(L,S).


%ksubset([],_,[]).
ksubset(R,K,S):-length(R,K),elements(R,S).
ksubset([X|L],K,[X|S]) :-K1 is K-1,
            ksubset(L,K1,S).
ksubset(L, K,[_|S]) :-K1 is K-1,
            ksubset(L,K1,S).
elements([],_).
elements([H|R],S):- member(H,S),elements(R,S).


vertexCover(N,L1,M,L2) :- numlist(1, N, L), comb(M,L,L2), covers(L2,L1).

covers(_,[]).
covers(L,[H|T]) :- isIn(L,H), covers(L,T).

isIn([A|T],(X/Y)) :-  (( A = X ; A = Y ) -> true
                       ;   isIn(T, (X/Y)) ).

comb(0,_,[]).
comb(N,[X|T],[X|Comb]):-    N>0,N1 is N-1,comb(N1,T,Comb).
comb(N,[_|T],Comb):-        N>0,comb(N,T,Comb).