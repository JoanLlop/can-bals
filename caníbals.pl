concatenar([], L, L).
concatenar([X|L1], L2, [X|L]):- concatenar(L1, L2, L).

permutacion([], []).
permutacion(L, [X|P]):- pert(X, L), resto(X, L, R), permutacion(R, P).

resto(X, [X|L], L).
resto(X, [Y|L], [Y|R]):-resto(X, L, R).


%pert(X,[_|Y]) :- pert(X,Y).
%pert(X,[X|_]).

pert(X, [X|_]).
pert(X, [_|L]):-pert(X, L).

subc(C, C).
subc(X, C):- C > 0, Cp is C-1, subc(X, Cp).


%ex. 2 ===============================================================================

prod([X|L], P):-
    prod(L, K),
    P is K*X.
prod([], P):- P is 1.

%ex. 3 ===============================================================================

pescalar([], [], 0).
pescalar([X|L1], [Y|L2], P):- pescalar(L1, L2, R), P is X*Y+R. 

%ex. 4 ===============================================================================

interseccio([], _, [] ).
interseccio( [X|L1], L2, [X|L3]):-
    member(X, L2),!,
    interseccio(L1, L2, L3).
interseccio( [_|L1], L2, L3):-
    interseccio(L1, L2, L3).

union([], L, L).
union( [X|L1], L2, L3):-
    member(X, L2),!,
    union(L1, L2, L3).
union( [X|L1], L2, [X|L3]):-
    union(L1, L2, L3).

%ex. 5 ===============================================================================

ultim(L,X):- concatenar(_,[X],L).
ultimo(L,X):- concatenar(_,[X],L).

invertir([], []).
invertir([X|L], I):- invertir(L, R), concatenar(R, [X], I).

%ex. 6 ===============================================================================

fib(1, 1).
fib(2, 1).
fib(N, F):-
    fib2(N, [F, _]).
fib2(1, [1, 0]).
fib2(2, [1, 1]).
fib2(N, [A,B] ):-
    N1 is N-1,
    fib2( N1, [B,C] ),!,
    A is B+C.

%ex. 7 ===============================================================================

dados(P, 1, [P]):- !, P>=1, P =< 6.
dados(P, N, [X|L]):- member(X, [1, 2, 3, 4, 5, 6]), P1 is P-X, N1 is N-1, dados(P1, N1, L).	

%ex. 8 ===============================================================================

suma_demes(L):- member(X, L), concatenar(R, [X|V], L), concatenar(R, V, L1), suma(L1, S), X is S.

suma_demas(L):- pert(X, L), resto(X, L, R), suma(R, X).

suma([], 0).
suma([S], S).
suma([X|L], S):- suma(L, S1), S is X+S1.

%pert(X, [X|_]).
%pert(X, [_|L]):-pert(X, L).

%ex. 9 ===============================================================================

suma_ants([]).
suma_ants([X|L]):- suma_ants2(X, L).

suma_ants2(X, [X|_]).
suma_ants2(Y, [X|L]):- suma_ants2(Y1, L), Y1 is Y+X.

%ex. 10 ==============================================================================

card1([]):- write(']'),!.

    
card1([X|L]):-
    count(X, L, N1),
    N is 1+N1,
    write(','),
    write('['),
    write(X),
    write(','),
    write(N), 
    write(']'),
    elimina(X, L, L1),
    card1(L1).

card([X|L]):-
    write('['),
    count(X, L, N1),
    N is 1+N1,
    write('['),
    write(X),
    write(','),
    write(N), 
    write(']'),
    elimina(X, L, L1),
    card1(L1).

count(_, [], 0).
count(X, [Y|L], N+1):-
    X =:= Y,!,
    count(X, L, N).
count(X, [_|L], N):-
    count(X, L, N).
    
elimina(_, [], []).
elimina(X, [Y|L], L1):-
    X == Y,!,
    elimina(X, L, L1).
elimina( X, [Y|L], [Y|L1] ):-
    elimina(X, L, L1).

%ex. 11 ==============================================================================

esta_ord([]).
esta_ord([X|L]):- esta_ord2(X, L).

esta_ord2(_, []).
esta_ord2(A, [X|L]):- \+(A>X), esta_ord2(X, L).

%ex. 12 ==============================================================================

ordenacion(L, L1):- permutacion(L, L1), esta_ord(L1). 

%ex. 13 ==============================================================================

%El numero de permutaciones respecte el numero d elements (n) és n!, i per cada permutació fem n comparacions => el nº de comparacions és O(n!*n) 

%ex. 14 ==============================================================================

insert_sort([], []):- !.
insert_sort([X|L1], L2):- ordenacion(L1, L3), !, insercio(X, L3, L2).

insercio(X, [], [X]):- !.
insercio(X, [Y|L1], [X|[Y|L1]]):- X =< Y, !.
insercio(X, [Y|L1], [Y|L2]):- insercio(X, L1, L2).

%ex. 15 ==============================================================================

%insercio es O(n) on n és el nº d elements, i es pot arribar a repetir n vegades (n*(n-1)/2 en realitat) => O(n^2).

%ex. 16 ==============================================================================

merge_sort([], []).
merge_sort([X], [X]).
merge_sort(L, O):- mi_concat(L1, L2, L), merge_sort(L1, O1), merge_sort(L2, O2), mi_merge(O1, O2, O), !.


mi_merge([], M2, M2).
mi_merge(M1, [], M1).
mi_merge([X|M1], [Y|M2], [X|M]):- X < Y, mi_merge(M1, [Y|M2], M).
mi_merge([X|M1], [Y|M2], [Y|M]):- \+(X < Y), mi_merge([X|M1], M2, M).

mi_concat([], [], []).
mi_concat([X], [], [X]).
mi_concat([X|L1], [Y|L2], [X,Y|L]):-mi_concat(L1, L2, L).

%ex. 17 ==============================================================================

diccionari(A, N):- nmembers(A, N, L), write1(L), write(' '), fail.

write1([]).
write1([X|L]):-write(X), write1(L).


nmembers(A, 1, [X|[]]):- pert(X, A).
nmembers(A, N, [X|L]):- N > 1, pert(X, A), N1 is N-1,nmembers(A, N1, L).

%ex. 18 ==============================================================================

palindromos(L):-setof(L1, (permutacion(L, L1), es_palindrom(L1)), Set), write(Set).

es_palindrom([]).
es_palindrom([_]).
es_palindrom([X|L]):-ultim(L, Y), X=Y, elimina_ultim(L, L1), es_palindrom(L1).

elimina_ultim([_], []).
elimina_ultim([X|L], [X|L1]):- elimina_ultim(L, L1).

%ex. 19 ==============================================================================

send_more_money():- permutacion([0, 1, 2, 3, 4, 5, 6, 7, 8, 9], L1), sumaP(L1), write(L1), !. 
sumaP([S,E,N,D,M,O,R,Y,_,_]):- AUX1 is (S*1000+E*100+N*10+D), AUX2 is (M*1000+O*100+R*10+E), AUX3 is M*10000+O*1000+N*100+E*10+Y, AUX3 is AUX1+AUX2.


%ex. 20 ==============================================================================

der(X, X, 1):-!.
der(C, _, 0) :- number(C).
der(A+B, X, A1+B1) :- der(A, X, A1), der(B, X, B1).
der(A-B, X, A1-B1) :- der(A, X, A1), der(B, X, B1).
der(A*B, X, A*B1+B*A1) :- der(A, X, A1), der(B, X, B1).
der(sin(A), X, cos(A)*B) :- der(A, X, B).
der(cos(A), X, -sin(A)*B) :- der(A, X, B).
der(e^A, X, B*e^A) :- der(A, X, B).
der(ln(A), X, B*1/A) :- der(A, X, B).

simplifica(E, SE):- op(E, Ep), !, simplifica(Ep, SE).
simplifica(E, E).

op(X*V+ (Y*V + Z), XY*V+Z):- number(X), number(Y), XY is X+Y.
op(X*V+ (V*Y + Z), XY*V+Z):- number(X), number(Y), XY is X+Y.
op(V*X+ (Y*V + Z), XY*V+Z):- number(X), number(Y), XY is X+Y.
op(V*X+ (V*Y + Z), XY*V+Z):- number(X), number(Y), XY is X+Y.

op(X*V+ (Z + Y*V), XY*V+Z):- number(X), number(Y), XY is X+Y.
op(X*V+ (Z + V*Y), XY*V+Z):- number(X), number(Y), XY is X+Y.
op(V*X+ (Z + Y*V), XY*V+Z):- number(X), number(Y), XY is X+Y.
op(V*X+ (Z + V*Y), XY*V+Z):- number(X), number(Y), XY is X+Y.


op(X+Y, X+Z):- op(Y, Z), !.
op(X+Y, Y+Z):- op(X, Z), !.
op(X*Y, X*Z):- op(Y, Z), !.
op(X*Y, Y*Z):- op(X, Z), !. 

op(_*0, 0):- !.
op(0*_, 0):- !.
op(X*1, X):- !.
op(1*X, X):- !.
op(X+0, X):- !.
op(0+X, X):- !.



op(X+Y, S):- number(X), number(Y), S is X+Y.
op(X*Y, S):- number(X), number(Y), S is X*Y.
op(X*V + Y*V, XY*V):- number(X), number(Y), XY is X+Y.
op(X*V + V*Y, XY*V):- number(X), number(Y), XY is X+Y.
op(V*X + Y*V, XY*V):- number(X), number(Y), XY is X+Y.
op(V*X + V*Y, XY*V):- number(X), number(Y), XY is X+Y.





%ex. 21 ==============================================================================

creuar():-viatge_anada(3, 3, 0, 0, [[3, 3, 0, 0]]).

viatge_anada(CR, MR, CHC, MHC, Mov):-
	subc(X, CR),
	CRP is CR-X,
	subc(Y, MR),
	MRP is MR-Y,
	S is X+Y,
	S=<2, S>=1,
	CHCP is CHC+X,
	MHCP is MHC+Y,
	condicions(CRP, MRP, CHCP, MHCP),
	no_visitat([CRP, MRP, CHCP, MHCP, 0], Mov),
	write("van "),
	write(X),
	write(" canivals i "),
	write(Y),write(" misioners."), nl,
	viatge_tornada(CRP, MRP, CHCP, MHCP, [[CRP, MRP, CHCP, MHCP, 0]|Mov]).

viatge_tornada(0, 0, 3, 3, _).
viatge_tornada(CR, MR, CHC, MHC, Mov):-
	subc(X, CHC),
	CHCP is CHC-X,
	subc(Y, MHC),
	MHCP is MHC-Y,
	S is X+Y,
	S=<2, S>=1,
	CRP is CR+X,
	MRP is MR+Y,
	condicions(CRP, MRP, CHCP, MHCP),
	no_visitat([CRP, MRP, CHCP, MHCP, 1], Mov),
	write("tornen "),
	write(X),
	write(" canivals i "),
	write(Y),write(" misioners."), nl,
	viatge_anada(CRP, MRP, CHCP, MHCP, [[CRP, MRP, CHCP, MHCP, 1]|Mov]).

condicions(_, 0, _, 0).
condicions(_, 0, CHC, MHC):-
	CHC =< MHC.
condicions(CR, MR, _, 0):-
	CR =< MR.
condicions(CR, MR, CHC, MHC):-
	CR =< MR,
	CHC =< MHC.



no_visitat(E, Mov):- pert(E, Mov), !, fail.
no_visitat(_,_).




