flatten([], []):-!.
flatten(X, [X]):- atom(X), !.
flatten(X, [X]):- number(X), !.
flatten([X|XS], L1):-
           flatten(X, L),
           flatten(XS, LS),
           append(L, LS, L1).


flatten_r(L, R):- flatten(L, L1), per_cada(L1, R).

elimina_r(_, [], []):- !.
elimina_r(X, [X|L], L1):- elimina_r(X, L, L1), !.
elimina_r(X, [Y|L], [Y|L1]):-elimina_r(X, L, L1), !.

per_cada([], []):-!.
per_cada([X|L], [X|LSR]):- elimina_r(X, L, L1), per_cada(L1, LSR). 


casas:-	Sol = [	[1,A1,B1,C1,D1,E1],
				[2,A2,B2,C2,D2,E2],
				[3,A3,B3,C3,D3,E3],
				[4,A4,B4,C4,D4,E4],
				[5,A5,B5,C5,D5,E5] ],
		        member( [_, roja, _, _, _, peru] , Sol),
		        member( [_, _, _, perro, _, francia] , Sol),
		        member( [_, _, pintor, _, _, japo] , Sol),
		        member( [_, _, _, _, ron, china] , Sol),
		        member( [1, _, _, _, _, hungria] , Sol),
		        member( [N, verde, _, _, coñac, _] , Sol),
		        	member(N, [1,2,3,4]), F is N+1,
		        member( [F, blanca, _, _, _, _] , Sol),
		        member( [_, _, escultor, caracol, _, _] , Sol),
		        member( [B, amarilla, actor, _, _, _] , Sol),
		        	member(B, [1, 2, 3, 4]), J is B+1, 
		        member( [3, _, _, _, cava, _] , Sol),
		        member( [J, _, _, caballo, _, _] , Sol),
		        member( [2, azul, _, _, _, _] , Sol),
		        member( [_, _, notario, _, whisky, _] , Sol),
		        member( [K, _, medico, _, _, _] , Sol),
		        	member(K, [1, 2, 3, 4, 5]), K =\= B, O is K-1,
		        member( [O, _, _, ardilla, _, _] , Sol),
		        member( [_, _, _, _, agua, _], Sol),
		        member( [_, _, _, conejo, _, _], Sol),
				write(Sol), nl.


reinas:-	Sol = [R1, R2, R3, R4, R5, R6, R7, R8],
				  member(1, Sol),
				  member(2, Sol),
				  member(3, Sol),
				  member(4, Sol),
				  member(5, Sol),
				  member(6, Sol),
				  member(7, Sol),
				  member(8, Sol),
				  forall(between(1, 7, N), no_cons(N, Sol)),
				write1(Sol), nl.

pos_x(N, Sol, X):-
	append(A, [N|_], Sol),!,
	length(A, X). 

no_cons(N, Sol):-
	pos_x(N, Sol, PXN),
	forall(member(E, Sol), no_diagonal1(N, PXN, E, Sol)),
	forall(member(E, Sol), no_diagonal2(N, PXN, E, Sol)).

no_diagonal2(N, _, N, _).
no_diagonal2(N, PXN, E, Sol):-
	pos_x(E, Sol, PXE),
	DIFE is PXE + E,
	DIFN is PXN + N,
	DIFN =\= DIFE.


no_diagonal1(N, _, N, _).
no_diagonal1(N, PXN, E, Sol):-
	pos_x(E, Sol, PXE),
	DIFE is PXE - E,
	DIFN is PXN - N,
	DIFN =\= DIFE.

write1(Sol):-
	length(Sol, LL),
	LL1 is LL-1,
	forall(member(X, Sol), write_line(X, LL1)).
write_line(X, LL):-
	X1 is X-1,
	forall(between(1, X1, _), write('.')),
	write('x'),
	forall(between(X, LL, _), write('.')), nl.