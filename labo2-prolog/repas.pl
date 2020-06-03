%2
prod([],1).
prod([X|L],P):- prod(L,P2), P is X*P2. 

%3
pescalar([],[],0).
pescalar([X|Xs], [Y|Ys], P):- 
    pescalar(Xs,Ys,P2), 
    P is X*Y+P2.

%4
%intersection(L1,L2,I)
intersec([],_,[]).
intersec([X|L1], L2 , [X|I]):- 
    member(X,L2),!, % se podria usar \+ member en vez de ! (en la otra)
    intersec(L1,L2,I).
intersec([_|L1], L2 , I):- 
    intersec(L1,L2,I).

%union(L1,L2,U)
union([],L2,L2).
union([X|L1], L2, U):-
    member(X,L2),!,
    union(L1,L2,U).
union([X|L1], L2, [X|U]):-
    union(L1,L2,U).

%5
%last(List, Last)
ultimo(L,Last):- 
    append(_, [Last], L).

%reverse(List, List2)
inverso([],[]).
inverso(L,[X|Invers]):- 
    append(Aux, [X], L), inverso(Aux, Invers).

%6
fib(2,1):-!.
fib(1,1):-!.
fib(N,F):- 
    N1 is N-1, 
    N2 is N-2, 
    fib(N1,F1), fib(N2,F2), 
    F is F1+F2. 

%7
dados(0,0,[]).
dados(P,N,[X|L]):-
    N > 0,
    member(X,[1,2,3,4,5,6]),
    Q is P-X,
    M is N-1, dados(Q,M,L).


%---------------------------COMMONS-----------------------------------------

%append
concat([],L,L).
concat([X|L1],L2,[X|L3]) :- concat(L1,L2,L3).

%member
pert(X,[X|_]).
pert(X,[_|Y]) :- pert(X,Y). 

no_pert(X, L) :- pert(X, L), !, fail.
no_pert(_, _).

%select
pert_con_resto(X,L,R) :- concat(L1,[X|L2],L), concat(L1,L2,R).  

%permutation
permutacion([],[]).
permutacion(L,[X|P]) :- pert_con_resto(X,L,R), permutacion(R,P).

%length
long([],0).
long([_|L],M) :- long(L,N), M is N+1.

subcjto([],[]). 
subcjto([_|C],S) :- subcjto(C,S). 
subcjto([X|C],[X|S]) :- subcjto(C,S). 

%sum_list
suma([],0).
suma([X|L],S) :- suma(L,S1), S is S1+X.

permutaciones_distintas(L, A, [P|X]) :-
 	permutacion(L, P),
 	no_pert(P, A),
    permutaciones_distintas(L, [P|A], X).
permutaciones_distintas(_, _, []).

%--------------------------------------------------------------------------



%8
suma_demas(L,X):-
    select(X,L,R),
    sum_list(R,X).

%9
suma_ants(L,X):-
    append(Ants,[X|_],L),
    sum_list(Ants,X).


%10
card( [], []).
card( [X|L] , [[X,N1] | Cr] ):- %hay mas apariciones
    card(L,C),
    select([X,N],C,Cr), !, 
    N1 is N+1.
card( [X|L] , [[X,1] | Cr] ):-  %no hay mas apariciones
    card(L,Cr).

card(L):-card(L,C), write(C).


%11
esta_ordenada([]).
esta_ordenada([_]):-!.
esta_ordenada([X,Y|L]):- 
    X =< Y,
    esta_ordenada([Y|L]).


%12
ordenacion(L1,L2):- 
    permutation(L1,L2), 
    esta_ordenada(L2),!.


%14
insercion(X, [], [X]).
insercion(X, [Y|L1], [X,Y|L1]):-
    X =< Y,!. %o poner X > Y (en la otra)
insercion(X, [Y|L1], [Y|L2]):-
    insercion(X, L1, L2).

ord_insercion([],[]).
ord_insercion([X|L1], L2):- 
    ord_insercion(L1, Aux),
    insercion(X,Aux,L2),!.

%16
split([],[],[]).
split([A],[A],[]).
split([A,B|R],[A|Ra],[B|Rb]) :-  split(R,Ra,Rb).

merge_sort([],[])   :- !.
merge_sort([X],[X]) :- !.
merge_sort(L,L3) :- split(L,L1,L2), merge_sort(L1,L11), merge_sort(L2,L22), 
  merge(L11,L22,L3). 
  
merge(L, [], L) :- !.
merge([], L, L).
merge([X|L1],[Y|L2],[X|L3]) :- X=<Y, !, merge(L1,[Y|L2],L3). % regla adecuada 
merge([X|L1],[Y|L2],[Y|L3]) :- merge([X|L1],L2,L3). 


%17
diccionario(A,N):-  nperts(A,N,S), escribir(S), fail.

nperts(_,0,[]):-!.
nperts(L,N,[X|S]):- pert(X,L), N1 is N-1, nperts(L,N1,S).

escribir([]):-write(' '),nl,!.
escribir([X|L]):- write(X), escribir(L).


%18
isPalindrom([]).
isPalindrom([_]).
isPalindrom([X|L]):-
    append(L2,[X],L),
    isPalindrom(L2).

palindromo(L):- permutation(L,L2), isPalindrom(L2), write(L2),nl, fail.

% Si no queremos que escriba repetidos se puede usar setof. 
palindroms(L) :- setof(P,(permutation(L,P), isPalindrom(P)),S), write(S). 


%19
suma([],[],[],C,C).
suma([X1|L1],[X2|L2],[X3|L3],Cin,Cout) :-
	X3 is (X1 + X2 + Cin) mod 10,
	C  is (X1 + X2 + Cin) //  10,
	suma(L1,L2,L3,C,Cout).


send_more_money1 :-

	L = [S, E, N, D, M, O, R, Y, _, _],
	permutacion(L, [0,1,2,3,4,5,6,7,8,9]),
	suma([D, N, E, S], [E, R, O, M], [Y, E, N, O], 0, M),

	write('S = '), write(S), nl,
	write('E = '), write(E), nl,
	write('N = '), write(N), nl,
	write('D = '), write(D), nl,
	write('M = '), write(M), nl,
	write('O = '), write(O), nl,
	write('R = '), write(R), nl,
	write('Y = '), write(Y), nl,
	write('  '), write([S,E,N,D]), nl,
	write('  '), write([M,O,R,E]), nl,
	write('-------------------'), nl,
	write([M,O,N,E,Y]), nl.


send_more_money2 :-

	L = [0,1,2,3,4,5,6,7,8,9],
	pert_con_resto(M,  [0,1], _),
	pert_con_resto(M,  L,  L0),
	pert_con_resto(O, L0, L1),
	pert_con_resto(R, L1, L2),
	pert_con_resto(Y, L2, L3),
	pert_con_resto(S, L3, L4),
	pert_con_resto(E, L4, L5),
	pert_con_resto(N, L5, L6),
	pert_con_resto(D, L6, _),
	suma([D, N, E, S], [E, R, O, M], [Y, E, N, O], 0, M),

	write('S = '), write(S), nl,
	write('E = '), write(E), nl,
	write('N = '), write(N), nl,
	write('D = '), write(D), nl,
	write('M = '), write(M), nl,
	write('O = '), write(O), nl,
	write('R = '), write(R), nl,
	write('Y = '), write(Y), nl,
	write('  '), write([S,E,N,D]), nl,
	write('  '), write([M,O,R,E]), nl,
	write('-------------------'), nl,
	write([M,O,N,E,Y]), nl.


%20
der(X, X, 1):-!.
der(C, _, 0) :- number(C).
der(A+B, X, A1+B1) :- der(A, X, A1), der(B, X, B1).
der(A-B, X, A1-B1) :- der(A, X, A1), der(B, X, B1).
der(A*B, X, A*B1+B*A1) :- der(A, X, A1), der(B, X, B1).
der(sin(A), X, cos(A)*B) :- der(A, X, B).
der(cos(A), X, -sin(A)*B) :- der(A, X, B).
der(e^A, X, B*e^A) :- der(A, X, B).
der(ln(A), X, B*1/A) :- der(A, X, B).


simplifica(E,E1):- unpaso(E,E2),!, simplifica(E2,E1).
simplifica(E,E).

unpaso(A+B,A+C):- unpaso(B,C),!.
unpaso(B+A,C+A):- unpaso(B,C),!.
unpaso(A*B,A*C):- unpaso(B,C),!.
unpaso(B*A,C*A):- unpaso(B,C),!.
unpaso(0*_,0):-!.
unpaso(_*0,0):-!.
unpaso(1*X,X):-!.
unpaso(X*1,X):-!.
unpaso(0+X,X):-!.
unpaso(X+0,X):-!.
unpaso(N1+N2,N3):- number(N1), number(N2), N3 is N1+N2,!.
unpaso(N1*N2,N3):- number(N1), number(N2), N3 is N1*N2,!.
unpaso(N1*X+N2*X,N3*X):- number(N1), number(N2), N3 is N1+N2,!.
unpaso(N1*X+X*N2,N3*X):- number(N1), number(N2), N3 is N1+N2,!.
unpaso(X*N1+N2*X,N3*X):- number(N1), number(N2), N3 is N1+N2,!.
unpaso(X*N1+X*N2,N3*X):- number(N1), number(N2), N3 is N1+N2,!.

%21
mis:- camino( [lado1,3,3], [lado2,0,0], [[lado1,3,3]] ).

camino(Fin,Fin,Cam):- inverso(Cam,Sol), write(Sol), nl.
camino(Ini,Fin,Cam):- paso(Ini,E), novisitado(E,Cam), camino(E,Fin,[E|Cam]).

novisitado(E,Cam):- pert(E,Cam), !,fail.
novisitado(_,_).

paso( [lado1,M1,C1], [lado2,M2,C2] ):- pasan(M,C), M2 is M1-M, C2 is C1-C, safe(M2,C2).
paso( [lado2,M1,C1], [lado1,M2,C2] ):- pasan(M,C), M2 is M1+M, C2 is C1+C, safe(M2,C2).

pasan(M,C):- member( [M,C], [ [0,1], [0,2], [1,0], [1,1], [2,0] ] ).

safe(M,C):- M>=0, M=<3, C>=0, C=<3, nocomen( M, C),
            M1 is 3-M,  C1 is 3-C,  nocomen(M1,C1).

nocomen(0,_).
nocomen(M,C):- M>=C.