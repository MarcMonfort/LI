%COMMONS
pert(X,[X|_]).
pert(X,[_|L]):- pert(X,L).

concat([],L,L).
concat([X|L1],L2,[X|L3]):- concat(L1,L2,L3).

long([],0).
long([_|L],N) :- long(L,N1), N is 1+N1.

suma([],0).
suma([X|L], S) :- suma(L,S1), S is X+S1.

pert_con_resto(X,L,R) :- concat(L1,[X|L2],L), concat(L1,L2,R). 

count(_,[],0).
count(X,[X|L],N) :- count(X,L,N1), N is 1+N1, !.
count(X,[_|L],N) :- count(X,L,N).


/*
P2 Escribe un predicado Prolog prod(L,P) que signifique “P es el
producto de los elementos de la lista de enteros dada L”. Debe poder generar la
P y tambi´en comprobar una P dada.
*/
prod([X],X).
prod([X|L],P):- prod(L,P1), P is P1*X.      %the other order dont work

/*
P3 Escribe un predicado Prolog pescalar(L1,L2,P) que signifique
“P es el producto escalar de los dos vectores L1 y L2”. Los dos vectores vienen
dados por las dos listas de enteros L1 y L2. El predicado debe fallar si los dos
vectores tienen una longitud distinta.
*/
pescalar([X],[X1], P) :- P is X*X1.
pescalar([X|L1],[Y|L2],P) :- 
    pescalar(L1,L2,P1), 
    P is P1+(X*Y).

/*
P4 Representando conjuntos con listas sin repeticiones, escribe predicados
para las operaciones de intersecci´on y uni´on de conjuntos dados.
*/
inter([],_,[]).
inter([X|L1],L2,[X|L3]) :- 
    pert(X,L2),!,
    inter(L1,L2,L3).
inter([_|L1],L2,L3) :- inter(L1,L2,L3).


union([],L,L).
union([X|L1],L2,L3) :-
    pert(X,L2),!,
    union(L1,L2,L3).
union([X|L1],L2,[X|L3]) :-
    union(L1,L2,L3).


/*
P5 Usando el concat, escribe predicados para el ´ultimo de una lista
dada, y para el inverso de una lista dada.
*/
ultimo(L,X):- concat(_,[X],L).

inverso([X],[X]).
inverso(L,[X|I]):- concat(L2,[X],L), inverso(L2,I).


/*
P6 Escribe un predicado Prolog fib(N,F) que signifique “F es el N-
´esimo n´umero de Fibonacci para la N dada”. Estos n´umeros se definen como:
fib(1) = 1, fib(2) = 1, y, si N > 2, como: fib(N) = fib(N - 1) + fib(N - 2).
*/
fib(1,1).
fib(2,1).
fib(N,F) :- 
    N > 2, 
    N1 is N-1, N2 is N-2, 
    fib(N1,F1), fib(N2, F2), 
    F is F1+F2.

/*
P7 Escribe un predicado Prolog dados(P,N,L) que signifique “la lista
L expresa una manera de sumar P puntos lanzando N dados”. Por ejemplo: si P es
5, y N es 2, una soluci´on ser´ıa [1,4]. (N´otese que la longitud de L es N). Tanto
P como N vienen instanciados. El predicado debe ser capaz de generar todas las
soluciones posibles.
*/
dados(0,0,[]).
dados(P,N,[X|L]) :-
	N>0,
	pert(X,[1,2,3,4,5,6]),
	Q is P-X,
	M is N-1, dados(Q,M,L).


/*
P8 Escribe un predicado suma_demas(L) que, dada una lista de enteros
L, se satisface si existe alg´un elemento en L que es igual a la suma de los
dem´as elementos de L, y falla en caso contrario.
*/
suma_demas(L) :- 
    pert(X,L),
    S is X+X,
    suma(L,S), !.

%suma_demas(L) :- pert_con_resto(X,L,R), suma(R,X), !. % sol profe

/*
P9 Escribe un predicado suma_ants(L) que, dada una lista de enteros
L, se satisface si existe alg´un elemento en L que es igual a la suma de los
elementos anteriores a ´el en L, y falla en caso contrario.
*/

suma_ants(L) :- concat(L1,[X|_],L), suma(L1,X), !. % si encontramos uno basta

%suma_ants(L) :- inverso(L,R), suma_ants_aux(R). %Mi solucion

%suma_ants_aux([X|L]) :- suma(L,X).
%suma_ants_aux([_|L]) :- suma_ants_aux(L). 


/*
P10 Escribe un predicado card(L) que, dada una lista de enteros L,
escriba la lista que, para cada elemento de L, dice cu´antas veces aparece este
elemento en L. Por ejemplo, card( [1,2,1,5,1,3,3,7] ) escribir´a
[[1,3],[2,1],[5,1],[3,2],[7,1]].
*/



card([],[]).
card( [X|L] , [ [X,N1] |Cr] ):-card(L,C),pert_con_resto([X,N],C,Cr),!,N1 is N+1. % wow, so cool!
card( [X|L] , [ [X,1]   |C] ):-card(L,C).

card(L):-card(L,C),write(C).
