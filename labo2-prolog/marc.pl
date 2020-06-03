%COMMONS
pert(X,[X|_]).
pert(X,[_|L]):- pert(X,L).

%concat([],L,L).
%concat([X|L1],L2,[X|L3]):- concat(L1,L2,L3).

long([],0).
long([_|L],N) :- long(L,N1), N is 1+N1.

suma([],0).
suma([X|L], S) :- suma(L,S1), S is X+S1.

%pert_con_resto(X,L,R) :- concat(L1,[X|L2],L), concat(L1,L2,R).

count(_,[],0).
count(X,[X|L],N) :- count(X,L,N1), N is 1+N1, !.
count(X,[_|L],N) :- count(X,L,N).


/*
2. (dificultad 1) Escribe un predicado Prolog prod(L,P) que signifique “P es el
producto de los elementos de la lista de enteros dada L”. Debe poder generar la
P y tambien comprobar una ´ P dada.
*/
prod([X],X).
prod([X|L],P):- prod(L,P1), P is P1*X.      %the other order dont work

/*
3. (dificultad 1) Escribe un predicado Prolog pescalar(L1,L2,P) que signifique
“P es el producto escalar de los dos vectores L1 y L2”. Los dos vectores vienen
dados por las dos listas de enteros L1 y L2. El predicado debe fallar si los dos
vectores tienen una longitud distinta.
*/
pescalar([X],[X1], P) :- P is X*X1.
pescalar([X|L1],[Y|L2],P) :-
    pescalar(L1,L2,P1),
    P is P1+(X*Y).

/*
4. (dificultad 2) Representando conjuntos con listas sin repeticiones, escribe
predicados para las operaciones de interseccion y uni ´ on de conjuntos dados.
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
5. (dificultad 2) Usando el concat, escribe predicados para el ultimo de una lista ´
dada, y para el inverso de una lista dada.
*/
ultimo(L,X):- concat(_,[X],L).

inverso([X],[X]).
inverso(L,[X|I]):- concat(L2,[X],L), inverso(L2,I).


/*
6. (dificultad 3) Escribe un predicado Prolog fib(N,F) que signifique “F es el
Nesimo n ´ umero de Fibonacci para la ´ N dada”. Estos numeros se definen como: ´
fib(1) = 1, fib(2) = 1, y, si N > 2, como: fib(N) = fib(N − 1) + fib(N − 2).
*/
fib(1,1).
fib(2,1).
fib(N,F) :-
    N > 2,
    N1 is N-1, N2 is N-2,
    fib(N1,F1), fib(N2, F2),
    F is F1+F2.

/*
7. (dificultad 3) Escribe un predicado Prolog dados(P,N,L) que signifique “la lista
L expresa una manera de sumar P puntos lanzando N dados”. Por ejemplo: si P es
5, y N es 2, una solucion ser ´ ´ıa [1,4]. (Notese que la longitud de ´ L es N). Tanto
P como N vienen instanciados. El predicado debe ser capaz de generar todas las
soluciones posibles.
*/
dados(0,0,[]).
dados(P,N,[X|L]) :-
	N>0,
	pert(X,[1,2,3,4,5,6]),
	Q is P-X,
	M is N-1, dados(Q,M,L).



    %--------------------------------------------------------------------------

     concat([],L,L).
     concat([X|L1],L2,[X|L3]) :- concat(L1,L2,L3).

    % pert(X,[X|_]).
    % pert(X,[_|Y]) :- pert(X,Y).

     pert_con_resto(X,L,R) :- concat(L1,[X|L2],L), concat(L1,L2,R).

     permutacion([],[]).
     permutacion(L,[X|P]) :- pert_con_resto(X,L,R), permutacion(R,P).

    % long([],0).
    % long([_|L],M) :- long(L,N), M is N+1.

    % subcjto([],[]).
    % subcjto([_|C],S) :- subcjto(C,S).
    % subcjto([X|C],[X|S]) :- subcjto(C,S).


    %--------------------------------------------------------------------------



/*
8. (dificultad 2) Escribe un predicado suma demas(L) que, dada una lista de
enteros L, se satisface si existe algun elemento en ´ L que es igual a la
suma de los demas elementos de ´ L, y falla en caso contrario.
*/
suma_demas(L) :-
    pert(X,L),
    S is X+X,
    suma(L,S), !.

%suma_demas(L) :- pert_con_resto(X,L,R), suma(R,X), !. % sol profe

/*
9. (dificultad 2) Escribe un predicado suma ants(L) que, dada una lista de
enteros L, se satisface si existe algun elemento en ´ L que es igual a la
suma de los elementos anteriores a el en ´ L, y falla en caso contrario.
*/

suma_ants(L) :- concat(L1,[X|_],L), suma(L1,X), !. % si encontramos uno basta

%suma_ants(L) :- inverso(L,R), suma_ants_aux(R). %Mi solucion

%suma_ants_aux([X|L]) :- suma(L,X).
%suma_ants_aux([_|L]) :- suma_ants_aux(L).


/*
10. (dificultad 2) Escribe un predicado card(L) que, dada una lista de enteros L,
escriba la lista que, para cada elemento de L, dice cuantas veces aparece este ´
elemento en L. Por ejemplo, card( [1,2,1,5,1,3,3,7] ) escribira´
[[1,3],[2,1],[5,1],[3,2],[7,1]].
*/


card([],[]).
card( [X|L] , [ [X,N1] |Cr] ):-card(L,C),pert_con_resto([X,N],C,Cr),!,N1 is N+1. % wow, so cool!
card( [X|L] , [ [X,1]   |C] ):-card(L,C).

card(L):-card(L,C),write(C).


/*
11. (dificultad 2) Escribe un predicado Prolog est´a ordenada(L) que signifique
“la lista L de numeros enteros est ´ a ordenada de menor a mayor”. Por ejemplo, ´
con ?-est´a ordenada([3,45,67,83]). dice yes
Con ?-est´a ordenada([3,67,45]). dice no
*/



esta_ordenada([]).
esta_ordenada([_]) :-!.     %evita True y despues False y que pete al hacer match despues.
esta_ordenada([X,Y|L]) :- X =< Y, esta_ordenada([Y|L]).
/*esta_ordenada([X|L]):-    %another way
    max_list(L,Max),
    X =< Max,
    esta_ordenada(L).
    */


/*
12. (dificultad 2) Escribe un predicado Prolog ordenaci´on(L1,L2) que signifique
“L2 es la lista de enteros L1 ordenada de menor a mayor”. Por ejemplo: si L1 es
[8,4,5,3,3,2], L2 sera´ [2,3,3,4,5,8]. Hazlo en una l´ınea, utilizando solo ´
los predicados permutaci´on y est´a ordenada.
*/

ordenacion(L1,L2) :- permutacion(L1,L2), esta_ordenada(L2).

/*
13. (dificultad 2) ¿Que n ´ umero de comparaciones puede llegar a hacer en el caso ´
peor el algoritmo de ordenacion basado en ´ permutaci´on y est´a ordenada?
*/

/*
14. (dificultad 3) Escribe un predicado Prolog ordenación(L1,L2) basado en el
método de la inserción, usando un predicado insercion(X,L1,L2) que signi-
fique: “L2 es la lista obtenida al insertar X en su sitio en la lista de enteros L1 que
está ordenada de menor a mayor”.
*/

insercion(X,[],[X]).
insercion(X,[Y|L1],[X,Y|L1]) :-
    X =< Y, !.
    %L2 = [Y|L1].
insercion(X,[Y|L1],[Y|L2]) :- insercion(X,L1,L2).

ordenacion_2([],[]).
ordenacion_2([X|L1],L2) :- ordenacion_2(L1,L3), insercion(X,L3,L2).


/*
15. (dificultad 2) ¿Qué número de comparaciones puede llegar a hacer en el caso
peor el algoritmo de ordenación basado en la inserción?
*/

/*
16. (dificultad 3) Escribe un predicado Prolog ordenación(L1,L2) basado en el
método de la fusión (merge sort): si la lista tiene longitud mayor que 1, con
concat divide la lista en dos mitades, ordena cada una de ellas (llamada recur-
siva) y después fusiona las dos partes ordenadas en una sola (como una “crema-
llera”). Nota: este algoritmo puede llegar a hacer como mucho n log n compa-
raciones (donde n es el tamaño de la lista), lo cual es demostrablemente óptimo.
*/

% Divide una lista en dos mitades
split([],[],[]).
split([A],[A],[]).
split([A,B|R],[A|Ra],[B|Rb]) :-  split(R,Ra,Rb).

merge(L,[],L) :- !.
merge([],L,L).
merge([X|L1],[Y|L2],[X|L3]) :- X =< Y,!, merge(L1,[Y|L2],L3).
merge([X|L1],[Y|L2],[Y|L3]) :- merge([X|L1],L2,L3).


ordenacion_3([],[]) :- !.
ordenacion_3([X],[X]) :- !.
ordenacion_3(L1,L2) :-
    split(L1,A,B),
    ordenacion_3(A,SA),
    ordenacion_3(B,SB),
    merge(SA,SB,L2).


/*
17. (dificultad 4) Escribe un predicado diccionario(A,N) que, dado un alfabe-
to A de sı́mbolos y un natural N, escriba todas las palabras de N sı́mbolos, por
orden alfabético (el orden alfabético es según el alfabeto A dado). Ejemplo:
diccionario( [ga,chu,le],2) escribirá:
gaga gachu gale chuga chuchu chule lega lechu lele.
Ayuda: define un predicado nmembers(A,N,L), que utiliza el pert para obtener
una lista L de N sı́mbolos, escrı́be los sı́mbolos de L todos pegados, y provoca
backtracking.
*/

writeDicc([X]) :- write(X),!.
writeDicc([X|L]) :- write(X), writeDicc(L).


nmembers(_,0,[]) :- !.% :- write([]).
nmembers(A,N,[X|L]) :-
    pert(X,A),
    N1 is N-1,
    nmembers(A,N1,L).

/*  pert_con_resto(X,A,R),  %Solo pilla uno ¿porque? -> keep it simple stupid!
    N1 is N-1,
    long(L,N1),
    nmembers(R,N1,L),!. */

diccionario(A,N) :- nmembers(A,N,L), writeDicc(L),nl, fail.
