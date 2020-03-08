%P2
prod([X],X).
prod([X|L],P):- prod(L,P1), P is P1*X.      %the other order dont work

%P3
pescalar([X],[X1], P) :- P is X*X1.
pescalar([X|L1],[Y|L2],P) :- 
    pescalar(L1,L2,P1), 
    P is P1+(X*Y).

%P4
pert(X,[X|_]).
pert(X,[_|L]):- pert(X,L).


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


%P5
concat([],L,L).
concat([X|L1],L2,[X|L3]):- concat(L1,L2,L3).

ultimo(L,X):- concat(_,[X],L).

inverso([X],[X]).
inverso(L,[X|I]):- concat(L2,[X],L), inverso(L2,I).