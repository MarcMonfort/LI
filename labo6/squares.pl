:- use_module(library(clpfd)).

%ejemplo(_, Big, [S1...SN]): how to fit all squares of sizes S1...SN in a square of size Big?
ejemplo(0,  3,[2,1,1,1,1,1]).
ejemplo(1,  4,[2,2,2,1,1,1,1]).
ejemplo(2,  5,[3,2,2,2,1,1,1,1]).
ejemplo(3, 19,[10,9,7,6,4,4,3,3,3,3,3,2,2,2,1,1,1,1,1,1]).

ejemplo(4,112,[50,42,37,35,33,29,27,25,24,19,18,17,16,15,11,9,8,7,6,4,2]).
ejemplo(5,175,[81,64,56,55,51,43,39,38,35,33,31,30,29,20,18,16,14,9,8,5,4,3,2,1]).

main:- 
    ejemplo(3,Big,Sides),
    nl, write('Fitting all squares of size '), write(Sides), write(' into big square of size '), write(Big), nl,nl,
    % 1. vars + domains
    % 1a. vars
    length(Sides,N),
    length(RowVars,N), % get list of N prolog vars: Row coordinates of each small square (ABAJO IZQUIERDA)
    length(ColVars,N), % get list of N prolog vars: Col coordinates of each small square
    append(RowVars, ColVars, L),

    insideBigSquare(N,Big,Sides,RowVars),
    insideBigSquare(N,Big,Sides,ColVars),
    nonoverlapping(N,Sides,RowVars,ColVars),
    %format("papa"),

    %label(L),
    labeling([ff], L), %First Fail
    displaySol(N,Sides,RowVars,ColVars), halt.



%RESTRICCIONS
insideBigSquare(_,_,[],[]).
insideBigSquare(_, Big, [S|Sides], [V|Vars]):-
    V #>= 1,
    V #=< Big-S+1,
    insideBigSquare(_, Big, Sides, Vars).


% No overlappiing
nonoverlapping(_,Sides,RowVars,ColVars):-
    rectangles(Sides, RowVars, ColVars, L),
    disjoint2(L).

rectangles([],[],[],[]).
rectangles([S|Sides], [X|RowVars], [Y|ColVars], [Rect|L]):-
    Rect = f(X, S, Y, S), % f(X_i, W_i, Y_i, H_i)
    rectangles(Sides, RowVars, ColVars, L ).


% SOL GITHUB (MAS ineficiente!!)
/* % Agafa cada quadrat i l'envia a comprovar que no té solapaments.        
nonoverlapping(_, [], [], []).        
nonoverlapping(_, [Side|Sides], [RowCoord|RowVars], [ColCoord|ColVars]):-
        neverOverlap(Side, Sides, RowCoord, RowVars, ColCoord, ColVars),
        nonoverlapping(_, Sides, RowVars, ColVars).

% Per un quadrat comprova si es solapa amb tots els altres.        
neverOverlap(_, [], _, [], _, []).
neverOverlap(Side1, [Side2|Sides], Row1, [Row2|RowVars], Col1, [Col2|ColVars]):-
        nonOverLapping2squares(Side1, Side2, Row1, Row2, Col1, Col2),
        neverOverlap(Side1, Sides, Row1, RowVars, Col1, ColVars).

% Per cada dos quadrats, comprova que no es solapin entre ells.        
nonOverLapping2squares(Side1, Side2, Row1, Row2, Col1, Col2):-
        Row1 + Side1 #=< Row2;
        Col1 + Side1 #=< Col2;
        Row2 + Side2 #=< Row1;
        Col2 + Side2 #=< Col1.
 */
% FI SOL GITHUB


%DISPLAY
displaySol(N,Sides,RowVars,ColVars):- 
    between(1,N,Row), nl, between(1,N,Col),
    nth1(K,Sides,S),    
    nth1(K,RowVars,RV),    RVS is RV+S-1,     between(RV,RVS,Row),
    nth1(K,ColVars,CV),    CVS is CV+S-1,     between(CV,CVS,Col),
    writeSide(S), fail.
displaySol(_,_,_,_):- nl,nl,!.

writeSide(S):- S<10, write('  '),write(S),!.
writeSide(S):-       write(' ' ),write(S),!.

