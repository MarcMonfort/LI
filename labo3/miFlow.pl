

%

%ENTRADA

%% Ejemplo 1:
/* size(9).
c(blue,  9,1,2,2).
c(brown, 3,1,8,4).
c(red,   3,4,1,7).
c(cyan,  1,8,4,4).
c(green, 1,9,5,2).
c(yellow,7,7,7,9).
c(pink,  6,5,8,7).
c(violet,8,9,9,6).
c(orange,5,8,8,8).
 */

/* %Ejemplo 2:
size(14).
c( blue,       9,10, 12,11).
c( brown,      4,9,  14,10).
c( red,        6,4,  7,8  ).
c( cyan,       7,3,  7,5  ).
c( green,      6,7,  8,8  ).
c( yellow,     8,1,  5,11 ).
c( pink,       11,3, 12,5 ).
c( violet,     5,2,  13,13).
c( orange,     11,4, 9,8  ).
c( darkblue,   2,2,  2,6  ).
c( darkgreen,  10,5, 10,8 ).
c( darkred,    14,11,11,14).
c( darkcyan,   6,5,  3,12 ).
c( white,      9,5,  8,12 ).
c( grey,       14,8, 10,10). */

/* size(4).
c( pink, 1,1,4,4). */

size(13).
c( blue,       6,1,  13,6).
c( brown,      7,3,  7,6).
c( red,        9,5,  12,5 ).
c( cyan,       5,1,  13,7).
c( green,      9,2,  4,10  ).
c( yellow,     4,2,   8,5 ).
c( pink,       11,3,  11,5 ).
c( violet,     5,5,   9,9).
c( orange,     3,8,  8,3  ).



symbolicOutput(0).  % set to 1 to see symbolic output only; 0 otherwise.

%%%%%% Some helpful definitions to make the code cleaner:

color(C):- c(C,_,_,_,_).
init(X,Y):- c(_,X,Y,_,_).
end(X,Y):- c(_,_,_,X,Y).
coord(X):- size(N), between(1,N,X).
square(X,Y):- coord(X), coord(Y).

neighbor(X,Y,A,B):- A=X, B is Y+1, square(A,B).
neighbor(X,Y,A,B):- A=X, B is Y-1, square(A,B).
neighbor(X,Y,A,B):- B=Y, A is X+1, square(A,B).
neighbor(X,Y,A,B):- B=Y, A is X-1, square(A,B).

getColor(X,Y,C):- c(C,X,Y,_,_).
getColor(X,Y,C):- c(C,_,_,X,Y).



isNumber(N):- size(S), S2 is S*S, between(1,S2,N).
lastNumber(N):- size(S), N is S*S.



%%%%%%  1. SAT Variables:
% color(X,Y,C) means "square [X,Y] has color C"
satVariable( color(X,Y,C) ):- square(X,Y), color(C), !.

% succ(X,Y,A,B) means "square [X,Y] has square [A,B] as its successor"
satVariable( succ(X,Y,A,B) ):- square(X,Y), square(A,B), !.

satVariable( num(X,Y,N) ):- square(X,Y), isNumber(N), !.




%%%%%%  2. Clause generation:sssss

writeClauses:-
    tieneUnSucesor,
    finalNoTieneSucesor,
    esSucesorDe,
    inicialNoEsSucesorDe,

    inicialFinalTieneColor,
    mismoColorSucesor,

    inicialNumeroUno,
    numeroSucesor,
    noLastNumber,

    %papa,

    true,!.                    % this way you can comment out ANY previous line of writeClauses
writeClauses:- told, nl, write('writeClauses failed!'), nl,nl, halt.


papa:- square(X,Y), findall( color(X,Y,C), color(C), Lits), exactly(1,Lits), fail.
papa.




%% Expressar propiedades de succesor:
% - cada casilla no final tiene exactamente un sucessor entre sus vecinos
tieneUnSucesor:- 
    square(X,Y),
    \+end(X,Y),
    findall( succ(X,Y,A,B), neighbor(X,Y,A,B), Lits), exactly(1,Lits), fail.
    /* square(A,B),
    \+neighbor(X,Y,A,B), dif((X,Y), (A,B)),
    writeClause([-succ(X,Y,A,B)]), fail. */
tieneUnSucesor.

% - cada casilla final no tiene sucesores % ¿¿para neighbors o para todas??
finalNoTieneSucesor:-
    end(X,Y),
    square(A,B),
    writeClause([-succ(X,Y,A,B)]), fail.
finalNoTieneSucesor.

% - cada casilla no inicial es sucesor de exactamente uno de sus vecinos
esSucesorDe:-
    square(A,B),
    \+init(A,B),
    findall( succ(X,Y,A,B), neighbor(A,B,X,Y), Lits), exactly(1,Lits), fail.
esSucesorDe.
    
% - cada casilla inicial no es sucesor de nadie
inicialNoEsSucesorDe:-
    init(A,B),
    square(X,Y),
    writeClause([-succ(X,Y,A,B)]), fail.
inicialNoEsSucesorDe.

%% Expresar coloracion de las casillas
% - Las casilla iniciales y finales tienen su color (solo 1)
inicialFinalTieneColor:-
    (init(X,Y) ; end(X,Y)),
    getColor(X,Y,C),
    writeClause([color(X,Y,C)]),
    color(C2), dif(C,C2),
    writeClause([-color(X,Y,C2)]), fail.
inicialFinalTieneColor.

% - Si una casilla (no final) tiene su color C, su sucessor también
mismoColorSucesor:-
    square(X,Y),
    neighbor(X,Y,A,B),
    color(C),
    writeClause([-color(X,Y,C), -succ(X,Y,A,B), color(A,B,C)]), fail.
mismoColorSucesor.


% - No hay ciclos!!!

% - La casilla inicial tiene el número uno
inicialNumeroUno:-
    square(X,Y), % todas!!
    writeClause([num(X,Y,1)]), fail.
inicialNumeroUno.


% - El sucessor tiene el número siguiente.
numeroSucesor:-
    square(X,Y),
    neighbor(X,Y,A,B),
    isNumber(N),
    N2 is N + 1,
    isNumber(N2), % mal , porque queremos permitir error ??? Esta bien!! (no ultimo)
    writeClause([-num(X,Y,N), -succ(X,Y,A,B), num(A,B,N2)]), fail.
numeroSucesor.

% - Ninguna casilla (no final) puede tener el lastNumber
noLastNumber:-
    square(X,Y),
    \+end(X,Y),
    lastNumber(N),
    writeClause([-num(X,Y,N)]), fail.
noLastNumber.

/* minimoUnNumero:- % ya no hace falta, ponemos uno al principio.
    square(X,Y),
    findall( num(X,Y,N), isNumber(N), Lits), (1,Lits), fail.
minimoUnNumero. */



%%%%%%  3. DisplaySol: show the solution. Here M contains the literals that are true in the model:



%%% DisplaySol: show the solution. Here M contains the literals that are true in the model:

/* displaySol(M):- nl, square(X,Y), color(C), member(color(X,Y,C), M), write(color(X,Y,C)), nl, fail.
displaySol(M):- nl, square(X,Y), square(A,B), member(succ(X,Y,A,B), M), write(succ(X,Y,A,B)), nl, fail.
displaySol(M):- nl, square(X,Y), isNumber(N), member(num(X,Y,N), M), write(num(X,Y,N)), nl, fail.
%displaySol(_):- nl,nl.
 */

displaySol(_):- nl,nl, write('Input:   '), coord(X), nl, coord(Y), writeInputSq(X,Y), fail. 
displaySol(M):- nl,nl, write('Solution:'), coord(X), nl, coord(Y),
		member(color(X,Y,Color),M), setColor(Color), write(' o'), fail. 
displaySol(_):- resetColor, !.

writeInputSq(X,Y):- c(Color,X,Y,_,_), setColor(Color), write(' o'), !.
writeInputSq(X,Y):- c(Color,_,_,X,Y), setColor(Color), write(' o'), !.
writeInputSq(_,_):- resetColor, write(' ·'), !.

setColor(Color):- colorCode(Color,Code), put(27), write('[0;38;5;'), write(Code), write('m'), !.
resetColor:- put(27), write('[0m'), !.

colorCode( blue,       69  ).
colorCode( brown,      138 ).
colorCode( red,        196 ).
colorCode( cyan,       51  ).
colorCode( green,      46  ).
colorCode( yellow,     226 ).
colorCode( pink,       201 ).
colorCode( violet,     90  ).
colorCode( orange,     208 ).
colorCode( darkblue,   21  ).
colorCode( darkgreen,  28  ).
colorCode( darkred,    88  ).
colorCode( darkcyan,   30  ).
colorCode( white,      15  ).
colorCode( grey,       8   ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Everything below is given as a standard library, reusable for solving
%    with SAT many different problems.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Express that Var is equivalent to the disjunction of Lits:
expressOr( Var, Lits) :- symbolicOutput(1), write( Var ), write(' <--> or('), write(Lits), write(')'), nl, !.
expressOr( Var, Lits ):- member(Lit,Lits), negate(Lit,NLit), writeClause([ NLit, Var ]), fail.
expressOr( Var, Lits ):- negate(Var,NVar), writeClause([ NVar | Lits ]),!.

% Express that Var is equivalent to the conjunction of Lits:
expressAnd( Var, Lits) :- symbolicOutput(1), write( Var ), write(' <--> and('), write(Lits), write(')'), nl, !.
expressAnd( Var, Lits):- member(Lit,Lits), negate(Var,NVar), writeClause([ NVar, Lit ]), fail.
expressAnd( Var, Lits):- findall(NLit, (member(Lit,Lits), negate(Lit,NLit)), NLits), writeClause([ Var | NLits]), !.


%%%%%% Cardinality constraints on arbitrary sets of literals Lits:

exactly(K,Lits):- symbolicOutput(1), write( exactly(K,Lits) ), nl, !.
exactly(K,Lits):- atLeast(K,Lits), atMost(K,Lits),!.

atMost(K,Lits):- symbolicOutput(1), write( atMost(K,Lits) ), nl, !.
atMost(K,Lits):-   % l1+...+ln <= k:  in all subsets of size k+1, at least one is false:
	negateAll(Lits,NLits),
	K1 is K+1,    subsetOfSize(K1,NLits,Clause), writeClause(Clause),fail.
atMost(_,_).

atLeast(K,Lits):- symbolicOutput(1), write( atLeast(K,Lits) ), nl, !.
atLeast(K,Lits):-  % l1+...+ln >= k: in all subsets of size n-k+1, at least one is true:
	length(Lits,N),
	K1 is N-K+1,  subsetOfSize(K1, Lits,Clause), writeClause(Clause),fail.
atLeast(_,_).

negateAll( [], [] ).
negateAll( [Lit|Lits], [NLit|NLits] ):- negate(Lit,NLit), negateAll( Lits, NLits ),!.

negate( -Var,  Var):-!.
negate(  Var, -Var):-!.

subsetOfSize(0,_,[]):-!.
subsetOfSize(N,[X|L],[X|S]):- N1 is N-1, length(L,Leng), Leng>=N1, subsetOfSize(N1,L,S).
subsetOfSize(N,[_|L],   S ):-            length(L,Leng), Leng>=N,  subsetOfSize( N,L,S).


%%%%%% main:

main:-  symbolicOutput(1), !, writeClauses, halt.   % print the clauses in symbolic form and halt
main:-  initClauseGeneration,
tell(clauses), writeClauses, told,          % generate the (numeric) SAT clauses and call the solver
tell(header),  writeHeader,  told,
numVars(N), numClauses(C),
write('Generated '), write(C), write(' clauses over '), write(N), write(' variables. '),nl,
shell('cat header clauses > infile.cnf',_),
write('Calling solver....'), nl,
shell('picosat -v -o model infile.cnf', Result),  % if sat: Result=10; if unsat: Result=20.
	treatResult(Result),!.

treatResult(20):- write('Unsatisfiable'), nl, halt.
treatResult(10):- write('Solution found: '), nl, see(model), symbolicModel(M), seen, displaySol(M), nl,nl,halt.
treatResult( _):- write('cnf input error. Wrote anything strange in your cnf?'), nl,nl, halt.


initClauseGeneration:-  %initialize all info about variables and clauses:
	retractall(numClauses(   _)),
	retractall(numVars(      _)),
	retractall(varNumber(_,_,_)),
	assert(numClauses( 0 )),
	assert(numVars(    0 )),     !.

writeClause([]):- symbolicOutput(1),!, nl.
writeClause([]):- countClause, write(0), nl.
writeClause([Lit|C]):- w(Lit), writeClause(C),!.
w(-Var):- symbolicOutput(1), satVariable(Var), write(-Var), write(' '),!.
w( Var):- symbolicOutput(1), satVariable(Var), write( Var), write(' '),!.
w(-Var):- satVariable(Var),  var2num(Var,N),   write(-), write(N), write(' '),!.
w( Var):- satVariable(Var),  var2num(Var,N),             write(N), write(' '),!.
w( Lit):- told, write('ERROR: generating clause with undeclared variable in literal '), write(Lit), nl,nl, halt.


% given the symbolic variable V, find its variable number N in the SAT solver:
:-dynamic(varNumber / 3).
var2num(V,N):- hash_term(V,Key), existsOrCreate(V,Key,N),!.
existsOrCreate(V,Key,N):- varNumber(Key,V,N),!.                            % V already existed with num N
existsOrCreate(V,Key,N):- newVarNumber(N), assert(varNumber(Key,V,N)), !.  % otherwise, introduce new N for V

writeHeader:- numVars(N),numClauses(C), write('p cnf '),write(N), write(' '),write(C),nl.

countClause:-     retract( numClauses(N0) ), N is N0+1, assert( numClauses(N) ),!.
newVarNumber(N):- retract( numVars(   N0) ), N is N0+1, assert(    numVars(N) ),!.

% Getting the symbolic model M from the output file:
symbolicModel(M):- get_code(Char), readWord(Char,W), symbolicModel(M1), addIfPositiveInt(W,M1,M),!.
symbolicModel([]).
addIfPositiveInt(W,L,[Var|L]):- W = [C|_], between(48,57,C), number_codes(N,W), N>0, varNumber(_,Var,N),!.
addIfPositiveInt(_,L,L).
readWord( 99,W):- repeat, get_code(Ch), member(Ch,[-1,10]), !, get_code(Ch1), readWord(Ch1,W),!. % skip line starting w/ c
readWord(115,W):- repeat, get_code(Ch), member(Ch,[-1,10]), !, get_code(Ch1), readWord(Ch1,W),!. % skip line starting w/ s
readWord(-1,_):-!, fail. %end of file
readWord(C,[]):- member(C,[10,32]), !. % newline or white space marks end of word
readWord(Char,[Char|W]):- get_code(Char1), readWord(Char1,W), !.
%========================================================================================
