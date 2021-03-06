/*
Problema 1: planificación de ligas deportivas. La liga española de primera división de fútbol tiene
20 equipos, que juegan todos contra todos en 19 jornadas la primera vuelta, y otra vez en otras 19
jornadas en la segunda vuelta, en el mismo orden, pero en casa del otro. Hay que admitir los siguientes
tipos de restricciones:
–el equipo i no quiere jugar en casa la jornada j,
–el equipo i sı́ quiere jugar en casa la jornada j,
–en las jornadas i e i + 1 no se admiten repeticiones: dos partidos seguidos en casa, o dos partidos
seguidos fuera,
–el partido i-i 0 (es decir, en casa del equipo i) no debe ser la jornada j
–el partido i-i 0 (es decir, en casa del equipo i) sı́ debe ser la jornada j.
No se permitirán tripeticiones, es decir, ningún equipo jugará tres jornadas seguidas en casa ni
tres jornadas seguidas fuera. Recomendación: primero aborda un subproblema, por ejemplo, una sola
vuelta, pocos equipos, etc. Extensión: a lo largo de la temporada ningún equipo tiene más de k
repeticiones (esto puede hacer que el problema SAT sea duro para ligas de mas de 16 equipos).
*/

symbolicOutput(0).  % set to 1 to see symbolic output only; 0 otherwise.

%:- include(entradaLigas).
/*
%miEntradaTest
numEquipos(4).
nocasa(_,_):-fail.  % evita que pete si no existe restricciones
nocasa(2,2).
nofuera(_,_):-fail.
nofuera(1,3).
nopartido(_,_,_):-fail.

sipartido(_,_,_):-fail.
sipartido(4,3,1).
norepes(_,_):-fail.
norepes(1,2).
*/

numEquipos(16).
nofuera(7,10).      %   –   el equipo i no quiere jugar en casa la jornada j,
nofuera(6,10).
nofuera(9,10).
nofuera(10,10).
nofuera(11,10).
nofuera(7,30).
nofuera(6,30).
nofuera(9,30).
nofuera(10,30).
nofuera(11,30).
nocasa(7,1).        %   –   el equipo i no quiere jugar en casa la jornada j,
nocasa(8,1).
nocasa(9,1).
nocasa(10,1).
nocasa(11,1).
nocasa(12,1).
norepes(1,2).       %   –   en las jornadas i e i + 1 no se admiten repeticiones: dos partidos seguidos en casa, o dos partidos seguidos fuera, (todos los equipos)
norepes(2,3).
norepes(28,29).
norepes(29,30).
nopartido(1,2,30).  %   –   el partido i-i 0 (es decir, en casa del equipo i) no debe ser la jornada j
nopartido(1,2,1).
nopartido(1,2,2).
nopartido(1,2,3).
nopartido(1,2,4).
nopartido(1,2,5).
nopartido(1,2,6).
nopartido(1,2,7).
sipartido(2,3,30).  %   –   el partido i-i 0 (es decir, en casa del equipo i) sı́ debe ser la jornada j.
sipartido(4,5,30).




%%%%%% Some helpful definitions to make the code cleaner:

%NEW
equipo(E):- numEquipos(N), between(1,N,E).
jornada(J):- numEquipos(N), N1 is N*2-2, between(1,N1,J). %N1 is N*2-2
primeraJornada(J):- numEquipos(N), N1 is N-1, between(1,N1,J).
segundaJornada(J):- numEquipos(N), N1 is N, N2 is N*2-2, between(N1,N2,J).
segundaVuelta(J1,J2):- primeraJornada(J1), numEquipos(N), J2 is J1+N-1. %¿¿¿Hace falta primeraJornada(N)???
tresJornadas(J1,J2,J3):- jornada(J1), jornada(J2), jornada(J3), J2 is J1+1, J3 is J2+1.



%%%%%%  1. SAT Variables:

%NEW
%p(E1,E2,J) mean equipo E1 y E2 juegan en la jornada J (E1 local).
satVariable( p(E1,E2,J) ):- equipo(E1), equipo(E2), jornada(J). %, E1 =\= E2 ???


%%%%%%  2. Clause generation:

%NEW
writeClauses:-
    notYourself,
    primeraIgualSegunda,
    unPartidoE1PorJornada,
    unPartidoE1E2porLiga,
    noJugarCasa,
    noJugarFuera,
    noPartido,
    siPartido,
    noRepes1,
    noTripeticiones,    % tarda mucho!!! peta con 4 (a no ser que primeraJornada)
    true,!.

writeClauses:- told, nl, write('writeClauses failed!'), nl,nl, halt.


notYourself:- equipo(E), jornada(J), writeClause([-p(E,E,J)]),fail.% findall( p(E,E,J), jornada(J), Lits ), exactly(0,Lits), fail.
notYourself.

primeraIgualSegunda:-
    equipo(E1), equipo(E2), E1 \= E2, primeraJornada(J1), segundaVuelta(J1,J2),
    writeClause([-p(E1,E2,J1),p(E2,E1,J2)]),fail.
primeraIgualSegunda.

unPartidoE1PorJornada:-
    equipo(E1), jornada(J),
    findall( p(E1,E2,J), (equipo(E2),E1 \= E2), Lits1 ),
    findall( p(E2,E1,J), (equipo(E2),E1 \= E2), Lits2 ),
    append(Lits1,Lits2,AllLits), exactly(1,AllLits),fail.
unPartidoE1PorJornada.

unPartidoE1E2porLiga:-
    equipo(E1), equipo(E2), E1 \= E2,
    findall( p(E1,E2,J), jornada(J), Lits ), exactly(1,Lits), fail. % no se puede usar primeraJornada() porque la vuelta tambine la cuenta
unPartidoE1E2porLiga.

noTripeticiones:- equipo(E1), equipo(E2), equipo(E3), equipo(E4),
    E1 \= E2, E1 \= E3, E1 \= E4,   % puede que pete con jornada en vez de primeraJornada() (peta con 4)
    E2 \= E3, E2 \= E4,
    E3 \= E4,       % con menos jornadas puede que pete!!!!!!
    jornada(J1), tresJornadas(J1,J2,J3),    %¿¿¿jornada() o primeraJornada()???
    writeClause([-p(E1,E2,J1),-p(E1,E3,J2),-p(E1,E4,J3)]),  %hay que pensar que es (...) -> ((E1) no local para ningun equipo)// en vez de ((E1) visitante para todos los equipos)
    writeClause([-p(E2,E1,J1),-p(E3,E1,J2),-p(E4,E1,J3)]), fail.
noTripeticiones.

noJugarCasa:- nocasa(E1,J), equipo(E2), E1 \= E2, writeClause([-p(E1,E2,J)]),fail.
noJugarCasa.

noJugarFuera:- nofuera(E1,J), equipo(E2), E1 \= E2, writeClause([-p(E2,E1,J)]),fail.
noJugarFuera.

noPartido:- nopartido(E1,E2,J), writeClause([-p(E1,E2,J)]),fail.
noPartido.

siPartido:- sipartido(E1,E2,J), writeClause([p(E1,E2,J)]), fail.
siPartido.

noRepes1:- norepes(J1,J2), equipo(E1), equipo(E2), equipo(E3),
    E1 \= E2, E1 \= E3, E2 \= E3,
    writeClause([-p(E1,E2,J1),-p(E1,E3,J2)]), %mismo pensamiento que noTripeticiones...
    writeClause([-p(E2,E1,J1),-p(E3,E1,J2)]), fail.
noRepes1.

%%%%%%  3. DisplaySol: show the solution. Here M contains the literals that are true in the model:

displaySol(M):-  jornada(J), nl,
    vuelta(J),nl,
    tab(5),write("—————————"),tab(2),write("Jornada: "), write(J),
    member(p(E1,E2,J), M ),
    nl, tab(6),sizeNum(E1), write(E1), write(" — "),write(E2),
    fail.
displaySol(_):- nl,nl.

sizeNum(N):-N < 10, write(" "),!.
sizeNum(_).

vuelta(1):-
    tab(1),write("Primera vuelta: " ), nl,
    write("————————————————————————————————————"),
    nl, tab(3),write("Local — Visitante"),!.
vuelta(J):- numEquipos(N), J is N, nl,nl,
    tab(1),write("Segunda vuelta: "), nl,
    write("————————————————————————————————————"),
    nl, tab(3),write("Local — Visitante"), !.
vuelta(_).

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
