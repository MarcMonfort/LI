symbolicOutput(0).  % set to 1 to see symbolic output only; 0 otherwise.
/*
Problema 2: planificación del horario semanal de la FIB: cinco dı́as de 8 a 20h (12 horas diarias).
- En los datos de entrada se indican las listas de aulas, profesores, y cursos que hay.
- Todos ellos reciben un natural no nulo como identificador.

- Por cada curso se da su lista de asignaturas,
  cada una con su número de horas semanales de clase (máximo una al dı́a),
  la lista de aulas adecuadas para ella,
  y la lista de profesores que la podrı́an impartir.

- Todas las sesiones de una misma asignatura deben impartirse en la misma aula por el mismo profesor.
- Por cada profesor se indican las horas (con un entero entre 1 y 60) en que no puede dar clase.
- Por supuesto, no puede haber más de una clase a la vez por profesor ni por aula.

- Cada curso ha de tener un horario compacto (sin horas libres entre dos clases el mismo dı́a)
  y no más de seis horas de clase al dı́a.
- Sesiones de un mismo curso no pueden solaparse en el tiempo.

Ayuda: es mejor introducir diversos tipos de variables que relacionan dos entidades
       (profesor-asignatura, asignatura-hora, etc.) que tener variables que relacionan más de dos.
Hay que mostrar la solución por horas (cada hora qué hay) y despúes de cada curso (qué clases tiene).
    miEj: hora 1 -> curso1-assignatura1-aula1-profesor-1 // curso2-...
*/
/*
% Entrada
numCursos(4).
numAssignatures(23).
numAules(3).
numProfes(5).

% Sintaxi: assig(curs,assignatura,hores,llistaAules,llistaProfessors).
assig(1,1,2,[1,2,3],[1,3]).
assig(1,2,2,[2,3],[5]).
assig(1,3,4,[2,3],[1,3,4,5]).
assig(1,4,3,[1],[1,2,4,5]).
assig(1,5,3,[1,2,3],[1,2,3,4,5]).

assig(2,6,3,[3],[1,3,5]).
assig(2,7,3,[1,2,3],[3,4,5]).
assig(2,8,4,[2,3],[3,4]).
assig(2,9,3,[2],[1,2,3,5]).
assig(2,10,3,[1,2,3],[4,5]).
assig(2,11,2,[1,3],[1,3,5]).

assig(3,12,3,[1,3],[1,3,4,5]).
assig(3,13,4,[1,2],[1,3,5]).
assig(3,14,2,[1,2,3],[1,2,4,5]).
assig(3,15,4,[3],[1,2,3]).
assig(3,16,3,[1,2,3],[2,5]).
assig(3,17,3,[1,2,3],[1]).
assig(3,18,3,[1,2,3],[3]).

assig(4,19,4,[1,2,3],[3]).
assig(4,20,4,[3],[1,3,4,5]).
assig(4,21,3,[2,3],[2,3]).
assig(4,22,2,[2,3],[2,3,4,5]).
assig(4,23,4,[1],[3]).

% Sintaxi: horesProhibides(professor,llistaHores).
horesProhibides(1,[4,7,12,15,16,18,26,29,30,37,38,45,50,54]).
horesProhibides(2,[1,5,6,9,11,13,17,20,25,29,30,32,33,37,38,42,44,49,50,55,57]).
horesProhibides(3,[5,7,8,10,21,22,25,34,38,39,60]).
horesProhibides(4,[4,9,10,14,17,20,21,22,24,25,27,31,33,38,39,41,42,43,47,55,57]).
horesProhibides(5,[2,20,26,27,30,31,44,53,56,58]).*/


%Mi entrada
numCursos(2).
numAssignatures(1).
numAules(2).
numProfes(1).

% Sintaxi: assig(curs,assignatura,hores,llistaAules,llistaProfessors).
assig(1,1,4,[1],[1,2]).
assig(1,2,1,[1],[1]).

assig(2,3,2,[2],[1,2]).

horesProhibides(1,[1]).
horesProhibides(2,[]).
horesProhibides(3,[1,2]).


%%%%%% Some helpful definitions to make the code cleaner:

assignatura(As):-numAssignatures(N), between(1,N,As).
aula(Au):-numAules(N), between(1,N,Au).
profe(P):-numProfes(N), between(1,N,P).
hora(H):-between(1,4,H). %¿¿¿Mejor 1 - 12???
curso(C):-numCursos(N), between(1,N,C).

assigCurso(As,C):- assignatura(As), curso(C), assig(C,As,_,_,_).
assigAula(As,Au):- assignatura(As), aula(Au), assig(_,As,_,LAu,_), member(Au,LAu).
assigProfe(As,P):- assignatura(As), profe(P), assig(_,As,_,_,LP), member(P,LP).
assigNHora(As,NH):- assignatura(As), assig(_,As,NH,_,_).

horaProhibidaProfe(P,H):- profe(P), horesProhibides(P,LH), member(H,LH).

%%%%%%  1. SAT Variables:

%¿¿¿Hace falta variable con Assig curso???
satVariable( ap(As,P) ):- assignatura(As), profe(P).
satVariable( aa(As,Au)):- assignatura(As), aula(Au).
satVariable( ah(As,H) ):- assignatura(As), hora(H).
satVariable( ph(P,H) ):- profe(P), hora(H).



%%%%%%  2. Clause generation:

writeClauses:-
    unaAssignaturaAula,
    unaAssignaturaProfe,
    numAssignaturaHoras, %tarda MUCHOOOO
    horaProhibidaProfe,
    maxUnProfeHora,


    %maxUnProfeHora,
    true,!.                    % this way you can comment out ANY previous line of writeClauses
writeClauses:- told, nl, write('writeClauses failed!'), nl,nl, halt.

% Sintaxi: assig(curs,assignatura,hores,llistaAules,llistaProfessors).
%lalala:- assig(C,As,H,LAu,LP), assig(1,1,2,[1,2,3],[1,3]), writeClause([-p(E1,E2,J1),p(E2,E1,J2)]),fail.
lalala.

%mama:- member(ap(As,P), M ), findall( aa(As,Au), (aula(Au),member(Au,LAu)), Lits ), exactly(1,Lits),fail.
mama.
% member(findall( aa(As,Au), aula(Au), Lits )

unaAssignaturaAula:- assignatura(As), findall( aa(As,Au), (assigAula(As,Au)), Lits ), exactly(1,Lits), fail.
unaAssignaturaAula.

unaAssignaturaProfe:- assignatura(As), findall( ap(As,P), (assigProfe(As,P)), Lits ), exactly(1,Lits), fail.
unaAssignaturaProfe.

numAssignaturaHoras:- assignatura(As), assigNHora(As,NH), findall( ah(As,H), hora(H), Lits ),  exactly(NH,Lits), fail.
numAssignaturaHoras.

horaProhibidaProfe:- profe(P), horaProhibidaProfe(P,H), assigProfe(As,P), writeClause([-ah(As,H), -ap(As,P)]), fail.
horaProhibidaProfe.

maxUnProfeHora:- profe(P), hora(H), assigProfe(As,P), assigProfe(As2,P), As \= As2,
    writeClause([-ap(As,P), -ap(As2,P), -ah(As,H), -ah(As2,H)]), fail.
maxUnProfeHora.

%- Por supuesto, no puede haber más de una clase a la vez por profesor ni por aula.
maxmaxmax:- profesor(P), hora(H), findall( ah(As,H),  assigProfe(As,P)   , Lits ), atMost(1,Lits),fail. %creo que falla, porque pone un maximo
maxmaxmax.
/* %¿¿¿Añadir relacion obvia???
siAsHsoPH:- profesor(P), assigProfe(As,P), hora(H), writeClause([-ah(As,H), ph(P,H)]), fail.
siAsHsoPH.
horaProfeAssignatura:- profe(P), assigProfe(As,P), hora(H), writeClause([-ah(As,H),-ap(As,P), ph(P,H)]), fail.
horaProfeAssignatura.
*/
/*
maxUnProfeHora:- profe(P), hora(H), assigProfe(As,P), assigProfe(As2,P), As \= As2,
    writeClause([-ap(As,P), -ap(As2,P), -ah(As,H), -ah(As2,H)]), fail.
maxUnProfeHora.
*/

/*%GENERAL
unaAssignaturaAula:- assignatura(As), findall( aa(As,Au), aula(Au), Lits ), exactly(1,Lits), fail.
unaAssignaturaAula.

unaAssignaturaProfe:- assignatura(As), findall( ap(As,P), profe(P), Lits ), exactly(1,Lits), fail.
unaAssignaturaProfe.
*/

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


%%%%%%  3. DisplaySol: show the solution. Here M contains the literals that are true in the model:
displaySol(M):- nl, assignatura(As), nl, member(ap(As,P), M ), write(ap(As,P)), write(' '),
    member(aa(As,Au), M ),write(aa(As,Au)), write(' '),
    member(ah(As,H), M ),write(ah(As,H)), write(' '), fail.
displaySol(_):- nl,nl.

/*
displaySol(M):- nl, assignatura(As), nl, member(ap(As,P), M ), write(ap(As,P)), write(' '), fail.
displaySol(_):- nl,nl.
*/

/*displaySol(M):- nl, hora(H), nl, member(ah(As,H), M ), write(ah(As,H)), write(' '), fail.
displaySol(_):- nl,nl.*/

line(I):-member(I,[4,7]), nl,!. %para separar dos veces si es otro bloque
line(_).
space(J):-member(J,[4,7]), write(' '),!. % lo mismo pero vertical
space(_).

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
