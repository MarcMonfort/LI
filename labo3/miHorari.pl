symbolicOutput(0).  % set to 1 to see symbolic output only; 0 otherwise.
/*
Problema 2: planificación del horario semanal de la FIB: cinco dı́as de 8 a 20h (12 horas diarias).
- En los datos de entrada se indican las listas de aulas, profesores, y cursos que hay.
- Todos ellos reciben un natural no nulo como identificador.

OK - Por cada curso se da su lista de asignaturas,
  cada una con su número de horas semanales de clase OK(máximo una al dı́a),
  la lista de aulas adecuadas para ella,
  y la lista de profesores que la podrı́an impartir.

OK - Todas las sesiones de una misma asignatura deben impartirse en la misma aula por el mismo profesor.
OK - Por cada profesor se indican las horas (con un entero entre 1 y 60) en que no puede dar clase.
OK - Por supuesto, no puede haber más de una clase a la vez por profesor ni por aula.

- Cada curso ha de tener un horario compacto (sin horas libres entre dos clases el mismo dı́a)
  y no más de seis horas de clase al dı́a.
OK - Sesiones de un mismo curso no pueden solaparse en el tiempo.

Ayuda: es mejor introducir diversos tipos de variables que relacionan dos entidades
       (profesor-asignatura, asignatura-hora, etc.) que tener variables que relacionan más de dos.
Hay que mostrar la solución por horas (cada hora qué hay) y despúes de cada curso (qué clases tiene).
    miEj: hora 1 -> curso1-assignatura1-aula1-profesor-1 // curso2-...
*/

%ENTRADA

numCursos(4).
numAssignatures(23).
numAules(3).
numProfes(5).

% Sintaxi: assig(curs,assignatura,hores,llistaAules,llistaProfessors).
assig(1,1,4,[1,2,3],[1,2,3,4,5]).
assig(1,2,2,[2],[3,4]).
assig(1,3,2,[1,3],[1,3,4]).
assig(1,4,2,[1,3],[1,2,3,4,5]).
assig(1,5,3,[2],[1,3]).

assig(2,6,4,[3],[2]).
assig(2,7,2,[1,3],[4]).
assig(2,8,3,[1,2,3],[2,3,4,5]).
assig(2,9,3,[1,3],[1,2,3,4,5]).
assig(2,10,4,[2],[1,2,3,5]).

assig(3,11,2,[1,2,3],[1,4]).
assig(3,12,3,[2,3],[1,2,3,4]).
assig(3,13,4,[2],[2,5]).
assig(3,14,3,[1],[1,2,3,4]).
assig(3,15,2,[1,2,3],[1,2,4,5]).
assig(3,16,4,[2],[1,3]).

assig(4,17,4,[1,2,3],[2]).
assig(4,18,2,[1,3],[2]).
assig(4,19,2,[1,3],[1,3]).
assig(4,20,4,[1,2,3],[2,3,4,5]).
assig(4,21,3,[1,2,3],[1,3]).
assig(4,22,4,[3],[1,3,4,5]).
assig(4,23,4,[2,3],[2]).

% Sintaxi: horesProhibides(professor,llistaHores).
horesProhibides(1,[5,8,9,14,21,22,24,26,30,31,41,55,56]).
horesProhibides(2,[5,7,8,9,12,15,19,20,25,26,27,29,31,33,37,38,42,46,50,55,58,60]).
horesProhibides(3,[10,14,20,23,26,32,37,39,40,43,44,53,55]).
horesProhibides(4,[2,7,14,16,19,23,24,31,32,34,40,42,45,47,48,49,50,51,53,54,56,58,59]).
horesProhibides(5,[7,10,11,12,15,20,22,25,27,34,37,39,41,42,46,49,51,52,60]).



%%%%%% Some helpful definitions to make the code cleaner:

assignatura(As):-numAssignatures(N), between(1,N,As).
aula(Au):-numAules(N), between(1,N,Au).
profe(P):-numProfes(N), between(1,N,P).
hora(H):-between(1,60,H). %¿¿¿Mejor 1 - 12???
curso(C):-numCursos(N), between(1,N,C). %year == curso
claseAssig(As,L):-assig(_,As,N,_,_), between(1,N,L).
dia(D):- between(0,4,D).


assigAula(As,Au):- assignatura(As), aula(Au), assig(_,As,_,LAu,_), member(Au,LAu).
assigProfe(As,P):- assignatura(As), profe(P), assig(_,As,_,_,LP), member(P,LP).
horaProhibidaProfe(P,H):- profe(P), horesProhibides(P,LH), member(H,LH).
assigCurso(As,C):- assignatura(As), curso(C), assig(C,As,_,_,_).

%¿¿se usa??
assigNHora(As,NH):- assignatura(As), assig(_,As,NH,_,_).



%%%%%%  1. SAT Variables:

%¿¿¿Hace falta variable con Assig curso???
satVariable( ap(As,P) ):- assignatura(As), profe(P). % ct(C,T)
satVariable( aa(As,Au)):- assignatura(As), aula(Au). % cr(C,R)
% cls(C,L,S) la classe número L de l'assignatura C s'imparte a l'slot S
satVariable( alh(As,L,H) ):- assignatura(As), claseAssig(As,L), hora(H).

satVariable( ch(C,H) ):- curso(C), hora(H).
satVariable( nch(C,H) ):- curso(C), hora(H).





%%%%%%  2. Clause generation:sssss

writeClauses:-
    unaAssignaturaAula,
    unaAssignaturaProfe,
    horaProhibidaProfe,
    mama,
    maxUnProfeHora, %MUCHAS CLAUSULAS
    maxUnaAulaHora,

    maxUnaAssigDia,

    solapaCurso,
    
    horarioCompacto,

    assigHoraCursoHora,
    noAssigHoraNoCursoHora,

    maxAssigCursoDia,

    
    true,!.                    % this way you can comment out ANY previous line of writeClauses
writeClauses:- told, nl, write('writeClauses failed!'), nl,nl, halt.

% Sintaxi: assig(curs,assignatura,hores,llistaAules,llistaProfessors).

mama:- assignatura(As), claseAssig(As,L), findall( alh(As,L,H), hora(H), Lits ), exactly(1,Lits), fail.
mama.

unaAssignaturaAula:- assignatura(As), findall( aa(As,Au), (assigAula(As,Au)), Lits ), exactly(1,Lits), fail.
unaAssignaturaAula.

unaAssignaturaProfe:- assignatura(As), findall( ap(As,P), (assigProfe(As,P)), Lits ), exactly(1,Lits), fail.
unaAssignaturaProfe.

horaProhibidaProfe:- profe(P), horaProhibidaProfe(P,H), assigProfe(As,P), claseAssig(As,L),
    writeClause([-alh(As,L,H), -ap(As,P)]), fail.
horaProhibidaProfe.

maxUnProfeHora:- profe(P), hora(H), 
    assigProfe(As,P), assigProfe(As2,P), As \= As2,
    claseAssig(As,L), claseAssig(As2,L2),
    writeClause([-alh(As,L,H), -alh(As2,L2,H), -ap(As,P), -ap(As2,P) ]), fail.
maxUnProfeHora.

maxUnaAulaHora:- aula(Au), hora(H), %%no hace falta aula(Au)???
    assigAula(As,Au), assigAula(As2,Au), As \= As2,
    claseAssig(As,L), claseAssig(As2,L2),
    writeClause([-alh(As,L,H), -alh(As2,L2,H), -aa(As,Au), -aa(As2,Au) ]), fail.
maxUnaAulaHora.

maxUnaAssigDia:- dia(D), assignatura(As), %¿Si pongo claseAssig aqui???
    findall( alh(As,L,H), (claseAssig(As,L), hora(H), A is D*12+1, B is A+11, between(A,B,H)), Lits ), 
    atMost(1,Lits), fail.
maxUnaAssigDia.

solapaCurso:- curso(C), hora(H),
    findall( alh(As,L,H), (assigCurso(As,C), claseAssig(As,L)) , Lits ), 
    atMost(1,Lits), fail.
solapaCurso.




assigHoraCursoHora:- curso(C), hora(H), assigCurso(As,C), claseAssig(As,L), 
    writeClause([ -alh(As,L,H), ch(C,H) ]), fail. 
    %writeClause([ -ch(C,H),-alh(As,L,H) ]), fail.
assigHoraCursoHora.

noAssigHoraNoCursoHora:- curso(C), hora(H),
    findall(alh(As,L,H), (assigCurso(As,C), claseAssig(As,L)), Lits),
    append([-ch(C,H)], Lits, TL), writeClause(TL), fail.
noAssigHoraNoCursoHora. 



horarioCompacto:- dia(D), curso(C), 
    A is D*12+1, B is A+11, between(A,B,H),
    H2 is H+1, H3 is H+2, H3 =< B,
    between(H3,B,Ht),
    %assigCurso(As,C), claseAssig(As,L), 
    writeClause([ -ch(C,H), ch(C,H2), -ch(C,Ht) ]), fail.   %esto esta mal
horarioCompacto.

maxAssigCursoDia:- dia(D), curso(C),
    findall( ch(C,H), (hora(H), A is D*12+1, B is A+11, between(A,B,H)), Lits ),
    atMost(6,Lits), fail.
maxAssigCursoDia.


/* horarioCompacto:- dia(D), curso(C), 
    A is D*12+1, B is A+11, between(A,B,H),
    H2 is H+1, H3 is H+2, H3 =< B,
    between(H3,B,Ht),
    assigCurso(As,C), assigCurso(As2,C), assigCurso(As3,C),
    As \= As2, As \= As3,
    As2 \= As3,
    claseAssig(As,L),claseAssig(As2,L2), claseAssig(As3,L3),
    writeClause([ -alh(As,L,H), alh(As2,L2,H2), -alh(As3,L3,Ht) ]), fail.
horarioCompacto. */


/* stfu:- curso(C), hora(H), assigCurso(As,C), claseAssig(As,L), 
    writeClause([ nch(C,H), alh(As,L,H) ]), fail. 
    %writeClause([ -ch(C,H),-alh(As,L,H) ]), fail.
stfu.

epi:- numAssignatures(N),
    findall( ch(C,H), (hora(H),curso(C)) , Lits),
    atMost(N,Lits), fail.
epi. */

/* max6AssigCurso:- dia(D), curso(C), 
    findall( alh(As,L,H), (assigCurso(As,C), claseAssig(As,L)) , Lits ), 
    atMost(1,Lits), fail.
max6AssigCurso. */

/* horarioCompacto:- dia(D), curso(C), 
    A is D*12+1, B is A+11, between(A,B,H),
    H2 is H+1, H3 is H+2, H3 =< B,
    assigCurso(As,C), assigCurso(As2,C), assigCurso(As3,C),
    As \= As2, As \= As3,
    As2 \= As3,
    claseAssig(As,L),claseAssig(As2,L2), claseAssig(As3,L3),
    writeClause([ -alh(As,L,H), alh(As2,L2,H2), -alh(As3,L3,H3) ]), fail.
horarioCompacto. */



%%%%%%  3. DisplaySol: show the solution. Here M contains the literals that are true in the model:


/* displaySol(M):- nl, curso(C), hora(H), member(ch(C,H), M), write(ch(C,H)), nl,
    assigCurso(As,C), claseAssig(As,L), member(alh(As,L,H), M ), write(alh(As,L,H)), nl,
    fail.
displaySol(_):- nl,nl.

displaySol(M):- nl, assignatura(As), nl, member(ap(As,P), M ), write(ap(As,P)), write(' '),
    member(aa(As,Au), M ),write(aa(As,Au)), write(' '),
    member(ah(As,H), M ),write(ah(As,H)), write(' '), fail.
displaySol(_):- nl,nl. */


%AQUI

extraBlank(N):-
    N < 10, !, write(' ').
extraBlank(_).

drawTail(Y, Hour):-
    Hour > 48,
    write('  Curs: '), write(Y), nl.
drawTail(_, _).

drawCell(Y, S, M):-
    member(alh(C,L,S), M),                   %% -------- ADAPTA la SAT variable cls(C,L,S)
        %write(alh(C,L,S)),
        %write(Y),
    assig(Y, C, _, _, _),!,
    write(' '), extraBlank(C), write(C), write(' - '),
    extraBlank(L), write(L),
    write('  ['), member(aa(C,R), M),        %% -------  ADAPTA la SAT variable cr(C,R)
    write('A:'), extraBlank(R), write(' '), write(R), write(']'),
    write('  ['), member(ap(C,T), M),        %% -------  ADAPTA la SAT variable ct(C,T)
    write('P:'), extraBlank(T), write(' '), write(T), write(']'),
    write(' ').
drawCell(_, _, _):-
    write('                           ').

drawRow(Row, _):-
    1 is Row mod 2,
    H is Row // 2 + 8,
    extraBlank(H),
    write(' '), write(H), write(':00 '),
    between(1, 141, _), write('='),
    fail.
drawRow(Row, _):-
    1 is Row mod 2, !, nl.

drawRow(Row, M):-
    curso(Y),
    write('       |'),
    between(0, 4, Day),
    Hour is Row // 2 + Day * 12,
    drawCell(Y, Hour, M),
    write('|'),
    drawTail(Y, Hour),
    fail.
drawRow(_, _).

drawHeader:-
    nl, nl,
    write(' Format de sortida: Assignatura - Hora [A: Aula] [P: Professor]'),
    nl, nl,
    write('                 Dilluns                     Dimarts                     dimecres                     Dijous                    Divendres').

displaySchedule(M):-
    drawHeader, nl,
    between(1, 25, Row),
    drawRow(Row, M),
    fail.

drawHeaderYear(Y):-
    nl, nl,
    write('----------------------------------------------------------------------------------------------------------------------------------------------------'),
    nl,
    write(' Horari del curs '), write(Y),
    nl,
    write(' Format de sortida: Assignatura - Hora [A: Aula] [P: Professor]'),
    nl, nl,
    write('                 Dilluns                     Dimarts                     dimecres                     Dijous                    Divendres').

drawTailYear(Hour):-
    Hour > 48, nl.
drawTailYear(_, _).

drawRowYear(Row, _, _):-
    1 is Row mod 2,
    H is Row // 2 + 8,
    extraBlank(H),
    write(' '), write(H), write(':00 '),
    between(1, 141, _), write('='),
    fail.
drawRowYear(Row, _, _):-
    1 is Row mod 2, !, nl.
drawRowYear(Row, Y, M):-
    write('       |'),
    between(0, 4, Day),
    Hour is Row // 2 + Day * 12,
    drawCell(Y, Hour, M),
    write('|'),
    drawTailYear(Hour),
    fail.
drawRowYear(_, _, _).

displayScheduleYear(Y,M):-
    drawHeaderYear(Y), nl,
    between(1, 25, Row),
    drawRowYear(Row, Y, M),
    fail.

displaySol(M):- displaySchedule(M), fail.
displaySol(M):- curso(Y), displayScheduleYear(Y,M), fail.
displaySol(_).


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
