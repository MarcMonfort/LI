symbolicOutput(0).  % set to 1 to see symbolic output only; 0 otherwise.
/*
Problema 2: planificación del horario semanal de la FIB: cinco dı́as de 8 a 20h (12 horas diarias).
En los datos de entrada se indican las listas de aulas, profesores, y cursos que hay.
Todos ellos reciben un natural no nulo como identificador.

Por cada curso se da su lista de asignaturas,
cada una con su número de horas semanales de clase 
-(máximo una al dı́a),
la lista de aulas adecuadas para ella,
y la lista de profesores que la podrı́an impartir.

- Todas las sesiones de una misma asignatura deben impartirse en la misma aula por el mismo profesor.
- Por cada profesor se indican las horas (con un entero entre 1 y 60) en que no puede dar clase.
- Por supuesto, no puede haber más de una clase a la vez por profesor ni por aula.

- Cada curso ha de tener un horario compacto (sin horas libres entre dos clases el mismo dı́a)
- y no más de seis horas de clase al dı́a.
- Sesiones de un mismo curso no pueden solaparse en el tiempo.

Ayuda: es mejor introducir diversos tipos de variables que relacionan dos entidades
       (profesor-asignatura, asignatura-hora, etc.) que tener variables que relacionan más de dos.
Hay que mostrar la solución por horas (cada hora qué hay) y despúes de cada curso (qué clases tiene).
    miEj: hora 1 -> curso1-assignatura1-aula1-profesor-1 // curso2-...
*/

%ENTRADA

% Sintaxi: assig(curs,assignatura,hores,llistaAules,llistaProfessors).
% Ej: assig(1,1,4,[1,2,3],[1,2,3,4,5]).

% Sintaxi: horesProhibides(professor,llistaHores).
% Ej: horesProhibides(1,[5,8,9,14,21,22,24,26,30,31,41,55,56]).


:- include(entradaHoraris5).

%%%%%% Some helpful definitions to make the code cleaner:
year(Y)              :- numCursos(N), between(1,N,Y).       %% AQUESTA ÉS OBLIGATÒRIA
course(C)            :- numAssignatures(N), between(1,N,C).
slot(S)              :- between(1,60,S).
room(R)              :- numAules(N), between(1,N,R).
teacher(T)           :- numProfes(N), between(1,N,T).
day(D)               :- between(1,5,D).

lectureOfCourse(C,L) :- assig(_,C,N,_,_), between(1,N,L).
roomOfCourse(C,R)    :- assig(_,C,_,LR,_), member(R,LR).
teacherOfCourse(C,T) :- assig(_,C,_,_,LT), member(T,LT).
slotOfDay(D,S)       :- slot(S), B is D*12, A is B-11, between(A,B,S).
courseOfYear(Y,C)    :- assig(Y,C,_,_,_).

forbTeacherSlot(T,S) :- horesProhibides(T,LS), member(S,LS). 


%%%%%%  1. SAT Variables:
% cls(C,L,S) la classe número L de l'assignatura C s'imparte a l'slot S
% cr(C,R) l'assignatura C és impartida en l'aula R
% ct(C,T) l'assignatura C és impartida pel professor T
% cs(C,S) la assignatura C s'impareix a l'slot S

satVariable( cls(C,L,S) )   :- course(C), lectureOfCourse(C,L), slot(S).
satVariable( cr(C,R) )      :- course(C), room(R).
satVariable( ct(C,T) )      :- course(C), teacher(T).

satVariable( cs(C,S) )      :- course(C), slot(S).
satVariable( cts(C,T,S) )   :- course(C), teacherOfCourse(C,T), slot(S).
satVariable( crs(C,R,S) )   :- course(C), roomOfCourse(C,R), slot(S).
satVariable( ts(T,S) )      :- teacher(T), slot(S).
satVariable( cd(C,D) )      :- course(C), day(D).
satVariable( ys(Y,S) )      :- year(Y), slot(S).


%%%%%%  2. Clause generation
writeClauses:-
    % one slot per course lecture
    oneSlotPerCourseLecture,
    % at most one lecture of a given course per day
    atMostOneCourseLecturePerDay,

    % one room per course
    oneRoomPerCourse, 
    % one teacher per course
    oneTeacherPerCourse,
    % forbiden teacher slot
    forbiddenTeacherSlot,

    % no overlapping teacher
    noOverlapTeacherSlot,
    % no overlapping room
    noOverlapRoomSlot,
    % no overlapping same year lessons
    noOverlapYear,

    % at most 6 lectures per year course / day 
    atMost6Lectures,
    % compact schedule
    compactSchedule,

    % define variables
    define_CS,
    define_CTS,
    define_CRS,
    define_TS,
    define_CD,
    define_YS,


    true,!.
writeClauses:- told, nl, write('writeClauses failed!'), nl,nl, halt.

% one slot per course lecture
oneSlotPerCourseLecture:- 
    course(C), lectureOfCourse(C,L),
    findall( cls(C,L,S), slot(S), Lits ),
    exactly(1,Lits), fail. 
oneSlotPerCourseLecture.

% at most one lecture of a given course per day
atMostOneCourseLecturePerDay:-
    course(C), day(D),
    findall( cls(C,L,S), (lectureOfCourse(C,L), slotOfDay(D,S)), Lits ),
    atMost(1,Lits), fail.
atMostOneCourseLecturePerDay.

% one room per course
oneRoomPerCourse:- 
    course(C), 
    findall( cr(C,R), roomOfCourse(C,R), Lits ),
    exactly(1, Lits), fail.
oneRoomPerCourse. 

% one teacher per course
oneTeacherPerCourse:-
    course(C),
    findall( ct(C,T), teacherOfCourse(C,T), Lits ),
    exactly(1,Lits), fail.
oneTeacherPerCourse.


% forbiden teacher slot
forbiddenTeacherSlot:-
    forbTeacherSlot(T,S),
    writeClause([-ts(T,S)]), fail.
forbiddenTeacherSlot.


% no overlapping teacher
noOverlapTeacherSlot:-
    teacher(T), slot(S),
    findall( cts(C,T,S), teacherOfCourse(C,T), Lits ),
    atMost(1,Lits), fail.
noOverlapTeacherSlot.

% no overlapping room
noOverlapRoomSlot:-
    room(R), slot(S),
    findall( crs(C,R,S), roomOfCourse(C,R), Lits ),
    atMost(1,Lits), fail.
noOverlapRoomSlot.

% no overlapping same year lessons
noOverlapYear:-
    year(Y), slot(S),
    findall( cs(C,S), courseOfYear(Y,C), Lits ),
    atMost(1,Lits), fail.
noOverlapYear.


% at most 6 lectures per year course / day 
atMost6Lectures:-
    year(Y), day(D),
    findall( cd(C,D), courseOfYear(Y,C), Lits ),
    atMost(6,Lits), fail.
atMost6Lectures.

% compact schedule
compactSchedule:-
    year(Y), day(D),
    slotOfDay(D,S1), S2 is S1+1, slotOfDay(D,S2), 
    slotOfDay(D,SSS), SSS >= S2+1, 
    writeClause([ -ys(Y,S1), ys(Y,S2), -ys(Y,SSS) ]), fail.
compactSchedule.



%%%%%%%%%%%%%%%%%%%%%%%
%%%   DEFINITIONS   %%%
%%%%%%%%%%%%%%%%%%%%%%%

% course-lecture-slot
% course-room
% course-teacher

% course-slot % cls -> cs
define_CS:- 
    course(C), slot(S),
    findall( cls(C,L,S), lectureOfCourse(C,L), Lits ),
    expressOr( cs(C,S), Lits ), fail.
define_CS.

% course-teacher-slot % (ct i cs) -> cts
define_CTS:- 
    course(C), teacherOfCourse(C,T), slot(S),
    expressAnd( cts(C,T,S), [ ct(C,T), cs(C,S) ] ), fail.
define_CTS.

% course-room-slot % (cr i cs) -> crs
define_CRS:- 
    course(C), roomOfCourse(C,R), slot(S),
    expressAnd( crs(C,R,S), [ cr(C,R), cs(C,S) ] ), fail.
define_CRS.

% teacher-slot % (ct i cs) -> ts
define_TS:- 
    course(C), teacherOfCourse(C,T), slot(S),
    writeClause( [ -ct(C,T), -cs(C,S), ts(T,S) ]), fail.
define_TS.

% course-day % (cs) -> cd
define_CD:-
    course(C), day(D),
    findall( cs(C,S), slotOfDay(D,S), Lits ),
    expressOr( cd(C,D), Lits ), fail.
define_CD.

% year-slot % (cs) -> ys
define_YS:-
    year(Y), slot(S),
    findall( cs(C,S), courseOfYear(Y,C), Lits ),
    expressOr( ys(Y,S), Lits ), fail.
define_YS.








%%%%%%%%%%%%  DISPLAY  %%%%%%%%%%%%%%%

/* displaySol(M):- nl, course(C), slot(S), member(cls(C,L,S), M), write(cls(C,L,S)), nl,fail.
displaySol(M):- nl, course(C), member(cs(C,S), M), write(cs(C,S)), nl,fail.
displaySol(_):- nl,nl. */

/* displaySol(M):- nl, teacher(C), member(ts(T,S), M), write(ts(T,S)), nl,fail.
displaySol(_):- nl,nl. */




extraBlank(N):- 
    N < 10, !, write(' ').
extraBlank(_).

drawTail(Y, Hour):-
    Hour > 48, 
    write('  Curs: '), write(Y), nl.
drawTail(_, _).

drawCell(Y, S, M):-
    member(cls(C,L,S), M),                   %% -------- ADAPTA la SAT variable cls(C,L,S)
    assig(Y, C, _, _, _), !,
    write(' '), extraBlank(C), write(C), write(' - '),
    extraBlank(L), write(L), 
    write('  ['), member(cr(C,R), M),        %% -------  ADAPTA la SAT variable cr(C,R)
    write('A:'), extraBlank(R), write(' '), write(R), write(']'),
    write('  ['), member(ct(C,T), M),        %% -------  ADAPTA la SAT variable ct(C,T)
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
    year(Y),
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
displaySol(M):- year(Y), displayScheduleYear(Y,M), fail.
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
