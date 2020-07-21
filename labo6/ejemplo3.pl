:- use_module(library(clpfd)).

listVars(0,[]):- !.
listVars(N,[_|L]):-
    N1 is N-1, listVars(N1,L).

% X_{n-1} #> 3*X_n+1
constraints([]).
constraints([_]).
constraints([X,Y|L]):-
    X #> 3*Y+1,
    constraints([Y|L]).

% [X_1, X_2, ..., X_{n-1}, X_n]
% X_1 #> 3*X_2 + 1
% X_2 #> 3*X_3 + 1
% ...
% X_{n-1} #> 3*X_n + 1

p:- N = 6,
    % 1. vars + Doms
    % 1a. vars
    listVars(N,LVars),
    % 1b. Doms
    LVars ins 2..1000,
    % 2. constraints
    constraints(LVars),
    % 3. labeling
    label( LVars ),
    % 4. write
    write( LVars ), nl, halt.
    