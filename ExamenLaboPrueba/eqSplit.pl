%% Write a Prolog predicate eqSplit(L,S1,S2) that, given a list of
%% integers L, splits it into two disjoint subsets S1 and S2 such that
%% the sum of the numbers in S1 is equal to the sum of S2. It should
%% behave as follows:
%%
%% ?- eqSplit([1,5,2,3,4,7],S1,S2), write(S1), write('    '), write(S2), nl, fail.
%%
%% [1,5,2,3]    [4,7]
%% [1,3,7]    [5,2,4]
%% [5,2,4]    [1,3,7]
%% [4,7]    [1,5,2,3]



/* subcjto([],[]). 
subcjto([_|C],S) :- subcjto(C,S). 
subcjto([X|C],[X|S]) :- subcjto(C,S). 

eqSplit(L,S1,S2):- 
    subcjto(L,S1), subcjto(L,S2), 
    sum_list(S1,N), sum_list(S2,N),
    append(S1,S2,Aux), subset(L, Aux),
    length(S1,X1), length(S2,X2), length(L,X3), X3 is X1 + X2. */


main:- eqSplit([1,5,2,3,4,7],S1,S2), write(S1), write('    '), write(S2), nl, fail.
main.

eqSplit(L,S1,S2):- subsetRest(L,S1,S2), sum_list(S1,N), sum_list(S2,N).

subsetRest([],[],[]).
subsetRest([X|L],[X|S1],   S2 ):- subsetRest(L,S1,S2).
subsetRest([X|L],   S1, [X|S2]):- subsetRest(L,S1,S2).

