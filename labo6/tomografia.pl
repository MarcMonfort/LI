% A matrix which contains zeroes and ones gets "x-rayed" vertically and
% horizontally, giving the total number of ones in each row and column.
% The problem is to reconstruct the contents of the matrix from this
% information. Sample run:
%
%	?- p.
%	    0 0 7 1 6 3 4 5 2 7 0 0
%	 0                         
%	 0                         
%	 8      * * * * * * * *    
%	 2      *             *    
%	 6      *   * * * *   *    
%	 4      *   *     *   *    
%	 5      *   *   * *   *    
%	 3      *   *         *    
%	 7      *   * * * * * *    
%	 0                         
%	 0                         
%	


:- use_module(library(clpfd)).

ejemplo1( [0,0,8,2,6,4,5,3,7,0,0], [0,0,7,1,6,3,4,5,2,7,0,0] ).
ejemplo2( [10,4,8,5,6], [5,3,4,0,5,0,5,2,2,0,1,5,1] ).
ejemplo3( [11,5,4], [3,2,3,1,1,1,1,2,3,2,1] ).


p:-	ejemplo1(RowSums,ColSums),
	% 1. vars + Domains
	% 1a. vars
	length(RowSums,NumRows),
	length(ColSums,NumCols),
	NVars is NumRows*NumCols,
	listVars(NVars,L),  % generate a list of Prolog vars (their names do not matter)
	% 1b. domains
	L ins 0..1,
	% 2. Constraints
	matrixByRows(L, NumCols, MatrixByRows),	% [[A,B,C], [D,E,F], [G,H,I]]
	transpose(MatrixByRows, MatrixByCols),

	declareConstraints(MatrixByRows, RowSums),
	declareConstraints(MatrixByCols, ColSums),

	% 3. Label
	label(L),
	% 4. write
	pretty_print(RowSums,ColSums,MatrixByRows).


listVars(NVars, L):- length(L, NVars).

matrixByRows([], _, []).
matrixByRows(L, NumCols, [Row|MatrixByRows]):-
	length(Row, NumCols),
	append(Row, L1, L),
	matrixByRows(L1, NumCols, MatrixByRows).


declareConstraints([], []).
declareConstraints([Row|Matrix], [S|Sums]):-
	sum(Row, #=, S),
	declareConstraints(Matrix, Sums).


%DISPLAY
pretty_print(_,ColSums,_):- write('     '), member(S,ColSums), writef('%2r ',[S]), fail.
pretty_print(RowSums,_,M):- nl,nth1(N,M,Row), nth1(N,RowSums,S), nl, writef('%3r   ',[S]), member(B,Row), wbit(B), fail.
pretty_print(_,_,_).
wbit(1):- write('*  '),!.
wbit(0):- write('   '),!.
    
