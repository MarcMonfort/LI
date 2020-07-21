:- use_module(library(clpfd)).

constraints( [X,Y,Z] ):-
    X #< Y,
    Z #= X + Y + 1.

p:- 
	% 1. vars + Doms
	X in 1..10,
	[Y,Z] ins 1..8,

	% 2. constraints
    constraints( [X,Y,Z] ),

	% 3. labeling
	label( [X,Y,Z] ),

	% 4. write
	write( [X,Y,Z] ), nl, fail. % change fail to halt for only 1 answer