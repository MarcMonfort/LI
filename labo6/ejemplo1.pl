:- use_module(library(clpfd)).

p:- 
	% 1. vars + Doms
	X in 1..10,
	[Y,Z] ins 1..8,

	% 2. constraints
	X #< Y,
	Z #= X + Y + 1,

	% 3. labeling
	label( [X,Y,Z] ),

	% 4. write
	write( [X,Y,Z] ), nl, fail. % change fail to halt for only 1 answer