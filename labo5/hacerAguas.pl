/* “Hacer Aguas”: disponemos de un grifo de agua, un cubo de 5 litros y otro de 8 litros. Se
puede verter el contenido de un cubo en otro, llenar un cubo, o vaciar un cubo del todo, y queremos
saber la secuencia m´ınima de operaciones para obtener exactamente 4 litros de agua en el cubo de 8
litros. */

unPaso([_,B],[5,B]).    %llenar cubo
unPaso([A,_],[A,8]).

unPaso([_,B],[0,B]).    %vaciar cubo
unPaso([A,_],[A,0]).

unPaso([A,B],[A2,B2]):- Left is 8-B, Left > 0, Fill is min(Left,A), B2 is B + Fill, A2 is A - Fill. 
unPaso([A,B],[A2,B2]):- Left is 5-A, Left > 0, Fill is min(Left,B), A2 is A + Fill, B2 is B - Fill.




camino( E,E, C,C ).
camino( EstadoActual, EstadoFinal, CaminoHastaAhora, CaminoTotal ):-
    unPaso( EstadoActual, EstSiguiente ),
    \+member(EstSiguiente,CaminoHastaAhora),
    camino( EstSiguiente, EstadoFinal, [EstSiguiente|CaminoHastaAhora], CaminoTotal ).
    
nat(0).
nat(N):- nat(N1), N is N1+1.

solucionOptima:-
    nat(N), % Buscamos soluci´on de "coste" 0; si no, de 1, etc.
    camino([0,0],[0,4],[[0,0]],C), % En "hacer aguas": -un estado es [cubo5,cubo8], y
    length(C,N), % -el coste es la longitud de C.
    write(C).