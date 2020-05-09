/* . “Misioneros”: buscamos la manera m´as r´apida para tres misioneros y tres can´ıbales de cruzar
un r´ıo, disponiendo de una canoa que puede ser utilizada por 1 o 2 personas (misioneros o can´ıbales),
pero siempre evitando que haya misioneros en minor´ıa en cualquier orilla (por razones obvias). */

%Mira que no haya festin
noDead([[Ma,Ca],[Mb,Cb]]):-
    Ma >= Ca,
    Mb >= Cb.

noDead([[Ma,_],[Mb,Cb]]):-
    Ma == 0,
    Mb >= Cb.

noDead([[Ma,Ca],[Mb,_]]):-
    Mb == 0,
    Ma >= Ca.

%%%%

%De A a B
unPaso([[Ma,Ca],[Mb,Cb],0], [[Ma2,Ca2],[Mb2,Cb2],1] ):-
    between(0,Ma,X),
    between(0,Ca,Y),

    2 >= X+Y,
    1 =< X+Y,

    Ma2 is Ma-X,
    Ca2 is Ca-Y,

    Mb2 is Mb+X,
    Cb2 is Cb+Y,

    noDead([[Ma2,Ca2],[Mb2,Cb2]]).

%De B a A
unPaso([[Ma,Ca],[Mb,Cb],1], [[Ma2,Ca2],[Mb2,Cb2],0] ):-
    between(0,Mb,X),
    between(0,Cb,Y),

    2 >= X+Y,
    1 =< X+Y,

    Ma2 is Ma+X,
    Ca2 is Ca+Y,

    Mb2 is Mb-X,
    Cb2 is Cb-Y,

    noDead([[Ma2,Ca2],[Mb2,Cb2]]).


camino( E,E, C,C ).
camino( EstadoActual, EstadoFinal, CaminoHastaAhora, CaminoTotal ):-
    unPaso( EstadoActual, EstSiguiente ),
    \+member(EstSiguiente,CaminoHastaAhora),
    camino( EstSiguiente, EstadoFinal, [EstSiguiente|CaminoHastaAhora], CaminoTotal ).
    
nat(0).
nat(N):- nat(N1), N is N1+1.

solucionOptima:-
    nat(N), % Buscamos soluci´on de "coste" 0; si no, de 1, etc.
    camino( [[3,3],[0,0],0], [[0,0],[3,3],1], [[[3,3],[0,0],0]], C), % En "misioneros" estado: [[0,0],[0,0]] , y
    length(C,N), % -el coste es la longitud de C.
    write(C).