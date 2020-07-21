/* Un programa escrito en el lenguaje sumbol tiene la siguiente sintaxis:
<programa> --> begin <instrucciones> end
<instrucciones> --> <instruccion>
<instrucciones> --> <instruccion> ; <instrucciones>
<instruccion> --> <variable> = <variable> + <variable>
<instruccion> --> if <variable> = <variable> then <instrucciones> else <instrucciones> endif
<variable>--> x
<variable>--> y
<variable>--> z

Tres ejemplos de programas sumbol:
begin x=x+z end
begin x=x+y; z=z+z; x=y+x end
begin x=y+z; if z=z then x=x+z; y=y+z else z=z+y endif; x=x+z end

Escribe en Prolog un sencillo analizador sint´actico para el lenguaje sumbol, es decir, una cl´ausula programa( P
) que se satisface si la lista de ´atomos P contiene un programa sumbol sint´acticamente correcto, y que falla en
caso contrario. Para ello (es obligatorio), haz corresponder una cl´ausula a cada regla de la gram´atica. 
Ejemplos:
?- programa( [begin, z, =, x, +, y, end] ).
yes
?- programa( [begin, z, =, x, +, y, ;, x, =, z, z, end] ). % aqui falta un "+"
no */

programa(P):- append([begin|Is],[end],P), instrucciones(Is).

instrucciones(Is):- instruccion(Is).
%instrucciones(Is):- append(I,[;|Is2],Is), instruccion(I), instrucciones(Is2).
instrucciones(Is):- append( [I, [;], Is2] ,Is), instruccion(I), instrucciones(Is2). % con append/2

instruccion([V1,=,V2,+,V3]):- variable(V1), variable(V2), variable(V3).
instruccion( I ):- 
    append([if|II],[endif], I), % append( [[if], II, [endif] ], I)  "el mateix amb append/2"
    append([V1,=,V2,then|Is1], [else|Is2], II),
    variable(V1), variable(V2), 
    instrucciones(Is1), instrucciones(Is2).


variable(x).
variable(y).
variable(z).


% usar append([ [1], [2,3] ], L)