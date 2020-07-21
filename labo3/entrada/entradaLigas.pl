numEquipos(16).
nofuera(7,10).      %   –   el equipo i no quiere jugar en casa la jornada j,
nofuera(6,10).
nofuera(9,10).
nofuera(10,10).
nofuera(11,10).
nofuera(7,30).
nofuera(6,30).
nofuera(9,30).
nofuera(10,30).
nofuera(11,30).
nocasa(7,1).        %   –   el equipo i no quiere jugar en casa la jornada j,
nocasa(8,1).
nocasa(9,1).
nocasa(10,1).
nocasa(11,1).
nocasa(12,1).
norepes(1,2).       %   –   en las jornadas i e i + 1 no se admiten repeticiones: dos partidos seguidos en casa, o dos partidos seguidos fuera, (todos los equipos)
norepes(2,3).
norepes(28,29).
norepes(29,30).
nopartido(1,2,30).  %   –   el partido i-i 0 (es decir, en casa del equipo i) no debe ser la jornada j
nopartido(1,2,1).
nopartido(1,2,2).
nopartido(1,2,3).
nopartido(1,2,4).
nopartido(1,2,5).
nopartido(1,2,6).
nopartido(1,2,7).
sipartido(2,3,30).  %   –   el partido i-i 0 (es decir, en casa del equipo i) sı́ debe ser la jornada j.
sipartido(4,5,30).
