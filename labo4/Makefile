file = miSudoku


$(file): $(file).pl
	swipl -q -O -g main --stand_alone=true -o $(file) -c $(file).pl


minColoring:	minColoring.pl
		swipl -q -O -g main --stand_alone=true -o minColoring -c minColoring.pl

gangsters: gangsters.pl
		swipl -q -O -g main --stand_alone=true -o gangsters -c gangsters.pl

banner: banner.pl
		swipl -q -O -g main --stand_alone=true -o banner -c banner.pl

banner_github: banner_github.pl
		swipl -q -O -g main --stand_alone=true -o banner_github -c banner_github.pl