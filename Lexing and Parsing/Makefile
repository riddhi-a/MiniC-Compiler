a.out: CS22B081.tab.c lex.yy.c
	g++ -o $@ CS22B081.tab.c lex.yy.c -lfl

CS22B081.tab.c CS22B081.tab.h: CS22B081.y
	bison -d CS22B081.y

lex.yy.c: CS22B081.l CS22B081.tab.h
	flex CS22B081.l

clean:
	rm -f lex.yy.c CS22B081.tab.c CS22B081.tab.h a.out

