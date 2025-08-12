a.out: a2.tab.c lex.yy.c
	g++ -o $@ a2.tab.c lex.yy.c -lfl

a2.tab.c a2.tab.h: a2.y
	bison -d a2.y

lex.yy.c: a2.l a2.tab.h
	flex a2.l

clean:
	rm -f lex.yy.c a2.tab.c a2.tab.h a.out

