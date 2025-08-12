a.out: a4.tab.c lex.yy.c
	g++ -o $@ a4.tab.c lex.yy.c -lfl

a4.tab.c a4.tab.h: a4.y
	bison -d a4.y

lex.yy.c: a4.l a4.tab.h
	flex a4.l

clean:
	rm -f lex.yy.c a4.tab.c a4.tab.h a.out

