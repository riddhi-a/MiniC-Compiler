%{
   #include "a2.tab.h"
   #include <map>
   #include <stdio.h>
   #include <stdlib.h>
   void yyerror(char *);
	extern char mytext[];
    
%}

%%
[0-9]+		{ yylval.strval=strdup(yytext);return NUMBER; }
int {return INT;}
char {return CHAR;}
"**"        { return POW;}
"<="        { return LEQ; }
"<"         { return LT; }
">="        { return GEQ; }
">"         { return GT; }
"=="        { return EEQ; }
"!="        { return NEQ; }
"&&"        { return AND; }
"\|\|"      { return OR; }   
"!"         { return NOT; }
\'[^\']\' {yylval.strval=strdup(yytext);return CHARLIT;}
"["             {return OSQRE;}
"]"             {return CSQRE;}
"{"             {return OCURLY;}
"}"             {return CCURLY;}
[+\-*/();]	{ return *yytext; }
=            { return EQ; }
if            {return IF;}
while        {return WHILE;}
else          {return ELSE;}
for           {return FOR;}
true          {return BTRUE;}
false         {return BFALSE;}
return        {return RETURN;}
"," {return COMMA;}
[a-zA-Z][a-z_0-9A-Z]* { yylval.strval=strdup(yytext); strcpy(mytext, yytext); return IDENTIFIER; }
"//".*  {}
\"[^\"]*\"  {yylval.strval=strdup(yytext); return STRING;}
[ \t]	    { /* ignore whitespace */ }
\n      { }
.	        { yyerror("syntax error"); exit(1); }
%%

int yywrap(void) {
    return 1;
}
