%{
   #include "CS22B081.tab.h"
   #include <map>
   #include <stdio.h>
   #include <stdlib.h>
   void yyerror(char *);
	extern char mytext[];
    extern int yylineno;
%}

%%
[0-9]+		{ return NUMBER; }
[0-9]+[.][0-9]+ { return FLOAT_NUM; }
int {return INT;}
float {return FLOAT;}
char {return CHAR;}
, {return COMMA;}
"**"        { return POW;}
[=<>!][=]         {return COMP;}
[<>]  {return COMP;}
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
void         {return VOID;}
return        {return RETURN;}
[a-zA-Z][a-z_0-9A-Z]* { strcpy(mytext, yytext); return IDENTIFIER; }
\"[^\"]*\"  { return STRING;}
[ \t]	    { /* ignore whitespace */ }
\n      { yylineno++; }
.	        { yyerror("syntax error"); exit(1); }
%%

int yywrap(void) {
    return 1;
}
