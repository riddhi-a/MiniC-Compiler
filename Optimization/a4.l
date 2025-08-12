%{
    #include "a4.tab.h"
    #include <stdio.h>
    #include <stdlib.h>
    #include <string.h> // Include for strdup
    extern int linecount;
    void yyerror(char *);
    extern char mytext[256];  // Define mytext globally if needed
%}

%%

"=" {return EQ;}
"!" {return NOT;}
[+\-*/] { return *yytext; }
"if" {return IF;}
"goto" {return GOTO;}
"==" {return EEQ;}
"!=" {return NEQ;}
">=" {return GEQ;}
"<=" {return LEQ;}
">" {return GT;}
"<" {return LT;}
":" {return COL;}
"print" {return PRINT;}
[a-zA-Z][a-zA-Z0-9]* { yylval.strval = strdup(yytext);return ID;}
[0-9]+            { yylval.strval = strdup(yytext);return INTLIT; }           
[ \t\r]               { /* ignore whitespace */ }
\n                  { /* ignore newlines */ }
.                   { yyerror("syntax error"); exit(1); }

%%

int yywrap(void) {
    return 1;
}
