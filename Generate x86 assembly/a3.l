%{
    #include "a3.tab.h"
    #include <stdio.h>
    #include <stdlib.h>
    #include <string.h> // Include for strdup

    void yyerror(char *);
    extern char mytext[256];  // Define mytext globally if needed
%}

%%

[0-9]+              { yylval.strval = strdup(yytext); return NUMBER; }
int                 { return INT; }
char                { return CHAR; }
":"                 { return COL; }
"<="                { return LEQ; }
"<"                 { return LT; }
">="                { return GEQ; }
">"                 { return GT; }
"=="                { return EEQ; }
"!="                { return NEQ; }  
"["                 { return OSQRE; }
"]"                 { return CSQRE; }
"not"               { return NOT; }
".globl"            { return FN; }
"global"            {return GL;}
.L[0-9]+            { yylval.strval = strdup(yytext);return LABEL; }           // Ensure labels start with .L and end with digits
\'([^\']|\\\'|\\0|\\\\|\\n)\'  { yylval.strval = strdup(yytext); return CHARLIT; }
[+\-*/();]          { return *yytext; }
"="                 { return EQ; }
"if"                { return IF; }
"goto"              { return GOTO; }
"param"[0-9]+       { yylval.strval = strdup(yytext);return PARAM; }           // Adjust if needed to capture numbers separately
"call"              { return CALL; }
"retval"            { return RETVAL; }
"return"            { return RETURN; }
","                 { return COMMA; }
[a-zA-Z][a-z_0-9A-Z]* { yylval.strval = strdup(yytext); strcpy(mytext, yytext); return IDENTIFIER; }
\"[^\"]*\"          { yylval.strval = strdup(yytext); return STRING; }
[ \t\r]               { /* ignore whitespace */ }
\n                  { /* ignore newlines */ }
.                   { yyerror("syntax error"); exit(1); }

%%

int yywrap(void) {
    return 1;
}
