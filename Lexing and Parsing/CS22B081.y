%{
    #include <bits/stdc++.h>
    using namespace std;
	void yyerror(char *);
	int yylex(void);
	char mytext[100];
	char var[100];
  char var2[100];
  char var3[100];
  char check[100];
	extern char *yytext;
    map<string,int> m;
    map<string,int> oned;
    map<string,int> twod;
    set<string> s1;
    set<string> s2;
    extern int yylineno;
%}

%token EQ POW NUMBER FLOAT_NUM IF ELSE WHILE FOR INT FLOAT CHAR COMMA IDENTIFIER STRING
%token COMP OSQRE CSQRE OCURLY CCURLY BTRUE BFALSE VOID RETURN

%union {
	int value;
	char strval[256];
}

%left '+' '-'
%left '*' '/'
%right POW

%%
overall: declaration
| program
| declaration overall 
| program overall
;

program: function OCURLY lines CCURLY

lines: oneline
	| oneline lines
	;

oneline:
    declaration
    | assignment
    | conditional_st
    | loop
    | function_call ';'
    | return
    ;
return_data: base_dec
 | VOID IDENTIFIER
;
parameter_pass: COMMA expression
|COMMA STRING
| parameter_pass COMMA expression
| parameter_pass COMMA STRING
;
parameter_list:
   COMMA base_dec
  | parameter_list COMMA base_dec
  | COMMA array
  | parameter_list COMMA array
  ;
function : return_data  '(' ')'
| return_data  '(' base_dec ')'|
| return_data  '(' base_dec parameter_list ')'
| return_data  '(' array')'
| return_data  '(' array parameter_list ')'
;
function_call : IDENTIFIER  '(' ')' 
| IDENTIFIER  '(' expression ')'  
| IDENTIFIER'(' STRING ')'  
| IDENTIFIER  '(' expression parameter_pass ')'  
| IDENTIFIER  '(' STRING parameter_pass ')'  
;
if_block: IF { s1.clear(); s2.clear(); } '(' condition ')' block 
;
conditional_st:
    if_block
  | if_block ELSE block
  ;
block: oneline
| OCURLY lines CCURLY
| OCURLY CCURLY
;
return : RETURN expression ';'
| RETURN ';'
;
condition: expression COMP expression { 
    // Check if any element of s1 belongs to oned or twod
    for (const auto& elem : s1) {
        if (oned.find(elem) != oned.end() || twod.find(elem) != twod.end()) {
            yyerror("syntax error");
            exit(1);
        }
    }
    // Check if any element of s2 belongs to twod
    for (const auto& elem : s2) {
        if (twod.find(elem) != twod.end()) {
            yyerror("syntax error");
            exit(1);
        }
    }
}
| BTRUE
| BFALSE
;
loop: 
    FOR '(' optional_init ';' { s1.clear(); s2.clear(); } condition ';' optional_update ')' block
  | WHILE '(' { s1.clear(); s2.clear(); } condition ')' block
  ;
optional_init: 
    IDENTIFIER EQ expression 
  | /* empty */   // Allows for an empty initialization
  ;
optional_update:
    IDENTIFIER EQ expression 
  | /* empty */   // Allows for an empty update
  ;
declaration: base_dec extra_dec ';'
    | base_dec ';'
    | array ';'
    | array extra_dec ';'
    ;
array : base_dec OSQRE expression CSQRE {oned[check]++;}
 | base_dec OSQRE expression CSQRE OSQRE expression CSQRE {twod[check]++;}
; 
base_dec : INT IDENTIFIER {strcpy(check, yytext);}
    | FLOAT IDENTIFIER {strcpy(check, yytext);}
    | CHAR IDENTIFIER {strcpy(check, yytext);}
    ;
extra_dec: COMMA IDENTIFIER
| COMMA array_elem
| COMMA array_elem OSQRE expression CSQRE
| extra_dec COMMA IDENTIFIER
| extra_dec COMMA array_elem
| extra_dec COMMA array_elem OSQRE expression CSQRE
;
assignments: IDENTIFIER EQ expression
           | assignments COMMA IDENTIFIER EQ expression
           | array_assign
           | assignments COMMA array_assign
           ;
array_assign: array_elem EQ expression
            | array_elem OSQRE expression CSQRE EQ expression
            /* | array_elem EQ function_call */
            ;
assignment: assignments ';' ;
array_elem : IDENTIFIER{strcpy(var3, mytext);} OSQRE expression CSQRE ;
expression: NUMBER 
	  | IDENTIFIER{ 
          strcpy(var2, mytext); s1.insert(var2); 
      }
    | array_elem {s2.insert(var3); }
    | array_elem OSQRE expression CSQRE
    | function_call
	  | expression '+' expression 
    | expression '-' expression 
	  | expression '*' expression 
	  | expression '/' expression 
    | expression POW expression 
    | '(' expression ')'
    ;

%%

void yyerror(char *s) {
    fprintf(stderr, "%d\n",yylineno);
}

int main(void) {
    yylineno=1;
    yyparse();
    return 0;
}
