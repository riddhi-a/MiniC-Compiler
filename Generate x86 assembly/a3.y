%{
    #include <bits/stdc++.h>
    using namespace std;
    void yyerror(char *);
    int yylex(void);
    char mytext[100];
    extern char *yytext;
    extern int yylineno;
    unordered_map<string,string> varmap;
    unordered_map<string,pair<string,string>> cmpstring;
    set<string> globarr;
    set<string> globint;
    set<string> argarr;
    set<string> locarr;
    string bss="\t.bss\n";
    string datal="\t.data\n";
    string textl="\t.text\n";
    string parampass="";
    struct help {
        string tempvar;
        string code;
        string goelse;
        string fcall;
    };
    int gcount=0;
    int pcount=0;
    bool flag=false;
    map<string,int> glob;
    map<string,int> local;
    
%}
%union {
    int value;
    char* strval;
    struct help* prod;
}
%token  INT CHAR COL LEQ LT GEQ GT EEQ NEQ OSQRE CSQRE NOT FN 
%token EQ IF GOTO CALL RETVAL RETURN COMMA GL
%type<prod> statement statements function functions terminal
%token<strval> NUMBER IDENTIFIER STRING CHARLIT LABEL PARAM
%left EEQ NEQ
%left LT GT LEQ GEQ
%left '+' '-'
%left '*' '/'
%right NOT

%% 
program: global_decls functions
       {
          cout<<bss<<datal<<textl<<$2->code;
       }
       ;
global_decls: global_decls global_decl
            |
            ;
global_decl : GL IDENTIFIER
            {
                bss+=((string)($2))+":\t.space 4\n";
                globint.insert((string)($2));
            }
            | GL IDENTIFIER OSQRE NUMBER CSQRE
            {
                bss+=((string)($2))+":\t.space "+to_string(stoi((string)$4))+"\n";
                globarr.insert((string)($2));
            }
            ;
functions: functions function
         {
            $$=new help();
            $$->code=$1->code+$2->code;
         }
         |
         {
            $$=new help();
            $$->code="";
         }
         ;
function: FN IDENTIFIER IDENTIFIER COL {gcount=0;} statements
          {
             string x=((string)$2);
             $$=new help();
             $$->code="\t.globl "+x+"\n"+x+":\n"+"pushl %ebp\n"+"movl %esp, %ebp\n";
             $$->code+="subl $"+to_string(-1*gcount)+", %esp\n";
             $$->code+=$6->code;
             gcount=0;
             varmap.clear();
             cmpstring.clear();
             argarr.clear();
             locarr.clear();
          }
        ;
statements: statements statement
           {
             $$=new help();
             $$->code=$1->code+$2->code;
           }
           |
           {
              $$=new help();
              $$->code="";
           }
          ;
statement: LABEL COL{$$=new help();$$->code=((string)$1)+":\n";}
         | INT IDENTIFIER 
         {
            $$=new help();
            $$->code="";
            gcount=gcount-4;
            varmap[((string)$2)]=to_string(gcount)+"(%ebp)";
         }
         | CHAR IDENTIFIER OSQRE NUMBER CSQRE
         {
            $$=new help();
            $$->code="";
            gcount=gcount-stoi(((string)$4));
            varmap[((string)$2)]=to_string(gcount)+"(%ebp)";
            locarr.insert(((string)$2));
         }
         | IF '(' IDENTIFIER ')' GOTO LABEL
         {
              $$=new help();
              string x = ((string)$3);
              $$->code="movl "+cmpstring[x].first+", %eax\n";
              $$->code+="cmpl "+cmpstring[x].second+", %eax\n";
              if(varmap[x]=="-3")
              {
                $$->code+="jl "+((string)$6)+"\n";
              }
              else if(varmap[x]=="-2")
              {
                $$->code+="je "+((string)$6)+"\n";
              }
              else if(varmap[x]=="-1")
              {
                $$->code+="jle "+((string)$6)+"\n";
              }
              else if(varmap[x]=="1")
              {
                $$->code+="jge "+((string)$6)+"\n";
              }
              else if(varmap[x]=="2")
              {
                $$->code+="jne "+((string)$6)+"\n";
              }
              else if(varmap[x]=="3")
              {
                $$->code+="jg "+((string)$6)+"\n";
              }
         }
         | GOTO LABEL
         {
            $$=new help();
            $$->code="jmp "+((string)$2)+"\n";
         }
         | IDENTIFIER EQ terminal
         {
            $$=new help();
            string x = ((string)$1);
            if(globint.find(x)!=globint.end() || globarr.find(x)!=globarr.end())
            {
               $$->code=x;
               varmap[$$->code]=$$->code;
            }
            else if(varmap.find(x)==varmap.end())
            {
                gcount=gcount-4;
                varmap[x]=to_string(gcount)+"(%ebp)";
            }
            if(locarr.find($3->code)!=locarr.end())
            {
               $$->code="leal "+varmap[$3->code]+", %ebx\n";
               $$->code+="movl %ebx, "+varmap[x]+"\n";
            }
            else
            {
            $$->code="movl "+varmap[$3->code]+", %ebx\n";
            $$->code+="movl %ebx, "+varmap[x]+"\n";
            }
         }
         | IDENTIFIER EQ terminal '+' terminal
         {
            $$=new help();
            string x = ((string)$1);
            if(globint.find(x)!=globint.end())
            {
               $$->code=x;
               varmap[$$->code]=$$->code;
            }
            else if(varmap.find(x)==varmap.end())
            {
                gcount=gcount-4;
                varmap[x]=to_string(gcount)+"(%ebp)";
            }
            $$->code="movl "+varmap[$3->code]+", %ebx\n";
            $$->code+="addl "+varmap[$5->code]+", %ebx\n";
            $$->code+="movl %ebx, "+varmap[x]+"\n";
         }
         | IDENTIFIER EQ terminal '-' terminal
         {
            $$=new help();
            string x = ((string)$1);
            if(globint.find(x)!=globint.end())
            {
               $$->code=x;
               varmap[$$->code]=$$->code;
            }
            else if(varmap.find(x)==varmap.end())
            {
                gcount=gcount-4;
                varmap[x]=to_string(gcount)+"(%ebp)";
            }
            $$->code="movl "+varmap[$3->code]+", %ebx\n";
            $$->code+="subl "+varmap[$5->code]+", %ebx\n";
            $$->code+="movl %ebx, "+varmap[x]+"\n";
         }
         | IDENTIFIER EQ terminal '*' terminal
         {
            $$=new help();
            string x = ((string)$1);
            if(globint.find(x)!=globint.end())
            {
               $$->code=x;
               varmap[$$->code]=$$->code;
            }
            else if(varmap.find(x)==varmap.end())
            {
                gcount=gcount-4;
                varmap[x]=to_string(gcount)+"(%ebp)";
            }
            $$->code="movl "+varmap[$3->code]+", %ebx\n";
            $$->code+="imull "+varmap[$5->code]+", %ebx\n";
            $$->code+="movl %ebx, "+varmap[x]+"\n";
         }
         | IDENTIFIER EQ terminal '/' terminal
         {
            $$=new help();
            string x = ((string)$1);
            if(globint.find(x)!=globint.end())
            {
               $$->code=x;
               varmap[$$->code]=$$->code;
            }
            else if(varmap.find(x)==varmap.end())
            {
                gcount=gcount-4;
                varmap[x]=to_string(gcount)+"(%ebp)";
            }
            $$->code="movl "+varmap[$3->code]+", %eax\n";
            $$->code+="movl "+varmap[$5->code]+", %ebx\n";
            $$->code+="cdq\n";
            $$->code+="idivl %ebx\n";
            //$$->code+="idivl "+varmap[$5->code]+", %ebx\n";
            $$->code+="movl %eax, "+varmap[x]+"\n";
         }
         | IDENTIFIER EQ terminal EEQ terminal
         {
             $$=new help();
             string x = ((string)$1);
             cmpstring[x]=make_pair(varmap[$3->code],varmap[$5->code]);
             varmap[x]="-2";
             $$->code="";
         }
         | IDENTIFIER EQ NOT terminal
         {
             $$=new help();
             string x = ((string)$1);
             cmpstring[x]=cmpstring[$4->code];
             if(varmap[$4->code]=="-2")
             {
                varmap[x]="2";
             }
             else if(varmap[$4->code]=="2")
             {
                varmap[x]="-2";
             }
             else if(varmap[$4->code]=="-3")
             {
                varmap[x]="3";
             }
             else if(varmap[$4->code]=="3")
             {
                varmap[x]="-3";
             }
             else if(varmap[$4->code]=="-1")
             {
                varmap[x]="1";
             }
             else if(varmap[$4->code]=="1")
             {
                varmap[x]="-1";
             }
             $$->code="";
         }
         | IDENTIFIER EQ terminal LEQ terminal
         {
             $$=new help();
             string x = ((string)$1);
             cmpstring[x]=make_pair(varmap[$3->code],varmap[$5->code]);
             varmap[x]="-1";
             $$->code="";
         }
         | IDENTIFIER EQ terminal GEQ terminal
         {
             $$=new help();
             string x = ((string)$1);
             cmpstring[x]=make_pair(varmap[$3->code],varmap[$5->code]);
             varmap[x]="1";
             $$->code="";
         }
         | IDENTIFIER EQ terminal LT terminal
         {
            $$=new help();
             string x = ((string)$1);
             cmpstring[x]=make_pair(varmap[$3->code],varmap[$5->code]);
             varmap[x]="-3";
             $$->code="";
         }
         | IDENTIFIER EQ terminal GT terminal
         {
             $$=new help();
             string x = ((string)$1);
             cmpstring[x]=make_pair(varmap[$3->code],varmap[$5->code]);
             varmap[x]="3";
             $$->code="";
         }
         | IDENTIFIER EQ terminal NEQ terminal
         {
             $$=new help();
             string x = ((string)$1);
             cmpstring[x]=make_pair(varmap[$3->code],varmap[$5->code]);
             varmap[x]="2";
             $$->code="";
         }
         | IDENTIFIER EQ STRING
         {
           datal+=((string)$1)+":\t.asciz "+((string)$3)+"\n";
           globarr.insert(((string)$1));
           $$=new help();
           $$->code="";
         }
         | IDENTIFIER EQ CHARLIT
         {
           $$=new help();
           string x=((string)$1);
           if(varmap.find(x)==varmap.end())
            {
                gcount=gcount-1;
                varmap[x]=to_string(gcount)+"(%ebp)";
            }
           string y=((string)$3);
           if(y[2]!='0')
           $$->code="movb $"+y+","+varmap[x]+"\n";
           else
           $$->code="movb $0,"+varmap[x]+"\n";
         }
         | IDENTIFIER OSQRE terminal CSQRE EQ IDENTIFIER
         {
           $$=new help();
           string x=((string)$1);
           string y=((string)$6);
           $$->code="movb "+varmap[y]+",%dl\n";
           if(locarr.find(x)!=locarr.end())
           {
              /*size_t pos = varmap[x].find('(');
             string numStr = varmap[x].substr(0, pos); 
             int numOffset = stoi(numStr); 
             $$->code += "movl $" +to_string(numOffset) + ", %ebx\n"; // Load offset into ebx
             $$->code += "addl " + varmap[$3->code] + ", %ebx\n"; // Add the terminal value
             $$->code += "movb %al, (%ebp, %ebx)\n";*/
             $$->code += "leal " +varmap[x] + ", %ecx\n";
             $$->code+="addl "+varmap[$3->code]+ ",%ecx\n";
             $$->code+="movb "+varmap[y]+",%dl\n";
             $$->code+="movb %dl, (%ecx)\n";
           
           }
            else if(globarr.find(x)!=globarr.end())
           {
              /*$$->code+="movl $"+x+",%ebx\n";
              $$->code+="addl "+varmap[$3->code]+",%ebx\n";
              $$->code+="movb %al, (%ebx)\n";*/
              
             $$->code += "leal " +x + ", %ecx\n";
             $$->code+="addl "+varmap[$3->code]+ ",%ecx\n";
             $$->code+="movb "+varmap[y]+",%dl\n";
             $$->code+="movb %dl, (%ecx)\n";
            }
            else
            {
             $$->code += "movl " +varmap[x] + ", %ecx\n";
             $$->code+="addl "+varmap[$3->code]+ ",%ecx\n";
             $$->code+="movb "+varmap[y]+",%dl\n";
             $$->code+="movb %dl, (%ecx)\n";
            }
         }
         | IDENTIFIER EQ IDENTIFIER OSQRE terminal CSQRE 
         {
           $$=new help();
           $$=new help();
           string x=((string)$1);
           if(varmap.find(x)==varmap.end())
            {
                gcount=gcount-1;
                varmap[x]=to_string(gcount)+"(%ebp)";
            }
           string y=((string)$3);
           if(locarr.find(y)!=locarr.end())
           {
             $$->code += "leal " +varmap[y] + ", %ecx\n";
             $$->code+="addl "+varmap[$5->code]+ ",%ecx\n";
             $$->code+="movb (%ecx),%dl\n";
             $$->code+="movb %dl,"+varmap[x]+"\n";
           
           }
            else if(globarr.find(y)!=globarr.end())
           {
             $$->code += "leal " +y + ", %ecx\n";
             $$->code+="addl "+varmap[$5->code]+ ",%ecx\n";
             $$->code+="movb (%ecx),%dl\n";
             $$->code+="movb %dl,"+varmap[x]+"\n";
            }
            else
            {
             $$->code += "movl " +varmap[y] + ", %ecx\n";
             $$->code+="addl "+varmap[$5->code]+ ",%ecx\n";
             $$->code+="movb (%ecx),%dl\n";
             $$->code+="movb %dl,"+varmap[x]+"\n";
            }
           /*if(globarr.find(y)!=globarr.end())
           {
              $$->code="movl $"+y+",%ebx\n";
              $$->code+="addl "+varmap[$5->code]+",%ebx\n";
              $$->code+="movb (%ebx),%al\n";
              $$->code+="movb %al,"+varmap[x]+"\n";
           }
           else
           {
             size_t pos = varmap[y].find('(');
             string numStr = varmap[y].substr(0, pos); 
             int numOffset = stoi(numStr); 
             $$->code += "movl $" +to_string(numOffset) + ", %ebx\n"; // Load offset into ebx
             $$->code += "addl " + varmap[$5->code] + ", %ebx\n"; // Add the terminal value
             $$->code += "movb (%ebp, %ebx),%al\n";
             $$->code+="movb %al,"+varmap[x]+"\n";
           }*/
           
         }
         | RETURN
         {
            $$=new help();
            $$->code="movl %ebp, %esp\npopl %ebp\nret\n";
         }
         | RETVAL EQ terminal
         {
            $$=new help();
            string x = $3->code;
            if(varmap.find(x)==varmap.end())
            {
                gcount=gcount-4;
                varmap[x]=to_string(gcount)+"(%ebp)";
            }
            $$->code="movl\t"+varmap[x]+", %eax\n";
         }
         | IDENTIFIER EQ RETVAL
         {
            $$=new help();
            string x = ((string)$1);
            if(globint.find(x)!=globint.end())
            {
               $$->code=x;
               varmap[$$->code]=$$->code;
            }
            else if(varmap.find(x)==varmap.end())
            {
                gcount=gcount-4;
                varmap[x]=to_string(gcount)+"(%ebp)";
            }
            $$->code="addl\t$"+to_string(4*pcount)+", %esp\n";
            $$->code+="movl\t%eax, "+varmap[x]+"\n";
            pcount=0;
            parampass="";
         }
         | IDENTIFIER EQ PARAM
         {
            $$=new help();
            string x = ((string)$1);
            argarr.insert(x);
            if(varmap.find(x)==varmap.end())
            {
                gcount=gcount-4;
                varmap[x]=to_string(gcount)+"("+"%ebp"+")";
            }
            string y=((string)$3);
            int z=stoi(y.substr(5));
            $$->code="movl\t"+to_string(4+z*4)+"(%ebp), %ebx\n";
            $$->code+="movl\t%ebx, "+varmap[x]+"\n";
         }
         | PARAM EQ IDENTIFIER
         {
            $$=new help();
            string x=((string)$3);
            if(globarr.find(x)!=globarr.end())
            {
                parampass="pushl $"+x+"\n"+parampass;
                $$->code="";
            }
            else if(locarr.find(x)!=locarr.end())
            {
               parampass = "leal "+varmap[x]+",%ecx\n";
               parampass="pushl %ecx\n"+parampass;
               $$->code="";
            }
            else
            {
                parampass="pushl "+varmap[x]+"\n"+parampass;
                $$->code="";
            }
            //$$->code="movl\t"+((string)$3)+", %eax\n";
            //$$->code+="pushl\t%eax\n";
            pcount++;
         }
         | CALL IDENTIFIER
         {
           $$=new help();
           $$->code=parampass+"call "+((string)$2)+"\n";
         }
         ;
terminal: '-' NUMBER
        {
            $$=new help();
            $$->code="-"+((string)$2);
            varmap[$$->code]="$"+$$->code;
        }
        | NUMBER
        {
            $$=new help();
            $$->code=((string)$1);
            varmap[$$->code]="$"+$$->code;
        }
        | IDENTIFIER
        {
            $$=new help();
            string x = ((string)$1);
            if(globint.find(x)!=globint.end())
            {
               $$->code=x;
               varmap[$$->code]=$$->code;
            }
            else if(globarr.find(x)!=globarr.end())
            {
              $$->code=x;
               varmap[$$->code]="$"+$$->code;
             }
            else
            {
            if(varmap.find(x)==varmap.end())
            {
                gcount=gcount-4;
                varmap[x]=to_string(gcount)+"(%ebp)";
            }
            $$->code=x;
            }
        }
        ;
%%

void yyerror(char *s) {
    fprintf(stderr, "syntax error\n");
    exit(1);
}

int main(void) {
    yyparse();
    return 0;
}
