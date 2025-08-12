%{
    #include <bits/stdc++.h>
    using namespace std;
    void yyerror(char *);
    int yylex(void);
    char mytext[100];
    extern char *yytext;
    extern int yylineno;

    struct help {
        string tempvar;
        string code;
        string goelse;
        string fcall;
    };
    int tcount = 1;
    int lcount=1;
    int cparam=1;
    int fparam=1;
    bool flag=false;
    map<string,int> glob;
    map<string,int> local;
    struct ConditionNode {
    string id;         
    ConditionNode* left;    
    ConditionNode* right;   
    ConditionNode* parent;  
    string truepart;  
    string falsepart;  
    string code;       
    string tempvar;
    ConditionNode(string node_id) : id(node_id), left(nullptr), right(nullptr), parent(nullptr), truepart(""), falsepart(""), code(""),tempvar("") {}
    ConditionNode(string node_id, string expr_code,string tempv) : id(node_id), left(nullptr), right(nullptr), parent(nullptr), truepart(""), falsepart(""), code(expr_code),tempvar(tempv) {}
    };
    
    void traverseLCR(ConditionNode* node, vector<ConditionNode*>& leafNodes) {
    if (node==nullptr) return;
    traverseLCR(node->left, leafNodes);
    if (node->id == "expression") {
        leafNodes.push_back(node);
        node->id = "L" + to_string(lcount++);
    }
    traverseLCR(node->right, leafNodes);
    }
    void traverseRCL(ConditionNode* node, ConditionNode*& latestsofar, ConditionNode*& rightsofar, const string& iflab, const string& elselab) {
    if (node==nullptr) return;
    traverseRCL(node->right, latestsofar, rightsofar, iflab, elselab);
    if (node->left==nullptr && node->right==nullptr) {
        if (latestsofar == nullptr) {
            node->truepart = iflab;
            node->falsepart = elselab;
        } else {
            node->truepart = latestsofar->truepart;
            node->falsepart = latestsofar->falsepart;
        }
        rightsofar = node;
    } else {
        if (node->id == "OR") {
                node->truepart = node->right->truepart;
                node->falsepart = rightsofar->id; 
                latestsofar = node;
        } else if (node->id == "AND") {
                node->truepart = rightsofar->id;
                node->falsepart = node->right->falsepart;
                latestsofar = node;
        }
        else if(node->id == "NOT"){
            if(latestsofar!=nullptr)
            {node->truepart = latestsofar->falsepart;
            node->falsepart = latestsofar->truepart;
            latestsofar = node;
            }
        }
    }
    
    traverseRCL(node->left, latestsofar, rightsofar, iflab, elselab);
    if (node->right!=nullptr) {
        node->truepart = node->right->truepart;
        node->falsepart = node->right->falsepart;
    }
    else if(node->left!=nullptr) {
        node->truepart = node->left->falsepart;
        node->falsepart=node->left->truepart;
    }
    latestsofar = node;
}
    vector<string> processConditionTree(ConditionNode* root) {
    vector<ConditionNode*> leafNodes;
    traverseLCR(root,leafNodes);
    string iflab = "L" + to_string(lcount++);
    string elselab = "L" + to_string(lcount++);
    ConditionNode* latestsofar = nullptr;
    ConditionNode* rightsofar = nullptr;
    traverseRCL(root, latestsofar, rightsofar, iflab, elselab);
    string s="";
    for (auto* node : leafNodes) {
        s += node->id + ":\n"+node->code;
        s += "if (" + node->tempvar + ") goto " + node->truepart + "\n";
        s += "goto " + node->falsepart + "\n";  
    }
    vector<string> v;
    v.push_back(root->truepart);
    v.push_back(root->falsepart);
    v.push_back(s);
    return v;
    }

    
%}

%token IF ELSE WHILE FOR INT CHAR COMMA 
%token OSQRE CSQRE OCURLY CCURLY BTRUE BFALSE RETURN 

%union {
    int value;
    char* strval;
    struct help* prod;
    struct ConditionNode* node;
}

%token <strval> NUMBER IDENTIFIER STRING CHARLIT
%type <prod> expression oneline block lines if_block conditional_st assignment optional_assign loop declaration
%type<prod> func_start base_dec parameter_list function func_def return_st func_call_start function_call parameter_pass
%token OR AND NEQ EQ LEQ GEQ LT GT NOT POW EEQ
%type<node> condition
%left OR
%left AND
%left EEQ NEQ
%left LT GT LEQ GEQ
%left '+' '-'
%left '*' '/'
%right POW
%right NOT

%%
program: lines 
        {
            cout<<$1->code;
        }
        ;
lines: oneline { $$=new help();$$->code=$1->code;}
     | function {  $$=new help();$$->code=$1->code;}
     | oneline lines { $$=new help();$$->code=$1->code+$2->code;}
     | function lines { $$=new help();$$->code=$1->code+$2->code;}
     ;
oneline: assignment { $$=new help();$$->code=$1->code;}
       | conditional_st { $$=new help();$$->code=$1->code;}
       | loop { $$=new help();$$->code=$1->code;}
       | return_st {  $$=new help();$$->code=$1->code;}
       | expression ';' { $$=new help();$$->code=$1->code;}
       | declaration {$$=new help();$$->code=$1->code;}
       ;
block: oneline { $$=new help();$$->code=$1->code;}
      | OCURLY lines CCURLY { $$=new help();$$->code=$2->code;}
      | OCURLY CCURLY { $$=new help();$$->code="";}
      ;
base_dec : INT IDENTIFIER {$$=new help();$$->code = ((string)$2)+"= param"+to_string(cparam++)+"\n";local[((string)$2)]=1;}
         | CHAR IDENTIFIER OSQRE CSQRE {$$=new help();$$->code = ((string)$2)+"= param"+to_string(cparam++)+"\n";local[((string)$2)]=2;}
         | CHAR IDENTIFIER OSQRE NUMBER CSQRE {$$=new help();$$->code = ((string)$2)+"= param"+to_string(cparam++)+"\n";local[((string)$2)]=2;}
         ;
declaration : INT IDENTIFIER ';' {$$=new help();if(flag){ local[((string)$2)]=1;$$->code="";} else { glob[((string)$2)]=1;$$->code="global "+((string)$2)+"\n";}}
            | CHAR IDENTIFIER OSQRE NUMBER CSQRE ';' {$$=new help();if(flag){ local[((string)$2)]=1;$$->code="";} else {glob[((string)$2)]=1;$$->code="global "+((string)$2)+"["+((string)$4)+"]"+"\n";}}
            ;
parameter_list: COMMA base_dec {$$=new help();$$->code = $2->code;}
               | parameter_list COMMA base_dec {$$=new help();$$->code = $1->code+$3->code;}
               ;
func_start: INT IDENTIFIER '(' 
          { 
            flag=true;
            cparam=1;
            $$=new help();
            $$->code=((string)$2)+":\n";
          }
          ;
func_def : func_start ')' {$$=new help();$$->code=$1->code;}
         | func_start base_dec ')' {$$=new help();$$->code=$1->code+$2->code;}
         | func_start base_dec parameter_list ')' {$$=new help();$$->code=$1->code+$2->code+$3->code;}
         ;
function: func_def block 
        { $$ = new help();
          $$->code = $1->code + $2->code;
          flag=false;
          local.clear();
        }
        ;
func_call_start :  IDENTIFIER  '(' {fparam=2; $$=new help(); $$->code="call "+((string)$1)+"\n";}
                ;
function_call : func_call_start ')' 
          {$$=new help();$$->code=$1->code;
           string retvar= "t" + to_string(tcount++);
           $$->code+=retvar+"=retval\n";
           $$->tempvar=retvar;}
| func_call_start expression ')' 
 {
            $$ = new help();
            string tempv;
            $$->code=$2->code;
            if ($2->tempvar[0] != 't' || !all_of($2->tempvar.begin() + 1, $2->tempvar.end(), ::isdigit)) {
            tempv = "t" + to_string(tcount++); 
            $$->code += tempv + " = " + $2->tempvar + "\n";
           } 
           else {
           tempv = $2->tempvar;
           }
           $$->code += "param1 =" + tempv + "\n"+$1->code;
           string retvar= "t" + to_string(tcount++);
           $$->code+=retvar+"=retval\n";
           $$->tempvar=retvar;
 }
| func_call_start STRING ')'  
{
            $$ = new help();
            string tempv = "t" + to_string(tcount++); 
            $$->code = tempv + " = " + ((string)$2) + "\n";
           $$->code += "param1 =" + tempv + "\n"+$1->code;
           string retvar= "t" + to_string(tcount++);
           $$->code+=retvar+"=retval\n";
           $$->tempvar=retvar;
 }
| func_call_start expression parameter_pass ')'  
{
            $$ = new help();
            string tempv;
            $$->code=$2->code;
            if ($2->tempvar[0] != 't' || !all_of($2->tempvar.begin() + 1, $2->tempvar.end(), ::isdigit)) {
            tempv = "t" + to_string(tcount++); 
            $$->code += tempv + " = " + $2->tempvar + "\n"+$3->code;
           } 
           else {
           tempv = $2->tempvar;
           $$->code+=$3->code;
           }
           $$->code += "param1 =" + tempv + "\n"+$3->fcall+$1->code;
           string retvar= "t" + to_string(tcount++);
           $$->code+=retvar+"=retval\n";
           $$->tempvar=retvar;
}
| func_call_start STRING parameter_pass ')'  
{
            $$ = new help();
            string tempv = "t" + to_string(tcount++); 
            $$->code = tempv + " = " + ((string)$2) + "\n"+$3->code;
            $$->code += "param1 =" + tempv + "\n"+$3->fcall+$1->code;
            string retvar= "t" + to_string(tcount++);
           $$->code+=retvar+"=retval\n";
           $$->tempvar=retvar;
 }
;
parameter_pass: COMMA expression
{
           $$ = new help();
            string tempv;
            $$->code=$2->code;
            if ($2->tempvar[0] != 't' || !all_of($2->tempvar.begin() + 1, $2->tempvar.end(), ::isdigit)) {
            tempv = "t" + to_string(tcount++); 
            $$->code += tempv + " = " + $2->tempvar + "\n";
           } 
           else {
           tempv = $2->tempvar;
           }
           $$->fcall= "param"+ to_string(fparam++)+" = " + tempv + "\n";
}
|COMMA STRING
{
            $$ = new help();
            string tempv = "t" + to_string(tcount++); 
            $$->code = tempv + " = " + ((string)$2) + "\n";
           $$->fcall= "param"+ to_string(fparam++)+" = " + tempv + "\n";
           
 }
| parameter_pass COMMA expression
{
           $$ = new help();
            string tempv;
            $$->code=$1->code+$3->code;
            if ($3->tempvar[0] != 't' || !all_of($3->tempvar.begin() + 1, $3->tempvar.end(), ::isdigit)) {
            tempv = "t" + to_string(tcount++); 
            $$->code += tempv + " = " + $3->tempvar + "\n";
           } 
           else {
           tempv = $3->tempvar;
           }
           $$->fcall=$1->fcall+"param"+ to_string(fparam++)+" = " + tempv + "\n";
}
| parameter_pass COMMA STRING
{
            $$ = new help();
            $$->code=$1->code;
            string tempv = "t" + to_string(tcount++); 
            $$->code = tempv + " = " + ((string)$3) + "\n";
            $$->fcall=$1->fcall+"param"+ to_string(fparam++)+" = " + tempv + "\n";
 }
;
return_st : RETURN expression ';' 
          {
             $$=new help();
             $$->code=$2->code+"retval="+$2->tempvar+"\nreturn\n";
          }
          ;
loop:  FOR '(' optional_assign ';' condition ';' optional_assign ')' block
    {
        $$=new help();
        if($5->left==nullptr && $5->right==nullptr)
        {
        string top = "L"+ to_string(lcount++);
        string body = "L"+ to_string(lcount++);
        string end = "L"+ to_string(lcount++);
        $$->code = $3->code+top+":\n"+ $5->code+"if (" + $5->tempvar + ") goto "+body +"\n"+"goto "+end+"\n";
        $$->code+= body+":\n"+$9->code+$7->code+"goto "+top+"\n"+end+":\n";
        }
        else
        {
            string top = "L"+ to_string(lcount++);
            vector<string> v = processConditionTree($5);
            string varif = v[0];
            string varelse = v[1];
            $$->code = $3->code+top+":\n"+v[2]+varif+":\n"+$9->code+$7->code+"goto "+top+"\n"+varelse+":\n";
        }
        
    }
    | WHILE '(' condition ')' block
    {
        $$=new help();
        if($3->right==nullptr && $3->left==nullptr)
        {
        string top = "L"+ to_string(lcount++);
        string body = "L"+ to_string(lcount++);
        string end = "L"+ to_string(lcount++);
        $$->code = top+":\n"+ $3->code + "if (" + $3->tempvar + ") goto "+body +"\n"+"goto "+end+"\n";
        $$->code+= body+":\n"+$5->code+"goto "+top+"\n"+end+":\n";
        }
        else
        {
            vector<string> v = processConditionTree($3);
            string varif = v[0];
            string varelse = v[1];
            string top = "L"+ to_string(lcount++);
            $$->code = top+":\n"+v[2]+varif+":\n"+$5->code+"goto "+top+"\n"+varelse+":\n";
        }
        
    }
    ;
optional_assign : IDENTIFIER EQ expression 
                 {
                    if(local[((string)$1)]!=1 && glob[((string)$1)]!=1)
                    {
                     cerr<<("undefined variable "+((string)$1));  
                     exit(1);
                    }
                    $$ = new help();
            string tempv;
            if ($3->tempvar[0] != 't' || !all_of($3->tempvar.begin() + 1, $3->tempvar.end(), ::isdigit)) {
            tempv = "t" + to_string(tcount++); 
            $$->code = $3->code + tempv + " = " + $3->tempvar + "\n";
           } 
           else {
           tempv = $3->tempvar;
           $$->code = $3->code;
           }
           $$->code += string($1) + " = " + tempv + "\n";
                 }
                | {$$=new help();$$->code="";}
                ;
if_block: IF '(' condition ')' block 
        {   
            if($3->right==nullptr && $3->left==nullptr)
            {
            $$=new help();
            string varif = "L"+ to_string(lcount++);
            string varelse = "L"+ to_string(lcount++);
            $$->goelse = varelse;
            $$->code =  $3->code + "if (" + $3->tempvar + ") goto "+varif +"\n"+"goto "+varelse+"\n";
            $$->code+= varif+":\n"+$5->code;
            }
            else
            {
            vector<string> v = processConditionTree($3);
            $$=new help();
            string varif = v[0];
            string varelse = v[1];
            $$->goelse = varelse;
            $$->code =v[2];
            $$->code+= varif+":\n"+$5->code;
            }
           
        }
        ;
conditional_st: if_block 
               {
                  $$=new help();
                  $$->code = $1->code + $1->goelse+":\n";
               }
              | if_block ELSE block
              {
                  $$=new help();
                  string varret = "L"+ to_string(lcount++);
                  $$->code = $1->code +"goto "+varret+"\n"+$1->goelse+":\n"+$3->code+varret+":\n";
              }
              ;
condition:
    condition AND condition
    {
        $$ = new ConditionNode("AND");   
        $$->left = $1;                   
        $$->right = $3;                  
        $1->parent = $$;                
        $3->parent = $$;                
    }
    |
    condition OR condition
    {
        $$ = new ConditionNode("OR");    
        $$->left = $1;                  
        $$->right = $3;                 
        $1->parent = $$;                 
        $3->parent = $$;                 
    }
    |
    '(' condition ')'
    {  
        $$ = $2;                         
    }
    |
    NOT '(' condition ')'
    {
        $$ = new ConditionNode("NOT");
        $$->left = $3; 
        $3->parent = $$;
    }
    |
    expression
    {
        $$ = new ConditionNode("expression", $1->code,$1->tempvar);
    }
    ;
assignment: IDENTIFIER EQ expression ';'
           {
             if(local[((string)$1)]!=1 && glob[((string)$1)]!=1)
                    {
                     cerr<<("undefined variable "+((string)$1));  
                     exit(1);
                    }
             $$ = new help();
            string tempv;
            if ($3->tempvar[0] != 't' || !all_of($3->tempvar.begin() + 1, $3->tempvar.end(), ::isdigit)) {
            tempv = "t" + to_string(tcount++); 
            $$->code = $3->code + tempv + " = " + $3->tempvar + "\n";
           } 
           else {
           tempv = $3->tempvar;
           $$->code = $3->code;
           }
           $$->code += string($1) + " = " + tempv + "\n";
           }
           | IDENTIFIER OSQRE expression CSQRE EQ CHARLIT ';'
           {
               if(local[((string)$1)]!=1 && glob[((string)$1)]!=1)
                    {
                     cerr<<("undefined variable "+((string)$1));  
                     exit(1);
                    }
               $$ = new help();
               string tempv;
            if ($3->tempvar[0] != 't' || !all_of($3->tempvar.begin() + 1, $3->tempvar.end(), ::isdigit)) {
            tempv = "t" + to_string(tcount++); 
            $$->code = $3->code + tempv + " = " + $3->tempvar + "\n";
           } 
           else {
           tempv = $3->tempvar;
           $$->code = $3->code;
           }
           string temp2 = "t"+to_string(tcount++);
           $$->code+=temp2+"="+((string)$6)+"\n";
           $$->code += (string($1)) + "["+tempv+"] = " + temp2 + "\n";
           }
           ;
expression: NUMBER {$$ = new help(); $$->code = ""; $$->tempvar = (string)$1;}
    | '-' NUMBER { $$ = new help();  $$->tempvar = "t" + to_string(tcount++);
    $$->code = $$->tempvar+"= -"+(string)$2+"\n"; }
    | IDENTIFIER {if(local[((string)$1)]!=1 && glob[((string)$1)]!=1)
                    {
                     cerr<<("undefined variable "+((string)$1));  
                     exit(1);
                    }
                    $$ = new help(); $$->code = ""; $$->tempvar = (string)$1;}
    | '-' IDENTIFIER { if(local[((string)$2)]!=1 && glob[((string)$2)]!=1)
                    {
                     cerr<<("undefined variable "+((string)$2));  
                     exit(1);
                    }
                    $$ = new help();  $$->tempvar = "t" + to_string(tcount++);
    $$->code = $$->tempvar+"= -"+(string)$2+"\n"; }
    | function_call {$$ = new help(); $$->code = $1->code; $$->tempvar = $1->tempvar;}
    | expression '+' expression 
    {$$ = new help();
    $$->tempvar = "t" + to_string(tcount++);
    $$->code = $1->code + $3->code + $$->tempvar + "=" + $1->tempvar + "+" + $3->tempvar + "\n";}
    | expression '-' expression 
    {$$ = new help();
    $$->tempvar = "t" + to_string(tcount++);
    $$->code = $1->code + $3->code + $$->tempvar + "=" + $1->tempvar + "-" + $3->tempvar + "\n";}
    | expression '*' expression 
    {$$ = new help();
    $$->tempvar = "t" + to_string(tcount++);
    $$->code = $1->code + $3->code + $$->tempvar + "=" + $1->tempvar + "*" + $3->tempvar + "\n";}
    | expression '/' expression 
    {$$ = new help();
    $$->tempvar = "t" + to_string(tcount++);
    $$->code = $1->code + $3->code + $$->tempvar + "=" + $1->tempvar + "/" + $3->tempvar + "\n";}
    | expression POW expression 
    {$$ = new help();
    $$->tempvar = "t" + to_string(tcount++);
    $$->code = $1->code + $3->code + $$->tempvar + "=" + $1->tempvar + "**" + $3->tempvar + "\n";}
    | '(' expression ')' 
    {$$ = new help();
    $$->tempvar=$2->tempvar;
    $$->code=$2->code;
    }
    | expression LT expression   
    {$$ = new help();
     $$->tempvar = "t" + to_string(tcount++);
     $$->code = $1->code + $3->code + $$->tempvar + "=" + $1->tempvar + "<" + $3->tempvar + "\n";}
    | expression LEQ expression
    {$$ = new help();
     $$->tempvar = "t" + to_string(tcount++);
     $$->code = $1->code + $3->code + $$->tempvar + "=" + $1->tempvar + "<=" + $3->tempvar + "\n";}
    | expression GT expression  
    {$$ = new help();
     $$->tempvar = "t" + to_string(tcount++);
     $$->code = $1->code + $3->code + $$->tempvar + "=" + $1->tempvar + ">" + $3->tempvar + "\n";}
    | expression GEQ expression
    {$$ = new help();
     $$->tempvar = "t" + to_string(tcount++);
     $$->code = $1->code + $3->code + $$->tempvar + "=" + $1->tempvar + ">=" + $3->tempvar + "\n";}
    | expression EEQ expression   
     {$$ = new help();
     $$->tempvar = "t" + to_string(tcount++);
     $$->code = $1->code + $3->code + $$->tempvar + "=" + $1->tempvar + "==" + $3->tempvar + "\n";}
    | expression NEQ expression   
    {$$ = new help();
     $$->tempvar = "t" + to_string(tcount++);
     $$->code = $1->code + $3->code + $$->tempvar + "=" + $1->tempvar + "!=" + $3->tempvar + "\n";}
    | NOT expression
    {
     $$ = new help();
     $$->tempvar = "t" + to_string(tcount++);
     $$->code = $2->code + $$->tempvar + "= not " + $2->tempvar + "\n";
    }

%%

void yyerror(char *s) {
    fprintf(stderr, "syntax error\n");
    exit(1);
}

int main(void) {
    yyparse();
    return 0;
}
