%{
    #include <bits/stdc++.h>
    using namespace std;
    void yyerror(char *);
    int yylex(void);
    char mytext[100];
    extern char *yytext;
    extern int yylineno;
    int linecount=1;
    map<string,int> labline;
    extern FILE *yyin;
    struct help {
        set<string> rhs;
        set<string> lhs;
        string branch;
    };
    struct Node{
        int id;
        set<string> use;
        set<string> def;
        set<string> in;
        set<string> out;
        set<int> next;
        string gotolabel="";
    };
    map<int,Node*> lineToNode;
%}
%union {
    int value;
    char* strval;
    struct help* prod;
}

%token<strval> ID INTLIT 
%token EQ NOT IF GOTO EEQ NEQ LEQ GEQ LT GT COL PRINT
%type<prod> Expression AssignmentStatement UnaryAssignmentStatement ConditionalJump IOStatement UnconditionalJump
%% 
program: program TACLine
       |
       ;
TACLine: AssignmentStatement 
        {
             Node* t = new Node();
             t->id=linecount;
             lineToNode[linecount]=t;
             t->def=$1->lhs;
             t->use=$1->rhs;
             t->next.insert(linecount+1);
             linecount++;
        }
       | UnaryAssignmentStatement
       {
             Node* t = new Node();
             t->id=linecount;
             lineToNode[linecount]=t;
             t->def=$1->lhs;
             t->use=$1->rhs;
             t->next.insert(linecount+1);
             linecount++;
        }
       | ConditionalJump
       {
             Node* t = new Node();
             t->id=linecount;
             lineToNode[linecount]=t;
             t->use=$1->rhs;
             t->gotolabel= $1->branch;
             t->next.insert(linecount+1);
             linecount++;
        }
       | UnconditionalJump
        {
             Node* t = new Node();
             t->id=linecount;
             lineToNode[linecount]=t;
             t->gotolabel= $1->branch;
             linecount++;
        }
       | LabelDefinition
       {
             Node* t = new Node();
             t->id=linecount;
             lineToNode[linecount]=t;
             t->next.insert(linecount+1);
             linecount++;
       }
       | IOStatement
       {
             Node* t = new Node();
             t->id=linecount;
             lineToNode[linecount]=t;
             t->use=$1->rhs;
             t->next.insert(linecount+1);
             linecount++;
       }
       ;
AssignmentStatement : ID EQ Expression
                    {
                        $$=new help();
                        $$->rhs=$3->rhs;
                        $$->lhs.insert((string)$1);
                    }
                    ;
Expression : ID Operator ID
           {
             $$=new help();
             $$->rhs.insert((string)$1);
             $$->rhs.insert((string)$3);
           }
           | ID Operator INTLIT
           {
             $$=new help();
             $$->rhs.insert((string)$1);
           }
           | INTLIT Operator ID
           {
             $$=new help();
             $$->rhs.insert((string)$3);
           }
           | INTLIT Operator INTLIT
           {
             $$=new help();
           }
           | ID
           {
             $$=new help();
             $$->rhs.insert((string)$1);
           }
           | INTLIT
           {
             $$=new help();
           }
           ;
Operator : '+' 
         | '-' 
         | '*'
         | '/'
         ;
UnaryAssignmentStatement :ID EQ UnaryOperator ID
                         {
                            $$=new help();
                            $$->rhs.insert((string)$4);
                            $$->lhs.insert((string)$1);
                         }
                         ;
UnaryOperator : '-'
              | NOT
              ;
ConditionalJump : IF ID ComparisonOperator ID GOTO ID
                {
                    $$=new help();
                    $$->rhs.insert((string)$4);
                    $$->rhs.insert((string)$2);
                    $$->branch=((string)$6);
                }
                ;
ComparisonOperator : EEQ
                   | NEQ 
                   | LT
                   | LEQ
                   | GT 
                   | GEQ
                   ;
UnconditionalJump : GOTO ID 
                {
                    $$=new help();
                    $$->branch=((string)$2);
                }
                  ;
LabelDefinition : ID COL {labline[((string)$1)]=linecount;}
                ;
IOStatement : PRINT ID
            {
                $$=new help();
                $$->rhs.insert((string)$2);
            }
            ;

%%

void yyerror(char *s) {
    fprintf(stderr, "syntax error\n");
    exit(1);
}
string getOutFile(const string &inputFilePath) {
    size_t pos = inputFilePath.find_last_of("/\\");
    string fileName = (pos == string::npos) ? inputFilePath : inputFilePath.substr(pos + 1);
    return fileName;
}
int main(int argc, char *argv[]) {
    if (argc != 3) {
        return 1;
    }
    const char *inputTACPath = argv[1];
    const char *inputQueryPath = argv[2];

    yyin = fopen(inputTACPath, "r");
    if (!yyin) {
        cerr << "Error opening input file: " << inputTACPath << endl;
        return 1;
    }
    yyparse();
    fclose(yyin);
    for (auto &entry : lineToNode) {
        Node* node = entry.second;
        auto labelEntry = labline.find(node->gotolabel);
        if (labelEntry != labline.end()) {
                int targetLine = labelEntry->second;
                node->next.insert(targetLine);
        }
        for (auto it = node->next.begin(); it != node->next.end();) {
            if (lineToNode.find(*it) == lineToNode.end()) {
                it = node->next.erase(it);
            } else {
                ++it;
            }
        }
    }
    bool flag = true;
    while (flag) {
        flag = false;

        for (auto it = lineToNode.rbegin(); it != lineToNode.rend(); ++it) {
            Node* node = it->second;
            set<string> newOut, newIn;

            
            for (int succ : node->next) {
                if (lineToNode.find(succ) != lineToNode.end()) {
                    Node* succNode = lineToNode[succ];
                    newOut.insert(succNode->in.begin(), succNode->in.end());
                }
            }

            // Compute IN(B) = USE(B) ∪ (OUT(B) − DEF(B))
            newIn = node->use;
            set<string> outMinusDef;
            for (const string &var : newOut) {
                if (node->def.find(var) == node->def.end()) {
                    outMinusDef.insert(var);
                }
            }
            newIn.insert(outMinusDef.begin(), outMinusDef.end());

            if (newOut.size() > node->out.size()) {
                flag = true;
                node->out = newOut;
            }
            if (newIn.size() > node->in.size()) {
                flag = true;
                 node->in = newIn;
            }
        }
    }
    string outputFileName = getOutFile(inputTACPath);
    ifstream queryFile(inputQueryPath);
    if (!queryFile) {
        cerr << "Error opening input query file: " << inputQueryPath << endl;
        return 1;
    }
    int queryLine;
    ofstream outputFile(outputFileName);
    while(queryFile >> queryLine)
    {
        if (lineToNode.find(queryLine) != lineToNode.end()) {
        Node *node = lineToNode[queryLine];
        if (node->out.empty()) {
            outputFile << " "; 
        } else {
            for (const auto &var : node->out) {
                outputFile << var << " ";
            }
        }
        outputFile << endl;
       } 
    }
    outputFile.close(); 

    return 0;
}
