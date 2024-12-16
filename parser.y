%{

#include <iostream>
#include "nodes.hpp"
#include "output.hpp"
#include "visitor.hpp"

// bison declarations
extern int yylineno;
extern int yylex();

using namespace std;
using namespace ast;

void yyerror(const char*);
enum RelOpType whatRelOpRecieved(string);
enum BinOpType whatBinOpRecieved(string);

// root of the AST, set by the parser and used by other parts of the compiler
shared_ptr<Node> program;

// TODO: Place any additional declarations here
%}



// TODO: Define tokens here
%nonassoc   T_ID
%nonassoc   T_STRING
%nonassoc   T_NUM
%nonassoc   T_NUM_B
%nonassoc   T_COMMA
%nonassoc   T_SC
%nonassoc   T_CONTINUE
%nonassoc   T_BREAK
%nonassoc   T_WHILE
%nonassoc   T_ELSE
%nonassoc   T_IF
%nonassoc   T_RETURN
%nonassoc   T_FALSE
%nonassoc   T_TRUE
%nonassoc   T_BOOL
%nonassoc   T_BYTE
%nonassoc   T_INT
%nonassoc   T_VOID

// TODO: Define precedence and associativity here
%left       T_OR
%left       T_AND
%left       T_RELOP
%left       T_BINOP
%right      T_NOT
%right      T_ASSIGN
%left       T_RBRACE
%left       T_LBRACE
%left       T_RPAREN
%left       T_LPAREN

%%

// %type <node> Program
// %type <funcs> Funcs
// %type <func> FuncDecl

// While reducing the start variable, set the root of the AST
Program: Funcs { program = $1; }

// TODO: Define grammar here
Funcs: { $$ = make_shared<Funcs>(); }
    | FuncDecl Funcs 
        { 
        shared_ptr<Funcs> funcs_ptr = dynamic_pointer_cast<Funcs>($2);
        shared_ptr<FuncDecl> func_ptr = dynamic_pointer_cast<FuncDecl>($1);
        funcs_ptr->push_back(func_ptr);
        $$ = funcs_ptr;
        }
    ;


FuncDecl: RetType T_ID T_LPAREN Formals T_RPAREN T_LBRACE Statements T_RBRACE 
    { 
    shared_ptr<ID> id_ptr = dynamic_pointer_cast<ID>($2);
    shared_ptr<Type> retType_ptr = dynamic_pointer_cast<Type>($1);
    shared_ptr<Formals> formals_ptr = dynamic_pointer_cast<Formals>($4);
    shared_ptr<Statements> statements_ptr = dynamic_pointer_cast<Statements>($7);
    $$ = make_shared<FuncDecl>(id_ptr, retType_ptr, formals_ptr, statements_ptr); 
    }
    ;

RetType: Type { $$ = $1; }
    | T_VOID { $$ = $1; }
    ;

Formals: { $$ = make_shared<Formals>(); }
    | FormalsList { $$ = $1; }
    ;

FormalsList: FormalDecl 
    { 
    shared_ptr<Formal> formalDecl_ptr = dynamic_pointer_cast<Formal>($1);
    $$ = make_shared<Formals>(formalDecl_ptr); 
    }
    | FormalDecl T_COMMA FormalsList 
        { 
        shared_ptr<Formals> formals_ptr = dynamic_pointer_cast<Formals>($3);
        shared_ptr<Formal> formal_ptr = dynamic_pointer_cast<Formal>($1);
        formals_ptr->push_back(formal_ptr);
        $$ = formals_ptr;
        }
    ;

FormalDecl: Type T_ID 
    { 
    shared_ptr<ID> id_ptr = dynamic_pointer_cast<ID>($2);
    shared_ptr<Type> retType_ptr = dynamic_pointer_cast<Type>($1);
    $$ = make_shared<Formal>(id_ptr, retType_ptr); 
    }
    ;

Statements: Statement 
    { 
    shared_ptr<Statement> statement_ptr = dynamic_pointer_cast<Statement>($1);
    $$ = make_shared<Statements>(statement_ptr); 
    }
    | Statements Statement 
        { 
        shared_ptr<Statements> statements_ptr = dynamic_pointer_cast<Statements>($1);
        shared_ptr<Statement> statement_ptr = dynamic_pointer_cast<Statement>($2);
        statements_ptr->push_back(statement_ptr);
        $$ = statements_ptr;
        }
    ;

Statement: T_LBRACE Statements T_RBRACE { $$ = $2; }
    | Type T_ID T_SC 
        { 
            shared_ptr<ID> id_ptr = dynamic_pointer_cast<ID>($2);
            shared_ptr<Type> retType_ptr = dynamic_pointer_cast<Type>($1);
            $$ = make_shared<VarDecl>(id_ptr, retType_ptr); 
        }
    | Type T_ID T_ASSIGN Exp T_SC 
        { 
            shared_ptr<ID> id_ptr = dynamic_pointer_cast<ID>($2);
            shared_ptr<Type> retType_ptr = dynamic_pointer_cast<Type>($1);
            shared_ptr<Exp> exp_ptr = dynamic_pointer_cast<Exp>($4);
            $$ = make_shared<VarDecl>(id_ptr, retType_ptr, exp_ptr); 
        }
    | T_ID T_ASSIGN Exp T_SC 
        { 
            shared_ptr<ID> id_ptr = dynamic_pointer_cast<ID>($1);
            shared_ptr<Exp> exp_ptr = dynamic_pointer_cast<Exp>($3);
            $$ = make_shared<Assign>(id_ptr, exp_ptr); 
        }
    | Call T_SC 
        { 
            $$ = $1; 
        }
    | T_RETURN T_SC 
        { 
            $$ = make_shared<Return>(); 
        }
    | T_RETURN Exp T_SC 
        { 
            shared_ptr<Exp> exp_ptr = dynamic_pointer_cast<Exp>($2);
            $$ = make_shared<Return>(exp_ptr); 
        }
    | T_IF T_LPAREN Exp T_RPAREN Statement 
        { 
            shared_ptr<Exp> exp_ptr = dynamic_pointer_cast<Exp>($3);
            shared_ptr<Statement> statement_ptr = dynamic_pointer_cast<Statement>($5);
            $$ = make_shared<If>(exp_ptr, statement_ptr); 
        }
    | T_IF T_LPAREN Exp T_RPAREN Statement T_ELSE Statement 
        { 
            shared_ptr<Exp> exp_ptr = dynamic_pointer_cast<Exp>($3);
            shared_ptr<Statement> statement_ptr1 = dynamic_pointer_cast<Statement>($5);
            shared_ptr<Statement> statement_ptr2 = dynamic_pointer_cast<Statement>($7);
            $$ = make_shared<If>(exp_ptr, statement_ptr1, statement_ptr2); 
        }
    | T_WHILE T_LPAREN Exp T_RPAREN Statement 
        { 
            shared_ptr<Exp> exp_ptr = dynamic_pointer_cast<Exp>($3);
            shared_ptr<Statement> statement_ptr = dynamic_pointer_cast<Statement>($5);
            $$ = make_shared<While>(exp_ptr, statement_ptr); 
        }
    | T_BREAK T_SC 
        { 
            $$ = make_shared<Break>(); 
        }
    | T_CONTINUE T_SC 
        { 
            $$ = make_shared<Continue>(); 
        }
    ;

Call: T_ID T_LPAREN ExpList T_RPAREN 
        { 
        shared_ptr<ID> id_ptr = dynamic_pointer_cast<ID>($1);
        shared_ptr<ExpList> expList_ptr = dynamic_pointer_cast<ExpList>($3);
        $$ = make_shared<Call>(id_ptr, expList_ptr); 
        }
    | T_ID T_LPAREN T_RPAREN 
        { 
        shared_ptr<ID> id_ptr = dynamic_pointer_cast<ID>($1);
        $$ = make_shared<Call>(id_ptr); 
        }
    ;

ExpList: Exp 
    { 
    shared_ptr<Exp> exp_ptr = dynamic_pointer_cast<Exp>($1);
    $$ = make_shared<ExpList>(exp_ptr); 
    }
    | Exp T_COMMA ExpList 
        { 
        shared_ptr<ExpList> exp_list_ptr = dynamic_pointer_cast<ExpList>($3);
        shared_ptr<Exp> exp_ptr = dynamic_pointer_cast<Exp>($1);
        exp_list_ptr->push_back(exp_ptr);
        $$ = exp_list_ptr;
        }
    ;

Type: T_INT 
    { 
        // shared_ptr<Type> retType_ptr = dynamic_pointer_cast<Type>($1);
        $$ = make_shared<Type>(BuiltInType::INT); 
    }
    | T_BYTE 
        { 
            // shared_ptr<Type> retType_ptr = dynamic_pointer_cast<Type>($1);
            $$ = make_shared<Type>(BuiltInType::BYTE); 
        }
    | T_BOOL 
        { 
            // shared_ptr<Type> retType_ptr = dynamic_pointer_cast<Type>($1);
            $$ = make_shared<Type>(BuiltInType::BOOL); 
        }
    ;

Exp: T_LPAREN Exp T_RPAREN { $$ = $2; }
    | Exp T_BINOP Exp 
        { 
            shared_ptr<Exp> exp_ptr1 = dynamic_pointer_cast<Exp>($1);
            shared_ptr<Exp> exp_ptr2 = dynamic_pointer_cast<Exp>($3);
            
            $$ = make_shared<BinOp>(exp_ptr1, exp_ptr2, whatBinOpRecieved($2->value));
        }
    | T_ID 
        { 
            //shared_ptr<ID> id_ptr = dynamic_pointer_cast<ID>($1);
            $$ = make_shared<ID>($1);
        }
    | Call 
        { $$ = $1; }
    | T_NUM 
        { 
            $$ = make_shared<Num>($1); 
        }
    | T_NUM_B 
        { 
            $$ = make_shared<NumB>($1); 
        }
    | T_STRING 
        { 
            $$ = make_shared<String>($1); 
        }
    | T_TRUE 
        { 
            $$ = make_shared<Bool>(true); 
        }
    | T_FALSE 
        { 
            $$ = make_shared<Bool>(false); 
        }
    | T_NOT Exp 
        { 
            shared_ptr<Exp> exp_ptr = dynamic_pointer_cast<Exp>($2);
            $$ = make_shared<Not>(exp_ptr); 
        }
    | Exp T_AND Exp 
        { 
            shared_ptr<Exp> exp_ptr1 = dynamic_pointer_cast<Exp>($1);
            shared_ptr<Exp> exp_ptr2 = dynamic_pointer_cast<Exp>($3);
            $$ = make_shared<And>(exp_ptr1, exp_ptr2); 
        }
    | Exp T_OR Exp 
        { 
            shared_ptr<Exp> exp_ptr1 = dynamic_pointer_cast<Exp>($1);
            shared_ptr<Exp> exp_ptr2 = dynamic_pointer_cast<Exp>($3);
            $$ = make_shared<Or>(exp_ptr1, exp_ptr2); 
        }
    | Exp T_RELOP Exp 
        { 
            shared_ptr<Exp> exp_ptr1 = dynamic_pointer_cast<Exp>($1);
            shared_ptr<Exp> exp_ptr2 = dynamic_pointer_cast<Exp>($3);
            
            RelOpType relop = dynamic_pointer_cast<RelOpType>($2);
            $$ = make_shared<RelOp>(exp_ptr1, exp_ptr2, relop);
        }
    | T_LPAREN Type T_RPAREN Exp 
        { 
            shared_ptr<Type> type_ptr = dynamic_pointer_cast<Type>($2);
            shared_ptr<Exp> exp_ptr = dynamic_pointer_cast<Exp>($4);
            $$ = make_shared<Cast>(exp_ptr, type_ptr); 
        }
    ;


%%

// TODO: Place any additional code here

void yyerror(const char* msg) {
    cerr << "Syntax error at line " << yylineno << ": " << msg << endl;
}

BinOpType whatBinOpRecieved(string received)
{
    BinOpType retval = 0;
    switch(received)
    {
        case '+':
            retval = ADD;
            break;
        case '-':
            retval = SUB;
            break;
        case '*':
            retval = MUL;
            break;
        case '/':
            retval = DIV;
            break;
        default:
            yyerror("Unknown binary operator");
            break;
    }

    return retval;
}

RelOpType whatRelOpRecieved(string received)
{
    RelOpType retval = 0;
    //string received = string(yytext);
    switch(received)
    {
        case "==":
            retval = EQ;
            break;
        case "!=":
            retval = NQ;
            break;
        case "<":
            retval = LT;
            break;
        case ">":
            retval = GT;
            break;
        case "<=":
            retval = LE;
            break;
        case ">=":
            retval = GE;
            break;
        default:
            yyerror("Unknown relop operator");
            break;
    }

    return retval;
}