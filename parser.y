%{

#include <iostream>
#include "nodes.hpp"
#include "output.hpp"

// bison declarations
extern int yylineno;
extern int yylex();

void yyerror(const char*);

// root of the AST, set by the parser and used by other parts of the compiler
std::shared_ptr<ast::Node> program;

using namespace std;

// TODO: Place any additional declarations here
%}

// TODO: Define tokens here
%nonassoc ID
%nonassoc STRING
%nonassoc NUM
%nonassoc NUM_B
%nonassoc COMMA
%nonassoc SC
%nonassoc CONTINUE
%nonassoc BREAK
%nonassoc WHILE
%nonassoc ELSE
%nonassoc IF
%nonassoc RETURN
%nonassoc FALSE
%nonassoc TRUE
%nonassoc BOOL
%nonassoc BYTE
%nonassoc INT
%nonassoc VOID


// TODO: Define precedence and associativity here
%left OR
%left AND
%left RELOP
%left BINOP
%right NOT
%right ASSIGN
%left RBRACE
%left LBRACE
%left RPAREN
%left LPAREN

%%

// While reducing the start variable, set the root of the AST
Program:  Funcs { program = $1; };

// TODO: Define grammar here
Funcs: { $$ = std::make_shared<ast::Funcs>(); }
    | FuncDecl Funcs 
        { 
        std::shared_ptr<ast::Funcs> funcs_ptr = std::dynamic_pointer_cast<ast::Funcs>($2);
        std::shared_ptr<ast::FuncDecl> func_ptr = std::dynamic_pointer_cast<ast::FuncDecl>($1);
        funcs_ptr->push_back(func_ptr);
        $$ = funcs_ptr;
        }
    ;

FuncDecl: RetType ID LPAREN Formals RPAREN LBRACE Statements RBRACE 
    { 
    std::shared_ptr<ast::ID> id_ptr = std::dynamic_pointer_cast<ast::ID>($2);
    std::shared_ptr<ast::Type> retType_ptr = std::dynamic_pointer_cast<ast::Type>($1);
    std::shared_ptr<ast::Formals> formals_ptr = std::dynamic_pointer_cast<ast::Formals>($4);
    std::shared_ptr<ast::Statements> statements_ptr = std::dynamic_pointer_cast<ast::Statements>($7);
    $$ = std::make_shared<ast::FuncDecl>(id_ptr, retType_ptr, formals_ptr, statements_ptr); 
    }
    ;

RetType: Type { $$ = $1; }
    | VOID { $$ = $1; }
    ;

Formals: { $$ = std::make_shared<ast::Formals>(); }
    | FormalsList { $$ = $1; }
    ;

FormalsList: FormalDecl 
    { 
    std::shared_ptr<ast::Formal> formalDecl_ptr = std::dynamic_pointer_cast<ast::Formal>($1);
    $$ = std::make_shared<ast::Formals>(formalDecl_ptr); 
    }
    | FormalDecl COMMA FormalsList 
        { 
        std::shared_ptr<ast::Formals> formals_ptr = std::dynamic_pointer_cast<ast::Formals>($3);
        std::shared_ptr<ast::Formal> formal_ptr = std::dynamic_pointer_cast<ast::Formal>($1);
        formals_ptr->push_back(formal_ptr);
        $$ = formals_ptr;
        }
    ;

FormalDecl: Type ID 
    { 
    std::shared_ptr<ast::ID> id_ptr = std::dynamic_pointer_cast<ast::ID>($2);
    std::shared_ptr<ast::Type> retType_ptr = std::dynamic_pointer_cast<ast::Type>($1);
    $$ = std::make_shared<ast::Formal>(id_ptr, retType_ptr); 
    }
    ;

Statements: Statement 
    { 
    std::shared_ptr<ast::Statement> statement_ptr = std::dynamic_pointer_cast<ast::Statement>($1);
    $$ = std::make_shared<ast::Statements>(statement_ptr); 
    }
    | Statements Statement 
        { 
        std::shared_ptr<ast::Statements> statements_ptr = std::dynamic_pointer_cast<ast::Statements>($1);
        std::shared_ptr<ast::Statement> statement_ptr = std::dynamic_pointer_cast<ast::Statement>($2);
        statements_ptr->push_back(statement_ptr);
        $$ = statements_ptr;
        }
    ;

Statement: LBRACE Statements RBRACE { $$ = $2; }
    | Type ID SC 
        { 
            std::shared_ptr<ast::ID> id_ptr = std::dynamic_pointer_cast<ast::ID>($2);
            std::shared_ptr<ast::Type> retType_ptr = std::dynamic_pointer_cast<ast::Type>($1);
            $$ = std::make_shared<ast::VarDecl>(id_ptr, retType_ptr); 
        }
    | Type ID ASSIGN Exp SC 
        { 
            std::shared_ptr<ast::ID> id_ptr = std::dynamic_pointer_cast<ast::ID>($2);
            std::shared_ptr<ast::Type> retType_ptr = std::dynamic_pointer_cast<ast::Type>($1);
            std::shared_ptr<ast::Exp> exp_ptr = std::dynamic_pointer_cast<ast::Exp>($4);
            $$ = std::make_shared<ast::VarDecl>(id_ptr, retType_ptr, exp_ptr); 
        }
    | ID ASSIGN Exp SC 
        { 
            std::shared_ptr<ast::ID> id_ptr = std::dynamic_pointer_cast<ast::ID>($1);
            std::shared_ptr<ast::Exp> exp_ptr = std::dynamic_pointer_cast<ast::Exp>($3);
            $$ = std::make_shared<ast::Assign>(id_ptr, exp_ptr); 
        }
    | Call SC 
        { 
            $$ = $1; 
        }
    | RETURN SC 
        { 
            $$ = std::make_shared<ast::Return>(); 
        }
    | RETURN Exp SC 
        { 
            std::shared_ptr<ast::Exp> exp_ptr = std::dynamic_pointer_cast<ast::Exp>($2);
            $$ = std::make_shared<ast::Return>(exp_ptr); 
        }
    | IF LPAREN Exp RPAREN Statement 
        { 
            std::shared_ptr<ast::Exp> exp_ptr = std::dynamic_pointer_cast<ast::Exp>($3);
            std::shared_ptr<ast::Statement> statement_ptr = std::dynamic_pointer_cast<ast::Statement>($5);
            $$ = std::make_shared<ast::If>(exp_ptr, statement_ptr); 
        }
    | IF LPAREN Exp RPAREN Statement ELSE Statement 
        { 
            std::shared_ptr<ast::Exp> exp_ptr = std::dynamic_pointer_cast<ast::Exp>($3);
            std::shared_ptr<ast::Statement> statement_ptr1 = std::dynamic_pointer_cast<ast::Statement>($5);
            std::shared_ptr<ast::Statement> statement_ptr2 = std::dynamic_pointer_cast<ast::Statement>($7);
            $$ = std::make_shared<ast::If>(exp_ptr, statement_ptr1, statement_ptr2); 
        }
    | WHILE LPAREN Exp RPAREN Statement 
        { 
            std::shared_ptr<ast::Exp> exp_ptr = std::dynamic_pointer_cast<ast::Exp>($3);
            std::shared_ptr<ast::Statement> statement_ptr = std::dynamic_pointer_cast<ast::Statement>($5);
            $$ = std::make_shared<ast::While>(exp_ptr, statement_ptr); 
        }
    | BREAK SC 
        { 
            $$ = std::make_shared<ast::Break>(); 
        }
    | CONTINUE SC 
        { 
            $$ = std::make_shared<ast::Continue>(); 
        }
    ;

Call: ID LPAREN ExpList RPAREN 
        { 
        std::shared_ptr<ast::ID> id_ptr = std::dynamic_pointer_cast<ast::ID>($1);
        std::shared_ptr<ast::ExpList> expList_ptr = std::dynamic_pointer_cast<ast::ExpList>($3);
        $$ = std::make_shared<ast::Call>(id_ptr, expList_ptr); 
        }
    | ID LPAREN RPAREN 
        { 
        std::shared_ptr<ast::ID> id_ptr = std::dynamic_pointer_cast<ast::ID>($1);
        $$ = std::make_shared<ast::Call>(id_ptr); 
        }
    ;

ExpList: Exp 
    { 
    std::shared_ptr<ast::Exp> exp_ptr = std::dynamic_pointer_cast<ast::Exp>($1);
    $$ = std::make_shared<ast::ExpList>(exp_ptr); 
    }
    | Exp COMMA ExpList 
        { 
        std::shared_ptr<ast::ExpList> exp_list_ptr = std::dynamic_pointer_cast<ast::ExpList>($3);
        std::shared_ptr<ast::Exp> exp_ptr = std::dynamic_pointer_cast<ast::Exp>($1);
        exp_list_ptr->push_back(exp_ptr);
        $$ = exp_list_ptr;
        }
    ;

Type: INT 
    { 
        // std::shared_ptr<ast::Type> retType_ptr = std::dynamic_pointer_cast<ast::Type>($1);
        $$ = std::make_shared<ast::Type>(ast::BuiltInType::INT); 
    }
    | BYTE 
        { 
            // std::shared_ptr<ast::Type> retType_ptr = std::dynamic_pointer_cast<ast::Type>($1);
            $$ = std::make_shared<ast::Type>(ast::BuiltInType::BYTE); 
        }
    | BOOL 
        { 
            // std::shared_ptr<ast::Type> retType_ptr = std::dynamic_pointer_cast<ast::Type>($1);
            $$ = std::make_shared<ast::Type>(ast::BuiltInType::BOOL); 
        }
    ;

Exp: LPAREN Exp RPAREN { $$ = $2; }
    | Exp BINOP Exp 
        { 
            /*std::shared_ptr<ast::Exp> exp_ptr1 = std::dynamic_pointer_cast<ast::Exp>($1);
            std::shared_ptr<ast::Exp> exp_ptr2 = std::dynamic_pointer_cast<ast::Exp>($3);
             char op = yytext[0];
            ast::BinOpType binop_type;
            switch (op) {  // $2 should be the operator character, e.g., '+', '-', '*', '/'
                case '+':
                    binop_type = ast::BinOpType::ADD;
                    break;
                case '-':
                    binop_type = ast::BinOpType::SUB;
                    break;
                case '*':
                    binop_type = ast::BinOpType::MUL;
                    break;
                case '/':
                    binop_type = ast::BinOpType::DIV;
                    break;
                default:
                    yyerror("Unknown binary operator");
                    YYERROR;
            }
            $$ = std::make_shared<ast::BinOp>(exp_ptr1, exp_ptr2, binop_type); */
        }
    | ID 
        { 
            /*std::shared_ptr<ast::ID> id_ptr = std::dynamic_pointer_cast<ast::ID>($1);
            $$ = std::make_shared<ast::ID>(id_ptr); */
        }
    | Call 
        { $$ = $1; }
    | NUM 
        { 
            // $$ = std::make_shared<ast::Num>($1); 
        }
    | NUM_B 
        { 
            // $$ = std::make_shared<ast::NumB>($1); 
        }
    | STRING 
        { 
            // $$ = std::make_shared<ast::String>($1); 
        }
    | TRUE 
        { 
            $$ = std::make_shared<ast::Bool>(true); 
        }
    | FALSE 
        { 
            $$ = std::make_shared<ast::Bool>(false); 
        }
    | NOT Exp 
        { 
            std::shared_ptr<ast::Exp> exp_ptr = std::dynamic_pointer_cast<ast::Exp>($2);
            $$ = std::make_shared<ast::Not>(exp_ptr); 
        }
    | Exp AND Exp 
        { 
            std::shared_ptr<ast::Exp> exp_ptr1 = std::dynamic_pointer_cast<ast::Exp>($1);
            std::shared_ptr<ast::Exp> exp_ptr2 = std::dynamic_pointer_cast<ast::Exp>($3);
            $$ = std::make_shared<ast::And>(exp_ptr1, exp_ptr2); 
        }
    | Exp OR Exp 
        { 
            std::shared_ptr<ast::Exp> exp_ptr1 = std::dynamic_pointer_cast<ast::Exp>($1);
            std::shared_ptr<ast::Exp> exp_ptr2 = std::dynamic_pointer_cast<ast::Exp>($3);
            $$ = std::make_shared<ast::Or>(exp_ptr1, exp_ptr2); 
        }
    | Exp RELOP Exp 
        { 
            // $$ = std::make_shared<ast::RelOp>($1, $3, $2); 
        }
    | LPAREN Type RPAREN Exp 
        { 
            std::shared_ptr<ast::Type> type_ptr = std::dynamic_pointer_cast<ast::Type>($2);
            std::shared_ptr<ast::Exp> exp_ptr = std::dynamic_pointer_cast<ast::Exp>($4);
            $$ = std::make_shared<ast::Cast>(exp_ptr, type_ptr); 
        }
    ;


%%

// TODO: Place any additional code here

void yyerror(const char* msg) {
    cerr << "Syntax error at line " << yylineno << ": " << msg << endl;
}
