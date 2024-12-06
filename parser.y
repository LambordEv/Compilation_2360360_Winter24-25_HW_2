%{

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
%nonassoc RBRACE
%nonassoc LBRACE
%nonassoc RPAREN
%nonassoc LPAREN
%nonassoc COMMA
%nonassoc SC
%nonassoc ASSIGN
%nonassoc BINOP
%nonassoc RELOP
%nonassoc CONTINUE
%nonassoc BREAK
%nonassoc WHILE
%nonassoc ELSE
%nonassoc IF
%nonassoc RETURN
%nonassoc FALSE
%nonassoc TRUE
%nonassoc NOT
%nonassoc OR
%nonassoc AND
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
    | FuncDecl Funcs { $$ = $2; $$->push_back($1); }
    ;

FuncDecl: RetType ID LPAREN Formals RPAREN LBRACE Statements RBRACE 
    { $$ = std::make_shared<ast::FuncDecl>($2, $1, $4, $7); }
    ;

RetType: Type { $$ = $1; }
    | VOID { $$ = /* ??? */ } /* <-------------------------------------------------- ??? */
    ;

Formals: { $$ = std::make_shared<ast::Formals>(); }
    | FormalsList { $$ = $1 }
    ;

FormalsList: FormalDecl { $$ = std::make_shared<ast::Formals>($1); }
    | FormalDecl COMMA FormalsList { $$ = $3; $$->push_back($1); }
    ;

FormalDecl: Type ID { $$ = std::make_shared<ast::Formal>($2, $1); }
    ;

Statements: Statement { $$ = std::make_shared<ast::Statements>($1); }
    | Statements Statement { $$ = $1; $$->push_back($2); }
    ;

Statement: LBRACE Statements RBRACE { $$ = $2 } /* <-------------------------------------------------- ??? */
    | Type ID SC { $$ = std::make_shared<ast::VarDecl>($2, $1); }
    | Type ID ASSIGN Exp SC { $$ = std::make_shared<ast::VarDecl>($2, $1, $3); }
    | ID ASSIGN Exp SC { $$ = std::make_shared<ast::Assign>($1, $3); }
    | Call SC { $$ = $1 } /* <-------------------------------------------------- ??? */
    | RETURN SC { $$ = std::make_shared<ast::Return>(); }
    | RETURN Exp SC { $$ = std::make_shared<ast::Return>($2); }
    | IF LPAREN Exp RPAREN Statement { $$ = std::make_shared<ast::If>($3, $5); }
    | IF LPAREN Exp RPAREN Statement ELSE Statement { $$ = std::make_shared<ast::If>($3, $5, $7); }
    | WHILE LPAREN Exp RPAREN Statement { $$ = std::make_shared<ast::While>($3, $5); }
    | BREAK SC { $$ = std::make_shared<ast::Break>(); }
    | CONTINUE SC { $$ = std::make_shared<ast::Continue>(); }
    ;

Call: ID LPAREN ExpList RPAREN { $$ = std::make_shared<ast::Call>($1, $3); }
    | ID LPAREN RPAREN { $$ = std::make_shared<ast::Call>($1); }
    ;

ExpList: Exp { $$ = std::make_shared<ast::ExpList>($1); }
    | Exp COMMA ExpList { $$ = $3; $$->push_back($1); }
    ;

Type: INT { $$ = std::make_shared<ast::Type>($1); }
    | BYTE { $$ = std::make_shared<ast::Type>($1); }
    | BOOL { $$ = std::make_shared<ast::Type>($1); }
    ;

Exp: LPAREN Exp RPAREN { $$ = $2; } /* <-------------------------------------------------- ??? */
    | Exp BINOP Exp { $$ = std::make_shared<ast::BinOp>($1, $3, $2); }
    | ID { $$ = std::make_shared<ast::ID>($1); }
    | Call { $$ = $1 } /* <-------------------------------------------------- ??? */
    | NUM { $$ = std::make_shared<ast::Num>($1); }
    | NUM_B { $$ = std::make_shared<ast::NumB>($1); }
    | STRING { $$ = std::make_shared<ast::String>($1); }
    | TRUE { $$ = std::make_shared<ast::Bool>($1); }
    | FALSE { $$ = std::make_shared<ast::Bool>($1); }
    | NOT Exp { $$ = std::make_shared<ast::Not>($2); }
    | Exp AND Exp { $$ = std::make_shared<ast::And>($1, $3); }
    | Exp OR Exp { $$ = std::make_shared<ast::Or>($1, $3); }
    | Exp RELOP Exp { $$ = std::make_shared<ast::RelOp>($1, $3, $2); }
    | LPAREN Type RPAREN Exp { $$ = std::make_shared<ast::Cast>($4, $2); }
    ;


%%

// TODO: Place any additional code here

void yyerror(const char* msg) {
    cerr << "Syntax error at line " << yylineno << ": " << msg << endl;
}
