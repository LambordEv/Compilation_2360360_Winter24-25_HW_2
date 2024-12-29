#ifndef NODES_HPP
#define NODES_HPP

#include <memory>
#include <string>
#include <vector>
#include "visitor.hpp"

namespace ast {

    /* Arithmetic operations */
    enum BinOpType {
        BIN_ERROR = -1,
        ADD, // Addition
        SUB, // Subtraction
        MUL, // Multiplication
        DIV  // Division
    };

    /* Relational operations */
    enum RelOpType {
        REL_ERROR = -1,
        EQ, // Equal
        NE, // Not equal
        LT, // Less than
        GT, // Greater than
        LE, // Less than or equal
        GE  // Greater than or equal
    };

    /* Built-in types */
    enum BuiltInType {
        TYPE_ERROR = -1,
        VOID,
        BOOL,
        BYTE,
        INT,
        STRING
    };

    /* Base class for all AST nodes */
    class Node {
    public:
        // Line number in the source code
        int line;
        std::string text;

        // Use this constructor only while parsing in bison or flex
        Node();
        int getLine() const { return line; }
        // Accept method for visitor pattern
        virtual void accept(Visitor &visitor) = 0;
    };

    /* Base class for all expressions */
    class Exp : virtual public Node {
    public:
        Exp() = default;
        virtual BuiltInType getType() { return BuiltInType::TYPE_ERROR; }
    };

    /* Base class for all statements */
    class Statement : virtual public Node {
    };

    /* Number literal */
    class Num : public Exp {
    public:
        // Value of the number
        int value;

        // Constructor that receives a C-style string that represents the number
        explicit Num(std::string str);
        int getValue() const { return value; }
        BuiltInType getType() override { return BuiltInType::INT; }
        void accept(Visitor &visitor) override {
            visitor.visit(*this);
        }
    };

    /* Byte literal */
    class NumB : public Exp {
    public:
        // Value of the number
        int value;

        // Constructor that receives a C-style (including b character) string that represents the number
        explicit NumB(std::string str);
        int getValue() const { return value; }
        BuiltInType getType() override { return BuiltInType::BYTE; }
        void accept(Visitor &visitor) override {
            visitor.visit(*this);
        }
    };

    /* String literal */
    class String : public Exp {
    public:
        // Value of the string
        std::string value;

        // Constructor that receives a C-style string that represents the string *including quotes*
        explicit String(std::string str);
        std::string getValue() const { return value; }
        BuiltInType getType() override { return BuiltInType::STRING; }
        void accept(Visitor &visitor) override {
            visitor.visit(*this);
        }
    };

    /* Boolean literal */
    class Bool : public Exp {
    public:
        // Value of the boolean
        bool value;

        // Constructor that receives the boolean value
        explicit Bool(bool value);
        bool getValue() const { return value; }
        BuiltInType getType() override { return BuiltInType::BOOL; }
        void accept(Visitor &visitor) override {
            visitor.visit(*this);
        }
    };

    /* Identifier */
    class ID : public Exp {
    public:
        // Name of the identifier
        std::string value;

        // Constructor that receives a C-style string that represents the identifier
        explicit ID(std::string str);
        std::string getValue() const { return value; }
        void accept(Visitor &visitor) override {
            visitor.visit(*this);
        }
    };

    /* Binary arithmetic operation */
    class BinOp : public Exp {
    public:
        // Left operand
        std::shared_ptr<Exp> left;
        // Right operand
        std::shared_ptr<Exp> right;
        // Operation
        BinOpType op;

        // Constructor that receives the left and right operands and the operation
        BinOp(BinOpType op, std::shared_ptr<Exp> left = nullptr, std::shared_ptr<Exp> right = nullptr);
        std::shared_ptr<Exp> getLeft() const { return left; }
        std::shared_ptr<Exp> getRight() const { return right; }
        BinOpType getOp() const { return op; }
        BuiltInType getType() override { 
            if (left->getType() == BuiltInType::BYTE && right->getType() == BuiltInType::BYTE) {
                return BuiltInType::BYTE;
            }
            else if ((left->getType() == BuiltInType::INT && right->getType() == BuiltInType::INT) ||
                (left->getType() == BuiltInType::BYTE && right->getType() == BuiltInType::INT) ||
                (left->getType() == BuiltInType::INT && right->getType() == BuiltInType::BYTE)) {
                return BuiltInType::INT;
            }
            return BuiltInType::TYPE_ERROR; 
        }
        void accept(Visitor &visitor) override {
            visitor.visit(*this);
        }
    };

    /* Binary relational operation */
    class RelOp : public Exp {
    public:
        // Left operand
        std::shared_ptr<Exp> left;
        // Right operand
        std::shared_ptr<Exp> right;
        // Operation
        RelOpType op;

        // Constructor that receives the left and right operands and the operation
        RelOp(RelOpType op, std::shared_ptr<Exp> left = nullptr, std::shared_ptr<Exp> right = nullptr);
        std::shared_ptr<Exp> getLeft() const { return left; }
        std::shared_ptr<Exp> getRight() const { return right; }
        BinOpType getOp() const { return op; }
        BuiltInType getType() override { 
            if ((left->getType() == BuiltInType::BYTE && right->getType() == BuiltInType::BYTE) ||
                (left->getType() == BuiltInType::INT && right->getType() == BuiltInType::INT) ||
                (left->getType() == BuiltInType::BYTE && right->getType() == BuiltInType::INT) ||
                (left->getType() == BuiltInType::INT && right->getType() == BuiltInType::BYTE)) {
                return BuiltInType::BOOL;
            }
            return BuiltInType::TYPE_ERROR; 
        }
        void accept(Visitor &visitor) override {
            visitor.visit(*this);
        }
    };

    /* Unary logical NOT operation */
    class Not : public Exp {
    public:
        // Operand
        std::shared_ptr<Exp> exp;

        // Constructor that receives the operand
        explicit Not(std::shared_ptr<Exp> exp);
        std::shared_ptr<Exp> getExp() const { return exp; }
        BuiltInType getType() override { 
            if (exp->getType() == BuiltInType::BOOL) {
                return BuiltInType::BOOL;
            }
            return BuiltInType::TYPE_ERROR; 
        }
        void accept(Visitor &visitor) override {
            visitor.visit(*this);
        }
    };

    /* Binary logical AND operation */
    class And : public Exp {
    public:
        // Left operand
        std::shared_ptr<Exp> left;
        // Right operand
        std::shared_ptr<Exp> right;

        // Constructor that receives the left and right operands
        And(std::shared_ptr<Exp> left, std::shared_ptr<Exp> right);
        std::shared_ptr<Exp> getLeft() const { return left; }
        std::shared_ptr<Exp> getRight() const { return right; }
        BuiltInType getType() override { 
            if (left->getType() == BuiltInType::BOOL && right->getType() == BuiltInType::BOOL) {
                return BuiltInType::BOOL;
            }
            return BuiltInType::TYPE_ERROR; 
        }
        void accept(Visitor &visitor) override {
            visitor.visit(*this);
        }
    };

    /* Binary logical OR operation */
    class Or : public Exp {
    public:
        // Left operand
        std::shared_ptr<Exp> left;
        // Right operand
        std::shared_ptr<Exp> right;

        // Constructor that receives the left and right operands
        Or(std::shared_ptr<Exp> left, std::shared_ptr<Exp> right);
        std::shared_ptr<Exp> getLeft() const { return left; }
        std::shared_ptr<Exp> getRight() const { return right; }
        BuiltInType getType() override { 
            if (left->getType() == BuiltInType::BOOL && right->getType() == BuiltInType::BOOL) {
                return BuiltInType::BOOL;
            }
            return BuiltInType::TYPE_ERROR; 
        }
        void accept(Visitor &visitor) override {
            visitor.visit(*this);
        }
    };

    /* Type symbol */
    class Type : public Node {
    public:
        // Type
        BuiltInType type;

        // Constructor that receives the type
        explicit Type(BuiltInType type);
        BuiltInType getTypeOfType() override { return type; }
        void accept(Visitor &visitor) override {
            visitor.visit(*this);
        }
    };

    /* Type cast */
    class Cast : public Exp {
    public:
        // Expression to be cast
        std::shared_ptr<Exp> exp;
        // Target type
        std::shared_ptr<Type> target_type;

        // Constructor that receives the expression and the target type
        Cast(std::shared_ptr<Exp> exp, std::shared_ptr<Type> type);
        std::shared_ptr<Exp> getExp() const { return exp; }
        std::shared_ptr<Exp> getTargetType() const { return target_type; }
        void accept(Visitor &visitor) override {
            visitor.visit(*this);
        }
    };

    /* List of expressions */
    class ExpList : public Node {
    public:
        // List of expressions
        std::vector<std::shared_ptr<Exp>> exps;

        // Constructor that receives no expressions
        ExpList() = default;

        // Constructor that receives the first expression
        explicit ExpList(std::shared_ptr<Exp> exp);

        // Method to add an expression at the beginning of the list
        void push_front(const std::shared_ptr<Exp> &exp);

        // Method to add an expression at the end of the list
        void push_back(const std::shared_ptr<Exp> &exp);

        std::vector<std::shared_ptr<Exp>> getExps() const { return exps; }

        std::vector<std::string> getType() override {
            std::vector<std::string> types;
            for(auto& exp : exps) {
                types.push_back(exp->getType());
            }
            return types;
        }

        void accept(Visitor &visitor) override {
            visitor.visit(*this);
        }
    };

    /* Function call */
    class Call : public Exp, public Statement {
    public:
        // Function identifier
        std::shared_ptr<ID> func_id;
        // List of arguments as expressions
        std::shared_ptr<ExpList> args;

        // Constructor that receives the function identifier and the list of arguments
        Call(std::shared_ptr<ID> func_id, std::shared_ptr<ExpList> args);

        // Constructor that receives only the function identifier (for parameterless functions)
        explicit Call(std::shared_ptr<ID> func_id);

        std::shared_ptr<ID> getFuncId() const { return func_id; }

        std::shared_ptr<ExpList> getArgs() const { return args; }

        void accept(Visitor &visitor) override {
            visitor.visit(*this);
        }
    };

    /* List of statements */
    class Statements : public Statement {
    public:
        // List of statements
        std::vector<std::shared_ptr<Statement>> statements;

        // Constructor that receives no statements
        Statements() = default;

        // Constructor that receives the first statement
        explicit Statements(std::shared_ptr<Statement> statement);

        // Method to add a statement at the beginning of the list
        void push_front(const std::shared_ptr<Statement> &statement);

        // Method to add a statement at the end of the list
        void push_back(const std::shared_ptr<Statement> &statement);


        std::vector<std::shared_ptr<Statement>> getStatements() const { return statements; }

        void accept(Visitor &visitor) override {
            visitor.visit(*this);
        }
    };

    /* Break statement */
    class Break : public Statement {
        void accept(Visitor &visitor) override {
            visitor.visit(*this);
        }
    };

    /* Continue statement */
    class Continue : public Statement {
        void accept(Visitor &visitor) override {
            visitor.visit(*this);
        }
    };

    /* Return statement */
    class Return : public Statement {
    public:
        // Expression to be returned. If the return is expressionless, this field is nullptr
        std::shared_ptr<Exp> exp;

        // Constructor that receives the expression to be returned
        explicit Return(std::shared_ptr<Exp> exp = nullptr);

        void accept(Visitor &visitor) override {
            visitor.visit(*this);
        }
    };

    /* If statement */
    class If : public Statement {
    public:
        // Condition expression
        std::shared_ptr<Exp> condition;
        // Statement to be executed if the condition is true
        std::shared_ptr<Statement> then;
        // Statement to be executed if the condition is false. For an if statement without else, this field is nullptr
        std::shared_ptr<Statement> otherwise;

        // Constructor that receives the condition, the statement to be executed if the condition is true, and the statement to be executed if the condition is false
        If(std::shared_ptr<Exp> condition, std::shared_ptr<Statement> then,
           std::shared_ptr<Statement> otherwise = nullptr);

        std::shared_ptr<Exp> getCondition() const { return condition; }
        std::shared_ptr<Statement> getThen() const { return then; }
        std::shared_ptr<Statement> getElse() const { return otherwise; }

        void accept(Visitor &visitor) override {
            visitor.visit(*this);
        }
    };

    /* While statement */
    class While : public Statement {
    public:
        // Condition expression
        std::shared_ptr<Exp> condition;
        // Statement to be executed while the condition is true
        std::shared_ptr<Statement> body;

        // Constructor that receives the condition and the statement to be executed while the condition is true
        While(std::shared_ptr<Exp> condition, std::shared_ptr<Statement> body);

        std::shared_ptr<Exp> getCondition() const { return condition; }
        std::shared_ptr<Statement> getBody() const { return body; }

        void accept(Visitor &visitor) override {
            visitor.visit(*this);
        }
    };

    /* Variable declaration */
    class VarDecl : public Statement {
    public:
        // Identifier of the variable
        std::shared_ptr<ID> id;
        // Type of the variable
        std::shared_ptr<Type> type;
        // Initial value of the variable. If the variable is not initialized, this field is nullptr
        std::shared_ptr<Exp> init_exp;

        // Constructor that receives the identifier, the type, and the initial value expression
        VarDecl(std::shared_ptr<ID> id, std::shared_ptr<Type> type, std::shared_ptr<Exp> init_exp = nullptr);

        std::shared_ptr<ID> getVarId() const { return id; }
        std::shared_ptr<Type> getVarType() const { return type; }
        std::shared_ptr<Exp> getVarInitExp() const { return init_exp; }

        void accept(Visitor &visitor) override {
            visitor.visit(*this);
        }
    };

    /* Assignment statement */
    class Assign : public Statement {
    public:
        // Identifier of the variable
        std::shared_ptr<ID> id;
        // Expression to be assigned
        std::shared_ptr<Exp> exp;

        // Constructor that receives the identifier and the expression to be assigned
        Assign(std::shared_ptr<ID> id, std::shared_ptr<Exp> exp);

        std::shared_ptr<ID> getAssignId() const { return id; }
        std::shared_ptr<Exp> getAssignExp() const { return exp; }

        void accept(Visitor &visitor) override {
            visitor.visit(*this);
        }
    };

    /* Formal parameter */
    class Formal : public Node {
    public:
        // Identifier of the parameter
        std::shared_ptr<ID> id;
        // Type of the parameter
        std::shared_ptr<Type> type;

        // Constructor that receives the identifier and the type
        Formal(std::shared_ptr<ID> id, std::shared_ptr<Type> type);

        std::shared_ptr<ID> getFormalId() const { return id; }
        std::shared_ptr<Type> getFormalType() const { return type; }

        void accept(Visitor &visitor) override {
            visitor.visit(*this);
        }
    };

    /* List of formal parameters */
    class Formals : public Node {
    public:
        // List of formal parameters
        std::vector<std::shared_ptr<Formal>> formals;

        // Constructor that receives no parameters
        Formals() = default;

        // Constructor that receives the first formal parameter
        explicit Formals(std::shared_ptr<Formal> formal);

        // Method to add a formal parameter at the beginning of the list
        void push_front(const std::shared_ptr<Formal> &formal);

        // Method to add a formal parameter at the end of the list
        void push_back(const std::shared_ptr<Formal> &formal);

        std::vector<std::shared_ptr<Formal>> getFormals() const { return formals; }
        void accept(Visitor &visitor) override {
            visitor.visit(*this);
        }
    };

    /* Function declaration */
    class FuncDecl : public Node {
    public:
        // Identifier of the function
        std::shared_ptr<ID> id;
        // Return type of the function
        std::shared_ptr<Type> return_type;
        // List of formal parameters
        std::shared_ptr<Formals> formals;
        // Body of the function
        std::shared_ptr<Statements> body;

        // Constructor that receives the identifier, the return type, the list of formal parameters, and the body
        FuncDecl(std::shared_ptr<ID> id, std::shared_ptr<Type> return_type, std::shared_ptr<Formals> formals,
                 std::shared_ptr<Statements> body);

        std::shared_ptr<ID> getFuncId() const { return id; }
        std::shared_ptr<Type> getFuncReturnType() const { return return_type; }
        std::shared_ptr<Formals> getFuncParams() const { return formals; }
        std::shared_ptr<Statements> getFuncsBody() const { return body; }

        void accept(Visitor &visitor) override {
            visitor.visit(*this);
        }
    };

    /* List of function declarations */
    class Funcs : public Node {
    public:
        // List of function declarations
        std::vector<std::shared_ptr<FuncDecl>> funcs;

        // Constructor that receives no function declarations
        Funcs() = default;

        // Constructor that receives the first function declaration
        explicit Funcs(std::shared_ptr<FuncDecl> func);

        // Method to add a function declaration at the beginning of the list
        void push_front(const std::shared_ptr<FuncDecl> &func);

        // Method to add a function declaration at the end of the list
        void push_back(const std::shared_ptr<FuncDecl> &func);

        std::vector<std::shared_ptr<FuncDecl>> getFuncs() const { return funcs; }
        
        void accept(Visitor &visitor) override {
            visitor.visit(*this);
        }
    };
}

#define YYSTYPE std::shared_ptr<ast::Node>

#endif //NODES_HPP
