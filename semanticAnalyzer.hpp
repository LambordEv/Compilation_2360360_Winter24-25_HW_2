#ifndef SEMANTIC_ANALYZER_HPP
#define SEMANTIC_ANALYZER_HPP

#include "visitor.hpp"
#include "symbolTable.hpp"
#include "output.hpp"
#include <string>
#include <stdexcept>
#include <vector>

class SemanticAnalyzer : public Visitor {
private:
    SymbolTable* symbolTable;
    output::ScopePrinter printer;

public:
    SemanticAnalyzer(SymbolTable* symbolTable)
        : symbolTable(symbolTable) {}

    // Start a new scope (for functions, blocks, if, while, etc.)
    void beginScope() {
        symbolTable->beginScope();
        printer.beginScope();
    }

    // End the current scope
    void endScope() {
        symbolTable->endScope();
        printer.endScope();
    }

    // Implementations of visit methods
    void visit(ast::Num& node) override {}

    void visit(ast::NumB& node) override {
        if (node.getValue() > 255) {
            errorByteTooLarge(node.getLine(), node.getValue());
        }
    }

    void visit(ast::String& node) override {}

    void visit(ast::Bool& node) override {}

    void visit(ast::ID& node) override {
        Symbol* var = symbolTable->getSymbol(node.getValue());
        if (!var) {
            errorUndef(node.getLine(), node.getValue());
        }
    }

    void visit(ast::BinOp& node) override {
        node.getLeft()->accept(*this);
        node.getRight()->accept(*this);

        if (node.getType() == BuiltInType::TYPE_ERROR) {
            errorMismatch(node.getLine());
        }
    }

    void visit(ast::RelOp& node) override {
        node.getLeft()->accept(*this);
        node.getRight()->accept(*this);

        if (node.getType() == BuiltInType::TYPE_ERROR) {
            errorMismatch(node.getLine());
        }
    }

    void visit(ast::Not& node) override {
        node.getExpr()->accept(*this);
        if (node.getType() == BuiltInType::TYPE_ERROR) {
            errorMismatch(node.getLine());
        }
    }

    void visit(ast::And& node) override {
        node.getLeft()->accept(*this);
        node.getRight()->accept(*this);

        if (node.getType() == BuiltInType::TYPE_ERROR) {
            errorMismatch(node.getLine());
        }
    }

    void visit(ast::Or& node) override {
        node.getLeft()->accept(*this);
        node.getRight()->accept(*this);

        if (node.getType() == BuiltInType::TYPE_ERROR) {
            errorMismatch(node.getLine());
        }
    }

    void visit(ast::Type& node) override {}

    void visit(ast::Cast& node) override {
        node.getExpr()->accept(*this);

        // ??? check + errors ???

    }

    void visit(ast::ExpList& node) override {
        for (auto& expr : node.getExpressions()) {
            expr->accept(*this);
        }
    }

    void visit(ast::Call& node) override {
        Symbol* func = symbolTable->getSymbol(node.getFuncId());
        if (!func) {
            errorUndefFunc(node.getLine(), node.getFuncId());
        }

        node.getArgs()->accept(*this);

        // need to compare types of arguments from symbolTable to types of arguments from node.getArgs()->getType()
        // if there is at least one mismatch need to call error function with arguments types as string list
        /*if (-------) {
            errorPrototypeMismatch(node.getLine(), node.getFuncId(), -------- );
        }*/
    }

    void visit(ast::Statements& node) override {
        for (auto& statement : node.getStatements()) {
            statement->accept(*this);
        }
    }

    void visit(ast::Break& node) override {
        /* need to implement inLoopScope() in symbolTable */
        // then we can use this VVV
        /* if (!symbolTable->inLoopScope()) {
            errorUnexpectedBreak(node.getLine());
        }*/ 
    }

    void visit(ast::Continue& node) override {
        /* need to implement inLoopScope() in symbolTable */
        // then we can use this VVV
        /* if (!symbolTable->inLoopScope()) {
            errorUnexpectedBreak(node.getLine());
        }*/ 
    }

    void visit(ast::Return& node) override {
        node.getExpr()->accept(*this);

        // Check if return type matches the function's return type
        Symbol* func = symbolTable->getCurrentFunction();
        if (func && func->getDataType() != node.getExpr()->getType()) {
            errorMismatch(node.getLine());
        }
    }

    void visit(ast::If& node) override {
        node.getCondition()->accept(*this);
        node.getThen()->accept(*this);
        if (node.getElse()) {
            node.getElse()->accept(*this);
        }

        if (node.getCondition->getType() != BuiltInType::BOOL) {
            errorMismatch(node.getLine());
        }
    }

    void visit(ast::While& node) override {
        node.getCondition()->accept(*this);
        node.getBody()->accept(*this);

        if (node.getCondition->getType() != BuiltInType::BOOL) {
            errorMismatch(node.getLine());
        }
    }


    void visit(ast::VarDecl& node) override {
        if (symbolTable->getSymbol(node.getVarId())) {
            errorDef(node.getLine(), node.getVarId());
        }
        // need to implement getOffset in symbolTable & need to finish declaring symbol structure for var and funcs
        // then we can use this VVVV
        /*symbolTable->addSymbol(node.getVarId(), Symbol(node.getVarId(), node.getVarType()));
        printer.emitVar(node.getVarId(), node.getVarType(), symbolTable->getOffset(node.getVarId()));*/
    }

    void visit(ast::Assign& node) override {
        Symbol* var = symbolTable->getSymbol(node.getAssignId());
        if (!var) {
            errorUndef(node.getLine(), node.getAssignId());
        }
        node.getAssignExp()->accept(*this);
    }

    void visit(ast::Formal& node) override {
        symbolTable->addSymbol(node.getFormalId(), Symbol(node.getFormalId(), node.getFormalType()));
        printer.emitVar(node.getFormalId(), node.getFormalType(), symbolTable->getOffset(node.getFormalId()));
    }

    void visit(ast::Formals& node) override {
        for (auto& formal : node.getFormals()) {
            formal->accept(*this);
        }
    }

    void visit(ast::FuncDecl& node) override {
        if (symbolTable->getSymbol(node.getFuncId())) {
            errorDef(node.getLine(), node.getFuncId());
        }

        // need to add here func params in symbol intit (need to change symbol implementation)
        // need to create a aux funxtion to transfer types list to string list
        symbolTable->addSymbol(node.getFuncId(), Symbol(node.getFuncId(), node.getFuncReturnType()));
        //printer.emitFunc(node.getFuncId(), node.getFuncReturnType(), -- params types in strings -- );

        beginScope();
        node.getFormals()->accept(*this);
        node.getBody()->accept(*this);
        endScope();
    }

    void visit(ast::Funcs& node) override {
        for (auto& funcDecl : node.getFuncs()) {
            funcDecl->accept(*this);
        }
    }
};

#endif // SEMANTIC_ANALYZER_HPP
