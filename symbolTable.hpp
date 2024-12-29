#ifndef SYMBOL_TABLE_HPP
#define SYMBOL_TABLE_HPP

#include "Scope.hpp"
#include <memory>
#include <unordered_map>
#include <string>
#include <stack>

class SymbolTable {
private:
    std::stack<Scope*> scopeStack;  // Stack to manage nested scopes

public:
    SymbolTable() {
        scopeStack.push(new Scope(nullptr));
        scopeStack.top()->addSymbol("print", SymbolType::FUNCTION, 0);
        scopeStack.top()->addSymbol("printi", SymbolType::FUNCTION, 1);
    }

    ~SymbolTable() {
        while (!scopeStack.empty()) {
            delete scopeStack.top();
            scopeStack.pop();
        }
    }

    void addSymbol(const std::string &name, const Symbol &symbol) {
        int offset = getCurrentScope()->allocateOffset();
        Symbol symbol(name, type, dataType, offset);
        getCurrentScope()->addSymbol(name, symbol);
    }

    Symbol* getSymbol(const std::string &name) {
        return getCurrentScope()->getSymbol(name);
    }

    void beginScope() {
        Scope* newScope = new Scope(scopeStack.top());
        scopeStack.push(newScope);
    }

    void endScope() {
        if (scopeStack.size() > 1) { 
            Scope* currentScope = scopeStack.top();
            scopeStack.pop();
            delete currentScope;
        } else {
            throw std::runtime_error("Cannot end the last scope");
        }
    }

    Scope* getCurrentScope() {
        return scopeStack.top();
    }
};

#endif // SYMBOL_TABLE_HPP
