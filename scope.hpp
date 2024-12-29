#ifndef SCOPE_HPP
#define SCOPE_HPP

#include "Symbol.hpp"
#include <unordered_map>
#include <memory>
#include <stdexcept>

class Scope {
public:
    Scope* parent;

private:
    std::unordered_map<std::string, Symbol> symbolTable;
    int nextOffset;
    int nextParamOffset;

public:
    Scope(Scope* parent)
        : parent(parent),
          nextOffset(0),
          nextParamOffset(-1) {}

    void addVariableSymbol(const std::string& name, const Symbol& symbol) {
        /*if (symbolTable.count(name) > 0) {
            //erroDef
        }*/
        symbolTable[name] = Symbol(name, symbol.getType(), nextOffset);
        ++nextOffset;
    }

    void addParameterSymbol(const std::string& name, const Symbol& symbol) {
        /*if (symbolTable.count(name) > 0) {
            //erroDef
        }*/
        symbolTable[name] = Symbol(name, symbol.getType(), nextParamOffset);
        --nextParamOffset;
    }

    Symbol* getSymbol(const std::string& name) {
        auto it = symbolTable.find(name);
        if (it != symbolTable.end()) {
            return &it->second;
        }

        Scope* next = parent;
        while (next) {
            it = next->symbolTable.find(name);
            if (it != next->symbolTable.end()) {
                return &it->second;
            }
            next = next->parent;
        }
        return nullptr;
    }

    int getNextOffset() const {
        return nextOffset;
    }

    int getNextParamOffset() const {
        return nextParamOffset;
    }
};

#endif // SCOPE_HPP
