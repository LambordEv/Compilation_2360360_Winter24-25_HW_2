#ifndef SYMBOL_HPP
#define SYMBOL_HPP

#include <string>
#include <stdexcept>

// Define a Symbol class to represent variables and functions
class Symbol {
public:
    enum class SymbolType {
        VARIABLE,
        FUNCTION
    };

private:
    std::string name;
    SymbolType type;
    int index;  // Used to represent the type of the variable/function

public:
    Symbol(const std::string &name, SymbolType type, const int index)
        : name(name), type(type), index(index) {}

    const std::string& getName() const { return name; }
    SymbolType getType() const { return type; }
    const int getIndex() const { return index; }
};

#endif // SYMBOL_HPP
