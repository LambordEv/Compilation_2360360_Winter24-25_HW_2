#ifndef SYMBOL_HPP
#define SYMBOL_HPP

#include <string>
#include <stdexcept>

class Symbol {
public:
    enum class SymbolType {
        VARIABLE,
        FUNCTION
    };

    enum class DataType {
        INT,
        BYTE,
        BOOL,
        STRING,
        VOID
    };

private:
    std::string name;
    SymbolType symbolType;
    DataType dataType;
    int offset;

public:
    Symbol(const std::string &name, SymbolType symbolType, DataType dataType, int offset)
        : name(name), symbolType(symbolType), dataType(dataType), offset(offset) {}

    const std::string &getName() const { return name; }
    SymbolType getSymbolType() const { return symbolType; }
    DataType getDataType() const { return dataType; }
    int getOffset() const { return offset; }
};

#endif // SYMBOL_HPP
