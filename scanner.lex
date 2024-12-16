%{
/*Declaration Section*/
/*------- Include Section -------*/
#include <stdio.h>
#include "output.hpp"
#include "parser.tab.h"


/*------- Function Declarion Section -------*/
using namespace ast;

void accumalateStringLexema(void);

/*------- Static Variables Declarion Section -------*/
static char accumalatedString[2096] = {0};
static char accumalatedStrLen = 0;

%}

%option yylineno
%option noyywrap

whitespace                              ([\t\r\n ])
decimalDigit                            ([0-9])
hexDigit                                ([0-9a-fA-F])
letter                                  ([a-zA-Z])

voidToken                               (void)
intToken                                (int)
byteToken                               (byte)
boolToken                               (bool)
andToken                                (and)
orToken                                 (or)
notToken                                (not)
trueToken                               (true)
falseToken                              (false)
returnToken                             (return)
ifToken                                 (if)
elseToken                               (else)
whileToken                              (while)
breakToken                              (break)
continueToken                           (continue)
semicolonToken                          (;)
commaToken                              (,)
lParenToken                             (\()
rParenToken                             (\))
lBraceToken                             (\{)
rBraceToken                             (\})
assignToken                             (\=)

relopSign                               (==|!=|<|>|<=|>=)
binopSign                               (\+|\-|\*|\/)

commentLexema                           (\/\/.*)

idLexema                                ({letter}+{decimalDigit}*{letter}*)

numLexema                               ((0)|([1-9]+{decimalDigit}*))

byteNumLexema                           ({numLexema}b)

%x STRING_LEXEMA
%x STRING_ESCAPE
stringLexemaEnterExit                   (\")


%%
{voidToken}                             { return T_VOID; }
{intToken}                              { return T_INT; }
{byteToken}                             { return T_BYTE; }
{boolToken}                             { return T_BOOL; }
{andToken}                              { return T_AND; }
{orToken}                               { return T_OR; }
{notToken}                              { return T_NOT; }
{trueToken}                             { return T_TRUE; }
{falseToken}                            { return T_FALSE; }
{returnToken}                           { return T_RETURN; }
{ifToken}                               { return T_IF; }
{elseToken}                             { return T_ELSE; }
{whileToken}                            { return T_WHILE; }
{breakToken}                            { return T_BREAK; }
{continueToken}                         { return T_CONTINUE; }
{semicolonToken}                        { return T_SC; }
{commaToken}                            { return T_COMMA; }
{lParenToken}                           { return T_LPAREN; }
{rParenToken}                           { return T_RPAREN; }
{lBraceToken}                           { return T_LBRACE; }
{rBraceToken}                           { return T_RBRACE; }
{assignToken}                           { return T_ASSIGN; }

{relopSign}                             { return T_RELOP; }
{binopSign}                             { return T_BINOP; }
{commentLexema}                         { ; }
{idLexema}                              { return T_ID; }
{numLexema}                             { return T_NUM; }
{byteNumLexema}                         { return T_NUM_B; }


{stringLexemaEnterExit}                 { BEGIN(STRING_LEXEMA); }
<STRING_LEXEMA>[\\]                     { BEGIN(STRING_ESCAPE); accumalateStringLexema(); }
<STRING_LEXEMA>{stringLexemaEnterExit}  { BEGIN(INITIAL); accumalateStringLexema(); return T_STRING; }
<STRING_LEXEMA><<EOF>>                  { BEGIN(INITIAL); return T_STRING; }
<STRING_LEXEMA>[\n]                     { BEGIN(INITIAL); return T_STRING; }
<STRING_LEXEMA>[\r]                     { BEGIN(INITIAL); return T_STRING; }
<STRING_LEXEMA>(.)                      { accumalateStringLexema(); }


<STRING_ESCAPE><<EOF>>                  { BEGIN(INITIAL); return T_STRING; }
<STRING_ESCAPE>.                        { BEGIN(STRING_LEXEMA); accumalateStringLexema(); }



{whitespace}                            ;
.                                       { output::errorLex(*yytext); }

%%

void accumalateStringLexema(void)
{
    for(int j = 0; j < yyleng; ++j)
    {
        //printf("%c", yytext[j]);
        accumalatedString[accumalatedStrLen + j] = yytext[j];
    }
    //printf("\n");

    accumalatedStrLen += yyleng;
}