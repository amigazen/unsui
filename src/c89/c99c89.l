%{
/*
 * C99 to C89 Converter - Flex Lexer
 * Tokenizes C99 source code for parsing by Bison
 *
 * Copyright (c) 2025 amigazen project. All rights reserved.
 */

#include "main.h"
#include "ast.h"

/* Flex function declarations */
int yylex(void);
void yyerror(const char* s);

/* Global variables for communication with parser */
extern int yylineno;
extern char* yytext;

%}

/* Flex options */
%option noyywrap
%option yylineno
%option case-insensitive

/* Character classes */
DIGIT       [0-9]
LETTER      [a-zA-Z_]
HEX_DIGIT   [0-9a-fA-F]
OCT_DIGIT   [0-7]
WHITESPACE  [ \t\r]
NEWLINE     \n

/* Token definitions */
%%

"//"                    { return C99_COMMENT; }
"inline"               { return INLINE; }
"restrict"             { return RESTRICT; }
"for"                  { return FOR; }
"if"                   { return IF; }
"else"                 { return ELSE; }
"while"                { return WHILE; }
"do"                   { return DO; }
"switch"               { return SWITCH; }
"case"                 { return CASE; }
"default"              { return DEFAULT; }
"break"                { return BREAK; }
"continue"             { return CONTINUE; }
"return"               { return RETURN; }
"goto"                 { return GOTO; }
"int"                  { return INT; }
"char"                 { return CHAR; }
"short"                { return SHORT; }
"long"                 { return LONG; }
"float"                { return FLOAT; }
"double"               { return DOUBLE; }
"void"                 { return VOID; }
"signed"               { return SIGNED; }
"unsigned"             { return UNSIGNED; }
"const"                { return CONST; }
"volatile"             { return VOLATILE; }
"static"               { return STATIC; }
"extern"               { return EXTERN; }
"auto"                 { return AUTO; }
"register"             { return REGISTER; }
"struct"               { return STRUCT; }
"union"                { return UNION; }
"enum"                 { return ENUM; }
"typedef"              { return TYPEDEF; }
"sizeof"               { return SIZEOF; }

"("                    { return LPAREN; }
")"                    { return RPAREN; }
"{"                    { return LBRACE; }
"}"                    { return RBRACE; }
"["                    { return LBRACKET; }
"]"                    { return RBRACKET; }
";"                    { return SEMICOLON; }
","                    { return COMMA; }
"."                    { return DOT; }
"->"                   { return ARROW; }

"="                    { return ASSIGN; }
"+"                    { return PLUS; }
"-"                    { return MINUS; }
"*"                    { return MULTIPLY; }
"/"                    { return DIVIDE; }
"%"                    { return MODULO; }
"&"                    { return ADDRESS; }
"|"                    { return BITWISE_OR; }
"^"                    { return BITWISE_XOR; }
"~"                    { return BITWISE_NOT; }
"<<"                   { return LEFT_SHIFT; }
">>"                   { return RIGHT_SHIFT; }

"++"                   { return INCREMENT; }
"--"                   { return DECREMENT; }
"+="                   { return PLUS_ASSIGN; }
"-="                   { return MINUS_ASSIGN; }
"*="                   { return MULTIPLY_ASSIGN; }
"/="                   { return DIVIDE_ASSIGN; }
"%="                   { return MODULO_ASSIGN; }
"&="                   { return AND_ASSIGN; }
"|="                   { return OR_ASSIGN; }
"^="                   { return XOR_ASSIGN; }
"<<="                  { return LEFT_SHIFT_ASSIGN; }
">>="                  { return RIGHT_SHIFT_ASSIGN; }

"=="                   { return EQ; }
"!="                   { return NE; }
"<"                    { return LT; }
">"                    { return GT; }
"<="                   { return LE; }
">="                   { return GE; }
"!"                    { return NOT; }
"&&"                   { return AND; }
"||"                   { return OR; }

"?"                    { return QUESTION; }
":"                    { return COLON; }

"malloc"               { return MALLOC; }
"free"                 { return FREE; }
"printf"               { return PRINTF; }
"scanf"                { return SCANF; }
"fprintf"              { return FPRINTF; }
"fscanf"               { return FSCANF; }
"sprintf"              { return SPRINTF; }
"sscanf"               { return SSCANF; }
"strcpy"               { return STRCPY; }
"strcat"               { return STRCAT; }
"strcmp"               { return STRCMP; }
"strlen"               { return STRLEN; }
"memcpy"               { return MEMCPY; }
"memcmp"               { return MEMCMP; }
"memset"               { return MEMSET; }

"NULL"                 { return NULL_CONST; }
"true"                 { return TRUE; }
"false"                { return FALSE; }

/* Identifiers */
{LETTER}({LETTER}|{DIGIT})* {
    yylval.string = strdup(yytext);
    return IDENTIFIER;
}

/* Numbers */
{DIGIT}+ {
    yylval.number = atoi(yytext);
    return NUMBER;
}

{DIGIT}+\.{DIGIT}+ {
    yylval.string = strdup(yytext);
    return FLOAT_NUMBER;
}

0[xX]{HEX_DIGIT}+ {
    yylval.string = strdup(yytext);
    return HEX_NUMBER;
}

0{OCT_DIGIT}+ {
    yylval.string = strdup(yytext);
    return OCT_NUMBER;
}

/* Strings */
\"([^"\\]|\\.)*\" {
    yylval.string = strdup(yytext);
    return STRING;
}

/* Character literals */
\'([^'\\]|\\.)\' {
    yylval.string = strdup(yytext);
    return CHAR_LITERAL;
}

/* C comments */
"/*"([^*]|\*+[^*/])*\*+"/" {
    /* Skip C comments */
}

/* C++ comments */
"//".* {
    /* Skip C++ comments */
}

/* Preprocessor directives */
^"#"[ \t]*"include"[ \t]*[<\"][^>\"]*[>\"] {
    yylval.string = strdup(yytext);
    return PREPROCESSOR_INCLUDE;
}

^"#"[ \t]*"define"[ \t]+[a-zA-Z_][a-zA-Z0-9_]* {
    yylval.string = strdup(yytext);
    return PREPROCESSOR_DEFINE;
}

^"#"[ \t]*"ifdef"[ \t]+[a-zA-Z_][a-zA-Z0-9_]* {
    yylval.string = strdup(yytext);
    return PREPROCESSOR_IFDEF;
}

^"#"[ \t]*"ifndef"[ \t]+[a-zA-Z_][a-zA-Z0-9_]* {
    yylval.string = strdup(yytext);
    return PREPROCESSOR_IFNDEF;
}

^"#"[ \t]*"endif" {
    return PREPROCESSOR_ENDIF;
}

^"#"[ \t]*"else" {
    return PREPROCESSOR_ELSE;
}

^"#"[ \t]*"pragma"[ \t].* {
    yylval.string = strdup(yytext);
    return PREPROCESSOR_PRAGMA;
}

/* Whitespace */
{WHITESPACE}+ {
    /* Skip whitespace */
}

{NEWLINE} {
    /* Count lines */
}

/* Any other character */
. {
    return yytext[0];
}

%%

/* Flex function implementations */
int yywrap(void) {
    return 1;
}

void yyerror(const char* s) {
    fprintf(stderr, "Lexical error at line %d: %s\n", yylineno, s);
}
