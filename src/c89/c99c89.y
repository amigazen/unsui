%{
/*
 * C99 to C89 Converter - Bison Grammar
 * Parses C99 source code and builds abstract syntax tree
 *
 * Copyright (c) 2025 amigazen project. All rights reserved.
 */

#include "main.h"
#include "ast.h"
#include "transform.h"
#include "generator.h"

/* Bison function declarations */
int yylex(void);
void yyerror(const char* s);
extern int yylineno;
extern char* yytext;

/* Global variables */
ASTNode* program_ast = NULL;
int temp_var_counter = 0;
int verbose_mode = 0;

%}

/* Token type definitions */
%union {
    int number;
    char* string;
    ASTNode* ast;
}

/* Token declarations */
%token <number> NUMBER
%token <string> IDENTIFIER STRING CHAR_LITERAL FLOAT_NUMBER HEX_NUMBER OCT_NUMBER
%token <string> PREPROCESSOR_INCLUDE PREPROCESSOR_DEFINE PREPROCESSOR_IFDEF
%token <string> PREPROCESSOR_IFNDEF PREPROCESSOR_ENDIF PREPROCESSOR_ELSE PREPROCESSOR_PRAGMA

/* Keywords */
%token FOR IF ELSE WHILE DO SWITCH CASE DEFAULT BREAK CONTINUE RETURN GOTO
%token INT CHAR SHORT LONG FLOAT DOUBLE VOID SIGNED UNSIGNED
%token CONST VOLATILE STATIC EXTERN AUTO REGISTER
%token STRUCT UNION ENUM TYPEDEF SIZEOF
%token INLINE RESTRICT C99_COMMENT

/* Operators */
%token LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET
%token SEMICOLON COMMA DOT ARROW
%token ASSIGN PLUS MINUS MULTIPLY DIVIDE MODULO
%token ADDRESS BITWISE_OR BITWISE_XOR BITWISE_NOT
%token LEFT_SHIFT RIGHT_SHIFT
%token INCREMENT DECREMENT
%token PLUS_ASSIGN MINUS_ASSIGN MULTIPLY_ASSIGN DIVIDE_ASSIGN MODULO_ASSIGN
%token AND_ASSIGN OR_ASSIGN XOR_ASSIGN LEFT_SHIFT_ASSIGN RIGHT_SHIFT_ASSIGN
%token EQ NE LT GT LE GE NOT AND OR
%token QUESTION COLON

/* Library functions */
%token MALLOC FREE PRINTF SCANF FPRINTF FSCANF SPRINTF SSCANF
%token STRCPY STRCAT STRCMP STRLEN MEMCPY MEMCMP MEMSET
%token NULL_CONST TRUE FALSE

/* Type declarations */
%type <ast> program external_declaration_list external_declaration
%type <ast> function_definition function_declaration
%type <ast> declaration declaration_list declaration_specifiers
%type <ast> init_declarator_list init_declarator
%type <ast> type_specifier struct_or_union_specifier enum_specifier
%type <ast> struct_declaration_list struct_declaration
%type <ast> struct_declarator_list struct_declarator
%type <ast> enum_list enumerator
%type <ast> parameter_list parameter_declaration parameter_type_list
%type <ast> compound_statement statement_list statement
%type <ast> expression_statement selection_statement iteration_statement
%type <ast> jump_statement labeled_statement
%type <ast> expression assignment_expression conditional_expression
%type <ast> logical_or_expression logical_and_expression inclusive_or_expression
%type <ast> exclusive_or_expression and_expression equality_expression
%type <ast> relational_expression shift_expression additive_expression
%type <ast> multiplicative_expression cast_expression unary_expression
%type <ast> postfix_expression primary_expression argument_expression_list
%type <ast> initializer initializer_list designator_list designator
%type <ast> type_name abstract_declarator direct_abstract_declarator
%type <ast> type_qualifier_list storage_class_specifier

/* Operator precedence and associativity */
%left OR
%left AND
%left EQ NE
%left LT GT LE GE
%left LEFT_SHIFT RIGHT_SHIFT
%left PLUS MINUS
%left MULTIPLY DIVIDE MODULO
%right NOT ADDRESS BITWISE_NOT
%right INCREMENT DECREMENT
%right ASSIGN PLUS_ASSIGN MINUS_ASSIGN MULTIPLY_ASSIGN DIVIDE_ASSIGN MODULO_ASSIGN
%right AND_ASSIGN OR_ASSIGN XOR_ASSIGN LEFT_SHIFT_ASSIGN RIGHT_SHIFT_ASSIGN
%right QUESTION COLON
%right LPAREN RPAREN

%%

/* Grammar rules */

program: external_declaration_list
    {
        program_ast = $1;
        $$ = $1;
        if (verbose_mode) {
            printf("Parsed program with %d external declarations\n", 
                   get_child_count($1));
        }
    }
    ;

external_declaration_list: /* empty */
    {
        $$ = create_ast_node(AST_PROGRAM);
    }
    | external_declaration_list external_declaration
    {
        add_child($1, $2);
        $$ = $1;
    }
    ;

external_declaration: function_definition
                  | declaration
                  | ';'
                  ;

function_definition: declaration_specifiers IDENTIFIER LPAREN parameter_list_opt RPAREN compound_statement
    {
        $$ = create_function_definition($1, $2, $4, $6);
        if (verbose_mode) {
            printf("Function definition: %s\n", $2);
        }
    }
    ;

/* C99 for loop with variable declaration - needs transformation */
iteration_statement: FOR LPAREN for_init_opt SEMICOLON expr_opt SEMICOLON expr_opt RPAREN statement
    {
        $$ = transform_c99_for_loop($3, $5, $7, $9);
        if (verbose_mode) {
            printf("Transformed C99 for loop to C89\n");
        }
    }
    ;

for_init: declaration
    {
        /* Extract declaration for C89 compatibility */
        $$ = extract_declaration_for_c89($1);
        if (verbose_mode) {
            printf("Extracted declaration from for loop init\n");
        }
    }
    | expr_opt
    ;

/* C99 compound literals - needs transformation */
postfix_expression: LPAREN type_specifier RPAREN LBRACE initializer_list_opt RBRACE
    {
        $$ = transform_compound_literal($2, $5);
        if (verbose_mode) {
            printf("Transformed compound literal to temporary variable\n");
        }
    }
    ;

/* C99 designated initializers - needs transformation */
initializer: LBRACE designator_list RBRACE
    {
        $$ = transform_designated_initializer($2);
        if (verbose_mode) {
            printf("Transformed designated initializer to positional\n");
        }
    }
    ;

/* Handle C99 variable declarations in statements - needs transformation */
statement: declaration
    {
        /* Move declaration to beginning of block */
        $$ = transform_declaration_statement($1);
        if (verbose_mode) {
            printf("Transformed declaration statement to block beginning\n");
        }
    }
    ;

/* Basic expression parsing */
expression: assignment_expression
         | expression COMMA assignment_expression
         ;

assignment_expression: conditional_expression
                    | unary_expression assignment_operator assignment_expression
                    ;

conditional_expression: logical_or_expression
                     | logical_or_expression QUESTION expression COLON conditional_expression
                     ;

logical_or_expression: logical_and_expression
                     | logical_or_expression OR logical_and_expression
                     ;

logical_and_expression: inclusive_or_expression
                     | logical_and_expression AND inclusive_or_expression
                     ;

inclusive_or_expression: exclusive_or_expression
                      | inclusive_or_expression BITWISE_OR exclusive_or_expression
                      ;

exclusive_or_expression: and_expression
                       | exclusive_or_expression BITWISE_XOR and_expression
                       ;

and_expression: equality_expression
              | and_expression ADDRESS equality_expression
              ;

equality_expression: relational_expression
                   | equality_expression EQ relational_expression
                   | equality_expression NE relational_expression
                   ;

relational_expression: shift_expression
                     | relational_expression LT shift_expression
                     | relational_expression GT shift_expression
                     | relational_expression LE shift_expression
                     | relational_expression GE shift_expression
                     ;

shift_expression: additive_expression
                | shift_expression LEFT_SHIFT additive_expression
                | shift_expression RIGHT_SHIFT additive_expression
                ;

additive_expression: multiplicative_expression
                   | additive_expression PLUS multiplicative_expression
                   | additive_expression MINUS multiplicative_expression
                   ;

multiplicative_expression: cast_expression
                         | multiplicative_expression MULTIPLY cast_expression
                         | multiplicative_expression DIVIDE cast_expression
                         | multiplicative_expression MODULO cast_expression
                         ;

cast_expression: unary_expression
               | LPAREN type_name RPAREN cast_expression
               ;

unary_expression: postfix_expression
                | INCREMENT unary_expression
                | DECREMENT unary_expression
                | unary_operator cast_expression
                | SIZEOF unary_expression
                | SIZEOF LPAREN type_name RPAREN
                ;

postfix_expression: primary_expression
                  | postfix_expression LBRACKET expression RBRACKET
                  | postfix_expression LPAREN argument_expression_list_opt RPAREN
                  | postfix_expression DOT IDENTIFIER
                  | postfix_expression ARROW IDENTIFIER
                  | postfix_expression INCREMENT
                  | postfix_expression DECREMENT
                  ;

primary_expression: IDENTIFIER
                  | NUMBER
                  | STRING
                  | CHAR_LITERAL
                  | LPAREN expression RPAREN
                  ;

/* Optional elements */
parameter_list_opt: /* empty */
                  | parameter_list
                  ;

expr_opt: /* empty */
         | expression
         ;

initializer_list_opt: /* empty */
                    | initializer_list
                    ;

argument_expression_list_opt: /* empty */
                            | argument_expression_list
                            ;

/* Helper functions */
assignment_operator: ASSIGN
                   | PLUS_ASSIGN
                   | MINUS_ASSIGN
                   | MULTIPLY_ASSIGN
                   | DIVIDE_ASSIGN
                   | MODULO_ASSIGN
                   | AND_ASSIGN
                   | OR_ASSIGN
                   | XOR_ASSIGN
                   | LEFT_SHIFT_ASSIGN
                   | RIGHT_SHIFT_ASSIGN
                   ;

unary_operator: ADDRESS
              | MULTIPLY
              | PLUS
              | MINUS
              | BITWISE_NOT
              | NOT
              ;

/* Additional grammar rules for completeness */
compound_statement: LBRACE statement_list_opt RBRACE
                  ;

statement_list_opt: /* empty */
                  | statement_list
                  ;

statement_list: statement
              | statement_list statement
              ;

selection_statement: IF LPAREN expression RPAREN statement
                  | IF LPAREN expression RPAREN statement ELSE statement
                  | SWITCH LPAREN expression RPAREN statement
                  ;

labeled_statement: IDENTIFIER COLON statement
                 | CASE constant_expression COLON statement
                 | DEFAULT COLON statement
                 ;

jump_statement: CONTINUE SEMICOLON
              | BREAK SEMICOLON
              | RETURN expr_opt SEMICOLON
              | GOTO IDENTIFIER SEMICOLON
              ;

constant_expression: conditional_expression
                   ;

%%

/* Error handling */
void yyerror(const char* s) {
    fprintf(stderr, "Parse error at line %d: %s\n", yylineno, s);
    fprintf(stderr, "Near token: %s\n", yytext);
}
