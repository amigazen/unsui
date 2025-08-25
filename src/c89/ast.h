/*
 * C99 to C89 Converter - Abstract Syntax Tree
 * Defines data structures for representing parsed C code
 *
 * Copyright (c) 2025 amigazen project. All rights reserved.
 */

#ifndef AST_H
#define AST_H

/* AST node types */
typedef enum {
    AST_PROGRAM,
    AST_FUNCTION_DEFINITION,
    AST_FUNCTION_DECLARATION,
    AST_VARIABLE_DECLARATION,
    AST_PARAMETER_DECLARATION,
    AST_TYPE_SPECIFIER,
    AST_STRUCT_DECLARATION,
    AST_UNION_DECLARATION,
    AST_ENUM_DECLARATION,
    AST_TYPEDEF_DECLARATION,
    AST_COMPOUND_STATEMENT,
    AST_EXPRESSION_STATEMENT,
    AST_SELECTION_STATEMENT,
    AST_ITERATION_STATEMENT,
    AST_JUMP_STATEMENT,
    AST_LABELED_STATEMENT,
    AST_BINARY_EXPRESSION,
    AST_UNARY_EXPRESSION,
    AST_IDENTIFIER,
    AST_NUMBER,
    AST_STRING,
    AST_CHAR_LITERAL,
    AST_FLOAT_NUMBER,
    AST_HEX_NUMBER,
    AST_OCT_NUMBER,
    AST_FUNCTION_CALL,
    AST_ARRAY_ACCESS,
    AST_MEMBER_ACCESS,
    AST_POINTER_MEMBER_ACCESS,
    AST_CAST_EXPRESSION,
    AST_INITIALIZER,
    AST_INITIALIZER_LIST,
    AST_DESIGNATOR,
    AST_DESIGNATOR_LIST,
    AST_COMPOUND_LITERAL,
    AST_FOR_LOOP,
    AST_WHILE_LOOP,
    AST_DO_WHILE_LOOP,
    AST_IF_STATEMENT,
    AST_SWITCH_STATEMENT,
    AST_CASE_STATEMENT,
    AST_DEFAULT_STATEMENT,
    AST_BREAK_STATEMENT,
    AST_CONTINUE_STATEMENT,
    AST_RETURN_STATEMENT,
    AST_GOTO_STATEMENT,
    AST_BLOCK,
    AST_TRANSFORMED_FOR_LOOP,
    AST_TRANSFORMED_COMPOUND_LITERAL,
    AST_TRANSFORMED_DESIGNATED_INIT,
    AST_TRANSFORMED_DECLARATION_STATEMENT,
    AST_EMPTY,
    AST_PREPROCESSOR_DIRECTIVE,
    AST_PARAMETER_LIST,
    AST_ARGUMENT_LIST,
    AST_TYPE_NAME,
    AST_ABSTRACT_DECLARATOR,
    AST_DIRECT_ABSTRACT_DECLARATOR,
    AST_TYPE_QUALIFIER_LIST,
    AST_STORAGE_CLASS_SPECIFIER,
    AST_DECLARATION_SPECIFIERS,
    AST_INIT_DECLARATOR_LIST,
    AST_INIT_DECLARATOR,
    AST_STRUCT_DECLARATOR_LIST,
    AST_STRUCT_DECLARATOR,
    AST_ENUM_LIST,
    AST_ENUMERATOR,
    AST_CONSTANT_EXPRESSION,
    AST_ASSIGNMENT_EXPRESSION,
    AST_CONDITIONAL_EXPRESSION,
    AST_LOGICAL_OR_EXPRESSION,
    AST_LOGICAL_AND_EXPRESSION,
    AST_INCLUSIVE_OR_EXPRESSION,
    AST_EXCLUSIVE_OR_EXPRESSION,
    AST_AND_EXPRESSION,
    AST_EQUALITY_EXPRESSION,
    AST_RELATIONAL_EXPRESSION,
    AST_SHIFT_EXPRESSION,
    AST_ADDITIVE_EXPRESSION,
    AST_MULTIPLICATIVE_EXPRESSION,
    AST_POSTFIX_EXPRESSION,
    AST_PRIMARY_EXPRESSION
} ASTType;

/* AST node structure */
typedef struct ASTNode {
    ASTType type;
    char* value;
    struct ASTNode* parent;
    struct ASTNode** children;
    int child_count;
    int child_capacity;
    int line_number;
    int column_number;
    int source_file;
    void* extra_data;  /* For additional node-specific information */
} ASTNode;

/* Extra data structures for specific node types */
typedef struct {
    char* function_name;
    char* return_type;
    int parameter_count;
    int is_inline;
    int is_static;
    int is_extern;
} FunctionData;

typedef struct {
    char* variable_name;
    char* variable_type;
    int is_const;
    int is_volatile;
    int is_static;
    int is_extern;
    int is_auto;
    int is_register;
} VariableData;

typedef struct {
    char* struct_name;
    char* union_name;
    char* enum_name;
    int is_forward_declaration;
    int member_count;
} TypeData;

typedef struct {
    char* operator;
    int precedence;
    int associativity;
} OperatorData;

/* AST Creation Functions */
ASTNode* create_ast_node(ASTType type);
ASTNode* create_function_definition(ASTNode* return_type, char* name, ASTNode* params, ASTNode* body);
ASTNode* create_variable_declaration(ASTNode* type, char* name, ASTNode* initializer);
ASTNode* create_for_loop(ASTNode* init, ASTNode* condition, ASTNode* increment, ASTNode* body);
ASTNode* create_while_loop(ASTNode* condition, ASTNode* body);
ASTNode* create_do_while_loop(ASTNode* body, ASTNode* condition);
ASTNode* create_if_statement(ASTNode* condition, ASTNode* then_stmt, ASTNode* else_stmt);
ASTNode* create_switch_statement(ASTNode* expression, ASTNode* body);
ASTNode* create_case_statement(ASTNode* expression, ASTNode* statement);
ASTNode* create_default_statement(ASTNode* statement);
ASTNode* create_break_statement(void);
ASTNode* create_continue_statement(void);
ASTNode* create_return_statement(ASTNode* expression);
ASTNode* create_goto_statement(char* label);
ASTNode* create_compound_statement(ASTNode** statements, int count);
ASTNode* create_binary_expression(ASTType op, ASTNode* left, ASTNode* right);
ASTNode* create_unary_expression(ASTType op, ASTNode* operand);
ASTNode* create_identifier(char* name);
ASTNode* create_number(int value);
ASTNode* create_string(char* value);
ASTNode* create_char_literal(char* value);
ASTNode* create_float_number(char* value);
ASTNode* create_hex_number(char* value);
ASTNode* create_oct_number(char* value);
ASTNode* create_function_call(char* name, ASTNode* arguments);
ASTNode* create_array_access(ASTNode* array, ASTNode* index);
ASTNode* create_member_access(ASTNode* structure, char* member);
ASTNode* create_pointer_member_access(ASTNode* pointer, char* member);
ASTNode* create_cast_expression(ASTNode* type, ASTNode* expression);
ASTNode* create_initializer(ASTNode* value);
ASTNode* create_initializer_list(ASTNode** values, int count);
ASTNode* create_designator(char* member);
ASTNode* create_designator_list(ASTNode** designators, int count);
ASTNode* create_compound_literal(ASTNode* type, ASTNode* initializer);
ASTNode* create_type_specifier(char* type_name);
ASTNode* create_struct_declaration(char* name, ASTNode* members);
ASTNode* create_union_declaration(char* name, ASTNode* members);
ASTNode* create_enum_declaration(char* name, ASTNode* enumerators);
ASTNode* create_typedef_declaration(char* name, ASTNode* type);
ASTNode* create_parameter_declaration(ASTNode* type, char* name);
ASTNode* create_parameter_list(ASTNode** parameters, int count);
ASTNode* create_argument_list(ASTNode** arguments, int count);
ASTNode* create_type_name(ASTNode* specifiers, ASTNode* declarator);
ASTNode* create_abstract_declarator(ASTNode* pointer, ASTNode* direct);
ASTNode* create_direct_abstract_declarator(ASTNode* array, ASTNode* function);
ASTNode* create_type_qualifier_list(ASTNode** qualifiers, int count);
ASTNode* create_storage_class_specifier(char* storage_class);
ASTNode* create_declaration_specifiers(ASTNode* storage, ASTNode* type, ASTNode* qualifiers);
ASTNode* create_init_declarator_list(ASTNode** declarators, int count);
ASTNode* create_init_declarator(ASTNode* declarator, ASTNode* initializer);
ASTNode* create_struct_declarator_list(ASTNode** declarators, int count);
ASTNode* create_struct_declarator(ASTNode* declarator, ASTNode* bit_field);
ASTNode* create_enum_list(ASTNode** enumerators, int count);
ASTNode* create_enumerator(char* name, ASTNode* value);
ASTNode* create_constant_expression(ASTNode* expression);
ASTNode* create_assignment_expression(ASTNode* left, char* operator, ASTNode* right);
ASTNode* create_conditional_expression(ASTNode* condition, ASTNode* then_expr, ASTNode* else_expr);
ASTNode* create_logical_or_expression(ASTNode* left, ASTNode* right);
ASTNode* create_logical_and_expression(ASTNode* left, ASTNode* right);
ASTNode* create_inclusive_or_expression(ASTNode* left, ASTNode* right);
ASTNode* create_exclusive_or_expression(ASTNode* left, ASTNode* right);
ASTNode* create_and_expression(ASTNode* left, ASTNode* right);
ASTNode* create_equality_expression(ASTNode* left, char* operator, ASTNode* right);
ASTNode* create_relational_expression(ASTNode* left, char* operator, ASTNode* right);
ASTNode* create_shift_expression(ASTNode* left, char* operator, ASTNode* right);
ASTNode* create_additive_expression(ASTNode* left, char* operator, ASTNode* right);
ASTNode* create_multiplicative_expression(ASTNode* left, char* operator, ASTNode* right);
ASTNode* create_postfix_expression(ASTNode* expression, char* operator);
ASTNode* create_primary_expression(ASTNode* expression);

/* AST Manipulation Functions */
void add_child(ASTNode* parent, ASTNode* child);
void remove_child(ASTNode* parent, ASTNode* child);
ASTNode* get_child(ASTNode* parent, int index);
int get_child_count(ASTNode* parent);
void set_parent(ASTNode* child, ASTNode* parent);
ASTNode* get_parent(ASTNode* child);
void free_ast(ASTNode* node);
void free_ast_recursive(ASTNode* node);

/* AST Utility Functions */
void print_ast(ASTNode* node, int depth);
void print_ast_to_file(ASTNode* node, FILE* file, int depth);
ASTNode* clone_ast(ASTNode* node);
ASTNode* clone_ast_recursive(ASTNode* node);
int is_leaf(ASTNode* node);
int is_binary_operator(ASTType type);
int is_unary_operator(ASTType type);
int is_assignment_operator(ASTType type);
int is_logical_operator(ASTType type);
int is_relational_operator(ASTType type);
int is_arithmetic_operator(ASTType type);
int is_bitwise_operator(ASTType type);
int is_shift_operator(ASTType type);
int is_equality_operator(ASTType type);
int get_operator_precedence(ASTType type);
int get_operator_associativity(ASTType type);
char* get_operator_string(ASTType type);
ASTType get_operator_type(char* operator);

/* AST Search Functions */
ASTNode* find_node_by_type(ASTNode* root, ASTType type);
ASTNode* find_node_by_value(ASTNode* root, char* value);
ASTNode* find_function_definition(ASTNode* root, char* name);
ASTNode* find_variable_declaration(ASTNode* root, char* name);
ASTNode* find_type_declaration(ASTNode* root, char* name);
ASTNode* find_identifier_usage(ASTNode* root, char* name);

/* AST Validation Functions */
int validate_ast(ASTNode* root);
int validate_function_definition(ASTNode* node);
int validate_variable_declaration(ASTNode* node);
int validate_expression(ASTNode* node);
int validate_statement(ASTNode* node);
int validate_type_specifier(ASTNode* node);

/* AST Statistics Functions */
int count_nodes(ASTNode* root);
int count_nodes_by_type(ASTNode* root, ASTType type);
int count_function_definitions(ASTNode* root);
int count_variable_declarations(ASTNode* root);
int count_statements(ASTNode* root);
int count_expressions(ASTNode* root);
int get_max_depth(ASTNode* root);
int get_node_depth(ASTNode* node);

/* Memory Management */
void* ast_malloc(size_t size);
void* ast_calloc(size_t count, size_t size);
void* ast_realloc(void* ptr, size_t size);
void ast_free(void* ptr);
char* ast_strdup(const char* str);
char* ast_strndup(const char* str, size_t n);

/* Error Handling */
typedef enum {
    AST_ERROR_NONE,
    AST_ERROR_MEMORY,
    AST_ERROR_INVALID_TYPE,
    AST_ERROR_INVALID_VALUE,
    AST_ERROR_INVALID_CHILD_COUNT,
    AST_ERROR_INVALID_PARENT,
    AST_ERROR_INVALID_OPERATION,
    AST_ERROR_VALIDATION_FAILED
} ASTError;

ASTError get_last_ast_error(void);
const char* get_ast_error_string(ASTError error);
void clear_ast_errors(void);

/* Debug Functions */
void enable_ast_debug(void);
void disable_ast_debug(void);
int is_ast_debug_enabled(void);
void set_ast_debug_level(int level);
int get_ast_debug_level(void);

#endif /* AST_H */
