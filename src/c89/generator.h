/*
 * C99 to C89 Converter - Code Generation
 * Declares functions for outputting C89-compatible C code from the transformed AST
 *
 * Copyright (c) 2025 amigazen project. All rights reserved.
 */

#ifndef GENERATOR_H
#define GENERATOR_H

#include "ast.h"

/* Code generation function declarations */

/* Main Generation Functions */
int generate_c89_code(ASTNode* ast, const char* output_file);
int generate_c89_code_to_stream(ASTNode* ast, FILE* output_stream);
char* generate_c89_code_to_string(ASTNode* ast);

/* Program Structure Generation */
int generate_program(ASTNode* program, FILE* output, int indent_level);
int generate_external_declarations(ASTNode* declarations, FILE* output, int indent_level);
int generate_function_definitions(ASTNode* functions, FILE* output, int indent_level);

/* Function Generation */
int generate_function_definition(ASTNode* function, FILE* output, int indent_level);
int generate_function_declaration(ASTNode* function, FILE* output, int indent_level);
int generate_function_parameters(ASTNode* parameters, FILE* output);
int generate_function_body(ASTNode* body, FILE* output, int indent_level);
int generate_return_statement(ASTNode* return_stmt, FILE* output, int indent_level);

/* Declaration Generation */
int generate_declarations(ASTNode* declarations, FILE* output, int indent_level);
int generate_variable_declaration(ASTNode* declaration, FILE* output, int indent_level);
int generate_type_declaration(ASTNode* declaration, FILE* output, int indent_level);
int generate_struct_declaration(ASTNode* struct_decl, FILE* output, int indent_level);
int generate_union_declaration(ASTNode* union_decl, FILE* output, int indent_level);
int generate_enum_declaration(ASTNode* enum_decl, FILE* output, int indent_level);
int generate_typedef_declaration(ASTNode* typedef_decl, FILE* output, int indent_level);

/* Statement Generation */
int generate_statements(ASTNode* statements, FILE* output, int indent_level);
int generate_compound_statement(ASTNode* compound_stmt, FILE* output, int indent_level);
int generate_expression_statement(ASTNode* expr_stmt, FILE* output, int indent_level);
int generate_selection_statement(ASTNode* selection_stmt, FILE* output, int indent_level);
int generate_iteration_statement(ASTNode* iteration_stmt, FILE* output, int indent_level);
int generate_jump_statement(ASTNode* jump_stmt, FILE* output, int indent_level);
int generate_labeled_statement(ASTNode* labeled_stmt, FILE* output, int indent_level);

/* Control Flow Generation */
int generate_if_statement(ASTNode* if_stmt, FILE* output, int indent_level);
int generate_switch_statement(ASTNode* switch_stmt, FILE* output, int indent_level);
int generate_case_statement(ASTNode* case_stmt, FILE* output, int indent_level);
int generate_default_statement(ASTNode* default_stmt, FILE* output, int indent_level);
int generate_for_loop(ASTNode* for_loop, FILE* output, int indent_level);
int generate_while_loop(ASTNode* while_loop, FILE* output, int indent_level);
int generate_do_while_loop(ASTNode* do_while_loop, FILE* output, int indent_level);
int generate_break_statement(ASTNode* break_stmt, FILE* output, int indent_level);
int generate_continue_statement(ASTNode* continue_stmt, FILE* output, int indent_level);

/* Expression Generation */
int generate_expressions(ASTNode* expressions, FILE* output, int indent_level);
int generate_binary_expression(ASTNode* binary_expr, FILE* output);
int generate_unary_expression(ASTNode* unary_expr, FILE* output);
int generate_assignment_expression(ASTNode* assign_expr, FILE* output);
int generate_conditional_expression(ASTNode* cond_expr, FILE* output);
int generate_logical_expression(ASTNode* logical_expr, FILE* output);
int generate_relational_expression(ASTNode* relational_expr, FILE* output);
int generate_arithmetic_expression(ASTNode* arithmetic_expr, FILE* output);
int generate_bitwise_expression(ASTNode* bitwise_expr, FILE* output);
int generate_shift_expression(ASTNode* shift_expr, FILE* output);
int generate_equality_expression(ASTNode* equality_expr, FILE* output);

/* Primary Expression Generation */
int generate_primary_expression(ASTNode* primary_expr, FILE* output);
int generate_identifier(ASTNode* identifier, FILE* output);
int generate_number(ASTNode* number, FILE* output);
int generate_string(ASTNode* string, FILE* output);
int generate_char_literal(ASTNode* char_lit, FILE* output);
int generate_float_number(ASTNode* float_num, FILE* output);
int generate_hex_number(ASTNode* hex_num, FILE* output);
int generate_oct_number(ASTNode* oct_num, FILE* output);
int generate_function_call(ASTNode* func_call, FILE* output);
int generate_array_access(ASTNode* array_access, FILE* output);
int generate_member_access(ASTNode* member_access, FILE* output);
int generate_pointer_member_access(ASTNode* ptr_member_access, FILE* output);
int generate_cast_expression(ASTNode* cast_expr, FILE* output);
int generate_parenthesized_expression(ASTNode* paren_expr, FILE* output);

/* Type Generation */
int generate_type_specifiers(ASTNode* type_spec, FILE* output);
int generate_type_qualifiers(ASTNode* qualifiers, FILE* output);
int generate_storage_class_specifiers(ASTNode* storage_spec, FILE* output);
int generate_type_name(ASTNode* type_name, FILE* output);
int generate_abstract_declarator(ASTNode* abstract_decl, FILE* output);
int generate_direct_abstract_declarator(ASTNode* direct_decl, FILE* output);

/* Initializer Generation */
int generate_initializers(ASTNode* initializers, FILE* output, int indent_level);
int generate_initializer(ASTNode* initializer, FILE* output);
int generate_initializer_list(ASTNode* init_list, FILE* output, int indent_level);
int generate_array_initializer(ASTNode* array_init, FILE* output, int indent_level);
int generate_struct_initializer(ASTNode* struct_init, FILE* output, int indent_level);
int generate_union_initializer(ASTNode* union_init, FILE* output, int indent_level);

/* Preprocessor Directive Generation */
int generate_preprocessor_directives(ASTNode* directives, FILE* output);
int generate_include_directive(ASTNode* include_dir, FILE* output);
int generate_define_directive(ASTNode* define_dir, FILE* output);
int generate_ifdef_directive(ASTNode* ifdef_dir, FILE* output);
int generate_ifndef_directive(ASTNode* ifndef_dir, FILE* output);
int generate_endif_directive(ASTNode* endif_dir, FILE* output);
int generate_else_directive(ASTNode* else_dir, FILE* output);
int generate_pragma_directive(ASTNode* pragma_dir, FILE* output);

/* Comment Generation */
int generate_comments(ASTNode* comments, FILE* output);
int generate_c_style_comment(const char* comment, FILE* output);
int generate_line_comment(const char* comment, FILE* output);

/* Formatting and Indentation */
int generate_indentation(FILE* output, int indent_level);
int generate_newline(FILE* output);
int generate_space(FILE* output);
int generate_tab(FILE* output);
int generate_semicolon(FILE* output);
int generate_comma(FILE* output);
int generate_parentheses(FILE* output, int open);
int generate_braces(FILE* output, int open);
int generate_brackets(FILE* output, int open);

/* Error Handling */
typedef enum {
    GENERATOR_ERROR_NONE,
    GENERATOR_ERROR_FILE_OPEN,
    GENERATOR_ERROR_WRITE_FAILED,
    GENERATOR_ERROR_INVALID_AST,
    GENERATOR_ERROR_UNSUPPORTED_NODE,
    GENERATOR_ERROR_MEMORY,
    GENERATOR_ERROR_FORMATTING
} GeneratorError;

GeneratorError get_last_generator_error(void);
const char* get_generator_error_string(GeneratorError error);
void clear_generator_errors(void);

/* Configuration and Options */
typedef struct {
    int indent_size;
    int use_tabs;
    int max_line_length;
    int add_trailing_semicolons;
    int add_braces_around_single_statements;
    int preserve_original_formatting;
    int generate_line_numbers;
    int generate_source_location_comments;
    char* output_encoding;
    int verbose_output;
    int debug_mode;
} GeneratorOptions;

void set_generator_options(GeneratorOptions* options);
GeneratorOptions* get_generator_options(void);
void reset_generator_options(void);

/* Statistics and Reporting */
typedef struct {
    int total_lines_generated;
    int total_characters_generated;
    int functions_generated;
    int variables_generated;
    int statements_generated;
    int expressions_generated;
    int preprocessor_directives_generated;
    int comments_generated;
    int errors_encountered;
    int warnings_generated;
    double generation_time_seconds;
} GeneratorStats;

GeneratorStats* get_generator_stats(void);
void reset_generator_stats(void);
void print_generator_stats(void);
void export_generator_stats(const char* filename);

/* Quality Assurance */
int validate_generated_code(const char* generated_code);
int check_c89_syntax(const char* code);
int run_generated_code_tests(const char* code);
int benchmark_generation_performance(void);
void generate_code_quality_report(const char* filename);

/* Utility Functions */
char* escape_string_for_c(const char* input);
char* format_identifier(const char* identifier);
char* format_number(const char* number);
char* format_operator(const char* operator);
int is_valid_c89_identifier(const char* identifier);
int is_valid_c89_constant(const char* constant);

/* Memory Management */
void cleanup_generator_data(void);
void free_generated_strings(void);
void reset_generator_counters(void);

#endif /* GENERATOR_H */
