/*
 * C99 to C89 Converter - Transformation Functions
 * Declares functions for converting C99 constructs to C89 equivalents
 *
 * Copyright (c) 2025 amigazen project. All rights reserved.
 */

#ifndef TRANSFORM_H
#define TRANSFORM_H

#include "ast.h"

/* Transformation function declarations */

/* C99 For Loop Transformation */
ASTNode* transform_c99_for_loop(ASTNode* init, ASTNode* condition, ASTNode* increment, ASTNode* body);
ASTNode* extract_declaration_for_c89(ASTNode* declaration);
ASTNode* create_c89_for_loop(ASTNode* init_decl, ASTNode* condition, ASTNode* increment, ASTNode* body);

/* Compound Literal Transformation */
ASTNode* transform_compound_literal(ASTNode* type, ASTNode* initializer);
ASTNode* create_temporary_variable_declaration(ASTNode* type, char* base_name);
ASTNode* create_temporary_variable_assignment(char* var_name, ASTNode* initializer);
char* generate_temporary_variable_name(const char* base_name);

/* Designated Initializer Transformation */
ASTNode* transform_designated_initializer(ASTNode* designator_list);
ASTNode* convert_designated_to_positional(ASTNode* designator_list, ASTNode* type);
ASTNode* create_positional_initializer(ASTNode* type, ASTNode* values);

/* Variable Declaration in Statements Transformation */
ASTNode* transform_declaration_statement(ASTNode* declaration);
ASTNode* move_declaration_to_block_beginning(ASTNode* declaration, ASTNode* block);
ASTNode* create_declaration_block(ASTNode* declaration, ASTNode* statements);

/* Inline Function Transformation */
ASTNode* transform_inline_function(ASTNode* function_def);
ASTNode* remove_inline_keyword(ASTNode* function_def);
ASTNode* create_static_function(ASTNode* function_def);

/* Restrict Keyword Transformation */
ASTNode* transform_restrict_qualifier(ASTNode* declaration);
ASTNode* remove_restrict_keyword(ASTNode* declaration);

/* Variable Length Array Transformation */
ASTNode* transform_variable_length_array(ASTNode* array_decl);
ASTNode* convert_vla_to_dynamic_allocation(ASTNode* array_decl);
ASTNode* create_malloc_call(ASTNode* size_expression, ASTNode* element_type);
ASTNode* create_free_call(char* variable_name);

/* Mixed Declarations and Code Transformation */
ASTNode* transform_mixed_declarations(ASTNode* statement);
ASTNode* separate_declarations_and_code(ASTNode* statement);
ASTNode* create_declaration_block_with_code(ASTNode* declarations, ASTNode* code);

/* C++ Style Comments Transformation */
ASTNode* transform_cpp_comments(ASTNode* node);
ASTNode* convert_cpp_to_c_comments(ASTNode* node);

/* Preprocessor Directive Transformation */
ASTNode* transform_preprocessor_directives(ASTNode* directive);
ASTNode* handle_pragma_directive(ASTNode* directive);
ASTNode* handle_include_guard(ASTNode* directive);

/* Type Qualifier Transformation */
ASTNode* transform_type_qualifiers(ASTNode* qualifiers);
ASTNode* handle_restrict_qualifier(ASTNode* qualifier);
ASTNode* handle_const_qualifier(ASTNode* qualifier);
ASTNode* handle_volatile_qualifier(ASTNode* qualifier);

/* Function Parameter Transformation */
ASTNode* transform_function_parameters(ASTNode* parameters);
ASTNode* handle_restrict_parameters(ASTNode* parameter);
ASTNode* validate_parameter_types(ASTNode* parameter);

/* Expression Transformation */
ASTNode* transform_expressions(ASTNode* expression);
ASTNode* transform_binary_expressions(ASTNode* expression);
ASTNode* transform_unary_expressions(ASTNode* expression);
ASTNode* transform_assignment_expressions(ASTNode* expression);

/* Statement Transformation */
ASTNode* transform_statements(ASTNode* statement);
ASTNode* transform_compound_statements(ASTNode* statement);
ASTNode* transform_selection_statements(ASTNode* statement);
ASTNode* transform_iteration_statements(ASTNode* statement);
ASTNode* transform_jump_statements(ASTNode* statement);

/* Declaration Transformation */
ASTNode* transform_declarations(ASTNode* declaration);
ASTNode* transform_variable_declarations(ASTNode* declaration);
ASTNode* transform_function_declarations(ASTNode* declaration);
ASTNode* transform_type_declarations(ASTNode* declaration);

/* Type System Transformation */
ASTNode* transform_type_specifiers(ASTNode* type_spec);
ASTNode* transform_struct_declarations(ASTNode* struct_decl);
ASTNode* transform_union_declarations(ASTNode* union_decl);
ASTNode* transform_enum_declarations(ASTNode* enum_decl);
ASTNode* transform_typedef_declarations(ASTNode* typedef_decl);

/* Initializer Transformation */
ASTNode* transform_initializers(ASTNode* initializer);
ASTNode* transform_array_initializers(ASTNode* initializer);
ASTNode* transform_struct_initializers(ASTNode* initializer);
ASTNode* transform_union_initializers(ASTNode* initializer);

/* Error Handling and Validation */
int validate_transformation(ASTNode* original, ASTNode* transformed);
int check_c89_compatibility(ASTNode* node);
int report_transformation_issues(ASTNode* node);
void log_transformation(const char* from, const char* to, int line);

/* Utility Functions */
ASTNode* create_transformation_wrapper(ASTNode* original, ASTNode* transformed);
ASTNode* create_error_node(const char* error_message);
ASTNode* create_warning_node(const char* warning_message);
int count_transformations(ASTNode* root);
void print_transformation_summary(ASTNode* root);

/* Memory Management for Transformations */
void cleanup_transformation_data(ASTNode* node);
void free_transformation_wrappers(ASTNode* node);
void reset_transformation_counters(void);

/* Configuration and Options */
typedef struct {
    int preserve_comments;
    int generate_warnings;
    int strict_mode;
    int verbose_output;
    int debug_mode;
    char* output_format;
    int max_line_length;
    int indent_size;
    int use_tabs;
} TransformationOptions;

void set_transformation_options(TransformationOptions* options);
TransformationOptions* get_transformation_options(void);
void reset_transformation_options(void);

/* Statistics and Reporting */
typedef struct {
    int total_transformations;
    int for_loop_transformations;
    int compound_literal_transformations;
    int designated_init_transformations;
    int declaration_statement_transformations;
    int inline_function_transformations;
    int restrict_qualifier_transformations;
    int vla_transformations;
    int mixed_declaration_transformations;
    int cpp_comment_transformations;
    int errors_encountered;
    int warnings_generated;
} TransformationStats;

TransformationStats* get_transformation_stats(void);
void reset_transformation_stats(void);
void print_transformation_stats(void);
void export_transformation_stats(const char* filename);

/* Batch Transformation */
ASTNode* transform_entire_program(ASTNode* program);
int transform_file(const char* input_file, const char* output_file);
int transform_directory(const char* input_dir, const char* output_dir);
int transform_multiple_files(char** input_files, int count, const char* output_dir);

/* Quality Assurance */
int verify_transformation_correctness(ASTNode* original, ASTNode* transformed);
int run_transformation_tests(void);
int benchmark_transformation_performance(void);
void generate_transformation_report(const char* filename);

#endif /* TRANSFORM_H */
