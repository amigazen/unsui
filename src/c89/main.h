/*
 * C99 to C89 Converter - Main Header
 * Consolidates all includes and function prototypes
 *
 * Copyright (c) 2025 amigazen project. All rights reserved.
 */

#ifndef C99C89_MAIN_H
#define C99C89_MAIN_H

/* System includes */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stdarg.h>
#include <time.h>

/* Amiga includes */
#include <proto/dos.h>
#include <dos/dos.h>
#include <dos/rdargs.h>
#include <exec/types.h>
#include <exec/memory.h>
#include <utility/utility.h>

/* Local includes */
#include "ast.h"
#include "transform.h"
#include "generator.h"

/* Version information */
#define C99C89_VERSION "1.0.0"
#define C99C89_VERSION_MAJOR 1
#define C99C89_VERSION_MINOR 0
#define C99C89_VERSION_PATCH 0

/* Build information */
#define C99C89_BUILD_DATE __DATE__
#define C99C89_BUILD_TIME __TIME__
#define C99C89_COMPILER "SAS/C"
#define C99C89_TARGET "Amiga"

/* Configuration constants */
#define MAX_LINE_LENGTH 1024
#define MAX_IDENTIFIER_LENGTH 256
#define MAX_STRING_LENGTH 4096
#define MAX_COMMENT_LENGTH 8192
#define MAX_ERROR_MESSAGES 100
#define MAX_WARNING_MESSAGES 100
#define DEFAULT_INDENT_SIZE 4
#define DEFAULT_TAB_SIZE 8
#define MAX_NESTING_DEPTH 100
#define MAX_FUNCTION_PARAMETERS 32
#define MAX_FUNCTION_ARGUMENTS 64
#define MAX_ARRAY_DIMENSIONS 16
#define MAX_STRUCT_MEMBERS 256
#define MAX_ENUM_VALUES 256
#define MAX_TYPEDEF_NAMES 128
#define MAX_INCLUDE_PATHS 32
#define MAX_DEFINE_MACROS 256
#define MAX_CONDITIONAL_LEVELS 32
#define MAX_PRAGMA_DIRECTIVES 64

/* Error codes */
#define C99C89_SUCCESS 0
#define C99C89_ERROR_INVALID_ARGS -1
#define C99C89_ERROR_FILE_OPEN -2
#define C99C89_ERROR_FILE_READ -3
#define C99C89_ERROR_FILE_WRITE -4
#define C99C89_ERROR_PARSE_FAILED -5
#define C99C89_ERROR_TRANSFORM_FAILED -6
#define C99C89_ERROR_GENERATE_FAILED -7
#define C99C89_ERROR_MEMORY -8
#define C99C89_ERROR_INVALID_SYNTAX -9
#define C99C89_ERROR_UNSUPPORTED_FEATURE -10
#define C99C89_ERROR_INTERNAL -11

/* Warning codes */
#define C99C89_WARNING_NONE 0
#define C99C89_WARNING_DEPRECATED_FEATURE 1
#define C99C89_WARNING_POTENTIAL_ISSUE 2
#define C99C89_WARNING_STYLE_VIOLATION 3
#define C99C89_WARNING_PERFORMANCE_ISSUE 4
#define C99C89_WARNING_COMPATIBILITY_ISSUE 5

/* Global variables */
extern int verbose_mode;
extern int debug_mode;
extern int warning_level;
extern int error_count;
extern int warning_count;
extern char* input_filename;
extern char* output_filename;
extern FILE* input_file;
extern FILE* output_file;
extern ASTNode* program_ast;

/* Command line parsing */
typedef struct {
    char* input_file;
    char* output_file;
    int verbose;
    int debug;
    int help;
    int version;
    int test_mode;
    int preserve_comments;
    int generate_warnings;
    int strict_mode;
    int indent_size;
    int use_tabs;
    int max_line_length;
    char* include_paths[MAX_INCLUDE_PATHS];
    int include_path_count;
    char* define_macros[MAX_DEFINE_MACROS];
    int define_macro_count;
} CommandLineOptions;

/* Function prototypes */

/* Main program functions */
int main(int argc, char* argv[]);
int process_command_line(int argc, char* argv[], CommandLineOptions* options);
void print_usage(const char* program_name);
void print_version(void);
void print_help(void);

/* File handling */
int open_input_file(const char* filename);
int open_output_file(const char* filename);
void close_files(void);
int read_input_file(void);
int write_output_file(void);
int validate_file_permissions(const char* filename);
int create_backup_file(const char* filename);

/* Error handling */
void report_error(const char* format, ...);
void report_warning(const char* format, ...);
void report_info(const char* format, ...);
void report_debug(const char* format, ...);
void set_error_location(int line, int column, const char* filename);
void clear_errors(void);
int has_errors(void);
int has_warnings(void);
void print_error_summary(void);

/* Memory management */
void* safe_malloc(size_t size);
void* safe_calloc(size_t count, size_t size);
void* safe_realloc(void* ptr, size_t size);
void safe_free(void* ptr);
char* safe_strdup(const char* str);
char* safe_strndup(const char* str, size_t n);
void cleanup_memory(void);
void check_memory_leaks(void);

/* String utilities */
char* duplicate_string(const char* str);
char* concatenate_strings(const char* str1, const char* str2);
char* format_string(const char* format, ...);
char* trim_whitespace(const char* str);
char* to_lowercase(const char* str);
char* to_uppercase(const char* str);
int string_ends_with(const char* str, const char* suffix);
int string_starts_with(const char* str, const char* prefix);
char* replace_string(const char* str, const char* old, const char* new);

/* File utilities */
char* get_file_extension(const char* filename);
char* get_file_basename(const char* filename);
char* get_file_directory(const char* filename);
char* change_file_extension(const char* filename, const char* new_ext);
int file_exists(const char* filename);
int is_readable_file(const char* filename);
int is_writable_file(const char* filename);
int is_directory(const char* pathname);
long get_file_size(const char* filename);
time_t get_file_modification_time(const char* filename);

/* Path utilities */
char* normalize_path(const char* path);
char* join_paths(const char* path1, const char* path2);
char* get_absolute_path(const char* relative_path);
char* get_relative_path(const char* from_path, const char* to_path);
int is_absolute_path(const char* path);
int is_relative_path(const char* path);

/* Configuration */
typedef struct {
    int max_line_length;
    int indent_size;
    int use_tabs;
    int preserve_comments;
    int generate_warnings;
    int strict_mode;
    int verbose_output;
    int debug_mode;
    char* output_format;
    char* include_paths[MAX_INCLUDE_PATHS];
    int include_path_count;
    char* define_macros[MAX_DEFINE_MACROS];
    int define_macro_count;
    char* pragma_directives[MAX_PRAGMA_DIRECTIVES];
    int pragma_directive_count;
} Configuration;

void load_configuration(const char* config_file);
void save_configuration(const char* config_file);
void set_default_configuration(void);
Configuration* get_configuration(void);
void free_configuration(void);

/* Statistics and reporting */
typedef struct {
    int total_files_processed;
    int total_lines_processed;
    int total_characters_processed;
    int total_functions_found;
    int total_variables_found;
    int total_statements_found;
    int total_expressions_found;
    int total_preprocessor_directives_found;
    int total_comments_found;
    int total_c99_features_found;
    int total_transformations_performed;
    int total_errors_encountered;
    int total_warnings_generated;
    double total_processing_time_seconds;
    double average_processing_time_per_file;
} ProcessingStats;

ProcessingStats* get_processing_stats(void);
void reset_processing_stats(void);
void update_processing_stats(void);
void print_processing_stats(void);
void export_processing_stats(const char* filename);

/* Testing and validation */
int run_self_tests(void);
int validate_input_file(const char* filename);
int validate_output_file(const char* filename);
int test_transformation_functions(void);
int test_code_generation_functions(void);
int benchmark_performance(void);
void generate_test_report(const char* filename);

/* Logging */
typedef enum {
    LOG_LEVEL_ERROR,
    LOG_LEVEL_WARNING,
    LOG_LEVEL_INFO,
    LOG_LEVEL_DEBUG,
    LOG_LEVEL_TRACE
} LogLevel;

void set_log_level(LogLevel level);
LogLevel get_log_level(void);
void enable_logging_to_file(const char* log_filename);
void disable_logging_to_file(void);
void log_message(LogLevel level, const char* format, ...);
void log_error(const char* format, ...);
void log_warning(const char* format, ...);
void log_info(const char* format, ...);
void log_debug(const char* format, ...);
void log_trace(const char* format, ...);

/* Signal handling */
void setup_signal_handlers(void);
void cleanup_signal_handlers(void);
void handle_interrupt_signal(int signal);
void handle_segmentation_fault(int signal);
void handle_abort_signal(int signal);

/* Utility functions */
int is_power_of_two(unsigned int n);
int round_up_to_power_of_two(unsigned int n);
int count_set_bits(unsigned int n);
int is_prime(unsigned int n);
unsigned int next_prime(unsigned int n);
int gcd(int a, int b);
int lcm(int a, int b);
double degrees_to_radians(double degrees);
double radians_to_degrees(double radians);

/* Time utilities */
double get_current_time(void);
double get_elapsed_time(double start_time);
char* format_duration(double seconds);
char* get_current_timestamp(void);
void sleep_milliseconds(int milliseconds);

/* Exit codes */
#define EXIT_SUCCESS 0
#define EXIT_FAILURE 1
#define EXIT_USAGE_ERROR 2
#define EXIT_FILE_ERROR 3
#define EXIT_PARSE_ERROR 4
#define EXIT_TRANSFORM_ERROR 5
#define EXIT_GENERATE_ERROR 6
#define EXIT_MEMORY_ERROR 7
#define EXIT_INTERNAL_ERROR 8

/* Cleanup function */
void cleanup_and_exit(int exit_code);

#endif /* C99C89_MAIN_H */
