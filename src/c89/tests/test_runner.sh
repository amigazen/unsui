#!/bin/sh
#
# C99 to C89 Converter Test Runner
# Tests the converter with various C99 constructs
#
# Copyright (c) 2025 amigazen project. All rights reserved.

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Test configuration
CONVERTER="./c99c89"
TEST_DIR="../examples"
OUTPUT_DIR="./test_output"
EXPECTED_DIR="../examples"

# Test results
TOTAL_TESTS=0
PASSED_TESTS=0
FAILED_TESTS=0

# Function to print colored output
print_status() {
    local status=$1
    local message=$2
    
    case $status in
        "PASS")
            echo -e "${GREEN}[PASS]${NC} $message"
            ;;
        "FAIL")
            echo -e "${RED}[FAIL]${NC} $message"
            ;;
        "INFO")
            echo -e "${BLUE}[INFO]${NC} $message"
            ;;
        "WARN")
            echo -e "${YELLOW}[WARN]${NC} $message"
            ;;
    esac
}

# Function to run a single test
run_test() {
    local input_file=$1
    local test_name=$(basename "$input_file" .c99)
    local output_file="$OUTPUT_DIR/${test_name}.c89"
    local expected_file="$EXPECTED_DIR/expected_output.c89"
    
    TOTAL_TESTS=$((TOTAL_TESTS + 1))
    
    print_status "INFO" "Running test: $test_name"
    
    # Create output directory if it doesn't exist
    mkdir -p "$OUTPUT_DIR"
    
    # Run the converter
    if $CONVERTER "$input_file" "$output_file" >/dev/null 2>&1; then
        # Check if output file was created
        if [ -f "$output_file" ]; then
            # Basic validation: check if file contains expected C89 patterns
            if grep -q "int i;" "$output_file" || grep -q "malloc" "$output_file" || grep -q "temp_" "$output_file"; then
                print_status "PASS" "$test_name: Successfully converted to C89"
                PASSED_TESTS=$((PASSED_TESTS + 1))
            else
                print_status "FAIL" "$test_name: Output doesn't contain expected C89 patterns"
                FAILED_TESTS=$((FAILED_TESTS + 1))
            fi
        else
            print_status "FAIL" "$test_name: Output file not created"
            FAILED_TESTS=$((FAILED_TESTS + 1))
        fi
    else
        print_status "FAIL" "$test_name: Converter failed to run"
        FAILED_TESTS=$((FAILED_TESTS + 1))
    fi
}

# Function to run all tests
run_all_tests() {
    print_status "INFO" "Starting C99 to C89 converter tests..."
    print_status "INFO" "Test directory: $TEST_DIR"
    print_status "INFO" "Output directory: $OUTPUT_DIR"
    
    # Check if converter exists
    if [ ! -f "$CONVERTER" ]; then
        print_status "FAIL" "Converter not found: $CONVERTER"
        print_status "INFO" "Please build the converter first with 'smake'"
        exit 1
    fi
    
    # Find all C99 test files
    local test_files=$(find "$TEST_DIR" -name "*.c99" | sort)
    
    if [ -z "$test_files" ]; then
        print_status "WARN" "No C99 test files found in $TEST_DIR"
        exit 0
    fi
    
    print_status "INFO" "Found $(echo "$test_files" | wc -l) test files"
    
    # Run each test
    for test_file in $test_files; do
        run_test "$test_file"
    done
    
    # Print summary
    echo
    print_status "INFO" "Test Summary:"
    print_status "INFO" "Total tests: $TOTAL_TESTS"
    print_status "INFO" "Passed: $PASSED_TESTS"
    print_status "INFO" "Failed: $FAILED_TESTS"
    
    if [ $FAILED_TESTS -eq 0 ]; then
        print_status "PASS" "All tests passed!"
        exit 0
    else
        print_status "FAIL" "Some tests failed!"
        exit 1
    fi
}

# Function to clean test output
clean_test_output() {
    if [ -d "$OUTPUT_DIR" ]; then
        rm -rf "$OUTPUT_DIR"
        print_status "INFO" "Cleaned test output directory"
    fi
}

# Function to show help
show_help() {
    echo "C99 to C89 Converter Test Runner"
    echo
    echo "Usage: $0 [OPTIONS]"
    echo
    echo "Options:"
    echo "  -h, --help     Show this help message"
    echo "  -c, --clean    Clean test output directory"
    echo "  -v, --verbose  Verbose output"
    echo "  -t, --test     Run specific test (provide test name)"
    echo
    echo "Examples:"
    echo "  $0                    # Run all tests"
    echo "  $0 --clean           # Clean test output"
    echo "  $0 --test for_loop   # Run specific test"
}

# Main script logic
main() {
    local verbose=0
    local specific_test=""
    
    # Parse command line arguments
    while [ $# -gt 0 ]; do
        case $1 in
            -h|--help)
                show_help
                exit 0
                ;;
            -c|--clean)
                clean_test_output
                exit 0
                ;;
            -v|--verbose)
                verbose=1
                shift
                ;;
            -t|--test)
                if [ -n "$2" ]; then
                    specific_test="$2"
                    shift 2
                else
                    print_status "FAIL" "Test name required for --test option"
                    exit 1
                fi
                ;;
            *)
                print_status "FAIL" "Unknown option: $1"
                show_help
                exit 1
                ;;
        esac
    done
    
    # Run specific test if requested
    if [ -n "$specific_test" ]; then
        local test_file="$TEST_DIR/${specific_test}.c99"
        if [ -f "$test_file" ]; then
            run_test "$test_file"
            exit $?
        else
            print_status "FAIL" "Test file not found: $test_file"
            exit 1
        fi
    fi
    
    # Run all tests
    run_all_tests
}

# Run main function with all arguments
main "$@"
