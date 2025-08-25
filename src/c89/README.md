# C99 to C89 Converter

A powerful preprocessor that converts modern C99 code to C89-compatible code for compilation with legacy compilers like SAS/C.

## Overview

This tool uses **Flex** (lexical analysis) and **Bison** (parsing) to:
1. **Parse C99 syntax** - Handle all modern C99 constructs
2. **Transform code** - Convert C99 features to C89 equivalents
3. **Generate C89 output** - Produce SAS/C-compatible code

## Why This Tool?

- **SAS/C Compatibility** - Output works with SAS/C compiler
- **Modern Development** - Write C99 code, compile with legacy tools
- **Gradual Migration** - Convert existing codebases incrementally
- **UNSUI Integration** - Part of the unsui POSIX runtime

## Supported C99→C89 Conversions

### 1. Variable Declarations in Loops
```c
// C99: for (int i = 0; i < n; i++)
// C89: { int i; for (i = 0; i < n; i++) }
```

### 2. Compound Literals
```c
// C99: return (struct point){1, 2};
// C89: { struct point temp = {1, 2}; return temp; }
```

### 3. Designated Initializers
```c
// C99: struct point p = {.x = 1, .y = 2};
// C89: struct point p = {1, 2};
```

### 4. Variable Length Arrays
```c
// C99: int array[n];
// C89: int* array = malloc(n * sizeof(int));
```

### 5. Inline Functions
```c
// C99: inline int square(int x) { return x * x; }
// C89: /* Remove inline keyword, keep function */
```

### 6. Mixed Declarations and Code
```c
// C99: if (int x = get_value()) { ... }
// C89: { int x = get_value(); if (x) { ... } }
```

## Project Structure

```
src/c89/
├── README.md              # This file
├── c99c89.y              # Bison grammar file
├── c99c89.l              # Flex lexer file
├── ast.h                 # Abstract syntax tree definitions
├── ast.c                 # AST manipulation functions
├── transform.h            # Transformation function declarations
├── transform.c            # C99→C89 transformation logic
├── generator.h            # Code generation declarations
├── generator.c            # C89 code output functions
├── main.c                # Main program entry point
├── main.h                # Consolidated headers
├── SMakefile             # Build configuration
├── examples/             # Test cases and examples
│   ├── for_loop.c99
│   ├── compound_literal.c99
│   ├── designated_init.c99
│   ├── vla.c99
│   └── expected_output.c89
└── tests/                # Test suite
    ├── test_runner.sh
    └── test_cases/
```

## Dependencies

- **Flex 2.5.1** - Lexical analysis (imported to `/src/flex/`)
- **Bison 1.25** - Parser generation (imported to `/src/bison/`)
- **SAS/C compiler** - For building on Amiga

## Building

### Prerequisites
1. **Flex** must be built and installed first
2. **Bison** must be built and installed second
3. **C89 converter** can then be built

### Build Order
```bash
cd src
smake flex      # Build and install Flex
smake bison     # Build and install Bison
smake c89       # Build C99→C89 converter
```

### Build Commands
```bash
# Build from project root
cd src && smake c89

# Build individual components
cd src/flex && smake
cd src/bison && smake
cd src/c89 && smake
```

## Usage

### Basic Conversion
```bash
# Convert single file
c99c89 input.c99 output.c89

# Convert with auto-generated output name
c99c89 input.c99  # Creates input.c89
```

### Command Line Options
```bash
c99c89 [OPTIONS] INPUT_FILE [OUTPUT_FILE]

OPTIONS:
  -h, --help     Display help information
  -v, --verbose  Be verbose about transformations
  -V, --version  Display version information
  -o FILE        Specify output file name
  -t, --test     Run in test mode
```

### Example Workflow
```bash
# 1. Write C99 code
cat > example.c99 << 'EOF'
int main() {
    for (int i = 0; i < 10; i++) {
        struct point p = (struct point){i, i*i};
        printf("(%d, %d)\n", p.x, p.y);
    }
    return 0;
}
EOF

# 2. Convert to C89
c99c89 example.c99

# 3. Compile with SAS/C
sc example.c89 -o example
```

## How It Works

### Phase 1: Lexical Analysis (Flex)
- **Tokenizes** C99 source code
- **Recognizes** C99-specific keywords and constructs
- **Handles** comments, strings, numbers, identifiers

### Phase 2: Parsing (Bison)
- **Builds** abstract syntax tree (AST)
- **Handles** C99 grammar rules
- **Creates** structured representation of code

### Phase 3: Transformation
- **Identifies** C99 constructs that need conversion
- **Applies** transformation rules
- **Modifies** AST to C89-compatible form

### Phase 4: Code Generation
- **Traverses** transformed AST
- **Generates** C89-compatible C code
- **Outputs** formatted, compilable source

## Transformation Examples

### Complex For Loop
```c
// Input (C99)
for (int i = 0, j = 10; i < j; i++, j--) {
    printf("%d %d\n", i, j);
}

// Output (C89)
{
    int i, j;
    for (i = 0, j = 10; i < j; i++, j--) {
        printf("%d %d\n", i, j);
    }
}
```

### Nested Compound Literals
```c
// Input (C99)
struct complex {
    struct point center;
    double radius;
} c = (struct complex){
    .center = (struct point){0, 0},
    .radius = 5.0
};

// Output (C89)
struct complex {
    struct point center;
    double radius;
} c;
{
    struct point temp_1 = {0, 0};
    c.center = temp_1;
    c.radius = 5.0;
}
```

## Testing

### Test Suite
```bash
cd src/c89/tests
./test_runner.sh
```

### Test Cases Include
- **Basic conversions** - Simple C99→C89 transformations
- **Complex scenarios** - Nested structures and expressions
- **Edge cases** - Error handling and boundary conditions
- **Performance tests** - Large file processing

### Validation
- **Compilation** - Ensure output compiles with SAS/C
- **Functionality** - Verify output produces same results
- **Standards** - Confirm C89 compliance

## Integration with UNSUI

### Build System
- **SMakefile integration** - Part of main project build
- **Dependency management** - Proper build order handling
- **Installation targets** - System-wide deployment

### Usage in UNSUI Tools
```bash
# Convert C99 code before compilation
c99c89 tool.c99 tool.c89
sc tool.c89 -o tool

# Batch conversion for multiple files
for file in *.c99; do
    c99c89 "$file"
done
```

## Future Enhancements

### Planned Features
1. **C++ to C conversion** - Handle C++ constructs
2. **Optimization passes** - Code optimization during conversion
3. **Configuration files** - Customizable transformation rules
4. **IDE integration** - Editor plugins and extensions

### Extensibility
- **Plugin architecture** - Custom transformation rules
- **Language support** - Additional input/output languages
- **Format options** - Multiple output styles

## Troubleshooting

### Common Issues
1. **Flex not found** - Ensure Flex is built and installed
2. **Bison not found** - Ensure Bison is built and installed
3. **Parse errors** - Check C99 syntax validity
4. **Output won't compile** - Verify SAS/C compatibility

### Debug Mode
```bash
# Enable verbose output
c99c89 -v input.c99

# Check intermediate AST
c99c89 --debug input.c99
```

## Contributing

### Development Setup
1. **Clone** the repository
2. **Build** dependencies (Flex, Bison)
3. **Modify** source code
4. **Test** changes thoroughly
5. **Submit** pull request

### Code Style
- **C89 compliance** - All code must compile with SAS/C
- **Documentation** - Comprehensive comments and docs
- **Testing** - Test cases for all new features

## Support

For issues and questions:
1. **Check documentation** - This README and inline code comments
2. **Review test cases** - Examples of expected behavior
3. **Examine transformations** - Understand conversion logic
4. **Submit issues** - Report bugs and request features