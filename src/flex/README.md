# Flex 2.5.4 - Fast Lexical Analyzer Generator

## Overview

This directory contains the **Flex 2.5.4** lexical analyzer generator, ported to Amiga OS using the SAS/C compiler. Flex is a tool for generating fast lexical analyzers (scanners) for text processing.

## What Flex Does

Flex reads a specification file (`.l` file) and generates a C source file that implements a lexical analyzer. The generated scanner recognizes patterns in text input and can perform actions when patterns are matched.

## Project Structure

```
src/flex/
├── main.h              # Consolidated header with all includes and prototypes
├── SMakefile           # Build configuration for SAS/C compiler
├── SCOPTIONS           # SAS/C compiler options
├── FlexLexer.h         # C++ header for Flex lexer classes
├── Src/                # Source code directory
│   ├── main.c          # Main program entry point
│   ├── scan.l          # Flex lexer specification
│   ├── parse.y         # Bison grammar specification
│   ├── flexdef.h       # Main header definitions
│   ├── config.h        # Configuration header
│   ├── version.h       # Version information
│   ├── dfa.c           # Deterministic finite automaton
│   ├── nfa.c           # Nondeterministic finite automaton
│   ├── gen.c           # Code generation
│   ├── misc.c          # Utility functions
│   ├── sym.c           # Symbol table management
│   ├── tblcmp.c        # Table compression
│   ├── ecs.c           # Equivalence classes
│   ├── ccl.c           # Character class handling
│   ├── yylex.c         # Lexer interface
│   ├── alloca.c        # Stack allocation
│   ├── libmain.c       # Library main function
│   ├── libyywrap.c     # Library yywrap function
│   ├── skel.c          # Generated skeleton code
│   └── flex.skl        # Skeleton template
├── Obj/                # Object files directory
├── fastwc/             # Fast word count example
└── Docs/               # Documentation
```

## Dependencies

- **SAS/C compiler** (version 6.57+)
- **Bison** - Parser generator (for building parse.c from parse.y)
- **Amiga OS development environment**

## Building

### Prerequisites
1. **Bison** must be built and installed first
2. **Flex** can then be built

### Build Commands
```bash
# Build from project root
cd src && smake flex

# Build individual component
cd src/flex && smake

# Build bootflex (preliminary version)
cd src/flex && smake bootflex

# Build final version
cd src/flex && smake flex
```

### Build Process
1. **bootflex**: Creates a preliminary version without using flex itself
2. **flex**: Creates the final version using the bootflex version
3. **check**: Verifies the build is working correctly
4. **install**: Installs flex binary and library files

## Usage

### Basic Usage
```bash
# Generate scanner from .l file
flex input.l

# Generate scanner with specific output file
flex -o scanner.c input.l

# Generate scanner with performance report
flex -p input.l
```

### Command Line Options
```bash
flex [OPTIONS] [FILE]

OPTIONS:
  -t, --stdout          Write to stdout instead of lex.yy.c
  -o, --outfile=FILE    Write output to FILE
  -P, --prefix=PREFIX   Use PREFIX instead of yy
  -l, --lex-compat      Maximal compatibility with original lex
  -n, --no-line         Don't generate #line directives
  -p, --perf-report     Write performance report to lex.yy.perf
  -s, --nodefault       Don't generate default rule
  -v, --verbose         Write summary of scanner statistics
  -w, --nowarn          Don't generate warnings
  -B, --batch           Generate batch scanner (opposite of -I)
  -I, --interactive     Generate interactive scanner
  -7, --7bit            Generate 7-bit scanner
  -8, --8bit            Generate 8-bit scanner
  -+, --c++             Generate C++ scanner class
  -C[aefFmr]            Scanner options:
    -Ca, --align        Trade off larger tables for better memory alignment
    -Ce, --ecs          Construct equivalence classes
    -Cf, --full         Use full scanner tables
    -CF, --fast         Use fast scanner tables
    -Cm, --meta-ecs     Construct meta-equivalence classes
    -Cr, --read         Use read() instead of stdio
```

## Examples

### Simple Lexer Example
```c
/* example.l */
%%
[0-9]+      { printf("Number: %s\n", yytext); }
[a-zA-Z]+   { printf("Word: %s\n", yytext); }
[ \t\n]     ; /* Skip whitespace */
.           { printf("Unknown: %s\n", yytext); }
%%

int yywrap(void) { return 1; }
int main(void) {
    yylex();
    return 0;
}
```

### Building the Example
```bash
# Generate scanner
flex example.l

# Compile with SAS/C
sc lex.yy.c -o example

# Run
example < input.txt
```

## Integration with UNSUI Project

This flex implementation is integrated with the UNSUI project and can be used by other tools that require lexical analysis:

- **C99 to C89 Converter** - Uses flex for tokenizing C99 source code
- **Other language processors** - Can be used for any text processing tasks

## Build Targets

- **all**: Build flex binary (default)
- **bootflex**: Build preliminary version
- **flex**: Build final version
- **check**: Verify build is working
- **install**: Install flex and library files
- **clean**: Remove object files and binaries

## Troubleshooting

### Common Issues
1. **Bison not found**: Ensure bison is built and in PATH
2. **Parse errors**: Check that parse.y is valid Bison grammar
3. **Memory issues**: Check SCOPTIONS for memory settings

### Build Verification
```bash
# Run self-check
smake check

# Test with sample input
echo "test 123" | ./Flex
```

## License

Flex is distributed under the BSD license. See the source files for copyright information.
