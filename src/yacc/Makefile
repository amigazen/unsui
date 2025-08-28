# Makefile for Berkeley Yacc (Amiga/UNSUI version)
# Based on original Amiga Makefile by William P Loftus

CC = gcc
CFLAGS = -Wall -Wextra -std=c89 -DAMIGA
LDFLAGS = 
LIBS = 

# Source files
SRCS = closure.c error.c lalr.c lr0.c main.c mkpar.c output.c reader.c skeleton.c symtab.c verbose.c warshall.c
OBJS = $(SRCS:.c=.o)

# Program name
PROGRAM = yacc

# Default target
all: $(PROGRAM)

# Build the program
$(PROGRAM): $(OBJS)
	@echo "Linking $(PROGRAM)..."
	$(CC) $(OBJS) $(LDFLAGS) $(LIBS) -o $(PROGRAM)
	@echo "done"

# Compile source files
%.o: %.c defs.h
	$(CC) $(CFLAGS) -c $< -o $@

# Clean up
clean:
	rm -f $(OBJS) $(PROGRAM)

# Install (optional)
install: $(PROGRAM)
	@echo "Installing $(PROGRAM)..."
	cp $(PROGRAM) /usr/local/bin/
	@echo "done"

# Dependencies
closure.o: closure.c defs.h
error.o: error.c defs.h
lalr.o: lalr.c defs.h
lr0.o: lr0.c defs.h
main.o: main.c defs.h
mkpar.o: mkpar.c defs.h
output.o: output.c defs.h
reader.o: reader.c defs.h
skeleton.o: skeleton.c defs.h
symtab.o: symtab.c defs.h
verbose.o: verbose.c defs.h
warshall.o: warshall.c defs.h

# Phony targets
.PHONY: all clean install
