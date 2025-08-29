# Makefile for Man (Amiga/UNSUI version)
# Based on ManMaker12 implementation by Mark Papadakis

CC = gcc
CFLAGS = -Wall -Wextra -std=c89 -DAMIGA
LDFLAGS = 
LIBS = 

# Source files
SRCS = man.c
OBJS = $(SRCS:.c=.o)

# Program name
PROGRAM = man

# Default target
all: $(PROGRAM)

# Build the program
$(PROGRAM): $(OBJS)
	@echo "Linking $(PROGRAM)..."
	$(CC) $(OBJS) $(LDFLAGS) $(LIBS) -o $(PROGRAM)
	@echo "done"

# Compile source files
%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

# Clean up
clean:
	rm -f $(OBJS) $(PROGRAM)

# Install (optional)
install: $(PROGRAM)
	@echo "Installing $(PROGRAM)..."
	cp $(PROGRAM) /usr/local/bin/
	@echo "done"

# Phony targets
.PHONY: all clean install

