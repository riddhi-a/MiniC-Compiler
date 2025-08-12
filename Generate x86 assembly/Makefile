# Makefile for TAC and x86 generation executables

# Compiler
CXX = g++
CXXFLAGS = -lfl

# Targets
EXECUTABLES = a.out tac

# Source files for a.out (x86 generation)
X86_SOURCES = a3.tab.c x86_lex.yy.c
X86_EXECUTABLE = a.out

# Source files for tac generation
TAC_SOURCES = tac.tab.c tac_lex.yy.c
TAC_EXECUTABLE = tac

# Default target
all: $(EXECUTABLES)

# Build rules for a.out
$(X86_EXECUTABLE): $(X86_SOURCES)
	$(CXX) -o $@ $^ $(CXXFLAGS)

# Build rules for tac
$(TAC_EXECUTABLE): $(TAC_SOURCES)
	$(CXX) -o $@ $^ $(CXXFLAGS)

# Rules for generating parser files for x86
a3.tab.c a3.tab.h: a3.y
	bison -d a3.y

# Rules for generating parser files for TAC
tac.tab.c tac.tab.h: tac.y
	bison -d tac.y

# Generate lexer file for x86
x86_lex.yy.c: a3.l a3.tab.h
	flex -o x86_lex.yy.c a3.l

# Generate lexer file for TAC
tac_lex.yy.c: tac.l tac.tab.h
	flex -o tac_lex.yy.c tac.l

# Clean up build files
clean:
	rm -f x86_lex.yy.c a3.tab.c a3.tab.h tac_lex.yy.c tac.tab.c tac.tab.h $(EXECUTABLES)

# Phony targets
.PHONY: all clean

