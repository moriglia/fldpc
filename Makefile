F_SRC_ROOT = src
F_TEST     = test
BUILD ?= build
O_LIB = $(BUILD)/lib
O_INC = $(BUILD)/inc
O_LIBOBJ = $(BUILD)/libobj

O_TEST = $(BUILD)/test
O_EXAMPLES = $(BUILD)/examples

OPTIMIZE ?= -O3
COMPILER ?= gfortran

LIB = $(HOME)/.local/lib
INCLUDES = $(HOME)/.local/include


.PHONY: all library examples clean test install

all: library install
library: $(O_LIB)/libfldpc.a


# Missing directory
%/:
	mkdir -p $@


# Library
$(O_LIBOBJ)/ldpc_decoder.o : $(O_LIBOBJ)/ldpc_edge_list.o
$(O_INC)/%.mod $(O_LIBOBJ)/%.o : $(F_SRC_ROOT)/%.f90 $(O_LIBOBJ)/ $(O_INC)/ 
	$(COMPILER) $(OPTIMIZE) -c $< -J$(O_INC) -I$(O_INC) -I$(INCLUDES) -L$(LIB) -o $@


LDPC_MODS = ldpc_edge_list ldpc_decoder
LDPC_OBJS = $(patsubst %, $(O_LIBOBJ)/%.o, $(LDPC_MODS))
$(O_LIB)/libfldpc.a : $(LDPC_OBJS) $(O_LIB)/
	ar r $@ $(LDPC_OBJS)


# Test
TEST_OBJS = $(patsubst $(F_TEST)/%.f90, $(O_TEST)/testobj/%.o, $(wildcard $(F_TEST)/*.f90))
$(O_TEST)/testobj/%.o: $(F_TEST)/%.f90 $(O_TEST)/testobj/ $(O_LIB)/libfldpc.a
	$(COMPILER) -o $@ -c $< -I$(O_INC) -I$(INCLUDES) -L$(LIB) \
		-J$(O_TEST)/testobj -I$(O_TEST)/testobj \
		-ltest-drive -lfldpc
$(O_TEST)/testobj/main.o: $(TEST_OBJS)
$(O_TEST)/main: $(TEST_OBJS)
	$(COMPILER) $^ -o $@ \
		-I$(O_INC) -I$(INCLUDES) -L$(LIB) \
		-I$(O_TEST)/testobj \
		-ltest-drive -lfldpc
test: $(O_TEST)/main
	$(O_TEST)/main




# Examples
EXAMPLE_LIST = $(patsubst examples/%.f90, $(O_EXAMPLES)/%, $(wildcard examples/*.f90))
examples: $(EXAMPLE_LIST)
$(O_EXAMPLES)/%: examples/%.f90 $(O_LIB)/libfldpc.a $(O_EXAMPLES)/
	$(COMPILER) $< -o $@ \
		-I$(O_INC) -I$(INCLUDES) -L$(LIB) -L$(O_LIB)\
		-lfldpc -lfortran_stdlib -lIO-Fortran-Library
	$@

clean:
	rm -rf build/


MODS = $(patsubst %, $(O_INC)/%.mod, $(LDPC_MODS))
install: $(O_LIB)/libfldpc.a $(MODS)
	cp $(O_LIB)/libfldpc.a $(LIB)/
	cp $(O_INC)/*.mod $(INCLUDES)/
