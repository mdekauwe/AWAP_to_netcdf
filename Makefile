# Find all source files, create a list of corresponding object files
SRCS=$(wildcard *.f90)
OBJS=$(patsubst %.f90, %.o, $(SRCS))

# Ditto for mods
MODS=$(wildcard mod*.f90)
MOD_OBJS=$(patsubst %.f90, %.o, $(MODS))

# Compiler/Linker settings
FC = gfortran
FLFLAGS = -g
FCFLAGS = -g -c -Wall -Wno-tabs
PROGRAM = awap_to_netcdf
PRG_OBJ = $(PROGRAM).o

# Clean the suffixes
.SUFFIXES:

# Set the suffixes we are interested in
.SUFFIXES: .f90 .o

# make without parameters will make first target found.
default : $(PROGRAM)

# Compiler steps for all objects
$(OBJS) : %.o : %.f90
	$(FC) $(FCFLAGS) -o $@ $<

# Linker
$(PROGRAM) : $(OBJS)
	$(FC) $(FLFLAGS) -o $@ $^

clean:
	rm -rf $(OBJS) $(PROGRAM) $(patsubst %.o, %.mod, $(MOD_OBJS))

.PHONY: default clean

# Dependencies

# Main program depends on all modules
$(PRG_OBJ): $(MOD_OBJS)

# Blocks and allocations depends on shared
mod_blocks.o mod_allocations.o : mod_shared.o
