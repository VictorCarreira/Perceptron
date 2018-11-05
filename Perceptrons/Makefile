# preamble
# A simple hand-made makefile for a package including applications
# built from Fortran 90 sources, taking into account the usual
# dependency cases.

# This makefile works with the GNU make command, the one find on
# GNU/Linux systems and often called gmake on non-GNU systems, if you
# are using an old style make command, please see the file
# Makefile_oldstyle provided with the package.

# ======================================================================
# Let's start with the declarations
# ======================================================================

# The compiler
FC = gfortran
# flags for debugging or for maximum performance, comment as necessary
# If you are not on a 64bits machine remove -m64
FCFLAGS = -g -fbounds-check -fbacktrace -ffpe-trap=invalid,zero,overflow,underflow,denormal -Wall -Wextra -Wno-compare-reals -Wno-unused-function -m64
#FCFLAGS = -O2
# flags forall (e.g. look for system .mod files, required in gfortran)
#FCFLAGS += -I/usr/include

# libraries needed for linking, unused in the examples
#LDFLAGS = -li_need_this_lib
#LDFLAGS = -L$HOME/libf77/$ARCH -llapack -lblas


#Criando variáveis para compilação em subddiretórios
#######################
# Define directories: #
#######################

# PROG = programs's directory
# BLD = Build directory, where executable files (.o and .mod) will go
# TEST = Test programs directory

SRC  =  modules/
#TEST = tests/
PROG = programs/

# Directories where build files (.o and .mod) and executables will go:
BLD  = build/
EXE  = exe/


# List of executables to be built within the package
#PROGRAMS = perceptron #coloque aqui o nome do seu programa principal. Ele dará o no-
                      #me para o executável

# "make" builds all
#all: $(PROGRAMS)

##########################
# List all the .o files: #
##########################

OBJS = $(BLD)algebra.o $(BLD)activation.o 

##########################
# List all the testing programs: #
##########################

#TESTS = signal_test

##########################
# List all the main programs: #
##########################

PROGRAMS = perceptron
#test_gravmag_mod test_kernels_mod

#######################
# Define the targets: #
#######################

programs: $(PROGRAMS)
#tests: $(TESTS)


###############################
# Commands to build programs: #
###############################

#perceptron.o: activation.o algebra.o
#perceptron: activation.o algebra.o

perceptron: $(PROG)perceptron.f95 $(OBJS)
	$(FC) $(FCFLAGS) $(OBJS) -o $(EXE)perceptron $(PROG)perceptron.f95

##########################
# List the dependencies directiong objects: #
##########################

$(BLD)algebra.o: $(SRC)algebra.f95
	$(FC) $(FCFLAGS) -c $(SRC)algebra.f95 -o $(BLD)algebra.o 

$(BLD)activation.o: $(SRC)activation.f95
	$(FC) $(FCFLAGS) -c $(SRC)activation.f95 -o $(BLD)activation.o 




#$conclusion
# ======================================================================
# And now the general rules, these should not require modification
# ======================================================================

# General rule for building prog from prog.o; $^ (GNU extension) is
# used in order to list additional object files on which the
# executable depends
#%: %.o
#	$(FC) $(FCFLAGS) -o $@ $^ $(LDFLAGS)

# General rules for building prog.o from prog.f90 or prog.F90; $< is
# used in order to list only the first prerequisite (the source file)
# and not the additional prerequisites such as module or include files
#%.o: %.f95
#	$(FC) $(FCFLAGS) -c $<

#%.o: %.F95
#	$(FC) $(FCFLAGS) -c $<

# Utility targets
#.PHONY: clean veryclean


#clean:
#	rm -f *.o *.mod *.MOD sistema


#veryclean: clean
#	rm -f *~ $(PROGRAMS)


############################
# cleanning folders
############################
clean:
	rm -rf $(BLD)*.o $(BLD)*.mod

clear:
	rm -rf $(BLD)*.dSYM $(PROGRAMS) 


