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
FCFLAGS += -I/usr/include

# libraries needed for linking, unused in the examples
#LDFLAGS = -li_need_this_lib
#LDFLAGS = -L$HOME/libf77/$ARCH -llapack -lblas

# List of executables to be built within the package
PROGRAMS = perceptron #coloque aqui o nome do seu programa principal. Ele dará o no-
                      #me para o executável

# "make" builds all
all: $(PROGRAMS)

#$intro
# ======================================================================
# Here comes the most interesting part: the rules for prog1, prog2,
# prog3 and prog4, modify to suit your needs
# ======================================================================

# In order to understand the next section, the process of building an
# executable has to be clear: this is typically done in two steps:

# Using Fortran MODULES: perceptron.f95 USEs a Fortran module defined
# inside mod.f95, there is no standard naming convention for compiled
# module files in f95, the dependency is more easily built on the object
# files, because when mod.o is generated, one is sure that
# any_module_inside_mod.mod has been newly generated as well; mod.o
# must also be linked in when building the executable, so the
# dependency on mod.o is added also for prog4, as in the external

perceptron.o: activation.o algebra.o
perceptron: activation.o algebra.o

#$conclusion
# ======================================================================
# And now the general rules, these should not require modification
# ======================================================================

# General rule for building prog from prog.o; $^ (GNU extension) is
# used in order to list additional object files on which the
# executable depends
%: %.o
	$(FC) $(FCFLAGS) -o $@ $^ $(LDFLAGS)

# General rules for building prog.o from prog.f90 or prog.F90; $< is
# used in order to list only the first prerequisite (the source file)
# and not the additional prerequisites such as module or include files
%.o: %.f95
	$(FC) $(FCFLAGS) -c $<

%.o: %.F95
	$(FC) $(FCFLAGS) -c $<

# Utility targets
.PHONY: clean veryclean

clean:
	rm -f *.o *.mod *.MOD sistema

veryclean: clean
	rm -f *~ $(PROGRAMS)
