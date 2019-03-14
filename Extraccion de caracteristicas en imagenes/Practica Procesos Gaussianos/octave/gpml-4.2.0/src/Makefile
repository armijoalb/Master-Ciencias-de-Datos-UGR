### 2) Choose Octave configuration ############################################
OCTAVE_INCLUDE ?= #-I/opt/octave-dev/include/octave-4.3.0+/octave/
MKOCTFILE      ?= mkoctfile #/opt/octave-dev/bin/mkoctfile

### 3) Choose L-BFGS-B configuration ##########################################
LBFGSB = lbfgsb.mex
LBFGSB_OBJS = solver_3_0.o linpack.o timer.o
LIBS        = -lblas

### 4) Choose FORTRAN compiler ################################################
FFLAGS       = -O3 -fPIC -fexceptions -Wall -g -Wno-uninitialized
F77         ?= gfortran
FORTRAN_LIB ?= gfortran

### 5) Flags for C++ compiler ####################################################
CXXFLAGS = -O3 -fPIC -pthread -Wall -Werror -ansi -ffast-math -fomit-frame-pointer -std=c++11

OBJS = matlabexception.o matlabscalar.o matlabstring.o \
       matlabmatrix.o arrayofmatrices.o program.o matlabprogram.o \
       lbfgsb.o

### minFunc
MINFUNC_MEXS = mcholC.mex lbfgsC.mex lbfgsAddC.mex lbfgsProdC.mex

%.o: %.cpp
	$(MKOCTFILE) $(CXXFLAGS) $(OCTAVE_INCLUDE) -c $< -o $@

%.o: %.f
	$(F77) $(FFLAGS) -c $< -o $@

%.mex: %.c
	$(MKOCTFILE) --mex $< -o $@

all: minFunc $(LBFGSB)

$(LBFGSB): $(LBFGSB_OBJS) $(OBJS)
	$(MKOCTFILE) --mex $^ $(LIBS) -o $@

minFunc: $(MINFUNC_MEXS)

clean:
	rm -f *.o

distclean: clean
	rm -f $(LBFGSB)
	rm -f $(MINFUNC_MEXS)

.PHONY: all clean distclean minfunc
