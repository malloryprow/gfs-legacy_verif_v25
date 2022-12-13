SHELL=/bin/sh
#
#
SRCS=	selecsum.f

OBJS=	selecsum.o

# Tunable parameters
#
# FC		Name of the fortran compiling system to use
# LDFLAGS	Flags to the loader
# LIBS		List of libraries
# CMD		Name of the executable
# PROFLIB	Library needed for profiling
#
FC =		ifort
LDFLAGS =	-qopenmp
#LIBS =  -L/nwprod/lib -lw3emc_8 -lw3nco_8 -lip_8 -lbufr_8_64
LIBS = -L${w3emc_ROOT}/lib -lw3emc_8 -L${w3nco_ROOT}/lib -lw3nco_8 -L${ip_ROOT}/lib -lip_8 -L${bufr_ROOT}/lib64 -lbufr_8
CMD =		verf_selecsum
PROFLIB =	-lprof

# To perform the default compilation, use the first line
# To compile with flowtracing turned on, use the second line
# To compile giving profile additonal information, use the third line
# WARNING:  SIMULTANEOUSLY PROFILING AND FLOWTRACING IS NOT RECOMMENDED 

FFLAGS =        -fixed -i8 -r8


# Lines from here on down should not need to be changed.  They are the
# actual rules which make uses to build a.out.
#
all:		$(CMD)

$(CMD):		$(OBJS)
	$(FC) $(LDFLAGS) -o $(@) $(OBJS) $(LIB1) $(LIB2) $(LIB3)

# Make the profiled version of the command and call it a.out.prof
#
$(CMD).prof:	$(OBJS)
	$(FC) $(LDFLAGS) -o $(@) $(OBJS) $(PROFLIB) $(LIBS)

clean:
	-rm -f $(OBJS)

clobber:	clean
	-rm -f $(CMD) $(CMD).prof

void:	clobber
	-rm -f $(SRCS) makefile
