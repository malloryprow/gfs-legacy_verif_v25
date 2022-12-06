SHELL=/bin/sh
#
#
SRCS=	s1sum.f 

OBJS=	s1sum.o 

# Tunable parameters
#
# CF		Name of the fortran compiling system to use
# LDFLAGS	Flags to the loader
# LIBS		List of libraries
# CMD		Name of the executable
# PROFLIB	Library needed for profiling
#
FC =            ifort
LDFLAGS =       -qopenmp
#LIBS =  -L/nwprod/lib -lw3emc_8 -lw3nco_8 -lip_8 -lsp_8 -lbufr_8_64 -lbacio_8
LIBS = -L${w3emc_ROOT}/lib -lw3emc_8 -L${w3nco_ROOT}/lib -lw3nco_8 -L${ip_ROOT}/lib -lip_8 -L${sp_ROOT}/lib -lsp_8 -L${bufr_ROOT}/lib64 -lbufr_8 -L${bacio_ROOT}/lib -lbacio_8
CMD =		verf_s1sum
PROFLIB =	-lprof

FFLAGS =        -fixed -i8 -r8

# Lines from here on down should not need to be changed.  They are the
# actual rules which make uses to build a.out.
#
all:		$(CMD)

$(CMD):		$(OBJS)
	$(FC) $(LDFLAGS) -o $(@) $(OBJS) $(LIBS)

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
