HOME =
LIBS =
INCLS =
FFLAGS = -g -Wno-unused-dummy-argument -fbounds-check
FC = gfortran

# PROGRAM NAME
PROGRAM = awap_to_netcdf
SRCS = awap_to_netcdf.f90
# MODULE NAME
#MOD1 = constants
#MOD2 = errors
OBJS = 	$(SRCS:.f90=.o)
############################################################

$(PROGRAM):	$(OBJS)
			$(FC) -o $(PROGRAM) $(OBJS) $(LIBS) $(INCLS)

$(PROGRAM).o:	$(PROGRAM).f90 #$(MOD1).f90 $(MOD2).f90
			 	$(FC) -c $(PROGRAM).f90 ${INCLS} $(FFLAGS)

#$(MOD1).o:	$(MOD1).f90
#			$(FC) -c $(MOD1).f90 ${INCLS} $(FFLAGS)

#$(MOD2).o:	$(MOD2).f90
#			$(FC) -c $(MOD2).f90 ${INCLS} $(FFLAGS)
clean:
	rm -f $(PROG) $(OBJS) *.mod
