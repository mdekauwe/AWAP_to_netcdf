HOME =
LIBS =
INCLS =
FFLAGS =
FC = gfortran

# PROGRAM NAME
PROGRAM = awap_to_netcdf
# MODULE NAME
#MOD1 = constants
#MOD2 = errors
objects = $(PROGRAM).o #$(MOD1).o $(MOD2).o $(PROGRAM).o
############################################################

$(PROGRAM):	$(objects)
			$(FC) -o $@ $(OBJS) $(LIBS) $(INCLS)

#$(MOD1).o:	$(MOD1).f90
#			$(FC) -c $(MOD1).f90 ${INCLS} $(FFLAGS)

#$(MOD2).o:	$(MOD2).f90
#			$(FC) -c $(MOD2).f90 ${INCLS} $(FFLAGS)
clean:
	rm -f $(PROG) $(OBJS) *.mod
