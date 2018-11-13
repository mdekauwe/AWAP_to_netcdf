# Compiler/Linker settings
FC = ifort #gfortran
CFLAGS=-O2
#NCDIR='/opt/local/lib/'
#NCMOD='/opt/local/include/'
NCDIR=/share/apps/netcdf/intel/4.1.3/lib
NCMOD=/share/apps/netcdf/intel/4.1.3/include
LDFLAGS=-L/lib -O2 -L$(NCDIR) -lnetcdf -lnetcdff

## these are all the files we are compiling
LSRC = awap_to_netcdf.f90 type_def.F90 bios_io.F90 bios_output.F90 \
       cable_bios_met_obs_params.F90 cable_weathergenerator.F90

# this is the executable we are building
PROG = awap_to_netcdf

# from the compiled object files
OBJS	= ${LSRC:.F90=.o}

#suffixes we use
.SUFFIXES:
.SUFFIXES: .F90 .o

#default rules for these suffixes
.F90.o:
	$(FC) $(CFLAGS) -I$(NCMOD) -c $<

# default target by convention is ``all''
all : $(PROG)

#build PROG (cable executable) by linking all objects
#$(PROG) : $(OBJS)
$(PROG) : awap_to_netcdf.o
	$(FC) $(LDFLAGS) -o $@ $(OBJS)


# dependencies
type_def.o: type_def.F90
bios_io.o: bios_io.F90 type_def.o
cable_weathergenerator.o: cable_weathergenerator.F90
cable_bios_met_obs_params.o: cable_bios_met_obs_params.F90 type_def.o bios_io.o
bios_output.o: bios_output.F90 type_def.o bios_io.o cable_weathergenerator.o \
               cable_bios_met_obs_params.o
awap_to_netcdf.o: awap_to_netcdf.f90 type_def.o bios_io.o bios_output.o \
                  cable_weathergenerator.o cable_bios_met_obs_params.o


# make clean option
clean:
	rm -f *.o *.mod
