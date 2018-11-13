#!/bin/bash
module unload netcdf
module load netcdf/4.2.1.1
module load  intel-fc/17.0.1.132 netcdf
export NCDIR=$NETCDF_ROOT'/lib/Intel'
export NCMOD=$NETCDF_ROOT'/include/Intel'
export FC=$F90
export CFLAGS='-O0 -fp-model precise'
export LDFLAGS='-L'$NCDIR' -O0'
export LD='-lnetcdf -lnetcdff'

make -f Makefile_offline
