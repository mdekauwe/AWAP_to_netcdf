#!/bin/bash

#PBS -m ae
#PBS -P dt6
#PBS -q normalbw
#PBS -l walltime=0:50:00
#PBS -l mem=256GB
#PBS -l ncpus=1
#PBS -j oe
#PBS -l wd
#PBS -l other=gdata1

./awap_to_netcdf
