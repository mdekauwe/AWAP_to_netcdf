#!/bin/bash

#PBS -m ae
#PBS -P w35
#PBS -q express
#PBS -l walltime=0:2:00
#PBS -l mem=10GB
#PBS -l ncpus=1
#PBS -j oe
#PBS -l wd
#PBS -l other=gdata1

./awap_to_netcdf
