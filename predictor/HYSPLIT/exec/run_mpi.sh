#!/bin/sh
PATH=$PATH:/bin/utils:$HOME/utility:/usr/bin/X11:.:/usr/local/bin:/usr/abin:/usr/local/mpi:/usr/local/lib:/usr/local/mpi/bin
export PATH

#-------------------------------------------------------------
# Code designed for SPMD model (Single Program,Multiple Data) 
# which means that that the same code is executed on multiple 
# processors, each working on their own data. Sharing is 
# accomplished through MPI and not through memory. This script
# can be run stand-alone or from the GUI through the MPI tabs.
# The script may need to be customized for different platorms.
# Uncomment/edit sections below or move them to the top for
# execution as appropriate for the application.
#-------------------------------------------------------------

  EXEC="../exec"

  if [ $# -eq 3 ];then
     NPROC=$1
     MODEL=$2
     START=$3
  elif [ $# -eq 2 ];then
     NPROC=$1
     MODEL=$2
     START=" "   
  elif [ $# -eq 1 ];then
     NPROC=2
     MODEL=$1
     START=" "   
  else
     echo "Usage: $0 [#processors] [model] [arguments]"
     echo "Usage: $0 [#processors] [model]"
     echo "Usage: $0 [model]"
     exit
  fi

  MPIPATH="/usr/local/mpi/mpich2/bin"
  if ! [ -f ${MPIPATH}/mpirun ];then
     echo "mpirun not found in: ${MPIPATH}"
     echo "edit run_mpi.sh with new path!"
     exit 1
  fi
  echo "Running model ${MODEL} with ${NPROC} processors!"

#---------------------------------------------------
# for general UNIX systems use the following command
# where -np defines the number of processors
#---------------------------------------------------

${MPIPATH}/mpirun -np ${NPROC} ${EXEC}/${MODEL} ${START}
exit 0

#---------------------------------------------------
# for IBM's POE (parallel operating environment)
#---------------------------------------------------

cat >execfile<<EOF
${EXEC}/${MODEL} ${START}
EOF

cat >hostlist<<EOF
castor
pollux
EOF

export MP_HOSTFILE=hostlist
export MP_PROCS=${NPROC}
export MP_RESD=no
export MP_EULIB=us
export MP_TASKS_PER_NODE=2
export MP_PGMMODEL=spmd
export MP_CPU_USE=multiple
export MP_ADAPTER_USE=shared
export MP_NODES=1
export MP_RETRY_COUNT=100
export MP_RETRY=10
export MP_CMDFILE=execfile
poe

rm execfile
rm hostlist
exit 0

#----------------------------------------------
# LINUX qsub command
#----------------------------------------------

account=HYS_DEV

cat > qjob.sh <<EOF
#!/bin/sh
#PBS -j oe
#PBS -l nodes=${NPROC}
#PBS -N ${MODEL}
#PBS -V

cd \$PBS_O_WORKDIR

echo "---------------"
echo \$PBS_NODEFILE
cat  \$PBS_NODEFILE
echo "---------------"

${MPIPATH}/mpirun ${EXEC}/${MODEL} ${START}
EOF

qsub -A ${account} qjob.sh
exit 0
