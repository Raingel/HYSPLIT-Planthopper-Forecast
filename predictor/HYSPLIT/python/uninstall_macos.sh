#!/bin/bash
# ------------------------------------------------------------------------
# uninstall_macos.sh - uninstall Python packages.
#
# 12 MAY 2020
# ------------------------------------------------------------------------

export ENVNAME=hysplit
if [ -d ~/opt/anaconda3 ]; then
    ANACONDA3_DIR=~/opt/anaconda3   # when the graphical installer is employed
elif [ -d ~/anaconda3 ]; then
    ANACONDA3_DIR=~/anaconda3       # when the command-line installer is employed
else
    echo "Cannot find where anaconda3 is installed in your home directory."
    exit 1
fi

source ${ANACONDA3_DIR}/etc/profile.d/conda.sh

echo "Removing anaconda environment $ENVNAME and packages therein ..."
conda deactivate
conda remove --name $ENVNAME --all
