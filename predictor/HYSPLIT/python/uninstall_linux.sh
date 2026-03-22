#!/bin/bash
# ------------------------------------------------------------------------
# uninstall_linux.sh - uninstall Python packages.
#
# 13 FEB 2020
# ------------------------------------------------------------------------

export ENVNAME=hysplit

source ~/anaconda3/etc/profile.d/conda.sh

echo "Removing anaconda environment $ENVNAME and packages therein ..."
conda deactivate
conda remove --name $ENVNAME --all
