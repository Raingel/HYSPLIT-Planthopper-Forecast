#!/bin/bash
# ------------------------------------------------------------------------
# install_macos.sh - installs Python packages for MacOS.
#
# 6 MAY 2021
#
# Anaconda3 must have been installed prior to running this script.
# ------------------------------------------------------------------------

# Name of anaconda3 environment for Hysplit graphics
ENVNAME=hysplit

function die() {
  local msg=$1

  echo "$msg"
  exit 1
}

if [ -d ~/opt/anaconda3 ]; then
    ANACONDA3_DIR=~/opt/anaconda3   # when the graphical installer is employed
elif [ -d ~/anaconda3 ]; then
    ANACONDA3_DIR=~/anaconda3       # when the command-line installer is employed
else
    die "Cannot find where anaconda3 is installed in your home directory."
fi
source ${ANACONDA3_DIR}/etc/profile.d/conda.sh

# Test if all required commands are available.
hash awk   > /dev/null 2>&1 || die "ERROR: command awk is missing."
hash conda > /dev/null 2>&1 || die "ERROR: command conda is missing."
hash cp    > /dev/null 2>&1 || die "ERROR: command cp is missing."
hash ln    > /dev/null 2>&1 || die "ERROR: command ln is missing."
hash mkdir > /dev/null 2>&1 || die "ERROR: command mkdir is missing."
hash unzip > /dev/null 2>&1 || die "ERROR: command unzip is missing."
#hash wget  > /dev/null 2>&1 || die "ERROR: command wget is missing."

# The pkgs_linux.txt file was created with a hysplit anaconda3 environment
# that had all required packages via conda list --explicit > pkgs_linux.txt.

if [ ! -r pkgs_linux.txt ]; then
    echo "File pkgs_linux.txt not found. If this file was accidentally deleted,"
    echo "HYSPLIT needs to be installed again."
    exit 2
else
    echo "Creating anaconda environment $ENVNAME ..."
    conda create --name $ENVNAME --file pkgs_macos.txt python=3.7
fi

conda activate $ENVNAME || die "Cannot activate the hysplit environment. Please check if anaconda3 is installed at ${ANACONDA3_DIR}."

# Find and check the location of the hysplit anaconda3 environment
ENVPATH=`conda info -e | grep -e "^${ENVNAME} " | awk '{if ($2=="*") print $NF;}'`
echo "Path to the $ENVNAME environment is $ENVPATH."
if [ -z "${ENVPATH}" ]; then
  die "ERROR: could not determine pathname for anaconda3 environment $ENVNAME."
elif [ ! -d "${ENVPATH}" ]; then
  die "ERROR: directory for anaconda3 environment $ENVNAME does not exist."
fi

echo "Installing HYSPLITDATA ..."
cd hysplitdata
python setup.py clean install --quiet --force
cd ..

echo "Installing HYSPLITPLOT ..."
cd hysplitplot
python setup.py clean install --quiet --force
cd ..

# timezonefinder 5.0.0 and later include ocean timezones.
# No need to additionally install a global timezone database.

exit 0
