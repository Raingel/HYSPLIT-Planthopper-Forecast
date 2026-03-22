@rem ------------------------------------------------------------------------
@rem install_win10.bat - installs Python packages for Windows 10.
@rem
@rem 6 MAY 2021
@rem
@rem Anaconda3 must have been installed prior to running this script.
@rem ------------------------------------------------------------------------

@echo off

set envname=hysplit

@rem The pkgs_win10.txt file was created with a hysplit anaconda3 environment
@rem that had all required packages via conda list --explicit > pkgs_win10.txt.

if not exist pkgs_win10.txt (
    echo File pkgs_win10.txt not found. If this file was accidentally deleted,
    echo HYSPLIT needs to be installed again.
    exit /B 2
) else (
    echo Creating anaconda environment %envname% ...
    call conda create --name %envname% --file pkgs_win10.txt python=3.7
)

call conda activate %envname%

echo Installing HYSPLITDATA ...
@cd hysplitdata
@python setup.py clean install --quiet --force
@cd ..

echo Installing HYSPLITPLOT ...
@cd hysplitplot
@python setup.py clean install --quiet --force
@cd ..

@rem timezonefinder 5.0.0 and later include ocean timezones.
@rem No need to additionally install a global timezone database.
