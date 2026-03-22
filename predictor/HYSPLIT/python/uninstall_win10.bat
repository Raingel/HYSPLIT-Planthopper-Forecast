@rem ------------------------------------------------------------------------
@rem uninstall_win10.bat - uninstall Python packages for Windows 10.
@rem
@rem 12 FEB 2020
@rem ------------------------------------------------------------------------

@echo off

set envname=hysplit

echo Removing anaconda environment %envname% and packages therein ...
@call conda deactivate
@call conda remove --name %envname% --all
