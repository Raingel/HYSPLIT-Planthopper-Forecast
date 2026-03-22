#-----------------------------------------------------------------------------
# Makefile for GNU Make - top-level makefile
#
# Last Revised: 23 Aug 2019 - Initial.
#               27 Aug 2019 - Added install option, permission changes,
#                             and removal of fcsubs lib during clean.
#               16 Nov 2023 - Pass LDFLAGS_CPROG to the configure script
#                             when building ascii2shp.
#-----------------------------------------------------------------------------
# Usage examples:
# 1) make		- builds all libraries and executables.
# 2) make base		- builds a subset of libraries and executables.
# 3) make clean		- deletes object files, libraries, and executables
#			  that running make created.
# 4) make install	- links gui to working directory and sets GS_DEVICE 

include ./Makefile.inc

TODAY= $(shell date +%F)
DIR= $(shell pwd)
LOG= "${DIR}/make.${TODAY}.log"

FCSUBLIBA=lib$(FC)fcsubs.a

target: all

base: library/libhysplit.a library/liblbfgsb.a
	@echo "==============================================================================" >>${LOG}
	@echo "Building exec directory programs [`date`] " | tee -a ${LOG}
	@echo "------------------------------------------------------------------------------" >>${LOG}
	@cd ${DIR}/exec; \
	make 1>>${LOG} 2>>${LOG} || (echo "ERROR: see '${LOG}' for details"; exit 1)

all: library/$(FCSUBLIBA) library/libhysplit.a library/liblbfgsb.a
	@echo "==============================================================================" >>${LOG}
	@echo "Building exec directory programs [`date`] " | tee -a ${LOG}
	@echo "------------------------------------------------------------------------------" >>${LOG}
	@cd ${DIR}/exec; \
	make 1>>${LOG} 2>>${LOG} || (echo "ERROR: see '${LOG}' for details"; exit 1)
	@echo "==============================================================================" >>${LOG}
	@echo "Building the ascii2shp converter [`date`] " | tee -a ${LOG}
	@echo "------------------------------------------------------------------------------" >>${LOG}
	@cd ${DIR}/gisprog/arlshapes; \
	chmod u+x configure; \
	./configure CC=$(CC) CFLAGS="$(LDFLAGS_CPROG)" F77=$(FC) 1>>${LOG} 2>>${LOG}; \
	make 1>>${LOG} 2>>${LOG} || (echo "ERROR: see '${LOG}' for details"; exit 1)
	@cd ${DIR}/gisprog/arlshapes; \
	make install 1>>${LOG} 2>>${LOG} || (echo "ERROR: see '${LOG}' for details"; exit 1)
	@echo "==============================================================================" >>${LOG}
	@echo "Building the dbf editor [`date`] " | tee -a ${LOG}
	@echo "------------------------------------------------------------------------------" >>${LOG}
	@cd ${DIR}/gisprog/dbf/source; \
	make 1>>${LOG} 2>>${LOG} || (echo "ERROR: see '${LOG}' for details"; exit 1)
	@echo "==============================================================================" >>${LOG}
	@echo "Building the NARR decoder [`date`] " | tee -a ${LOG}
	@echo "------------------------------------------------------------------------------" >>${LOG}
	@cd ${DIR}/data2arl/narr2arl; \
	make 1>>${LOG} 2>>${LOG} || (echo "ERROR: see '${LOG}' for details"; exit 1)
	@echo "==============================================================================" >>${LOG}
	@echo "Building the WRF-ARW decoder (requires export NetCDF lib/include) [`date`] " | tee -a ${LOG}
	@echo "------------------------------------------------------------------------------" >>${LOG}
	@cd ${DIR}/data2arl/arw2arl; \
	make 1>>${LOG} 2>>${LOG} || (echo "ERROR: see '${LOG}' for details"; exit 1)
	@echo "==============================================================================" >>${LOG}
	@echo "Building the CMAQ converters (requires NetCDF & IOAPI) [`date`] " | tee -a ${LOG}
	@echo "------------------------------------------------------------------------------" >>${LOG}
	@cd ${DIR}/cmaq; \
	make con2cdf4 1>>${LOG} 2>>${LOG} || (echo "ERROR: see '${LOG}' for details"; exit 1)
	@cd ${DIR}/testing; \
	chmod u+x *.scr

library/$(FCSUBLIBA):
	@echo "==============================================================================" >>${LOG}
	@echo "Building fcsubs library [`date`] " | tee -a ${LOG}
	@echo "------------------------------------------------------------------------------" >>${LOG}
	@cd ${DIR}/library/fcsubs; \
	chmod u+x configure; \
	./configure CC=$(CC) F77=$(FC) 1>>${LOG} 2>>${LOG}; \
	make libdir=${DIR}/library install 1>>${LOG} 2>>${LOG} || (echo "ERROR: see '${LOG}' for details"; exit 1)

library/libhysplit.a:
	@echo "==============================================================================" >>${LOG}
	@echo "Building hysplit source library [`date`] " | tee -a ${LOG}
	@echo "------------------------------------------------------------------------------" >>${LOG}
	@cd ${DIR}/library/hysplit; \
	cp -p ../../source/version?.inc .; \
	make 1>>${LOG} 2>>${LOG} || (echo "ERROR: see '${LOG}' for details"; exit 1)

library/liblbfgsb.a:
	@echo "==============================================================================" >>${LOG}
	@echo "Building lbfgsb source library [`date`] " | tee -a ${LOG}
	@echo "------------------------------------------------------------------------------" >>${LOG}
	@cd ${DIR}/library/lbfgsb; \
	make 1>>${LOG} 2>>${LOG} || (echo "ERROR: see '${LOG}' for details"; exit 1)

clean:
	@for subdir in ${DIR}/library/fcsubs ${DIR}/library/hysplit ${DIR}/library/lbfgsb \
		${DIR}/gisprog/arlshapes \
		${DIR}/gisprog/dbf/source \
		${DIR}/data2arl/narr2arl ${DIR}/data2arl/arw2arl ${DIR}/cmaq ${DIR}/exec ; \
	do \
	    echo "Cleaning $${subdir} ..."; \
	    (cd $${subdir}; make clean) \
	done
	cd ${DIR}/library/fcsubs; make libdir=${DIR}/library clean

install:
	@cd ${DIR}/working; \
	ln -s ../guicode/hysplit.tcl hysplit.tcl; \
	chmod u+x hysplit.tcl; \
	export GS_DEVICE=x11
