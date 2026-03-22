#--------------------------------------------------------
# Master Makefile for exectables directory /exec
# Last Revised: 14 Nov 2003 - initial version
#               10 Jan 2005 - support for g95 
#               10 Nov 2005 - simplified directories
#               16 Oct 2006 - added chk_times
#               23 Jan 2007 - added wrf2arl
#               31 Jul 2007 - added dat2cntl
#               09 Aug 2007 - support for gfortran
#               16 Oct 2007 - added meds2arl converter
#		29 Jan 2008 - MPI trajectory hymodelp
#               30 May 2008 - added GEM option hymodelg
#               20 Aug 2008 - several additional programs
#               09 Oct 2008 - gfortran big-endian
#               14 Nov 2008 - renamed main executables
#               13 Feb 2009 - parallization options for SMP
#               02 Sep 2009 - added gen2xml and catps2ps
#               25 Sep 2009 - datem programs
#               14 Apr 2010 - added par2asc/asc2par
#               30 Apr 2010 - added conappend
#               28 May 2010 - added parshift
#               14 Jul 2010 - added clustering routines
#               19 Aug 2010 - revised and move to /exec
#               05 Oct 2010 - added con2rem for dose
#               19 Nov 2010 - added new program nmmb2arl
#               04 Mar 2011 - new program dustedit
#               09 Aug 2011 - new program vmsmerge
#               21 Sep 2011 - new program condecay
#               29 Sep 2011 - new program rap2arl (Rapid Refresh to replace RUC)
#               03 Nov 2011 - new program stat2grid
#               19 Apr 2012 - new program parsplot
#               15 Jun 2012 - additional pgf90 options
#               03 Jul 2012 - new program conedit
#               29 Aug 2012 - new program con2ctbt
#               16 Oct 2012 - intel compiler option change
#               25 Feb 2013 - new program conhavrg
#               05 Mar 2013 - new program con2srs
#               08 Mar 2013 - new program conmaxv
#               25 Jun 2013 - new programs c2array, tcsolve
#               29 Oct 2013 - new program concmbn
#               30 Oct 2013 - new programs par2conc, paro2n
#               04 Mar 2014 - new program conmaxpd
#               03 Sep 2014 - new program tcmsum
#               20 Nov 2014 - new program accudiv
#               08 Dec 2014 - new program poleplot
#               23 Dec 2014 - new program data_del
#               05 Feb 2015 - new program ensperc
#               05 Feb 2015 - new program enstala
#               06 Jan 2016 - new program nuctree and testnuc
#               18 Jul 2016 - new program hysptest
#               15 Aug 2016 - new program lbfgsb
#               29 Sep 2016 - new program macc2date
#               03 Jan 2017 - added library to conmerge.f compile
#               12 Jan 2017 - new program ecm2arl (corr. added Oct 06 2016)
#		26 Apr 2017 - new program var2datem
#		24 Jul 2018 - new program conavg
#               04 Feb 2019 - Copy from exec to data2arl/legacy, keeping only legacy code
#               06 Feb 2019 - few more legacy codes
#               26 Aug 2019 - use a Makefile.inc.
#--------------------------------------------------------

include ../../Makefile.inc

SHELL = /bin/sh

# directory location for source and libary
SRC  = .
LIB  = ../../library
EXE  = ../../exec

# directory with include files
FFLAGS += $(FFLAGS_FREEFORM) -I../../library/hysplit

#-----OPENMP compilation
#     hycm_gem \
#-----MPI dependent routines
#     hycm_std hycm_ens hycm_cb4 hytm_std \
#-----add to list below if required

all : afwa2arl avn2arl avn2gbl cmp2arl cmp3arl ecm2arl eta04arl eta12arl gdas2arl gfs2arl \
      gfslr2arl grad2arl grib2arl meds2arl nam40arl nams2arl ncr2arl nmm2arl nmmb2arl \
      pNA45 pNA15 pNA05 rap2arl rsmp2arl rsms2arl ruc2arl
 
afwa2arl : $(SRC)/afwa2arl.f 
	$(FC) $(FFLAGS) -o $(EXE)/$@ $(SRC)/$@.f -L$(LIB) $(CLIB) -lhysplit

avn2arl : $(SRC)/avn2arl.f 
	$(FC) $(FFLAGS) -o $(EXE)/$@ $(SRC)/$@.f -L$(LIB) $(CLIB) -lhysplit

avn2gbl : $(SRC)/avn2gbl.f 
	$(FC) $(FFLAGS) -o $(EXE)/$@ $(SRC)/$@.f -L$(LIB) $(CLIB) -lhysplit

cmp3arl : $(SRC)/cmp3arl.f
	$(FC) $(FFLAGS) -o $(EXE)/$@ $(SRC)/$@.f -L$(LIB) $(CLIB) -lhysplit

cmp2arl : $(SRC)/cmp2arl.f
	$(FC) $(FFLAGS) -o $(EXE)/$@ $(SRC)/$@.f -L$(LIB) $(CLIB) -lhysplit

ecm2arl : $(SRC)/grib2arl.f 
	$(FC) $(FFLAGS) -o $(EXE)/$@ $(SRC)/$@.f -L$(LIB) $(CLIB) -lhysplit

eta04arl : $(SRC)/eta04arl.f 
	$(FC) $(FFLAGS) -o $(EXE)/$@ $(SRC)/$@.f -L$(LIB) $(CLIB) -lhysplit

eta12arl : $(SRC)/eta12arl.f   
	$(FC) $(FFLAGS) -o $(EXE)/$@ $(SRC)/$@.f -L$(LIB) $(CLIB) -lhysplit

gdas2arl : $(SRC)/gdas2arl.f
	$(FC) $(FFLAGS) -o $(EXE)/$@ $(SRC)/$@.f -L$(LIB) $(CLIB) -lhysplit

gfs2arl : $(SRC)/gfs2arl.f 
	$(FC) $(FFLAGS) -o $(EXE)/$@ $(SRC)/$@.f -L$(LIB) $(CLIB) -lhysplit

gfslr2arl : $(SRC)/gfslr2arl.f
	$(FC) $(FFLAGS) -o $(EXE)/$@ $(SRC)/$@.f -L$(LIB) $(CLIB) -lhysplit

grad2arl : $(SRC)/grad2arl.f
	$(FC) -o $(EXE)/$@ $(FFLAGS) $(SRC)/$@.f -L$(LIB) -lhysplit

grib2arl : $(SRC)/grib2arl.f 
	$(FC) $(FFLAGS) -o $(EXE)/$@ $(SRC)/$@.f -L$(LIB) $(CLIB) -lhysplit

meds2arl : $(SRC)/meds2arl.f
	$(FC) $(FFLAGS) -o $(EXE)/$@ $(SRC)/$@.f -L$(LIB) $(CLIB) -lhysplit

nam40arl : $(SRC)/nam40arl.f   
	$(FC) $(FFLAGS) -o $(EXE)/$@ $(SRC)/$@.f -L$(LIB) $(CLIB) -lhysplit

nams2arl : $(SRC)/nams2arl.f   
	$(FC) $(FFLAGS) -o $(EXE)/$@ $(SRC)/$@.f -L$(LIB) $(CLIB) -lhysplit

ncr2arl : $(SRC)/ncr2arl.f 
	$(FC) $(FFLAGS) -o $(EXE)/$@ $(SRC)/$@.f -L$(LIB) $(CLIB) -lhysplit

nmm2arl : $(SRC)/nmm2arl.f 
	$(FC) $(FFLAGS) -o $(EXE)/$@ $(SRC)/$@.f -L$(LIB) $(CLIB) -lhysplit

nmmb2arl : $(SRC)/nmmb2arl.f   
	$(FC) $(FFLAGS) -o $(EXE)/$@ $(SRC)/$@.f -L$(LIB) $(CLIB) -lhysplit

pNA45 : $(SRC)/pNA45.f 
	$(FC) $(FFLAGS) -o $(EXE)/$@ $(SRC)/$@.f -L$(LIB) $(CLIB) -lhysplit

pNA15 : $(SRC)/pNA15.f 
	$(FC) $(FFLAGS) -o $(EXE)/$@ $(SRC)/$@.f -L$(LIB) $(CLIB) -lhysplit

pNA05 : $(SRC)/pNA05.f 
	$(FC) $(FFLAGS) -o $(EXE)/$@ $(SRC)/$@.f -L$(LIB) $(CLIB) -lhysplit

rap2arl : $(SRC)/rap2arl.f
	$(FC) $(FFLAGS) -o $(EXE)/$@ $(SRC)/$@.f -L$(LIB) $(CLIB) -lhysplit

rsmp2arl : $(SRC)/rsmp2arl.f 
	$(FC) $(FFLAGS) -o $(EXE)/$@ $(SRC)/$@.f -L$(LIB) $(CLIB) -lhysplit

rsms2arl : $(SRC)/rsms2arl.f
	$(FC) $(FFLAGS) -o $(EXE)/$@ $(SRC)/$@.f -L$(LIB) $(CLIB) -lhysplit

ruc2arl : $(SRC)/ruc2arl.f
	$(FC) $(FFLAGS) -o $(EXE)/$@ $(SRC)/$@.f -L$(LIB) $(CLIB) -lhysplit

clean :
	rm -f *.mod
	rm -f $(EXE)/afwa2arl
	rm -f $(EXE)/avn2arl
	rm -f $(EXE)/avn2gbl
	rm -f $(EXE)/cmp2arl
	rm -f $(EXE)/cmp3arl
	rm -f $(EXE)/ecm2arl
	rm -f $(EXE)/eta04arl
	rm -f $(EXE)/eta12arl
	rm -f $(EXE)/gdas2arl
	rm -f $(EXE)/gfs2arl
	rm -f $(EXE)/gfslr2arl
	rm -f $(EXE)/grad2arl
	rm -f $(EXE)/grib2arl
	rm -f $(EXE)/meds2arl
	rm -f $(EXE)/nam40arl
	rm -f $(EXE)/nams2arl
	rm -f $(EXE)/ncr2arl
	rm -f $(EXE)/nmm2arl
	rm -f $(EXE)/nmmb2arl
	rm -f $(EXE)/pNA45
	rm -f $(EXE)/pNA15
	rm -f $(EXE)/pNA05
	rm -f $(EXE)/rap2arl
	rm -f $(EXE)/rsmp2arl
	rm -f $(EXE)/rsms2arl
	rm -f $(EXE)/ruc2arl
