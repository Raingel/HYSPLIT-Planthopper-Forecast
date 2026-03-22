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
#               04 Feb 2019 - move legacy code to data2arl/legacy
#               06 Feb 2019 - more legacy code to data2arl/legacy
#               21 May 2019 - add con2inv
#               23 Aug 2019 - moved compiler options and library
#                             definitions to ../Makefile.inc.
#               26 Aug 2019 - remove the INC variable.
#               28 Aug 2019 - update MPI compilation process.
#               30 Oct 2019 - rebuild programs when libhyplit.a is updated.
#               23 Feb 2021 - add drn2arl and add MINC for MPI compilation
#               11 May 2021 - add aer2arl
#               17 Jun 2021 - link with the standard c++ library too.
#               23 Jul 2021 - add cat2svg and splitsvg.
#               14 Oct 2021 - link with libhysplitcxx.a.
#                             build GRIB-dependent executables if GRIBUSE
#                             is true.
#               20 Jan 2022 - omit building display and coversheet.
#               26 Jan 2022 - use FFLAGS_STRICT instead of FFLAGS.
#               16 Nov 2023 - use LDFLAGS_FPROG when building lbfgsb.
#--------------------------------------------------------

# compiler specific flags
include ../Makefile.inc

# modify options for the FORTRAN and C++ compilers
FFLAGS_STRICT += $(FFLAGS_FREEFORM) -I../library/hysplit
CXXFLAGS += -I../library/hysplit

SHELL = /bin/sh

# directory location for source and libary
SRC = ../source
LIB = ../library

ADDITIONAL_EXE = ascii2shp dbf2txt txt2dbf
HYSPLIT_LIB = $(LIB)/libhysplit.a
HYSPLIT_CXX_LIB = $(LIB)/libhysplitcxx.a

LIB_LINK = -L$(LIB) -lhysplit
LIB_CXX_LINK = -L$(LIB) -lhysplitcxx -lstdc++ -lhysplit

#-----OPENMP compilation
#     hycm_gem \

#-----GRIB dependent executables
ifeq ($(GRIBUSE),TRUE)
    GRIB_EXE = con2grib con2grib2
else
    GRIB_EXE =
endif

#-----MPI dependent executables
ifeq ($(MPIUSE),TRUE)
    MPI_EXE = hycm_std hycm_ens hycm_cb4 hytm_std
else
    MPI_EXE = 
endif

all : $(MPI_EXE) \
      $(GRIB_EXE) \
      con2asc conavg con2ctbt con2stn con2rem condecay con2arcv con2grad \
      con2dose concrop concadd concmbn concsum concacc coninfo \
      conread conedit constats conprob con2inv conlight conhavrg con2srs conmaxv \
      hycs_std hycs_ens hycs_var hycs_gem hyts_std hyts_ens \
      hycs_ier hycs_grs hycs_so2 hycs_cb4 \
      parshift trajfreq trajgrad trajmerg trajfind latlon catps2ps gen2xml \
      trajplot volcplot concplot timeplot gridplot boxplots isochron \
      par2asc parmerge parvplot parhplot parsplot parxplot par2conc paro2n \
      ensplots conavgpd conmaxpd poleplot \
      asc2par confreq stn2par matrix dustedit fires firew \
      chk_data chk_file chk_index chk_rec chk_times data_avrg file_merge \
      gelabel edit_flux edit_head edit_index edit_miss edit_null file_copy \
      arl2grad trajfrmt conmerge conmask conpuff data_year \
      pole2merc rec_insert rec_merge stabplot stn2ge vmixing velvar \
      xtrct_grid xtrct_stn xtrct_time zcoord profile contour \
      metdates timeplus add_velv add_data add_time add_grid add_miss \
      filedates datesmry datecol showgrid rec_copy edit_flux gridxy2ll \
      stn2arl snd2arl drn2arl aer2arl \
      inventory content unpacker findgrib metpoint \
      prntbdy dustbdy dat2cntl conappend \
      arl2meds goes2ems c2datem scatter statmain \
      cluster clusend cluslist clusmem clusplot merglist trajmean sfc2arl \
      vmsread vmsmerge stat2grid constnlst c2array tcsolve tcmsum accudiv \
      data_del ensperc enstala nuctree testnuc hysptest lbfgsb macc2date \
      metlatlon var2datem mergextr \
      cat2svg splitsvg

con2grib : $(SRC)/con2grib.f $(HYSPLIT_LIB)
	$(FC) -o $@ -I$(GRIB_INC) $(FFLAGS_STRICT) $(SRC)/$@.f $(LIB_LINK) $(GRIB_LINK)

con2grib2 : $(SRC)/con2grib2.f $(HYSPLIT_LIB)
	$(FC) -o $@ -I$(GRIB_INC) $(FFLAGS_STRICT) $(SRC)/$@.f $(LIB_LINK) $(GRIB_LINK)

con2asc : $(SRC)/con2asc.f $(HYSPLIT_LIB)
	$(FC) -o $@ $(FFLAGS_STRICT) $(SRC)/$@.f $(LIB_LINK) 

conavg : $(SRC)/conavg.f $(HYSPLIT_LIB)
	$(FC) -o $@ $(FFLAGS_STRICT) $(SRC)/$@.f $(LIB_LINK) 

con2ctbt : $(SRC)/con2ctbt.f $(HYSPLIT_LIB)
	$(FC) -o $@ $(FFLAGS_STRICT) $(SRC)/$@.f $(LIB_LINK)

con2srs : $(SRC)/con2srs.f $(HYSPLIT_LIB)
	$(FC) -o $@ $(FFLAGS_STRICT) $(SRC)/$@.f $(LIB_LINK)

con2rem : $(SRC)/con2rem.f $(HYSPLIT_LIB)
	$(FC) -o $@ $(FFLAGS_STRICT) $(SRC)/$@.f $(LIB_LINK)

con2stn : $(SRC)/con2stn.f $(HYSPLIT_LIB)
	$(FC) -o $@ $(FFLAGS_STRICT) $(SRC)/$@.f $(LIB_LINK)

condecay : $(SRC)/condecay.f $(HYSPLIT_LIB)
	$(FC) -o $@ $(FFLAGS_STRICT) $(SRC)/$@.f $(LIB_LINK)

conappend : $(SRC)/conappend.f $(HYSPLIT_LIB)
	$(FC) -o $@ $(FFLAGS_STRICT) $(SRC)/$@.f $(LIB_LINK)

con2arcv : $(SRC)/con2arcv.f $(HYSPLIT_LIB)
	$(FC) -o $@ $(FFLAGS_STRICT) $(SRC)/$@.f $(LIB_LINK) 

concadd : $(SRC)/concadd.f $(HYSPLIT_LIB)
	$(FC) -o $@ $(FFLAGS_STRICT) $(SRC)/$@.f $(LIB_LINK)

concmbn : $(SRC)/concmbn.f $(HYSPLIT_LIB)
	$(FC) -o $@ $(FFLAGS_STRICT) $(SRC)/$@.f $(LIB_LINK)

concsum : $(SRC)/concsum.f $(HYSPLIT_LIB)
	$(FC) -o $@ $(FFLAGS_STRICT) $(SRC)/$@.f $(LIB_LINK)

concacc : $(SRC)/concacc.f $(HYSPLIT_LIB)
	$(FC) -o $@ $(FFLAGS_STRICT) $(SRC)/$@.f $(LIB_LINK)

concrop : $(SRC)/concrop.f $(HYSPLIT_LIB)
	$(FC) -o $@ $(FFLAGS_STRICT) $(SRC)/$@.f $(LIB_LINK)

coversheet : $(SRC)/coversheet.f $(HYSPLIT_LIB) $(HYSPLIT_CXX_LIB)
	$(FC) -o $@ $(FFLAGS_STRICT) $(SRC)/$@.f $(LIB_CXX_LINK)

con2grad : $(SRC)/con2grad.f
	$(FC) -o $@ $(FFLAGS_STRICT) $(SRC)/$@.f 

conmask : $(SRC)/conmask.f 
	$(FC) -o $@ $(FFLAGS_STRICT) $(SRC)/$@.f          

conmerge : $(SRC)/conmerge.f $(HYSPLIT_LIB) 
	$(FC) -o $@ $(FFLAGS_STRICT) $(SRC)/$@.f $(LIB_LINK)

conpuff : $(SRC)/conpuff.f 
	$(FC) -o $@ $(FFLAGS_STRICT) $(SRC)/$@.f          

coninfo : $(SRC)/coninfo.f $(HYSPLIT_LIB)
	$(FC) -o $@ $(FFLAGS_STRICT) $(SRC)/$@.f $(LIB_LINK) 

conread : $(SRC)/conread.f 
	$(FC) -o $@ $(FFLAGS_STRICT) $(SRC)/$@.f          

conedit : $(SRC)/conedit.f 
	$(FC) -o $@ $(FFLAGS_STRICT) $(SRC)/$@.f          

confreq : $(SRC)/confreq.f $(HYSPLIT_LIB)
	$(FC) -o $@ $(FFLAGS_STRICT) $(SRC)/$@.f $(LIB_LINK)

conavgpd : $(SRC)/conavgpd.f $(HYSPLIT_LIB)
	$(FC) -o $@ $(FFLAGS_STRICT) $(SRC)/$@.f $(LIB_LINK) 

conmaxpd : $(SRC)/conmaxpd.f $(HYSPLIT_LIB)
	$(FC) -o $@ $(FFLAGS_STRICT) $(SRC)/$@.f $(LIB_LINK) 

constats : $(SRC)/constats.f 
	$(FC) -o $@ $(FFLAGS_STRICT) $(SRC)/$@.f          

conprob : $(SRC)/conprob.f $(HYSPLIT_LIB)
	$(FC) -o $@ $(FFLAGS_STRICT) $(SRC)/$@.f $(LIB_LINK)

con2inv : $(SRC)/con2inv.f 
	$(FC) -o $@ $(FFLAGS_STRICT) $(SRC)/$@.f 

conlight : $(SRC)/conlight.f 
	$(FC) -o $@ $(FFLAGS_STRICT) $(SRC)/$@.f 

conhavrg : $(SRC)/conhavrg.f 
	$(FC) -o $@ $(FFLAGS_STRICT) $(SRC)/$@.f 

conmaxv : $(SRC)/conmaxv.f $(HYSPLIT_LIB)
	$(FC) -o $@ $(FFLAGS_STRICT) $(SRC)/$@.f $(LIB_LINK) 

con2dose : $(SRC)/con2dose.f $(HYSPLIT_LIB)
	$(FC) -o $@ $(FFLAGS_STRICT) $(SRC)/$@.f $(LIB_LINK) 

stn2par : $(SRC)/stn2par.f $(HYSPLIT_LIB)
	$(FC) -o $@ $(FFLAGS_STRICT) $(SRC)/$@.f $(LIB_LINK)

matrix : $(SRC)/matrix.f 
	$(FC) -o $@ $(FFLAGS_STRICT) $(SRC)/$@.f 

dustedit : $(SRC)/dustedit.f 
	$(FC) -o $@ $(FFLAGS_STRICT) $(SRC)/$@.f 

fires : $(SRC)/fires.f $(HYSPLIT_LIB)
	$(FC) -o $@ $(FFLAGS_STRICT) $(SRC)/$@.f $(LIB_LINK)

firew : $(SRC)/firew.f $(HYSPLIT_LIB)
	$(FC) -o $@ $(FFLAGS_STRICT) $(SRC)/$@.f $(LIB_LINK)

goes2ems : $(SRC)/goes2ems.f $(HYSPLIT_LIB)
	$(FC) -o $@ $(FFLAGS_STRICT) $(SRC)/$@.f $(LIB_LINK)

hycs_std : $(SRC)/hymodelc.F $(HYSPLIT_LIB)
	$(FC) -o $@ $(FFLAGS_HYCS_STD_EXE) $(FFLAGS_STRICT) $(SRC)/hymodelc.F $(LIB_LINK) 

hycs_ens : $(SRC)/hymodelc.F $(HYSPLIT_LIB)
	$(FC) -o $@ $(FFLAGS_HYCS_ENS_EXE) $(FFLAGS_STRICT) $(SRC)/hymodelc.F $(LIB_LINK) 

hycs_var : $(SRC)/hymodelc.F $(HYSPLIT_LIB)
	$(FC) -o $@ $(FFLAGS_HYCS_VAR_EXE) $(FFLAGS_STRICT) $(SRC)/hymodelc.F $(LIB_LINK) 

hycs_gem : $(SRC)/hymodelc.F $(HYSPLIT_LIB)
	$(FC) -o $@ $(FFLAGS_HYCS_GEM_EXE) $(FFLAGS_STRICT) $(SRC)/hymodelc.F $(LIB_LINK) 

hycm_ens : $(SRC)/hymodelc.F $(HYSPLIT_LIB)
	$(MPIFC) -o $@ $(FFLAGS_HYCM_ENS_EXE) $(FFLAGS_STRICT) $(MINC) $(SRC)/hymodelc.F $(MLIB) $(LIB_LINK)

hycm_std : $(SRC)/hymodelc.F $(HYSPLIT_LIB)
	$(MPIFC) -o $@ $(FFLAGS_HYCM_STD_EXE) $(FFLAGS_STRICT) $(MINC) $(SRC)/hymodelc.F $(MLIB) $(LIB_LINK) 

hycs_ier : $(SRC)/hymodelc.F $(HYSPLIT_LIB)
	$(FC) -o $@ $(FFLAGS_HYCS_IER_EXE) $(FFLAGS_STRICT) $(SRC)/hymodelc.F $(LIB_LINK) 

hycs_grs : $(SRC)/hymodelc.F $(HYSPLIT_LIB)
	$(FC) -o $@ $(FFLAGS_HYCS_GRS_EXE) $(FFLAGS_STRICT) $(SRC)/hymodelc.F $(LIB_LINK) 

hycs_so2 : $(SRC)/hymodelc.F $(HYSPLIT_LIB)
	$(FC) -o $@ $(FFLAGS_HYCS_SO2_EXE) $(FFLAGS_STRICT) $(SRC)/hymodelc.F $(LIB_LINK) 

hycs_cb4 : $(SRC)/hymodelc.F $(HYSPLIT_LIB)
	$(FC) -o $@ $(FFLAGS_HYCS_CB4_EXE) $(FFLAGS_STRICT) $(SRC)/hymodelc.F $(LIB_LINK)

hycm_cb4 : $(SRC)/hymodelc.F $(HYSPLIT_LIB)
	$(MPIFC) -o $@ $(FFLAGS_HYCM_CB4_EXE) $(FFLAGS_STRICT) $(MINC) $(SRC)/hymodelc.F $(MLIB) $(LIB_LINK)

hysptest : $(SRC)/hysptest.F $(HYSPLIT_LIB)
	$(FC) -o $@ $(FFLAGS_HYSPTEST_EXE) $(FFLAGS_STRICT) $(SRC)/hysptest.F $(LIB_LINK) 

trajfreq : $(SRC)/trajfreq.f $(HYSPLIT_LIB)
	$(FC) -o $@ $(FFLAGS_STRICT) $(SRC)/$@.f $(LIB_LINK) 

trajfrmt : $(SRC)/trajfrmt.f 
	$(FC) -o $@ $(FFLAGS_STRICT) $(SRC)/$@.f 

trajgrad : $(SRC)/trajgrad.f 
	$(FC) -o $@ $(FFLAGS_STRICT) $(SRC)/$@.f 

trajmerg : $(SRC)/trajmerg.f 
	$(FC) -o $@ $(FFLAGS_STRICT) $(SRC)/$@.f 

trajfind : $(SRC)/trajfind.f 
	$(FC) -o $@ $(FFLAGS_STRICT) $(SRC)/$@.f 

latlon : $(SRC)/latlon.F 
	$(FC) -o $@ $(FFLAGS_LATLON_EXE) $(FFLAGS_STRICT) $(SRC)/$@.F 

gen2xml : $(SRC)/gen2xml.f  
	$(FC) -o $@ $(FFLAGS_STRICT) $(SRC)/$@.f

catps2ps : $(SRC)/catps2ps.f 
	$(FC) -o $@ $(FFLAGS_STRICT) $(SRC)/$@.f

hytm_std : $(SRC)/hymodelt.F $(HYSPLIT_LIB)
	$(MPIFC) -o $@ $(FFLAGS_HYTM_STD_EXE) $(FFLAGS_STRICT) $(MINC) $(SRC)/hymodelt.F $(MLIB) $(LIB_LINK) 

hyts_ens : $(SRC)/hymodelt.F $(HYSPLIT_LIB)
	$(FC) -o $@ $(FFLAGS_HYTS_ENS_EXE) $(FFLAGS_STRICT) $(SRC)/hymodelt.F $(LIB_LINK) 

hyts_std : $(SRC)/hymodelt.F $(HYSPLIT_LIB)
	$(FC) -o $@ $(FFLAGS_HYTS_STD_EXE) $(FFLAGS_STRICT) $(SRC)/hymodelt.F $(LIB_LINK) 

trajplot : $(SRC)/trajplot.f $(HYSPLIT_LIB) $(HYSPLIT_CXX_LIB)
	$(FC) $(FFLAGS_STRICT) -o $@ $(SRC)/$@.f $(LIB_CXX_LINK) 

volcplot : $(SRC)/volcplot.f $(HYSPLIT_LIB) $(HYSPLIT_CXX_LIB)
	$(FC) $(FFLAGS_STRICT) -o $@ $(SRC)/$@.f $(LIB_CXX_LINK) 

concplot : $(SRC)/concplot.f $(HYSPLIT_LIB) $(HYSPLIT_CXX_LIB)
	$(FC) $(FFLAGS_STRICT) -o $@ $(SRC)/$@.f $(LIB_CXX_LINK) 

gridplot : $(SRC)/gridplot.f $(HYSPLIT_LIB) $(HYSPLIT_CXX_LIB)
	$(FC) $(FFLAGS_STRICT) -o $@ $(SRC)/$@.f $(LIB_CXX_LINK) 

poleplot : $(SRC)/poleplot.f $(HYSPLIT_LIB) $(HYSPLIT_CXX_LIB)
	$(FC) $(FFLAGS_STRICT) -o $@ $(SRC)/$@.f $(LIB_CXX_LINK) 

isochron : $(SRC)/isochron.f $(HYSPLIT_LIB) $(HYSPLIT_CXX_LIB)
	$(FC) $(FFLAGS_STRICT) -o $@ $(SRC)/$@.f $(LIB_CXX_LINK) 

ensplots : $(SRC)/ensplots.f $(HYSPLIT_LIB) $(HYSPLIT_CXX_LIB)
	$(FC) $(FFLAGS_STRICT) -o $@ $(SRC)/$@.f $(LIB_CXX_LINK) 

boxplots : $(SRC)/boxplots.f $(HYSPLIT_LIB) $(HYSPLIT_CXX_LIB)
	$(FC) $(FFLAGS_STRICT) -o $@ $(SRC)/$@.f $(LIB_CXX_LINK) 

timeplot : $(SRC)/timeplot.f $(HYSPLIT_LIB) $(HYSPLIT_CXX_LIB)
	$(FC) $(FFLAGS_STRICT) -o $@ $(SRC)/$@.f $(LIB_CXX_LINK) 

asc2par : $(SRC)/asc2par.f 
	$(FC) $(FFLAGS_STRICT) -o $@ $(SRC)/$@.f  

par2asc : $(SRC)/par2asc.f 
	$(FC) $(FFLAGS_STRICT) -o $@ $(SRC)/$@.f  

parshift : $(SRC)/parshift.f 
	$(FC) $(FFLAGS_STRICT) -o $@ $(SRC)/$@.f  

parmerge : $(SRC)/parmerge.f 
	$(FC) $(FFLAGS_STRICT) -o $@ $(SRC)/$@.f  

parvplot : $(SRC)/parvplot.f $(HYSPLIT_LIB) $(HYSPLIT_CXX_LIB)
	$(FC) $(FFLAGS_STRICT) -o $@ $(SRC)/$@.f $(LIB_CXX_LINK) 

parhplot : $(SRC)/parhplot.f $(HYSPLIT_LIB) $(HYSPLIT_CXX_LIB)
	$(FC) $(FFLAGS_STRICT) -o $@ $(SRC)/$@.f $(LIB_CXX_LINK) 

parsplot : $(SRC)/parsplot.f $(HYSPLIT_LIB) $(HYSPLIT_CXX_LIB)
	$(FC) $(FFLAGS_STRICT) -o $@ $(SRC)/$@.f $(LIB_CXX_LINK) 

parxplot : $(SRC)/parxplot.f $(HYSPLIT_LIB) $(HYSPLIT_CXX_LIB)
	$(FC) $(FFLAGS_STRICT) -o $@ $(SRC)/$@.f $(LIB_CXX_LINK) 

par2conc : $(SRC)/par2conc.f $(HYSPLIT_LIB)
	$(FC) $(FFLAGS_STRICT) -o $@ $(SRC)/$@.f $(LIB_LINK) 

paro2n   : $(SRC)/paro2n.f $(HYSPLIT_LIB)
	$(FC) $(FFLAGS_STRICT) -o $@ $(SRC)/$@.f $(LIB_LINK) 

gelabel : $(SRC)/gelabel.f $(HYSPLIT_LIB) $(HYSPLIT_CXX_LIB)
	$(FC) $(FFLAGS_STRICT) -o $@ $(SRC)/$@.f $(LIB_CXX_LINK) 

chk_rec : $(SRC)/chk_rec.f 
	$(FC) -o $@ $(FFLAGS_STRICT) $(SRC)/$@.f 

chk_times : $(SRC)/chk_times.f 
	$(FC) -o $@ $(FFLAGS_STRICT) $(SRC)/$@.f 

chk_data : $(SRC)/chk_data.f 
	$(FC) -o $@ $(FFLAGS_STRICT) $(SRC)/$@.f  

timeplus : $(SRC)/timeplus.f $(HYSPLIT_LIB)
	$(FC) -o $@ $(FFLAGS_STRICT) $(SRC)/$@.f $(LIB_LINK)

rec_copy : $(SRC)/rec_copy.f $(HYSPLIT_LIB)
	$(FC) -o $@ $(FFLAGS_STRICT) $(SRC)/$@.f $(LIB_LINK)

rec_merge : $(SRC)/rec_merge.f $(HYSPLIT_LIB)
	$(FC) -o $@ $(FFLAGS_STRICT) $(SRC)/$@.f $(LIB_LINK)

rec_insert : $(SRC)/rec_insert.f $(HYSPLIT_LIB)
	$(FC) -o $@ $(FFLAGS_STRICT) $(SRC)/$@.f $(LIB_LINK)

chk_file : $(SRC)/chk_file.f $(HYSPLIT_LIB)
	$(FC) -o $@ $(FFLAGS_STRICT) $(SRC)/$@.f $(LIB_LINK)

chk_index : $(SRC)/chk_index.f $(HYSPLIT_LIB)
	$(FC) -o $@ $(FFLAGS_STRICT) $(SRC)/$@.f $(LIB_LINK)

datecol : $(SRC)/datecol.f $(HYSPLIT_LIB)
	$(FC) -o $@ $(FFLAGS_STRICT) $(SRC)/$@.f $(LIB_LINK)

metdates : $(SRC)/metdates.f $(HYSPLIT_LIB)
	$(FC) -o $@ $(FFLAGS_STRICT) $(SRC)/$@.f $(LIB_LINK)

filedates : $(SRC)/filedates.f $(HYSPLIT_LIB)
	$(FC) -o $@ $(FFLAGS_STRICT) $(SRC)/$@.f $(LIB_LINK)

datesmry : $(SRC)/datesmry.f $(HYSPLIT_LIB)
	$(FC) -o $@ $(FFLAGS_STRICT) $(SRC)/$@.f $(LIB_LINK)

gridxy2ll : $(SRC)/gridxy2ll.f $(HYSPLIT_LIB)
	$(FC) -o $@ $(FFLAGS_STRICT) $(SRC)/$@.f $(LIB_LINK)

arl2grad : $(SRC)/arl2grad.f $(HYSPLIT_LIB)
	$(FC) -o $@ $(FFLAGS_STRICT) $(SRC)/$@.f $(LIB_LINK) 

xtrct_stn : $(SRC)/xtrct_stn.f $(HYSPLIT_LIB)
	$(FC) -o $@ $(FFLAGS_STRICT) $(SRC)/$@.f $(LIB_LINK) 
 
xtrct_time : $(SRC)/xtrct_time.f $(HYSPLIT_LIB)
	$(FC) -o $@ $(FFLAGS_STRICT) $(SRC)/$@.f $(LIB_LINK) 
 
xtrct_grid : $(SRC)/xtrct_grid.f $(HYSPLIT_LIB)
	$(FC) -o $@ $(FFLAGS_STRICT) $(SRC)/$@.f $(LIB_LINK)
 
add_velv : $(SRC)/add_velv.f $(HYSPLIT_LIB)
	$(FC) -o $@ $(FFLAGS_STRICT) $(SRC)/$@.f $(LIB_LINK)
 
add_data : $(SRC)/add_data.f $(HYSPLIT_LIB)
	$(FC) -o $@ $(FFLAGS_STRICT) $(SRC)/$@.f $(LIB_LINK)
 
add_miss : $(SRC)/add_miss.f $(HYSPLIT_LIB)
	$(FC) -o $@ $(FFLAGS_STRICT) $(SRC)/$@.f $(LIB_LINK)
 
add_time : $(SRC)/add_time.f $(HYSPLIT_LIB)
	$(FC) -o $@ $(FFLAGS_STRICT) $(SRC)/$@.f $(LIB_LINK)
 
add_grid : $(SRC)/add_grid.f $(HYSPLIT_LIB)
	$(FC) -o $@ $(FFLAGS_STRICT) $(SRC)/$@.f $(LIB_LINK)
 
data_avrg : $(SRC)/data_avrg.f $(HYSPLIT_LIB)
	$(FC) -o $@ $(FFLAGS_STRICT) $(SRC)/$@.f $(LIB_LINK) 
 
data_del : $(SRC)/data_del.f $(HYSPLIT_LIB)
	$(FC) -o $@ $(FFLAGS_STRICT) $(SRC)/$@.f $(LIB_LINK) 
 
data_year : $(SRC)/data_year.f $(HYSPLIT_LIB)
	$(FC) -o $@ $(FFLAGS_STRICT) $(SRC)/$@.f $(LIB_LINK) 
 
edit_flux : $(SRC)/edit_flux.f $(HYSPLIT_LIB)
	$(FC) -o $@ $(FFLAGS_STRICT) $(SRC)/$@.f $(LIB_LINK) 

edit_head : $(SRC)/edit_head.f $(HYSPLIT_LIB)
	$(FC) -o $@ $(FFLAGS_STRICT) $(SRC)/$@.f $(LIB_LINK) 

edit_null : $(SRC)/edit_null.f $(HYSPLIT_LIB)
	$(FC) -o $@ $(FFLAGS_STRICT) $(SRC)/$@.f $(LIB_LINK) 

edit_miss : $(SRC)/edit_miss.f $(HYSPLIT_LIB)
	$(FC) -o $@ $(FFLAGS_STRICT) $(SRC)/$@.f $(LIB_LINK) 

edit_index : $(SRC)/edit_index.f $(HYSPLIT_LIB)
	$(FC) -o $@ $(FFLAGS_STRICT) $(SRC)/$@.f $(LIB_LINK) 

pole2merc : $(SRC)/pole2merc.f $(HYSPLIT_LIB)
	$(FC) -o $@ $(FFLAGS_STRICT) $(SRC)/$@.f $(LIB_LINK)

contour : $(SRC)/contour.f $(HYSPLIT_LIB) $(HYSPLIT_CXX_LIB)
	$(FC) -o $@ $(FFLAGS_STRICT) $(SRC)/$@.f $(LIB_CXX_LINK)

profile : $(SRC)/profile.F $(HYSPLIT_LIB)
	$(FC) -o $@ $(FFLAGS_PROFILE_EXE) $(FFLAGS_STRICT) $(SRC)/$@.F $(LIB_LINK) 

#display : $(SRC)/display.F $(HYSPLIT_LIB) $(HYSPLIT_CXX_LIB)
#	$(FC) -o $@ $(FFLAGS_DISPLAY_EXE) $(FFLAGS_STRICT) $(SRC)/$@.F $(LIB_CXX_LINK)

metpoint : $(SRC)/metpoint.f $(HYSPLIT_LIB)
	$(FC) -o $@ $(FFLAGS_STRICT) $(SRC)/$@.f $(LIB_LINK) 

vmixing : $(SRC)/vmixing.f $(HYSPLIT_LIB)
	$(FC) -o $@ $(FFLAGS_STRICT) $(SRC)/$@.f $(LIB_LINK) 

velvar : $(SRC)/velvar.f $(HYSPLIT_LIB)
	$(FC) -o $@ $(FFLAGS_STRICT) $(SRC)/$@.f $(LIB_LINK) 

stabplot : $(SRC)/stabplot.f $(HYSPLIT_LIB) $(HYSPLIT_CXX_LIB)
	$(FC) -o $@ $(FFLAGS_STRICT) $(SRC)/$@.f $(LIB_CXX_LINK) 

stn2ge : $(SRC)/stn2ge.f $(HYSPLIT_LIB)
	$(FC) -o $@ $(FFLAGS_STRICT) $(SRC)/$@.f $(LIB_LINK)

showgrid : $(SRC)/showgrid.F $(HYSPLIT_LIB) $(HYSPLIT_CXX_LIB)
	$(FC) -o $@ $(FFLASG_SHOWGRID_EXE) $(FFLAGS_STRICT) $(SRC)/$@.F $(LIB_CXX_LINK)

zcoord : $(SRC)/zcoord.f 
	$(FC) -o $@ $(FFLAGS_STRICT) $(SRC)/$@.f  

file_copy : $(SRC)/file_copy.f 
	$(FC) -o $@ $(FFLAGS_STRICT) $(SRC)/$@.f 

file_merge : $(SRC)/file_merge.f $(HYSPLIT_LIB)
	$(FC) -o $@ $(FFLAGS_STRICT) $(SRC)/$@.f $(LIB_LINK) 

prntbdy : $(SRC)/prntbdy.f
	$(FC) -o $@ $(FFLAGS_STRICT) $(SRC)/$@.f

dat2cntl : $(SRC)/dat2cntl.f $(HYSPLIT_LIB)
	$(FC) -o $@ $(FFLAGS_STRICT) $(SRC)/$@.f $(LIB_LINK)

dustbdy : $(SRC)/dustbdy.F $(HYSPLIT_LIB)
	$(FC) -o $@ $(FFLAGS_DUSTBDY_EXE) $(FFLAGS_STRICT) $(SRC)/$@.F $(LIB_LINK)

stn2arl : $(SRC)/stn2arl.f $(HYSPLIT_LIB)
	$(FC) $(FFLAGS_STRICT) -o $@ $(SRC)/$@.f -L$(LIB) $(CLIB) -lhysplit

sfc2arl : $(SRC)/sfc2arl.f $(HYSPLIT_LIB)
	$(FC) $(FFLAGS_STRICT) -o $@ $(SRC)/$@.f -L$(LIB) $(CLIB) -lhysplit

snd2arl : $(SRC)/snd2arl.f $(HYSPLIT_LIB)
	$(FC) $(FFLAGS_STRICT) -o $@ $(SRC)/$@.f -L$(LIB) $(CLIB) -lhysplit

drn2arl : $(SRC)/drn2arl.f $(HYSPLIT_LIB)
	$(FC) $(FFLAGS_STRICT) -o $@ $(SRC)/$@.f -L$(LIB) $(CLIB) -lhysplit

aer2arl : $(SRC)/aer2arl.f $(HYSPLIT_LIB)
	$(FC) $(FFLAGS_STRICT) -o $@ $(SRC)/$@.f -L$(LIB) $(CLIB) -lhysplit

arl2meds : $(SRC)/arl2meds.f $(HYSPLIT_LIB)
	$(FC) $(FFLAGS_STRICT) -o $@ $(SRC)/$@.f -L$(LIB) $(CLIB) -lhysplit

inventory : $(SRC)/inventory.f
	$(FC) $(FFLAGS_STRICT) -o $@ $(SRC)/$@.f                

content : $(SRC)/content.f
	$(FC) $(FFLAGS_STRICT) -o $@ $(SRC)/$@.f            

unpacker : $(SRC)/unpacker.f
	$(FC) $(FFLAGS_STRICT) -o $@ $(SRC)/$@.f            

findgrib : $(SRC)/findgrib.f
	$(FC) $(FFLAGS_STRICT) -o $@ $(SRC)/$@.f            

c2datem : $(SRC)/c2datem.f $(HYSPLIT_LIB)
	$(FC) $(FFLAGS_STRICT) -o $@ $(SRC)/$@.f $(LIB_LINK)         

c2array : $(SRC)/c2array.f
	$(FC) $(FFLAGS_STRICT) -o $@ $(SRC)/$@.f $(LIB_LINK)

accudiv : $(SRC)/accudiv.f
	$(FC) $(FFLAGS_STRICT) -o $@ $(SRC)/$@.f            

ensperc : $(SRC)/ensperc.f $(HYSPLIT_LIB)
	$(FC) $(FFLAGS_STRICT) -o $@ $(SRC)/$@.f $(LIB_LINK)

enstala : $(SRC)/enstala.f $(HYSPLIT_LIB)
	$(FC) $(FFLAGS_STRICT) -o $@ $(SRC)/$@.f $(LIB_LINK)

tcmsum : $(SRC)/tcmsum.f $(HYSPLIT_LIB)
	$(FC) -o $@ $(FFLAGS_STRICT) $(SRC)/$@.f $(LIB_LINK)

tcsolve : $(SRC)/tcsolve.f $(HYSPLIT_LIB)
	$(FC) $(FFLAGS_STRICT) -o $@ $(SRC)/$@.f $(LIB_LINK)

scatter : $(SRC)/scatter.f $(HYSPLIT_LIB) $(HYSPLIT_CXX_LIB)
	$(FC) $(FFLAGS_STRICT) -o $@ $(SRC)/$@.f $(LIB_CXX_LINK) 

statmain : $(SRC)/statmain.f $(HYSPLIT_LIB)
	$(FC) $(FFLAGS_STRICT) -o $@ $(SRC)/$@.f $(LIB_LINK)

stat2grid : $(SRC)/stat2grid.f 
	$(FC) $(FFLAGS_STRICT) -o $@ $(SRC)/$@.f

clusend : $(SRC)/clusend.f 
	$(FC) $(FFLAGS_STRICT) -o $@ $(SRC)/$@.f   
            
cluslist : $(SRC)/cluslist.f 
	$(FC) $(FFLAGS_STRICT) -o $@ $(SRC)/$@.f   
            
clusmem : $(SRC)/clusmem.f 
	$(FC) $(FFLAGS_STRICT) -o $@ $(SRC)/$@.f   
            
clusplot : $(SRC)/clusplot.f $(HYSPLIT_LIB) $(HYSPLIT_CXX_LIB)
	$(FC) $(FFLAGS_STRICT) -o $@ $(SRC)/$@.f $(LIB_CXX_LINK)
             
cluster : $(SRC)/cluster.f $(HYSPLIT_LIB)
	$(FC) $(FFLAGS_STRICT) -o $@ $(SRC)/$@.f $(LIB_LINK)
            
merglist : $(SRC)/merglist.f $(HYSPLIT_LIB)
	$(FC) $(FFLAGS_STRICT) -o $@ $(SRC)/$@.f $(LIB_LINK)
            
trajmean : $(SRC)/trajmean.f $(HYSPLIT_LIB)
	$(FC) $(FFLAGS_STRICT) -o $@ $(SRC)/$@.f $(LIB_LINK)  
                 
vmsread : $(SRC)/vmsread.f 
	$(FC) $(FFLAGS_STRICT) -o $@ $(SRC)/$@.f   

vmsmerge: $(SRC)/vmsmerge.f 
	$(FC) $(FFLAGS_STRICT) -o $@ $(SRC)/$@.f   

constnlst : $(SRC)/constnlst.f $(HYSPLIT_LIB)
	$(FC) -o $@ $(FFLAGS_STRICT) $(SRC)/$@.f $(LIB_LINK)

testnuc : $(SRC)/testnuc.f $(HYSPLIT_LIB)
	$(FC) $(FFLAGS_STRICT) -o $@ $(SRC)/$@.f $(LIB_LINK) 

nuctree : $(SRC)/nuctree.f $(HYSPLIT_LIB)
	$(FC) $(FFLAGS_STRICT) -o $@ $(SRC)/$@.f $(LIB_LINK) 

lbfgsb : $(SRC)/lbfgsb.F 
	$(FC) $(LDFLAGS_FPROG) -o $@ $(SRC)/$@.F -L$(LIB) -llbfgsb 

macc2date : $(SRC)/macc2date.f $(HYSPLIT_LIB)
	$(FC) $(FFLAGS_STRICT) -o $@ $(SRC)/$@.f $(LIB_LINK) 

metlatlon : $(SRC)/metlatlon.f $(HYSPLIT_LIB)
	$(FC) $(FFLAGS_STRICT) -o $@ $(SRC)/$@.f $(LIB_LINK) 

var2datem : $(SRC)/var2datem.f $(HYSPLIT_LIB)
	$(FC) $(FFLAGS_STRICT) -o $@ $(SRC)/$@.f $(LIB_LINK) 

mergextr : $(SRC)/mergextr.f $(HYSPLIT_LIB)
	$(FC) $(FFLAGS_STRICT) -o $@ $(SRC)/$@.f $(LIB_LINK) 

cat2svg : $(SRC)/cat2svg.o $(HYSPLIT_LIB) $(HYSPLIT_CXX_LIB)
	$(FC) $(FFLAGS_CAT2SVG_EXE) $(FFLAGS_STRICT) -o $@ $(SRC)/$@.o $(LIB_CXX_LINK)

splitsvg : $(SRC)/splitsvg.cpp $(HYSPLIT_LIB)
	$(CXX) $(CXXFLAGS) -o $@ $(SRC)/$@.cpp $(LIB_LINK) 

pbl2asc : $(SRC)/pbl2asc.f $(HYSPLIT_LIB)
	$(FC) $(FFLAGS_STRICT) -o $@ $(SRC)/$@.f $(LIB_LINK) 

#
# Speical executables for the unregistered version
#

public: public/hycs_std public/hycs_ens public/hycs_var public/hycs_gem \
	public/hycm_cb4 public/hycm_ens public/hycm_std \
	public/hycs_cb4 public/hycs_grs public/hycs_ier public/hycs_so2

public/hycs_std : $(SRC)/hymodelc.F $(HYSPLIT_LIB)
	$(FC) -DPUB -o $@ $(FFLAGS_HYCS_STD_EXE) $(FFLAGS_STRICT) $(SRC)/hymodelc.F $(LIB_LINK) 

public/hycs_ens : $(SRC)/hymodelc.F $(HYSPLIT_LIB)
	$(FC) -DPUB -o $@ $(FFLAGS_HYCS_ENS_EXE) $(FFLAGS_STRICT) $(SRC)/hymodelc.F $(LIB_LINK) 

public/hycs_var : $(SRC)/hymodelc.F $(HYSPLIT_LIB)
	$(FC) -DPUB -o $@ $(FFLAGS_HYCS_VAR_EXE) $(FFLAGS_STRICT) $(SRC)/hymodelc.F $(LIB_LINK) 

public/hycs_gem : $(SRC)/hymodelc.F $(HYSPLIT_LIB)
	$(FC) -DPUB -o $@ $(FFLAGS_HYCS_GEM_EXE) $(FFLAGS_STRICT) $(SRC)/hymodelc.F $(LIB_LINK) 

public/hycm_cb4 : $(SRC)/hymodelc.F $(HYSPLIT_LIB)
	$(MPIFC) -DPUB -o $@ $(FFLAGS_HYCM_CB4_EXE) $(FFLAGS_STRICT) $(MINC) $(SRC)/hymodelc.F $(MLIB) $(LIB_LINK)

public/hycm_ens : $(SRC)/hymodelc.F $(HYSPLIT_LIB)
	$(MPIFC) -DPUB -o $@ $(FFLAGS_HYCM_ENS_EXE) $(FFLAGS_STRICT) $(MINC) $(SRC)/hymodelc.F $(MLIB) $(LIB_LINK)

public/hycm_std : $(SRC)/hymodelc.F $(HYSPLIT_LIB)
	$(MPIFC) -DPUB -o $@ $(FFLAGS_HYCM_STD_EXE) $(FFLAGS_STRICT) $(MINC) $(SRC)/hymodelc.F $(MLIB) $(LIB_LINK)

public/hycs_cb4 : $(SRC)/hymodelc.F $(HYSPLIT_LIB)
	$(FC) -DPUB -o $@ $(FFLAGS_HYCS_CB4_EXE) $(FFLAGS_STRICT) $(SRC)/hymodelc.F $(LIB_LINK)

public/hycs_grs : $(SRC)/hymodelc.F $(HYSPLIT_LIB)
	$(FC) -DPUB -o $@ $(FFLAGS_HYCS_GRS_EXE) $(FFLAGS_STRICT) $(SRC)/hymodelc.F $(LIB_LINK)

public/hycs_ier : $(SRC)/hymodelc.F $(HYSPLIT_LIB)
	$(FC) -DPUB -o $@ $(FFLAGS_HYCS_IER_EXE) $(FFLAGS_STRICT) $(SRC)/hymodelc.F $(LIB_LINK)

public/hycs_so2 : $(SRC)/hymodelc.F $(HYSPLIT_LIB)
	$(FC) -DPUB -o $@ $(FFLAGS_HYCS_SO2_EXE) $(FFLAGS_STRICT) $(SRC)/hymodelc.F $(LIB_LINK)

clean :
	rm -f *.o
	rm -f *.mod
	rm -f hy??_???
	rm -f arw2arl
	rm -f asc2par
	rm -f conavg
	rm -f con2asc
	rm -f con2cdf4
	rm -f con2ctbt
	rm -f con2stn
	rm -f con2srs
	rm -f con2rem
	rm -f condecay
	rm -f con2arcv
	rm -f con2grad
	rm -f con2grib
	rm -f con2grib2
	rm -f con2dose 
	rm -f conappend  
	rm -f concadd  
	rm -f concmbn  
	rm -f concsum  
	rm -f concacc  
	rm -f concrop 
	rm -f coversheet
	rm -f grib2con
	rm -f conmerge
	rm -f conmask
	rm -f conpuff
	rm -f coninfo
	rm -f conread
	rm -f conedit
	rm -f confreq
	rm -f conavgpd
	rm -f conmaxpd
	rm -f constats
	rm -f conprob
	rm -f con2inv
	rm -f conlight
	rm -f conhavrg
	rm -f conmaxv 
	rm -f stn2par
	rm -f dustedit
	rm -f fires
	rm -f firew
	rm -f goes2ems
	rm -f matrix
	rm -f trajgrad
	rm -f trajmerg
	rm -f trajfind
	rm -f trajfrmt
	rm -f trajfreq
	rm -f latlon
	rm -f narr2arl
	rm -f gen2xml
	rm -f catps2ps
	rm -f trajplot
	rm -f volcplot
	rm -f concplot
	rm -f isochron
	rm -f gridplot
	rm -f poleplot
	rm -f ensplots
	rm -f boxplots
	rm -f timeplot
	rm -f par2asc
	rm -f parmerge
	rm -f parshift
	rm -f parvplot
	rm -f parhplot
	rm -f parsplot
	rm -f parxplot
	rm -f par2conc
	rm -f paro2n
	rm -f gelabel
	rm -f arl2grad
	rm -f chk_data
	rm -f chk_file
	rm -f chk_index
	rm -f gridxy2ll
	rm -f chk_rec
	rm -f chk_times
	rm -f datecol
	rm -f metdates
	rm -f filedates
	rm -f datesmry
	rm -f data_avrg
	rm -f data_del
	rm -f data_year
	rm -f edit_flux
	rm -f edit_head
	rm -f edit_index
	rm -f edit_miss
	rm -f edit_null
	rm -f file_merge
	rm -f file_copy
	rm -f pole2merc
	rm -f timeplus 
	rm -f rec_copy 
	rm -f rec_insert
	rm -f rec_merge
	rm -f stabplot
	rm -f stn2ge  
	rm -f metpoint
	rm -f vmixing
	rm -f velvar 
	rm -f xtrct_grid
	rm -f xtrct_stn
	rm -f xtrct_time
	rm -f add_velv
	rm -f add_data
	rm -f add_grid
	rm -f add_miss
	rm -f add_time
	rm -f zcoord
	rm -f contour
	rm -f profile
	rm -f showgrid
	rm -f sfc2arl
	rm -f stn2arl
	rm -f snd2arl
	rm -f drn2arl
	rm -f aer2arl
	rm -f arl2meds
	rm -f inventory
	rm -f content
	rm -f unpacker
	rm -f findgrib
	rm -f prntbdy
	rm -f dustbdy
	rm -f dat2cntl
	rm -f c2datem  
	rm -f c2array
	rm -f accudiv   
	rm -f ensperc
	rm -f enstala
	rm -f tcmsum   
	rm -f tcsolve  
	rm -f scatter 
	rm -f statmain
	rm -f stat2grid
	rm -f clusend
	rm -f cluslist
	rm -f clusmem
	rm -f clusplot
	rm -f cluster
	rm -f merglist
	rm -f trajmean
	rm -f vmsread
	rm -f vmsmerge
	rm -f constnlst
	rm -f testnuc 
	rm -f nuctree
	rm -f hysptest
	rm -f lbfgsb 
	rm -f macc2date
	rm -f metlatlon
	rm -f var2datem
	rm -f mergextr
	rm -f $(ADDITIONAL_EXE)
	rm -f cat2svg splitsvg
