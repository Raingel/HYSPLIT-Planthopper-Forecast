#-----------------------------------------------------------------
# Master Makefile for the ../source directory
#   Last Revised: 10 Aug 2004
#                 10 Jan 2005 - g95 fortran
#                 25 May 2005 - added massum
#                 13 Oct 2005 - lagrangian sampling
#                 12 May 2006 - deallocation
#                 12 Jan 2007 - cyl2xy
#                 09 Aug 2007 - support for gfortran
#                 30 May 2008 - added GEM routines
#                 12 Aug 2008 - shapefile reader subroutines
#                 13 Feb 2009 - parallization options for SMP
#                 05 Oct 2010 - particle redistribution (emsize)
#                 02 Dec 2010 - added setclr
#                 15 Mar 2011 - added psnoaa for postscript logo
#                 18 May 2011 - added adv2pt for nearest neighbor
#                 16 Oct 2012 - intel compiler option change
#                 26 Feb 2013 - added prvals for external precip
#                 25 Mar 2013 - added cr8kml
#                 06 Nov 2014 - new routines mkcape and mixcon
#                 09 Apr 2015 - foot and mouth disease DEPFMD
#                 06 Jan 2016 - added daughter product routines
#                 14 Apr 2016 - added decodd subroutine to read height in datem file
#                 07 Aug 2017 - new subroutine PSZAPF for ZapfDingBats
#                 20 Apr 2018 - new subroutine metwbl for weather balloons
#                 06 Jun 2019 - new subroutine zindex
#                 19 Aug 2019 - new subroutine xcircl
#                 23 Aug 2019 - moved compiler options and library definitions
#                               to ../../Makefile.inc
#                 26 Aug 2019 - remove the INC variable.
#                 30 Jul 2020 - new subroutine sorise
#                 07 Aug 2020 - new subroutine rdwhyb
#                 17 Jun 2021 - add c++ files for generating graphics.
#                 14 Oct 2021 - separate library for c++ files.
#                 26 Jan 2022 - compile using FFLAGS_STRICT except for chmgea.f.
#                 18 Oct 2022 - add additional include directory, psplot_inc.
#                 20 Oct 2022 - add hysetup.f.
#                 09 May 2023 - add freita.f.
#                 06 Jun 2023 - add region.f.
#                 09 Aug 2023 - add sort.f and prerrx.f
#-----------------------------------------------------------------

include ../../Makefile.inc

SHELL = /bin/sh

DIR=..
LIB=$(DIR)/libhysplit.a
LIB_CXX=$(DIR)/libhysplitcxx.a

# more include files in the psplot_inc directory
FFLAGS_STRICT+= -I./psplot_inc

#LFLAGS = -vr
LFLAGS = -r

CODE =          \
       funits.f \
       hysetup.f \
       prfmod.f \
       metval.f \
       stbcon.f \
       mapbox.f \
       iercfg.f \
       adviec.f \
       grdsiz.f \
       advrkm.f \
       advrnt.f \
       adv2nt.f \
       adv2pt.f \
       adv3nt.f \
       rdizsg.f \
       output.f \
       cgrell.f \
       prfwrf.f \
       rdwhyb.f \
       advsti.f \
       rtsafe.f \
       rsatpt.f \
       dthees.f \
       advgre.f \
       advloc.f \
       adv3nw.f \
       adviso.f \
       zindex.f \
       advmet.f \
       advpnt.f \
       advsfc.f \
       advrng.f \
       chem02.f \
       cmtout.f \
       condsk.f \
       conini.f \
       conset.f \
       consum.f \
       conzro.f \
       cyl2xy.f \
       cr8kml.f \
       datset.f \
       dalloc.f \
       decodi.f \
       decodr.f \
       decodd.f \
       depdry.f \
       depelm.f \
       depfmd.f \
       deprad.f \
       depset.f \
       depsus.f \
       emsdat.f \
       emsgrd.f \
       emsini.f \
       emsize.f \
       emspnt.f \
       emsset.f \
       emsmat.f \
       emstmp.f \
       emrise.f \
       sorise.f \
       frtmod.f \
       geplot.f \
       gbl2ll.f \
       gbl2xy.f \
       gbldxy.f \
       gbldll.f \
       gblset.f \
       grfbdy.f \
       lagems.f \
       lagout.f \
       lagsum.f \
       lonavg.f \
       massum.f \
       metdivmod.f \
       metgrd.f \
       metini.f \
       metinp.f \
       metlvl.f \
       metold.f \
       metpos.f \
       metset.f \
       metslp.f \
       metsub.f \
       metsum.f \
       metter.f \
       metwbl.f \
       metwnd.f \
       mkcape.f \
       mixcon.f \
       p10adj.f \
       pakinp.f \
       pakout.f \
       pakset.f \
       pakrec.f \
       pakini.f \
       pakndx.f \
       pardsp.f \
       stltdsp.f \
       hordsp.f \
       parinp.f \
       parpuf.f \
       parout.f \
       parvar.f \
       parvr3.f \
       random.f \
       prfcom.f \
       prfecm.f \
       prfprs.f \
       prfsig.f \
       prfter.f \
       prvals.f \
       pufdel.f \
       pufdsp.f \
       pufmin.f \
       pufmrg.f \
       pufpar.f \
       pufrnd.f \
       pufsph.f \
       pufspv.f \
       pufsrt.f \
       region.f \
       runset.f \
       setclr.f \
       sfcinp.f \
       stbanl.f \
       stbhor.f \
       stbsnd.f \
       stbvar.f \
       stbtke.f \
       stbkth.f \
       stbinp.f \
       stbhan.f \
       strutl.f \
       sunang.f \
       sunflx.f \
       tm2day.f \
       tm2jul.f \
       tm2min.f \
       tminit.f \
       tmplus.f \
       trjdsk.f \
       trjset.f \
       con2xy.f \
       conavg.f \
       coninp.f \
       congrd.f \
       conmap.f \
       conndx.f \
       consmt.f \
       contur.f \
       contxt.f \
       map2xy.f \
       mapbdy.f \
       mapini.f \
       monset.f \
       shpbdy.f \
       shpsub.f \
       trjinp.f \
       trjlmt.f \
       trjmap.f \
       trjndx.f \
       psplot.f \
       psnoaa.f \
       pszapf.f \
       cc2gll.f \
       cc2gxy.f \
       ccrvll.f \
       ccrvxy.f \
       cg2cll.f \
       cg2cxy.f \
       cg2wll.f \
       cg2wxy.f \
       cgszll.f \
       cgszxy.f \
       cll2xy.f \
       cnllxy.f \
       cnxyll.f \
       cpolll.f \
       cpolxy.f \
       cspanf.f \
       cw2gll.f \
       cw2gxy.f \
       cxy2ll.f \
       eqvlat.f \
       stcm1p.f \
       stcm2p.f \
       stlmbr.f \
       fcclos.f \
       fcopen.f \
       fcptps.f \
       fcptpd.f \
       fcgtps.f \
       fcread.f \
       w3code.f \
       iercon.f \
       ierno2.f \
       ierset.f \
       iersum.f \
       iervoc.f \
       grseqn.f \
       grscon.f \
       grsdst.f \
       prchem.f \
       cb4con.f \
       cb4dat.f \
       cb4dst.f \
       cb4grd.f \
       cb4ini.f \
       cb4pcn.f \
       cb4sum.f \
       chmphg.f \
       chmgea.f \
       viscrt.f \
       nucdtr.f \
       nucdcy.f \
       gemcfg.f \
       gemkon.f \
       gemvar.f \
       gemchk.f \
       gemcol.f \
       gemdat.f \
       gemday.f \
       gemdep.f \
       gemdiv.f \
       gemeqn.f \
       gemgrd.f \
       gemini.f \
       geminp.f \
       gemout.f \
       gempar.f \
       gemset.f \
       gemstb.f \
       gemswf.f \
       gemsum.f \
       gemtst.f \
       stltmass.f \
       xcircl.f \
       prerrx.f \
       sort.f \

CODE_C = \
       shpext.c

CODE_CXX = \
       cutils.cpp \
       genctr.cpp \
       grfabs.cpp \
       grfps.cpp  \
       grfsvg.cpp \
       xmldoc.cpp

.f.o:
	$(FC) -c $(FFLAGS_STRICT) $(FFLAGS_FREEFORM) $?

.c.o:
	$(CC) -c $(CFLAGS) $?

.cxx.o:
	$(CXX) -c $(CXXFLAGS) $?

build: $(LIB) $(LIB_CXX)

$(LIB) : $(CODE:.f=.o) $(CODE_C:.c=.o)
	$(AR) $(LFLAGS) $(LIB) $(CODE:.f=.o) $(CODE_C:.c=.o)

$(LIB_CXX) : $(CODE_CXX:.cpp=.o)
	$(AR) $(LFLAGS) $(LIB_CXX) $(CODE_CXX:.cpp=.o)

chmgea.o: chmgea.f
	$(FC) -c $(FFLAGS) $(FFLAGS_FREEFORM) $?

clean :
	rm -f *.o
	rm -f *.mod
	rm -f $(LIB)
	rm -f $(LIB_CXX)
