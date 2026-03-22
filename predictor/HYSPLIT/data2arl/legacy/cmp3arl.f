      PROGRAM CMP2ARL

!-------------------------------------------------------------------
! LAST REVISION: 09 Feb 1999 (RRD)
!-------------------------------------------------------------------

! Convert Navy COAMPS3 output files to ARL format compatible for input
! to Hysplit4.  ARL format consists of one file, direct-access I/O, with
! meteorological data ordered by time. COAMPS data will remain on native
! grid and sigma levels.  Grid information is encoded into the ARL format
! index record.  Note that ARL formatted files can be created by time period
! and then "cat" together.  This program will process multiple time periods.
! This program will only extract the minimum fields required by Hysplit.

!-------------------------------------------------------------------

! There are several differences between version 2 and 3.  In particular the 
! file name convention has changed and 3d variables are now written by level
! in each file rather than writing a single 3d variable level to one file.
! The header file (datahd_sfc....) has been expanded from 500 variables. Note
! no documentation exists describing the new format and the grid description
! information has changed.  Outputs appear to be interpolated to either standard
! pressure levels or height levels.  Not all variables are available for each
! vertical output type.  Vertical motion fields are not available although they
! could be computed by integrating the divergence field (only available on
! pressure level output).  Data fields are no longer written IEEE unformatted,
! (4 byte header and trailer) but require direct access with the record length 
! equaling 4 times the number of grid points. 

! Code Notes:

! 1.  We modified your cmp2arl.f90, not your cmp3arl.f90, and include it
! here as cmp3narl.f90.

! 2.  All changes are noted with either a !JJS or !PFC.

! 3.  We ran this on an pentium-linux machine, and therefore needed to 
! add the endian-swap subroutine called SWAP32.

! Problems:

! 1.  There is currently an error in the COAMPS 3 header data - the
! longitude of the 1,1 nest grid point is incorrect (set to the XLON1
! variable in SUB HEADER)in the current version of COAMPS.  We have
! notified the COAMPS people at NRL-Monterey for a fix, but I am unsure
! when it will be implemented, as it has no effect on any COAMPS data 
! and is only included in the output header.  To workaround this, we
! hard-coded these data points for our runs (see lines 222-231).

! 2.  We used a lambert conformal projection with the projection
! intersection at 30 and 60 deg.  Therefore, in SUB WINDEX, we changed
! GRIDS(3) for MTYPE.EQ.2 to equal REFLT2 (30 deg), a variable which had
! to be added to the subroutine call list.  GRIDS(4) was left set equal
! to ALIGN.  The other projections are untested.

! 3.  We processed hours 0-11 in each data set and therefore changed the
! DO limit on line 114.

! -------------------------------------
! Peter F. Caffrey, Ph.D.
! Naval Research Laboratory, Code 7228
! 4555 Overlook Ave SW
! Washington, DC 20375

! (202) 767-8474
! (202) 404-8011 (fax)

! Peter.Caffrey@nrl.navy.mil
!-------------------------------------------------------------------

      LOGICAL   FTEST
      CHARACTER FNAME*80, DATAD*80, BASEF*10

! the following parameter statement sets the maximum grid dimensions to be
! used in subsequent subroutines for both input and output data grids

!JJS      PARAMETER (MXP=200, MYP=200, MZP=50)
      PARAMETER (MXP=300, MYP=300, MZP=50)
      PARAMETER (MXYP=MXP*MYP)

!     real data array and packed 2D data array
      REAL RVAR(MXP,MYP,MZP), SVAR(MXP,MYP,MZP), WVAR(MXP,MYP)
      CHARACTER CVAR(MXYP)*1

!     height of each model level and delta-levels
      REAL SIGMA(MZP), DSIGMA(MZP)

! the maximum number of variables supported in ARL format
! where the character field defines the variable type at the surface (0)
! and for the upper levels (1). Units conversion specified for each.

      PARAMETER (MVAR=6)
      CHARACTER*4 VCHAR0(MVAR), VCHAR1(MVAR)                                   
      CHARACTER*6 XCHAR0(MVAR), XCHAR1(MVAR)
      REAL        CNVRT0(MVAR), CNVRT1(MVAR)

!     number of ARL variables at the surface and each level
      INTEGER NVAR(0:1)
      DATA    NVAR/6,6/

!     define output variables in ARL character format
      DATA VCHAR0/ 'PRSS','TPPT','UMOF','VMOF','SHTF','SHGT'/
      DATA VCHAR1/ 'PRES','UWND','VWND','WWND','TEMP','SPHU'/

!     define equivalent COAMPS variables
!JJS      DATA XCHAR0/ 'tepr','prcp','strs','strs','hfls','topo'/
!JJS      DATA XCHAR1/ 'pppp','uuuu','vvvv','wwww','pott','wvap'/
      DATA XCHAR0/ 'trpres','ttlpcp','wstres','wstres','sehflx','terrht'/
      DATA XCHAR1/ 'ttlprs','uuwind','vvwind','wwwind','pottmp','wvapor'/

!     units conversion factors for COAMPS to ARL format
      DATA CNVRT0/  1.0, 0.001,  1.0,  1.0,  1.0 , 1.0   /
      DATA CNVRT1/  1.0,   1.0,  1.0,  1.0,  1.0 , 1.0   /

! check for required command line arguments
! where the model initialization time is given on the command line and
! 12 hours of forecast data will be processed with each execution

      NARG=IARGC()
      IF(NARG.LT.4)THEN
         WRITE(*,*)'Convert Navy COAMPS3 output files to ARL format'
         WRITE(*,*)' '
         WRITE(*,*)'Usage: cmp2arl [Arg-1] [Arg-2] [Arg-3] [Arg-4]'
         WRITE(*,*)'  1-[Directory] : Data directory with coamps files'
         WRITE(*,*)'  2-[yyyymmddhh]: Time of first file to series'
         WRITE(*,*)'  3-[ngrid]     : Selected coamps grid number'
         WRITE(*,*)'  4-[hour]      : Starting forecast hour'
         STOP
      END IF

!     decode argument list to determine data processing

      DO WHILE (NARG.GT.0)
         CALL GETARG(NARG,FNAME)
         IF(NARG.EQ.1)DATAD=FNAME
         IF(NARG.EQ.2)BASEF=FNAME
         IF(NARG.EQ.3)READ(FNAME,'(I1)')NGRID
         IF(NARG.EQ.4)READ(FNAME,'(I1)')NHOUR
         NARG=NARG-1
      END DO


! write ARL packing index file - the file is read by the packing
! subroutine initialization routine (pakset) and the information is
! extracted and written on each time period index record

!     grid information is decoded from the COAMPS data file
      CALL HEADER(DATAD,BASEF,NGRID,MZP,MXYP,NSIGMA,MPTYPE,                    &
         TNGLAT,ALIGN,NXP,NYP,DELX,XLAT1,XLON1,SIGMA,DSIGMA,REFLT1,REFLT2)

      CALL WINDEX(NGRID,NSIGMA,MPTYPE,TNGLAT,ALIGN,NXP,NYP,                    &
         DELX,XLAT1,XLON1,SIGMA,MVAR,NVAR,VCHAR0,VCHAR1,FNAME,REFLT1,REFLT2)
!     compute size of packed data array
      NXYP=NXP*NYP

!     initialize packing subroutines (use same unit number for writing)
      CALL PAKSET(20,FNAME,1,NXP,NYP,(NSIGMA+1))
      print *,' NXP=',NXP, '  NYP=',NYP


! construct standardized file name for ARL packed output file and
! open to write

      FNAME='ARL#_'//BASEF
      WRITE(FNAME(4:4),'(I1)')NGRID
      LREC=NXYP+50
      OPEN(20,FILE=FNAME,RECL=LREC,ACCESS='DIRECT',FORM='UNFORMATTED')

! the files for each forecast cycle are processed as a single group
! for 12 hours or until there are no more files in the directory

!PFC      DO KHOUR=NHOUR,12
      DO KHOUR=NHOUR,11
!        open and read COAMPS files that correspond to variable selection
         CALL FMTARL(NGRID,NSIGMA,NXP,NYP,NXYP,SIGMA,DSIGMA,MVAR,              &
            NVAR,VCHAR0,VCHAR1,XCHAR0,XCHAR1,CNVRT0,CNVRT1,                    &
            DATAD,BASEF,KHOUR,RVAR,CVAR,SVAR,WVAR)

!        close out time period by writing index record
         CALL PAKNDX(20)
      END DO
      CLOSE (20)

      END

!-------------------------------------------------------------------

      SUBROUTINE HEADER(DATAD,BASEF,NGRID,MZP,MXYP,NSIGMA,MPTYPE,              &
         TNGLAT,ALIGN,NXP,NYP,DELX,XLAT1,XLON1,SIGMA,DSIGMA,REFLT1,REFLT2)

! decodes COAMPS header file and extracts information required
! for conversion to ARL format and decoding appropriate data files
! Information checked if supported under ARL format conventions

      LOGICAL   FTEST
!JJS      REAL      HDATA(500), SIGMA(MZP), DSIGMA(MZP)
      REAL      HDATA(2000), SIGMA(MZP), DSIGMA(MZP)
      CHARACTER FNAME*80, DATAD*80, BASEF*10

!     construct file name     hhhmmss    level-1  level-2
!JJS      FNAME='dataa#'//BASEF//'0000000'//'00000'//'00000'//'sfl'
      FNAME='datahd_sfc_000000_000000_1a2000x0001_'//BASEF                     &
            //'_00000000_infofld'
!     header file only written for grid #1
!JJS      WRITE(FNAME(6:6),'(A1)')'1'
!     WRITE(FNAME(6:6),'(I1)')NGRID

!     truncate directory string
      KLEN=INDEX(DATAD,' ')-1

!     test for existence of data file before proceeding to decoder
      INQUIRE(FILE=DATAD(1:KLEN)//FNAME,EXIST=FTEST)
      IF(FTEST)THEN
         OPEN(10,FILE=DATAD(1:KLEN)//FNAME,FORM='FORMATTED',                   &
               STATUS='OLD',ERR=900)
         WRITE(*,*)'Started processing: ',FNAME
         READ(10,'(5E13.6)')HDATA
         write (11,'(5E13.6)') HDATA

!        number of full sigma levels
         NSIGMA=INT(HDATA(2))
         IF(NSIGMA.GT.MZP)THEN
            WRITE(*,*)'ERROR: header - exceeding max allowed levels'
            WRITE(*,*)'  Dimensioned limit  : ',MZP
            WRITE(*,*)'  Levels in data file: ',NSIGMA
            WRITE(*,*)'  Recompile program with larger MZP limits'
            STOP
         END IF

!        map projection: 1-mercator 2-lambert 3-polar 4-cartesian 5-spherical
         MPTYPE=INT(HDATA(3))
         IF(MPTYPE.GT.3)THEN
            WRITE(*,*)'ERROR: header - map projection not supported'
            WRITE(*,*)'  Projection type: ',MPTYPE
            STOP
         END IF

!        map projection intersection latitudes
         REFLT1=HDATA(4)
         REFLT2=HDATA(5)
!        convert intersection latitude to ARL mapping convention
         TNGLAT=REFLT1
         IF(MPTYPE.EQ.2)TNGLAT=EQVLAT(REFLT1,REFLT2)
!        grid alignment longitude
         ALIGN=HDATA(6)
!        coarse projection standard latitude & longitude
!        appears to be the center of the grid
         STDLAT=HDATA(7)
         STDLON=HDATA(8)

!        number of grid points in the x and y directions
!JJS         NXP=INT(HDATA( (NGRID+2)*10+0 ))
!JJS         NYP=INT(HDATA( (NGRID+2)*10+1 ))
         NXP=INT(HDATA( NGRID*30+0 ))
         NYP=INT(HDATA( NGRID*30+1 ))
         IF(NXP*NYP.GT.MXYP)THEN
            WRITE(*,*)'ERROR: header - exceeding max grid points'
            WRITE(*,*)'  Dimensioned limit : ',MXYP
            WRITE(*,*)'  X,Y points in file: ',NXP,NYP
            WRITE(*,*)'  Recompile program with larger MXP,MYP limits'
            STOP
         END IF

!        grid spacing (m) in the x and y directions
!JJS         DELX=HDATA(9) /3**(NGRID-1)
!JJS         DELY=HDATA(10)/3**(NGRID-1)
         DELX=HDATA( NGRID*30+7 )
         DELY=HDATA( NGRID*30+8 )
         IF(DELX.NE.DELY)THEN
            WRITE(*,*)'ERROR: header - only square grid cells supported'
            WRITE(*,*)'   Grid size x,y: ',DELX,DELY
            STOP
         END IF

!        latitude and longitude of 1,1 nest grid point
!JJS         XLAT1=HDATA( (NGRID+2)*10+9 ) 
!JJS         XLON1=HDATA( (NGRID+2)*10+10 ) 
         XLAT1=HDATA( NGRID*30+9)
!PFC         XLON1=HDATA( NGRID*30+10 )
         SELECT CASE (NGRID)
	   CASE(1) 
	     XLON1=197.148
	   CASE(2) 
	     XLON1=201.079
	   CASE(3) 
	     XLON1=200.491
	   CASE(4) 
	     XLON1=201.638
	 END SELECT

!        enter dsigma level heights into array
         DO K=1,NSIGMA
!JJS            DSIGMA(K)=HDATA(100+K)
            DSIGMA(K)=HDATA(500+K)
         END DO

!        enter sigma level heights into array
         DO K=1,NSIGMA
!JJS            SIGMA(K)=HDATA(200+K)
            SIGMA(K)=HDATA(800+K)
         END DO

      ELSE
         WRITE(*,*)'ERROR: header - File not found: '
         WRITE(*,*)'  Directory: ',DATAD(1:KLEN)
         WRITE(*,*)'  File     : ',FNAME
         STOP
      END IF

!     normal termination
      CLOSE (10)
      RETURN

!     abnormal termination
  900 WRITE(*,*)'ERROR: header - opening/reading header file: ',FNAME
      STOP
      END

!-------------------------------------------------------------------

      SUBROUTINE WINDEX(NGRID,NSIGMA,MPTYPE,TNGLAT,ALIGN,NXP,NYP,              &
         DELX,XLAT1,XLON1,SIGMA,MVAR,NVAR,VCHAR0,VCHAR1,FNAME,REFLT1,REFLT2)

! creates the index record configuration file required for ARL packing
! information about grid, variables, and the order they are written.
! The index CFG file is opened by the packing initialization subroutine
! and the file is created only upon the first entry to this routine.

!     file name construct
      CHARACTER FNAME*80

!     array to hold grid information
      REAL GRIDS(12)
!     heights of each sigma level
      REAL SIGMA(NSIGMA)
!     number of variables per level
      INTEGER NVAR(0:1)
!     arl variable character IDs for surface and upper level
      CHARACTER*4 VCHAR0(MVAR), VCHAR1(MVAR)

!     grid orientation (meridian parallel to grid)
      GRIDS(6)=0.0

!     lat/lon of grid point 1,1
      GRIDS(8)=1.
      GRIDS(9)=1.
      GRIDS(10)=XLAT1
      GRIDS(11)=XLON1
!     unused variable
      GRIDS(12)=0.0

!     defines a polar sterographic projection
      IF(MPTYPE.EQ.3)THEN

!        pole position of grid projection
         IF(TNGLAT.GT.0.0)THEN
            GRIDS(1)=90.0
         ELSE
            GRIDS(1)=-90.0
         END IF
         GRIDS(2)=0.0
!        position that defines grid spacing
         GRIDS(3)=TNGLAT
         GRIDS(4)=ALIGN
!        delta=x grid size
         GRIDS(5)=DELX/1000.0
!        cone angle
         GRIDS(7)=90.0

!     defines a merecator projection
      ELSEIF(MPTYPE.EQ.1)THEN

!        pole position of grid projection
         GRIDS(1)=90.0
         GRIDS(2)=0.0
!        position that defines grid spacing
         GRIDS(3)=TNGLAT
         GRIDS(4)=ALIGN
!        longitudinal direction grid length
         GRIDS(5)=DELX/1000.0
!        cone angle
         GRIDS(7)=0.0

!     defines a lambert conformal projection
      ELSEIF(MPTYPE.EQ.2)THEN

!        pole position of grid projection
         GRIDS(1)=TNGLAT
         GRIDS(2)=ALIGN
!        position that defines grid spacing
!PFC        GRIDS(3)=TNGLAT
         GRIDS(3)=REFLT2
         GRIDS(4)=ALIGN
!        delta=x grid size
         GRIDS(5)=DELX/1000.0
!        cone angle
         GRIDS(7)=TNGLAT

      END IF

! configuration file is constructed each time the program is executed
! filename should be unique for each series of packed data

      FNAME='CFG#_COAMP'
      WRITE(FNAME(4:4),'(I1)')NGRID

!     write the packer configuration file
      OPEN(10,FILE=FNAME)

!     write the abbreviated data set name (by grid number)
      WRITE(10,'(20X,A3,I1)')'CMP',NGRID
!     grid number 99 and 3 for terrain coordinate system
      WRITE(10,'(20X,I4)') 99, 3
!     information about grid orientation
      WRITE(10,'(20X,F10.2)')(GRIDS(I),I=1,12)
!     grid size and levels (+1 for surface)
      WRITE(10,'(20X,I4)')NXP,NYP,(NSIGMA+1)

!     surface level information
      HEIGHT=0.0
      WRITE(10,'(20X,F6.0,I3,20(1X,A4))')                                      &
        HEIGHT,NVAR(0),(VCHAR0(N),N=1,NVAR(0))

!     upper level information
      DO K=1,NSIGMA
         HEIGHT=SIGMA(NSIGMA-K+1)
         WRITE(10,'(20X,F6.0,I3,20(1X,A4))')                                   &
           HEIGHT,NVAR(1),(VCHAR1(N),N=1,NVAR(1))
      END DO

      CLOSE (10)
      RETURN
      END

!--------------------------------------------------------------------

      SUBROUTINE FMTARL(NGRID,NSIGMA,NXP,NYP,NXYP,SIGMA,DSIGMA,MVAR,           &
         NVAR,VCHAR0,VCHAR1,XCHAR0,XCHAR1,CNVRT0,CNVRT1,                       &
         DATAD,BASEF,KHOUR,RVAR,CVAR,SVAR,WVAR)

! opens each COAMPS file for a specified time according the equivalent
! variable list of ARL data to pack. Units converted if necessary and
! written to ARL packed data format, one record per variable per level.

!     file name construct and testing
!JJS      CHARACTER FNAME*36, DATAD*80, BASEF*10, BTIME*7
      CHARACTER FNAME*80, DATAD*80, BASEF*10, BTIME*8
      LOGICAL FTEST, ECONV 

!     heights and delta of each sigma level
      REAL SIGMA(NSIGMA), DSIGMA(NSIGMA)
!     number of variables per level
      INTEGER NVAR(0:1)
!     arl variable character IDs for surface and upper level
      CHARACTER*4 VCHAR0(MVAR), VCHAR1(MVAR)
!     coamps equivalent character IDs for surface and upper level
      CHARACTER*6 XCHAR0(MVAR), XCHAR1(MVAR)
!     coamps to arl unit conversion constants
      REAL        CNVRT0(MVAR), CNVRT1(MVAR)

!     unpacked output array
      REAL RVAR(NXP,NYP,NSIGMA), SVAR(NXP,NYP,NSIGMA), WVAR(NXP,NYP)
!     packed output array
      CHARACTER CVAR(NXYP)*1
!     to convert from big_endian to little_endian or vice versa.
!     change to .false. if no conversion is needed.
      DATA ECONV/.true./

! file name from variable, date, and time. Note that ARL packed format
! data use 2 digit year identification.  All related Hysplit4 data
! subroutines Y2K compliant.  Two digit year format goes back to data
! archive starting in 1987.

      KMIN=0
!     extract current date from file name base
      READ(BASEF,'(I4,3I2)')KYR,IMO,IDA,IHR
      IYR=KYR-100*INT(KYR/100)
!     adjust for forecast hour
      CALL TMPLUS(IYR,IMO,IDA,IHR,KHOUR)

! process surface variables first

      DO N=1,NVAR(0)
!        default units conversion
         CNVRT=CNVRT0(N)

!        special processing for variables with no equivalent
!JJS         IF(XCHAR0(N).EQ.'topo')THEN
         IF(XCHAR0(N).EQ.'terrht')THEN
!           special constant fields base time = 0
            BTIME='00000000'
!JJS         ELSEIF(XCHAR0(N).EQ.'prcp')THEN
         ELSEIF(XCHAR0(N).EQ.'ttlpcp')THEN
!           precip only available at 12 forecast, req each time
            BTIME='00120000'
            CNVRT= FLOAT(KHOUR)/12.0
         ELSE
!          construct base time (hhhmmss) for file name
           BTIME='####0000'
           WRITE(BTIME(1:4),'(I4.4)')KHOUR
!JJS           IF(XCHAR0(N).EQ.'strs')THEN
           IF(XCHAR0(N).EQ.'wstres')THEN
!             component stress not available, but write as component
!             so that sum of component squared equals scalar stress
              CNVRT=0.7071068
            END IF
         END IF

!        construct file name
!JJS         FNAME=XCHAR0(N)//'a#'//BASEF//BTIME//'00000'//'00000'//'sfl'
         FNAME=XCHAR0(N)//'_sfc_000000_000000_#a####x####_'//BASEF//          &
               '_'//BTIME//'_fcstfld'
!        add current grid number to name
         WRITE(FNAME(26:26),'(I1)')NGRID
!JJS add 10/31/02
         WRITE(FNAME(28:31),'(I4.4)')NXP
         WRITE(FNAME(33:36),'(I4.4)')NYP
!JJS add 10/31/02
!        truncate directory string
         KLEN=INDEX(DATAD,' ')-1

!        test for existence of data file before proceeding to decoder
         INQUIRE(FILE=DATAD(1:KLEN)//FNAME,EXIST=FTEST)
         IF(FTEST)THEN

!JJS            OPEN(10,FILE=DATAD(1:KLEN)//FNAME,FORM='UNFORMATTED',              &
!JJS                ACCESS='SEQUENTIAL',STATUS='OLD',ERR=900)
            OPEN(10,FILE=DATAD(1:KLEN)//FNAME,FORM='UNFORMATTED',              &
                ACCESS='DIRECT',RECL=NXYP*4,STATUS='OLD',ERR=900)
            WRITE(*,*)'Started processing: ',FNAME
            READ(10,REC=1)((RVAR(I,J,1),I=1,NXP),J=1,NYP)
            CLOSE (10)
!JJS add 11/15/02
            IF (ECONV) CALL SWAP32(RVAR(1,1,1),NXYP) 
!JJS add 11/15/02

!           perform any required units conversion
            CALL DATCNV(.FALSE.,RVAR(1,1,1),NXP,NYP,CNVRT,0.0,                 &
                 RMIN,RMAX)
!           then pack into ARL format and continue
            CALL PAKREC(20,RVAR(1,1,1),CVAR,NXP,NYP,NXYP,VCHAR0(N),            &
                 IYR,IMO,IDA,IHR,KMIN,KHOUR,1,0)
         ELSE
            IF(XCHAR0(N).EQ.'trpres')THEN
!              surface pressure variable will be computed from upper level
!              save index for use in upper level loop
               KSFCP=N
            ELSE
               WRITE(*,*)'WARNING: fmtarl - file not found: ',FNAME
            END IF
         END IF
      END DO

! process data files at sigma levels

! construct base time (hhhmmss) for file name
      BTIME='####0000'
      WRITE(BTIME(1:4),'(I4.4)')KHOUR

      DO N=1,NVAR(1)

!        col:    1->4    56   7->16  17->23  24->28   29->33  34->36
!JJS         FNAME=XCHAR1(N)//'a#'//BASEF//BTIME//'00000'//'00000'//'sgl'
         FNAME=XCHAR1(N)//'_sig_######_######_#a####x####_'//BASEF//          &
               '_'//BTIME//'_fcstfld'
!JJS add 10/31/02
         WRITE(FNAME(28:31),'(I4.4)')NXP
         WRITE(FNAME(33:36),'(I4.4)')NYP
!JJS add 10/31/02

!        update name with level information
!JJS         WRITE(FNAME(24:28),'(I5.5)')INT(SIGMA(1))
!JJS         WRITE(FNAME(29:33),'(I5.5)')INT(SIGMA(NSIGMA))
!JJS         IF(XCHAR1(N).EQ.'wwww')THEN
!JJS            WRITE(FNAME(24:28),'(I5.5)')                                       &
!JJS                  INT(SIGMA(1)+DSIGMA(1)/2.0)
!JJS            WRITE(FNAME(29:33),'(I5.5)')                                       &
!JJS                  INT(SIGMA(NSIGMA)-DSIGMA(NSIGMA)/2.0)
!JJS         END IF
         WRITE(FNAME(12:17),'(I6.6)')INT(SIGMA(1))
         WRITE(FNAME(19:24),'(I6.6)')INT(SIGMA(NSIGMA))
         IF(XCHAR1(N).EQ.'wwwind')THEN
            WRITE(FNAME(12:17),'(I6.6)')                                       &
                  INT(SIGMA(1)+DSIGMA(1)/2.0)
            WRITE(FNAME(19:24),'(I6.6)')                                       &
                  INT(SIGMA(NSIGMA)-DSIGMA(NSIGMA)/2.0)
         END IF

!        add current grid number to name
!JJS         WRITE(FNAME(6:6),'(I1)')NGRID
         WRITE(FNAME(26:26),'(I1)')NGRID
!        truncate directory string
         KLEN=INDEX(DATAD,' ')-1
!        test for existence of data file before proceeding to decoder
         INQUIRE(FILE=DATAD(1:KLEN)//FNAME,EXIST=FTEST)
         IF(FTEST)THEN

!JJS            OPEN(10,FILE=DATAD(1:KLEN)//FNAME,FORM='UNFORMATTED',              &
!JJS                ACCESS='SEQUENTIAL',STATUS='OLD',ERR=900)
!JJS            WRITE(*,*)'Started processing: ',FNAME

!           special processing for vertical velocity due to staggered
!           coordinate system from the remaining diagnostic variables

            IF(XCHAR1(N).EQ.'wwwind')THEN
!              vertical velocities defined at level interfaces with
!              additional wvar is the value at the ground surface
!              requiring NSIGMA+1 data levels for input
!JJS add 11/1/02
               NSIGMA1=NSIGMA+1
               OPEN(10,FILE=DATAD(1:KLEN)//FNAME,FORM='UNFORMATTED',           &
                   ACCESS='DIRECT',RECL=NXYP*NSIGMA1*4,STATUS='OLD',ERR=900)
               WRITE(*,*)'Started processing: ',FNAME
!JJS add 11/1/02
               READ(10,REC=1)RVAR,WVAR
!JJS add 11/15/02 
               IF (ECONV) THEN
                  CALL SWAP32(WVAR,NXYP)
                  NXYZP=NXYP*NSIGMA
                  CALL SWAP32(RVAR(1,1,1),NXYZP)
               ENDIF
!JJS add 11/15/02 

!              average vertical velocities to define mid-point value
               CALL WWWCNV(RVAR,WVAR,NXP,NYP,NSIGMA)

            ELSE
!              all other diagnostic variables of NSIGMA levels
!JJS add 11/1/02
               OPEN(10,FILE=DATAD(1:KLEN)//FNAME,FORM='UNFORMATTED',           &
                   ACCESS='DIRECT',RECL=NXYP*NSIGMA*4,STATUS='OLD',ERR=900)
               WRITE(*,*)'Started processing: ',FNAME
!JJS add 11/1/02
               READ(10,REC=1)RVAR
!JJS add 11/15/02 
               IF (ECONV) THEN
                  NXYZP=NXYP*NSIGMA
                  CALL SWAP32(RVAR(1,1,1),NXYZP)
               ENDIF
!JJS add 11/15/02 
            END IF
            CLOSE (10)

!           loop through each level and pack as data record
            DO K=1,NSIGMA

!              perform any required units conversion
               CALL DATCNV(.FALSE.,RVAR(1,1,K),NXP,NYP,                        &
                    CNVRT1(N),0.0,RMIN,RMAX)

!              shift winds down by 0.5 grid points
               IF(XCHAR1(N).EQ.'uuwind')                                         &
                  CALL UUUCNV(RVAR(1,1,K),WVAR,NXP,NYP)
               IF(XCHAR1(N).EQ.'vvwind')                                         &
                  CALL VVVCNV(RVAR(1,1,K),WVAR,NXP,NYP)

!              convert potential temperature to temperature
               IF(XCHAR1(N).EQ.'pottmp')                                         &
                  CALL TMPCNV(RVAR(1,1,K),SVAR(1,1,K),NXP,NYP)

!              then pack into ARL format and continue
               CALL PAKREC(20,RVAR(1,1,K),CVAR,NXP,NYP,NXYP,VCHAR1(N),         &
                    IYR,IMO,IDA,IHR,KMIN,KHOUR,(NSIGMA-K+2),0)
            END DO

!           additional surface variables estimated from upper levels
            IF(XCHAR1(N).EQ.'ttlprs')THEN

!              raise surface pressure 1 hPa for every 10 meters
               ADJUST=SIGMA(NSIGMA)/10.0
               CALL DATCNV(.FALSE.,RVAR(1,1,NSIGMA),NXP,NYP,                   &
                    1.0,ADJUST,RMIN,RMAX)
               CALL PAKREC(20,RVAR(1,1,NSIGMA),CVAR,NXP,NYP,NXYP,              &
                    VCHAR0(KSFCP),IYR,IMO,IDA,IHR,KMIN,KHOUR,1,0)

!              also save for processing potential temperature
               DO K=1,NSIGMA
               DO J=1,NYP
               DO I=1,NXP
                  SVAR(I,J,K)=RVAR(I,J,K)
               END DO
               END DO
               END DO
            END IF

         ELSE
            WRITE(*,*)'WARNING: fmtarl - file not found: ',FNAME
         END IF
      END DO

      RETURN

  900 WRITE(*,*)'ERROR: fmtarl - opening file: ',FNAME
      STOP
      END

!-----------------------------------------------------------

      SUBROUTINE DATCNV(DIAG,RVAR,NXP,NYP,CNVRT,ADJUST,RMIN,RMAX)

! units conversion from coamps to arl

      LOGICAL DIAG
      REAL RVAR(NXP,NYP)

      RMIN=+1.0E+25
      RMAX=-1.0E-25

      DO J=1,NYP
      DO I=1,NXP
         IF(CNVRT.NE.1.0.OR.ADJUST.NE.0.0)                                     &
            RVAR(I,J)=RVAR(I,J)*CNVRT+ADJUST
         RMIN=MIN(RMIN,RVAR(I,J))
         RMAX=MAX(RMAX,RVAR(I,J))
      END DO
      END DO

      IF(DIAG)WRITE(*,*)'Minimum: ',RMIN,'    Maximum: ',RMAX

      RETURN
      END

!-----------------------------------------------------------

      SUBROUTINE TMPCNV(RVAR,SVAR,NXP,NYP)

! convert potential temperature to temperature

      REAL RVAR(NXP,NYP), SVAR(NXP,NYP)

      DO J=1,NYP
      DO I=1,NXP
         RVAR(I,J)=RVAR(I,J)*(SVAR(I,J)/1000.0)**0.286
      END DO
      END DO

      RETURN
      END

!-----------------------------------------------------------

      SUBROUTINE WWWCNV(RVAR,WVAR,NXP,NYP,NSIGMA)

! average vertical velocities to match height of other diagnostic
! variables

      REAL RVAR(NXP,NYP,NSIGMA), WVAR(NXP,NYP)

      DO J=1,NYP
      DO I=1,NXP
!        the k index starts from the top and goes down
         DO K=1,(NSIGMA-1)
            RVAR(I,J,K)=0.5*(RVAR(I,J,K)+RVAR(I,J,K+1))
         END DO
!        ground level value defined as additional variable wvar
         RVAR(I,J,NSIGMA)=0.5*(RVAR(I,J,NSIGMA)+WVAR(I,J))
      END DO
      END DO

      RETURN
      END

!------------------------------------------------------------

      SUBROUTINE UUUCNV(RVAR,WVAR,NXP,NYP)

! interpolate u shifted components to position of thermodynamic
! variables (u is offset by +0.5 in x)

      REAL RVAR(NXP,NYP), WVAR(NXP,NYP)

      DO J=1,NYP

!        at the edges special action, upper edge always zero
         WVAR(1,J)=RVAR(1,J)
         WVAR(2,J)=0.5*(RVAR(1,J)+RVAR(2,J))
         WVAR(NXP,J)=RVAR(NXP-1,J)
         WVAR(NXP-1,J)=0.5*(RVAR(NXP-1,J)+RVAR(NXP-2,J))

         DO I=3,(NXP-2)
!           initialize regression coefficient
            CALL LINEAR(0,X,Y,B0,B1,B2)

!           sum preceeding two and forward by one
            DO II=I-2,I+1
               CALL LINEAR(1,FLOAT(II),RVAR(II,J),B0,B1,B2)
            END DO

!           compute regression coefficients
            CALL LINEAR(-1,X,Y,B0,B1,B2)

!           and then compute variable value at thermodynamic point
!           which is shifted down by 0.5 grid points
            XP=I-0.5
            WVAR(I,J)=B2*XP*XP+B1*XP+B0
         END DO

!        fill new data back into array
         DO I=1,NXP
            RVAR(I,J)=WVAR(I,J)
         END DO

      END DO

      RETURN
      END

!------------------------------------------------------------

      SUBROUTINE VVVCNV(RVAR,WVAR,NXP,NYP)

! interpolate v shifted components to position of thermodynamic
! variables (v is offset by +0.5 in y)

      REAL RVAR(NXP,NYP), WVAR(NXP,NYP)

      DO I=1,NXP

!        at the edges special action
         WVAR(I,1)=RVAR(I,1)
         WVAR(I,2)=0.5*(RVAR(I,1)+RVAR(I,2))
         WVAR(I,NYP)=RVAR(I,NYP-1)
         WVAR(I,NYP-1)=0.5*(RVAR(I,NYP-1)+RVAR(I,NYP-2))

         DO J=3,(NYP-2)
!           initialize regression coefficient
            CALL LINEAR(0,X,Y,B0,B1,B2)

!           sum preceeding two and forward by one
            DO JJ=J-2,J+1
               CALL LINEAR(1,FLOAT(JJ),RVAR(I,JJ),B0,B1,B2)
            END DO

!           compute regression coefficients
            CALL LINEAR(-1,X,Y,B0,B1,B2)

!           and then compute variable value at thermodynamic point
!           which is shifted down by 0.5 grid points
            YP=J-0.5
            WVAR(I,J)=B2*YP*YP+B1*YP+B0
         END DO

!        fill new data back into array
         DO J=1,NYP
            RVAR(I,J)=WVAR(I,J)
         END DO

      END DO

      RETURN
      END

!------------------------------------------------------------

      SUBROUTINE LINEAR(K,X,Y,B0,B1,B2)

! solution of the equation: Y = B0 + B1 X + B2 X^2
! for regression solutions

      SAVE A,B,C,D,E,C1,C2,C3

      IF(K.EQ.0)THEN
!        initialize regression sums
         A=0.0
         B=0.0
         C=0.0
         D=0.0
         E=0.0
         C1=0.0
         C2=0.0
         C3=0.0

      ELSEIF(K.LT.0)THEN
!        computes solution for set of simultaneous linear equations
!        where x=B0; y=B1; z=B2 as defined in the argument list
!        Ax + By + Cz = C1
!        Bx + Cy + Dz = C2
!        Cx + Dy + Ez = C3

         B2=( (C1*B/A-C2)*(B*C/A-D)-(C1*C/A-C3)*(B*B/A-C) ) /                  &
            ( (B*C/A-D)**2 - (C*C/A-E)*(B*B/A-C) )

         B1=(C1*B/A-C2)/(B*B/A-C) - B2*(B*C/A-D)/(B*B/A-C)

         B0=C1/A - B1*B/A - B2*C/A

      ELSE
!        perform summation
         A=A+1.0
         B=B+X
         C=C+X*X
         D=D+X*X*X
         E=E+X*X*X*X
         C1=C1+Y
         C2=C2+X*Y
         C3=C3+X*X*Y

      END IF

      RETURN
      END

       SUBROUTINE SWAP32(A,N)
!
!      REVERSE ORDER OF BYTES IN INTEGER*4 WORD, or REAL*4 OF N ELEMENTS
       INTEGER*4   A(N)
!
       CHARACTER*1 JTEMP(4)
       CHARACTER*1 KTEMP
!
       EQUIVALENCE (JTEMP(1),ITEMP)
!
       SAVE
!
       DO 10 I = 1,N
         ITEMP    = A(I)
         KTEMP    = JTEMP(4)
         JTEMP(4) = JTEMP(1)
         JTEMP(1) = KTEMP
         KTEMP    = JTEMP(3)
         JTEMP(3) = JTEMP(2)
         JTEMP(2) = KTEMP
         A(I)     = ITEMP
 10    CONTINUE
       RETURN
       END
