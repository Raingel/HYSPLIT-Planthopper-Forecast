!$$$  MAIN PROGRAM DOCUMENTATION BLOCK
!
! MAIN PROGRAM: NCR2ARL      DECODE GRIB REANALYSIS FIELDS FOR HYSPLIT
!   PRGMMR: DRAXLER          ORG: R/E/AR      DATE: 1998-08-26
!
! ABSTRACT:  THIS CODE WRITTEN AT THE AIR RESOURCES LABORATORY ...
!     converts ncar/ncep reanalysis grib files to arl packed format
!     processes only one time period per execution. Input grib data file
!     should contain only data from one time period.  Data are converted
!     from lat/lon to conformal map projection.  The conformal map is
!     at 100km resolution of 100x100 points centered at the lat/lon
!     given on the command line.
!
! PROGRAM HISTORY LOG:
!   LAST REVISED: 18 Feb 1997 (RRD)
!                  8 Jul 1998 (RRD) - avn extra levels relh
!                  3 Feb 1999 (RRD) - fixed bug in mercator configuration
!                                   - added lambert conformal option
!                                   - prime meridian interpolation patch
!                 22 Feb 1999 (RRD) - modified for 2.5 deg pressure reanalysis
!                 12 Nov 1999 (RRD) - pc argument list compatibility
!                 10 Mar 2000 (RRD) - pc unix IO compatibility
!                 21 Dec 2000 (RRD) - fortran90 upgrade
!                 05 Mar 2002 (RRD) - improved w3lib compatibility
!                 16 Jul 2002 (RRD) - ecmwf grib test
!                 24 Feb 2010 (RRD_ - pressure level test
!
! USAGE:  NCR2ARL [GRIB_FILE] [CENTER_LAT] [CENTER_LON]
!   INPUT ARGUMENT LIST:
!     GRIB_FILE - name of input grib data
!     CENTER_LAT - latitude of output grid center
!     CENTER_LON - longitude of output grid center
!   OUTPUT ARGUMENT LIST:
!     NONE
!   INPUT FILES:
!     grib input data files (no unit #, uses special directIO routines)
!   OUTPUT FILES:
!     unit 20 DATA.NCR - ARL packed data output file
!     unit 30 CFG_NCR - characteristics of ouput grid
!     unit 40 NCRTIME - time indicator file
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE:  CRAY
!
!$$$

      PROGRAM NCR2ARL
!*PC* USE DFLIB

      LOGICAL   FTEST
      CHARACTER FNAME*80
      INTEGER*4 HANDLE

!->used by directIO library routines
      INTEGER*4 FCOPEN

!=>required for NCEP operational implementation
!     CALL W3TAGB('NCR2ARL ',1998,0238,0067,'R/E/AR ')

!=>check for command line arguments
!     WIN32:NARGS()=arguments+command    UNIX:IARGC()=arguments
      NARG=IARGC()
!     NARG=NARGS()-1

      IF(NARG.EQ.0.OR.NARG.EQ.2.OR.NARG.GT.3)THEN
         WRITE(*,*)'Converts ncar/ncep reanalysis grib files to arl format'
         WRITE(*,*)'Usage: ncr2arl [file] [clat] [clon]'
         STOP
      END IF

!     process optional grid center latitude longitude
      IF(NARG.EQ.3)THEN
         CALL GETARG(2,FNAME)
         READ(FNAME,'(F10.0)')CLAT
         CALL GETARG(3,FNAME)
         READ(FNAME,'(F10.0)')CLON
      ELSE
         CLAT=0.0
         CLON=0.0
      END IF

!     open input data file
      CALL GETARG(1,FNAME)
      INQUIRE(FILE=FNAME,EXIST=FTEST)
      IF(FTEST)THEN
!->used by directIO library routines
         HANDLE=FCOPEN(FNAME,'r')

!        main decoding routine (assume one time period per process)
         WRITE(*,*)'started processing: ',FNAME
         CALL XTRACT(HANDLE,CLAT,CLON)

!->used by directIO library routines
         CALL FCCLOS(HANDLE,*900)
  900    CONTINUE
      ELSE
         WRITE(*,*)'File not found:',FNAME
      END IF

!     close out time period and write index record
      CALL PAKNDX(20)
      CLOSE (20)

!=>required for NCEP operational implementation
!     CALL W3TAGE('NCR2ARL ')

      END


!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM:  EXTRACT          MAIN ROUTINE FOR DECODING GRIB DATA
!   PRGMMR:    ROLAND DRAXLER   ORG: R/E/AR      DATE:96-06-01
!
! ABSTRACT:  THIS CODE WRITTEN AT THE AIR RESOURCES LABORATORY ...
!            EXTRACTS ONE RECORD FROM INPUT GRIB FILE AND ADDS ONE
!            RECORD TO THE OUTPUT FILE CENTERED ABOUT INDICATED LAT/LON
!
! PROGRAM HISTORY LOG:
!   LAST REVISED: 10 Jul 1997 - RRD
!                 25 Feb 2003 (RRD) - ichar function replacement
!
! USAGE:  CALL XTRACT(HANDLE,CLAT,CLON)
!   INPUT ARGUMENT LIST:
!     HANDLE - defines input file to directIO routines
!     CLAT,CLON - center position of output grid
!   OUTPUT ARGUMENT LIST:
!     NONE
!   INPUT FILES:
!     HANDLE defines input data file
!   OUTPUT FILES:
!     NONE
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE:  CRAY
!
!$$$

      SUBROUTINE XTRACT(HANDLE,CLAT,CLON)

!     input buffer array limit
      PARAMETER (MAXLON=144,MAXLAT=73,MAXB=150000)

!     number of levels, variables, and output buffer array limits
      PARAMETER (NLVL=12, MVAR=6, MAXX=100, MAXY=100)
      PARAMETER (MAXC=MAXX*MAXY)

!     arrays to hold grib, character, and level variable information
      INTEGER     HANDLE, VGRIB, VGRIB0(MVAR), VGRIB1(MVAR),                   &
                  STYP(MVAR), SIG0(MVAR), SIGL(NLVL), NVAR(NLVL)
      CHARACTER*4 VCHAR, VCHAR0(MVAR), VCHAR1(MVAR)
      REAL        CNVRT, CNVRT0(MVAR), CNVRT1(MVAR)

!     define arrays for product definition section, grid description,
!     and other grib information
      INTEGER   KPDS(25),KGDS(25),KPTR(25)

      LOGICAL FTEST
!     input data buffer
      CHARACTER BUFF(MAXB)*1
!     bit map section buffer
      LOGICAL*1 KBMS(MAXB)

!     unpacked input and output array
      REAL RVAR(MAXLON,MAXLAT), XVAR(MAXX,MAXY), YVAR(MAXX,MAXY)

!     lat/lon grid conversion equivalence table
      REAL TLAT(MAXX,MAXY),TLON(MAXX,MAXY)

!     packed output array
      CHARACTER CVAR(MAXC)*1, FNAME*80

!     remap array from one to two dimensions
      REAL SVAR(MAXLON*MAXLAT)
      INTEGER SHAPE(2)
      DATA SHAPE/MAXLON,MAXLAT/

!     default information (grid id, output records,
      DATA IG/99/, KREC/0/

!     predefine number of variables surface and aloft
!     RELH drops out after 300 mb
      DATA NVAR / 1, 7*6, 4*5/

!     set output structure by defining GRIB variables (sfc + levels 1->n)
      DATA VGRIB0/  1,  0,  0,  0,  0,  0/
      DATA VGRIB1/  7, 11, 33, 34, 39, 52/

!     special surface variables level identification
      DATA SIG0/  0,  0,  0,  0,  0,   0/
      DATA STYP/  1,  0,  0,  0,  0,   0/

!     set output structure in ARL character format
      DATA VCHAR0                                                              &
         /'PRSS','    ','    ','    ','    ','    '/
      DATA VCHAR1                                                              &
         /'HGTS','TEMP','UWND','VWND','WWND','RELH'/

!     set requested sigma levels for input and output
      DATA SIGL/ 0, 1000,925,850,700,500,400,300,250,200,150,100/

!     units conversion factors before converting to ARL format
      DATA CNVRT0/ 0.01,   0.0,  0.0,  0.0,  0.0,  0.0/
      DATA CNVRT1/  1.0,   1.0,  1.0,  1.0, 0.01,  1.0/

!     lat/lon grid corner (1,1) and increment
      DATA CLAT1/90.0/, CLON1/0.0/, DLAT/2.5/, DLON/2.5/

      SAVE KREC

!-------------------------------------------------------------------------------
! When dealing with some F90 compilers, replace ICHAR below with JCHAR function
  CHARACTER(1)        :: mychr
  INTEGER             :: jchar
  JCHAR(MYCHR)=IAND(ICHAR(MYCHR),255)
!-------------------------------------------------------------------------------

  INTERFACE
  SUBROUTINE W3FI63(BUFF,KPDS,KGDS,KBMS,RVAR,KPTR,KRET)
  CHARACTER(1), INTENT(IN)  :: BUFF(:)
  LOGICAL(1),   INTENT(OUT) :: KBMS(:)
  INTEGER,      INTENT(OUT) :: KPDS(:)
  INTEGER,      INTENT(OUT) :: KGDS(:)
  REAL,         INTENT(OUT) :: RVAR(:)
  INTEGER,      INTENT(OUT) :: KPTR(:)
  INTEGER,      INTENT(OUT) :: KRET
  END SUBROUTINE w3fi63
  END INTERFACE

!------------------------------------------------------------------

!     input grib record byte counter
      KBYTE=0

!     read the indicator section
  100 CONTINUE
!->used by directIO library routines
      CALL FCPTPS(HANDLE,KBYTE,*900)
      CALL FCREAD(HANDLE,BUFF,1,8,*900)

!     grib test for ecmwf compatibility
      IF(BUFF(1)//BUFF(2)//BUFF(3)//BUFF(4).NE.'GRIB')THEN
         KBYTE=KBYTE+1
         GOTO 100
      END IF

!     determine the length of the entire grib record
      KLEN=JCHAR(BUFF(7))+JCHAR(BUFF(6))*256+JCHAR(BUFF(5))*65536
      IF(KLEN.GT.MAXB)THEN
         WRITE(*,*)'Grib record: ',KLEN
         WRITE(*,*)'Exceedes buffer: ',MAXB
         STOP
      END IF

!     load the indicator section and pds segment into the buffer
!->used by directIO library routines
      CALL FCPTPS(HANDLE,KBYTE,*900)
      CALL FCREAD(HANDLE,BUFF,1,36,*900)

!     product definition section (+8 byte indicator section offset)
      KOFF=8
      KVARB=JCHAR(BUFF(KOFF+9))
      LTYPE=JCHAR(BUFF(KOFF+10))
      LEVEL=JCHAR(BUFF(KOFF+12))+JCHAR(BUFF(KOFF+11))*256

!     check if 2d variable present in selection table
      KL=1
      DO KV=1,NVAR(KL)
         VGRIB=VGRIB0(KV)
         VCHAR=VCHAR0(KV)
         CNVRT=CNVRT0(KV)
!        matches id and special level indicator
         IF(KVARB.EQ.VGRIB.AND.LEVEL.EQ.SIG0(KV).AND.                          &
            STYP(KV).EQ.LTYPE)GO TO 300
      END DO

!     then check for 3d variable
      KL=2
      DO KV=1,NVAR(KL)
         VGRIB=VGRIB1(KV)
         VCHAR=VCHAR1(KV)
         CNVRT=CNVRT1(KV)
         IF(KVARB.EQ.VGRIB.AND.LTYPE.EQ.100)GO TO 200
      END DO
      GO TO 800

!     check if 3d level is present in selection table
  200 DO KL=2,NLVL
         IF(LEVEL.EQ.SIGL(KL))GO TO 300
      END DO

!     if all tests fail go and read next grib record
      GO TO 800

!     load the entire grib data record into the buffer
  300 KREC=KREC+1
!->used by directIO library routines
      CALL FCPTPS(HANDLE,KBYTE,*900)
      CALL FCREAD(HANDLE,BUFF,1,KLEN,*900)

!     call the nmc grib unpacker
      CALL W3FI63(BUFF,KPDS,KGDS,KBMS,SVAR,KPTR,KRET)
      IF(KRET.NE.0)THEN
         WRITE(*,*)'Error W3FI63: ',KRET
         STOP
      END IF

!     new century fix
      KPDS(8)=MOD(KPDS(8),100)

!     set the current time
      IYR=KPDS(8)
      IMO=KPDS(9)
      IDA=KPDS(10)
      IHR=KPDS(11)
      IMN=KPDS(12)
      IFH=MAX(KPDS(14),KPDS(15))
      CALL TMPLUS(IYR,IMO,IDA,IHR,IFH)

!     for the the first record create an index record for pakrec
      if(KREC.EQ.1)THEN

!        decode grib information and create index record structure
         CALL MAKNDX(KGDS,VCHAR0,VCHAR1,MVAR,NLVL,NVAR,                        &
            SIGL,IG,NXP,NYP,CLAT,CLON,NLAT,NLON)

         IF(NLAT.GT.MAXLAT.OR.NLON.GT.MAXLON)THEN
            WRITE(*,*)'Input array size: ',NLAT,NLON
            WRITE(*,*)'Exceeds dimension:',MAXLAT,MAXLON
            STOP
         END IF

         IF(NXP.GT.MAXX.OR.NYP.GT.MAXY)THEN
            WRITE(*,*)'Real array size: ',NXP,NYP
            WRITE(*,*)'Exceedes dimensions: ',MAXX,MAXY
            STOP
         END IF

         NXY=NXP*NYP
         IF(NXY.GT.MAXC)THEN
            WRITE(*,*)'Packed output array: ',NXY
            WRITE(*,*)'Exceeds dimensions: ',MAXC
            STOP
         END IF
         CALL PAKSET(20,'CFG_NCR',1,NXP,NYP,NZP)

!        set up the grid to lat/lon equivalence array
         CALL MKGRID(NXP,NYP,TLAT,TLON)

!        write current output time to special file
         OPEN(40,FILE='NCRTIME')
         WRITE(40,'(5I2.2)')KPDS(8),KPDS(9),KPDS(10),KPDS(11),IFH
         CLOSE (40)

!        standard output name for packed data
         FNAME='DATA.NCR'

         INQUIRE(FILE=FNAME,EXIST=FTEST)
!        open data file
         LREC=NXY+50
         OPEN(20,FILE=FNAME,RECL=LREC,                                         &
              ACCESS='DIRECT',FORM='UNFORMATTED')

         IF(.NOT.FTEST)THEN
!           if file already exists data will be filled in as needed
!           open output data set and initialize to missing
            CALL DATINI(20,CVAR,NXY,NLVL,NVAR,IG,IYR,IMO,IDA,IHR)
            WRITE(*,*)'Initialized output data set: ',FNAME
         END IF
      END IF

!     remap input data from one- to two-dimensional array
      RVAR=RESHAPE(SVAR,SHAPE)

!     interpolate from lat/lon to conformal grid
      CALL REGRID(RVAR,NLON,NLAT,XVAR,NXP,NYP,TLAT,TLON,                       &
         CLAT1,CLON1,DLAT,DLON)
      IF(CNVRT.NE.1)CALL DATCNV(XVAR,NXP,NYP,CNVRT)

!     then pack into ARL format and continue
!     WRITE(*,'(1X,A,2X,6I5)')VCHAR,KREC,LTYPE,LEVEL,KV,KL,SIGL(KL)
      CALL PAKREC(20,XVAR,CVAR,NXP,NYP,NXY,VCHAR,                              &
          IYR,IMO,IDA,IHR,IMN,IFH,KL,0)

  800 KBYTE=KBYTE+KLEN
      GO TO 100
  900 CONTINUE

!     compute number of records per time period in output file
      NREC=1
      DO K=1,NLVL
         NREC=NREC+NVAR(K)
      END DO

!     rotate vector variables from true to grid orientation
      CALL ROTATE(NREC,CVAR,NXY,XVAR,YVAR,NXP,NYP,IMN,IFH)

      RETURN
      END


!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM:  MAKNDX           CREATE THE INDEX RECORD CONFIGURATION FILE
!   PRGMMR:    ROLAND DRAXLER   ORG: R/E/AR      DATE:96-06-01
!
! ABSTRACT:  THIS CODE WRITTEN AT THE AIR RESOURCES LABORATORY ...
!            CREATES THE CONFIGURATION FILE FOR THE OUTPUT DATA WHICH
!            DEFINES THE GRID SYSTEM AS WELL AS ALL VARIABLES THAT ARE
!            TO BE WRITTEN TO THE OUTPUT FILE.
!
! PROGRAM HISTORY LOG:
!   LAST REVISED: 03 Feb 1998 (RRD) - mercator fix, added lambert
!
! USAGE:  CALL MAKNDX(KGDS,VCHAR0,VCHAR1,MVAR,NLVL,NVAR,
!              SIGL,IG,NXP,NYP,CLAT,CLON,NLAT,NLON)
!   INPUT ARGUMENT LIST:
!     KGDS - grid definitions from w3lib decoder
!     VCHAR0 - character array of surface field identification
!     VCHAR1 - character array of upper level field identifications
!     MVAR - maximum dimension for number of variables
!     NLVL - number of data levels in output file
!     SIGL - height of each output level
!     IG - grid identification number
!     CLAT,CLON - center position of output grid
!   OUTPUT ARGUMENT LIST:
!     NXP,NYP - output grid dimensions
!     NLAT,NLON - grid dimensions of input data grid
!   INPUT FILES:
!     NONE
!   OUTPUT FILES:
!     UNIT 30 - CFG_NCR defines the output file grid and structure
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE:  CRAY
!
!$$$

      SUBROUTINE MAKNDX(KGDS,VCHAR0,VCHAR1,MVAR,NLVL,NVAR,                     &
         SIGL,IG,NXP,NYP,CLAT,CLON,NLAT,NLON)

      LOGICAL FTEST
      INTEGER KGDS(25)

!     arrays to hold variable selection information
      INTEGER   SIGL(NLVL), NVAR(NLVL)
      CHARACTER*4 VCHAR0(MVAR), VCHAR1(MVAR)

      COMMON / SETUP / GRIDS(12), PARMAP(9)

!     set the input grid dimensions
      NLON=KGDS(2)
      NLAT=KGDS(3)

!     set the output grid dimensions
      NXP=100
      NYP=100

!     if configuration exists exit
      INQUIRE(FILE='CFG_NCR',EXIST=FTEST)
!     if test used must delete cfg file before each execution
!     IF(FTEST)RETURN

!     grid orientation
      GRIDS(6)=0.0
!     delta=x grid size in km
      GRIDS(5)=100.0

!     synch point in x,y coordintes
      GRIDS(8)=(NXP+1.0)/2.0
      GRIDS(9)=(NYP+1.0)/2.0
!     synch point in lat/lon coordinates
      GRIDS(10)=CLAT
      GRIDS(11)=CLON

!     variable reserved for future use
      GRIDS(12)=0.0

!     defines a polar sterographic projection
      IF(ABS(CLAT).GT.60.0)THEN

!        set the pole position and reference lat/lon
         IF(CLAT.GT.0.0)THEN
            GRIDS(1)=90.0
         ELSE
            GRIDS(1)=-90.0
         END IF

!        pole longtitude (+180 from cut)
         GRIDS(2)=CLON

!        reference lat/lon (at which grid size specified)
         GRIDS(3)=CLAT
!        reference longitude and grid alignment
         GRIDS(4)=CLON

!        tangent latitude
         GRIDS(7)=GRIDS(1)

!     defines a mercator projection
      ELSEIF(ABS(CLAT).LT.30.0)THEN

!        pole lat/lon axis through pole
         GRIDS(1)=0.0
         GRIDS(2)=CLON

!        reference lat
         GRIDS(3)=CLAT
!        reference lon
         GRIDS(4)=CLON

!        tangent latitude
         GRIDS(7)=0.0

!     defines a lambert conformal projection
      ELSE

!        pole lat/lon axis through pole
         GRIDS(1)=CLAT
         GRIDS(2)=CLON

!        reference lat
         GRIDS(3)=CLAT
!        reference lon
         GRIDS(4)=CLON

!        tangent latitude
         GRIDS(7)=CLAT
      END IF

!     write the packer configuration file
      OPEN(30,FILE='CFG_NCR')
      WRITE(30,'(20X,A4)')'NCAR'

!     grid number 99 and 2 for pressure coordinate system
      WRITE(30,'(20X,I4)') IG, 2

      WRITE(30,'(20X,F10.2)')(GRIDS(I),I=1,12)
      WRITE(30,'(20X,I4)')NXP,NYP,NLVL

!     upper level information
      DO NL=1,NLVL
         SIGMA=SIGL(NL)
         IF(NL.EQ.1)THEN
            WRITE(30,'(20X,F6.0,I3,20(1X,A4))')                                &
            SIGMA,NVAR(NL),(VCHAR0(NV),NV=1,NVAR(NL))
         ELSE
            WRITE(30,'(20X,F6.0,I3,20(1X,A4))')                                &
            SIGMA,NVAR(NL),(VCHAR1(NV),NV=1,NVAR(NL))
         END IF
      END DO
      CLOSE (30)
      RETURN
      END


!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM:  DATINI           INITIALIZE OUTPUT FILE FOR ONE TIME PERIOD
!   PRGMMR:    ROLAND DRAXLER   ORG: R/E/AR      DATE:96-06-01
!
! ABSTRACT:  THIS CODE WRITTEN AT THE AIR RESOURCES LABORATORY ...
!            DUMMY ROUTINE TO FILL IN HEADER BYTES ON ALL RECORS OF THE
!            OUTPUT DATA FILE WITH CORRECT TIME BUT NULL DATA FIELDS.
!
! PROGRAM HISTORY LOG:
!   LAST REVISED: 18 Feb 1997 - RRD
!
! USAGE:  CALL DATINI(KUNIT,CVAR,NXY,NLVL,NVAR,IG,IY,IM,ID,IH)
!   INPUT ARGUMENT LIST:
!     KUNIT - unit number of the output file
!     CVAR - dummy character string to represent the gridded data
!     NXY - the length of the gridded data field in bytes
!     NLVL - number of data levels in output file
!     NVAR - array defining the number of variables on each level
!     IG - grid identification number
!     IY,IM,ID,IH - current date/time
!   OUTPUT ARGUMENT LIST:
!     NONE
!   INPUT FILES:
!     NONE
!   OUTPUT FILES:
!     UNIT 20 - DATA.NCR is the output data file
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE:  CRAY
!
!$$$

      SUBROUTINE DATINI(KUNIT,CVAR,NXY,NLVL,NVAR,IG,IY,IM,ID,IH)

      CHARACTER CVAR(NXY)*1, LABEL*50
      INTEGER   NVAR(NLVL)

      IC=-1
      NEXP=0
      PREC=0.0
      VAR1=0.0
      MREC=0

!     initialize packed data array
      DO K=1,NXY
         CVAR(K)=' '
      END DO
      CVAR(NXY)=CHAR(13)

!     header label output format
  100 FORMAT(7I2,A4,I4,2E14.7)

!     index record
      WRITE(LABEL,100)IY,IM,ID,IH,IC,0,IG,'NULL',NEXP,PREC,VAR1
      MREC=MREC+1
      WRITE(KUNIT,REC=MREC)LABEL,CVAR

      DO NL=1,NLVL
      DO NV=1,NVAR(NL)
         IL=NL-1
         WRITE(LABEL,100)IY,IM,ID,IH,IC,IL,IG,'NULL',NEXP,PREC,VAR1
         MREC=MREC+1
         WRITE(KUNIT,REC=MREC)LABEL,CVAR
      END DO
      END DO

      RETURN
      END


!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM:  DATCNV           CONVERT UNITS FOR ALL ELEMENTS OF THE DATA
!   PRGMMR:    ROLAND DRAXLER   ORG: R/E/AR      DATE:96-06-01
!
! ABSTRACT:  THIS CODE WRITTEN AT THE AIR RESOURCES LABORATORY ...
!            IS USED TO CONVERT EACH ELEMENT OF THE OUTPUT DATA GRID
!            TO CONFORM WITH UNIT CONVENTIONS USED IN OTHER APPLICATIONS
!
! PROGRAM HISTORY LOG:
!   LAST REVISED: 18 Feb 1997 (RRD)
!
! USAGE:  CALL DATCNV(RVAR,NXP,NYP,CNVRT)
!   INPUT ARGUMENT LIST:
!     RVAR - real data array
!     NXP,NYP - dimensions of the array
!     CNVRT - conversion factor
!   OUTPUT ARGUMENT LIST:
!     RVAR - real data array after conversion
!   INPUT FILES:
!     NONE
!   OUTPUT FILES:
!     NONE
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE:  CRAY
!
!$$$

      SUBROUTINE DATCNV(RVAR,NXP,NYP,CNVRT)

      REAL RVAR(NXP,NYP)

      DO J=1,NYP
      DO I=1,NXP
         RVAR(I,J)=RVAR(I,J)*CNVRT
      END DO
      END DO

      RETURN
      END



!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM:  MKGRID           DETERMINE THE LAT/LON OF EACH GRID POINT
!   PRGMMR:    ROLAND DRAXLER   ORG: R/E/AR      DATE:96-06-01
!
! ABSTRACT:  THIS CODE WRITTEN AT THE AIR RESOURCES LABORATORY ...
!            GOES THROUGH EACH NODE OF THE OUTPUT GRID AND PLACES THE
!            LAT/LON VALUE OF THAT POINT INTO AN ARRAY
!
! PROGRAM HISTORY LOG:
!   LAST REVISED: 03 Feb 1999 (RRD) - mercator definition patch
!
! USAGE:  CALL MKGRID(NX,NY,TLAT,TLON)
!   INPUT ARGUMENT LIST:
!     NX,NY - dimensions of output grid
!   OUTPUT ARGUMENT LIST:
!     TLAT,TLON - array of positions for each node
!   INPUT FILES:
!     NONE
!   OUTPUT FILES:
!     NONE
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE:  CRAY
!
!$$$

      SUBROUTINE MKGRID(NX,NY,TLAT,TLON)

      REAL TLAT(NX,NY),TLON(NX,NY)
      COMMON / SETUP / GRIDS(12), PARMAP(9)

!     define the tangent latitude and reference longitude
      CALL STLMBR(PARMAP,GRIDS(7),GRIDS(4))

!     define the grid by a one-point specification
      CALL STCM1P(PARMAP,GRIDS(8),GRIDS(9),GRIDS(10),GRIDS(11),                &
                         GRIDS(3),GRIDS(4),GRIDS(5),GRIDS(6))

!     determine the lat/lon at the grid locations
      DO I=1,NX
      DO J=1,NY
         CALL CXY2LL(PARMAP,FLOAT(I),FLOAT(J),TLAT(I,J),TLON(I,J))
!        cxy2ll returns -180:+180 while input data array from 0:360
         IF(TLON(I,J).LT.0.0)TLON(I,J)=360.0+TLON(I,J)
      END DO
      END DO

      RETURN
      END



!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM:  REGRID           INTERPOLATES DATA TO THE OUTPUT GRID
!   PRGMMR:    ROLAND DRAXLER   ORG: R/E/AR      DATE:96-06-01
!
! ABSTRACT:  THIS CODE WRITTEN AT THE AIR RESOURCES LABORATORY ...
!            FOR A GIVEN VARIABLE WILL GO THROUGH EACH NODE OF THE OUTPUT
!            GRID AND ITERPOLATE A VALUE TO THAT POINT FROM THE INPUT
!            DATA GRID.  ONLY LINEAR INTERPOLATION METHODS ARE USED.
!
! PROGRAM HISTORY LOG:
!   LAST REVISED: 03 Feb 1999 (RRD) - fixed interpolation around prime
!
! USAGE:  CALL REGRID(V1,NX1,NY1,V2,NX2,NY2,TLAT,TLON,
!                     CLAT1,CLON1,DLAT,DLON)
!   INPUT ARGUMENT LIST:
!     V1 - real array of input data on lat/lon grid
!     NX1,NY1 - dimensions of input data
!     NX2,NY2 - dimensions of output data
!     TLAT,TLON - array of positions for each node of output data
!     CLAT1,CLON1 - position of norwest corner (1,1) of input grid
!     DLAT,DLON - spacing between elements of input grid
!   OUTPUT ARGUMENT LIST:
!     V2 - real array of interpolated output data
!   INPUT FILES:
!     NONE
!   OUTPUT FILES:
!     NONE
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE:  CRAY
!
!$$$

      SUBROUTINE REGRID(V1,NX1,NY1,V2,NX2,NY2,TLAT,TLON,                       &
         CLAT1,CLON1,DLAT,DLON)

!     old data and         new data
      REAL   V1(NX1,NY1), V2(NX2,NY2)

!     grid conversion array
      REAL   TLAT(NX2,NY2), TLON(NX2,NY2)

!     interpolate values to new grid
      DO I=1,NX2
      DO J=1,NY2

!        compute adjacent index values on grid 1
       XP=1.0+(TLON(I,J)-CLON1)/DLON
       YP=1.0+(CLAT1-TLAT(I,J))/DLAT

!        compute base index
         ILO=INT(XP)
         JLO=INT(YP)

!        interpolation fractions from base point
         FXI=XP-ILO
         FYJ=YP-JLO

!        compute upper index point
         IHI=ILO+1
         JHI=JLO+1

!        check limits on upper point
!        note grid wraps around at the prime meridian
       IF(IHI.GT.NX1)IHI=1
       IF(JHI.GT.NY1)JHI=1

!        interpolate across at top and bottom
         TOP=(V1(IHI,JHI)-V1(ILO,JHI))*FXI+V1(ILO,JHI)
         BOT=(V1(IHI,JLO)-V1(ILO,JLO))*FXI+V1(ILO,JLO)

!        interpolate between top and bottom
         V2(I,J)=(TOP-BOT)*FYJ+BOT

      END DO
      END DO

      RETURN
      END



!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM:  U2GRID           CONVERT WINDS FROM TRUE TO GRID ORIENTATION
!   PRGMMR:    ROLAND DRAXLER   ORG: R/E/AR      DATE:96-06-01
!
! ABSTRACT:  THIS CODE WRITTEN AT THE AIR RESOURCES LABORATORY ...
!            CONVERTS THE COMPONENT WINDS FROM THE LAT/LON GRID RELATIVE
!            TO NORTH TO COMPONENTS RELATIVE TO THE ORIENTATION OF THE
!            OUTPUT GRID AT EACH NODE OF THE OUTPUT GRID
!
! PROGRAM HISTORY LOG:
!   LAST REVISED: 18 Feb 1997 - RRD
!
! USAGE:  CALL U2GRID(UU,VV,NX,NY)
!   INPUT ARGUMENT LIST:
!     UU,VV - true winds on output grid
!     NX,NY - dimensions of output grid
!   OUTPUT ARGUMENT LIST:
!     UU,VV - winds rotated to output grid
!   INPUT FILES:
!     NONE
!   OUTPUT FILES:
!     NONE
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE:  CRAY
!
!$$$

      SUBROUTINE U2GRID(UU,VV,NX,NY)

      REAL   UU(NX,NY), VV(NX,NY)
      COMMON/ SETUP / GRIDS(12), PARMAP(9)

!     convert compass winds to grid-orientation
      DO I=1,NX
      DO J=1,NY
         CALL CC2GXY(PARMAP,FLOAT(I),FLOAT(J),UU(I,J),VV(I,J),UG,VG)
         UU(I,J)=UG
         VV(I,J)=VG
      END DO
      END DO

      RETURN
      END



!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM:  ROTATE           CONVERT WINDS FROM TRUE TO GRID ORIENTATION
!   PRGMMR:    ROLAND DRAXLER   ORG: R/E/AR      DATE:96-06-01
!
! ABSTRACT:  THIS CODE WRITTEN AT THE AIR RESOURCES LABORATORY ...
!            AFTER THE OUTPUT FILE HAS BEEN WRITTEN IT IS NECESSARY TO
!            GO BACK AND CONVERT THE WINDS FROM COMPASS ORIENTATION TO
!            GRID ORIENTATION.  THIS WAS NOT POSSIBLE AS THE FILE WAS
!            INITIALLY WRITTEN BECAUSE THE U,V WIND COMPONENTS WERE NOT
!            IN SEQUENTIAL ORDER IN THE INPUT DATA FILE.  BOTH COMPONENTS
!            ARE REQUIRED SIMULTANEOUSLY TO DO THE ROTATION. THIS ROUTINE
!            READS THE OUTPUT FILE, ROTATES THE WINDS, AND WRITES THOSE
!            RECORDS BACK INTO THE FILE.
!
! PROGRAM HISTORY LOG:
!   LAST REVISED: 18 Feb 1997 - RRD
!
! USAGE:  CALL ROTATE(NREC,CVAR,NXY,XVAR,YVAR,NXP,NYP,IMN,IFH)
!   INPUT ARGUMENT LIST:
!     NREC - number of records per time period
!     CVAR - dummy character array to hold packed data field
!     NXY - length of CVAR array
!     XVAR - dummy real array to hold the first data field
!     YVAR - dummy real array to hold the second data field
!     NXP,NYP - dimensions of real arrays
!     IMN - current minutes
!     IFH - current forecast hour
!   OUTPUT ARGUMENT LIST:
!     NONE
!   INPUT FILES:
!     UNIT 20 - DATA.NCR direct access
!   OUTPUT FILES:
!     UNIT 20 - DATA.NCR direct access
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE:  CRAY
!
!$$$

      SUBROUTINE ROTATE(NREC,CVAR,NXY,XVAR,YVAR,NXP,NYP,IMN,IFH)

!     data array
      CHARACTER LABEL*50, CVAR(NXY)*1, VARB*4, VARBX*4, VARBY*4
      REAL XVAR(NXP,NYP), YVAR(NXP,NYP)

!---------------------------------------------------------------------
  INTERFACE
  SUBROUTINE PAKINP(RVAR,CVAR,NX,NY,NX1,NY1,LX,LY,PREC,NEXP,VAR1,KSUM)
  REAL,          INTENT(OUT)   :: rvar (:,:)     ! real data unpacked
  CHARACTER(1),  INTENT(IN)    :: cvar (:)       ! packed input of NX*NY
  INTEGER,       INTENT(IN)    :: nx,ny          ! size of input array  
  INTEGER,       INTENT(IN)    :: nx1,ny1        ! optional sub-grid left edge 
  INTEGER,       INTENT(IN)    :: lx,ly          ! length of sub-grid
  REAL,          INTENT(IN)    :: prec           ! precision of packed data 
  INTEGER,       INTENT(IN)    :: nexp           ! packing scaling exponent
  REAL,          INTENT(IN)    :: var1           ! value of array at (1,1)
  INTEGER,       INTENT(INOUT) :: ksum           ! rotating checksum 
  END SUBROUTINE pakinp
  END INTERFACE
!---------------------------------------------------------------------

  100 FORMAT(7I2,A4,I4,2E14.7)

      KREC=1
      DO WHILE (KREC.LT.NREC)
         READ(20,REC=KREC,ERR=900)LABEL
         READ(LABEL,'(14X,A4)')VARB

         IF(VARB(1:1).EQ.'U')THEN
!           first record of record pair gives u-component variable
            READ(20,REC=KREC)LABEL,CVAR
            READ(LABEL,100)IYR,IMO,IDA,IHR,IFH,KL,KG,VARBX,NEXP,               &
                           PREC,VAR1
            KSUM=-1
            CALL PAKINP(XVAR,CVAR,NXP,NYP,1,1,NXP,NYP,PREC,NEXP,VAR1,KSUM)

!           second record in pair gives v-component variable
            KREC=KREC+1
            READ(20,REC=KREC)LABEL,CVAR
            READ(LABEL,100)IYR,IMO,IDA,IHR,IFH,KL,KG,VARBY,NEXP,               &
                           PREC,VAR1
            KSUM=-1
            CALL PAKINP(YVAR,CVAR,NXP,NYP,1,1,NXP,NYP,PREC,NEXP,VAR1,KSUM)

            IF(VARBY(1:1).EQ.'V')THEN
!              both components available then rotate
               LEVEL=KL+1
               CALL U2GRID(XVAR,YVAR,NXP,NYP)

!              rewrite those records into the data file
               CALL PAKREC(20,XVAR,CVAR,NXP,NYP,NXY,VARBX,                     &
                    IYR,IMO,IDA,IHR,IMN,IFH,LEVEL,0)
               CALL PAKREC(20,YVAR,CVAR,NXP,NYP,NXY,VARBY,                     &
                    IYR,IMO,IDA,IHR,IMN,IFH,LEVEL,0)
!              WRITE(*,*)'Rotate: ',VARBX,VARBY,LEVEL
            ELSE
               WRITE(*,*)'Error ROTATE: record pair not U,V'
               STOP
            END IF
         END IF
         KREC=KREC+1

      END DO
      RETURN
  900 WRITE(*,*)'Error ROTATE: reading input data'
      STOP
      END
