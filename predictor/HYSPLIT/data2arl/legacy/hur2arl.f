!$$$  MAIN PROGRAM DOCUMENTATION BLOCK
!
! MAIN PROGRAM: HUR2ARL      DECODE HURRICANE MODEL FIELD FOR HYSPLIT
!   PRGMMR: DRAXLER          ORG: R/ARL       DATE: 2000-01-07
!
! ABSTRACT:  THIS CODE WRITTEN AT THE AIR RESOURCES LABORATORY ...
!     converts an hurrican model grib file to arl packed format
!     processes only one time period per execution. Input grib data file
!     should contain only data from one time period. Data are converted
!     from lat/lon to mercator map projection.
!
! PROGRAM HISTORY LOG:
!   LAST REVISED: 07 Jan 2000 (RRD)
!                 09 Mar 2000 (RRD) - generic IO subroutines
!                 07 Mar 2001 (RRD) - grib id test
!                 05 Mar 2002 (RRD) - improved w3lib consistency
!                 24 Feb 2010 (RRD) - pressure level test
!
! USAGE:  AVN2ARL [GRIB_FILE]
!   INPUT ARGUMENT LIST:
!     GRIB_FILE - name of input grib data
!   OUTPUT ARGUMENT LIST:
!     NONE
!   INPUT FILES:
!     unit 10 - grib input data files
!   OUTPUT FILES:
!     unit 20 DATA.HUR - ARL packed data output file
!     unit 30 CFG_HUR  - characteristics of ouput grid
!     unit 40 HURTIME  - time indicator file
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE:  IBM
!
!$$$

      PROGRAM HUR2ARL

      LOGICAL   FTEST
      CHARACTER FNAME*80
      INTEGER*4 HANDLE, FCOPEN

!=>required for NCEP operational implementation
!     CALL W3TAGB('HUR2ARL ',1998,0238,0067,'R/ARL')
 
!=>check for command line arguments
      NARG=IARGC()

      IF(NARG.NE.1)THEN
         WRITE(*,*)'Converts an hurrican model grib file to arl format'
         WRITE(*,*)'Usage: hur2arl [file]'
         STOP
      END IF

!     open input data file
      CALL GETARG(1,FNAME)
      INQUIRE(FILE=FNAME,EXIST=FTEST)
      IF(FTEST)THEN
!        faster direct IO routine
         HANDLE=FCOPEN(FNAME,'r')

!        main decoding routine (assume one time period per process)
         WRITE(*,*)'started processing: ',FNAME
         CALL XTRACT(HANDLE)

!        faster direct IO routine
         CALL FCCLOS(HANDLE,*900)
  900    CONTINUE
      ELSE
         WRITE(*,*)'File not found:',FNAME
      END IF

!     close out time period and write index record
      CALL PAKNDX(20)
      CLOSE (20)

!=>required for NCEP operational implementation
!     CALL W3TAGE('HUR2ARL ')

      END


!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM:  EXTRACT          MAIN ROUTINE FOR DECODING AVN GRIB DATA
!   PRGMMR:    ROLAND DRAXLER   ORG: R/ARL       DATE:2000-01-07
!
! ABSTRACT:  THIS CODE WRITTEN AT THE AIR RESOURCES LABORATORY ...
!            EXTRACTS ONE RECORD FROM INPUT GRIB FILE AND ADDS ONE
!            RECORD TO THE OUTPUT FILE CENTERED ABOUT INDICATED LAT/LON
!
! PROGRAM HISTORY LOG:
!   LAST REVISED: 07 Jan 2000 (RRD)
!                 25 Feb 2003 (RRD) - ichar function replacement
!
! USAGE:  CALL XTRACT(HANDLE)
!   INPUT ARGUMENT LIST:
!     HANDLE - defines input file to directIO routines
!   OUTPUT ARGUMENT LIST:
!     NONE
!   INPUT FILES:
!     HANDLE defines input data file
!   OUTPUT FILES:
!     NONE
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE:  IBM
!
!$$$

      SUBROUTINE XTRACT(HANDLE)

!     input buffer array limit
      PARAMETER (MAXLON=360,MAXLAT=180,MAXB=200000)

!     number of levels, variables, and output buffer array limits
      PARAMETER (NLVL=20, MVAR=5, MAXX=250, MAXY=250)
      PARAMETER (MAXC=MAXX*MAXY)

!     arrays to hold grib, character, and level variable information
      INTEGER HANDLE, VGRIB, VGRIB0(MVAR), VGRIB1(MVAR),                       &
              STYP(MVAR), SIG0(MVAR), SIGL(NLVL), NVAR(NLVL)
      CHARACTER*4 VCHAR, VCHAR0(MVAR), VCHAR1(MVAR)
      REAL CNVRT, CNVRT0(MVAR), CNVRT1(MVAR)

!     define arrays for product definition section, grid description,
!     and other grib information
      INTEGER KPDS(25),KGDS(25),KPTR(25)

!     input data buffer and bit map section
      CHARACTER BUFF(MAXB)*1
      LOGICAL*1 KBMS(MAXB)
      LOGICAL   FTEST

!     unpacked input and output array
      REAL RVAR(MAXLON,MAXLAT), XVAR(MAXX,MAXY)

!     terrain heights (written instead of sfc pressure)
      REAL TVAR(MAXLON,MAXLAT)

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
      DATA NVAR / 5, 19*5/

!     set output structure by defining GRIB variables (sfc + levels 1->n)
      DATA VGRIB0/  0,  2, 61, 33, 34/
      DATA VGRIB1/  7, 11, 33, 34, 52/

!     special surface variables level identification
!     note winds at 35m will be written to the 10m level identification
      DATA SIG0/  0,   0,   0,  35,  35/
      DATA STYP/  0, 102, 102, 105, 105/

!     set output structure in ARL character format
      DATA VCHAR0                                                              &
         /'SHGT','MSLP','TPP6','U10M','V10M'/
      DATA VCHAR1                                                              &
         /'HGTS','TEMP','UWND','VWND','RELH'/

!     set requested sigma levels for input and output
      DATA SIGL/0,1000,950,900,850,800,750,700,650,600,550,500,                &
                   450,400,350,300,250,200,150,100/

!     units conversion factors before converting to ARL format
      DATA CNVRT0/  1.0,  0.01, .001, 1.0, 1.0/
      DATA CNVRT1/  1.0,   1.0,  1.0, 1.0, 100.0/

!     diagnostic variables from pakset
      COMMON / DIAG01 / NEXP, PREC, VAR1

      SAVE KREC

!-------------------------------------------------------------------------------
! only required when dealing with SUN F90 compiler
! replace ICHAR below with internally defined JCHAR function
  CHARACTER(1)                 :: mychr    
  INTEGER                      :: jchar
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

!-------------------------------------------------------------------

!     input grib record byte counter
      KBYTE=0

!=>read the grib indicator section

  100 CONTINUE
!     direct IO subroutines
      CALL FCPTPS(HANDLE,KBYTE,*900)
      CALL FCREAD(HANDLE,BUFF,1,8,*900)
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

!=>load the grib pds segment into the buffer

!     direct IO subroutines
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

!=>load the entire grib data record into the buffer

  300 KREC=KREC+1
!     direct IO subroutines
      CALL FCPTPS(HANDLE,KBYTE,*900)
      CALL FCREAD(HANDLE,BUFF,1,KLEN,*900)

!     call the nmc grib unpacker
      CALL W3FI63(BUFF,KPDS,KGDS,KBMS,SVAR,KPTR,KRET)
!     remap input data from one- to two-dimensional array
      RVAR=RESHAPE(SVAR,SHAPE)
      IF(KRET.NE.0)THEN
         WRITE(*,*)'Error W3FI63: ',KRET
         STOP
      END IF

!     century fix
      KPDS(8)=MOD(KPDS(8),100)

!     set the current time
      IYR=KPDS(8)
      IMO=KPDS(9)
      IDA=KPDS(10)
      IHR=KPDS(11)
      IMN=KPDS(12)
      IFH=MAX(KPDS(14),KPDS(15))
      CALL TMPLUS(IYR,IMO,IDA,IHR,IFH)

!=>after decoding the first grib record create the output grid and
!  initialize the pakrec routines

      IF(KREC.EQ.1)THEN

!        decode grib information and create index record structure
         CALL MAKNDX(KGDS,VCHAR0,VCHAR1,MVAR,NLVL,NVAR,                        &
            SIGL,IG,NXP,NYP,NLAT,NLON,CLAT1,CLON1,DLAT,DLON)

         IF(NLAT.GT.MAXLAT.OR.NLON.GT.MAXLON)THEN
            WRITE(*,*)'Input array  size: ',NLAT,NLON
            WRITE(*,*)'Exceeds dimension:',MAXLAT,MAXLON
            STOP
         END IF

         IF(NXP.GT.MAXX.OR.NYP.GT.MAXY)THEN
            WRITE(*,*)'Output array size: ',NXP,NYP
            WRITE(*,*)'Exceeds dimension: ',MAXX,MAXY
            STOP
         END IF

         NXY=NXP*NYP
         IF(NXY.GT.MAXC)THEN
            WRITE(*,*)'Packed output array: ',NXY
            WRITE(*,*)'Exceeds  dimensions: ',MAXC
            STOP
         END IF
         FNAME='CFG_HUR'
         CALL PAKSET(20,FNAME,1,NXP,NYP,NZP)

!        set up the grid to lat/lon equivalence array
         CALL MKGRID(NXP,NYP,TLAT,TLON)

!        initialize terrain height array
         CALL TERRAIN(TVAR,NLON,NLAT,CLAT1,CLON1,DLAT,DLON)

!        write current output time to special file
         OPEN(40,FILE='HURTIME')
         WRITE(40,'(5I2.2)')KPDS(8),KPDS(9),KPDS(10),KPDS(11),IFH
         CLOSE (40)

!        standard output name for packed data
         FNAME='DATA.HUR'

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

!=>interpolate data field from lat/lon to conformal grid and write

!     interpolate from lat/lon to conformal grid
      CALL REGRID(RVAR,NLON,NLAT,XVAR,NXP,NYP,TLAT,TLON,                       &
         CLAT1,CLON1,DLAT,DLON)
      IF(CNVRT.NE.1.0)CALL DATCNV(XVAR,NXP,NYP,CNVRT)

!     then pack into ARL format and continue
      CALL PAKREC(20,XVAR,CVAR,NXP,NYP,NXY,VCHAR,                              &
          IYR,IMO,IDA,IHR,IMN,IFH,KL,0)

!     optional diagnostics
!     WRITE(*,*)'Variable: ',VCHAR,LTYPE,KV
!     WRITE(*,*)'Record  : ',KREC
!     WRITE(*,*)'Level   : ',KL,LEVEL,SIGL(KL)
!     WRITE(*,*)'Packing : ',NEXP,PREC,VAR1

!=>special terrain height output because data set lacking surface pressure

      IF(VCHAR.EQ.'MSLP')THEN
!        when mslp appears write out additional surface terrain height field
         CALL REGRID(TVAR,NLON,NLAT,XVAR,NXP,NYP,TLAT,TLON,                    &
              CLAT1,CLON1,DLAT,DLON)
         CALL PAKREC(20,XVAR,CVAR,NXP,NYP,NXY,'SHGT',                          &
              IYR,IMO,IDA,IHR,IMN,IFH,KL,0)
      END IF

  800 KBYTE=KBYTE+KLEN
      GO TO 100

  900 CONTINUE
      IF(KREC.EQ.0)THEN
         WRITE(*,*)'ERROR: reading data file'
         STOP
      END IF

!     compute number of records per time period in output file
      NREC=1
      DO K=1,NLVL
         NREC=NREC+NVAR(K)
      END DO

      RETURN
      END


!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM:  MAKNDX           CREATE THE INDEX RECORD CONFIGURATION FILE
!   PRGMMR:    ROLAND DRAXLER   ORG: R/ARL       DATE:2000-01-07
!
! ABSTRACT:  THIS CODE WRITTEN AT THE AIR RESOURCES LABORATORY ...
!            CREATES THE CONFIGURATION FILE FOR THE OUTPUT DATA WHICH
!            DEFINES THE GRID SYSTEM AS WELL AS ALL VARIABLES THAT ARE
!            TO BE WRITTEN TO THE OUTPUT FILE.
!
! PROGRAM HISTORY LOG:
!   LAST REVISED: 07 Jan 2000 (RRD)
!
! USAGE:  CALL MAKNDX(KGDS,VCHAR0,VCHAR1,MVAR,NLVL,NVAR,
!              SIGL,IG,NXP,NYP,NLAT,NLON,CLAT1,CLON1,DLAT,DLON)
!   INPUT ARGUMENT LIST:
!     KGDS - grid definitions from w3lib decoder
!     VCHAR0 - character array of surface field identification
!     VCHAR1 - character array of upper level field identifications
!     MVAR - maximum dimension for number of variables
!     NLVL - number of data levels in output file
!     SIGL - height of each output level
!     IG - grid identification number
!     CLAT1,CLON1 - position of northwest corner (1,1) of input grid
!     DLAT,DLON - spacing between elements of input grid
!   OUTPUT ARGUMENT LIST:
!     NXP,NYP - output grid dimensions
!     NLAT,NLON - grid dimensions of input data grid
!   INPUT FILES:
!     NONE
!   OUTPUT FILES:
!     UNIT 30 - CFG_HUR defines the output file grid and structure
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE:  IBM
!
!$$$

      SUBROUTINE MAKNDX(KGDS,VCHAR0,VCHAR1,MVAR,NLVL,NVAR,                     &
         SIGL,IG,NXP,NYP,NLAT,NLON,CLAT1,CLON1,DLAT,DLON)

      LOGICAL PRIME
      INTEGER KGDS(25)

!     arrays to hold variable selection information
      INTEGER   SIGL(NLVL), NVAR(NLVL)
      CHARACTER*4 VCHAR0(MVAR), VCHAR1(MVAR)

      COMMON / SETUP / GRIDS(12), PARMAP(9), PRIME

!=>input grib file characteristics

!     grid dimensions
      NLON=KGDS(2)
      NLAT=KGDS(3)
!     first (sw) grid point lat/lon
      CLAT1=KGDS(4)/1000.0
      CLON1=KGDS(5)/1000.0
!     last (ne) grid point lat/lon
      CLAT2=KGDS(7)/1000.0
      CLON2=KGDS(8)/1000.0
!     direction increment
      DLON= KGDS(9)/1000.0
      DLAT=KGDS(10)/1000.0
!     write(*,*)'Input grid: ',clat1,clon1,clat2,clon2

!=>set the internal lat/lon grid system

      PRIME=.FALSE.
!     does the grid straddle the prime meridian
      IF(CLON1.LT.0.0.AND.CLON2.GT.0.0)PRIME=.TRUE.
      IF(CLON1.GT.180.0.AND.CLON2.LT.180.0)PRIME=.TRUE.
!     if over prime use -180 to +180 else use 0 to 360
      IF(.NOT.PRIME.AND.CLON1.LT.0.0)CLON1=360.0+CLON1
      IF(.NOT.PRIME.AND.CLON2.LT.0.0)CLON2=360.0+CLON2
!     write(*,*)'Input grid: ',clat1,clon1,clat2,clon2

!=>temporary projection to optimize grid

!     estimate grid size (111 km/deg)
      GSIZE=0.75*DLAT*111.0
!     reference latitude/longitude
      RLAT=0.5*(CLAT1+CLAT2)
      RLON=0.5*(CLON1+CLON2)
      IF(RLON.GT. 180.0)RLON=RLON-360.0
      IF(RLON.LT.-180.0)RLON=RLON+360.0

!     define the tangent latitude and reference longitude
      CALL STLMBR(PARMAP,0.0,RLON)
!     define the grid by a one-point specification, shift by
!     one grid point to insure conformal grid within data region
      CALL STCM1P(PARMAP,1.,1.,CLAT1,CLON1,RLAT,RLON,GSIZE,0.0)
!     find the other corner (ne) point
      CALL CLL2XY(PARMAP,CLAT2,CLON2,XP,YP)

!=>set the output grid characteristics in the index array

!     number of grid points
      NXP=INT(XP)
      NYP=INT(YP)

!     grid orientation
      GRIDS(6)=0.0
!     delta=x grid size in km
      GRIDS(5)=0.90*GSIZE

!     synch point at grid center in x,y coordintes
      GRIDS(8)=NINT((NXP+1.0)/2.0)
      GRIDS(9)=NINT((NYP+1.0)/2.0)
!     convert synch point to lat/lon coordinates
      CALL CXY2LL(PARMAP,GRIDS(8),GRIDS(9),CLAT,CLON)
      CLAT=NINT(CLAT*10.0)/10.0
      CLON=NINT(CLON*10.0)/10.0
!     write(*,*)'Grid center: ',clat,clon
      GRIDS(10)=CLAT
      GRIDS(11)=CLON

!     variable reserved for future use
      GRIDS(12)=0.0

!=>define a mercator projection

!     pole lat/lon axis through pole
      GRIDS(1)=0.0
      GRIDS(2)=CLON

!     reference lat
      GRIDS(3)=CLAT
!     reference lon
      GRIDS(4)=CLON

!     tangent latitude
      GRIDS(7)=0.0

!     write the packer configuration file
      OPEN(30,FILE='CFG_HUR')
      WRITE(30,'(20X,A4)')'GFDL'

!     dummy grid identification number 99
!     pressure coordinate identification: 2
      WRITE(30,'(20X,I4)') IG, 2

!     grid characteristics array
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
!     UNIT 20 - DATA.AVN is the output data file
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

      LOGICAL PRIME
      REAL TLAT(NX,NY),TLON(NX,NY)
      COMMON / SETUP / GRIDS(12), PARMAP(9), PRIME

!     define the tangent latitude and reference longitude
      CALL STLMBR(PARMAP,GRIDS(7),GRIDS(4))

!     define the grid by a one-point specification
      CALL STCM1P(PARMAP,GRIDS(8),GRIDS(9),GRIDS(10),GRIDS(11),                &
                         GRIDS(3),GRIDS(4),GRIDS(5),GRIDS(6))

!     determine the lat/lon at the grid locations
      DO I=1,NX
      DO J=1,NY
         CALL CXY2LL(PARMAP,FLOAT(I),FLOAT(J),TLAT(I,J),TLON(I,J))
!        cxy2ll returns -180:+180 therefore if the input grid straddles
!        the international dateline then convert negative longitudes to a
!        positive value to obtain the correct interpolated value
         IF(.NOT.PRIME.AND.TLON(I,J).LT.0.0)TLON(I,J)=360.0+TLON(I,J)
      END DO
      END DO
!     write(*,*)'Internal: ',tlat(1,1),tlon(1,1),tlat(nx,ny),tlon(nx,ny)

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
!                 10 Jan 2000 (RRD) - grid orientation 1,1 at sw corner
!
! USAGE:  CALL REGRID(V1,NX1,NY1,V2,NX2,NY2,TLAT,TLON,
!                     CLAT1,CLON1,DLAT,DLON)
!   INPUT ARGUMENT LIST:
!     V1 - real array of input data on lat/lon grid
!     NX1,NY1 - dimensions of input data
!     NX2,NY2 - dimensions of output data
!     TLAT,TLON - array of positions for each node of output data
!     CLAT1,CLON1 - position of (1,1) corner of input grid
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

!     old data and new data
      REAL V1(NX1,NY1), V2(NX2,NY2)

!     grid conversion array
      REAL TLAT(NX2,NY2), TLON(NX2,NY2)

!     interpolate values to new grid
      DO I=1,NX2
      DO J=1,NY2

!        index computed based on lat/lon of lower left corner
!        dateline and prime meridian adjusted in MKGRID where values
!        of the TLAT and TLON array have been set
       XP=1.0+(TLON(I,J)-CLON1)/DLON
         YP=1.0+(TLAT(I,J)-CLAT1)/DLAT

!        compute base index
         ILO=INT(XP)
         JLO=INT(YP)

!        interpolation fractions from base point
         FXI=XP-ILO
         FYJ=YP-JLO

!        compute upper index point
         IHI=ILO+1
         JHI=JLO+1

!        check limits
!        IF(ILO.LT.  1)ILO=1
!        IF(JLO.LT.  1)JLO=1
!        IF(IHI.GT.NX1)IHI=NX1
!        IF(JHI.GT.NY1)JHI=NY1

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
! SUBPROGRAM:  TERRAIN          EXTRACTS TERRAIN HEIGHT FROM LAT/LON FILE
!   PRGMMR:    ROLAND DRAXLER   ORG: R/ARL       DATE:2000-01-10
!
! ABSTRACT:  THIS CODE WRITTEN AT THE AIR RESOURCES LABORATORY ...
!            EXTRACTS ONE RECORD FROM INPUT GRIB FILE AND ADDS ONE
!            RECORD TO THE OUTPUT FILE CENTERED ABOUT INDICATED LAT/LON
!
! PROGRAM HISTORY LOG:
!   LAST REVISED: 10 Jan 2000 (RRD)
!
! USAGE:  CALL TERRAIN(TVAR,NLON,NLAT,CLAT1,CLON1,DLAT,DLON)
!   INPUT ARGUMENT LIST:
!     NLON,NLAT - dimensions of input data
!     CLAT1,CLON1 - position of 1,1 corner of input grid
!     DLAT,DLON - spacing between elements of input grid
!   OUTPUT ARGUMENT LIST:
!     TVAR - meters above msl for input data grid
!   OUTPUT ARGUMENT LIST:
!     NONE
!   INPUT FILES:
!     TERRAIN.ASC
!   OUTPUT FILES:
!     NONE
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE:  IBM
!
!$$$

      SUBROUTINE TERRAIN(TVAR,NLON,NLAT,CLAT1,CLON1,DLAT,DLON)

      LOGICAL FTEST
      REAL TVAR(NLON,NLAT)
      INTEGER HEIGHTS(180,360)

      INQUIRE(FILE='TERRAIN.ASC',EXIST=FTEST)
      IF(FTEST)THEN
         OPEN(50,FILE='TERRAIN.ASC')
      ELSE
         WRITE(*,*)'ERROR: Terrain.asc file required in local directory'
         STOP
      END IF

!     start from nw corner
      DO I=180,1,-1
         READ(50,'(2(180I4))')(HEIGHTS(I,J),J=1,360)
      END DO

!#####################################################
!check logic to insure terrain file nw corner 90N 180W
!#####################################################

!     convert to data grid - pick nearest location
      DO J=1,NLAT
!        PLAT=(J-1)*DLAT+CLAT1
         PLAT=(J-1)*DLAT-90.0
!        real index on global grid
         YY=1+PLAT+90
         JJ=INT(YY)
         FY=YY-JJ

         DO I=1,NLON
!           PLON=(I-1)*DLON+CLON1
            PLON=(I-1)*DLON-180.0
            IF(PLON.LT.0.0)PLON=360.0+PLON
!           real index on global grid
            XX=1+PLON
            II=INT(XX)
            FX=XX-II

!           interpolate to position
            TOP=(HEIGHTS(JJ+1,II+1)-HEIGHTS(JJ+1,II))*FX+                      &
                 HEIGHTS(JJ+1,II)
            BOT=(HEIGHTS(JJ  ,II+1)-HEIGHTS(JJ  ,II))*FX+                      &
                 HEIGHTS(JJ  ,II)
            TVAR(I,J)=(TOP-BOT)*FY+BOT
         END DO
      END DO

      CLOSE(50)
      RETURN
      END
