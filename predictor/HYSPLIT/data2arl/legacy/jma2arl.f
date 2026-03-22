!$$$  MAIN PROGRAM DOCUMENTATION BLOCK
!
! MAIN PROGRAM: JMA2ARL      DECODE JMA MODEL FIELD FOR HYSPLIT
!   PRGMMR: DRAXLER          ORG: R/ARL       DATE: 2000-01-07
!
! ABSTRACT:  THIS CODE WRITTEN AT THE AIR RESOURCES LABORATORY ...
!     converts JMA real binary file to arl packed format
!     processes only one time period per execution. Input grib data file
!     should contain only data from one time period. Data are converted
!     from lat/lon to mercator map projection.
!
! PROGRAM HISTORY LOG:
!   LAST REVISED: 14 Jan 2000 (RRD)
!
! USAGE:  JMA2ARL [BINARY_FILE] [YYMMDDHH] [HH+??]
!   INPUT ARGUMENT LIST:
!     GRIB_FILE - name of input grib data
!   OUTPUT ARGUMENT LIST:
!     NONE
!   INPUT FILES:
!     grib input data files (no unit #, uses special directIO routines)
!   OUTPUT FILES:
!     unit 20 DATA.JMA - ARL packed data output file
!     unit 30 CFG_JMA  - characteristics of ouput grid
!     unit 40 JMATIME  - time indicator file
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE:  IBM
!
!$$$

      PROGRAM JMA2ARL

      CHARACTER LABEL*80

      NARG=IARGC()
      IF(NARG.NE.2)THEN
         WRITE(*,*)'Converts Japan Meteorological Agency binary file to arl format'
         WRITE(*,*)'Usage: jma2arl [yymmddhh] [hh+??]'
         STOP
      END IF

!     set initialization date and forecast time
      CALL GETARG(1,LABEL)
      READ(LABEL,'(4I2)')IY,IM,ID,IH
      CALL GETARG(2,LABEL)
      READ(LABEL,'(I2)')IFHR

!     convert initialization and forecast to current time
      IYR=IY
      IMO=IM
      IDA=ID
      IHR=IH
      CALL TMPLUS(IYR,IMO,IDA,IHR,IFHR)

!     main decoding routine (assume one time period per process)
      CALL XTRACT(IYR,IMO,IDA,IHR,IFHR)

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
!   LAST REVISED: 14 Feb 2000 (RRD)
!
! USAGE:  CALL XTRACT(IY,IM,ID,IH,IFHR)
!   INPUT ARGUMENT LIST:
!     IY,IM,ID,IH - forecast initialization time
!     IFHR - forecast hour
!   OUTPUT ARGUMENT LIST:
!     NONE
!   INPUT FILES:
!     NONE
!   OUTPUT FILES:
!     NONE
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE:  IBM
!
!$$$

      SUBROUTINE XTRACT(IYR,IMO,IDA,IHR,IFHR)

!     input array limits
      PARAMETER (MAXLON=113,MAXLAT=65)

!     number of levels, variables, and output buffer array limits
      PARAMETER (NLVL=12, MVAR=6, MAXX=250, MAXY=250)
      PARAMETER (MAXC=MAXX*MAXY)

!     internal and external variable identification
      CHARACTER*1 VGRIB0(MVAR), VGRIB1(MVAR), FCST(12), VARID
      CHARACTER*2 SIG0(MVAR),   SIG1(NLVL)

      INTEGER SIGL(NLVL), NVAR(NLVL)
      CHARACTER*4 VCHAR, VCHAR0(MVAR), VCHAR1(MVAR)
      REAL        CNVRT, CNVRT0(MVAR), CNVRT1(MVAR)

      LOGICAL FTEST

!     unpacked input and output array
      REAL RVAR(MAXLON,MAXLAT), XVAR(MAXX,MAXY)

!     special variable to hold dew point depression
      REAL DVAR(MAXLON,MAXLAT)

!     terrain heights (written instead of sfc pressure)
      REAL TVAR(MAXLON,MAXLAT)

!     lat/lon grid conversion equivalence table
      REAL TLAT(MAXX,MAXY),TLON(MAXX,MAXY)

!     packed output array
      CHARACTER CVAR(MAXC)*1, FNAME*80

!==>data structure tables

!     default information (grid id, record counter)
      DATA IG/99/, KREC/0/, IMIN/0/

!     predefine number of variables surface and aloft
      DATA NVAR / 6, 7*5, 4*4/

!     forecast time file name marker
      DATA FCST/'a','b','c','d','e','f','g','h','i','j','k','l'/

!     relate ARL variable with file name convention
      DATA VGRIB0/'#','p','e','u','v','t'/
      DATA VGRIB1/'h','u','v','t','r','#'/

!     variables level identification
      DATA SIG0/'  ','89','88','98','98','98'/
      DATA SIG1/'98','99','92','85','70','50',                                 &
                '40','30','25','20','15','10'/

!     set output structure in ARL character format
      DATA VCHAR0                                                              &
         /'SHGT','MSLP','TPPA','U10M','V10M','T02M'/
      DATA VCHAR1                                                              &
         /'HGTS','UWND','VWND','TEMP','RELH','    '/

!     set requested levels for input and output
      DATA SIGL/0,1000,925,850,700,500,400,300,250,200,150,100/

!     units conversion factors before converting to ARL format
      DATA CNVRT0/  1.0,  0.01, .001, 1.0, 1.0, 1.0/
      DATA CNVRT1/  1.0,   1.0,  1.0, 1.0, 1.0, 1.0/

!     input lat/lon number of grid points
      DATA NLAT,NLON/65,113/
!     south west corner point
      DATA CLAT1,CLON1/-20.0,60.0/
!     grid point increment
      DATA DLAT,DLON/1.25,1.25/

!     diagnostic variables from pakset
      COMMON / DIAG01 / NEXP, PREC, VAR1

!==>create index record structure and return NXP,NYP size

      CALL MAKNDX(VCHAR0,VCHAR1,MVAR,NLVL,NVAR,                                &
           SIGL,IG,NLAT,NLON,CLAT1,CLON1,DLAT,DLON,NXP,NYP)

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
      CALL PAKSET(20,'CFG_JMA',1,NXP,NYP,NZP)

!     set up the grid to lat/lon equivalence array
      CALL MKGRID(NXP,NYP,TLAT,TLON)

!     initialize terrain height array
      CALL TERRAIN(TVAR,NLON,NLAT,CLAT1,CLON1,DLAT,DLON)

!     write current output time to special file
      OPEN(40,FILE='JMATIME')
      WRITE(40,'(5I2.2)')IYR,IMO,IDA,IHR,IFHR
      CLOSE (40)

!     forecast index
      KFI=1+IFHR/6
      IF(KFI.LT.1.OR.KFI.GT.12)THEN
         WRITE(*,*)'Forecast time index exceed max: ',KFI
         STOP
      END IF

!==>open output file for ARL packed data

!     standard output name for packed data
      FNAME='DATA.JMA'

      INQUIRE(FILE=FNAME,EXIST=FTEST)
!     open data file
      LREC=NXY+50
      OPEN(20,FILE=FNAME,RECL=LREC,ACCESS='DIRECT',FORM='UNFORMATTED')

      IF(.NOT.FTEST)THEN
!        if file already exists data will be filled in as needed
!        open output data set and initialize to missing
         CALL DATINI(20,CVAR,NXY,NLVL,NVAR,IG,IYR,IMO,IDA,IHR)
         WRITE(*,*)'Initialized output data set: ',FNAME
      END IF

!==>loop through data starting at the surface

      DO NL=1,NLVL
      DO NV=1,NVAR(NL)

!        set input file name
         SIGMA=SIGL(NL)

         IF(NL.EQ.1)THEN
            VCHAR=VCHAR0(NV)
            CNVRT=CNVRT0(NV)
            VARID=VGRIB0(NV)
            FNAME='h'//VARID//'c'//FCST(KFI)//SIG0(NV)//'d'
         ELSE
            VCHAR=VCHAR1(NV)
            CNVRT=CNVRT1(NV)
            VARID=VGRIB1(NV)
            FNAME='h'//VARID//'c'//FCST(KFI)//SIG1(NL)//'d'
         END IF

!        check for proper files
         IF(VARID.NE.'#')THEN

!           check status and read
            INQUIRE(FILE=FNAME,EXIST=FTEST)
            IF(FTEST)THEN
               WRITE(*,*)'Opening file: ',FNAME(1:10), VCHAR,SIGMA
               OPEN(10,FILE=FNAME,FORM='UNFORMATTED')
               IF(VARID.EQ.'r')THEN
!                 special holder for dewpoint (assume RVAR has temp)
                  READ(10)((DVAR(I,J),I=1,NLON),J=NLAT,1,-1)
!                 convert to RH and put into RVAR
                  CALL DEW2RH(RVAR,DVAR,NLON,NLAT)
               ELSE
                  READ(10)((RVAR(I,J),I=1,NLON),J=NLAT,1,-1)
               END IF
               CLOSE(10)
            ELSE
               WRITE(*,*)'File not found: ',FNAME(1:10), VCHAR,SIGMA
!              dummy statements to create files for testing
!              OPEN(10,FILE=FNAME,FORM='UNFORMATTED')
!              WRITE(10)TVAR
!              CLOSE(10)
               STOP
            END IF

!           interpolate from lat/lon to conformal grid
            CALL REGRID(RVAR,NLON,NLAT,XVAR,NXP,NYP,TLAT,TLON,                 &
                 CLAT1,CLON1,DLAT,DLON)
            IF(CNVRT.NE.1.0)CALL DATCNV(XVAR,NXP,NYP,CNVRT)

!           then pack into ARL format and continue
            CALL PAKREC(20,XVAR,CVAR,NXP,NYP,NXY,VCHAR,                        &
                 IYR,IMO,IDA,IHR,IMIN,IFHR,NL,0)

            IF(VCHAR.EQ.'MSLP')THEN
!              when mslp appears write out additional surface terrain
               CALL REGRID(TVAR,NLON,NLAT,XVAR,NXP,NYP,TLAT,TLON,              &
                    CLAT1,CLON1,DLAT,DLON)
               CALL PAKREC(20,XVAR,CVAR,NXP,NYP,NXY,'SHGT',                    &
                    IYR,IMO,IDA,IHR,IMIN,IFHR,NL,0)
            END IF

!        proper file test
         END IF

!     variables and levels loop
      END DO
      END DO

!     close out time period and write index record
      CALL PAKNDX(20)
      CLOSE (20)

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
!   LAST REVISED: 14 Feb 2000 (RRD)
!
! USAGE:  CALL MAKNDX(VCHAR0,VCHAR1,MVAR,NLVL,NVAR,
!              SIGL,IG,NLAT,NLON,CLAT1,CLON1,DLAT,DLON,NXP,NYP)
!   INPUT ARGUMENT LIST:
!     VCHAR0 - character array of surface field identification
!     VCHAR1 - character array of upper level field identifications
!     MVAR - maximum dimension for number of variables
!     NLVL - number of data levels in output file
!     SIGL - height of each output level
!     IG - grid identification number
!     NLAT,NLON - grid dimensions of input data grid
!     CLAT1,CLON1 - position of northwest corner (1,1) of input grid
!     DLAT,DLON - spacing between elements of input grid
!   OUTPUT ARGUMENT LIST:
!     NXP,NYP - output grid dimensions
!   INPUT FILES:
!     NONE
!   OUTPUT FILES:
!     UNIT 30 - CFG_JMA defines the output file grid and structure
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE:  IBM
!
!$$$

      SUBROUTINE MAKNDX(VCHAR0,VCHAR1,MVAR,NLVL,NVAR,                          &
         SIGL,IG,NLAT,NLON,CLAT1,CLON1,DLAT,DLON,NXP,NYP)

      LOGICAL PRIME

!     arrays to hold variable selection information
      INTEGER   SIGL(NLVL), NVAR(NLVL)
      CHARACTER*4 VCHAR0(MVAR), VCHAR1(MVAR)

      COMMON / SETUP / GRIDS(12), PARMAP(9), PRIME

!     last (ne) grid point lat/lon
      CLAT2=CLAT1+(NLAT-1)*DLAT
      CLON2=CLON1+(NLON-1)*DLON

!=>set the internal lat/lon grid system

      PRIME=.FALSE.
!     does the grid straddle the prime meridian
      IF(CLON1.LT.0.0.AND.CLON2.GT.0.0)PRIME=.TRUE.
      IF(CLON1.GT.180.0.AND.CLON2.LT.180.0)PRIME=.TRUE.
!     if over prime use -180 to +180 else use 0 to 360
      IF(.NOT.PRIME.AND.CLON1.LT.0.0)CLON1=360.0+CLON1
      IF(.NOT.PRIME.AND.CLON2.LT.0.0)CLON2=360.0+CLON2

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
      OPEN(30,FILE='CFG_JMA')
      WRITE(30,'(20X,A4)')'JMA '

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


      SUBROUTINE DEW2RH(RVAR,DVAR,NLON,NLAT)
      REAL RVAR(NLON,NLAT), DVAR(NLON,NLAT)
!     covert temp and depression to RH
      DO J=1,NLAT
      DO I=1,NLON
         TEMP=RVAR(I,J)
         DEWP=TEMP-DVAR(I,J)
         EDEW=EXP(21.4-(5351.0/DEWP))
         ESAT=EXP(21.4-(5351.0/TEMP))
!        put RH back into temperature variable
         RVAR(I,J)=100.0*EDEW/ESAT
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

!     convert to data grid - pick nearest location
      DO J=1,NLAT
         PLAT=(J-1)*DLAT+CLAT1
!        real index on global grid
         YY=1+PLAT+90
         JJ=INT(YY)
         FY=YY-JJ

         DO I=1,NLON
            PLON=(I-1)*DLON+CLON1
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
