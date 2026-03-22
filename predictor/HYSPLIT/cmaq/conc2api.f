!$$$  PROGRAM DOCUMENTATION BLOCK
!
! PROGRAM:     CONC2API         READS HYSPLIT GRIDDED CONCENTRATION FILE
!                               AND CONVERT TP CMAQ I/O API FORMAT 
!   PRGMMR:    ROLAND DRAXLER   ORG: R/ARL       DATE:05-11-16
!
! ABSTRACT:  THIS CODE WRITTEN AT THE AIR RESOURCES LABORATORY ...
!   PROGRAM TO READ THE GRIDDED HYSPLIT CONCENTRATION FILE AND
!   CONVERT TO THE CMAQ I/O API FORMAT
!
! PROGRAM HISTORY LOG:
!   LAST REVISED: 16 Nov 2005 (RRD) - initial version from conread
!                 14 Oct 2021 (SYZ) - removed unused variables
!
! USAGE:  CON2API
!
!   INPUT PARAMETERS:
!     PROMPTS FOR ENTRY ON STANDARD INPUT AS REQUIRED
!   INPUT FILES:
!     UNIT 10 - binary concentration file as named on input
!   OUTPUT FILES:
!     NONE
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE:  IBM RS6000
!
!$$$

PROGRAM CONC2API

  IMPLICIT NONE

  INCLUDE 'PARMS3.EXT'      ! I/O API constants
  INCLUDE 'FDESC3.EXT'      ! I/O API file description data structure
  INCLUDE 'IODECL3.EXT'     ! I/O API function declarations

  CHARACTER(80)             :: LABEL, INPFILE, OUTFILE
  CHARACTER(4)              :: PTYPE, MODEL

  REAL,         ALLOCATABLE :: CSUM  (:,:,:) ! concentration array
  CHARACTER(4), ALLOCATABLE :: IDENT (:) 
  INTEGER,      ALLOCATABLE :: HEIGHT(:)

  LOGICAL    :: ftest
  INTEGER(2) :: ip,jp
  INTEGER    :: ii,jj,kk,kl,kp,n,nloc,nlat,nlon,cpack 
  INTEGER    :: np,nxyp,kret,krec,ntyp,nlvl,level
  REAL       :: clat,clon,dlat,dlon,olon,olat,olvl 

  INTEGER    :: narg,iargc
  INTEGER    :: jdate,jtime,kdate,ktime
  INTEGER    :: iyr1,imo1,ida1,ihr1,imn1,ifh1
  INTEGER    :: iyr2,imo2,ida2,ihr2,imn2,ifh2

  INTEGER       :: DATE_TIME(8)                   ! cpu clock
  CHARACTER(12) :: REAL_CLOCK(3)

  INTEGER       :: JULIAN,SECSDIFF,SEC2TIME       ! ioapi times
  CHARACTER(24) :: DT2STR
  EXTERNAL      JULIAN,SECSDIFF,SEC2TIME,DT2STR

  INTEGER :: LOGDEV
  LOGDEV   = INIT3()     !  initialization returns unit # for log

!------------------------------------------------------------------

  IF(logdev.LT.0)THEN
     WRITE(*,*)'ERROR: Cannot open the IOAPI log file'
     STOP
  END IF

! check for command line input/output file names
  NARG=IARGC()
  IF(NARG.LT.2)THEN
     WRITE(*,*)'Usage: conc2api [hysplit_infile] [ioapi_outfile]'
     STOP
  END IF
  DO WHILE(NARG.GT.0)
     CALL GETARG(NARG,LABEL)
     IF(narg.EQ.1) INPFILE=LABEL
     IF(narg.EQ.2) OUTFILE=LABEL
     NARG=NARG-1
  END DO

! hysplit binary input file
  INQUIRE (FILE=INPFILE, EXIST=FTEST)
  IF(FTEST)THEN
     OPEN(10,FILE=INPFILE,FORM='UNFORMATTED',ACCESS='SEQUENTIAL')
  ELSE
     WRITE(*,*)'File not found: ',LABEL
     STOP
  END IF

! delete ioapi output file if it exists
  INQUIRE (FILE=OUTFILE, EXIST=FTEST)
  IF(FTEST)THEN
     OPEN(20,FILE=OUTFILE)
     CLOSE(20,STATUS='DELETE')
  END IF

! meteo file information and calculation start data
  READ(10,IOSTAT=kret)MODEL,IYR1,IMO1,IDA1,IHR1,IFH1,NLOC,CPACK
  IF(kret.ne.0)CPACK=0   ! old format files don't have cpack
  IF(cpack.LT.0.OR.cpack.GT.1)THEN
     WRITE(*,*)'Unsupported concentration packing, CPACK=',CPACK
     STOP
  END IF

! pollutant source locations for this simulation
  DO N=1,NLOC
     READ(10,IOSTAT=kret)IYR2,IMO2,IDA2,IHR2,OLAT,OLON,OLVL,IMN2
     IF(kret.ne.0)IMN2=0
  END DO

! set meteorology data time
  IYR1=1900+IYR1
  IF(IYR1.LT.1948)IYR1=IYR1+100
  JDATE=1000*IYR1+JULIAN(IYR1,IMO1,IDA1)
  JTIME=10000*IHR1

! set emissions start time
  IYR2=1900+IYR2
  IF(IYR2.LT.1948)IYR2=IYR2+100
  KDATE=1000*IYR2+JULIAN(IYR2,IMO2,IDA2)
  KTIME=10000*IHR2+100*IMN2  

! horizontal grid index record
  READ(10) NLAT, NLON, DLAT, DLON, CLAT, CLON

! vertical grid index record
  READ(10) NLVL
  BACKSPACE(10)
  ALLOCATE (height(nlvl), STAT=kret)
  READ(10) NLVL, (HEIGHT(KK),KK=1,NLVL)
  ALLOCATE (csum(nlon,nlat,nlvl), STAT=kret)

! pollutant identification record
  READ(10) NTYP
  BACKSPACE(10)
  ALLOCATE (IDENT(ntyp), STAT=kret) 
  READ(10) NTYP, (IDENT(KK),KK=1,NTYP)

!                                   HORIZONTAL STRUCTURE
!                                 ---------------------------
  FTYPE3D = GRDDED3               ! gridded file 
  GDTYP3D = LATGRD3               ! latitude-longitude grid
  XORIG3D = CLON                  ! grid corner point (1,1)
  YORIG3D = CLAT
  XCELL3D = DLON                  ! grid cell size
  YCELL3D = DLAT
  NCOLS3D = NLON                  ! number of grid cells
  NROWS3D = NLAT

!                                   VERTICAL STRUCTURE
!                                 ---------------------------
  VGTYP3D = VGHVAL3               ! height above ground
  NLAYS3D = NLVL
  VGLVS3D(1) = 0.0                ! start at ground level
  DO KK=2,NLVL+1
     VGLVS3D(KK) = HEIGHT(KK-1)
  END DO

!                                   VARIABLE DESCRIPTIONS
!                                 ---------------------------
  NVARS3D = NTYP
  DO KK=1,NTYP
     VNAME3D (KK) = IDENT(KK)     ! pollutant name
     UNITS3D (KK) = 'mass/m3'       
     VDESC3D (KK) = 'cell average concentration'
     VTYPE3D (KK) = M3REAL
  END DO

!                                   FILE DESCRIPTION 
!                                 ---------------------------
  GDNAM3D      = 'Lat-Lon Grid'
  FDESC3D( 1 ) = 'HYSPLIT concentrations output'
  FDESC3D( 2 ) = 'Meteorology model: '//MODEL 
  LABEL = DT2STR(JDATE,JTIME)
  FDESC3D( 3 ) = 'Meteorology valid: '//LABEL(1:24)
  LABEL = DT2STR(KDATE,KTIME)
  FDESC3D( 4 ) = 'Pollutant release: '//LABEL(1:24)
  WRITE(LABEL,'(3F10.3)') OLAT,OLON,OLVL
  FDESC3D( 5 ) = 'Source locations : '//LABEL(1:30)
  DO KK=6,MXDESC3 
     FDESC3D (KK) = ' '     !  rest of lines are blank
  END DO                 

! -----------------------------------------------------------
! time period loop for concentration grid

  KREC=0
  DO WHILE (KREC.GE.0)

     READ(10,END=900)IYR1,IMO1,IDA1,IHR1,IMN1,IFH1   ! sample start
     READ(10)        IYR2,IMO2,IDA2,IHR2,IMN2,IFH2   ! sample stop

     IYR1=1900+IYR1
     IF(IYR1.LT.1948)IYR1=IYR1+100
     JDATE=1000*IYR1+JULIAN(IYR1,IMO1,IDA1)
     JTIME=10000*IHR1+100*IMN1  

     IYR2=1900+IYR2
     IF(IYR2.LT.1948)IYR2=IYR2+100
     KDATE=1000*IYR2+JULIAN(IYR2,IMO2,IDA2)
     KTIME=10000*IHR2+100*IMN2  

!    after the first data record open the output file with previously
!    defined characteristics and assuming that subsequent output time
!    interval will be the same as for this first time period

     IF(krec.EQ.0)THEN
        SDATE3D = JDATE                     ! file start date
        STIME3D = JTIME                     ! file start time
        TSTEP3D = SEC2TIME(SECSDIFF(jdate,jtime,kdate,ktime)) 

!       current processor clock date and time
        CALL DATE_AND_TIME(REAL_CLOCK(1),REAL_CLOCK(2),REAL_CLOCK(3),DATE_TIME)
        CDATE3D=1000*DATE_TIME(1)+JULIAN(DATE_TIME(1),DATE_TIME(2),DATE_TIME(3))
        CTIME3D=10000*DATE_TIME(5)+100*DATE_TIME(6)+DATE_TIME(6)

        IF(.NOT.OPEN3(OUTFILE,FSUNKN3,'conc2api'))THEN
           FTEST = SHUT3()
           WRITE(*,*)'ERROR: cannot open output file'
           STOP
        END IF
     END IF

!    one ioapi output record for each pollutant species 
     DO KP=1,NTYP

!       load data array for all levels, one output record contains all levels
        DO KL=1,NLVL
           IF(CPACK.EQ.1)THEN
              CSUM = 0.0
              READ(10)PTYPE,LEVEL,NXYP,(IP,JP,CSUM(IP,JP,KL),NP=1,NXYP)
           ELSE
              READ(10)PTYPE,LEVEL,((CSUM(II,JJ,KL),II=1,NLON),JJ=1,NLAT)
           END IF
        END DO

        IF(.NOT.WRITE3(OUTFILE,VNAME3D(KP),JDATE,JTIME,CSUM))THEN
           FTEST = SHUT3()
           WRITE(*,*)'ERROR: cannot write to output file'
           STOP
        END IF
        KREC=KREC+1

     END DO  ! species loop

  END DO     ! time periods loop

900 DEALLOCATE (csum,ident,height,STAT=kret)

  FTEST = SHUT3()
  IF(.NOT.FTEST) WRITE(*,*)'ERROR: shutdown of IOAPI'

END PROGRAM conc2api
