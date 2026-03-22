!$$$  PROGRAM DOCUMENTATION BLOCK
!
! PROGRAM:     TRAJ2API         READS HYSPLIT TRAJECTORY FILE
!                               AND CONVERT TO CMAQ I/O API FORMAT 
!   PRGMMR:    ROLAND DRAXLER   ORG: R/ARL       DATE:05-11-30
!
! ABSTRACT:  THIS CODE WRITTEN AT THE AIR RESOURCES LABORATORY ...
!   PROGRAM TO READ THE HYSPLIT TRAJECTORY ENDPOINTS FILE AND
!   CONVERT TO THE CMAQ I/O API FORMAT
!
! PROGRAM HISTORY LOG:
!   LAST REVISED: 30 Nov 2005 (RRD) - initial version from conc2api
!                 14 Oct 2021 (SYZ) - removed unused variables
!
! USAGE: TRAJ2API
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

PROGRAM TRAJ2API

  IMPLICIT NONE

  INCLUDE 'PARMS3.EXT'      ! I/O API constants
  INCLUDE 'FDESC3.EXT'      ! I/O API file description data structure
  INCLUDE 'IODECL3.EXT'     ! I/O API function declarations

  INTEGER,        PARAMETER :: MAXTRAJ = 1
  INTEGER,        PARAMETER :: MAXVARB = 8

  CHARACTER(8)    :: LDIAG(maxvarb)
  REAL            :: TDIAG(maxtraj,maxvarb)
  REAL            :: OLAT(maxtraj),OLON(maxtraj),OLVL(maxtraj)
  INTEGER         :: TRID(maxtraj)

  LOGICAL         :: FTEST
  CHARACTER(80)   :: LABEL, INPFILE, OUTFILE
  CHARACTER(8)    :: MODEL, DIRCTN,  MOTION
  INTEGER         :: NGRD,  NTRAJ,   NDIAG, TFMT,   KRET

  REAL            :: hours
  INTEGER         :: k,l,narg,iargc,grid,fcst
  INTEGER         :: jdate,jtime,kdate,ktime
  INTEGER         :: iyr1,imo1,ida1,ihr1,imn1,ifh1
  INTEGER         :: iyr2,imo2,ida2,ihr2

  INTEGER         :: DATE_TIME(8)     
  CHARACTER(12)   :: REAL_CLOCK(3)

  INTEGER         :: LOGDEV
  INTEGER         :: JULIAN,SECSDIFF,SEC2TIME     
  CHARACTER(24)   :: DT2STR
  EXTERNAL           JULIAN,SECSDIFF,SEC2TIME,DT2STR

  COMMON / OUTDAT / NTRAJ,TRID,OLAT,OLON,OLVL,TDIAG

!------------------------------------------------------------------

  LOGDEV   = INIT3()     !  initialization returns unit # for log
  IF(logdev.LT.0)THEN
     WRITE(*,*)'ERROR: Cannot open the IOAPI log file'
     STOP
  END IF

! check for command line input/output file names
  NARG=IARGC()
  IF(NARG.LT.2)THEN
     WRITE(*,*)'Usage: traj2api [hysplit_infile] [ioapi_outfile]'
     STOP
  END IF
  DO WHILE(NARG.GT.0)
     CALL GETARG(NARG,LABEL)
     IF(narg.EQ.1) INPFILE=LABEL
     IF(narg.EQ.2) OUTFILE=LABEL
     NARG=NARG-1
  END DO

! hysplit trajectory input file
  INQUIRE (FILE=INPFILE, EXIST=FTEST)
  IF(FTEST)THEN
     OPEN(10,FILE=INPFILE)
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

! number of meteorological grids used in calculation
! and the format ID (0 or missing = old) of the file
  READ(10,'(2I6)',IOSTAT=kret)NGRD,TFMT
  IF(kret.NE.0) tfmt=0

! for each grid load the model and data file starting time
  DO K=1,NGRD
     READ(10,'(A8,5I6)') MODEL,IYR1,IMO1,IDA1,IHR1,IFH1
     IF(K.EQ.1)THEN
!       set meteorology data time 
        IYR1=1900+IYR1
        IF(IYR1.LT.1948)IYR1=IYR1+100
        JDATE=1000*IYR1+JULIAN(IYR1,IMO1,IDA1)
        JTIME=10000*IHR1
     END IF
  END DO

! number of different trajectories in the file
  IF(tfmt.EQ.0)THEN
     READ(10,'(I6,2A8)')NTRAJ,DIRCTN,MOTION
  ELSE
     READ(10,'(I6,2(1X,A8))')NTRAJ,DIRCTN,MOTION
  END IF
  IF(NTRAJ.GT.MAXTRAJ)THEN
     WRITE(*,*)'Number of trajectories exceeds MAX: ',NTRAJ,' > ',MAXTRAJ
     STOP
  END IF

! load starting point for each different trajectory
  DO K=1,NTRAJ
     IF(tfmt.EQ.0)THEN
        READ(10,'(4I6,2F8.3,F8.1)') IYR2,IMO2,IDA2,IHR2,OLAT(k),OLON(k),OLVL(k)
     ELSE
        READ(10,'(4I6,2F9.3,F8.1)') IYR2,IMO2,IDA2,IHR2,OLAT(k),OLON(k),OLVL(k)
     END IF
     IF(K.EQ.1)THEN
!       set trajectory start time
        IYR2=1900+IYR2
        IF(IYR2.LT.1948)IYR2=IYR2+100
        KDATE=1000*IYR2+JULIAN(IYR2,IMO2,IDA2)
        KTIME=10000*IHR2
     END IF
  END DO

! number of diagnostic meteo variables and their label
  READ(10,'(I6)')NDIAG
  BACKSPACE(10)
  IF(NDIAG.GT.MAXVARB)THEN
     WRITE(*,*)'Number of variables exceeds MAX: ',NDIAG,' > ',MAXVARB
     STOP
  END IF
  IF(tfmt.EQ.0)THEN
     READ(10,'(I6,10A8)')NDIAG,(LDIAG(k),k=1,ndiag)
  ELSE
     READ(10,'(I6,10(1X,A8))')NDIAG,(LDIAG(k),k=1,ndiag)
  END IF

!------------------------------------------------------------
! FILE DESCRIPTION 
!------------------------------------------------------------

  NCOLS3D = 1            
  NROWS3D = MAXTRAJ    
  NTHIK3D = 1
  NLAYS3D = 1
  VGTYP3D = IMISS3
  VGTOP3D = IMISS3
  FTYPE3D = IDDATA3             

! default(3)=lat,lon,agl + ndiag(1)=pres, ... 
  NVARS3D = 3 + NDIAG

  VNAME3D (1) = 'LAT'  
  UNITS3D (1) = 'Degrees'       
  VDESC3D (1) = 'Latitude'
  VTYPE3D (1) = M3REAL

  VNAME3D (2) = 'LON'  
  UNITS3D (2) = 'Degrees'       
  VDESC3D (2) = 'Longitude'
  VTYPE3D (2) = M3REAL

  VNAME3D (3) = 'ZAGL' 
  UNITS3D (3) = 'Meters'       
  VDESC3D (3) = 'Height above ground'
  VTYPE3D (3) = M3REAL

  DO K=1,NDIAG
     VNAME3D (3+K) = LDIAG(K) 
     UNITS3D (3+K) = ' '       
     VDESC3D (3+K) = ' ' 
     VTYPE3D (3+K) = M3REAL
  END DO

  FDESC3D( 1 ) = 'HYSPLIT trajectory: '//DIRCTN
  FDESC3D( 2 ) = 'Meteorology model: '//MODEL 
  FDESC3D( 3 ) = 'Vertical motion: '//MOTION
  LABEL = DT2STR(JDATE,JTIME)
  FDESC3D( 4 ) = 'Meteorology valid: '//LABEL(1:24)
  LABEL = DT2STR(KDATE,KTIME)
  FDESC3D( 5 ) = 'Trajectory start: '//LABEL(1:24)
  DO K=1,NTRAJ
     WRITE(LABEL,'(3F10.3)') OLAT(k),OLON(k),OLVL(k)
     FDESC3D(5+K) = 'Source location: '//LABEL(1:30)
  END DO
  DO K=(6+NTRAJ),MXDESC3 
     FDESC3D (K) = ' ' 
  END DO                 

  SDATE3D = KDATE                     ! file start date
  STIME3D = KTIME                     ! file start time
  TSTEP3D = 10000                     ! HHMMSS -- 1 hour 
! TSTEP3D = SEC2TIME(SECSDIFF(jdate,jtime,kdate,ktime)) 

! current processor clock date and time
  CALL DATE_AND_TIME(REAL_CLOCK(1),REAL_CLOCK(2),REAL_CLOCK(3),DATE_TIME)
  CDATE3D=1000*DATE_TIME(1)+JULIAN(DATE_TIME(1),DATE_TIME(2),DATE_TIME(3))
  CTIME3D=10000*DATE_TIME(5)+100*DATE_TIME(6)+DATE_TIME(6)

  IF(.NOT.OPEN3(OUTFILE,FSUNKN3,'traj2api'))THEN
     FTEST = SHUT3()
     WRITE(*,*)'ERROR: cannot open output file'
     STOP
  END IF

!------------------------------------------------------------
! time period loop
!------------------------------------------------------------

  KRET=0
  tloop : DO WHILE (KRET.EQ.0)

     DO K=1,NTRAJ
        IF(tfmt.EQ.0)THEN
           READ(10,'(8I6,F8.1,2F8.3,10F8.1)',IOSTAT=KRET)        &
           TRID(k),GRID,IYR1,IMO1,IDA1,IHR1,IMN1,FCST,HOURS,     &
           OLAT(k),OLON(k),OLVL(k),(TDIAG(k,l),l=1,ndiag)
        ELSE
           READ(10,'(8I6,F8.1,2F9.3,9(1X,F8.1))',IOSTAT=KRET)    &
           TRID(k),GRID,IYR1,IMO1,IDA1,IHR1,IMN1,FCST,HOURS,     &
           OLAT(k),OLON(k),OLVL(k),(TDIAG(k,l),l=1,ndiag)
        END IF
     END DO
     IF(KRET.NE.0) EXIT tloop

     IYR1=1900+IYR1
     IF(IYR1.LT.1948)IYR1=IYR1+100
     JDATE=1000*IYR1+JULIAN(IYR1,IMO1,IDA1)
     JTIME=10000*IHR1+100*IMN1  

     IF(.NOT.WRITE3(OUTFILE,'ALL',JDATE,JTIME,NTRAJ))THEN
        FTEST = SHUT3()
        WRITE(*,*)'ERROR: cannot write to output file'
        STOP
     END IF

  END DO tloop

  FTEST = SHUT3()
  IF(.NOT.FTEST) WRITE(*,*)'ERROR: shutdown of IOAPI'

END PROGRAM traj2api
