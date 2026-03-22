PROGRAM meds2arl

!----------------------------------------------------------------------
! The MEDS gridded data format was written by the WES Access Program 
! (wacc_main) and by other utility programs written by ENSCO for AFTAC.
! The data are stored as fixed length records.  Each record contains 
! data for a single date-time-level-variable.  A header at the beginning 
! of each record defines the time, level, variable, and grid for that 
! record.  Each record in a particular file will encompass the same 
! geographic area, and will therefore contain the same number of bytes. 
! While the records are generally sorted by date/time and level, there
! is no intrinsic requirement to do so.  There is also no requirement 
! for the file to be complete, that is, containing all combination of 
! dates, times, levels, and variables.  It is up to the data user to 
! insure that the necessary data has been found and read. These records 
! were produced on big-endian UNIX systems. The byte order will be 
! opposite that of an Intel-based PC. With a single exception, all 
! fields are two-byte integers.
!----------------------------------------------------------------------
! Last Revised: 16 Oct 2007 (RRD) - initial version from grad2arl
!               18 Sep 2008 (RRD) - terrain field or file now required
!               05 Jan 2009 (RRD) - corrected 30 mb height  
!               14 Jan 2009 (RRD) - create surface pressure field
!----------------------------------------------------------------------

  IMPLICIT NONE

  INTEGER(2)              :: scot,ecot
  INTEGER(2)              :: srow,erow,scol,ecol,yr,day,hr,fcsthr,level
  CHARACTER(2)            :: cvar
  INTEGER(2), ALLOCATABLE :: data(:,:)

  CHARACTER(1), ALLOCATABLE :: CDATA(:)
  REAL,         ALLOCATABLE :: SIGMA(:),HEIGHT(:) 
  REAL,         ALLOCATABLE :: RDATA(:,:),TDATA(:,:) 
  REAL,         ALLOCATABLE :: SURFT(:,:),SURFP(:,:) 

  LOGICAL       :: ftest
  REAL          :: dewp,edew,esat,fact
  INTEGER       :: i,j,k,l,n,narg,iargc,kret,reclen,numrec
  CHARACTER(80) :: label,binfile,cfgfile,arlfile,txtfile,fixfile 

  INTEGER       :: IFHR,LEVELS
  INTEGER       :: JET1,JET2,IY,IM,ID,IH,IN,NXP,NYP,NXY,NZP
  REAL          :: CLAT1,CLON1,DLAT,DLON

! variables that will be output in ARL format
  INTEGER,PARAMETER :: KVAR = 11  ! total number of variables
  INTEGER,PARAMETER :: KSFC = 5   ! surface variables up through KSFC
  INTEGER           :: NVAR(KVAR) ! variable counter
  CHARACTER(4)      :: AVAR(KVAR) = (/'SHGT','PRSS','T02M','U10M','V10M','HGTS',  &
                                      'TEMP','UWND','VWND','WWND','RELH'/)
  REAL              :: UNIT(KVAR) = (/ 1.0,   1.0,   0.1,   0.1,   0.1,   1.0,    &
                                       0.1,   0.1,   0.1, 0.00001, 0.1  /)

  REAL              :: SIGTOP = 100.0  ! highest level with W and DP
  REAL              :: SIGTWW, SIGTDP

! standard heights
  INTEGER :: NLVL(26)
  INTEGER :: STDHT(26) = (/ 111,  323,  540,  765,  990, 1457, 1949, 2466, &
                           3012, 3591, 4206, 4865, 5574, 6344, 7185, 8117, &
                           9164,10363,11784,13608,16180,18442,20576,23849, &
                          26481,31055 /)

  INTEGER :: STDPR(26) = (/1000,  975,  950,  925,  900,  850,  800,  750, &
                            700,  650,  600,  550,  500,  450,  400,  350, &
                            300,  250,  200,  150,  100,   70,   50,   30, &
                             20,   10 /)

!--------------------------------------
  INTERFACE
  SUBROUTINE PAKSET(LUNIT,FNAME,KREC1,NXP,NYP,NZP)
  IMPLICIT NONE
  INTEGER,       INTENT(IN)    :: lunit     ! output unit number
  CHARACTER(*),  INTENT(IN)    :: fname     ! file name of METDATA.CFG
  INTEGER,       INTENT(IN)    :: krec1     ! position of index record 
  INTEGER,       INTENT(OUT)   :: nxp, nyp  ! horizontal grid dimensions
  INTEGER,       INTENT(OUT)   :: nzp       ! vertical grid dimen (incl sfc)
  END SUBROUTINE pakset

  SUBROUTINE PAKREC(LUNIT,RVAR,CVAR,NX,NY,NXY,KVAR,IY,IM,ID,IH,MN,IC,LL,KINI)
  IMPLICIT NONE
  INTEGER,      INTENT(IN)    :: LUNIT       ! output unit number
  INTEGER,      INTENT(IN)    :: NX,NY       ! dimensions of RVAR
  INTEGER,      INTENT(IN)    :: NXY         ! dimensions of CVAR
  REAL,         INTENT(IN)    :: RVAR(NX,NY) ! input data to be packed
  CHARACTER(1), INTENT(INOUT) :: CVAR(NXY)   ! packed data array
  CHARACTER(4), INTENT(IN)    :: KVAR        ! descriptor of variable written
  INTEGER,      INTENT(IN)    :: IY,IM,ID    ! date identification
  INTEGER,      INTENT(IN)    :: IH,MN       ! time identification (MN-minutes)
  INTEGER,      INTENT(IN)    :: IC          ! forecast hour, ICX hour for >99
  INTEGER,      INTENT(IN)    :: LL          ! level indicator
  INTEGER,      INTENT(IN)    :: KINI        ! initialization (0-no 1-yes)
  END SUBROUTINE pakrec

  SUBROUTINE MAKNDX(KUNIT,FNAME,LEVELS,NXP,NYP,KVAR,NVAR,KSFC,AVAR,SIGMA, &
                    CLAT1,CLON1,DLAT,DLON,IY,IM,ID,IH,SIGTOP)
  IMPLICIT NONE
  INTEGER,      INTENT(IN)  :: KUNIT
  CHARACTER(*), INTENT(IN)  :: FNAME
  INTEGER,      INTENT(IN)  :: LEVELS
  INTEGER,      INTENT(IN)  :: NXP,NYP
  INTEGER,      INTENT(IN)  :: KVAR
  INTEGER,      INTENT(IN)  :: NVAR(KVAR)
  INTEGER,      INTENT(IN)  :: KSFC   
  CHARACTER(4), INTENT(IN)  :: AVAR(:)
  REAL,         INTENT(IN)  :: SIGMA(:)
  REAL,         INTENT(IN)  :: CLAT1,CLON1,DLAT,DLON
  INTEGER,      INTENT(IN)  :: IY,IM,ID,IH
  REAL,         INTENT(IN)  :: SIGTOP
  END SUBROUTINE makndx
  END INTERFACE
!---------------------------------------

  binfile='meds.bin'  ! input data
  cfgfile='meds.cfg'  ! packing configuration
  arlfile='meds.arl'  ! hysplit formatted output file
  txtfile='meds.txt'  ! diagnostic message file
  fixfile='meds.fix'  ! fixed terrain file

  dlat=1.0           
  dlon=1.0           

! go through each argument

  NARG=IARGC()
  DO WHILE (NARG.GT.0)
     CALL GETARG(NARG,LABEL)
     SELECT CASE (LABEL(1:2))

!    ARL packed output file name
     CASE ('-o','-O')
        READ(LABEL(3:),'(A)')ARLFILE

!    meds binary input file name
     CASE ('-i','-I')
        READ(LABEL(3:),'(A)')BINFILE

!    meds binary input terrain file name
     CASE ('-t','-T')
        READ(LABEL(3:),'(A)')FIXFILE

!    input data grid resolution (degrees) 
     CASE ('-r','-R')
        READ(LABEL(3:),'(F10.0)')DLAT
        DLON=DLAT

     END SELECT
     NARG=NARG-1
  END DO

! check for required files
  INQUIRE(FILE=binfile,EXIST=ftest)
  IF(.NOT.ftest)THEN
     WRITE(*,*)'Usage: meds2arl [-options {default}]'
     WRITE(*,*)' -i[Input MEDS file name {meds.bin}]'
     WRITE(*,*)' -t[Input MEDS terr name {meds.fix}]'
     WRITE(*,*)' -o[Output ARL file name {meds.arl}]'
     WRITE(*,*)' -r[Resolution MEDS data degs {1.0}]'
     STOP
  END IF

! diagnostic message output
  OPEN(50,FILE=txtfile)

! determine record length and open file
  OPEN(10,FILE=binfile,RECL=20,ACCESS='DIRECT')
  READ(10,REC=1) srow,erow,scol,ecol,yr,day,hr,fcsthr,level,cvar
  WRITE(50,'(A,2I5)')'Start and end rows: ',srow,erow
  WRITE(50,'(A,2I5)')'Start and end cols: ',scol,ecol
  WRITE(50,'(A,3I5)')'Rec1 date and time: ',yr,day,hr
  reclen=20+2*(ecol-scol+1)*(erow-srow+1)
  CLOSE(10)
  OPEN(10,FILE=binfile,RECL=reclen,ACCESS='DIRECT')

! output grid structure
  NXP=ecol-scol+1
  NYP=erow-srow+1
  WRITE(50,'(A,2F10.1)')'MEDS grid dimensions (nx,ny): ',NXP,NYP     
  CLAT1=-90.0+(srow-1)*dlat 
  CLON1=(scol-1)*dlon  
  WRITE(50,'(A,2F10.1)')'MEDS grid corner point (L,L): ',CLON1,CLAT1
  NXY=NXP*NYP
  ALLOCATE (DATA(NXP,NYP))
  ALLOCATE (CDATA(NXY),RDATA(NXP,NYP),TDATA(NXP,NYP))
  ALLOCATE (SURFT(NXP,NYP),SURFP(NXP,NYP))

! set the initial time from the first record
  CALL TMINIT
  IY=YR
  CALL TM2MIN(IY,1,1,0,0,JET1)
  JET1=JET1+(day-1)*1440+hr*60
  CALL TM2DAY(JET1,IY,IM,ID,IH,IN)  

! check for required variables
  n=1
  kret=0
  nvar=0
  nlvl=0

  init : DO WHILE (kret.EQ.0)
     READ(10,REC=n,IOSTAT=kret)  &
          srow,erow,scol,ecol,yr,day,hr,fcsthr,level,cvar
     IF(kret.NE.0) EXIT init

!    diagnostic dump of contents
     WRITE(50,'(I5,I4,I3,I5,A3)')yr,day,hr,level,cvar

!    special 2D fixed variable: Surface Terrain (ST) height
     IF(cvar.EQ.'ST')THEN
        nvar(1)=1 
        nvar(2)=1
        READ(10,REC=n)srow,erow,scot,ecot,yr,day,hr,fcsthr,level,cvar,data
        IF(scol.NE.scot.OR.ecol.NE.ecot)THEN
           WRITE(*,*)'Start or columns for terrain do not match data'
           WRITE(*,*)'MEDS: ',scol,ecol
           WRITE(*,*)'TERR: ',scot,ecot
           STOP
        END IF
        DO j=1,nyp
        DO i=1,nxp
           surft(i,j)=data(i,j)
        END DO
        END DO
     END IF

!    count the 2D variables in the file
     IF(cvar.EQ.'T '.AND.(level.EQ.0.OR.level.EQ.9995))nvar(3)=1 
     IF(cvar.EQ.'U '.AND.(level.EQ.0.OR.level.EQ.9995))nvar(4)=1 
     IF(cvar.EQ.'V '.AND.(level.EQ.0.OR.level.EQ.9995))nvar(5)=1 

!    count the 3D variables in the file
     IF(cvar.EQ.'D '.AND.(level.GT.0.AND.level.LE.1000))nvar( 6)=1
     IF(cvar.EQ.'T '.AND.(level.GT.0.AND.level.LE.1000))nvar( 7)=1 
     IF(cvar.EQ.'U '.AND.(level.GT.0.AND.level.LE.1000))nvar( 8)=1 
     IF(cvar.EQ.'V '.AND.(level.GT.0.AND.level.LE.1000))nvar( 9)=1 

!    find the top level that these data are available
     IF(cvar.EQ.'W '.AND.(level.GT.0.AND.level.LE.1000))THEN
        nvar(10)=1 
        sigtww=level
     END IF
     IF(cvar.EQ.'DP'.AND.(level.GT.0.AND.level.LE.1000))THEN
        nvar(11)=1 
        sigtdp=level
     END IF

!    count the levels in the file
     DO k=1,26
        IF(stdpr(k).EQ.level) nlvl(k)=1
     END DO
     n=n+1
  END DO init     
  numrec=n-1

! other messages
  WRITE(50,'(A,F8.1)')'### Top level for vertical vel = ',SIGTWW
  WRITE(50,'(A,F8.1)')'### Top level for dew pnt depr = ',SIGTDP
  IF(sigtww.NE.sigtdp) &
     WRITE(*,*)'Moisture and vertical motion end at different levels'
  SIGTOP=MAX(sigtww,sigtdp)

! if terrain height not included, then load terrain data file
  IF(nvar(1).EQ.0)THEN
     OPEN(40,FILE=fixfile,RECL=reclen,ACCESS='DIRECT')
     READ(40,REC=1)srow,erow,scot,ecot,yr,day,hr,fcsthr,level,cvar,data
     IF(scol.NE.scot.OR.ecol.NE.ecot)THEN
        WRITE(*,*)'Start or columns for terrain do not match data'
        WRITE(*,*)'MEDS: ',scol,ecol
        WRITE(*,*)'TERR: ',scot,ecot
        STOP
     END IF
     DO j=1,nyp
     DO i=1,nxp
        surft(i,j)=data(i,j)
     END DO
     END DO
     CLOSE(40)
     nvar(1)=1
     nvar(2)=1
     WRITE(50,'(A)')'### Found terrain height in fixed input file'
  ELSE
     WRITE(50,'(A)')'### Found terrain height record in data file'
  END IF

  IF(SUM(nvar).NE.kvar)THEN
     WRITE(*,*)'Input file missing variables!'
     WRITE(*,*)'ST,Tsfc,Usfc,Vsfc,  D,T,U,V,W,DP'
     WRITE(*,*) nvar(1:ksfc),'            ',nvar(ksfc+1:)
     WRITE(*,*)'Only found: ',SUM(nvar)
  END IF

! collapse the level array space to those available
  levels=SUM(nlvl)
  ALLOCATE (sigma(levels),height(levels))

! fill the output level array variables
  k=0
  DO n=1,26     
     IF(nlvl(n).NE.0)THEN
        k=k+1
        sigma(k)=stdpr(n)                
        height(k)=stdht(n)
     END IF
  END DO

! determine the surface pressure from the terrain height
  DO j=1,nyp
  DO i=1,nxp
     n=1
     DO WHILE (n.LE.k.AND.stdht(n).LT.surft(i,j))
        n=n+1
     END DO

     IF(n.EQ.1)THEN
        fact=surft(i,j)/stdht(n) 
        surfp(i,j)=fact*(stdpr(n)-1013.0)+1013.0 
     ELSEIF(n.LT.k)THEN
        fact=(surft(i,j)-stdht(n-1))/(stdht(n)-stdht(n-1)) 
        surfp(i,j)=fact*(stdpr(n)-stdpr(n-1))+stdpr(n-1)
     ELSE
        WRITE(*,*)'No match of terrain height with STD atmosphere: ',SURFT(I,J)
        WRITE(*,*)'At grid point (i,j): ',I,J  
        STOP
     END IF

  END DO
  END DO

! create the packing configuration file
  CALL MAKNDX(30,CFGFILE,LEVELS,NXP,NYP,KVAR,NVAR,KSFC,AVAR,SIGMA, &
                 CLAT1,CLON1,DLAT,DLON,IY,IM,ID,IH,SIGTOP)
  CALL PAKSET(20,CFGFILE,1,NXP,NYP,NZP)
  OPEN(20,FILE=ARLFILE,RECL=(NXY+50),FORM='UNFORMATTED',ACCESS='DIRECT')

!--------------------------------
! process data file by record   

  DO N=1,NUMREC  
     READ(10,REC=n)srow,erow,scol,ecol,yr,day,hr,fcsthr,level,cvar,data

!    compute the current time
     IY=yr
     IFHR=fcsthr
     CALL TM2MIN(IY,1,1,0,0,JET2)
     JET2=JET2+(day-1)*1440+hr*60
     CALL TM2DAY(JET2,IY,IM,ID,IH,IN)  

!    special treatment for first record (same as new time)
     IF(n.EQ.1)THEN
        WRITE(*,*)'Starting new time: ',IY,IM,ID,IH
        CALL PAKREC(20,SURFT,CDATA,NXP,NYP,NXY,AVAR(1),IY,IM,ID,IH,IN,IFHR,1,1)
        CALL PAKREC(20,SURFP,CDATA,NXP,NYP,NXY,AVAR(2),IY,IM,ID,IH,IN,IFHR,1,1)
     END IF

!    has the time changed between this record and the last record
     IF(JET2.GT.JET1)THEN
        CALL PAKNDX(20)
        WRITE(*,*)'Starting new time: ',IY,IM,ID,IH
        CALL PAKREC(20,SURFT,CDATA,NXP,NYP,NXY,AVAR(1),IY,IM,ID,IH,IN,IFHR,1,1)
        CALL PAKREC(20,SURFP,CDATA,NXP,NYP,NXY,AVAR(2),IY,IM,ID,IH,IN,IFHR,1,1)
        JET1=JET2
     ELSEIF(JET2.LT.JET1)THEN
        WRITE(*,*)'Data not in time sequence: ',IY,IM,ID,IH
        STOP
     ELSE
        CONTINUE    
     END IF

!    find the level
     L=0
     zvalue : DO j=1,levels
        IF(level.EQ.sigma(j))THEN
          L=J+1
          EXIT zvalue
        ELSEIF(level.EQ.0.OR.level.EQ.9995)THEN
          L=1      
          EXIT zvalue
        ELSE
          CYCLE zvalue 
        END IF 
     END DO zvalue

!    find the variables
     k=0
     IF(cvar.EQ.'ST')THEN  
        k=1
     ELSEIF(cvar.EQ.'T '.AND.L.EQ.1)THEN
        k=3
     ELSEIF(cvar.EQ.'U '.AND.L.EQ.1)THEN  
        k=4
     ELSEIF(cvar.EQ.'V '.AND.L.EQ.1)THEN 
        k=5
     ELSEIF(cvar.EQ.'D ')THEN      
        k=6
     ELSEIF(cvar.EQ.'T ')THEN
        k=7
!       save temperature field for RH
        TDATA=DATA*UNIT(K) 
     ELSEIF(cvar.EQ.'U ')THEN  
        k=8
     ELSEIF(cvar.EQ.'V ')THEN 
        k=9
     ELSEIF(cvar.EQ.'W ')THEN  
        k=10
     ELSEIF(cvar.EQ.'DP')THEN  
        k=11
!       skip surface RH
        IF(L.EQ.1) K=0
     ELSE 
        CONTINUE
     END IF

!    convert to ARL format and write data record
     IF(K.GT.0.AND.L.GT.0)THEN

!       special unit conversions
        IF(cvar.EQ.'D ')THEN
           RDATA=STDHT(L-1)+DATA*UNIT(K)
        ELSEIF(cvar.EQ.'DP')THEN
!          convert dewpoint to relative humidity
           RDATA=DATA*UNIT(K)
           DO j=1,nyp
           DO i=1,nxp
              dewp=tdata(i,j)-rdata(i,j)
              edew=EXP(21.4-(5351.0/dewp))
              esat=EXP(21.4-(5351.0/tdata(i,j)))
              RDATA(i,j)=100.0*edew/esat
           END DO
           END DO
        ELSE
           RDATA=DATA*UNIT(K)
        END IF

        CALL PAKREC(20,RDATA,CDATA,NXP,NYP,NXY,AVAR(K),IY,IM,ID,IH,IN,IFHR,L,0)
     END IF

  END DO
  CALL PAKNDX(20)
  CLOSE(50)


END PROGRAM meds2arl

!------------------------------------------------------------
! Creates the CFG file requred by the ARL packing subroutines

SUBROUTINE MAKNDX(KUNIT,FNAME,LEVELS,NXP,NYP,KVAR,NVAR,KSFC,AVAR,SIGMA, &
                  CLAT1,CLON1,DLAT,DLON,IY,IM,ID,IH,SIGTOP)

  IMPLICIT NONE

  INTEGER,      INTENT(IN)  :: KUNIT
  CHARACTER(*), INTENT(IN)  :: FNAME 
  INTEGER,      INTENT(IN)  :: LEVELS
  INTEGER,      INTENT(IN)  :: NXP,NYP
  INTEGER,      INTENT(IN)  :: KVAR 
  INTEGER,      INTENT(IN)  :: NVAR(KVAR)
  INTEGER,      INTENT(IN)  :: KSFC 
  CHARACTER(4), INTENT(IN)  :: AVAR(:)
  REAL,         INTENT(IN)  :: SIGMA(:)
  REAL,         INTENT(IN)  :: CLAT1,CLON1,DLAT,DLON
  INTEGER,      INTENT(IN)  :: IY,IM,ID,IH
  REAL,         INTENT(IN)  :: SIGTOP

  REAL                      :: SIG,GRIDS(12)
  INTEGER                   :: I,K,L,N1,N2,NV

! set the lat/lon position of the maximum grid point
  GRIDS(1)=CLAT1+(NYP-1)*DLAT
  GRIDS(2)=CLON1+(NXP-1)*DLON
  GRIDS(2)=MOD(GRIDS(2),360.0)
! set the reference lat/lon
  GRIDS(3)=DLAT
  GRIDS(4)=DLON
! lat/lon grids following always zero
  GRIDS(5)=0.0   
  GRIDS(6)=0.0 
  GRIDS(7)=0.0 
! set the x,y coordinates of the minimum point
  GRIDS(8)=1.0
  GRIDS(9)=1.0     
! set the lat/lon of the sync point
  GRIDS(10)=CLAT1   
  GRIDS(11)=CLON1
! reserved variable
  GRIDS(12)=0.0

! open and write the packer configuration file
  OPEN(KUNIT,FILE=FNAME)
! model identification
  WRITE(KUNIT,'(20X,A4)')'MEDS'
! grid number
  WRITE(KUNIT,'(20X,I4)')99
! vertical coordinate system 
! (1=sigma, 2=pressure, 3=terrain, 4-hybrid)
  WRITE(KUNIT,'(20X,I4)') 2    
! grid definition array
  WRITE(KUNIT,'(20X,F10.2)')(GRIDS(I),I=1,12)
! number of x,y,z points
  WRITE(KUNIT,'(20X,I4)')NXP,NYP,(LEVELS+1)

  DO L=0,LEVELS

     IF(L.EQ.0)THEN
        SIG=0.0
        NV=SUM(NVAR(:KSFC))
        N1=1
        N2=N1+NV-1
     ELSE
        SIG=SIGMA(L)
        NV=KVAR-KSFC
        N1=KSFC+1
        N2=KVAR
!       drop humidity and vertical motion
        IF(sig.LT.sigtop) THEN
           NV=NV-2
           N2=N2-2
        END IF
     END IF

     IF(SIG.LE.1.0)THEN
        WRITE(KUNIT,'(20X,F6.4,I3,99(1X,A4))')SIG,NV,(AVAR(K),K=N1,N2)
     ELSEIF(SIG.GT.1.0.AND.SIG.LT.10.0)THEN
        WRITE(KUNIT,'(20X,F6.4,I3,99(1X,A4))')SIG,NV,(AVAR(K),K=N1,N2)
     ELSEIF(SIG.GE.10.0.AND.SIG.LT.100.0)THEN
        WRITE(KUNIT,'(20X,F6.3,I3,99(1X,A4))')SIG,NV,(AVAR(K),K=N1,N2)
     ELSEIF(SIG.GE.100.0.AND.SIG.LT.1000.0)THEN
        WRITE(KUNIT,'(20X,F6.2,I3,99(1X,A4))')SIG,NV,(AVAR(K),K=N1,N2)
     ELSEIF(SIG.GE.1000.0)THEN
        WRITE(KUNIT,'(20X,F6.1,I3,99(1X,A4))')SIG,NV,(AVAR(K),K=N1,N2)
     END IF

  END DO
  CLOSE (KUNIT)

END SUBROUTINE makndx
