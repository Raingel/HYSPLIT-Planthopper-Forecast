PROGRAM GRAD2ARL

!-------------------------------------------------------------------------------
! Converts GRADS file with one time period of data to ARL format. The GRADS
! binary file is assumed to be a simple 4 byte per real variable. Variables 
! are written in the order specified in the GRADS control file.  The program 
! is designed to work only for input data on a regular latitude-longitude grid.
! The north-south index is assumed to increase from north to south.  
!
! Description of grads control file:
! http://www.iges.org/grads/gadoc/aboutgriddeddata.html#descriptor
!-------------------------------------------------------------------------------
! Last Revision: 29 Jan 2003 (RRD) - initial version from data_avrg 
! Last Revision: 03 Aug 2007 (BS)  - hybrid levels option - cntl file
!                                      - was zdef 64 levels 1000. 975. 950. ...
!                                      - now zdef 64 linear 1 1
! Last Revision: 12 Dec 2007 (BS)  - -m 4-character model ID 
!-------------------------------------------------------------------------------

  IMPLICIT NONE

  LOGICAL       :: FTEST1,FTEST2
  INTEGER       :: NREC,NARG,IARGC,LREC,LEVELS,NUMVAR
  INTEGER       :: I,J,N,L,LL,IY,IM,ID,IH,NXP,NYP,NXY,NZP,IVC
  REAL          :: CLAT1,CLON1,DLAT,DLON

  CHARACTER(4)  :: MDLID
  CHARACTER(50) :: LABEL
  CHARACTER(80) :: CTLFILE,BINFILE,CFGFILE,ARLFILE,ABFILE

  CHARACTER(1), ALLOCATABLE :: CDATA(:)
  REAL,         ALLOCATABLE :: RDATA(:,:)     
  REAL,         ALLOCATABLE :: SIGMA(:)

  INTEGER       :: IMIN = 0
  INTEGER       :: IFHR = 0

! variables that will be output in ARL format
  INTEGER,PARAMETER :: NVAR   = 5
  CHARACTER(3)      :: GVAR(NVAR) = (/'PS ','T  ','Q  ','U  ','V  '/)
  CHARACTER(4)      :: AVAR(NVAR) = (/'PRSS','TEMP','SPHU','UWND','VWND'/)
  REAL              :: UNIT(NVAR) = (/  0.01,   1.0,   1.0,   1.0,  1.0 /)

! variables that will be read from the grads binary file
  INTEGER,PARAMETER :: MAXVAR = 20
  INTEGER           :: NUMB(MAXVAR) ! the output array index value
  INTEGER           :: NUML(MAXVAR) ! the number of levels 

!-------------------------------------------------------------------------------
  INTERFACE
  SUBROUTINE PAKSET(LUNIT,FNAME,KREC1,NXP,NYP,NZP)
  IMPLICIT NONE
  INTEGER,       INTENT(IN)    :: lunit     ! output unit number
  CHARACTER(*),  INTENT(IN)    :: fname     ! file name of METDATA.CFG
  INTEGER,       INTENT(IN)    :: krec1     ! position of index record at time-1
  INTEGER,       INTENT(OUT)   :: nxp, nyp  ! horizontal grid dimensions
  INTEGER,       INTENT(OUT)   :: nzp       ! vertical grid dimension (incl sfc)
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

  SUBROUTINE GRADCNTL(CTLFILE,SIGMA,LEVELS,NVAR,GVAR,NUMVAR,NUMB,NUML,  &
             NXP,NYP,CLAT1,CLON1,DLAT,DLON,IY,IM,ID,IH,ABFILE,IVC)
  IMPLICIT NONE
  CHARACTER(80),INTENT(IN)  :: CTLFILE
  REAL,         INTENT(OUT) :: SIGMA(:)
  INTEGER,      INTENT(IN)  :: LEVELS
  INTEGER,      INTENT(IN)  :: NVAR 
  CHARACTER(3), INTENT(IN)  :: GVAR(:)
  INTEGER,      INTENT(OUT) :: NUMVAR
  INTEGER,      INTENT(OUT) :: NUMB(:),NUML(:) 
  INTEGER,      INTENT(OUT) :: NXP,NYP
  REAL,         INTENT(OUT) :: CLAT1,CLON1,DLAT,DLON
  INTEGER,      INTENT(OUT) :: IY,IM,ID,IH
  CHARACTER(80),INTENT(IN)  :: ABFILE
  INTEGER,      INTENT(OUT) :: IVC
  END SUBROUTINE gradcntl

  SUBROUTINE MAKNDX(KUNIT,FNAME,LEVELS,NXP,NYP,NVAR,AVAR,NUMVAR,NUMB,NUML,SIGMA, &
                    CLAT1,CLON1,DLAT,DLON,IY,IM,ID,IH,IVC,MDLID)
  IMPLICIT NONE
  INTEGER,      INTENT(IN)  :: KUNIT
  CHARACTER(*), INTENT(IN)  :: FNAME 
  INTEGER,      INTENT(IN)  :: LEVELS
  INTEGER,      INTENT(IN)  :: NXP,NYP
  INTEGER,      INTENT(IN)  :: NVAR 
  CHARACTER(4), INTENT(IN)  :: AVAR(:)
  INTEGER,      INTENT(IN)  :: NUMVAR
  INTEGER,      INTENT(IN)  :: NUMB(:),NUML(:) 
  REAL,         INTENT(IN)  :: SIGMA(:)
  REAL,         INTENT(IN)  :: CLAT1,CLON1,DLAT,DLON
  INTEGER,      INTENT(IN)  :: IY,IM,ID,IH
  INTEGER,      INTENT(IN)  :: IVC
  CHARACTER(4), INTENT(IN)  :: MDLID
  END SUBROUTINE makndx
  END INTERFACE
!-------------------------------------------------------------------------------

  ABFILE='abfile'
  MDLID='MDID'
  CTLFILE='gdas.grads.cntl'
  BINFILE='gdas.bin'
  CFGFILE='gdas.cfg'
  ARLFILE='gdas.arl'
  LEVELS=40

! go through each argument

  NARG=IARGC()
  DO WHILE (NARG.GT.0)
     CALL GETARG(NARG,LABEL)
     SELECT CASE (LABEL(1:2))

!    grads text control file
     CASE ('-i','-I')
        READ(LABEL(3:),'(A)')CTLFILE

!    ARL packed output file name
     CASE ('-o','-O')
        READ(LABEL(3:),'(A)')ARLFILE 

!    grads binary input file name
     CASE ('-b','-B')
        READ(LABEL(3:),'(A)')BINFILE 

!    number of data levels
     CASE ('-k','-K')
        READ(LABEL(3:),'(I10)')LEVELS 

!    file with hybrid A,B coordinate values
     CASE ('-c','-C')
        READ(LABEL(3:),'(A)')ABFILE

!    model identification
     CASE ('-m','-M')
        READ(LABEL(3:),'(A)')MDLID

     END SELECT
     NARG=NARG-1
  END DO

! check for required files
  INQUIRE(FILE=CTLFILE,EXIST=FTEST1)
  INQUIRE(FILE=BINFILE,EXIST=FTEST2)
  IF(.NOT.FTEST1.OR..NOT.FTEST2)THEN
     WRITE(*,*)'Usage: grad2arl [-options {default}]'
     WRITE(*,*)' -b[Input binary file name {gdas.bin}]'
     WRITE(*,*)' -i[Input control file name {gdas.grads.cntl}]'
     WRITE(*,*)' -o[Output ARL file name {gdas.arl}]'
     WRITE(*,*)' -k[Number of output levels above sfc {40}]'
     WRITE(*,*)' -c[A,B coordinate file for hybrid levels {abfile}]'
     WRITE(*,*)' -m[Model identification (4-characters) {MDID}]'
     WRITE(*,*)
     IF(.NOT.FTEST1)WRITE(*,*)'File not found: ',CTLFILE
     IF(.NOT.FTEST2)WRITE(*,*)'File not found: ',BINFILE
     STOP
  END IF

! read control file for grid definitions
  ALLOCATE (SIGMA(LEVELS))  
  CALL GRADCNTL(CTLFILE,SIGMA,LEVELS,NVAR,GVAR,NUMVAR,NUMB,NUML, &
                NXP,NYP,CLAT1,CLON1,DLAT,DLON,IY,IM,ID,IH,ABFILE,IVC)
  NXY=NXP*NYP
  ALLOCATE (CDATA(NXY),RDATA(NXP,NYP))

!-------------------------------------------------------------------------------
! define the meteorological data files

! create the packing configuration file
  CALL MAKNDX(30,CFGFILE,LEVELS,NXP,NYP,NVAR,AVAR,NUMVAR,NUMB,NUML,SIGMA, &
                    CLAT1,CLON1,DLAT,DLON,IY,IM,ID,IH,IVC,MDLID)
! set output configuration
  CALL PAKSET(40,CFGFILE,1,NXP,NYP,NZP)

! input BIN file 
  LREC=4*NXY
  OPEN(30,FILE=BINFILE,RECL=LREC,FORM='UNFORMATTED',ACCESS='DIRECT')

! output ARL file 
  LREC=NXY+50
  OPEN(40,FILE=ARLFILE,RECL=LREC,FORM='UNFORMATTED',ACCESS='DIRECT')

!-------------------------------------------------------------------------------
! process data file by variable

  NREC=0
  DO N=1,NUMVAR

     DO L=1,NUML(N)
        LL=L+1
        IF(NUML(N).LE.1)LL=1
        NREC=NREC+1
        IF(NUMB(N).GT.0.AND.L.LE.LEVELS)THEN
!          variable for output    
           READ(30,REC=NREC)((RDATA(I,NYP-J+1),I=1,NXP),J=1,NYP)
           IF(UNIT(NUMB(N)).NE.1.0) RDATA=RDATA*UNIT(NUMB(N))
           CALL PAKREC(40,RDATA,CDATA,NXP,NYP,NXY,AVAR(NUMB(N)),   &
                       IY,IM,ID,IH,IMIN,IFHR,LL,1)
        END IF
     END DO

  END DO 
  CALL PAKNDX(40)
  WRITE(*,*)'Finished time period: ',IY,IM,ID,IH

END PROGRAM grad2arl

!-------------------------------------------------------------------------------
! Creates the CFG file requred by the ARL packing subroutines

SUBROUTINE MAKNDX(KUNIT,FNAME,LEVELS,NXP,NYP,NVAR,AVAR,NUMVAR,NUMB,NUML,SIGMA, &
                  CLAT1,CLON1,DLAT,DLON,IY,IM,ID,IH,IVC,MDLID)

  IMPLICIT NONE

  INTEGER,      INTENT(IN)  :: KUNIT
  CHARACTER(*), INTENT(IN)  :: FNAME 
  INTEGER,      INTENT(IN)  :: LEVELS
  INTEGER,      INTENT(IN)  :: NXP,NYP
  INTEGER,      INTENT(IN)  :: NVAR 
  CHARACTER(4), INTENT(IN)  :: AVAR(:)
  INTEGER,      INTENT(IN)  :: NUMVAR
  INTEGER,      INTENT(IN)  :: NUMB(:),NUML(:) 
  REAL,         INTENT(IN)  :: SIGMA(:)
  REAL,         INTENT(IN)  :: CLAT1,CLON1,DLAT,DLON
  INTEGER,      INTENT(IN)  :: IY,IM,ID,IH
  INTEGER,      INTENT(IN)  :: IVC
  CHARACTER(4), INTENT(IN)  :: MDLID

  CHARACTER(4),ALLOCATABLE  :: VARID(:)
  REAL                      :: SIG,GRIDS(12)
  INTEGER                   :: I,J,K,L,NV
  ALLOCATE (VARID(NVAR))

! set the lat/lon position of the maximum grid point
  GRIDS(1)=CLAT1+(NYP-1)*DLAT
  GRIDS(2)=CLON1+(NXP-1)*DLON
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
 !WRITE(KUNIT,'(20X,A4)')'GFSS'
  WRITE(KUNIT,'(20X,A4)')MDLID
! grid number
  WRITE(KUNIT,'(20X,I4)')99
! vertical coordinate system (1=sigma, 2=pressure, 3=terrain, 4-hybrid)
  WRITE(KUNIT,'(20X,I4)')IVC
! grid definition array
  WRITE(KUNIT,'(20X,F10.2)')(GRIDS(I),I=1,12)
! number of x,y,z points
  WRITE(KUNIT,'(20X,I4)')NXP,NYP,(LEVELS+1)

! upper level information
  DO L=0,LEVELS

     NV=0
     DO I=1,NVAR
    
!       match the input variable with the output   
        varloop : DO J=1,NUMVAR
           IF(NUMB(J).EQ.I) EXIT varloop
        END DO varloop

        IF(L.EQ.0)THEN
           SIG=1.0
!          find all surface variables
           IF(NUML(J).LE.1)THEN
              NV=NV+1
              VARID(NV)=AVAR(I)
           END IF
        ELSE
           SIG=SIGMA(L)
!          find all upper level variables
           IF(NUML(J).GT.1)THEN
              NV=NV+1
              VARID(NV)=AVAR(I)
           END IF
        END IF
     END DO

   ! 8-9-07 change from 4 to 5 sig figures for sig >1
     IF(SIG.LE.1.0)THEN
        WRITE(KUNIT,'(20X,F6.4,I3,99(1X,A4))')SIG,NV,(VARID(K),K=1,NV)
     ELSEIF(SIG.GT.1.0.AND.SIG.LT.10.0)THEN
        WRITE(KUNIT,'(20X,F6.4,I3,99(1X,A4))')SIG,NV,(VARID(K),K=1,NV)
     ELSEIF(SIG.GE.10.0.AND.SIG.LT.100.0)THEN
        WRITE(KUNIT,'(20X,F6.3,I3,99(1X,A4))')SIG,NV,(VARID(K),K=1,NV)
     ELSEIF(SIG.GE.100.0)THEN
        WRITE(KUNIT,'(20X,F6.2,I3,99(1X,A4))')SIG,NV,(VARID(K),K=1,NV)
     END IF

  END DO

  DEALLOCATE (VARID)
  CLOSE (KUNIT)

END SUBROUTINE makndx

!-------------------------------------------------------------------------------
! Read the GRADS control file and extract the variables required to create the
! ARL packed meteorological data file

SUBROUTINE GRADCNTL(CTLFILE,SIGMA,LEVELS,NVAR,GVAR,NUMVAR,NUMB,NUML,  &
           NXP,NYP,CLAT1,CLON1,DLAT,DLON,IY,IM,ID,IH,ABFILE,IVC)

  IMPLICIT NONE

  CHARACTER(80),INTENT(IN)  :: CTLFILE
  REAL,         INTENT(OUT) :: SIGMA(:)
  INTEGER,      INTENT(IN)  :: LEVELS
  INTEGER,      INTENT(IN)  :: NVAR 
  CHARACTER(3), INTENT(IN)  :: GVAR(:)
  INTEGER,      INTENT(OUT) :: NUMVAR
  INTEGER,      INTENT(OUT) :: NUMB(:),NUML(:) 
  INTEGER,      INTENT(OUT) :: NXP,NYP
  REAL,         INTENT(OUT) :: CLAT1,CLON1,DLAT,DLON
  INTEGER,      INTENT(OUT) :: IY,IM,ID,IH
  CHARACTER(80),INTENT(IN)  :: ABFILE
  INTEGER,      INTENT(OUT) :: IVC

  INTEGER           :: I,K1,KK,KRET
  CHARACTER(80)     :: LABEL
  REAL, ALLOCATABLE :: ZVALS(:)
  CHARACTER(3)      :: MON
  CHARACTER(3)      :: MONTH(12) = (/'Jan','Feb','Mar','Apr','May','Jun', &
                                     'Jul','Aug','Sep','Oct','Nov','Dec'/)
  REAL          :: a,b	! A,B hybrid coordinate value
  INTEGER       :: k,nc,nl
  CHARACTER(6)  :: lev

  OPEN(10,FILE=CTLFILE)

  KRET=0
recloop : DO WHILE (KRET.EQ.0)
     READ(10,'(A)',IOSTAT=KRET)LABEL
     IF(KRET.NE.0)EXIT recloop

     K1=INDEX(LABEL,' ')-1
     SELECT CASE (LABEL(1:K1))

        CASE ('xdef')
           READ(LABEL(K1+1:K1+6),'(I6)')NXP 
           READ(LABEL(18:),'(2F12.6)')CLON1,DLON

        CASE ('ydef')
           READ(LABEL(K1+1:K1+6),'(I6)')NYP 
           READ(LABEL(18:),'(2F12.6)')CLAT1,DLAT

        CASE ('zdef')
           READ(LABEL(K1+1:K1+6),'(I6)')KK  

         ! ncep output from ss2gg has 'linear' with this formatting 8-3-07
           READ(LABEL(K1+8:K1+13),'(A)')LEV 
           IF(LEV.NE.'levels')THEN
              OPEN(20,FILE=ABFILE)

            ! number of columns, number of levels
              READ(20,*)NC,NL
                IF(KK.NE.NL)THEN
                   WRITE(*,*)'ERROR - Number of levels in control file',KK
                   WRITE(*,*)'        and ABfile are not equal',NL
                   STOP 998
                END IF
                IF(LEVELS.GT.KK)THEN
                   WRITE(*,*)'ERROR - Number of levels is greater than available',LEVELS,KK
                   STOP 999
                END IF
 
            ! read a,b; convert to 'sigma' (a is interger part; b is decimal)
              DO K=1,LEVELS
                 READ(20,*)A,B

               ! ncep global_hyblev.l64.txt 1st level a=0 b=1.0
               ! force it to be a sigma value instead of a pressure value
                 IF(K.EQ.1.AND.A.EQ.0.0.AND.B.EQ.1.0) B=0.9999

               ! convert Pascals to hectoPascals  
                 A=NINT(A/100.0)    

               ! 5 significant digits if a(hPa) > 1.0, else 4
                 IF(A.GT.100.)THEN
                    B=NINT(B*100.)/100.0
                 ELSEIF(A.GT.10.)THEN
                    B=NINT(B*1000.)/1000.0
                 ELSEIF(A.GT.1.)THEN
                    B=NINT(B*10000.)/10000.0
                 ELSE
                    B=NINT(B*10000.)/10000.0
                 END IF
     
                 SIGMA(K)=A+B
                !write(*,'(A,1X,I2,1X,F9.4)')'k,sigma:',k,sigma(k)
              END DO
              CLOSE(20)
              IVC=4			! vertical coordinate hybrid 

           ELSE
              ALLOCATE (ZVALS(KK))
              READ(10,'(5F12.6)')ZVALS
              SIGMA=ZVALS(1:LEVELS)
              DEALLOCATE (ZVALS)
              IVC=1			! vertical coordinate sigma
           END IF

        CASE ('tdef')
           READ(LABEL(K1+1:K1+6),'(I6)')KK
           IF(KK.GT.1)THEN
              WRITE(*,*)'Only one time period supported: ',KK
              STOP
           END IF
           READ(LABEL(19:20),'(I2)')IH
           READ(LABEL(22:23),'(I2)')ID
           READ(LABEL(24:26),'(A3)')MON
           READ(LABEL(27:30),'(I4)')IY
           IM=0
           DO I=1,12
              IF(MONTH(I).EQ.MON)IM=I
           END DO
           IY=IY-2000

        CASE ('vars')
           READ(LABEL(K1+1:K1+6),'(I6)')NUMVAR
           NUMB=0
           DO KK=1,NUMVAR
              READ(10,'(A)',IOSTAT=KRET)LABEL
              IF(KRET.NE.0)EXIT recloop
              READ(LABEL(5:7),'(I3)')NUML(KK)
              DO I=1,NVAR
                 IF(LABEL(1:3).EQ.GVAR(I))THEN
                    NUMB(KK)=I
                 END IF
              END DO
           END DO

     END SELECT
  END DO recloop
  CLOSE(10)

END SUBROUTINE gradcntl
