!##############################################################################
!$$$  MAIN PROGRAM DOCUMENTATION BLOCK
!
! MAIN PROGRAM: NMM2ARL      NONHYDROSTATIC MESOCALE MODEL FOR HYSPLIT
!   PRGMMR: DRAXLER          ORG: R/ARL       DATE: 2002-08-20
!
! ABSTRACT:  THIS CODE WRITTEN AT THE AIR RESOURCES LABORATORY ...
!     Converts output from the nonhydrostactic mesoscale model to hysplit
!     format. Input grib data are on the native model staggered Arakawa
!     e-grid and the hybrid vertical coordinate.  The program is designed
!     to process only one time period per execution.  Multiple input files
!     may be specified on the command line, each containing different
!     variables required for the specific time period.  Input data are
!     assumed to be already on an acceptable map projection for Hysplit.
!     Only packing to ARL format and units conversion is required.
!
! PROGRAM HISTORY LOG:
!   LAST REVISED: 20 Aug 2002 (RRD) - initial version
!                 25 Oct 2002 (RRD) - conforms to nmm sigma system
!                 23 Jan 2003 (RRD) - modified to handle 4km grids
!                 20 Jan 2004 (RRD) - named diagnostic message file
!
! USAGE:  NMM2ARL [GRIB_FILE_NAME] [GRIB_FILE_NAME2] [...]
!   INPUT ARGUMENT LIST:
!     GRIB_FILE_NAME - multiple grib files may be defined
!   OUTPUT ARGUMENT LIST:
!     NONE
!   INPUT FILES:
!     GRIB_DATA - grib input data files use special directIO routines
!   OUTPUT FILES:
!     unit 20 DATA.NMM - ARL packed data output file
!     unit 30 CFG_NMM  - output format configuration file
!     unit 50 nmm2arl.msg - diagnostic message file
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE:  IBM RS6000
!
!$$$

PROGRAM NMM2ARL

  IMPLICIT NONE

  LOGICAL       :: ftest           ! test for existence
  CHARACTER(80) :: fname           ! name of grib file 
  INTEGER       :: iargc,narg      ! command line arguments
  INTEGER       :: handle,fcopen   ! file i/o handle 
  INTEGER       :: krec = 0        ! output file rec counter
  INTEGER       :: kbyte = 0       ! grib file byte pointer
  INTEGER       :: kret            ! decoder return code

!---------------------------------------------------------------
  INTERFACE
  SUBROUTINE XTRACT(handle,krec,kbyte,kret)
  INTEGER,INTENT(IN)       :: HANDLE       ! IO handle
  INTEGER,INTENT(INOUT)    :: KREC         ! output record counter
  INTEGER,INTENT(INOUT)    :: KBYTE        ! input file byte counter
  INTEGER,INTENT(OUT)      :: KRET         ! termination code
  END SUBROUTINE xtract
  END INTERFACE
!---------------------------------------------------------------

! required for NCEP operational implementation
! CALL W3TAGB('NMM2ARL ',1998,0238,0070,'R/ARL')

! check for command line arguments
  NARG=IARGC()

  IF(NARG.EQ.0)THEN
     WRITE(*,*)'Converts output from the nonhydrostactic mesoscale model'
     WRITE(*,*)'Replaced by nams2arl'
     WRITE(*,*)'Usage: nmm2arl [file_1] [file_2] [...]'
     STOP
  END IF

! diagnostic message output file
  OPEN(50,FILE='nmm2arl.msg')

! process each data file
  DO WHILE (NARG.GT.0)
     CALL GETARG(narg,fname)
     INQUIRE(file=fname,exist=ftest)

     IF(FTEST)THEN
        KBYTE=0
        HANDLE=FCOPEN(fname,'r')
        WRITE(*,*)'Started processing: ',fname
        CALL XTRACT(handle,krec,kbyte,kret)
        IF(kret.ne.0)WRITE(*,*)'Error from XTRACT: ',kret
        CALL FCCLOS(handle,*900)
900     CONTINUE     
     ELSE    
        WRITE(*,*)'File not found:',fname
     END IF   

!    close out time period by writing the index record
     CALL PAKNDX(20)

!    process next file
     NARG=NARG-1   
  END DO 

! close output file 
  CLOSE (20)

! required for NCEP operational implementation
! CALL W3TAGE('NMM2ARL ')

END PROGRAM nmm2arl


!##############################################################################
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM:  EXTRACT          MAIN ROUTINE FOR DECODING ECM GRIB DATA
!   PRGMMR:    ROLAND DRAXLER   ORG: R/ARL       DATE:2002-08-21
!
! ABSTRACT:  THIS CODE WRITTEN AT THE AIR RESOURCES LABORATORY ...
!            EXTRACTS ONE RECORD FROM INPUT GRIB FILE AND ADDS ONE
!            RECORD TO THE OUTPUT FILE CENTERED ABOUT INDICATED LAT/LON
!
! PROGRAM HISTORY LOG:
!   LAST REVISED: 21 Aug 2002 (RRD) - initial version from grib2arl.f
!                 25 Feb 2003 (RRD) - ichar function replacement
!
! USAGE:  CALL XTRACT(HANDLE,KREC,KBYTE,KRET)
!   INPUT ARGUMENT LIST:  see below
!   OUTPUT ARGUMENT LIST: none
!   INPUT FILES:          none
!   OUTPUT FILES:         none
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE:  IBM RS6000
!
!$$$

SUBROUTINE XTRACT(HANDLE,KREC,KBYTE,KRET)

  IMPLICIT NONE

! argument list variables  
  INTEGER,INTENT(IN)       :: HANDLE       ! IO handle
  INTEGER,INTENT(INOUT)    :: KREC         ! output record counter
  INTEGER,INTENT(INOUT)    :: KBYTE        ! input file byte counter
  INTEGER,INTENT(OUT)      :: KRET         ! termination code

  INTEGER                  :: NUMPTS       ! extract grid size
  REAL                     :: CLAT, CLON   ! center position
  INTEGER                  :: KLVLS        ! number of output levels
  INTEGER                  :: ZERO = 0     ! initialize output file
  INTEGER                  :: SFCP         ! use surface pressure flag
  REAL                     :: offset       ! upper level pressure offset

! temporary holding variable
  INTEGER      :: VGRIB
  CHARACTER(4) :: VCHAR
  REAL         :: CNVRT

! arrays for grib description
  INTEGER      :: KPDS(25)              ! product definition
  INTEGER      :: KGDS(25)              ! grid definition
  INTEGER      :: KPTR(25)              ! section lengths
  INTEGER      :: SHAPE(2)              ! two dimensional shape

! sfc arrays to hold grib, character, and level information
  INTEGER,ALLOCATABLE      :: VGRIB0(:) ! sfc grib ID  
  INTEGER,ALLOCATABLE      :: STYP  (:) ! sfc variable code 
  INTEGER,ALLOCATABLE      :: SIG0  (:) ! sfc level code 
  CHARACTER(4),ALLOCATABLE :: VCHAR0(:) ! arl variable string
  REAL,ALLOCATABLE         :: CNVRT0(:) ! units conversion factor

! 3d arrays to hold grib, character, and level information
  INTEGER,ALLOCATABLE      :: VGRIB1(:) ! 3d grib variable code
  CHARACTER(4),ALLOCATABLE :: VCHAR1(:) ! arl variable string
  REAL,ALLOCATABLE         :: CNVRT1(:) ! units conversion factor
  REAL,ALLOCATABLE         :: SIGL  (:) ! input/output levels

! input data buffer and bit map section
  CHARACTER(1),ALLOCATABLE :: BUFF(:)
  LOGICAL(1)               :: KBMS(1)

! raw input and remapped input arrays
  REAL,ALLOCATABLE         :: RVAR(:)   ! one dimensional space
  REAL,ALLOCATABLE         :: MVWP(:,:) ! mass variable at wind point
  REAL,ALLOCATABLE         :: MVMP(:,:) ! mass variable at wind point
  REAL,ALLOCATABLE         :: WVMP(:,:) ! wind variable at mass point
  REAL,ALLOCATABLE         :: WVWP(:,:) ! wind variable at mass point

! latitude and longitudes for input array 
  REAL,ALLOCATABLE         :: TLAT(:,:),TLON(:,:)

! output array to indicate if wind (T) or mass (F) variable
  LOGICAL ,ALLOCATABLE     :: WIND(:,:)
! input grid point value on the output grid
  INTEGER, ALLOCATABLE     :: IGP(:,:),JGP(:,:)

! packed output array
  REAL,        ALLOCATABLE :: SVAR(:,:)      
  CHARACTER(1),ALLOCATABLE :: CVAR(:)
  CHARACTER(4)             :: model
  CHARACTER(80)            :: fname 
  LOGICAL                  :: ftest 

! mapping variables that define the output grid  
  REAL                     :: parmap(9),gridkm

  INTEGER :: kij,iwp,jwp,imp,jmp 
  INTEGER :: iyr,imo,ida,ihr,iy0,im0,id0,ih0
  INTEGER :: i,j,k,ifh,imn
  INTEGER :: kvc,nlon,n2dv,nlvl,nlat,n3dv
  INTEGER :: nx,ny,nz,kl,kv,klen,lrec,nxy,nxp,nyp

  REAL    :: dummy(1)
  REAL    :: clon1,clat1,clat2,clon2
  REAL    :: TPH0D,TLM0D,DLMD,DPHD
  REAL    :: tlat1,tlon1,tlat2,tlon2,dist1,dist2
  REAL    :: x1,y1,x2,y2,x3,y3,x4,y4,x5,y5,x6,y6

  SAVE VGRIB0,VCHAR0,CNVRT0,VGRIB1,VCHAR1,CNVRT1,SIG0,STYP,       &
       N2DV,N3DV,NXP,NYP,NXY,SHAPE,TLAT,TLON,RVAR,MVMP,MVWP,      &
       WVWP,WVMP,WIND,IGP,JGP,BUFF,CVAR,SVAR,NLVL

  COMMON /STAGEG/ NLON,NLAT,TPH0D,TLM0D,DLMD,DPHD

!-------------------------------------------------------------------------------
! When dealing with some F90 compilers, replace ICHAR below with JCHAR function
  CHARACTER(1)        :: mychr
  INTEGER             :: jchar
  JCHAR(MYCHR)=IAND(ICHAR(MYCHR),255)
!-------------------------------------------------------------------------------

  INTERFACE

  SUBROUTINE MAKNDX (N2DV,N3DV,VCHAR0,VCHAR1,SIGL,NXP,NYP,NZP,   &
                     CLAT2,CLON2,GRIDKM,KVC,MODEL,OFFSET,FNAME)
  IMPLICIT NONE
  INTEGER,      INTENT(IN)   :: n2dv          ! number sfc variables 
  INTEGER,      INTENT(IN)   :: n3dv          ! number of 3d variables
  CHARACTER(4), INTENT(IN)   :: vchar0 (:)    ! sfc variable id
  CHARACTER(4), INTENT(IN)   :: vchar1 (:)    ! 3d variable id
  REAL,         INTENT(IN)   :: sigl   (:)    ! level information
  INTEGER,      INTENT(IN)   :: nxp           ! x dimension
  INTEGER,      INTENT(IN)   :: nyp           ! y dimension
  INTEGER,      INTENT(IN)   :: nzp           ! z dimension
  REAL,         INTENT(IN)   :: clat2         ! corner grid lat
  REAL,         INTENT(IN)   :: clon2         ! corner grid lon
  REAL,         INTENT(IN)   :: gridkm        ! spacing 
  INTEGER,      INTENT(IN)   :: kvc           ! vertical coordinate index
  CHARACTER(4), INTENT(IN)   :: model         ! meteorological model
  REAL,         INTENT(IN)   :: OFFSET        ! pressure where levels isobaric
  CHARACTER(80),INTENT(IN)   :: fname 
  END SUBROUTINE makndx

  SUBROUTINE PAKSET(LUNIT,FNAME,KREC1,NXP,NYP,NZP)
  IMPLICIT NONE
  INTEGER,       INTENT(IN)    :: lunit     ! output unit number
  CHARACTER(*),  INTENT(INOUT) :: fname     ! file name of METDATA.CFG
  INTEGER,       INTENT(IN)    :: krec1     ! position of index record at time-1
  INTEGER,       INTENT(OUT)   :: nxp, nyp  ! horizontal grid dimensions
  INTEGER,       INTENT(OUT)   :: nzp       ! vertical grid dimension (incl sfc)
  END SUBROUTINE pakset

  SUBROUTINE PAKREC(LUNIT,RVAR,CVAR,NX,NY,NXY,KVAR,IY,IM,ID,IH,MN,IC,LL,KINI)
  IMPLICIT NONE
  INTEGER,      INTENT(IN)  :: LUNIT       ! output unit number
  INTEGER,      INTENT(IN)  :: NX,NY       ! dimensions of RVAR
  INTEGER,      INTENT(IN)  :: NXY         ! dimensions of CVAR
  REAL,         INTENT(IN)  :: RVAR(NX,NY) ! input data to be packed
  CHARACTER(1), INTENT(OUT) :: CVAR(NXY)   ! packed data array
  CHARACTER(4), INTENT(IN)  :: KVAR        ! descriptor of variable written
  INTEGER,      INTENT(IN)  :: IY,IM,ID    ! date identification
  INTEGER,      INTENT(IN)  :: IH,MN       ! time identification (MN-minutes)
  INTEGER,      INTENT(IN)  :: IC          ! forecast hour, ICX hour for >99
  INTEGER,      INTENT(IN)  :: LL          ! level indicator 
  INTEGER,      INTENT(IN)  :: KINI        ! initialization (0-no 1-yes)
  END SUBROUTINE pakrec

  SUBROUTINE W3FI63(BUFF,KPDS,KGDS,KBMS,RVAR,KPTR,KRET)
  CHARACTER(1), INTENT(IN)  :: BUFF(:)
  LOGICAL(1),   INTENT(OUT) :: KBMS(:)
  INTEGER,      INTENT(OUT) :: KPDS(:)
  INTEGER,      INTENT(OUT) :: KGDS(:)
  REAL,         INTENT(OUT) :: RVAR(:)
  INTEGER,      INTENT(OUT) :: KPTR(:)
  INTEGER,      INTENT(OUT) :: KRET
  END SUBROUTINE w3fi63

  SUBROUTINE W3FI00(BUFF,KPDS,KGDS,KBMS,RVAR,KPTR,KRET)
  CHARACTER(1), INTENT(IN)  :: BUFF(:)
  LOGICAL(1),   INTENT(OUT) :: KBMS(:)
  INTEGER,      INTENT(OUT) :: KPDS(:)
  INTEGER,      INTENT(OUT) :: KGDS(:)
  REAL,         INTENT(OUT) :: RVAR(:)
  INTEGER,      INTENT(OUT) :: KPTR(:)
  INTEGER,      INTENT(OUT) :: KRET
  END SUBROUTINE w3fi00

  SUBROUTINE SIGMA (SIGL,NLVL,OFFSET)
  REAL,    INTENT(OUT) :: SIGL(:)     ! mid point sigma levels 1 = bottom
  INTEGER, INTENT(IN)  :: NLVL        ! number of sigma levels to ouput
  REAL,    INTENT(OUT) :: OFFSET      ! pressure where levels isobaric
  END SUBROUTINE sigma
  END INTERFACE

!-------------------------------------------------------------------------------
! read all the bytes corresponding to one grib record
!-------------------------------------------------------------------------------

! inital dummy allocation to permit reading of grib record length
  IF(.NOT.ALLOCATED(BUFF)) ALLOCATE (BUFF(8),STAT=KRET)

100 CONTINUE 

!   load the indicator section to determine record length
    CALL FCPTPS(HANDLE,KBYTE,*920)
    CALL FCREAD(HANDLE,BUFF,1,8,*900)
    IF(BUFF(1)//BUFF(2)//BUFF(3)//BUFF(4).NE.'GRIB')THEN
       KBYTE=KBYTE+1
       GOTO 100
    END IF
    KLEN=JCHAR(BUFF(7))+JCHAR(BUFF(6))*256+JCHAR(BUFF(5))*65536

!###alternate NCEP library routine
!###INTEGER             :: mova2i
!###KLEN=mova2i(BUFF(7))+mova2i(BUFF(6))*256+mova2i(BUFF(5))*65536

!   reallocate buffer if necessary
    IF(KLEN.GT.SIZE(BUFF,1))THEN
       DEALLOCATE (BUFF,STAT=KRET)
       ALLOCATE (BUFF(KLEN),STAT=KRET)
       IF(KRET.NE.0) WRITE(*,*)'Buffer allocation error: ',KRET,KLEN
       WRITE(50,*)'Buffer allocation: ',KLEN
    END IF

!   read the entire data record
    CALL FCPTPS(HANDLE,KBYTE,*920)
    CALL FCREAD(HANDLE,BUFF,1,KLEN,*900)

!   call the nmc light grib unpacker (does not decode array)
    CALL W3FI00(BUFF,KPDS,KGDS,KBMS,DUMMY,KPTR,KRET)
    IF(KRET.NE.0) WRITE(*,*)'Error W3FI00: ',KRET

!-------------------------------------------------------------------------------
! check the times
!-------------------------------------------------------------------------------

!   century fix
    KPDS(8)=MOD(KPDS(8),100)

!   set the initialization time
    IYR=KPDS(8)
    IMO=KPDS(9)
    IDA=KPDS(10)
    IHR=KPDS(11)
    IMN=KPDS(12)

!   forecast time 
    IFH=MAX(KPDS(14),KPDS(15))

!   compute the current time
    CALL TMPLUS(IYR,IMO,IDA,IHR,IFH)

!   save the time for testing
    IF(KREC.EQ.0)THEN
       IY0=IYR
       IM0=IMO
       ID0=IDA
       IH0=IHR
    ELSE
       IF(IY0.NE.IYR.OR.IM0.NE.IMO.OR.    &
          ID0.NE.IDA.OR.IH0.NE.IHR)GOTO 910 
    END IF

!-------------------------------------------------------------------------------
! determine the positions of each input data grid the first time prior to
! any data processing and then define the output grid based upon the input grid
!-------------------------------------------------------------------------------

    IF(.NOT.ALLOCATED(TLAT).OR..NOT.ALLOCATED(TLON))THEN

! horizontal input grid for grib data  

       NLON=KGDS(2)    ! number of I points
       NLAT=KGDS(3)    ! number of J points
       SHAPE(1)=NLON
       SHAPE(2)=NLAT

!      the real data array (xvar) and the dummy array for input (rvar)
       ALLOCATE (RVAR(NLON*NLAT),STAT=KRET)
       IF(KRET.NE.0) WRITE(*,*)'RVAR allocation error: ',KRET,NLON,NLAT
       ALLOCATE (MVMP(NLON,NLAT),STAT=KRET)
       IF(KRET.NE.0) WRITE(*,*)'MVMP allocation error: ',KRET,NLON,NLAT
       ALLOCATE (MVWP(NLON,NLAT),STAT=KRET)
       IF(KRET.NE.0) WRITE(*,*)'MVWP allocation error: ',KRET,NLON,NLAT
       ALLOCATE (WVWP(NLON,NLAT),STAT=KRET)
       IF(KRET.NE.0) WRITE(*,*)'WVWP allocation error: ',KRET,NLON,NLAT
       ALLOCATE (WVMP(NLON,NLAT),STAT=KRET)
       IF(KRET.NE.0) WRITE(*,*)'WVMP allocation error: ',KRET,NLON,NLAT
       WRITE(50,*)'Input mass and wind array allocation: ',NLON,NLAT

!      the lat-lon array that identifies each input grid location
       ALLOCATE (TLAT(NLON,NLAT),STAT=KRET)
       IF(KRET.NE.0) WRITE(*,*)'Allocate Error TLAT: ',KRET,NLON,NLAT
       ALLOCATE (TLON(NLON,NLAT),STAT=KRET)
       IF(KRET.NE.0) WRITE(*,*)'Allocate Error TLON: ',KRET,NLON,NLAT

!      input grid center latitude(7) longitude(8)
       TPH0D=KGDS(7)/1000.0
       TLM0D=KGDS(8)/1000.0
       WRITE(50,*)'Input grid center: ',TPH0D,TLM0D 

!      grid spacing in degrees: force a value for an exact match 
       DLMD=KGDS(9)/1000.0
       DPHD=KGDS(10)/1000.0
       GRIDKM=SQRT(DPHD**2.0+DLMD**2.0)*111.1984
       WRITE(50,*)'Input  grid resolution: ',GRIDKM

!      grid spacing in degrees: force a value for an exact match 
       IF(NINT(GRIDKM).EQ.8)THEN
!         applies to the 8-km meso model grid domains (except Alaska)
          DLMD=24.0/449.0
          DPHD=1.0/19.0 
       ELSEIF(NINT(GRIDKM).EQ.4)THEN
!         applies to the 4-km meso model grid domains 
          DLMD=24.0/898.0
          DPHD=1.0/38.0 
       ELSE  
          WRITE(*,*)'ERROR: undefined grid resolution'
          STOP
       END IF

!      define the location of grid mass points
       DO J=1,NLAT
       DO I=1,NLON
!         lat-lon of the mass points ... the mass point corresponds
!         to the (1,1) grid point on the lower left and this grid will
!         be used to set up the output grid
          CALL GMP2LL(I,J,TLAT(I,J),TLON(I,J))
       END DO
       END DO

! define the output grid

!      temporary variable for grid center
       NXP=(NLON+1)/2
       NYP=(NLAT+1)/2

!      define grid spacing at grid center 
       CLAT2=TLAT(NXP,NYP+1)
       CLAT1=TLAT(NXP,NYP-1)
       GRIDKM=0.50*(CLAT2-CLAT1)*111.1984
       WRITE(50,*)'Output grid resolution: ',GRIDKM

!      initially define the output grid at central location
       CLAT2=TLAT(NXP,NYP)
       CLON2=TLON(NXP,NYP)
       CALL STLMBR(PARMAP,0.0,CLON2)       ! force mercator projection 
       CALL STCM1P(PARMAP,500.0,500.0,CLAT2,CLON2,CLAT2,CLON2,GRIDKM,0.0)
       WRITE(50,*)'Grid center lat & long: ',CLAT2,CLON2 

!        XY4         XY6        XY2
!         x----------------------x
!          \     ----------     /
!           \    |        |    /
!            \   |        |   /
!             \  ----------  /
!              x------------x
!             XY1    XY5   XY3

!      determine the grid departure from a rectangle, to be able to define a
!      the rectangular output grid within the data grid 
       CALL CLL2XY(PARMAP,TLAT(NLON,1),   TLON(NLON,1),   X1,Y1)
       CALL CLL2XY(PARMAP,TLAT(NLON,NLAT),TLON(NLON,NLAT),X4,Y4)
       KIJ=MAX(2,INT((X1-X4)/2.0))  ! number of grid points excluded 

!      find the grid limits that are within the input data domain 
       CALL CLL2XY(PARMAP,TLAT(NLON-KIJ,KIJ+1),   TLON(NLON-KIJ,KIJ+1),   X1,Y1)
       CALL CLL2XY(PARMAP,TLAT(KIJ+1,NLAT-KIJ),   TLON(KIJ+1,NLAT-KIJ),   X2,Y2)
       CALL CLL2XY(PARMAP,TLAT(KIJ+1,KIJ+1),      TLON(KIJ+1,KIJ+1),      X3,Y3)
       CALL CLL2XY(PARMAP,TLAT(NLON-KIJ,NLAT-KIJ),TLON(NLON-KIJ,NLAT-KIJ),X4,Y4)
       X1=MAX(X1,X4)
       X2=MIN(X2,X3)

       CALL CLL2XY(PARMAP,TLAT(NXP,2*KIJ+1), TLON(NXP,2*KIJ+1), X5,Y5)
       CALL CLL2XY(PARMAP,TLAT(NXP,NLAT-KIJ),TLON(NXP,NLAT-KIJ),X6,Y6)
       Y1=MAX(Y1,Y5,Y3)
       Y2=MIN(Y2,Y4,Y6)

!      set up the output grid system
       NXP=NINT(X2-X1)+1
       NYP=NINT(Y2-Y1)+1

!      redefine so that lower left corner point is 1,1
       CALL STLMBR(PARMAP,0.0,CLON2)       ! force mercator projection 
       CALL STCM1P(PARMAP,(NXP+1.0)/2.0,(NYP+1.0)/2.0,CLAT2,CLON2,  &
                   CLAT2,CLON2,GRIDKM,0.0)

! find the nearest input grid point to every output grid point

       ALLOCATE (WIND(NXP,NYP),IGP(NXP,NYP),JGP(NXP,NYP),STAT=KRET)
       IF(KRET.NE.0) WRITE(*,*)'Output grid allocation error: ',KRET,NXP,NYP
       WRITE(50,*)'Output grid allocation: ',NXP,NYP

       WIND=.FALSE.
       IGP=0
       JGP=0

       DO J=1,NYP
       DO I=1,NXP
          CALL CXY2LL(PARMAP,FLOAT(I),FLOAT(J),CLAT,CLON)
          CALL LL2GWP(CLAT,CLON,IWP,JWP)
          CALL LL2GMP(CLAT,CLON,IMP,JMP)
          CALL GWP2LL(IWP,JWP,TLAT1,TLON1)
          CALL GMP2LL(IMP,JMP,TLAT2,TLON2)
          DIST1=(TLAT1-CLAT)**2.0+((TLON1-CLON)*COS(CLAT/57.3))**2.0
          DIST2=(TLAT2-CLAT)**2.0+((TLON2-CLON)*COS(CLAT/57.3))**2.0
          IF(DIST1.LT.DIST2)THEN
             WIND(I,J)=.TRUE.
             IGP(I,J)=MIN(MAX(IWP,1),NLON)
             JGP(I,J)=MIN(MAX(JWP,1),NLAT)
          ELSE
             IGP(I,J)=MIN(MAX(IMP,1),NLON)
             JGP(I,J)=MIN(MAX(JMP,1),NLAT)
          END IF
       END DO
       END DO

       WRITE(50,*)'J grid range: ',KIJ,MINVAL(JGP),MAXVAL(JGP),(NLAT-KIJ+1)
       WRITE(50,*)'I grid range: ',KIJ,MINVAL(IGP),MAXVAL(IGP),(NLON-KIJ+1)
       IF(MINVAL(IGP).LT.KIJ     .OR.MINVAL(JGP).LT.KIJ.OR.   &
          MAXVAL(IGP).GT.NLON-KIJ+1.OR.MAXVAL(JGP).GT.NLAT-KIJ+1)THEN
          WRITE(*,*)'Output grid exceeds input (in-min out-min out-max in-max)'
          WRITE(*,*)'J grid range: ',KIJ,MINVAL(JGP),MAXVAL(JGP),(NLAT-KIJ+1)
          WRITE(*,*)'I grid range: ',KIJ,MINVAL(IGP),MAXVAL(IGP),(NLON-KIJ+1)
          STOP
       END IF
    END IF

!-------------------------------------------------------------------------------
! determine the variables and levels for processing and allocate the remaining
! array space for the packing and output grid interpolation requirements
!-------------------------------------------------------------------------------

    IF(.NOT.ALLOCATED(CVAR))THEN

       NLVL=40          ! number of output levels 
       N2DV= 5          ! number of 2D variables
       N3DV= 5          ! number of 3D variables
       MODEL='NMM'

!      surface variables
       ALLOCATE (VGRIB0(N2DV),STYP(N2DV),SIG0(N2DV),VCHAR0(N2DV),              &
                 CNVRT0(N2DV),STAT=KRET)
       IF(KRET.NE.0) WRITE(*,*)'2-dim variable allocation error: ',KRET,N2DV

!      upper level variables
       ALLOCATE (VGRIB1(N3DV),VCHAR1(N3DV),CNVRT1(N3DV))
       IF(KRET.NE.0) WRITE(*,*)'3-dim variable allocation error: ',KRET,N3DV

!      model sigma levels
       ALLOCATE (SIGL(NLVL))
       IF(KRET.NE.0) WRITE(*,*)'Level variable allocation error: ',KRET,NLVL
!      DO K=1,NLVL
!         SIGL(K)=1.0-FLOAT(K)/60.0
!      END DO

!      load sigma levels
       CALL SIGMA (SIGL,NLVL,OFFSET)

!      surface variable definitions 
       VGRIB0 = (/    1,    61,    33,    34,    11 /)  
       SIG0   = (/    0,     0,    10,    10,     2 /)
       STYP   = (/    1,     1,   105,   105,   105 /)
       VCHAR0 = (/'PRSS','TPP6','U10M','V10M','T02M'/)
       CNVRT0 = (/ 0.01  ,.001 ,  1.0  , 1.0  , 1.0 /)

!      upper level variable definitions
       VGRIB1 = (/   11,    33,    34,    39,    51 /)
       VCHAR1 = (/'TEMP','UWND','VWND','WWND','SPHU'/)
       CNVRT1 = (/  1.0,   1.0,   1.0,  0.01,   1.0 /)

!      vertical coordinate system 
!      kvc = 1(sigma); 2(pressure); 3(height); 4(hybrid)
       KVC=1 

!      reserve space for output data arrays
       NXY=NXP*NYP
       ALLOCATE (CVAR(NXY),SVAR(NXP,NYP),STAT=KRET)
       IF(KRET.NE.0) WRITE(*,*)'Output array allocation error: ',KRET,NXY,NXP,NYP

!      create the configuration file 
       FNAME='CFG_NMMHH'
       WRITE(FNAME(8:9),'(I2.2)')IFH
       CALL MAKNDX(N2DV,N3DV,VCHAR0,VCHAR1,SIGL,NXP,NYP,NLVL,   &
            CLAT2,CLON2,GRIDKM,KVC,MODEL,OFFSET,FNAME)

!      configure the packing routines
       CALL PAKSET(20,FNAME,1,NX,NY,NZ)
       WRITE(50,*)'Set grid from pakset: ',nx,ny,nz

!      standard output name for packed data
       FNAME='DATA.NMMHH'
       WRITE(FNAME(9:10),'(I2.2)')IFH
       INQUIRE(FILE=FNAME,EXIST=FTEST)
       LREC=NXY+50
       OPEN(20,FILE=FNAME,RECL=LREC,ACCESS='DIRECT',FORM='UNFORMATTED')

!      diagnostic output labels 
       WRITE(50,'(A)')'   Lev  Var  Var Sigma  Forecast'
    END IF

!-------------------------------------------------------------------------------
! determine if this grib record will be converted to ARL packed format
!-------------------------------------------------------------------------------

!   check if 2d variable present in selection table
    KL=0
    DO KV=1,N2DV 
       VGRIB=VGRIB0(KV)
       VCHAR=VCHAR0(KV)
       CNVRT=CNVRT0(KV)

!      matches id and special level indicator
       IF(KPDS(5).EQ.VGRIB.AND.KPDS(7).EQ.SIG0(KV).AND.    &
          STYP(KV).EQ.KPDS(6))GOTO 300
    END DO

!   then check for 3d variable
    DO KV=1,N3DV 
       VGRIB=VGRIB1(KV)
       VCHAR=VCHAR1(KV)
       CNVRT=CNVRT1(KV)
       IF(KPDS(5).EQ.VGRIB)GO TO 200
    END DO
    GO TO 800

200 CONTINUE

!   check if 3d level is present in selection table
    IF(KPDS(1).EQ.7)THEN
!      noaa-ncep center
       DO KL=1,NLVL
          IF(KPDS(7).EQ.KL)GO TO 300
       END DO
    END IF

!   if all tests fail go and read next grib record
!   WRITE(50,*)'Level match not found: ',VCHAR,'   KPDS(5,6,7): ',KPDS(5:7)
    GO TO 800

!-------------------------------------------------------------------------------
! grib variable matches output list ... continue with processing
!-------------------------------------------------------------------------------

300 CONTINUE

!   load the entire grib data record into the buffer
    CALL W3FI63(BUFF,KPDS,KGDS,KBMS,RVAR,KPTR,KRET)
    IF(KRET.NE.0)THEN
       WRITE(50,*)'Error W3FI63: ',KRET
       STOP 210
    END IF

!   units conversion
    IF(CNVRT.GE.0.0)THEN
       IF(CNVRT.NE.1.0) RVAR=RVAR*CNVRT
    ELSE
!      less than zero conversion indicates log of parameter
       IF(CNVRT.LT.0.0) RVAR=EXP(RVAR)*ABS(CNVRT)
    END IF

!-------------------------------------------------------------------------------
! interpolate mass points and wind points ... grid runs east to west
! and for the equivalent grid point the mass grid is shifted to the
! east, hence for a given i,j the mass point is along the same j
!
!             Wi+1,j+1    Ti+1,j+1  Wi,j+1     Ti,j+1
! Wi+1,j      Ti+1,j      Wi,j      Ti,j       Wi-1,j    Ti-1,j
!             Wi+1,j-1    Ti+1,j-1  Wi,j-1     Ti,j-1
!-------------------------------------------------------------------------------

    IF(VCHAR(1:1).EQ.'U'.OR.VCHAR(1:1).EQ.'V')THEN
!      load wind variable data into wind point array
       WVWP=RESHAPE(RVAR,SHAPE)

!      interpolate wind variable to mass points
       DO J=2,(NLAT-1)
       DO I=2,(NLON-1)
          WVMP(I,J)=(WVWP(I,J)+WVWP(I-1,J)+WVWP(I,J+1)+WVWP(I,J-1))/4.0   
       END DO     
       END DO

!      find value for output grid from nearest mass or wind point
       DO J=1,NYP
       DO I=1,NXP
          IF(WIND(I,J))THEN
             SVAR(NXP-I+1,J)=WVWP(IGP(I,J),JGP(I,J))
          ELSE
             SVAR(NXP-I+1,J)=WVMP(IGP(I,J),JGP(I,J))
          END IF
       END DO
       END DO

    ELSE
!      load mass variable data into mass point array
       MVMP=RESHAPE(RVAR,SHAPE)

!      interpolated mass variable to wind points 
       DO J=2,(NLAT-1)
       DO I=1,(NLON-1)
          MVWP(I,J)=(MVMP(I,J)+MVMP(I+1,J)+MVMP(I+1,J+1)+MVMP(I+1,J-1))/4.0   
       END DO     
       END DO

!      find value for output grid from nearest mass or wind point
       DO J=1,NYP
       DO I=1,NXP
          IF(WIND(I,J))THEN
             SVAR(NXP-I+1,J)=MVWP(IGP(I,J),JGP(I,J))
          ELSE
             SVAR(NXP-I+1,J)=MVMP(IGP(I,J),JGP(I,J))
          END IF
       END DO
       END DO
    END IF

!   projection forced as mercator, therefore no wind rotation required
!   if projection changes add rotation here prior to writing

!   pack into ARL format and continue
    WRITE(50,'(1X,2I5,1X,A,1X,3I5)')KL,KV,VCHAR,KPDS(7),KPDS(14),KPDS(15)
    CALL PAKREC(20,SVAR,CVAR,NXP,NYP,NXY,VCHAR,IYR,IMO,IDA,IHR,IMN,IFH,        &
               (KL+1),ZERO)
    KREC=KREC+1

!-------------------------------------------------------------------------------
! position to the next grib record
!-------------------------------------------------------------------------------

800 KBYTE=KBYTE+KLEN
    GO TO 100

!   normal end of file
900 KRET=0    
    RETURN   

!   next-time period
910 KRET=1    
    RETURN   

!   end-of-input
920 KRET=2

END SUBROUTINE xtract


!##############################################################################
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM:  MAKNDX           CREATE THE INDEX RECORD CONFIGURATION FILE
!   PRGMMR:    ROLAND DRAXLER   ORG: R/ARL       DATE:96-06-01
!
! ABSTRACT:  THIS CODE WRITTEN AT THE AIR RESOURCES LABORATORY ...
!            CREATES THE CONFIGURATION FILE FOR THE OUTPUT DATA WHICH
!            DEFINES THE GRID SYSTEM AS WELL AS ALL VARIABLES THAT ARE
!            TO BE WRITTEN TO THE OUTPUT FILE.
!
! PROGRAM HISTORY LOG:
!   LAST REVISED: 03 Feb 1998 (RRD) - mercator fix, added lambert
!                 15 Nov 2001 (RRD) - dynamic array allocation
!                 20 Feb 2002 (RRD) - ecmwf compatibility
!                 25 Oct 2002 (RRD) - pressure offset in std sigma system
!
! USAGE:  CALL MAKNDX(N2DV,N3DV,VCHAR0,VCHAR1,LEVEL,NXP,NYP,NZP, 
!                     CLAT2,CLON2,GRIDKM,NLAT,NLON,OFFSET)
!   INPUT ARGUMENT LIST:    see below
!   OUTPUT ARGUMENT LIST:   see below
!   INPUT FILES:            NONE
!   OUTPUT FILES:           UNIT 30 CFG_ARL output file structure
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE:  IBM RS6000
!
!$$$

SUBROUTINE MAKNDX (N2DV,N3DV,VCHAR0,VCHAR1,SIGL,NXP,NYP,NZP,     &
                   CLAT2,CLON2,GRIDKM,KVC,MODEL,OFFSET,FNAME)

  IMPLICIT NONE

  INTEGER,      INTENT(IN)   :: n2dv          ! number sfc variables 
  INTEGER,      INTENT(IN)   :: n3dv          ! number of 3d variables
  CHARACTER(4), INTENT(IN)   :: vchar0 (:)    ! sfc variable id
  CHARACTER(4), INTENT(IN)   :: vchar1 (:)    ! 3d variable id
  REAL,         INTENT(IN)   :: sigl   (:)    ! level information
  INTEGER,      INTENT(IN)   :: nxp           ! x dimension
  INTEGER,      INTENT(IN)   :: nyp           ! y dimension
  INTEGER,      INTENT(IN)   :: nzp           ! z dimension
  REAL,         INTENT(IN)   :: clat2         ! corner grid lat
  REAL,         INTENT(IN)   :: clon2         ! corner grid lon
  REAL,         INTENT(IN)   :: gridkm        ! spacing 
  INTEGER,      INTENT(IN)   :: kvc           ! vertical coordinate index
  CHARACTER(4), INTENT(IN)   :: model         ! meteorological model
  REAL,         INTENT(IN)   :: offset        ! level where isobaric
  CHARACTER(80),INTENT(IN)   :: fname 
  
  CHARACTER(4)  :: VCHAR(50) ! variable id
  CHARACTER(20) :: LABEL(18) ! optional field label

  INTEGER       :: I,N,NL,MVAR  
  REAL          :: SIG
  REAL          :: GRIDS(12)

! optional field label string
  DATA LABEL/'Model Type:','Grid Numb:','Vert Coord:','Pole Lat:',             &
    'Pole Lon:','Ref Lat:','Ref Lon:','Grid Size:','Orientation:',             &
    'Cone Angle:','Sync X Pt:','Sync Y Pt:','Sync Lat:','Sync Lon:',           &
    'Reserved:','Numb X pt:','Numb Y pt:','Numb Levels:'/

! pole lat/lon is used to identify the point of the maximum index
  GRIDS(1)=0.0      ! force projection as mercator
  GRIDS(2)=CLON2 
! the reference lat/lon defines grid spacing  
  GRIDS(3)=CLAT2
  GRIDS(4)=CLON2
! delta=x grid size in km
  GRIDS(5)=GRIDKM 
! grid orientation
  GRIDS(6)=0.0
! tangent latitude
  GRIDS(7)=CLAT2
! sync x,y defines lower left grid point 
  GRIDS(8)=(NXP+1)/2
  GRIDS(9)=(NYP+1)/2
! lat/lon of lower left point
  GRIDS(10)=CLAT2
  GRIDS(11)=CLON2
! variable reserved for future use
! defines pressure offset in NMM hybrid system
  GRIDS(12)=OFFSET

! write the packer configuration file
  OPEN(30,FILE=FNAME)

! default grid number 99 (field not used)
  WRITE(30,'(A20,A4)')LABEL(1), MODEL
  WRITE(30,'(A20,A4)')LABEL(2),'  99'

! coordinate (1:sigma 2:pressure 3:terrain 4:hybrid)
  WRITE(30,'(A20,I4)') LABEL(3), KVC      

! grid geolocation parameters and projection
  DO I=1,12
     WRITE(30,'(A20,F10.2)')LABEL(I+3),GRIDS(I)
  END DO

! grid dimensions
  WRITE(30,'(A20,I4)') LABEL(16), NXP
  WRITE(30,'(A20,I4)') LABEL(17), NYP
  WRITE(30,'(A20,I4)') LABEL(18),(NZP+1)   

! upper level information
  DO NL=0,NZP   

     WRITE(LABEL(1),'(A6,I2,A1)')'Level ',NL,':'
     IF(NL.EQ.0)THEN
        SIG=1.0     
        IF(KVC.EQ.2)SIG=0.0
        MVAR=N2DV
        VCHAR(1:MVAR)=VCHAR0(1:MVAR)
     ELSE
        SIG=SIGL(NL)
        MVAR=N3DV
        VCHAR(1:MVAR)=VCHAR1(1:MVAR)
     END IF

     IF(SIG.LT.1.0)THEN
        WRITE(30,'(A20,F6.5,I3,10(1X,A4))')LABEL(1),SIG,MVAR,(VCHAR(N),N=1,MVAR)
     ELSEIF(SIG.GE.1.AND.SIG.LT.10.0)THEN
        WRITE(30,'(A20,F6.4,I3,10(1X,A4))')LABEL(1),SIG,MVAR,(VCHAR(N),N=1,MVAR)
     ELSEIF(SIG.GE.10.AND.SIG.LT.100.0)THEN
        WRITE(30,'(A20,F6.3,I3,10(1X,A4))')LABEL(1),SIG,MVAR,(VCHAR(N),N=1,MVAR)
     ELSEIF(SIG.GE.100.AND.SIG.LT.1000.0)THEN
        WRITE(30,'(A20,F6.2,I3,10(1X,A4))')LABEL(1),SIG,MVAR,(VCHAR(N),N=1,MVAR)
     ELSEIF(SIG.GE.1000)THEN
        WRITE(30,'(A20,F6.1,I3,10(1X,A4))')LABEL(1),SIG,MVAR,(VCHAR(N),N=1,MVAR)
     END IF

  END DO
  CLOSE (30) 

END SUBROUTINE makndx


!##############################################################################
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM:  MKGRID           DETERMINE THE LAT/LON OF EACH GRID POINT
!   PRGMMR:    ROLAND DRAXLER   ORG: R/ARL       DATE:96-06-01
!
! ABSTRACT:  THIS CODE WRITTEN AT THE AIR RESOURCES LABORATORY ...
!            GOES THROUGH EACH NODE OF THE OUTPUT GRID AND PLACES THE
!            LAT/LON VALUE OF THAT POINT INTO AN ARRAY
!
! PROGRAM HISTORY LOG:
!   LAST REVISED: 03 Feb 1999 (RRD) - mercator definition patch
!                 15 Nov 2001 (RRD) - dynamic array allocation 
!
! USAGE:  CALL MKGRID(NX,NY,TLAT,TLON)
!   INPUT ARGUMENT LIST:   see below
!   OUTPUT ARGUMENT LIST:  see below
!   INPUT FILES:           NONE
!   OUTPUT FILES:          NONE
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE:  IBM RS6000
!
!$$$

SUBROUTINE MKGRID(NX,NY,TLAT,TLON)

  IMPLICIT NONE

  INTEGER, INTENT(IN)  :: NX,NY      ! output grid dimensions
  REAL,    INTENT(OUT) :: TLAT(:,:)  ! position of nodes
  REAL,    INTENT(OUT) :: TLON(:,:)  ! position of nodes

  INTEGER              :: I,J
  REAL                 :: GRIDS(12), PARMAP(9)

  COMMON / SETUP / GRIDS, PARMAP

! define the tangent latitude and reference longitude
  CALL STLMBR(PARMAP,GRIDS(7),GRIDS(4))

! define the grid by a one-point specification
  CALL STCM1P(PARMAP,GRIDS(8),GRIDS(9),GRIDS(10),GRIDS(11),                    &
                     GRIDS(3),GRIDS(4),GRIDS(5),GRIDS(6))

! determine the lat/lon at the grid locations
  DO J=1,NY
  DO I=1,NX

!    cxy2ll returns -180 to +180
     CALL CXY2LL(PARMAP,FLOAT(I),FLOAT(J),TLAT(I,J),TLON(I,J))
!    shift to 0 to 360 system
     IF(TLON(I,J).LT.0.0)TLON(I,J)=360.0+TLON(I,J)

  END DO
  END DO

END SUBROUTINE mkgrid


!##############################################################################
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!$$$ 

SUBROUTINE GWP2LL(IV,JV,GLATD,GLOND)

! convert grid at wind point to latitude-longitude

  REAL, PARAMETER :: D2R=1.74532925E-2
  REAL, PARAMETER :: R2D=1./D2R

  COMMON /STAGEG/ IM,JM,TPH0D,TLM0D,DLMD,DPHD

  IMT=2*IM-1
  JMT=JM/2+1

  DPH=DPHD*D2R
  DLM=DLMD*D2R
  TPH0=TPH0D*D2R
  TLM0=TLM0D*D2R
  
! FIND THE ROW AND COLUMN WHICH HOLD KV

  JV2=JV/2
  IADD1=0
  IADD2=0
  IF(2*JV2.EQ.JV)THEN
     IADD1=-1
     IADD2=IM-1
  ENDIF
  KV=(JV2+IADD1)*IMT+IADD2+IV
 
  NROW=2*(KV/IMT)
  KNROW=IMT*NROW/2
  KREM=KV-KNROW
  IF(KREM.LE.IM-1)THEN
     NROW=NROW+1
     NCOL=2*KREM
  ELSE
     NROW=NROW+2
     NCOL=2*(KREM-IM)+1
  ENDIF

! FIND THE TRANSFORMED LATITUDE (POSITIVE NORTH) AND 
! LONGITUDE (POSITIVE EAST)

  TLATD=(NROW-(JM+1)/2)*DPHD
  TLOND=(NCOL-IM)*DLMD
 
! NOW CONVERT TO GEODETIC LATITUDE (POSITIVE NORTH) AND
! LONGITUDE (POSITIVE WEST)

  TLATR=TLATD*D2R
  TLONR=TLOND*D2R
  ARG1=SIN(TLATR)*COS(TPH0)+COS(TLATR)*SIN(TPH0)*COS(TLONR)
  GLATR=ASIN(ARG1)
  GLATD=GLATR*R2D
  ARG2=COS(TLATR)*COS(TLONR)/(COS(GLATR)*COS(TPH0))-  &
       TAN(GLATR)*TAN(TPH0)
  IF(ABS(ARG2).GT.1.)ARG2=ABS(ARG2)/ARG2
  FCTR=1.
  IF(TLOND.GT.0.)FCTR=-1.
  GLOND=TLM0D+FCTR*ACOS(ARG2)*R2D
 
END SUBROUTINE gwp2ll

!##############################################################################
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!$$$ 

SUBROUTINE GMP2LL(IH,JH,GLATD,GLOND)

! convert grid at mass point to latitude-longitude

  REAL, PARAMETER :: D2R=1.74532925E-2
  REAL, PARAMETER :: R2D=1./D2R

  COMMON /STAGEG/ IM,JM,TPH0D,TLM0D,DLMD,DPHD

  IMT=2*IM-1
  JMT=JM/2+1

  DPH=DPHD*D2R
  DLM=DLMD*D2R
  TPH0=TPH0D*D2R
  TLM0=TLM0D*D2R
  
! FIND THE ROW AND COLUMN WHICH HOLD KH

  JH2=JH/2
  IADD1=0
  IADD2=0
  IF(2*JH2.EQ.JH)THEN
     IADD1=-1
     IADD2=IM
  ENDIF
  KH=(JH2+IADD1)*IMT+IADD2+IH
 
  NROW=2*(KH/IMT)
  KNROW=IMT*NROW/2
  KREM=KH-KNROW
  IF(KREM.LE.IM)THEN
     NROW=NROW+1
     NCOL=2*KREM-1
  ELSE
     NROW=NROW+2
     NCOL=2*(KREM-IM)
  ENDIF

! FIND THE TRANSFORMED LATITUDE (POSITIVE NORTH) AND 
! LONGITUDE (POSITIVE EAST)

  TLATD=(NROW-(JM+1)/2)*DPHD
  TLOND=(NCOL-IM)*DLMD
 
! NOW CONVERT TO GEODETIC LATITUDE (POSITIVE NORTH) AND
! LONGITUDE (POSITIVE WEST)

  TLATR=TLATD*D2R
  TLONR=TLOND*D2R
  ARG1=SIN(TLATR)*COS(TPH0)+COS(TLATR)*SIN(TPH0)*COS(TLONR)
  GLATR=ASIN(ARG1)
  GLATD=GLATR*R2D
  ARG2=COS(TLATR)*COS(TLONR)/(COS(GLATR)*COS(TPH0))-    &
       TAN(GLATR)*TAN(TPH0)

  IF(ABS(ARG2).GT.1.)ARG2=ABS(ARG2)/ARG2
  FCTR=1.
  IF(TLOND.GT.0.)FCTR=-1.
  GLOND=TLM0D+FCTR*ACOS(ARG2)*R2D

END SUBROUTINE GMP2LL

!##############################################################################
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!$$$ 

SUBROUTINE LL2GMP(GLATD,GLOND,II,JJ)

! convert latitude-longitude to mass point grid point

  REAL, PARAMETER :: D2R=1.74532925E-2
  REAL, PARAMETER :: R2D=1./D2R

  COMMON /STAGEG/ IM,JM,TPH0D,TLM0D,DLMD,DPHD

  IMT=2*IM-1
  JMT=JM/2+1

!-----------------------------------------------------------------
  
! CONVERT FROM GEODETIC TO TRANSFORMED COORDINATES (DEGREES)

  GLAT=GLATD*D2R
  GLON=GLOND*D2R
  DPH=DPHD*D2R
  DLM=DLMD*D2R
  TPH0=TPH0D*D2R
  TLM0=TLM0D*D2R

  X=COS(TPH0)*COS(GLAT)*COS(GLON-TLM0)+SIN(TPH0)*SIN(GLAT)
  Y=-COS(GLAT)*SIN(GLON-TLM0)
  Z=COS(TPH0)*SIN(GLAT)-SIN(TPH0)*COS(GLAT)*COS(GLON-TLM0)
  TLAT=R2D*ATAN(Z/SQRT(X*X+Y*Y))
  TLON=R2D*ATAN(Y/X)

! FIND THE K VALUE OF THE NEAREST H POINT

  ROW=TLAT/DPHD+JMT
  COL=TLON/DLMD+IM
  NROW=INT(ROW)
  NCOL=INT(COL)
  TLAT=TLAT*D2R
  TLON=TLON*D2R

! FIRST CONSIDER THE SITUATION WHERE THE POINT X IS AT
!
!             V      H
!  
!                X
!             H      V

  IF(MOD(NROW,2).EQ.1.AND.MOD(NCOL,2).EQ.1.OR.      &
     MOD(NROW,2).EQ.0.AND.MOD(NCOL,2).EQ.0)THEN
     TLAT1=(NROW-JMT)*DPH
     TLAT2=TLAT1+DPH
     TLON1=(NCOL-IM)*DLM
     TLON2=TLON1+DLM
     DLM1=TLON-TLON1
     DLM2=TLON-TLON2
     D1=ACOS(COS(TLAT)*COS(TLAT1)*COS(DLM1)+SIN(TLAT)*SIN(TLAT1))
     D2=ACOS(COS(TLAT)*COS(TLAT2)*COS(DLM2)+SIN(TLAT)*SIN(TLAT2))
     IF(D1.GT.D2)THEN
        NROW=NROW+1
        NCOL=NCOL+1
     ENDIF

! NOW CONSIDER THE SITUATION WHERE THE POINT X IS AT
!
!              H      V
!
!                 X
!              V      H

  ELSE
     TLAT1=(NROW+1-JMT)*DPH
     TLAT2=TLAT1-DPH
     TLON1=(NCOL-IM)*DLM
     TLON2=TLON1+DLM
     DLM1=TLON-TLON1
     DLM2=TLON-TLON2
     D1=ACOS(COS(TLAT)*COS(TLAT1)*COS(DLM1)+SIN(TLAT)*SIN(TLAT1))
     D2=ACOS(COS(TLAT)*COS(TLAT2)*COS(DLM2)+SIN(TLAT)*SIN(TLAT2))
     IF(D1.LT.D2)THEN
        NROW=NROW+1
     ELSE
        NCOL=NCOL+1
     ENDIF
  ENDIF
  JJ=NROW
  II=NCOL/2
  IF(MOD(JJ,2).EQ.1)II=II+1

! NOW WE CAN FIND THE K VALUE

  KROWS=((NROW-1)/2)*IMT
  IF(MOD(NROW,2).EQ.1)THEN
     K=KROWS+(NCOL+1)/2
  ELSE
     K=KROWS+IM+NCOL/2
  ENDIF

END SUBROUTINE ll2gmp

!##############################################################################
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!$$$ 

SUBROUTINE LL2GWP(GLATD,GLOND,II,JJ)

! convert latitude-longitude to wind point grid point

  REAL, PARAMETER :: D2R=1.74532925E-2
  REAL, PARAMETER :: R2D=1./D2R

  COMMON /STAGEG/ IM,JM,TPH0D,TLM0D,DLMD,DPHD

  IMT=2*IM-1
  JMT=JM/2+1

!-----------------------------------------------------------------

! GIVEN THE GEODETIC LATITUDE (GLATD) AND LONGITUDE (GLOND),
! FIND THE NEAREST V POINT ON THE E GRID
! CONVERT FROM GEODETIC TO TRANSFORMED COORDINATES (DEGREES)

  GLAT=GLATD*D2R
  GLON=GLOND*D2R
  DPH=DPHD*D2R
  DLM=DLMD*D2R
  TPH0=TPH0D*D2R
  TLM0=TLM0D*D2R

  X=COS(TPH0)*COS(GLAT)*COS(GLON-TLM0)+SIN(TPH0)*SIN(GLAT)
  Y=-COS(GLAT)*SIN(GLON-TLM0)
  Z=COS(TPH0)*SIN(GLAT)-SIN(TPH0)*COS(GLAT)*COS(GLON-TLM0)
  TLAT=R2D*ATAN(Z/SQRT(X*X+Y*Y))
  TLON=R2D*ATAN(Y/X)

! IDENTIFY THE ROW AND COLUMN OF THE LOWER LEFT CORNER OF THE BOX
! THAT SURROUNDS THE GIVEN GEODETIC LOCATION

  ROW=TLAT/DPHD+JMT
  COL=TLON/DLMD+IM
  NROW=INT(ROW)
  NCOL=INT(COL)
  TLAT=TLAT*D2R
  TLON=TLON*D2R

! FIRST CONSIDER THE SITUATION WHERE THE POINT X IS AT
!
!             H      V
! 
!                 X
!             V      H

  IF(MOD(NROW,2).EQ.0.AND.MOD(NCOL,2).EQ.1.OR.  &
     MOD(NROW,2).EQ.1.AND.MOD(NCOL,2).EQ.0)THEN
     TLAT1=(NROW-JMT)*DPH
     TLAT2=TLAT1+DPH
     TLON1=(NCOL-IM)*DLM
     TLON2=TLON1+DLM
     DLM1=TLON-TLON1
     DLM2=TLON-TLON2
     D1=ACOS(COS(TLAT)*COS(TLAT1)*COS(DLM1)+SIN(TLAT)*SIN(TLAT1))
     D2=ACOS(COS(TLAT)*COS(TLAT2)*COS(DLM2)+SIN(TLAT)*SIN(TLAT2))
     IF(D1.GT.D2)THEN
        NROW=NROW+1
        NCOL=NCOL+1
     ENDIF

! NOW CONSIDER THE SITUATION WHERE THE POINT X IS AT
!
!              V      H
!
!                 X
!              H      V

  ELSE
     TLAT1=(NROW+1-JMT)*DPH
     TLAT2=TLAT1-DPH
     TLON1=(NCOL-IM)*DLM
     TLON2=TLON1+DLM
     DLM1=TLON-TLON1
     DLM2=TLON-TLON2
     D1=ACOS(COS(TLAT)*COS(TLAT1)*COS(DLM1)+SIN(TLAT)*SIN(TLAT1))
     D2=ACOS(COS(TLAT)*COS(TLAT2)*COS(DLM2)+SIN(TLAT)*SIN(TLAT2))
     IF(D1.LT.D2)THEN
        NROW=NROW+1
     ELSE
        NCOL=NCOL+1
     ENDIF
  ENDIF
  JJ=NROW
  II=NCOL/2
  IF(MOD(JJ,2).EQ.0)II=II+1

! NOW WE CAN FIND THE K VALUE

  KROWS=((NROW-1)/2)*IMT
  IF(MOD(NROW,2).EQ.1)THEN
     K=KROWS+NCOL/2
  ELSE
     K=KROWS+IM-1+(NCOL+1)/2
  ENDIF

END SUBROUTINE ll2gwp

!##############################################################################
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!$$$ 

SUBROUTINE SIGMA (SIGL,NLVL,OFFSET)

   REAL,    INTENT(OUT) :: SIGL(:)     ! mid point sigma levels 1 = bottom
   INTEGER, INTENT(IN)  :: NLVL        ! number of sigma levels to ouput
   REAL,    INTENT(OUT) :: OFFSET      ! pressure offset where levels isobaric

!  -------------- pt --------------- 25 mb
!  0<= sg1 <=1          sg2=0  
!  ------------- pdtop ------------- 420 mb (ptsgm)
!  sg1=1                0<= sg2 <=1
!  -------------- ps ---------------
!
! p=pt                  & ! pt const press at the top of model's atmosphere
!   +sg1*pdtop          & ! pdtop is constant thickness of the pressure layer
!   +sg2*(ps-pdtop-pt)  & ! ps is surface pressure

   INTEGER, PARAMETER :: LM=60         ! number of midpoint levels
   REAL               :: DSG(LM)       ! standard atmosphere thickness
   REAL               :: SGM(LM+1)     ! standard atmosphere sigma 
   REAL               :: DSG1(LM)      ! thickness in pressure regions
   REAL               :: DSG2(LM)      ! thickness in sigma regions
   REAL               :: SG1(LM+1)     ! interface sigma in pressure regions
   REAL               :: SG2(LM+1)     ! interface sigma in sigma regions
   REAL               :: SGML1(LM)     ! midpoint sigma in pressure regions
   REAL               :: SGML2(LM)     ! midpoint sigma in sigma regions
   REAL               :: ESIGMA        ! ecmwf equivalent sigma value
   INTEGER            :: LPT2          ! index of first sigma level
   INTEGER            :: L             ! level index 

   REAL,    PARAMETER :: pt=2500.0     ! top of atmosphere pressure
   REAL,    PARAMETER :: ptsgm=42000.  ! thickness of pressure layer

   DATA DSG/                                                     &
    .02835192,.02916347,.02994705,.02971081,.02792074,.02478650, &
    .02119629,.01821432,.01657202,.01662089,.01739752,.01824192, &
    .01889966,.01961820,.02049084,.02137079,.02209854,.02265381, &
    .02308464,.02332395,.02336217,.02293201,.02249343,.02223221, &
    .02206148,.02186187,.02162075,.02138152,.02113940,.02078838, &
    .02017513,.01926569,.01825996,.01743231,.01687049,.01646386, &
    .01607185,.01562498,.01511582,.01457278,.01405813,.01360196, &
    .01311248,.01240457,.01132916,.00997069,.00870742,.00787578, &
    .00743750,.00712628,.00677314,.00639189,.00604119,.00570599, &
    .00537055,.00510294,.00496033,.00490722,.00488027,.00485253/

!-------------------------------------------------------------------------------
!  First choose thicknesses of layers dsg(l) and sigma coordinate values at
!  the middle of sigma layers in an atmosphere with flat bottom and ps=1013 mb

   sgm(1)=0.0
   do l=1,lm
      sgm(l+1)=sgm(l)+dsg(l)
   enddo

!-------------------------------------------------------------------------------
!  Find pdtop

   lpt2=0
   do l=1,lm+1
      pl=sgm(l)*(101300.-pt)+pt
      if(pl.lt.ptsgm) lpt2=l
   enddo

   if(lpt2.gt.0) then
      pt2=sgm(lpt2)*(101300.-pt)+pt
   else
      pt2=pt
   endif

!  write(*,*) '*** Sigma system starts at ',pt2,' Pa, from level ',lpt2
   pdtop=pt2-pt

!-------------------------------------------------------------------------------
!  Define thicknesses of layers in pressure and sigma regions

   do l=1,lm
      if(l.le.lpt2) then
         dsg1(l)=dsg(l) ! pressure region
      else
         dsg2(l)=dsg(l) ! sigma region
      endif
   enddo

!-------------------------------------------------------------------------------
!  pressure region sigmas

   if(lpt2.gt.0) then
      sg1(1)=0.
      do l=1,lpt2
         sg1(l+1)=sg1(l)+dsg1(l)  ! interfaces in pressure region
      enddo
      do l=2,lpt2
         sg1(l)=sg1(l)/sg1(lpt2+1)  ! rescale sg1 to interval from 0 to 1
      enddo
      sg1(lpt2+1)=1.
      do l=lpt2+2,lm+1
         sg1(l)=1.
      enddo
      do l=1,lpt2
         dsg1(l)=sg1(l+1)-sg1(l) ! scaled thicknesses of sg1 (pressure) layers
         sgml1(l)=(sg1(l)+sg1(l+1))*0.5
      enddo
   endif

   sg1(lpt2+1)=1.
   do l=lpt2+1,lm
      dsg1(l)=0.
      sgml1(l)=1.
      sg1(l+1)=1.
   enddo

!-------------------------------------------------------------------------------
! sigma region sigmas

   do l=1,lpt2+1
      sg2(l)=0.
   enddo

   do l=lpt2+1,lm
      sg2(l+1)=sg2(l)+dsg2(l) ! interfaces in sigma region
   enddo

   do l=lpt2+2,lm
      sg2(l)=sg2(l)/sg2(lm+1) ! rescale sg2 to interval from 0 to 1
   enddo
   sg2(lm+1)=1.

   do l=lpt2+1,lm
      dsg2(l)=sg2(l+1)-sg2(l) ! scaled thicknesses of sg2 (sigma) layers
      sgml2(l)=(sg2(l)+sg2(l+1))*0.5
   enddo

   do l=1,lpt2
      dsg2(l)=0.
      sgml2(l)=0.
   enddo

!-------------------------------------------------------------------------------
! ecmwf equivalent sigmas

  ps=101300.0
  do l=lm,1,-1
     p=pt+sgml1(l)*pdtop+sgml2(l)*(ps-pdtop-pt)

!    ecmwf equivalent
!    offset=pt+sgml1(l)*pdtop
!    esigma=sgml2(l)*(ps-pdtop-pt)/ps
!    if(lm-l+1.le.nlvl) sigl(lm-l+1)=offset/100.0+esigma

!    standard sigma system equivalent
!    if(lm-l+1.le.nlvl) sigl(lm-l+1)=(p-pt)/(ps-pt)

!    return only NMM sigma value below constant pressure region
     sigl(lm-l+1)=sgml2(l)
  end do
  offset=(pt+pdtop)/100.0

END SUBROUTINE sigma
