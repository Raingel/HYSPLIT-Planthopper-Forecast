!##############################################################################
!$$$  MAIN PROGRAM DOCUMENTATION BLOCK
!
! MAIN PROGRAM: NAMS2ARL     NONHYDROSTATIC MESOCALE MODEL FOR HYSPLIT
!   PRGMMR: DRAXLER          ORG: R/ARL       DATE: 2002-08-20
!
! ABSTRACT:  THIS CODE WRITTEN AT THE AIR RESOURCES LABORATORY ...
!     Converts output from the nonhydrostactic mesoscale model to hysplit
!     format. Input grib data are on the native model staggered Arakawa
!     e-grid and the hybrid vertical coordinate.  The program is designed
!     to process only one time period per execution.  Multiple input files
!     may be specified on the command line, each containing different
!     variables required for the specific time period.  Input data are
!     reprojected from the rotated lat-lon grid to a lambert conformal
!     projection output grid using the nearest neighbor approach for 
!     both the mass and wind points. 
!
! PROGRAM HISTORY LOG:
!   LAST REVISED: 20 Aug 2002 (RRD) - initial version
!                 25 Oct 2002 (RRD) - conforms to nmm sigma system
!                 23 Jan 2003 (RRD) - modified to handle 4-km grids
!                 11 Aug 2006 (RRD) - modified to handle 12-km WRF NMM
!                 28 Aug 2006 (RRD) - set precip bucket by cycle
!                 25 Sep 2006 (RRD) - define subgrid on command line
!                 16 Oct 2006 (RRD) - option to process only 2D var
!                 29 Mar 2007 (BS)  - WRF --> NAMS (s=sigma)
!                 13 Mar 2008 (BS)  - PBLH
!                 09 Mar 2009 (BS)  - STYP initialization (cirrus)
!
! USAGE:  NAMS2ARL [-options]
!   INPUT ARGUMENT LIST:
!     GRIB_FILE_NAME - multiple grib files may be defined
!   OUTPUT ARGUMENT LIST:
!     NONE
!   INPUT FILES:
!     GRIB_DATA - grib input data files use special directIO routines
!   OUTPUT FILES:
!     unit 20 DATA.NAMS- ARL packed data output file
!     unit 30 CFG.NAMS - output format configuration file
!     unit 40 NAMSTIME  - processing time indicator file
!     unit 50 MESSAGE  - diagnostic messages
!     unit 60 RAIN.SUM - accumulated precipitation field
!     unit 70 RAIN.3HR - difference  precipitation field
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE:  IBM RS6000
!   NOTES:    Use the version of the W3LIB as compiled in the ARL library
!             rather than the NCEP library due to incompatibilities for 
!             argument list variables defined using dynamic allocation
!
!$$$

PROGRAM NAMS2ARL

  IMPLICIT NONE

  LOGICAL       :: ftest           ! test for existence
  CHARACTER(80) :: fname(2),label  ! name of grib file 
  INTEGER       :: iargc,narg      ! command line arguments
  INTEGER       :: handle,fcopen   ! file i/o handle 
  INTEGER       :: krec = 0        ! output file rec counter
  INTEGER       :: kbyte = 0       ! grib file byte pointer
  INTEGER       :: kret            ! decoder return code
  INTEGER       :: nfile = 0       ! number of grib input files
  INTEGER       :: rain            ! 1=p 2=d (see below)
  INTEGER       :: vres            ! vertical resolution flag
  INTEGER       :: xopt            ! extraction options      
  REAL          :: glimit(4)       ! grid limits (lat1,lon2,lat2,lon2)

!---------------------------------------------------------------
  INTERFACE
  SUBROUTINE XTRACT(handle,glimit,vres,xopt,rain,krec,kbyte,kret)
  INTEGER,INTENT(IN)       :: HANDLE       ! IO handle
  REAL,   INTENT(IN)       :: GLIMIT(4)    ! grid limits (lat1,lon2,lat2,lon2)
  INTEGER,INTENT(IN)       :: VRES         ! vertical resolution
  INTEGER,INTENT(IN)       :: XOPT         ! extraction options 
  INTEGER,INTENT(IN)       :: RAIN         ! precipitation processing
  INTEGER,INTENT(INOUT)    :: KREC         ! output record counter
  INTEGER,INTENT(INOUT)    :: KBYTE        ! input file byte counter
  INTEGER,INTENT(OUT)      :: KRET         ! termination code
  END SUBROUTINE xtract
  END INTERFACE
!---------------------------------------------------------------

! required for NCEP operational implementation
! CALL W3TAGB('NAMS2ARL ',1998,0238,0070,'R/ARL')

! check for command line arguments
  NARG=IARGC()

  IF(NARG.EQ.0)THEN
     WRITE(*,*)'Converts output from the nonhydrostactic mesoscale model to hysplit'
     WRITE(*,*)'format. Input grib data are on the native model staggered Arakawa'
     WRITE(*,*)'e-grid and the hybrid vertical coordinate.' 
     WRITE(*,*)' '
     WRITE(*,*)'Usage: nams2arl [-options]'
     WRITE(*,*)' -i[primary grib data: file name {required}]'
     WRITE(*,*)' -s[supplemental grib data: file name {optional}]'
     WRITE(*,*)' -g[grid: {none}=aqm; 0,0,0,0=full; '
     WRITE(*,*)'    lower left lat,lon, upper right lat,lon]'
     WRITE(*,*)' -p[precip options:  p=previous, {d}=difference]'
     WRITE(*,*)' -v[vertical resolution: {1}=cmaq, 2=<700 3=full 4=full+]'
     WRITE(*,*)' -x[extract: {1}=all, 2=2D variables]'
     STOP
  END IF

! default options
  VRES=1
  RAIN=2
  XOPT=1
  GLIMIT(1)=  20.0
  GLIMIT(2)=-125.0
  GLIMIT(3)=  50.0
  GLIMIT(4)= -55.0

! go through each argument
  DO WHILE (NARG.GT.0)
     CALL GETARG(NARG,LABEL)
     SELECT CASE (LABEL(1:2))

!    main grib input data file name   
     CASE ('-i','-I')
        NFILE=NFILE+1
        READ(LABEL(3:),'(A)')FNAME(NFILE)

!    supplemental grib input data file name   
     CASE ('-s','-S')
        NFILE=NFILE+1
        READ(LABEL(3:),'(A)')FNAME(NFILE)

!    optional subgrid limits: lower left lat,lon followed by the
!    upper right lat,lon; for example: -g30.0,110.0,50.0,80.0
     CASE ('-g','-G')
        READ(LABEL(3:),*)GLIMIT

!    precipitation processing  
     CASE ('-p','-P')
        SELECT CASE (LABEL(3:3))
        CASE ('p','P')
!          use previous precip (use RAIN.3HR)
           RAIN=1
        CASE ('d','D')
!          use difference precip (current-RAIN.SUM)
           RAIN=2
        END SELECT

!    vertical resolution
     CASE ('-v','-V')
        READ(LABEL(3:),'(I1)') VRES
        VRES=MAX(1,MIN(VRES,4))

!    extraction options 
     CASE ('-x','-X')
        READ(LABEL(3:),'(I1)') XOPT
        XOPT=MAX(1,MIN(XOPT,2))

     END SELECT
     NARG=NARG-1
  END DO

! diagnostic message file
  OPEN(50,FILE='MESSAGE')
  WRITE(50,*)'------------------------------------'
  WRITE(50,*)'COMMAND LINE OPTIONS'
  WRITE(50,*)'Subgrid selection: ',GLIMIT
  WRITE(50,*)'Precip processing: ',RAIN
  WRITE(50,*)'Vertical grid pts: ',VRES
  WRITE(50,*)'Extraction option: ',XOPT
  WRITE(50,*)'------------------------------------'

  IF(NFILE.EQ.0)THEN
    WRITE(*,*)'ERROR - no input file, possibly other inputs missing'
    STOP 999
  END IF

! process each data file
  DO WHILE (NFILE.GT.0)
     INQUIRE(file=fname(nfile),exist=ftest)

     IF(FTEST)THEN
        KBYTE=0
        HANDLE=FCOPEN(fname(nfile),'r')
        WRITE(*,*)'Started processing: ',fname(nfile)
        CALL XTRACT(handle,glimit,vres,xopt,rain,krec,kbyte,kret)
        IF(kret.ne.0)WRITE(*,*)'Error from XTRACT: ',kret
        CALL FCCLOS(handle,*900)
900     CONTINUE     
     ELSE    
        WRITE(*,*)'File not found:',fname(nfile)
     END IF   

!    close out time period by writing the index record
     CALL PAKNDX(20)

!    process next file
     NFILE=NFILE-1   
  END DO 

! close output file 
  CLOSE (20)

! required for NCEP operational implementation
! CALL W3TAGE('NAMS2ARL ')

END PROGRAM nams2arl


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
!                 17 Jul 2006 (RRD) - wrf nearest neighbor interpolation
!
! USAGE:  CALL XTRACT(HANDLE,GLIMIT,RAIN,KREC,KBYTE,KRET)
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

SUBROUTINE XTRACT(HANDLE,GLIMIT,VRES,XOPT,RAIN,KREC,KBYTE,KRET)

  IMPLICIT NONE

! argument list variables  
  INTEGER,INTENT(IN)       :: HANDLE       ! IO handle
  REAL,   INTENT(IN)       :: GLIMIT(4)    ! grid limits (lat1,lon2,lat2,lon2)
  INTEGER,INTENT(IN)       :: VRES         ! vertical resolution
  INTEGER,INTENT(IN)       :: XOPT         ! extraction options 
  INTEGER,INTENT(IN)       :: RAIN         ! precipitation processing
  INTEGER,INTENT(INOUT)    :: KREC         ! output record counter
  INTEGER,INTENT(INOUT)    :: KBYTE        ! input file byte counter
  INTEGER,INTENT(OUT)      :: KRET         ! termination code

  INTEGER                  :: NUMPTS       ! extract grid size
  REAL                     :: CLAT, CLON   ! center position
  INTEGER                  :: ZERO = 0     ! initialize output file
  INTEGER                  :: SFCP         ! use surface pressure flag

! selection mask for WRF-NMM sigma levels                     
  INTEGER      :: SMASK (61) = 0

! temporary holding variable
  INTEGER      :: VGRIB
  CHARACTER(4) :: UCHAR, VCHAR
  REAL         :: CNVRT

! arrays for grib description
  INTEGER      :: KPDS(25)              ! product definition
  INTEGER      :: KGDS(25)              ! grid definition
  INTEGER      :: KPTR(25)              ! section lengths

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
  REAL,ALLOCATABLE         :: XVAR(:,:) ! two dimensional space
 
! latitude and longitudes for input mass array 
  REAL,ALLOCATABLE         :: TLAT(:,:),TLON(:,:)

! latitude and longitudes for input vector array 
  REAL,ALLOCATABLE         :: VLAT(:,:),VLON(:,:)
  REAL,ALLOCATABLE         :: DRAY(:,:),CRAY(:,:)

! input grid mass/wind point value on the output grid
  INTEGER, ALLOCATABLE     :: IMGP(:,:),JMGP(:,:)
  INTEGER, ALLOCATABLE     :: IWGP(:,:),JWGP(:,:)

! packed output array
  REAL,        ALLOCATABLE :: SVAR(:,:)  
  REAL,        ALLOCATABLE :: TVAR(:,:)     
  CHARACTER(1),ALLOCATABLE :: CVAR(:)
  CHARACTER(4)             :: model
  CHARACTER(80)            :: fname 
  LOGICAL                  :: ftest 

! mapping variables that define the output grid  
  REAL                     :: parmap(9),gridkm
  INTEGER                  :: rotate

  INTEGER :: iyr,imo,ida,ihr,iy0,im0,id0,ih0
  INTEGER :: i,j,k,ii,jj,ifh,imn
  INTEGER :: kvc,nlon,n2dv,nlvl,nlat,n3dv
  INTEGER :: nx,ny,nz,kl,kv,klen,lrec,nxy,nxp,nyp

  REAL    :: crsum, prsum
  REAL    :: dummy(1),ug,vg,uc,vc
  REAL    :: xcp,ycp,clon1,clat1,clat2,clon2
  REAL    :: TPH0D,TLM0D,DLMD,DPHD
  REAL    :: x1,y1,x2,y2,x3,y3,x4,y4,x5,y5,x6,y6

  SAVE VGRIB0,VCHAR0,CNVRT0,VGRIB1,VCHAR1,CNVRT1,SIG0,STYP,       &
       N2DV,N3DV,NXP,NYP,NXY,TLAT,TLON,RVAR,XVAR,PARMAP,          &
       IWGP,JWGP,IMGP,JMGP,BUFF,CVAR,SVAR,TVAR,NLVL

!---------------------------------------------------------------------

  COMMON /STAGEG/ NLON,NLAT,TPH0D,TLM0D,DLMD,DPHD

!---------------------------------------------------------------------

  INTERFACE

  SUBROUTINE MAKNDX (N2DV,N3DV,VCHAR0,VCHAR1,SIGL,NXP,NYP,NZP,   &
                     CLAT2,CLON2,GRIDKM,KVC,MODEL)
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

  SUBROUTINE SIGMA (SIGL,NLVL,SMASK,VRES)
  REAL,    INTENT(OUT)   :: SIGL(:)     ! mid point sigma levels 1 = bottom
  INTEGER, INTENT(INOUT) :: NLVL        ! number of sigma levels to ouput
  INTEGER, INTENT(INOUT) :: SMASK(:)    ! number of sigma levels to ouput
  INTEGER, INTENT(IN)    :: VRES        ! vertical resolution
  END SUBROUTINE sigma

  SUBROUTINE rotlat(IM,JM,TPH0D,TLM0D,VLAT,VLON,DRAY,CRAY)
  IMPLICIT NONE
  INTEGER, INTENT(IN)  :: IM,JM
  REAL,    INTENT(IN)  :: TPH0D,TLM0D                                                                           
  REAL,    INTENT(IN)  :: VLAT(:,:) ! LATITUDE OF E-GRID V POINTS (DEGREES)
  REAL,    INTENT(IN)  :: VLON(:,:) ! LONGITUDE OF E-GRID V POINTS (DEGREES)
  REAL,    INTENT(OUT) :: DRAY(:,:) ! ROTATION COSINE
  REAL,    INTENT(OUT) :: CRAY(:,:) ! ROTATION SINE
  END SUBROUTINE rotlat

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
    KLEN=ICHAR(BUFF(7))+ICHAR(BUFF(6))*256+ICHAR(BUFF(5))*65536

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
       WRITE(50,*)'Current time:',IYR,IMO,IDA,IHR
       IY0=IYR
       IM0=IMO
       ID0=IDA
       IH0=IHR
    ELSE
       IF(IY0.NE.IYR.OR.IM0.NE.IMO.OR.ID0.NE.IDA.OR.IH0.NE.IHR)GOTO 910 
    END IF

!-------------------------------------------------------------------------------
! determine the positions of each input data grid the first time prior to
! any data processing and then define the output grid based upon the input grid
!-------------------------------------------------------------------------------

    IF(.NOT.ALLOCATED(TLAT).OR..NOT.ALLOCATED(TLON))THEN

! horizontal input grid for grib data  

       NLON=KGDS(2)  ! number of input grid I points
       NLAT=KGDS(3)  ! number of input grid J points 

!      the 2-dim data array (xvar,yvar) and the 1-dim array for input (rvar)
       ALLOCATE (RVAR(NLON*NLAT),STAT=KRET)
       IF(KRET.NE.0) WRITE(*,*)'RVAR allocation error: ',KRET,NLON,NLAT
       ALLOCATE (XVAR(NLON,NLAT),STAT=KRET)
       IF(KRET.NE.0) WRITE(*,*)'XVAR allocation error: ',KRET,NLON,NLAT

!      the lat-lon array that identifies each input grid location
       ALLOCATE (TLAT(NLON,NLAT),STAT=KRET)
       IF(KRET.NE.0) WRITE(*,*)'Allocate Error TLAT: ',KRET,NLON,NLAT
       ALLOCATE (TLON(NLON,NLAT),STAT=KRET)
       IF(KRET.NE.0) WRITE(*,*)'Allocate Error TLON: ',KRET,NLON,NLAT
       WRITE(50,*)'Input mass point positions array allocation: ',NLON,NLAT

       ALLOCATE (VLAT(NLON,NLAT),STAT=KRET)
       IF(KRET.NE.0) WRITE(*,*)'Allocate Error VLAT: ',KRET,NLON,NLAT
       ALLOCATE (VLON(NLON,NLAT),STAT=KRET)
       IF(KRET.NE.0) WRITE(*,*)'Allocate Error VLON: ',KRET,NLON,NLAT
       WRITE(50,*)'Input vectory positions array allocation: ',NLON,NLAT
 
       ALLOCATE (DRAY(NLON,NLAT),STAT=KRET)
       IF(KRET.NE.0) WRITE(*,*)'Allocate Error DRAY: ',KRET,NLON,NLAT
       ALLOCATE (CRAY(NLON,NLAT),STAT=KRET)
       IF(KRET.NE.0) WRITE(*,*)'Allocate Error CRAY: ',KRET,NLON,NLAT
       WRITE(50,*)'Input wind rotation angles array allocation: ',NLON,NLAT

!      input grid center latitude(7) longitude(8)
       TPH0D=KGDS(7)/1000.0
       TLM0D=KGDS(8)/1000.0
       WRITE(50,*)'Input grid center: ',TPH0D,TLM0D 

!      grid spacing in degrees
       DPHD=KGDS(9)/1000.0
       DLMD=KGDS(10)/1000.0

       GRIDKM=111.1984
       WRITE(50,*)'dLon-DLMD:',DLMD,'   dLat-DPHD:',DPHD
       WRITE(50,*)'dLon-km  :',DLMD*GRIDKM,'   dLat-km  :',DPHD*GRIDKM

!      define the location of grid mass points (note that the GMP and
!      GWP conversion programs reverse the i-array direction)
       DO J=1,NLAT
          I=0
          DO II=NLON,1,-1
          I=I+1
!         lat-lon of the mass points ... the mass point corresponds
!         to the (1,1) grid point on the lower left and this grid will
!         be used to set up the output grid
          CALL GMP2LL(II,J,TLAT(I,J),TLON(I,J))

!         wind point array positions required for wind vector rotation
          CALL GWP2LL(II,J,VLAT(I,J),VLON(I,J))
       END DO
       END DO

!      define cosine/sine array for wind rotation
       CALL ROTLAT(NLON,NLAT,TPH0D,TLM0D,VLAT,VLON,DRAY,CRAY)

       write(50,*)'Input grid mass corner points ...'
       write(50,101) TLAT(1,nlat),TLAT(nlon,nlat)
       write(50,102) TLAT(1,1),   TLAT(nlon,1)
       write(50,103) TLON(1,nlat),TLON(nlon,nlat)
       write(50,104) TLON(1,1),   TLON(nlon,1)

   101 format(" LAT(1,JM)=",e10.4,"  LAT(IM,JM)=",e10.4)
   102 format(" LAT(1,1) =",e10.4,"  LAT(IM,1) =",e10.4)
   103 format(" LON(1,JM)=",e10.4,"  LON(IM,JM)=",e10.4)
   104 format(" LON(1,1) =",e10.4,"  LON(IM,1) =",e10.4)

! define the output grid

!      temporary variable for grid center
       NXP=(NLON+1)/2
       NYP=(NLAT+1)/2
       WRITE(50,*)'Temporary output grid center x,y: ',NXP,NYP

!      define grid spacing at grid center 
       CLAT2=TLAT(NXP,NYP+1)
       CLAT1=TLAT(NXP,NYP-1)
       GRIDKM=NINT(0.75*(CLAT2-CLAT1)*111.1984)-1.0
       WRITE(50,*)'Output grid resolution: ',GRIDKM

!      initially define the output grid at central location
       CLAT2=NINT(TLAT(NXP,NYP))
       CLON2=NINT(TLON(NXP,NYP))
       CALL STLMBR(PARMAP,CLAT2,CLON2)     ! force lambert projection 

       CALL STCM1P(PARMAP,500.0,500.0,CLAT2,CLON2,CLAT2,CLON2,GRIDKM,0.0)
       WRITE(50,*)'Output grid center lat,lon: ',CLAT2,CLON2 

!        XY4         XY6        XY2
!         x----------------------x
!          \     ----------     /
!           \    |        |    /
!            \   |        |   /
!             \  ----------  /
!              x------------x
!             XY1    XY5   XY3


    IF(SUM(GLIMIT).EQ.0.0)THEN
!      full grid output

!      find the grid limits that are within the input data domain
       CALL CLL2XY(PARMAP,TLAT(NLON,1),TLON(NLON,1),X3,Y3)
       WRITE(50,*)'Corner XY3: ',INT(X3),INT(Y3)
       CALL CLL2XY(PARMAP,TLAT(1,NLAT),TLON(1,NLAT),X4,Y4)
       WRITE(50,*)'Corner XY4: ',INT(X4),INT(Y4)
       CALL CLL2XY(PARMAP,TLAT(1,1),TLON(1,1),X1,Y1)
       WRITE(50,*)'Corner XY1: ',INT(X1),INT(Y1)
       CALL CLL2XY(PARMAP,TLAT(NLON,NLAT),TLON(NLON,NLAT),X2,Y2)
       WRITE(50,*)'Corner XY2: ',INT(X2),INT(Y2)
       CALL CLL2XY(PARMAP,TLAT(NXP,1), TLON(NXP,1), X5,Y5)
       CALL CLL2XY(PARMAP,TLAT(NXP,NLAT),TLON(NXP,NLAT),X6,Y6)

!      stay within bounds
       X1=MAX(X1,X4)
       X2=MIN(X2,X3)
       Y1=MAX(Y1,Y5,Y3)
       Y2=MIN(Y2,Y4,Y6)
       WRITE(50,*)'Full grid corner points: ',X1,Y1,X2,Y2

!      set up the output grid system
       NXP=NINT(X2-X1)+1
       NYP=NINT(Y2-Y1)+1

    ELSE
!      sub grid output (-g command line)
       CALL CLL2XY(PARMAP,GLIMIT(1),GLIMIT(2),X1,Y1)
       CALL CLL2XY(PARMAP,GLIMIT(3),GLIMIT(4),X2,Y2)
       WRITE(50,*)'Subgrid corners: ',GLIMIT     
       WRITE(50,*)'Subgrid corner points: ',X1,Y1,X2,Y2

!      set up the output grid system
       NXP=NINT(X2-X1)+1
       NYP=NINT(Y2-Y1)+1

!      set the reference point for the grid center
       XCP=(X2+X1)/2.0
       YCP=(Y2+Y1)/2.0
       CALL CXY2LL(PARMAP,XCP,YCP,CLAT2,CLON2)
       WRITE(50,*)'Output grid center: ',XCP,YCP,CLAT2,CLON2

       CLAT2=NINT(CLAT2)
       CLON2=NINT(CLON2)
!      CLAT2=SIGN(FLOAT(NINT(ABS(CLAT2))),CLAT2)
!      CLON2=SIGN(FLOAT(NINT(ABS(CLON2))),CLON2)
     END IF

!      redefine so that lower left corner point is 1,1 and force lambert 
       XCP=(NXP+1.0)/2.0
       YCP=(NYP+1.0)/2.0
       CALL STLMBR(PARMAP,CLAT2,CLON2) 
       CALL STCM1P(PARMAP,XCP,YCP,CLAT2,CLON2,CLAT2,CLON2,GRIDKM,0.0)
       WRITE(50,*)'Output grid center: ',XCP,YCP,CLAT2,CLON2

! find the nearest input grid point to every output grid point

       ALLOCATE (IMGP(NXP,NYP),JMGP(NXP,NYP),          &
                 IWGP(NXP,NYP),JWGP(NXP,NYP), STAT=KRET)
       IF(KRET.NE.0) WRITE(*,*)'Output grid allocation error: ',KRET,NXP,NYP
       WRITE(50,*)'Final output grid allocation: ',NXP,NYP

       IMGP=0
       JMGP=0
       IWGP=0
       JWGP=0

       DO J=1,NYP
       DO I=1,NXP
          CALL CXY2LL(PARMAP,FLOAT(I),FLOAT(J),CLAT,CLON)

          CALL LL2GWP(CLAT,CLON,II,JJ)  ! wind grid point
          IWGP(I,J)=MIN(MAX(NLON-II+1,1),NLON)
          JWGP(I,J)=MIN(MAX(JJ,1),NLAT)

          CALL LL2GMP(CLAT,CLON,II,JJ)  ! mass grid point
          IMGP(I,J)=MIN(MAX(NLON-II+1,1),NLON)
          JMGP(I,J)=MIN(MAX(JJ,1),NLAT)
       END DO
       END DO

    END IF

!-------------------------------------------------------------------------------
! determine the variables and levels for processing and allocate the remaining
! array space for the packing and output grid interpolation requirements
!-------------------------------------------------------------------------------

    IF(.NOT.ALLOCATED(CVAR))THEN

     ! if increase n2dv, check write formatting (makndx) so fields written on 1 line
       N2DV= 14         ! number of 2D variables 
       N3DV= 6          ! number of 3D variables
       MODEL='NAMS'      ! ID=NAMS reserved in HYSPLIT for staggered output grids

!      surface variables
       ALLOCATE (VGRIB0(N2DV),STYP(N2DV),SIG0(N2DV),VCHAR0(N2DV),              &
                 CNVRT0(N2DV),STAT=KRET)
       IF(KRET.NE.0) WRITE(*,*)'2-dim variable allocation error: ',KRET,N2DV

!      upper level variables
       ALLOCATE (VGRIB1(N3DV),VCHAR1(N3DV),CNVRT1(N3DV))
       IF(KRET.NE.0) WRITE(*,*)'3-dim variable allocation error: ',KRET,N3DV

!      determine number of sigma levels
       NLVL=0
       CALL SIGMA (SIGL,NLVL,SMASK,VRES)
       ALLOCATE (SIGL(NLVL))
       WRITE(50,*)'Number of output sigma levels: ',NLVL
       CALL SIGMA (SIGL,NLVL,SMASK,VRES)

!      surface variable definitions (also available UMOF, VMOF 124,125) 
      !VGRIB0 = (/    1,    61,    33,    34,    11,   122,   253 /)  
      !SIG0   = (/    0,     0,    10,    10,     2 ,    0,     0 /)
      !STYP   = (/    1,     1,   105,   105,   105 ,    1,     1 /)
      !VCHAR0 = (/'PRSS','TPP3','U10M','V10M','T02M','SHTF','USTR'/)
      !CNVRT0 = (/ 0.01  ,.001 ,  1.0 ,  1.0 ,  1.0 ,  1.0 ,  1.0 /)

       VCHAR0 = (/ 'SHGT','MSLP','TPP3','T02M',                  &
                   'RH2M','U10M','V10M','PRSS','LHTF',           &
                   'SHTF','USTR','RGHS','DSWF','PBLH'/)
       VGRIB0 = (/ 7,  2, 61, 11,                                &
                  52, 33, 34,  1, 121,                           &
                  122,253,83, 204,221/)
       SIG0 = (/  0,   0,   0,   2,                              &
                  2,  10,  10,   0,   0,                         &
                  0,   0,   0,   0,   0/)
       STYP = (/  1,   102,   1, 105,                            &
                  105, 105, 105, 1,  1,                          &
                  1,   1,   1,   1,  1/)
       CNVRT0 = (/  1.0, 0.01, 0.001, 1.0,                       &
                    1.0,   1.0,  1.0, 0.01, 1.0,                 &
                    1.0,   1.0,  1.0, 1.0,  1.0/)

!      upper level variable definitions
       VGRIB1 = (/   11,    33,    34,    39,    51,   158 /)
       VCHAR1 = (/'TEMP','UWND','VWND','WWND','SPHU','TKEN'/)
       CNVRT1 = (/  1.0,   1.0,   1.0,  0.01,   1.0 ,  1.0 /)

!      vertical coordinate system 
!      kvc = 1(sigma); 2(pressure); 3(height); 4(hybrid)
       KVC=1

!      reserve space for output data arrays
       NXY=NXP*NYP
       ALLOCATE (CVAR(NXY),SVAR(NXP,NYP),TVAR(NXP,NYP),STAT=KRET)
       IF(KRET.NE.0) WRITE(*,*)'Output array allocation error: ',KRET,NXY,NXP,NYP

!      create the configuration file 
       CALL MAKNDX(N2DV,N3DV,VCHAR0,VCHAR1,SIGL,NXP,NYP,NLVL,   &
            CLAT2,CLON2,GRIDKM,KVC,MODEL)

!      configure the packing routines
       FNAME='CFG.NAMS'
       CALL PAKSET(20,FNAME,1,NX,NY,NZ)
       WRITE(50,*)'Set grid from pakset: ',nx,ny,nz

!      standard output name for packed data
       LREC=NXY+50
       OPEN(20,FILE='DATA.NAMS',RECL=LREC,ACCESS='DIRECT',FORM='UNFORMATTED')

!      diagnostic output labels 
       WRITE(50,'(A)')'   Lev  Var  Var Sigma Fhr1 Fhr2'
    END IF

!-------------------------------------------------------------------------------
! determine if this grib record will be converted to ARL packed format
!-------------------------------------------------------------------------------

    KL=0 ! set default to surface
!   check if 2d variable present in selection table
    DO KV=1,N2DV 
       VGRIB=VGRIB0(KV)
       VCHAR=VCHAR0(KV)
       CNVRT=CNVRT0(KV)

!      matches id and special level indicator
       IF(KPDS(5).EQ.VGRIB.AND.KPDS(7).EQ.SIG0(KV).AND.    &
          KPDS(6).EQ.STYP(KV))GOTO 300
    END DO
    IF(XOPT.EQ.2) GOTO 800       ! surface fields only

!   then check for 3d variable
    DO KV=1,N3DV 
       VGRIB=VGRIB1(KV)
       VCHAR=VCHAR1(KV)
       CNVRT=CNVRT1(KV)
!      the #109 identifies the field as a hybrid level
       IF(KPDS(5).EQ.VGRIB.AND.KPDS(6).EQ.109)GO TO 200
    END DO
    GO TO 800

200 CONTINUE

!   check if output level is present in level selection mask 
    KL=SMASK(KPDS(7))
    IF(KL.NE.0)GO TO 300

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

    IF((VCHAR(1:1).EQ.'U'.OR.VCHAR(1:1).EQ.'V').AND.VCHAR.NE.'USTR')THEN
!      wind variable code section
       K=0
       DO J=1,NLAT
       DO I=1,NLON
          K=K+1
          XVAR(I,J)=RVAR(K)   
       END DO     
       END DO

!     find value for output grid from nearest mass or wind point
      DO J=1,NYP
      DO I=1,NXP
         IF(VCHAR(1:1).EQ.'U') SVAR(I,J)=XVAR(IWGP(I,J),JWGP(I,J))
         IF(VCHAR(1:1).EQ.'V') TVAR(I,J)=XVAR(IWGP(I,J),JWGP(I,J))
      END DO
      END DO

!     increment wind rotation flag (both components required)
      ROTATE=ROTATE+1
      IF(VCHAR(1:1).EQ.'U') UCHAR=VCHAR

    ELSE
!      mass variable code section 
       K=0
       DO J=1,NLAT
       DO I=1,NLON
          K=K+1
          XVAR(I,J)=RVAR(K)   
       END DO     
       END DO

!      find value for output grid from nearest mass or wind point
       DO J=1,NYP
       DO I=1,NXP
          SVAR(I,J)=XVAR(IMGP(I,J),JMGP(I,J))
       END DO
       END DO

!      special treatment for precipitation to get 3 hour accumulation 
!      00z & 12z accumulation period = 12 hours
!      06z & 18z accumulation period =  3 hours

       IF(VCHAR.EQ.'TPP3') THEN
          CRSUM=SUM(SVAR)  ! current time sum
          WRITE(50,*)'Current Time Precip Sum: ',CRSUM

!         read previous precip bucket total 
          INQUIRE(FILE='RAIN.SUM',EXIST=FTEST)
          IF(FTEST)THEN
             OPEN(60,FILE='RAIN.SUM',FORM='UNFORMATTED',ACTION='READ')
             READ(60,IOSTAT=KRET)TVAR
             CLOSE(60)
             IF(KRET.NE.0)TVAR=0.0
          ELSE
             TVAR=0.0
          END IF
          PRSUM=SUM(TVAR)  ! previous time sum
          WRITE(50,*)'Precip  (from RAIN.SUM): ',PRSUM

!         at 3 hr bucket times replace with current precip array 
          IF(MOD(IFH+3,3).EQ.0)THEN
             OPEN(60,FILE='RAIN.SUM',FORM='UNFORMATTED',ACTION='WRITE')
             WRITE(60)SVAR
             CLOSE(60)
             WRITE(50,*)'Updated RAIN.SUM at forecast hour: ',IFH
          END IF

          IF(RAIN.EQ.1)THEN
!            use previously saved precipitation value 
             INQUIRE(FILE='RAIN.3HR',EXIST=FTEST)
             IF(FTEST)THEN
                OPEN(70,FILE='RAIN.3HR',FORM='UNFORMATTED',ACTION='READ')
                READ(70,IOSTAT=KRET)SVAR
                CLOSE(70)
                IF(KRET.NE.0)SVAR=0.0
                WRITE(50,*)'Using previous precipitation file: RAIN.3HR'
             ELSE
                SVAR=0.0
                WRITE(50,*)'Precipitation file RAIN.3HR not found!'
             END IF

          ELSE
!            use difference between current and previous 
            !IF(CRSUM.GE.PRSUM)THEN
             IF(KPDS(15)-KPDS(14).GT.3)THEN
                WRITE(50,*)'Using difference precip - accumulating bucket',kpds(14),kpds(15)
!               rain bucket still accumulating
                SVAR=SVAR-TVAR
                CRSUM=SUM(SVAR)  
                WRITE(50,*)'Difference precipitation Sum: ',CRSUM
             ELSE
                WRITE(50,*)'Using difference precip -- no diff -- new bucket',kpds(14),kpds(15)
             END IF

             IF(MOD(IFH,3).EQ.0)THEN
                OPEN(70,FILE='RAIN.3HR',FORM='UNFORMATTED',ACTION='WRITE')
                WRITE(70)SVAR
                CLOSE(70)
                WRITE(50,*)'Updated RAIN.3HR at forecast hour: ',IFH
             END IF
          END IF
       END IF

!      reset wind rotation flag
       ROTATE=0
    END IF

!-------------------------------------------------------------------------------
! pack into ARL format and write to disk (wind variables wait for both U,V)
!-------------------------------------------------------------------------------

    IF(ROTATE.EQ.2)THEN
       DO J=1,NYP
       DO I=1,NXP
          II=IWGP(I,J)
          JJ=JWGP(I,J)

!         convert NAMS input grid to compass direction
          UC=DRAY(II,JJ)*SVAR(I,J)+CRAY(II,JJ)*TVAR(I,J)
          VC=DRAY(II,JJ)*TVAR(I,J)-CRAY(II,JJ)*SVAR(I,J)

!         convert compass direction to output grid-orientation 
          CALL CC2GXY(PARMAP,FLOAT(I),FLOAT(J),UC,VC,UG,VG)
          SVAR(I,J)=UG
          TVAR(I,J)=VG
       END DO
       END DO

!      pack and write U-component
       WRITE(50,'(1X,2I5,1X,A,1X,3I5)')KL,KV,UCHAR,KPDS(7),KPDS(14),KPDS(15)
       CALL PAKREC(20,SVAR,CVAR,NXP,NYP,NXY,UCHAR,IYR,IMO,IDA,IHR,IMN,IFH,   &
                  (KL+1),ZERO)
       KREC=KREC+1

!      pack and write V-component
       WRITE(50,'(1X,2I5,1X,A,1X,3I5)')KL,KV,VCHAR,KPDS(7),KPDS(14),KPDS(15)
       CALL PAKREC(20,TVAR,CVAR,NXP,NYP,NXY,VCHAR,IYR,IMO,IDA,IHR,IMN,IFH,   &
                  (KL+1),ZERO)
       KREC=KREC+1
       ROTATE=0

    ELSEIF(ROTATE.EQ.1)THEN
        CONTINUE

    ELSE
!      pack into ARL format and continue
       WRITE(50,'(1X,2I5,1X,A,1X,3I5)')KL,KV,VCHAR,KPDS(7),KPDS(14),KPDS(15)
       CALL PAKREC(20,SVAR,CVAR,NXP,NYP,NXY,VCHAR,IYR,IMO,IDA,IHR,IMN,IFH,   &
                  (KL+1),ZERO)
       KREC=KREC+1
    END IF

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
!
! USAGE:  CALL MAKNDX(N2DV,N3DV,VCHAR0,VCHAR1,LEVEL,NXP,NYP,NZP, 
!                     CLAT2,CLON2,GRIDKM,NLAT,NLON)
!   INPUT ARGUMENT LIST:    see below
!   OUTPUT ARGUMENT LIST:   see below
!   INPUT FILES:            NONE
!   OUTPUT FILES:           UNIT 30 CFG.NAMS output file structure
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE:  IBM RS6000
!
!$$$

SUBROUTINE MAKNDX (N2DV,N3DV,VCHAR0,VCHAR1,SIGL,NXP,NYP,NZP,     &
                   CLAT2,CLON2,GRIDKM,KVC,MODEL)

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
  GRIDS(1)=CLAT2
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
! sync x,y defines the center point       
  GRIDS(8)=(NXP+1.0)/2.0
  GRIDS(9)=(NYP+1.0)/2.0
! lat/lon of the center point
  GRIDS(10)=CLAT2
  GRIDS(11)=CLON2
! variable reserved for future use
  GRIDS(12)=0.0

! write the packer configuration file
  OPEN(30,FILE='CFG.NAMS')

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

   ! format for arbitrary max number variables (20)
     IF(SIG.LT.1.0)THEN
        WRITE(30,'(A20,F6.5,I3,20(1X,A4))')LABEL(1),SIG,MVAR,(VCHAR(N),N=1,MVAR)
     ELSEIF(SIG.GE.1.AND.SIG.LT.10.0)THEN
        WRITE(30,'(A20,F6.4,I3,20(1X,A4))')LABEL(1),SIG,MVAR,(VCHAR(N),N=1,MVAR)
     ELSEIF(SIG.GE.10.AND.SIG.LT.100.0)THEN
        WRITE(30,'(A20,F6.3,I3,20(1X,A4))')LABEL(1),SIG,MVAR,(VCHAR(N),N=1,MVAR)
     ELSEIF(SIG.GE.100.AND.SIG.LT.1000.0)THEN
        WRITE(30,'(A20,F6.2,I3,20(1X,A4))')LABEL(1),SIG,MVAR,(VCHAR(N),N=1,MVAR)
     ELSEIF(SIG.GE.1000)THEN
        WRITE(30,'(A20,F6.1,I3,20(1X,A4))')LABEL(1),SIG,MVAR,(VCHAR(N),N=1,MVAR)
     END IF

  END DO
  CLOSE (30) 

END SUBROUTINE makndx




!##############################################################################
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!$$$  From: Eric Rodgers W/NP22

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
!$$$  From: Eric Rodgers W/NP22

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
!$$$  From: Eric Rodgers W/NP22

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

END SUBROUTINE LL2GMP

!##############################################################################
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!$$$  From: Eric Rodgers W/NP22

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

SUBROUTINE SIGMA (SIGL,NLVL,SMASK,VRES)

   REAL,    INTENT(OUT)   :: SIGL(:)     ! mid point sigma levels
   INTEGER, INTENT(INOUT) :: NLVL        ! number of sigma levels to output
   INTEGER, INTENT(INOUT) :: SMASK(:)    ! selection mask for output levels
   INTEGER, INTENT(IN)    :: VRES        ! vertical resolution (1-cmaq,2-700,3-full)

   INTEGER :: K
   INTEGER :: VMASK (61) ! selection mask for sigma levels                     
   INTEGER :: VMASK1(61) ! cmaq                      
   INTEGER :: VMASK2(61) ! <=700                     
   INTEGER :: VMASK3(61) ! full domain                     
   INTEGER :: VMASK4(61) ! possible nam12 replacement
   REAL    :: WRFSIG(61) ! number of sigma levels for WRF-NMM                  

   SAVE VMASK, WRFSIG

!  selection mask set to mimic the resolution of the CMAQ AQM conversion
   DATA VMASK1/      0,      1,      1,      0,      1,      0,  &
                     1,      0,      1,      0,      1,      0,  &
                     0,      0,      1,      0,      0,      1,  &
                     0,      0,      1,      0,      1,      1,  &
                     0,      1,      0,      1,      0,      1,  &
                     0,      1,      0,      0,      1,      0,  &
                     0,      1,      0,      0,      0,      1,  &
                     0,      0,      0,      1,      0,      0,  &
                     0,      1,      0,      0,      0,      0,  &
                     1,      0,      0,      0,      1,      0,  &
                     0 / 


   DATA VMASK2/      0,      1,      1,      1,      1,      1,  &
                     1,      1,      1,      1,      1,      1,  &
                     1,      1,      1,      1,      1,      1,  &
                     1,      1,      1,      1,      1,      1,  &
                     1,      1,      1,      1,      1,      1,  &
                     0,      0,      0,      0,      0,      0,  &
                     0,      0,      0,      0,      0,      0,  &
                     0,      0,      0,      0,      0,      0,  &
                     0,      0,      0,      0,      0,      0,  &
                     0,      0,      0,      0,      0,      0,  &
                     0 / 

   DATA VMASK3/      0,      1,      1,      1,      1,      1,  &
                     1,      1,      1,      1,      1,      1,  &
                     1,      1,      1,      1,      1,      1,  &
                     0,      1,      0,      1,      0,      1,  &
                     0,      1,      0,      1,      0,      1,  &
                     0,      1,      0,      1,      0,      1,  &
                     0,      1,      0,      0,      1,      0,  &
                     0,      1,      0,      0,      1,      0,  &
                     0,      1,      0,      0,      1,      0,  &
                     0,      1,      0,      0,      1,      0,  &
                     0 / 

   DATA VMASK4/      0,      1,      1,      1,      1,      1,  &
                     1,      1,      1,      1,      1,      1,  &
                     1,      1,      1,      1,      1,      1,  &
                     0,      1,      0,      1,      0,      1,  &
                     0,      1,      0,      1,      0,      1,  &
                     0,      1,      0,      1,      0,      1,  &
                     0,      1,      0,      1,      0,      1,  &
                     0,      1,      0,      1,      0,      1,  &
                     0,      1,      0,      1,      0,      1,  &
                     0,      1,      0,      1,      1,      1,  &
                     0 / 

   DATA WRFSIG/ 1.0000, 0.9953, 0.9905, 0.9857, 0.9808, 0.9758,  &
                0.9707, 0.9655, 0.9602, 0.9547, 0.9490, 0.9431,  &
                0.9369, 0.9304, 0.9236, 0.9164, 0.9084, 0.8995,  &
                0.8888, 0.8768, 0.8633, 0.8491, 0.8339, 0.8177,  &
                0.8010, 0.7836, 0.7658, 0.7472, 0.7281, 0.7084,  &
                0.6882, 0.6673, 0.6457, 0.6235, 0.6006, 0.5773,  &
                0.5537, 0.5298, 0.5058, 0.4817, 0.4576, 0.4333,  &
                0.4088, 0.3842, 0.3597, 0.3354, 0.3115, 0.2880,  &
                0.2660, 0.2452, 0.2260, 0.2080, 0.1898, 0.1705,  &
                0.1481, 0.1238, 0.0992, 0.0744, 0.0496, 0.0248,  &
                0.0000 /

!   select vertical resolution
    IF(VRES.EQ.1) VMASK=VMASK1
    IF(VRES.EQ.2) VMASK=VMASK2
    IF(VRES.EQ.3) VMASK=VMASK3
    IF(VRES.EQ.4) VMASK=VMASK4

!   determine the number of output levels and compute the
!   array index to correspond input level with output level
    IF(NLVL.EQ.0)THEN
       DO K=1,61   
          IF(VMASK(K).EQ.1)THEN
             NLVL=NLVL+1
             SMASK(K)=NLVL
          END IF
       END DO

!   compute midpoint sigma level
    ELSE
       NLVL=0
       DO K=1,61   
          IF(VMASK(K).EQ.1)THEN
             NLVL=NLVL+1
             SIGL(NLVL)=0.5*(WRFSIG(K-1)+WRFSIG(K))
             WRITE(50,*) K,SMASK(K),SIGL(NLVL)
          ELSE
             WRITE(50,*) K,SMASK(K)
          END IF
       END DO
    END IF

END SUBROUTINE sigma

!##############################################################################
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!$$$  From: vecrot_rotlat by Tom Black

SUBROUTINE ROTLAT (IM,JM,TPH0D,TLM0D,VLAT,VLON,DRAY,CRAY)

  IMPLICIT NONE
              
  INTEGER, INTENT(IN)  :: IM,JM
  REAL,    INTENT(IN)  :: TPH0D,TLM0D                                                                           
  REAL,    INTENT(IN)  :: VLAT(:,:)    ! LATITUDE OF E-GRID V POINTS (DEGREES)
  REAL,    INTENT(IN)  :: VLON(:,:)    ! LONGITUDE OF E-GRID V POINTS (DEGREES)
  REAL,    INTENT(OUT) :: DRAY(:,:)    ! ROTATION COSINE
  REAL,    INTENT(OUT) :: CRAY(:,:)    ! ROTATION SINE

  REAL,    PARAMETER   :: D2RAD = 1.745329E-2  ! degress to radians

  INTEGER :: I, J
  REAL    :: ERPHI0,ERLAM0,SPHI0,CPHI0,TLAT,TLON
  REAL    :: RELM,SRLM,CRLM,SPH,CPH,CC,TPH,RCTPH                                                                                   

                                                                                         
  ERPHI0=TPH0D*D2RAD
  IF(TLM0D.LT.0.0)THEN
     ERLAM0=(TLM0D+360.)*D2RAD
  ELSE
     ERLAM0=TLM0D*D2RAD
  END IF                                                                               
  SPHI0 = SIN(ERPHI0)
  CPHI0 = COS(ERPHI0)
	
  DO J = 1, JM
  DO I = 1, IM

     TLAT = VLAT(I,J) * D2RAD
     TLON = VLON(I,J) * D2RAD
     RELM = TLON - ERLAM0
     SRLM = SIN(RELM)
     CRLM = COS(RELM)
     SPH  = SIN(TLAT)
     CPH  = COS(TLAT)
     CC = CPH * CRLM
     TPH = ASIN(CPHI0 * SPH - SPHI0 * CC)
     RCTPH = 1.0 / COS(TPH)
     CRAY(I,J) = SPHI0 * SRLM * RCTPH
     DRAY(I,J) = (CPHI0 * CPH + SPHI0 * SPH * CRLM) * RCTPH

  END DO
  END DO

END SUBROUTINE rotlat
