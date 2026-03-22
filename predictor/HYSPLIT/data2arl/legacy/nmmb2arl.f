!##############################################################################
!$$$  MAIN PROGRAM DOCUMENTATION BLOCK
!
! MAIN PROGRAM: NMMB2ARL     NONHYDROSTATIC MESOCALE MODEL B-GRIB FOR HYSPLIT
!   PRGMMR: DRAXLER          ORG: R/ARL       DATE: 2002-08-20
!
! ABSTRACT:  THIS CODE WRITTEN AT THE AIR RESOURCES LABORATORY ...
!     Converts output from the nonhydrostactic NMM-B model to hysplit
!     format. Input grib data are on the native model staggered Arakawa
!     B-grid and the hybrid vertical coordinate.  The program is designed
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
!                 22 Nov 2010 (RRD) - conversion to NMM-B grid
!                 14 Jan 2011 (RRD) - handle nested grids
!                 21 Jan 2011 (RRD) - correction to vector nest
!                 01 Mar 2011 (BS)  - VMASK4 for NMM-B
!                                     time-range indicator to get instantaneous fluxes, 
!                                     add cloud cover
!                 01 Jul 2011 (BS)  - change suffix NMMB to NAMS for legacy application
!                 12 Jul 2011 (BS)  - option for TPP1
!                 26 Oct 2011 (BS)  - add 3d pressure field
!                 22 May 2015 (BS)  - stop error codes
!
! USAGE:  NMMB2ARL [-options]
!   INPUT ARGUMENT LIST:
!     GRIB_FILE_NAME - multiple grib files may be defined
!   OUTPUT ARGUMENT LIST:
!     NONE
!   INPUT FILES:
!     GRIB_DATA - grib input data files use special directIO routines
!   OUTPUT FILES:
!     unit 20 DATA.NAMS - ARL packed data output file (legacy name NAMS instead of NMMB)
!     unit 30 CFG.NAMS  - output format configuration file
!     unit 40 NMMBTIME  - processing time indicator file
!     unit 50 MESSAGE   - diagnostic messages
!     unit 60 RAIN.SUM  - accumulated precipitation field
!     unit 70 RAIN.3HR  - difference  precipitation field
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE:  IBM RS6000
!   NOTES:    Use the version of the W3LIB as compiled in the ARL library
!             rather than the NCEP library due to incompatibilities for 
!             argument list variables defined using dynamic allocation
!
!$$$

PROGRAM NMMB2ARL

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
  INTEGER       :: accum           ! precip accumulation time
  INTEGER       :: xopt            ! extraction options      
  REAL          :: glimit(4)       ! grid limits (lat1,lon2,lat2,lon2)

!---------------------------------------------------------------
  INTERFACE
  SUBROUTINE XTRACT(handle,glimit,vres,accum,xopt,rain,krec,kbyte,kret)
  INTEGER,INTENT(IN)       :: HANDLE       ! IO handle
  REAL,   INTENT(INOUT)    :: GLIMIT(4)    ! grid limits (lat1,lon2,lat2,lon2)
  INTEGER,INTENT(IN)       :: VRES         ! vertical resolution
  INTEGER,INTENT(IN)       :: ACCUM        ! precip accumulation
  INTEGER,INTENT(IN)       :: XOPT         ! extraction options 
  INTEGER,INTENT(IN)       :: RAIN         ! precipitation processing
  INTEGER,INTENT(INOUT)    :: KREC         ! output record counter
  INTEGER,INTENT(INOUT)    :: KBYTE        ! input file byte counter
  INTEGER,INTENT(OUT)      :: KRET         ! termination code
  END SUBROUTINE xtract
  END INTERFACE
!---------------------------------------------------------------

! check for command line arguments
  NARG=IARGC()

  IF(NARG.EQ.0)THEN
     WRITE(*,*)'Converts output from the nonhydrostactic mesoscale model to HYSPLIT'
     WRITE(*,*)'format. Input grib data are on the native model staggered Arakawa'
     WRITE(*,*)'B-grid remapped to a Lambert Conformal A-grid projection. The native'
     WRITE(*,*)'hybrid vertical coordinate is used by HYSPLIT. A default profile is' 
     WRITE(*,*)'used unless the file nmmprofile.txt is found in the root directory.'
     WRITE(*,*)' '
     WRITE(*,*)'Usage: nmmb2arl [-options]'
     WRITE(*,*)' -i[primary grib data: file name {required}]'
     WRITE(*,*)' -s[supplemental grib data: file name {optional}]'
     WRITE(*,*)' -g[grid: {missing} = default to egrid domain;'
     WRITE(*,*)'    lower left lat,lon, upper right lat,lon;'
     WRITE(*,*) '   0,0,0,0 = set to square dimensions then optimize;'
     WRITE(*,*)' -p[precip options:  p=previous, {d}=difference]'
     WRITE(*,*)' -a[accumulation time (h) for precip: 1 or (3)]'
     WRITE(*,*)' -v[vertical resolution: 1=all, 2=<700 3=full {4}=full+]'
     WRITE(*,*)' -x[extract: {1}=all, 2=2D variables]'
     STOP
  END IF

! default options
  VRES=4
  ACCUM=3
  RAIN=2
  XOPT=1

! default limits for current operational NAMS
  GLIMIT(1)=  -3.0
  GLIMIT(2)=-146.0
  GLIMIT(3)=  38.0
  GLIMIT(4)=  11.0

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

!    precip accumulation time (h) 'buckets'
     CASE ('-a','-A')
        READ(LABEL(3:),'(I1)') ACCUM
        ACCUM=MAX(1,MIN(ACCUM,1))

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
  WRITE(50,*)'Accumulation     : ',ACCUM
  WRITE(50,*)'Extraction option: ',XOPT
  WRITE(50,*)'------------------------------------'

  IF(NFILE.EQ.0)THEN
    WRITE(*,*)'FATAL ERROR - no input file, possibly other inputs missing'
    STOP 999
  END IF

! process each data file
  DO WHILE (NFILE.GT.0)
     INQUIRE(file=fname(nfile),exist=ftest)

     IF(FTEST)THEN
        KBYTE=0
        HANDLE=FCOPEN(fname(nfile),'r')
        WRITE(*,*)'Started processing: ',fname(nfile)
        CALL XTRACT(handle,glimit,vres,accum,xopt,rain,krec,kbyte,kret)
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

END PROGRAM nmmb2arl


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

SUBROUTINE XTRACT(HANDLE,GLIMIT,VRES,ACCUM,XOPT,RAIN,KREC,KBYTE,KRET)

  IMPLICIT NONE

! argument list variables  
  INTEGER,INTENT(IN)       :: HANDLE       ! IO handle
  REAL,   INTENT(INOUT)    :: GLIMIT(4)    ! grid limits (lat1,lon2,lat2,lon2)
  INTEGER,INTENT(IN)       :: VRES         ! vertical resolution
  INTEGER,INTENT(IN)       :: ACCUM        ! precip accumulation
  INTEGER,INTENT(IN)       :: XOPT         ! extraction options 
  INTEGER,INTENT(IN)       :: RAIN         ! precipitation processing
  INTEGER,INTENT(INOUT)    :: KREC         ! output record counter
  INTEGER,INTENT(INOUT)    :: KBYTE        ! input file byte counter
  INTEGER,INTENT(OUT)      :: KRET         ! termination code

  INTEGER                  :: NUMPTS       ! extract grid size
  REAL                     :: CLAT, CLON   ! center position
  INTEGER                  :: ZERO = 0     ! initialize output file
  INTEGER                  :: SFCP         ! use surface pressure flag

!--------------------------------------------------------------
! The number of levels created by vcgenerator and incorporated into
! the data statement of subroutine sigma. This number may differ
! from the actual number of levels contained within the input data.
 
  INTEGER, PARAMETER        :: MAXLEV=60 

! selection mask for WRF-NMMB hybrid mid-point levels                     
  INTEGER      :: SMASK (MAXLEV) = 0

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
  REAL(8),ALLOCATABLE      :: TLAT(:,:),TLON(:,:)

! latitude and longitudes for input vector array 
  REAL(8),ALLOCATABLE      :: VLAT(:,:),VLON(:,:)

! input grid mass/wind point value on the output grid
  INTEGER, ALLOCATABLE     :: IMGP(:,:),JMGP(:,:)
  INTEGER, ALLOCATABLE     :: IWGP(:,:),JWGP(:,:)

! packed output array
  REAL,        ALLOCATABLE :: SVAR(:,:)  
  REAL,        ALLOCATABLE :: TVAR(:,:)     
  CHARACTER(1),ALLOCATABLE :: CVAR(:)
  CHARACTER(4)             :: model
  CHARACTER(80)            :: fname 
  LOGICAL                  :: ftest, offg

! mapping variables that define the output grid  
  REAL                     :: parmap(9),gridkm
  INTEGER                  :: rotate

  INTEGER :: klat,klon,imin,jmin,noff
  INTEGER :: iyr,imo,ida,ihr,iy0,im0,id0,ih0,itr
  INTEGER :: i,j,k,ii,jj,ifh,imn
  INTEGER :: kvc,nlon,n2dv,nlvl,nlat,n3dv
  INTEGER :: nx,ny,nz,kl,kv,klp,kvp,klen,lrec,nxy,nxp,nyp

  REAL    :: crsum, prsum
  REAL    :: dummy(1),ug,vg,uc,vc
  REAL    :: xcp,ycp,clon1,clat1,clat2,clon2
  REAL    :: x1,y1,x2,y2

!#RLL ----------------------------------------------------------
  REAL(8) :: TPH0D,TLM0D,orient,xlat,xlon,ylat,ylon,glat,glon
  REAL(8) :: parms(3,3),ofstlt,ofstln,dlmd,dphd
  REAL(8) :: ucd,vcd,ugd,vgd,wdofst,rsind,rcosd
  REAL(8),   ALLOCATABLE :: cofset(:,:),sofset(:,:)
!#RLL ----------------------------------------------------------

  SAVE VGRIB0,VCHAR0,CNVRT0,VGRIB1,VCHAR1,CNVRT1,SIG0,STYP,       &
       N2DV,N3DV,NXP,NYP,NXY,TLAT,TLON,RVAR,XVAR,PARMAP,          &
       IWGP,JWGP,IMGP,JMGP,BUFF,CVAR,SVAR,TVAR,NLVL

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

!   forecast time  (kpds(14)=P1, kpds(15)=P2)
!                   max give time since initialization
!                   otherwise avg or accum from P1 to P2
    IFH=MAX(KPDS(14),KPDS(15))

!   Time range: instantaneous (0), average (3), accumulation (4), etc.
    ITR=KPDS(16)
   !write(*,*)'kpds14:',kpds(14),' kpds15:',kpds(15),' kpds16:',kpds(16)

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

!      input grid center latitude(7) longitude(8)
       TPH0D=KGDS(7)/1000.0
       TLM0D=KGDS(8)/1000.0
       WRITE(50,*)'Input grid center: ',TPH0D,TLM0D 
       ORIENT=0.0

!      input grid 1,1 corner point
       CLAT1=KGDS(4)/1000.0
       CLON1=KGDS(5)/1000.0
       WRITE(50,*)'Input grid corner: ',CLAT1,CLON1 

!      grid spacing in degrees
       DPHD=KGDS(9)/1000.0       ! latitude
       DLMD=KGDS(10)/1000.0      ! longtude
       IF(KGDS(9).EQ.0.OR.KGDS(10).EQ.0)THEN
          WRITE(50,*)'Warning: grid spacing not set, using 12-km default!'
          DPHD=0.108
          DLMD=0.126
       END IF

!      check to insure that the grid matches the known 12-km grid
!      when no match, then assume grid is a nest and use values below for parent        
       FTEST=.FALSE.
       IF(NLON.NE.954)             FTEST=.TRUE.
       IF(NLAT.NE.835)             FTEST=.TRUE.
       IF(INT(DLMD*1000.0).NE.126) FTEST=.TRUE.       
       IF(INT(DPHD*1000.0).NE.108) FTEST=.TRUE.
       IF(INT(TPH0D).NE.54)        FTEST=.TRUE.
       IF(INT(TLM0D).NE.-106)      FTEST=.TRUE.

       IF(FTEST)THEN
!         assume that the input grid is a nest
          WRITE(50,*)'Input grid does not match 12 km grid!'
!         insure nest spacing even multiple of parent spacing
          JJ=NINT(0.108/DPHD)
          II=NINT(0.126/DLMD)
!         recompute nested grid spacing to insure correct values 
          DPHD=0.108/JJ
          DLMD=0.126/II
!         set grid dimensions based upon parent
          KLAT=835*JJ                  
          KLON=954*II
          WRITE(50,*)'Nest/Parent ratio (lat,lon): ',JJ,II
       ELSE
          KLAT=NLAT
          KLON=NLON
       END IF

!      estimate of resolution
       GRIDKM=111.1984
       WRITE(50,*)'dLon-DLMD:',DLMD,'   dLat-DPHD:',DPHD
       WRITE(50,*)'dLon-km  :',DLMD*GRIDKM,'   dLat-km  :',DPHD*GRIDKM

!      initialize the rotated lat-lon grid conversion routine
       call rstprm(parms, tph0d, tlm0d, orient)
!      degrees to center lat-lon in rotated grid
       ofstlt=dphd*(1+(klat+1)/2)
       ofstln=dlmd*(1+(klon+1)/2)

!      input grid corner point in full parent grid units
       xlat=clat1 ! double precision
       xlon=clon1
       CALL rer2gd(parms, xlat, xlon, glat, glon)
       imin=(glon+ofstln)/dlmd 
       jmin=(glat+ofstlt)/dphd 
       WRITE(50,*)'Input LL corner point I,J on RLL grid: ',imin,jmin

!      define the location of grid mass and wind points 
       DO I=1,NLON
          ii=i+imin-1
          xlon=ii*dlmd - ofstln          ! mass point
          ylon=(ii+0.5)*dlmd - ofstln    ! vector point
          DO J=1,NLAT
             jj=j+jmin-1
!            lat-lon of the mass points ... the mass point corresponds
!            to the (1,1) grid point on the lower left and this grid will
!            be used to set up the output grid
             xlat=jj*dphd - ofstlt
             call rgd2er (parms,xlat,xlon,tlat(i,j),tlon(i,j))

!            wind point array positions required for wind vector rotation
!            where B-grid values offset by +0.5 in both I and J
             ylat=(jj+0.5)*dphd - ofstlt
             call rgd2er (parms,ylat,ylon,vlat(i,j),vlon(i,j))
          END DO
       END DO

       write(50,*)'Input grid mass corner points ...'
       write(50,101) TLAT(1,nlat),TLAT(nlon,nlat)
       write(50,102) TLAT(1,1),   TLAT(nlon,1)
       write(50,103) TLON(1,nlat),TLON(nlon,nlat)
       write(50,104) TLON(1,1),   TLON(nlon,1)

       write(50,*)
       write(50,*)'Input grid mass corner points ...'
       write(50,101) VLAT(1,nlat),VLAT(nlon,nlat)
       write(50,102) VLAT(1,1),   VLAT(nlon,1)
       write(50,103) VLON(1,nlat),VLON(nlon,nlat)
       write(50,104) VLON(1,1),   VLON(nlon,1)

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
       GRIDKM=NINT(0.5*(CLAT2-CLAT1)*111.1984)
       WRITE(50,*)'Output grid resolution: ',GRIDKM

!      initially define the output grid at central location
       CLAT2=NINT(TLAT(NXP,NYP))
       CLON2=NINT(TLON(NXP,NYP))
       CALL STLMBR(PARMAP,CLAT2,CLON2) 

       XCP=500.0
       YCP=500.0
!      force Lambert projection
       CALL STCM1P(PARMAP,XCP,YCP,CLAT2,CLON2,CLAT2,CLON2,GRIDKM,0.0)
       WRITE(50,*)'Output grid center lat,lon: ',CLAT2,CLON2 

! ................ LON ..................
! .
! .                             XY2
! .       x----------------------x
! L        \     ----------     /
! A         \    |        |    /
! T          \   |        |   /
! .           \  ----------  /
! .            x------------x
! .           XY1                    


!   grid optimization loop
    IF(SUM(GLIMIT).EQ.0.0)THEN

!      initialize corner point from input data
       GLIMIT(1)=TLAT(1,1)
       GLIMIT(2)=TLON(1,1)

!      first guess grid size to cover input data
       CALL CLL2XY(PARMAP,GLIMIT(1),GLIMIT(2),X1,Y1)
       NXP=1+INT((XCP-X1)*2.0)
       NYP=1+INT((YCP-Y1)*2.0)

!      set to square grid
       NXP=MAX(NXP,NYP)
       NYP=MAX(NXP,NYP) 
       WRITE(50,*)'Number x,y: ',NXP,NYP         

!      set first pass grid interation parameters
       NXP=NXP+2
       NYP=NYP+2
       K=MIN(NXP,NYP)/2

!      find the grid limits that are within the input data domain
       gloop : DO WHILE (K.GT.0)
          NXP=NXP-2
          NYP=NYP-2

!         recompute lower left corner
          X1=XCP-(NXP-1.0)/2.0
          Y1=YCP-(NYP-1.0)/2.0
          CALL CXY2LL(PARMAP,X1,Y1,GLIMIT(1),GLIMIT(2))

!         upper right corner
          X2=X1+NXP-1.0
          Y2=Y1+NYP-1.0
          CALL CXY2LL(PARMAP,X2,Y2,GLIMIT(3),GLIMIT(4))

!         determine the I,J on the input grid for every output point
!         along the grid perimiter to insure all output points have a
!         corresponding point on the input grid
          NOFF=0
          OFFG=.FALSE.

!         west edge
          I=INT(X1+1.0)
          DO J=INT(Y1+1.0),INT(Y2)
             CALL CXY2LL(PARMAP,FLOAT(I),FLOAT(J),CLAT,CLON)
             xlat=clat ! double precision required
             xlon=clon 
             CALL rer2gd(parms, xlat, xlon, glat, glon)
!            mass grid point
             ii=(glon+ofstln)/dlmd-imin+1 
             jj=(glat+ofstlt)/dphd-jmin+1 
             IF(II.LT.1.OR.II.GT.NLON.OR.JJ.LT.1.OR.JJ.GT.NLAT)THEN
                OFFG=.TRUE.
                NOFF=NOFF+1
             END IF
          END DO

!         east edge
          I=INT(X2)
          DO J=INT(Y1+1.0),INT(Y2)
             CALL CXY2LL(PARMAP,FLOAT(I),FLOAT(J),CLAT,CLON)
             xlat=clat ! double precision required
             xlon=clon 
             CALL rer2gd(parms, xlat, xlon, glat, glon)
!            mass grid point
             ii=(glon+ofstln)/dlmd-imin+1 
             jj=(glat+ofstlt)/dphd-jmin+1 
             IF(II.LT.1.OR.II.GT.NLON.OR.JJ.LT.1.OR.JJ.GT.NLAT)THEN
                OFFG=.TRUE.
                NOFF=NOFF+1
             END IF
          END DO

!         south edge
          J=INT(Y1+1.0)
          DO I=INT(X1+1.0),INT(X2)
             CALL CXY2LL(PARMAP,FLOAT(I),FLOAT(J),CLAT,CLON)
             xlat=clat ! double precision required
             xlon=clon 
             CALL rer2gd(parms, xlat, xlon, glat, glon)
!            mass grid point
             ii=(glon+ofstln)/dlmd-imin+1 
             jj=(glat+ofstlt)/dphd-jmin+1 
             IF(II.LT.1.OR.II.GT.NLON.OR.JJ.LT.1.OR.JJ.GT.NLAT)THEN
                OFFG=.TRUE.
                NOFF=NOFF+1
             END IF
          END DO

!         north edge
          J=INT(Y2)
          DO I=INT(X1+1.0),INT(X2)
             CALL CXY2LL(PARMAP,FLOAT(I),FLOAT(J),CLAT,CLON)
             xlat=clat ! double precision required
             xlon=clon 
             CALL rer2gd(parms, xlat, xlon, glat, glon)
!            mass grid point
             ii=(glon+ofstln)/dlmd-imin+1 
             jj=(glat+ofstlt)/dphd-jmin+1 
             IF(II.LT.1.OR.II.GT.NLON.OR.JJ.LT.1.OR.JJ.GT.NLAT)THEN
                OFFG=.TRUE.
                NOFF=NOFF+1
             END IF
          END DO

          IF(OFFG)THEN
             WRITE(50,*)NOFF,' points outside of the input grid, pass = ',K
!            keep reducing output grid dimensions until it fits inside input grid
             K=K-1
          ELSE 
             EXIT gloop
          END IF
       END DO gloop

!      possible command line input to skip this processing
       WRITE(50,'(A,2I5)')   ' Number x,y: ',NXP,NYP         
       WRITE(50,'(A,4F10.2)')' Grid limits -g (lat,lon): ',GLIMIT     
       WRITE(50,'(A,4F10.2)')' Grid corner points (x,y): ',X1,Y1,X2,Y2

    ELSE
!      sub grid output (-g command line)
       CALL CLL2XY(PARMAP,GLIMIT(1),GLIMIT(2),X1,Y1)
       CALL CLL2XY(PARMAP,GLIMIT(3),GLIMIT(4),X2,Y2)
       WRITE(50,*)'Subgrid corner (lat,lon): ',GLIMIT     
       WRITE(50,*)'Subgrid corner pts (x,y): ',X1,Y1,X2,Y2
              
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

!#RLL  sine and cosine of the rotation angle 
       ALLOCATE (sofset(NXP,NYP),cofset(NXP,NYP),STAT=KRET)
 
       IMGP=0
       JMGP=0
       IWGP=0
       JWGP=0

       NOFF=0
       OFFG=.FALSE.
       DO J=1,NYP
       DO I=1,NXP
          CALL CXY2LL(PARMAP,FLOAT(I),FLOAT(J),CLAT,CLON)
!#RLL     ----------------------------------------------------------
          xlat=clat ! double precision required
          xlon=clon 
          
!         determine the I,J on the input grid for every output point
!         CALL rer2gd(parms, xlat, xlon, glat, glon)
          CALL rg2mpd(parms, xlat, xlon, glat, glon, wdofst, rcosd, rsind)

!         sine and cosine of rotation angle used later for winds
          cofset(i,j)=rcosd
          sofset(i,j)=rsind    

!         mass grid point
          ii=(glon+ofstln)/dlmd-imin+1 
          jj=(glat+ofstlt)/dphd-jmin+1 
          IMGP(I,J)=MIN(MAX(II,1),NLON)
          JMGP(I,J)=MIN(MAX(JJ,1),NLAT)

!         test location
          IF(II.LT.1.OR.II.GT.NLON.OR.JJ.LT.1.OR.JJ.GT.NLAT)THEN
              OFFG=.TRUE.
              NOFF=NOFF+1
          END IF

!         wind grid point
          ii=(glon+ofstln)/dlmd-imin-0.5 
          jj=(glat+ofstlt)/dphd-jmin-0.5
          IWGP(I,J)=MIN(MAX(II,1),NLON)
          JWGP(I,J)=MIN(MAX(JJ,1),NLAT)
          IF(I.EQ.1.AND.J.EQ.NYP) WRITE(50,*)'Rotation angle at (1,NYP) = ', wdofst
!#RLL     ----------------------------------------------------------
       END DO
       END DO
       IF(OFFG)THEN
          WRITE(50,*)'WARNING: some output points outside of the input grid!'
          WRITE(50,*) NOFF,' points outside of grid, out of ',(NXP*NYP)
       END IF

    END IF

!-------------------------------------------------------------------------------
! determine the variables and levels for processing and allocate the remaining
! array space for the packing and output grid interpolation requirements
!-------------------------------------------------------------------------------

    IF(.NOT.ALLOCATED(CVAR))THEN

!      if increase n2dv, check write formatting (makndx) so fields written on 1 line
       N2DV= 19         ! number of 2D variables  (3-1-11 add TCLD,LCLD,MCLD,HCLD)
       N3DV= 7          ! number of 3D variables
       MODEL='NAMS'    

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
       VCHAR0 = (/ 'SHGT','MSLP','TPP3','T02M','SOLM',           &
                   'RH2M','U10M','V10M','PRSS','LHTF',           &
                   'SHTF','USTR','RGHS','DSWF','PBLH',           &
                   'TCLD','LCLD','MCLD','HCLD'/)
         IF(ACCUM.EQ.1)VCHAR0(3)='TPP1'

       VGRIB0 = (/      7,     2,    61,    11,    86,           &
                       52,    33,    34,     1,   121,           &
                      122,   253,    83,   204,   221,           &
                       71,    73,    74,    75/)

       SIG0 = (/        0,     0,     0,     2,   200,           &
                        2,    10,    10,     0,     0,           &
                        0,     0,     0,     0,     0,           &
                        0,     0,     0,     0/)

       STYP = (/        1,   102,     1,   105,   112,           &
                      105,   105,   105,     1,     1,           &
                        1,     1,     1,     1,     1,           &
                      200,   214,   224,   234/)

       CNVRT0 = (/    1.0,  0.01, 0.001,   1.0,   1.0,           &
                      1.0,   1.0,   1.0,  0.01,   1.0,           &
                      1.0,   1.0,   1.0,   1.0,   1.0,           &
                      1.0,   1.0,   1.0,   1.0/)

!      upper level variable definitions
       VGRIB1 = (/   11,    33,    34,    39,    51,   158,    1/)
       VCHAR1 = (/'TEMP','UWND','VWND','WWND','SPHU','TKEN','PRES'/)
       CNVRT1 = (/  1.0,   1.0,   1.0,  0.01,   1.0 ,  1.0, 0.01/)

!      vertical coordinate system 
!      kvc = 1(sigma); 2(pressure); 3(height); 4(hybrid)
       KVC=4

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
       WRITE(50,'(A)')'   Lev  Var  Var Sigma Fhr1 Fhr2  Value(center)'
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

!      skip non-instantaneous fluxes and total cloud cover 
       IF((KPDS(5).EQ.121.OR.KPDS(5).EQ.122.OR.KPDS(5).EQ.204.OR.  &
           KPDS(5).EQ.71).AND.ITR.NE.0) GOTO 800

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
    KL=SMASK(MAXLEV-KPDS(7)+1)
    IF(KL.NE.0)GO TO 300

!   if all tests fail go and read next grib record
!   WRITE(50,*)'Level match not found: ',VCHAR,'   KPDS(5,6,7): ',KPDS(5:7)
    GO TO 800

!-------------------------------------------------------------------------------
! grib variable matches output list ... continue with processing
!-------------------------------------------------------------------------------

300 CONTINUE

   !write(*,*)'kpds:',kpds

!   basic check TPP1 at 6-h fcst (need diff test for TPP3 since nam bgrd cycs 0 and 12 are TPPT
    IF(ACCUM.EQ.1.AND.KPDS(5).EQ.61.AND.KPDS(15).EQ.6)THEN
       IF(KPDS(14).NE.6-ACCUM)THEN
          WRITE(*,*)'FATAL ERROR - mismatch between P1, P2, and accum: ', &
                                           KPDS(14),KPDS(15),ACCUM
          STOP 888
       END IF
    END IF

!   load the entire grib data record into the buffer
    CALL W3FI63(BUFF,KPDS,KGDS,KBMS,RVAR,KPTR,KRET)
    IF(KRET.NE.0)THEN
       WRITE(50,*)'Error W3FI63: ',KRET
       WRITE(50,*)'Variable: ',VCHAR,'   Level:',KL
       IF(KRET.EQ.99) WRITE(50,*)'Bit map encoded variable'
       WRITE(*,*)'FATAL ERROR: W3FI63 non-zero return code ... see MESSAGE file'
       STOP 777
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

       IF(KL.NE.KLP)THEN
          WRITE(50,*)'U and V components not at the same level: ',KL,KLP
          WRITE(*,*)'FATAL ERROR: U,V components not together!'
          STOP 666
       END IF

       DO J=1,NYP
       DO I=1,NXP

!#RLL     ----------------------------------------------------------
!         wind relative to grid at output location in double precision
          ugd=SVAR(I,J)
          vgd=TVAR(I,J)

!         sine and cosine array
          ucd =  cofset(i,j) * ugd + sofset(i,j) * vgd
          vcd = -sofset(i,j) * ugd + cofset(i,j) * vgd

!         double to single precision
          UC=UCD
          VC=VCD
!#RLL     ----------------------------------------------------------

!         convert compass direction to output grid-orientation 
          CALL CC2GXY(PARMAP,FLOAT(I),FLOAT(J),UC,VC,UG,VG)
          SVAR(I,J)=UG
          TVAR(I,J)=VG
       END DO
       END DO

!      pack and write U-component
       WRITE(50,'(1X,2I5,1X,A,1X,3I5,E12.3)')                                &
             KL,KVP,UCHAR,KPDS(7),KPDS(14),KPDS(15),SVAR(NXP/2,NYP/2)
       CALL PAKREC(20,SVAR,CVAR,NXP,NYP,NXY,UCHAR,IYR,IMO,IDA,IHR,IMN,IFH,   &
                  (KL+1),ZERO)
       KREC=KREC+1

!      pack and write V-component
       WRITE(50,'(1X,2I5,1X,A,1X,3I5,E12.3)')                                &
             KL,KV,VCHAR,KPDS(7),KPDS(14),KPDS(15),TVAR(NXP/2,NYP/2)
       CALL PAKREC(20,TVAR,CVAR,NXP,NYP,NXY,VCHAR,IYR,IMO,IDA,IHR,IMN,IFH,   &
                  (KL+1),ZERO)
       KREC=KREC+1
       ROTATE=0

    ELSEIF(ROTATE.EQ.1)THEN
        CONTINUE

    ELSE
!      pack into ARL format and continue
       WRITE(50,'(1X,2I5,1X,A,1X,3I5,E12.3)')                                &
             KL,KV,VCHAR,KPDS(7),KPDS(14),KPDS(15),SVAR(NXP/2,NYP/2)
       CALL PAKREC(20,SVAR,CVAR,NXP,NYP,NXY,VCHAR,IYR,IMO,IDA,IHR,IMN,IFH,   &
                  (KL+1),ZERO)
       KREC=KREC+1
    END IF

    KLP=KL ! previous level
    KVP=KV ! previous variable

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
!$$$ 

SUBROUTINE SIGMA (SIGL,NLVL,SMASK,VRES)
 
   REAL,    INTENT(OUT)   :: SIGL(:)     ! mid point sigma levels
   INTEGER, INTENT(INOUT) :: NLVL        ! number of sigma levels to output
   INTEGER, INTENT(INOUT) :: SMASK(:)    ! selection mask for output levels
   INTEGER, INTENT(IN)    :: VRES        ! vertical resolution (1-full,2-700,3-reduce,4-nams)

   INTEGER                :: J,K
   REAL                   :: SIG1,SIG2
   LOGICAL                :: FTEST  

   INTEGER, PARAMETER     :: MAXLEV=60  
   REAL,    PARAMETER     :: PDTOP=30000.0
   REAL,    PARAMETER     :: PT = 100.0

   INTEGER :: VMASK (MAXLEV) ! selection mask for sigma levels                     
   INTEGER :: VMASK1(MAXLEV) ! full                      
   INTEGER :: VMASK2(MAXLEV) ! <=700                     
   INTEGER :: VMASK3(MAXLEV) ! reduced resolution from NAMS    
   INTEGER :: VMASK4(MAXLEV) ! operational NAMS   
   REAL    :: WRFSIG(MAXLEV) ! number of sigma levels for WRF-NMMB

!  add one for the ground surface 
   REAL    :: A,  AA(MAXLEV+1)
   REAL    :: B,  BB(MAXLEV+1)

   SAVE VMASK, WRFSIG

!  selection mask to extract all levels        
   DATA VMASK1/      1,      1,      1,      1,      1,      1,  &
                     1,      1,      1,      1,      1,      1,  &
                     1,      1,      1,      1,      1,      1,  &
                     1,      1,      1,      1,      1,      1,  &
                     1,      1,      1,      1,      1,      1,  &
                     1,      1,      1,      1,      1,      1,  &
                     1,      1,      1,      1,      1,      1,  &
                     1,      1,      1,      1,      1,      1,  &
                     1,      1,      1,      1,      1,      1,  &
                     1,      1,      1,      1,      1,      1/    

!  select only low level values below 700 hPa
   DATA VMASK2/      0,      0,      0,      0,      0,      0,  &
                     0,      0,      0,      0,      0,      0,  &
                     0,      0,      0,      0,      0,      0,  &
                     0,      0,      0,      0,      0,      0,  &
                     0,      0,      0,      0,      0,      0,  &
                     0,      0,      1,      1,      1,      1,  &
                     1,      1,      1,      1,      1,      1,  &
                     1,      1,      1,      1,      1,      1,  &
                     1,      1,      1,      1,      1,      1,  &
                     1,      1,      1,      1,      1,      1/     

!  full vertical extent but skipping some levels
   DATA VMASK3/      0,      0,      1,      0,      0,      1,  &
                     0,      0,      1,      0,      0,      1,  &
                     0,      0,      1,      0,      0,      1,  &
                     0,      0,      1,      0,      0,      1,  &
                     0,      0,      1,      0,      0,      1,  &
                     0,      0,      1,      0,      0,      1,  &
                     0,      1,      0,      1,      0,      1,  &
                     0,      1,      0,      1,      0,      1,  &
                     0,      1,      0,      1,      1,      1,  &
                     1,      1,      1,      1,      1,      1/    


!  full vertical extent, skipping fewer levels 
!    (1st 20 then every other gives 24 lvls through 700 mb for US tiles)
   DATA VMASK4/      0,      0,      1,      0,      1,      0,  &
                     1,      0,      1,      0,      1,      0,  &
                     1,      0,      1,      0,      1,      0,  &
                     1,      0,      1,      0,      1,      0,  &
                     1,      0,      1,      0,      1,      0,  &
                     1,      0,      1,      0,      1,      0,  &
                     1,      0,      1,      0,      1,      1,  &
                     1,      1,      1,      1,      1,      1,  &
                     1,      1,      1,      1,      1,      1,  &
                     1,      1,      1,      1,      1,      1/

!  60 LEVEL NMM-B already computed for mid-point values
!  vertical hybrid P=A+B*Psfc, where below WRFSIG=A.B
   DATA WRFSIG/                                                       &
       7.00000,  19.00000,  32.00000,  44.00000,  56.00000,  69.00000,&
      81.00000,  94.00000, 107.00000, 121.00000, 135.00000, 149.00000,&
     164.00000, 180.00000, 198.00000, 217.00000, 238.00000, 261.00000,&
     287.00000, 315.00073, 341.00583, 357.02017, 364.04520, 361.07892,&
     352.11874, 339.16272, 324.20935, 307.25754, 288.30655, 269.35596,&
     250.40527, 231.45401, 211.50194, 192.54840, 174.59247, 157.63353,&
     141.67122, 127.70550, 114.73615, 103.76331,  92.78738,  83.80839,&
      76.82683,  69.84277,  63.85654,  58.86892,  53.88034,  48.89120,&
      44.90158,  39.91147,  35.92096,  31.93012,  27.93897,  24.94757,&
      20.95602,  16.96431,  13.97243,  10.98040,   6.98827,   3.99610/  

!   select vertical resolution
    IF(VRES.EQ.1) VMASK=VMASK1
    IF(VRES.EQ.2) VMASK=VMASK2
    IF(VRES.EQ.3) VMASK=VMASK3
    IF(VRES.EQ.4) VMASK=VMASK4

!   Determine the number of HYSPLIT output levels and compute the
!   array index to correspond GRIB file input level with output level.
!   First pass into this subroutine NLVL will be equal to zero.
    IF(NLVL.EQ.0)THEN
       DO K=MAXLEV,1,-1
          IF(VMASK(K).EQ.1)THEN
             NLVL=NLVL+1
!            The hysplit vertical index starts at one just above the surface
!            while the NMMB index starts at one at the top of the atmosphere
             SMASK(K)=NLVL
          END IF
       END DO

!      look for availability of sigma profile at interface levels 
!      otherwise use the default profile which is already at mid-points
       INQUIRE(FILE='nmmprofile.txt',EXIST=ftest)
       IF(FTEST)THEN
          OPEN(90,FILE='nmmprofile.txt')
          DO K=(MAXLEV+1),1,-1
             READ(90,*)J,SIG1,SIG2
!            P=A+B*(Psfc-Ptop)
             BB(K)=SIG2
             AA(K)=SIG1*PDTOP+PT
          END DO
          CLOSE(90)

!         compute mid-level sigma values (skip first above surface)
          DO K=MAXLEV,2,-1
             A=MAX(PT,0.5*(AA(K)+AA(K-1)))
             B=0.5*(BB(K)+BB(K-1))
             WRFSIG(K)=NINT(A/100.0)+B
          END DO
          WRFSIG(1)=PT/100.0
          WRITE(50,*)'Using sigma levels from nmmprofile.txt'

!         output midpoint levels to a file for diagnostics
!         and to compare with the default levels
          OPEN(90,FILE='hysplit_sigma.txt')
          DO K=1,MAXLEV
             WRITE(90,'(I2,F10.5,A1)') k,wrfsig(k),','
          END DO
          CLOSE(90)

       ELSE
          WRITE(50,*)'Using sigma levels from internal data statement'
       END IF

!   map to output vertical distribution
    ELSE
       WRITE(50,*)' NMMB-level  HYSP-level  HYSP-sigma      Pressure'
       DO K=1,MAXLEV
          IF(SMASK(K).NE.0)THEN
             SIGL(SMASK(K))=WRFSIG(K)
             A=INT(WRFSIG(K))
             B=WRFSIG(K)-A
             WRITE(50,*) K,SMASK(K),SIGL(SMASK(K)),(A+1012.0*B)
          ELSE
             WRITE(50,*) K,SMASK(K),WRFSIG(K),'   skipped '
          END IF
       END DO
    END IF

END SUBROUTINE sigma

!================================================================
! All the following subroutines were developed by Albion Taylor
! (albiontaylor@erols.com) to convert i,j coordinates, latitude-
! longitude, and vectors, between an earth reference and rotated 
! latitude-longitude framework. Eventually these subroutines may
! be placed in their own library.

      subroutine ll2xyz(xlat,xlon,xyz)
      IMPLICIT NONE
! Utility routine to convert latitude and longitude to a
! unit vector from the Earth's center extending through
! the xlat,xlon point on the earth
      real * 8 xyz(3),radpdg
      real * 8 xlat,xlon
      real * 8 clat,clon,slat,slon
      parameter (radpdg=3.14159265358979d0/180.)
        clat=dcos(radpdg*xlat)
        slat=dsin(radpdg*xlat)
        clon=dcos(radpdg*xlon)
        slon=dsin(radpdg*xlon)
        xyz(3)=slat
        xyz(2)=clat*slon
        xyz(1)=clat*clon
        return
      end

      subroutine xyz2ll(xyz,xlat,xlon)
      IMPLICIT NONE
! Utility routine to convert a unit vector from the Earth's center
! to the latitude and longitude of the point on the Earth through
! which the vector would extend.  Inverse of ll2xyz
      real * 8 xlat,xlon
      real * 8 xyz(3),dgprad,slat,clat
      real * 8 clon,slon
      parameter (dgprad=180./3.14159265358979d0)
        clat = dsqrt(xyz(2)**2 + xyz(1)**2)
        slat = xyz(3)
        xlat = dgprad * datan2(slat,clat)
        if (clat .gt. 0.) then
          xlon = dgprad * datan2(xyz(2),xyz(1))
        else
          xlon = 0.
        endif
        return
      end

      subroutine ruvmcd(tlat,tlon,u,v,ruv)
      IMPLICIT NONE
! returns vector pair ruv - first column position, second wind
      real * 8 tlat,tlon,u,v
      real * 8 ruv(3,2),ruv2(3,2)
      real * 8 basis(3,3)
        call matzero(ruv2,3,2)
        ruv2(1,1) = 1.
        ruv2(2,2) = u
        ruv2(3,2) = v
        call rstprm(basis,tlat,tlon,0.d0)
        call matmult(basis,1,ruv2,ruv,2)
      end

      subroutine decruv(ruv,glat,glon,u,v)
      IMPLICIT NONE
! takes vector pair ruv - first column position, second wind
! returns latitude, longitude, and u,v wind components
      real * 8 glat,glon,u,v
      real * 8 ruv(3,2),ruv2(3,2)
      real *8 posvct(3),basis(3,3)
      integer k
        do k=1,3
          posvct(k) = ruv(k,1)
        enddo

        call  mkbasis(basis,posvct)

        call matmult(basis,0,ruv,ruv2,2)
 
        u = ruv2(2,2)
        v = ruv2(3,2)
        call xyz2ll(posvct,glat,glon)
      end

      subroutine matzero(a,nrows,ncols)
      IMPLICIT NONE
! zeroes matrix a
      integer nrows,ncols,k,l
      real * 8 a(nrows,ncols)
        do k=1,nrows
          do l=1,ncols
            a(k,l) = 0.
          enddo
        enddo
        return
      end

      subroutine matmult(a,kt,b,c,ncols)
      IMPLICIT NONE
      real * 8 a(3,3),b(3,ncols),c(3,ncols)
      integer kt,ncols, l,m,n 
        if (kt .eq. 0) then
          do l=1,ncols
            do m=1,3
              c(m,l) = 0.
              do n=1,3
                c(m,l) = c(m,l) + a(n,m)*b(n,l)
              enddo
            enddo
          enddo
        else
          do l=1,ncols
            do m=1,3
              c(m,l) = 0.
              do n=1,3
                c(m,l) = c(m,l) + a(m,n)*b(n,l)
              enddo
            enddo
          enddo
        endif
        return
      end

      subroutine rrotat(parms,kdir,xyzin,xyzout)
      IMPLICIT NONE
! utility routine to rotate geocentric vectors according
! to orthogonal matrix parms.  kdir indicates
! the directon of the rotation
      real * 8 parms(3,3), xyzin(3),xyzout(3)
      integer kdir, k,l
! if kdir == 1, rotate forward
      if (kdir .ge. 0) then
        do k=1,3
          xyzout(k) = 0.
          do l=1,3
            xyzout(k) = xyzout(k) + parms(k,l) * xyzin(l)
          enddo
        enddo
      else
! if kdir == -1, rotate backwards 
        do k=1,3
          xyzout(k) = 0.
          do l=1,3
            xyzout(k) = xyzout(k) + parms(l,k) * xyzin(l)
          enddo
        enddo
      endif
      return
      end

      subroutine mkbasis(parms,posvect)
      IMPLICIT NONE
      real * 8 parms(3,3),posvect(3)
      real * 8 den
      integer k
!The WMO convention can be given this operational definition:
! At the North Pole, face into the wind and report the value of
! the west longitude meridian along which the wind is coming at
! you; at the South Pole do likewise but report the east longitude
! meridian value. This is equivalent to placing the origin of a
! right-handed Cartesian coordinate system on the North Pole with the
! y-axis pointing to the prime (0 degree) meridian and the x-axis
! pointing to the 90 degrees west meridian, and then resolving any
! vector wind at the pole point into components along those axes.
! At the South Pole the coordinate axes are oriented such that the
! y-axis points toward 180 degrees west.
! Those components are the u- and v-values given as the single pair
! of pole point winds in the GRIB format.
        den=0.
        do k=1,3
          den = den + posvect(k)**2
        enddo
        den = dsqrt(den)
! check den != 0
        if (den .lt. 1.d-10 ) then
          parms(1,1)=1.0
          parms(2,1) = 0.0
          parms(3,1) = 0.0
        else
! Column 1 is the position vector, normalized
          do k=1,3
            parms(k,1) = posvect(k)/den
          enddo
         endif
! Column 2 is the East vector, normalized
         den = dsqrt(parms(1,1)**2 + parms(2,1)**2)
         if (den .lt. 1.d-10 ) then
           parms(1,2) = 0.0
           parms(2,2) = - parms(1,3)
           parms(3,2) = 0.0
         else
           parms(1,2) = - parms(2,1)/den
           parms(2,2) = parms(1,1)/den
           parms(3,2) = 0.0
         endif
! Column 3 is the North Vector, normalized
         parms(1,3) = - parms(3,1) * parms(2,2)
         parms(2,3) = parms(3,1) * parms(1,2)
         parms(3,3) = parms(1,1) * parms(2,2) - parms(1,2) * parms(2,1)
         return
      end

      subroutine rstprm( parms, cenlat,cenlon,orient)
      IMPLICIT NONE
! routine to call first, to set parms to adjust the lat=0,lon=0
! point of the model grid to the lat=cenlat,lon=cenlon point
! on the earth.  orient is the angle between the "North"
! pointing meridian through 0,0 on the model grid and the
! North pointing meridian on the earth through cenlat and cenlon
! In most cases, orient is expected to be zero.
! All angles are in degrees, and orient is measured clockwise
! from the Earth's North direction to the model's "North"
! direction.
      real * 8 cenlat,cenlon,orient
      real * 8 parms(3,3),radpdg
      real * 8 ccnlat,scnlat,ccnlon,scnlon,corien,sorien,temp
      integer k
      parameter (radpdg=3.14159265358979d0/180.)
        ccnlat = dcos(radpdg * cenlat)
        scnlat = dsin(radpdg * cenlat)
        ccnlon = dcos(radpdg * cenlon)
        scnlon = dsin(radpdg * cenlon)
! Radial vector through cenlat, cenlon
        parms(3,1) = scnlat
        parms(2,1) = ccnlat * scnlon
        parms(1,1) = ccnlat * ccnlon
! East pointing vector through cenlat, cenlon
        parms(3,2) = 0.
        parms(2,2) = ccnlon
        parms(1,2) = -scnlon
! North pointing vector through cenlat, cenlon
        parms(3,3) = ccnlat
        parms(2,3) = - scnlat * scnlon
        parms(1,3) = - scnlat * ccnlon
! Rotate North and East vectors through orient degrees Clockwise
        corien = dcos(radpdg * orient)
        sorien = dsin(radpdg * orient)
        do k = 1,3
          temp = parms(k,3)
          parms(k,3) = corien * temp + sorien * parms(k,2)
          parms(k,2) = corien * parms(k,2) - sorien * temp
        enddo
        return
      end

      subroutine rgd2er (parms, gdlat, gdlon, erlat, erlon)
      IMPLICIT NONE
! converts model grid "lat - lon" values to earth lat-lon values
! according to information set in parms by rstprm
      real * 8 gdlat, gdlon, erlat, erlon
      real * 8 parms(3,3)
      real * 8 vectrg(3),vectre(3)
        call ll2xyz(gdlat, gdlon, vectrg)
        call rrotat(parms, 1, vectrg, vectre)
        call xyz2ll(vectre, erlat, erlon)
        return
      end

      subroutine rer2gd (parms, erlat, erlon, gdlat, gdlon)
      IMPLICIT NONE
! converts earth lat-lon values to model grid "lat - lon" values
! according to information set in parms by rstprm
! inverse of rgd2er
      real * 8 erlat, erlon, gdlat, gdlon
      real * 8 parms(3,3)
      real * 8 vectrg(3),vectre(3)
        call ll2xyz(erlat, erlon, vectre)
        call rrotat(parms, -1, vectre, vectrg)
        call xyz2ll(vectrg, gdlat, gdlon)
        return
      end

      subroutine ruvm2g (parms,tlat,tlon,uin,vin,glat,glon,uout,vout)
      IMPLICIT NONE
      real * 8 parms(3,3)
      real * 8 ruv(3,2),ruv2(3,2)
      real * 8 tlat,tlon,uin,vin,glat,glon,uout,vout
        call ruvmcd(tlat,tlon,uin,vin,ruv)
! rotate ruv vectors
        call matmult(parms,1,ruv,ruv2,2)
        call decruv (ruv2,glat,glon,uout,vout)
        return
      end

      subroutine ruvg2m (parms,tlat,tlon,uin,vin,glat,glon,uout,vout)
      IMPLICIT NONE
      real * 8 parms(3,3)
      real * 8 ruv(3,2),ruv2(3,2)
      real * 8 tlat,tlon,uin,vin,glat,glon,uout,vout
        call ruvmcd(tlat,tlon,uin,vin,ruv)
! rotate ruv vectors
        call matmult(parms,0,ruv,ruv2,2)
        call decruv (ruv2,glat,glon,uout,vout)
        return
      end

      subroutine rm2gpd(parms, tlat, tlon, glat, glon, wdofst,cofset,sofset)
!  input parms, model tlat,tlon
! output terrestrial latitude and longitude glat,glon
! also output wind bearing offset wdofst in degrees.  Wind bearing
! relative to earth is wind bearing relative to model, plus wdofst.
!  cofset and sofset are cosine and sine of wdofset;
! terrestrial winds gu = cofset * tu + sofset * tv,
!                   gv = -sofset * tu + cofset * tv.
! Similarly, tu = cofset * gu - sofset * gv,
!            tv = sofset * gu + cofset * gv
      IMPLICIT NONE
      real * 8 parms(3,3),basis1(3,3),basis2(3,3),vectre(3),idem(3,3)
      real * 8 tlat, tlon, glat, glon, wdofst,cofset,sofset
      real * 8 dgprad
      parameter (dgprad=180d0/3.14159265358979d0)
      integer k
        call rstprm( basis1, tlat, tlon, 0.d0)
        call matmult(parms,1,basis1,basis2,3)
        do k=1,3
          vectre(k) = basis2(k,1)
        enddo
        call xyz2ll(vectre, glat, glon)
!        write(*,*) 'glat,glon = ',glat,glon
        call mkbasis(basis1,vectre)
        call matmult(basis2,0,basis1,idem,3)
!       write(*,*) idem(1,1),idem(2,1),idem(3,1)
!       write(*,*) idem(1,2),idem(2,2),idem(3,2)
!       write(*,*) idem(1,3),idem(2,3),idem(3,3)
!       write(*,*) '--------------------------------'
        cofset = (idem(2,2) + idem(3,3)) * .5
        sofset = (idem(3,2) - idem(2,3)) * .5
        wdofst = dgprad * datan2 ( sofset, cofset)
!       write(*,*) 'bearing offset = ',wdofst,cofset,sofset
        
        return
      end

      subroutine rg2mpd(parms, glat, glon, tlat, tlon, wdofst,cofset,sofset)
!  input parms, model tlat,tlon
! output terrestrial latitude and longitude glat,glon
! also output wind bearing offset wdofst in degrees.  Wind bearing
! relative to earth is wind bearing relative to model, plus wdofst.
!  cofset and sofset are cosine and sine of wdofset;
! terrestrial winds gu = cofset * tu + sofset * tv,
!                   gv = -sofset * tu + cofset * tv.
! Similarly, tu = cofset * gu - sofset * gv,
!            tv = sofset * gu + cofset * gv
      IMPLICIT NONE
      real * 8 parms(3,3),basis1(3,3),basis2(3,3),vectre(3),idem(3,3)
      real * 8 tlat, tlon, glat, glon, wdofst,cofset,sofset
      real * 8 dgprad
      parameter (dgprad=180d0/3.14159265358979d0)
      integer k
        call rstprm( basis1, glat, glon, 0.d0)
        call matmult(parms,0,basis1,basis2,3)
        do k=1,3
          vectre(k) = basis2(k,1)
        enddo
        call xyz2ll(vectre, tlat, tlon)
!        write(*,*) 'tlat,tlon = ',tlat,tlon
        call mkbasis(basis1,vectre)
        call matmult(basis2,0,basis1,idem,3)
!       write(*,*) idem(1,1),idem(2,1),idem(3,1)
!       write(*,*) idem(1,2),idem(2,2),idem(3,2)
!       write(*,*) idem(1,3),idem(2,3),idem(3,3)
!       write(*,*) '--------------------------------'
        cofset = (idem(2,2) + idem(3,3)) * .5
        sofset = - (idem(3,2) - idem(2,3)) * .5
        wdofst = dgprad * datan2 ( sofset, cofset)
!       write(*,*) 'bearing offset = ',wdofst,cofset,sofset

        return
      end
