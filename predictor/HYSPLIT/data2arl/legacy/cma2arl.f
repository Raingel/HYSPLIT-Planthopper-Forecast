!$$$  MAIN PROGRAM DOCUMENTATION BLOCK
!
! MAIN PROGRAM: CMA2ARL      DECODE CMA GSM  MODEL FIELD FOR HYSPLIT
!   PRGMMR: DRAXLER          ORG: R/ARL       DATE: 1998-08-26
!
! ABSTRACT:  THIS CODE WRITTEN AT THE AIR RESOURCES LABORATORY ...
! Convert GRIB to ARL format with data organized by time such that a consecutive 
! group of records contains all the variables at the same time.  The ARL format
! consists of an index record followed by data records.  One record per        
! variable per level, then followed by records for the next time period.  All
! records are of fixed length and packed one byte per variable.  Packing
! information is coded in the header portion of each record. 
!
! The program repacks the input data into ARL format in two different modes. 
! If no grid spacing is defined (=0) then the latlon input grid is repacked 
! and written directly to ARL format.  A latlon subgrid may be specified by
! setting the number of ouput grid points to a non-zero value.  The subgrid
! is geolocated by setting the lower left corner point.        
!
! The latlon input data may also be interpolated to a conformal map projection
! by specifying the output grid size (km), grid center latlon, and number of
! grid points in each direction.  The projection is automatically defined 
! depending upon the latitude of the grid center, where Polar Sterographic
! is used for latitudes above 60, Mercator for latitudes less than 30, and 
! Lambert Conformal for intermediate latitudes. The conformal map is at 100km 
! resolution of 100x100 points centered at the command line lat/lon point.
!
! PROGRAM HISTORY LOG:
!   LAST REVISED: 18 Feb 1997 (RRD)
!                  8 Jul 1998 (RRD) - avn extra levels relh
!                  3 Feb 1999 (RRD) - fixed bug in mercator configuration
!                                   - added lambert conformal option
!                                   - prime meridian interpolation patch
!                 12 Nov 1999 (RRD) - pc argument list compatibility
!                 10 Mar 2000 (RRD) - standardized PC/UNIX IO
!                 12 Apr 2000 (RRD) - increased resolution and fields
!                 21 Dec 2000 (RRD) - fortran90 upgrade
!                 07 Mar 2001 (RRD) - GRIB id test
!                 15 Nov 2001 (RRD) - global grid option added
!                 28 Jan 2002 (RRD) - converted avn2arl to cma2arl
!                 10 Nov 2005 (RRD) - error wind rotation >150 hPa
!
! USAGE:  CMA2ARL [-options]
!   INPUT ARGUMENT LIST:
!         -i [input date field]
!         -x [center longitude]
!         -y [center latitude]
!         -g [1:nhem 2:shem 3:global]
!         -n [number of x,y extract grid points]
!   OUTPUT ARGUMENT LIST: none 
!   INPUT FILES:          grib input data files 
!   OUTPUT FILES:
!      unit 20 DATA.CMA - ARL packed data output file
!      unit 30 CFG_CMA  - characteristics of output grid
!      unit 40 CMATIME  - time indicator file
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE:  IBM RS6000
!
!$$$

PROGRAM CMA2ARL

  IMPLICIT NONE

  LOGICAL       :: FTEST
  CHARACTER(80) :: LABEL, FNAME
  INTEGER*8     :: IDATE(4),IFTIME
  INTEGER       :: IARGC, HANDLE, FCOPEN, NARG, KGRID, NUMPTS
  REAL          :: CLAT, CLON

  INTERFACE
    SUBROUTINE XTRACT(IDATE,IFTIME,CLAT,CLON,KGRID,NUMPTS)
    IMPLICIT NONE
    INTEGER*8,INTENT(IN)  :: IDATE(4),IFTIME ! processing date
    INTEGER,INTENT(IN)    :: KGRID           ! projection type
    INTEGER,INTENT(IN)    :: NUMPTS          ! extract grid size
    REAL,   INTENT(IN)    :: CLAT, CLON      ! center position
    END SUBROUTINE xtract
  END INTERFACE

! check for command line arguments
  NARG=IARGC()

  IF(NARG.EQ.0)THEN
     WRITE(*,*)'Converts meteorological data from the global model output from'
     WRITE(*,*)'the China Meteorological Administration.'
     WRITE(*,*)' '
     WRITE(*,*)'Usage: cma2arl [-options]'
     WRITE(*,*)' -i[input date field: yyyymmddhhfff]'
     WRITE(*,*)' -x[extract center longitude]'
     WRITE(*,*)' -y[extract center latitude]'
     WRITE(*,*)' -g[0:conformal 1:nhem 2:shem 3:global]'
     WRITE(*,*)' -n[number of x,y extract grid points]'
     STOP
  END IF

! default values
  CLAT= 60.0
  CLON=-80.0
  KGRID=0 
  NUMPTS=100

! go through each argument
  DO WHILE (NARG.GT.0)

     CALL GETARG(NARG,LABEL)
     SELECT CASE (LABEL(1:2))

!    grib input file name   
     CASE ('-i','-I')
        READ(LABEL(3:),'(A)')FNAME
!       determine processing date from input string
!       year month day cycle forecast
        READ(FNAME,'(I4,3I2,I3)')IDATE,IFTIME   

!    extract grid center longitude 
     CASE ('-x','-X')
        READ(LABEL(3:),'(F10.0)')CLON  

!    extract grid center latitude    
     CASE ('-y','-Y')
        READ(LABEL(3:),'(F10.0)')CLAT  

!    extract grid selection
     CASE ('-g','-G')
        READ(LABEL(3:),'(I1)')KGRID 

!    extract grid dimension
     CASE ('-n','-N')
        READ(LABEL(3:),'(I3)')NUMPTS

     END SELECT
     NARG=NARG-1

  END DO

! main decoding routine (assume one time period per process)
  WRITE(*,*)'started processing: ',FNAME
  CALL XTRACT(IDATE,IFTIME,CLAT,CLON,KGRID,NUMPTS)

END PROGRAM cma2arl



!##############################################################################
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM:  EXTRACT          MAIN ROUTINE FOR DECODING CMA DATA
!   PRGMMR:    ROLAND DRAXLER   ORG: R/E/AR      DATE:96-06-01
!
! ABSTRACT:  THIS CODE WRITTEN AT THE AIR RESOURCES LABORATORY ...
!            EXTRACTS ONE RECORD FROM INPUT GRIB FILE AND ADDS ONE
!            RECORD TO THE OUTPUT FILE CENTERED ABOUT INDICATED LAT/LON
!
! PROGRAM HISTORY LOG:
!   LAST REVISED: 10 Jul 1997 (RRD)
!                 11 Apr 2000 (RRD) - increased vertical resolution
!                 15 Nov 2001 (RRD) - global grid options
!
! USAGE:  CALL XTRACT(IDATE,IFTIME,CLAT,CLON,KGRID,NUMPTS)
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

SUBROUTINE XTRACT(IDATE,IFTIME,CLAT,CLON,KGRID,NUMPTS)

  IMPLICIT NONE

! argument list variables  
  INTEGER*8,INTENT(IN) :: IDATE(4),IFTIME,IPTIME ! processing date
  INTEGER,INTENT(IN)   :: KGRID         ! projection type
  INTEGER,INTENT(IN)   :: NUMPTS        ! extract grid size
  REAL,INTENT(INOUT)   :: CLAT, CLON    ! center position

! input buffer 
  INTEGER, PARAMETER   :: NLON=640
  INTEGER, PARAMETER   :: NLAT=321

! output array limits
  INTEGER, PARAMETER   :: NLVL=14     ! number of levels
  INTEGER, PARAMETER   :: N2DV=6      ! number of 2D variables 
  INTEGER, PARAMETER   :: N3DV=6      ! number of 3D variables

! temporary holding variable
  INTEGER*8    :: VGRIB, LEVEL(2), RECORD, IRET, II, JJ, KK
  CHARACTER(4) :: VCHAR
  REAL         :: CNVRT

! sfc arrays to hold grib, character, and level information
  INTEGER      :: VGRIB0(N2DV)        ! sfc grib ID  
  CHARACTER(4) :: VCHAR0(N2DV)        ! arl variable string
  REAL         :: CNVRT0(N2DV)        ! units conversion factor

! 3d arrays to hold grib, character, and level information
  INTEGER      :: VGRIB1(N3DV)        ! 3d grib variable code
  CHARACTER(4) :: VCHAR1(N3DV)        ! arl variable string
  REAL         :: CNVRT1(N3DV)        ! units conversion factor
  INTEGER      :: SIGL  (NLVL)        ! input/output levels
  INTEGER      :: NVAR  (NLVL)        ! variables each level

! arrays for grib description
  INTEGER      :: KPDS(25)            ! product definition
  INTEGER      :: KGDS(25)            ! grid definition
  INTEGER      :: KPTR(25)

! unpacked input and output array
  REAL*8                   :: RVAR(NLON,NLAT)
  REAL*8                   :: GVAR(NLON,NLAT-1)
  REAL,ALLOCATABLE         :: XVAR(:,:)      

! lat/lon grid conversion equivalence table
  REAL,ALLOCATABLE         :: TLAT(:,:)
  REAL,ALLOCATABLE         :: TLON(:,:)       

! packed output array
  CHARACTER(1),ALLOCATABLE :: CVAR(:)

! file information
  CHARACTER(80):: FNAME, INDEX_DIC
  LOGICAL      :: FTEST

  LOGICAL :: global 
  INTEGER :: kunit 
  INTEGER :: lrec,nrec,nxy,nxp,nyp
  INTEGER :: k,i1,j1,iyr,imo,ida,ihr,ifh,imn 
  INTEGER :: nx,ny,nz,kl,kv,nv,kret,krec,klen
  REAL    :: clon1,clat1,dlon,dlat,gridkm

!-------------------------------------------------------------------------------

! surface variable 
  DATA VGRIB0/     1,     2,    54,   202,   203,  229 /    
  DATA VCHAR0/'PRSS','MSLP','TPP3','U10M','V10M','T02M'/
  DATA CNVRT0/ 1.0  , 1.0  ,0.001 , 1.0  , 1.0  , 1.0  /

! upper level variables
  DATA VGRIB1/    7,    11,    33,    34,    40,    52 /    ! DATA ID
  DATA VCHAR1/'HGTS','TEMP','UWND','VWND','WWND','RELH'/    ! ARL ID
  DATA CNVRT1/  1.0,   1.0,   1.0,   1.0,  0.01,   1.0 /    ! CONVERSION

! set levels for input and output
  DATA SIGL/1000,925,850,700,600,500,400,300,250,200,150,100,70,50/

! lat/lon grid corner (1,1) and increment
  DATA CLAT1/90.0/, CLON1/0.0/, DLAT/0.5625/, DLON/0.5625/

! output record counter
  DATA KREC/0/
  SAVE KREC

!-------------------------------------------------------------------------------
! When dealing with SUN F90 compiler, replace ICHAR below with JCHAR function
! CHARACTER(1)        :: mychr    
! INTEGER, FUNCTION   :: jchar
! JCHAR(MYCHR)=IAND(ICHAR(MYCHR),255)
!-------------------------------------------------------------------------------

  INTERFACE

  SUBROUTINE INTPGL(NLON,NLAT,DATAG,DATAL)
  IMPLICIT NONE
  INTEGER,INTENT(IN) :: NLON,NLAT
  REAL*8,INTENT(IN)  :: DATAG(640,320)
  REAL*8,INTENT(OUT) :: DATAL(640,321)
  END SUBROUTINE intpgl
 
  SUBROUTINE REGRID(GLOBAL,V1,NX1,NY1,V2,NX2,NY2,TLAT,TLON,    &
                    CLAT1,CLON1,DLON,DLAT)
  IMPLICIT NONE
  LOGICAL, INTENT(IN)    :: GLOBAL          ! global lat/lon grid 
  INTEGER, INTENT(IN)    :: NX1, NY1        ! input dimensions
  INTEGER, INTENT(IN)    :: NX2, NY2        ! output dimensions
  REAL*8,  INTENT(IN)    :: V1(:,:)         ! input array
  REAL,    INTENT(OUT)   :: V2(:,:)         ! output array
  REAL ,   INTENT(IN)    :: TLAT(:,:)       ! position for each node 
  REAL ,   INTENT(IN)    :: TLON(:,:)       ! position for each node 
  REAL,    INTENT(IN)    :: CLAT1, CLON1    ! position of corner (1,1)
  REAL,    INTENT(IN)    :: DLAT,  DLON     ! spacing between input grid
  END SUBROUTINE regrid

  SUBROUTINE MKGRID(NX,NY,TLAT,TLON)
  IMPLICIT NONE
  INTEGER, INTENT(IN)  :: NX,NY      ! output grid dimensions
  REAL,    INTENT(OUT) :: TLAT(:,:)  ! position of nodes
  REAL,    INTENT(OUT) :: TLON(:,:)  ! position of nodes
  END SUBROUTINE mkgrid

  SUBROUTINE MAKNDX (N2DV,N3DV,VCHAR0,VCHAR1,LEVEL,NXP,NYP,NZP,CLAT,CLON, &
                     KGRID,GLOBAL,DLAT,DLON,NLAT,NLON)
  IMPLICIT NONE
  INTEGER,      INTENT(IN)   :: n2dv          ! number sfc variables 
  INTEGER,      INTENT(IN)   :: n3dv          ! number of 3d variables
  CHARACTER(4), INTENT(IN)   :: vchar0 (:)    ! sfc variable id
  CHARACTER(4), INTENT(IN)   :: vchar1 (:)    ! 3d variable id
  INTEGER,      INTENT(IN)   :: level  (:)    ! level information
  INTEGER,      INTENT(IN)   :: nxp           ! x dimension
  INTEGER,      INTENT(IN)   :: nyp           ! y dimension
  INTEGER,      INTENT(IN)   :: nzp           ! z dimension
  REAL,         INTENT(IN)   :: clat          ! center grid lat
  REAL,         INTENT(IN)   :: clon          ! center grid lon
  INTEGER,      INTENT(IN)   :: kgrid         ! grid type 
  LOGICAL,      INTENT(IN)   :: global        ! global lat/lon flag
  REAL,         INTENT(IN)   :: dlat          ! lat spacing 
  REAL,         INTENT(IN)   :: dlon          ! lon spacing
  INTEGER,      INTENT(IN)   :: nlat          ! number of lats
  INTEGER,      INTENT(IN)   :: nlon          ! number of lons 
  END SUBROUTINE makndx

  SUBROUTINE ROTATE(NREC,NXP,NYP)
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: NREC       ! number of records per time period
  INTEGER, INTENT(IN) :: NXP,NYP    ! dimensions of real arrays
  END SUBROUTINE rotate

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

  END INTERFACE

!-------------------------------------------------------------------------------

! predefine number of variables aloft - this version assumes all variables
! are available at all levels. Some models drop moisture & wvel near top.

  DO KL=1,NLVL
     NVAR(KL)=N3DV
  END DO

! set up the grid system
  GLOBAL=.FALSE.
  IF(KGRID.EQ.0)THEN
!    conformal extract  
     NXP=NUMPTS
     NYP=NUMPTS
     GRIDKM=100.0
  ELSEIF(KGRID.EQ.1)THEN
!    northern hemisphere polar stereographic
     NXP=257 
     NYP=257
     CLAT=90.0
     CLON=0.0
     GRIDKM=95.25
  ELSEIF(KGRID.EQ.2)THEN
!    southern hemisphere polar stereographic
     NXP=257 
     NYP=257
     CLAT=-90.0
     CLON=0.0
     GRIDKM=95.25
  ELSEIF(KGRID.EQ.3)THEN
!    global latitude-longitude grid (same as input)
     NXP=NLON
     NYP=NLAT
     GRIDKM=0.0   
     GLOBAL=.TRUE.
  END IF

! reserve space for output data arrays
  NXY=NXP*NYP
  ALLOCATE (cvar(nxy))
  ALLOCATE (tlat(nxp,nyp),tlon(nxp,nyp))
  ALLOCATE (xvar(nxp,nyp))

!-------------------------------------------------------------------------------

! create the configuration file if it doesn't exist
  CALL MAKNDX(N2DV,N3DV,VCHAR0,VCHAR1,SIGL,NXP,NYP,NLVL,CLAT,CLON, &
              KGRID,GLOBAL,DLAT,DLON,NLAT,NLON)

! configure the packing routines
  FNAME='CFG_CMA'
  CALL PAKSET(20,FNAME,1,NX,NY,NZ)
  WRITE(*,*)'Set grid from pakset: ',nx,ny,nz

! determine how the input data will be remapped 
  IF(.NOT.GLOBAL) CALL MKGRID(NXP,NYP,TLAT,TLON)

! standard output name for packed data
  INQUIRE(FILE='DATA.CMA',EXIST=FTEST)
  LREC=NXY+50
  OPEN(20,FILE='DATA.CMA',RECL=LREC,ACCESS='DIRECT',FORM='UNFORMATTED')

! index dictionary for cma archive
  INDEX_DIC(1:32)= '/u/nwp/gdbt213/dic/index_nfs_dic'
  LEVEL(1)=1
  KK=1
  RECORD=NLON*NLAT 

!-------------------------------------------------------------------------------

! set the current time from initialization and forecast hour
  IYR=IDATE(1)-2000
  IMO=IDATE(2)
  IDA=IDATE(3)
  IHR=IDATE(4)
  IMN=0
  IFH=IFTIME
  CALL TMPLUS(IYR,IMO,IDA,IHR,IFH)

  DO KL=0,NLVL

     IF(KL.EQ.0)THEN
        NV=N2DV
     ELSE
        NV=N3DV
     END IF

  DO KV=1,NV

     IF(KL.EQ.0)THEN
!       process 2d variable 
        VGRIB=VGRIB0(KV)
        VCHAR=VCHAR0(KV)
        CNVRT=CNVRT0(KV)
        LEVEL(2)=9999
        II=NLON
        JJ=NLAT-1
!       extract variable from the cma archive 
        IF(VGRIB.LE.2)THEN
           JJ=NLAT
           IF(VGRIB.EQ.2) LEVEL(2)=9998
!          surface and mslp already on lat/lon grid 
           CALL GDBRCK('BJGL',IDATE,IFTIME,VGRIB,LEVEL,II,JJ,KK,  &
                        RECORD,RVAR,INDEX_DIC,IRET)
        ELSE
           IPTIME=IFTIME
           IF(VGRIB.EQ.54.AND.IFTIME.EQ.0)IPTIME=3       
           CALL GDBRCK('BJGL',IDATE,IPTIME,VGRIB,LEVEL,II,JJ,KK,  &
                        RECORD,GVAR,INDEX_DIC,IRET)
!          interpolate from gaussian to lat/lon
           CALL INTPGL(NLON,NLAT,GVAR,RVAR)   
        END IF
     ELSE  
!       process 3d variable 
        VGRIB=VGRIB1(KV)
        VCHAR=VCHAR1(KV)
        CNVRT=CNVRT1(KV)
        LEVEL(2)=SIGL(KL)
        II=NLON
        JJ=NLAT
!       extract variable from the cma archive 
        CALL GDBRCK('BJGL',IDATE,IFTIME,VGRIB,LEVEL,II,JJ,KK,  &
                     RECORD,RVAR,INDEX_DIC,IRET)
     END IF


     KREC=KREC+1
!    for the the first record create an index record for pakrec
     IF(KREC.EQ.1)THEN
!       write current output time to special file
        OPEN(40,FILE='CMATIME')
        WRITE(40,'(4I2.2,I3.3)')IYR,IMO,IDA,IHR,IFH
        CLOSE (40)
     END IF

!    interpolate from lat/lon to conformal grid
     CALL REGRID(GLOBAL,RVAR,NLON,NLAT,XVAR,NXP,NYP,TLAT,TLON,    &
                 CLAT1,CLON1,DLON,DLAT)
     IF(CNVRT.NE.1.0) XVAR=XVAR*CNVRT

!    then pack into ARL format and continue
     CALL PAKREC(20,XVAR,CVAR,NXP,NYP,NXY,VCHAR,                  &
                 IYR,IMO,IDA,IHR,IMN,IFH,(KL+1),1)

  END DO
  END DO

! compute number of records per time period in output file
  NREC=N2DV+1
  DO K=1,NLVL
     NREC=NREC+NVAR(K)
  END DO

! rotate vector variables from true to grid orientation
  IF(.NOT.GLOBAL)CALL ROTATE(NREC,NXP,NYP)

! close out time period and write index record
  CALL PAKNDX(20)
  CLOSE (20)

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
!
! USAGE:  CALL MAKNDX(N2DV,N3DV,VCHAR0,VCHAR1,LEVEL,NXP,NYP,NZP,CLAT,CLON, 
!                     KGRID,GLOBAL,DLAT,DLON,NLAT,NLON)
!   INPUT ARGUMENT LIST:    see below
!   OUTPUT ARGUMENT LIST:   see below
!   INPUT FILES:            NONE
!   OUTPUT FILES:           UNIT 30 CFG_CMA output file structure
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE:  IBM RS6000
!
!$$$

SUBROUTINE MAKNDX (N2DV,N3DV,VCHAR0,VCHAR1,LEVEL,NXP,NYP,NZP,CLAT,CLON, &
                   KGRID,GLOBAL,DLAT,DLON,NLAT,NLON)

  IMPLICIT NONE

  INTEGER,      INTENT(IN)   :: n2dv          ! number sfc variables 
  INTEGER,      INTENT(IN)   :: n3dv          ! number of 3d variables
  CHARACTER(4), INTENT(IN)   :: vchar0 (:)    ! sfc variable id
  CHARACTER(4), INTENT(IN)   :: vchar1 (:)    ! 3d variable id
  INTEGER,      INTENT(IN)   :: level  (:)    ! level information
  INTEGER,      INTENT(IN)   :: nxp           ! x dimension
  INTEGER,      INTENT(IN)   :: nyp           ! y dimension
  INTEGER,      INTENT(IN)   :: nzp           ! z dimension
  REAL,         INTENT(IN)   :: clat          ! center grid lat
  REAL,         INTENT(IN)   :: clon          ! center grid lon
  INTEGER,      INTENT(IN)   :: kgrid         ! grid type 
  LOGICAL,      INTENT(IN)   :: global        ! global lat/lon flag
  REAL,         INTENT(IN)   :: dlat          ! lat spacing 
  REAL,         INTENT(IN)   :: dlon          ! lon spacing
  INTEGER,      INTENT(IN)   :: nlat          ! number of lats
  INTEGER,      INTENT(IN)   :: nlon          ! number of lons 
  
  CHARACTER(4)  :: VCHAR(50) ! variable id
  CHARACTER(4)  :: GTYPE     ! grid identification
  CHARACTER(20) :: LABEL(18) ! optional field label

  INTEGER       :: I,N,NL,MVAR  
  REAL          :: SIG, GRIDKM 
  REAL          :: GRIDS(12), PARMAP(9)

  COMMON / SETUP / GRIDS, PARMAP

! optional field label string
  DATA LABEL/'Model Type:','Grid Numb:','Vert Coord:','Pole Lat:',         &
    'Pole Lon:','Ref Lat:','Ref Lon:','Grid Size:','Orientation:',         &
    'Cone Angle:','Sync X Pt:','Sync Y Pt:','Sync Lat:','Sync Lon:',       &
    'Reserved:','Numb X pt:','Numb Y pt:','Numb Levels:'/

! reserve space for output data arrays
  IF(KGRID.EQ.0)THEN
     GRIDKM=100.0
     GTYPE='CMAX'
  ELSEIF(KGRID.EQ.1)THEN
     GRIDKM=95.25
     GTYPE='CMAN'
  ELSEIF(KGRID.EQ.2)THEN
     GRIDKM=95.25
     GTYPE='CMAS'
  ELSEIF(KGRID.EQ.3)THEN
     GRIDKM=0.0   
     GTYPE='CMAG'
  END IF

! grid orientation
  GRIDS(6)=0.0
! delta=x grid size in km
  GRIDS(5)=GRIDKM
! variable reserved for future use
  GRIDS(12)=0.0

  IF(GLOBAL)THEN
!    sync x,y defines lower left grid point 
     GRIDS(8)=1.0 
     GRIDS(9)=1.0 
!    lat/lon of lower left point
     GRIDS(10)=-90.0
     GRIDS(11)=  0.0    
!    latlon grid not global
     IF(NYP.LT.NLAT)GRIDS(10)=CLAT-NYP*DLAT/2.0
     IF(NXP.LT.NLON)GRIDS(11)=CLON-NXP*DLON/2.0
  ELSE
!    synch point in x,y coordintes
     GRIDS(8)=(NXP+1.0)/2.0
     GRIDS(9)=(NYP+1.0)/2.0
!    synch point in lat/lon coordinates
     GRIDS(10)=CLAT
     GRIDS(11)=CLON
  END IF

! defines a global latlon grid
  IF(GLOBAL)THEN

!    pole lat/lon is used to identify the 
!    latlon point of the maximum index
     GRIDS(1)=GRIDS(10)+DLAT*(NYP-1)
     GRIDS(2)=GRIDS(11)+DLON*(NXP-1)
     GRIDS(7)=0.0  

!    the reference lat/lon defines grid spacing  
     GRIDS(3)=DLAT
     GRIDS(4)=DLON

! defines a polar sterographic projection
  ELSEIF(ABS(CLAT).GT.60.0)THEN

!    set the pole position and reference lat/lon
     IF(CLAT.GT.0.0)THEN
        GRIDS(1)=90.0
     ELSE
        GRIDS(1)=-90.0
     END IF

!    pole longtitude (+180 from cut)
     GRIDS(2)=CLON

!    reference lat/lon (at which grid size specified)
     GRIDS(3)=CLAT
!    reference longitude and grid alignment
     GRIDS(4)=CLON

!    tangent latitude
     GRIDS(7)=GRIDS(1)

! defines a mercator projection
  ELSEIF(ABS(CLAT).LT.30.0)THEN

!    pole lat/lon axis through pole
     GRIDS(1)=0.0
     GRIDS(2)=CLON

!    reference lat
     GRIDS(3)=CLAT
!    reference lon
     GRIDS(4)=CLON

!    tangent latitude
     GRIDS(7)=0.0

! defines a lambert conformal projection
  ELSE

!    pole lat/lon axis through pole
     GRIDS(1)=CLAT
     GRIDS(2)=CLON

!    reference lat
     GRIDS(3)=CLAT
!    reference lon
     GRIDS(4)=CLON

!    tangent latitude
     GRIDS(7)=CLAT

  END IF

! write the packer configuration file
  OPEN(30,FILE='CFG_CMA')

! default grid number 99 (field not used)
  WRITE(30,'(A20,A4)')LABEL(1),GTYPE 
  WRITE(30,'(A20,A4)') LABEL(2),'  99'

! coordinate (1:sigma 2:pressure 3:terrain 4:hybrid)
  WRITE(30,'(A20,A4)') LABEL(3), '   2'

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
        SIG=0.0     
        MVAR=N2DV
        VCHAR=VCHAR0
     ELSE
        SIG=LEVEL(NL)
        MVAR=N3DV
        VCHAR=VCHAR1
     END IF

     IF(SIG.LT.1.0)THEN
        WRITE(30,'(A20,F6.5,I3,10(1X,A4))')LABEL(1),   &
        SIG,MVAR,(VCHAR(N),N=1,MVAR)
     ELSEIF(SIG.GE.1.AND.SIG.LT.10.0)THEN
        WRITE(30,'(A20,F6.4,I3,10(1X,A4))')LABEL(1),   &
        SIG,MVAR,(VCHAR(N),N=1,MVAR)
     ELSEIF(SIG.GE.10.AND.SIG.LT.100.0)THEN
        WRITE(30,'(A20,F6.3,I3,10(1X,A4))')LABEL(1),   &
        SIG,MVAR,(VCHAR(N),N=1,MVAR)
     ELSEIF(SIG.GE.100.AND.SIG.LT.1000.0)THEN
        WRITE(30,'(A20,F6.2,I3,10(1X,A4))')LABEL(1),   &
        SIG,MVAR,(VCHAR(N),N=1,MVAR)
     ELSEIF(SIG.GE.1000)THEN
        WRITE(30,'(A20,F6.1,I3,10(1X,A4))')LABEL(1),   &
        SIG,MVAR,(VCHAR(N),N=1,MVAR)
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
  CALL STCM1P(PARMAP,GRIDS(8),GRIDS(9),GRIDS(10),GRIDS(11),                &
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
!
! SUBPROGRAM:  REGRID           INTERPOLATES DATA TO THE OUTPUT GRID
!   PRGMMR:    ROLAND DRAXLER   ORG: R/ARL       DATE:96-06-01
!
! ABSTRACT:  THIS CODE WRITTEN AT THE AIR RESOURCES LABORATORY ...
!            FOR A GIVEN VARIABLE WILL GO THROUGH EACH NODE OF THE OUTPUT
!            GRID AND ITERPOLATE A VALUE TO THAT POINT FROM THE INPUT
!            DATA GRID.  ONLY LINEAR INTERPOLATION METHODS ARE USED.
!
! PROGRAM HISTORY LOG:
!   LAST REVISED: 03 Feb 1999 (RRD) - fixed interpolation around prime
!                 15 Nov 2001 (RRD) - dynamic array allocation
!
! USAGE:  CALL REGRID(GLOBAL,I1,J1,V1,NX1,NY1,V2,NX2,NY2,TLAT,TLON, 
!                     CLAT1,CLON1,DLON,DLAT)
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

SUBROUTINE REGRID(GLOBAL,V1,NX1,NY1,V2,NX2,NY2,TLAT,TLON,    &
                  CLAT1,CLON1,DLON,DLAT)

  IMPLICIT NONE

  LOGICAL, INTENT(IN)    :: GLOBAL          ! global lat/lon grid 
  INTEGER, INTENT(IN)    :: NX1, NY1        ! input dimensions
  INTEGER, INTENT(IN)    :: NX2, NY2        ! output dimensions
  REAL*8,  INTENT(IN)    :: V1(:,:)         ! input array
  REAL,    INTENT(OUT)   :: V2(:,:)         ! output array
  REAL ,   INTENT(IN)    :: TLAT(:,:)       ! position for each node 
  REAL ,   INTENT(IN)    :: TLON(:,:)       ! position for each node 
  REAL,    INTENT(IN)    :: CLAT1, CLON1    ! position of corner (1,1)
  REAL,    INTENT(IN)    :: DLAT,  DLON     ! spacing between input grid
 
  INTEGER :: i,j,ii,jj,ilo,ihi,jlo,jhi
  REAL    :: xp,yp,fxi,fyj,top,bot,temp

! just move data into output array if latlon grid 
  IF(GLOBAL)THEN
     DO J=1,NY2  
!       flip order of vertical array element
        JJ=NY2-J+1
        DO I=1,NX2   
           V2(I,J)=V1(I,JJ)        
        END DO
     END DO
     RETURN 
  END IF

! interpolate values to new grid
  DO I=1,NX2
  DO J=1,NY2

!    compute adjacent index values on grid 1
     XP=1.0+(TLON(I,J)-CLON1)/DLON
     YP=1.0+(CLAT1-TLAT(I,J))/DLAT

!    compute indicies
     ILO=INT(XP)
     IHI=ILO+1
     JLO=INT(YP)
     JHI=JLO+1

!    interpolation fractions (off grid extrapolated)
     FXI=XP-ILO
     FYJ=YP-JLO

!    global grids check for wrap at prime meridian
     IF(IHI.GT.NX1)IHI=1
     IF(JHI.GT.NY1)JHI=1

!    interpolate across at top and bottom
     TOP=(V1(IHI,JHI)-V1(ILO,JHI))*FXI+V1(ILO,JHI)
     BOT=(V1(IHI,JLO)-V1(ILO,JLO))*FXI+V1(ILO,JLO)

!    interpolate between top and bottom
     V2(I,J)=(TOP-BOT)*FYJ+BOT

  END DO
  END DO

END SUBROUTINE regrid



!##############################################################################
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM:  ROTATE           CONVERT WINDS FROM TRUE TO GRID ORIENTATION
!   PRGMMR:    ROLAND DRAXLER   ORG: R/ARL       DATE:96-06-01
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
!   LAST REVISED: 18 Feb 1997 (RRD)
!                 15 Nov 2001 (RRD) - dynamic array allocation
!
! USAGE:  CALL ROTATE(NREC,NXP,NYP)
!   INPUT ARGUMENT LIST:  see below
!   OUTPUT ARGUMENT LIST: none
!   INPUT FILES:          UNIT 20 - DATA.CMA direct access
!   OUTPUT FILES:         UNIT 20 - DATA.CMA direct access
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE:  IBM RS6000
!
!$$$

SUBROUTINE ROTATE(NREC,NXP,NYP)

  IMPLICIT NONE

  INTEGER, INTENT(IN) :: NREC       ! number of records per time period
  INTEGER, INTENT(IN) :: NXP,NYP    ! dimensions of real arrays

! data array
  CHARACTER(1), ALLOCATABLE :: CVAR(:)
  REAL,         ALLOCATABLE :: XVAR(:,:), YVAR(:,:) 

  CHARACTER(50) :: LABEL 
  CHARACTER(4)  :: VARB, VARBX, VARBY
  INTEGER       :: IYR,IMO,IDA,IHR,IFH,IMN
  INTEGER       :: NXY,KL,KG,NEXP,KREC,KSUM,LEVEL
  REAL          :: PREC,VAR1

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

  SUBROUTINE U2GRID(UU,VV,NX,NY)
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: NX, NY      ! array dimensions
  REAL, INTENT(INOUT) :: UU(:,:)     ! u component
  REAL, INTENT(INOUT) :: VV(:,:)     ! v component
  END SUBROUTINE u2grid

  END INTERFACE
!---------------------------------------------------------------------

  IMN=0
  NXY=NXP*NYP
  ALLOCATE (CVAR(NXP*NYP))
  ALLOCATE (XVAR(NXP,NYP),YVAR(NXP,NYP))

  100 FORMAT(7I2,A4,I4,2E14.7)

  KREC=1
  DO WHILE (KREC.LT.NREC)
     READ(20,REC=KREC,ERR=900)LABEL
     READ(LABEL,'(14X,A4)')VARB

     IF(VARB(1:1).EQ.'U')THEN
!       first record of record pair gives u-component variable
        READ(20,REC=KREC)LABEL,CVAR
        READ(LABEL,100)IYR,IMO,IDA,IHR,IFH,KL,KG,VARBX,NEXP,PREC,VAR1
        KSUM=-1
        CALL PAKINP(XVAR,CVAR,NXP,NYP,1,1,NXP,NYP,PREC,NEXP,VAR1,KSUM)

!       second record in pair gives v-component variable
        KREC=KREC+1
        READ(20,REC=KREC)LABEL,CVAR
        READ(LABEL,100)IYR,IMO,IDA,IHR,IFH,KL,KG,VARBY,NEXP,PREC,VAR1
        KSUM=-1
        CALL PAKINP(YVAR,CVAR,NXP,NYP,1,1,NXP,NYP,PREC,NEXP,VAR1,KSUM)

        IF(VARBY(1:1).EQ.'V')THEN
!          both components available then rotate
           LEVEL=KL+1
           CALL U2GRID(XVAR,YVAR,NXP,NYP)

!          rewrite those records into the data file
           CALL PAKREC(20,XVAR,CVAR,NXP,NYP,NXY,VARBX,                     &
                       IYR,IMO,IDA,IHR,IMN,IFH,LEVEL,0)
           CALL PAKREC(20,YVAR,CVAR,NXP,NYP,NXY,VARBY,                     &
                       IYR,IMO,IDA,IHR,IMN,IFH,LEVEL,0)
!          WRITE(*,*)'Rotate: ',VARBX,VARBY,LEVEL
        ELSE
           WRITE(*,*)'Error ROTATE: record pair not U,V'
           STOP
        END IF
     END IF
     KREC=KREC+1

  END DO
  DEALLOCATE (CVAR,XVAR,YVAR)
  WRITE(*,*)'Wind components rotated to grid alignment' 
  RETURN

  900 WRITE(*,*)'Error ROTATE: reading input data'
  STOP

END SUBROUTINE rotate



!##############################################################################
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM:  U2GRID           CONVERT WINDS FROM TRUE TO GRID ORIENTATION
!   PRGMMR:    ROLAND DRAXLER   ORG: R/ARL       DATE:96-06-01
!
! ABSTRACT:  THIS CODE WRITTEN AT THE AIR RESOURCES LABORATORY ...
!            CONVERTS THE COMPONENT WINDS FROM THE LAT/LON GRID RELATIVE
!            TO NORTH TO COMPONENTS RELATIVE TO THE ORIENTATION OF THE
!            OUTPUT GRID AT EACH NODE OF THE OUTPUT GRID
!
! PROGRAM HISTORY LOG:
!   LAST REVISED: 18 Feb 1997 (RRD) 
!                 15 Nov 2001 (RRD) - dynamic array allocation
!
! USAGE:  CALL U2GRID(UU,VV,NX,NY)
!   INPUT ARGUMENT LIST:  see below
!   OUTPUT ARGUMENT LIST: see below
!   INPUT FILES:          NONE
!   OUTPUT FILES:         NONE
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE:  IBM RS6000
!
!$$$

SUBROUTINE U2GRID(UU,VV,NX,NY)

  IMPLICIT NONE

  INTEGER, INTENT(IN) :: NX, NY      ! array dimensions
  REAL, INTENT(INOUT) :: UU(:,:)     ! u component
  REAL, INTENT(INOUT) :: VV(:,:)     ! v component

  INTEGER :: i,j
  REAL    :: ug,vg 
  REAL    :: GRIDS(12), PARMAP(9)

  COMMON/ SETUP / GRIDS, PARMAP

! convert compass winds to grid-orientation
  DO I=1,NX
  DO J=1,NY

     CALL CC2GXY(PARMAP,FLOAT(I),FLOAT(J),UU(I,J),VV(I,J),UG,VG)
     UU(I,J)=UG
     VV(I,J)=VG

  END DO
  END DO

END SUBROUTINE u2grid


!##############################################################################
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
!$$$

SUBROUTINE INTPGL(NLON,NLAT,DATAG,DATAL)
 
  IMPLICIT NONE

  INTEGER,INTENT(IN) :: NLON,NLAT
  REAL*8,INTENT(IN)  :: DATAG(640,320)
  REAL*8,INTENT(OUT) :: DATAL(640,321)

  REAL*8  :: Y,Y1
  INTEGER :: I,J,J1
  REAL*8  :: COLAT(321),GLAT(320)

  DATA GLAT/320*0.0/
  SAVE GLAT,COLAT

! input gaussian latitudes the first time
  IF(GLAT(1).EQ.0.0)THEN
     IF(NLAT.NE.321.OR.NLON.NE.640)THEN
        WRITE(*,*)'ERROR intpgl: gaussian interpolation'
        WRITE(*,*)'input dimensions not correct- ',nlon,nlat
        STOP
     END IF

     open(9,file='gauss.213')
     do i=1,320
        READ(9,*) GLAT(i)
     enddo
     close(9)

     DO I=1,321
        COLAT(I)=90.0-(I-1)*0.5625
     END DO    
  END IF
 
  DO 20 I=1,640
  DO 20 J=2,320
     DO 30 J1=1,320
        IF(COLAT(J).GE.GLAT(J1)) GOTO 40
  30 CONTINUE
     GOTO 20
  40 CONTINUE
     Y=COLAT(J)-GLAT(J1)
     Y=Y/(GLAT(J1-1)-GLAT(J1))
     DATAL(I,J)=DATAG(I,J1)-Y*(DATAG(I,J1)-DATAG(I,J1-1))
  20 CONTINUE

     DO 50 I=1,640
     Y=(COLAT(1)-GLAT(1))/(GLAT(1)-GLAT(2))
     DATAL(I,1)=DATAG(I,1)+Y*(DATAG(I,1)-DATAG(I,2))
     Y1=(COLAT(321)-GLAT(320))/(GLAT(320)-GLAT(319))
     DATAL(I,321)=DATAG(I,320)+Y1*(DATAG(I,320)-DATAG(I,319))
  50 CONTINUE

END SUBROUTINE intpgl 

!#####################################################
! contents of GAUSS.213 follow:

89.5700895506075057
89.0131761310220782
88.4529738367130705
87.8920284453444225
87.3308011797376338
86.7694375145276382
86.2079976214231181
85.6465108479528965
85.0849932009119101
84.5234541489144391
83.9618996497181200
83.4003336387370098
82.8387588197095397
82.2771771114337582
81.7155899132664842
81.1539982697129716
80.5924029761777518
80.0308046490314808
79.4692037732916674
78.9076007358379883
78.3459958490356172
77.7843893678486751
77.2227815024451729
76.6611724276204853
76.0995622899381203
75.5379512132081175
74.9763393027374576
74.4147266486620396
73.8531133285838166
73.2914994096763053
72.7298849503795424
72.1682700017747578
71.6066546087075864
71.0450388107113611
70.4834226427713020
69.9218061359604945
69.3601893179717308
68.7985722135654640
68.2369548449477463
67.6753372320917208
67.1137193930113654
66.5521013439961706
65.9904830998127636
65.4288646738789339
64.8672460784143112
64.3056273245713044
63.7440084225492285
63.1823893816940938
62.6207702105861514
62.0591509171167885
61.4975315085564702
60.9359119916146454
60.3742923724930236
59.8126726569327758
59.2510528502565776
58.6894329574061402
58.1278129829758257
57.5661929312427176
57.0045728061936074
56.4429526115493303
55.8813323507866144
55.3197120271579408
54.7580916437093066
54.1964712032965679
53.6348507085999842
53.0732301621377403
52.5116095662779614
51.9499889232498901
51.3883682351539619
50.8267475039710490
50.2651267315709376
49.7035059197201008
49.1418850700887475
48.5802641842574445
48.0186432637230567
47.4570223099043460
46.8954013241470378
46.3337803077285528
45.7721592618623205
45.2105381877018431
44.6489170863444258
44.0872959588346234
43.5256748061674017
42.9640536292912003
42.4024324291106112
41.8408112064890005
41.2791899622508964
40.7175686971841699
40.1559474120422095
39.5943261075458111
39.0327047843849471
38.4710834432205573
37.9094620846860551
37.3478407093888620
36.7862193179117796
36.2245979108143175
35.6629764886338663
35.1013550518869124
34.5397336010700613
33.9781121366611245
33.4164906591199937
32.8548691688896142
32.2932476663967734
31.7316261520529608
31.1700046262550678
30.6083830893861517
30.0467615418160641
29.4851399839021227
28.9235184159896903
28.3618968384127648
27.8002752514945044
27.2386536555477434
26.6770320508754857
26.1154104377713523
25.5537888165200293
24.9921671873976905
24.4305455506723668
23.8689239066043406
23.3073022554465119
22.7456805974447285
22.1840589328380915
21.6224372618593108
21.0608155847349572
20.4991939016857714
19.9375722129269199
19.3759505186682652
18.8143288191146034
18.2527071144659061
17.6910854049175441
17.1294636906605078
16.5678419718816130
16.0062202487637002
15.4445985214858439
14.8829767902235144
14.3213550551487643
13.7597333164304221
13.1981115742342219
12.6364898287229952
12.0748680800568096
11.5132463283931301
10.9516245738869475
10.3900028166909451
9.82838105695561026
9.26675929482938976
8.70513753045880101
8.14351576398857269
7.58189399556176014
7.02027222531986705
6.45865045340296717
5.89702867994981617
5.33540690509796622
4.77378512898387708
4.21216335174302259
3.65054157351000708
3.08891979441865949
2.52729801460214976
1.96567623419308535
1.40405445332361722
0.842432672125542403
0.280810890730404383
-0.280810890730404383
-0.842432672125542403
-1.40405445332361722
-1.96567623419308535
-2.52729801460214976
-3.08891979441865949
-3.65054157351000708
-4.21216335174302259
-4.77378512898387708
-5.33540690509796622
-5.89702867994981617
-6.45865045340296717
-7.02027222531986705
-7.58189399556176014
-8.14351576398857269
-8.70513753045880101
-9.26675929482938976
-9.82838105695561026
-10.3900028166909451
-10.9516245738869475
-11.5132463283931301
-12.0748680800568096
-12.6364898287229952
-13.1981115742342219
-13.7597333164304221
-14.3213550551487643
-14.8829767902235144
-15.4445985214858439
-16.0062202487637002
-16.5678419718816130
-17.1294636906605078
-17.6910854049175441
-18.2527071144659061
-18.8143288191146034
-19.3759505186682652
-19.9375722129269199
-20.4991939016857714
-21.0608155847349572
-21.6224372618593108
-22.1840589328380915
-22.7456805974447285
-23.3073022554465119
-23.8689239066043406
-24.4305455506723668
-24.9921671873976905
-25.5537888165200293
-26.1154104377713523
-26.6770320508754857
-27.2386536555477434
-27.8002752514945044
-28.3618968384127648
-28.9235184159896903
-29.4851399839021227
-30.0467615418160641
-30.6083830893861517
-31.1700046262550678
-31.7316261520529608
-32.2932476663967734
-32.8548691688896142
-33.4164906591199937
-33.9781121366611245
-34.5397336010700613
-35.1013550518869124
-35.6629764886338663
-36.2245979108143175
-36.7862193179117796
-37.3478407093888620
-37.9094620846860551
-38.4710834432205573
-39.0327047843849471
-39.5943261075458111
-40.1559474120422095
-40.7175686971841699
-41.2791899622508964
-41.8408112064890005
-42.4024324291106112
-42.9640536292912003
-43.5256748061674017
-44.0872959588346234
-44.6489170863444258
-45.2105381877018431
-45.7721592618623205
-46.3337803077285528
-46.8954013241470378
-47.4570223099043460
-48.0186432637230567
-48.5802641842574445
-49.1418850700887475
-49.7035059197201008
-50.2651267315709376
-50.8267475039710490
-51.3883682351539619
-51.9499889232498901
-52.5116095662779614
-53.0732301621377403
-53.6348507085999842
-54.1964712032965679
-54.7580916437093066
-55.3197120271579408
-55.8813323507866144
-56.4429526115493303
-57.0045728061936074
-57.5661929312427176
-58.1278129829758257
-58.6894329574061402
-59.2510528502565776
-59.8126726569327758
-60.3742923724930236
-60.9359119916146454
-61.4975315085564702
-62.0591509171167885
-62.6207702105861514
-63.1823893816940938
-63.7440084225492285
-64.3056273245713044
-64.8672460784143112
-65.4288646738789339
-65.9904830998127636
-66.5521013439961706
-67.1137193930113654
-67.6753372320917208
-68.2369548449477463
-68.7985722135654640
-69.3601893179717308
-69.9218061359604945
-70.4834226427713020
-71.0450388107113611
-71.6066546087075864
-72.1682700017747578
-72.7298849503795424
-73.2914994096763053
-73.8531133285838166
-74.4147266486620396
-74.9763393027374576
-75.5379512132081175
-76.0995622899381203
-76.6611724276204853
-77.2227815024451729
-77.7843893678486751
-78.3459958490356172
-78.9076007358379883
-79.4692037732916674
-80.0308046490314808
-80.5924029761777518
-81.1539982697129716
-81.7155899132664842
-82.2771771114337582
-82.8387588197095397
-83.4003336387370098
-83.9618996497181200
-84.5234541489144391
-85.0849932009119101
-85.6465108479528965
-86.2079976214231181
-86.7694375145276382
-87.3308011797376338
-87.8920284453444225
-88.4529738367130705
-89.0131761310220782
-89.5700895506075057
