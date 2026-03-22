!$$$  MAIN PROGRAM DOCUMENTATION BLOCK
!
! MAIN PROGRAM: HRRR@ARL     DECODE GRIB NAM12K MODEL FIELD FOR HYSPLIT
!   PRGMMR: STUNDER          ORG: R/ARL       DATE: 2017-05 10
!
! ABSTRACT:  THIS CODE WRITTEN AT THE AIR RESOURCES LABORATORY ...
! Convert GRIB to ARL format with data organized by time such that a consecutive 
! group of records contains all the variables at the same time.  The ARL format
! consists of an index record followed by data records.  One record per        
! variable per level, then followed by records for the next time period.  All
! records are of fixed length and packed one byte per variable.  Packing
! information is coded in the header portion of each record. 
!
! Input grib2 is on a Lambert Conoformal grid.
!
! This program taken from hysplit_gdas2arl_g2
!     !!!!!!!!!!!!!! There may be remnants of lat-lon grid here !!!!!!!!!!!!!!!!!!!
!    4-27-16 - add START=kret to ALLOCATE lines and error ckecking
!
! PROGRAM HISTORY LOG:
!                 19 Nov 2014 (BS)  - from gdas2arl_g2
!   LAST REVISED: 13 Jan 2016 (BS)  - created
!   LAST REVISED: 20 Jan 2016 (BS)  - nam para (nam.t00z.awphys06.tm00.grib2)
!   LAST REVISED: 24 Mar 2017 (BS)  - from nmmb (hybrid) to hrrr (sigma)
!                 06 Apr 2022 (SYZ) - merge with a version requiring idx file.
!                                     remove unused variables and GOTO statements.
!                                     fix array index out-of-bounds error.
!                 07 Apr 2022 (SYZ) - bug fix in finding 3d parameter.
!
! USAGE:  HRRR2ARL [-options]
!   INPUT ARGUMENT LIST:
!         -i [grib input file]
!         -z [zero initialization of output 0:no {1}:yes]
!         -m [model]
!         -v [vertical resolution: 1=all, 2=<700 3=full {4}=full+]'

!   OUTPUT ARGUMENT LIST: none 
!   INPUT FILES:          grib input data files 
!   OUTPUT FILES:
!      unit 20 DATA.HRRR- ARL packed data output file
!      unit 30 CFG_HRRR - characteristics of output grid
!      unit 40 HRRRTIME  - time indicator file
!      unit 50 MESSAGE   - diagnostic messages       !**** is this here? ****!
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE:  IBM RS6000
!
!$$$

PROGRAM HRRR2ARL

  IMPLICIT NONE

  LOGICAL       :: FTEST
  CHARACTER(80) :: LABEL, GNAME, GNAMEINDX
  INTEGER       :: ZERO, IARGC, NARG
  INTEGER       :: vres                   !  vertical resolution flag
 !INTEGER       :: kvc             ! vertical coordinate index

  CHARACTER(4) :: GTYPE

  INTERFACE

    SUBROUTINE XTRACT(GNAME,GNAMEINDX,ZERO,GTYPE,VRES)
    IMPLICIT NONE
    CHARACTER(80),INTENT(IN) :: GNAME        ! grib2 filename
    CHARACTER(80),INTENT(IN) :: GNAMEINDX    ! grib2 index file filename
    INTEGER,INTENT(IN)       :: ZERO         ! initialize output file
    CHARACTER(4), INTENT(IN) :: GTYPE         ! grid identification (model)
    INTEGER,INTENT(IN)       :: VRES         ! vertical resolution
    END SUBROUTINE xtract

  END INTERFACE

! check for command line arguments
  NARG=IARGC()

  IF(NARG.EQ.0)THEN
     WRITE(*,*)'Usage: hrrr2arl [-options]'
     WRITE(*,*)' -i[grib input file]'
     WRITE(*,*)' -m[model (HRRR)]'            
     WRITE(*,*)' -p[precip options:  p=previous, {d}=difference]'
     WRITE(*,*)' -v[vertical resolution: 1=all, 2=<700 3=full {4}=full+]'
     WRITE(*,*)' -z[zero initialization of output 0:no {1}:yes]'
     STOP
  END IF

! default values
  ZERO=1
  GTYPE='HRRR'
  VRES=4
 !KVC=1    ! 1=sigma, 2=pressure, 4=hybrid

! go through each argument
  DO WHILE (NARG.GT.0)

     CALL GETARG(NARG,LABEL)
     SELECT CASE (LABEL(1:2))

!    grib input file name   
     CASE ('-i','-I')
        READ(LABEL(3:),'(A)')GNAME
        GNAMEINDX=GNAME//'.idx'

!    file initialization
     CASE ('-z','-Z')
        READ(LABEL(3:),'(I1)')ZERO

!    grib input file name   
     CASE ('-m','-M')
        READ(LABEL(3:),'(A)')GTYPE

!    vertical resolution
     CASE ('-v','-V')
        READ(LABEL(3:),'(I1)') VRES
        VRES=MAX(1,MIN(VRES,4))

     END SELECT
     NARG=NARG-1

  END DO

! diagnostic message file
  OPEN(50,FILE='MESSAGE')
  WRITE(50,*)'------------------------------------'
  WRITE(50,*)'COMMAND LINE OPTIONS'
  WRITE(50,*)'Vertical grid pts: ',VRES
  WRITE(50,*)'------------------------------------'

! open input data file
  INQUIRE(FILE=GNAME,EXIST=FTEST)
  IF(FTEST)THEN

     INQUIRE(FILE=GNAMEINDX,EXIST=FTEST)
     IF(FTEST)THEN
!    main decoding routine (assume one time period per process)
     WRITE(*,*)'started processing: ',GNAME
     CALL XTRACT(GNAME,GNAMEINDX,ZERO,GTYPE,VRES)
     ELSE
     WRITE(*,*)'File not found:',GNAMEINDX
     STOP
     END IF

!900  CONTINUE
  ELSE
     WRITE(*,*)'File not found:',GNAME
     STOP
  END IF

  WRITE(*,*)'ended processing: ',GNAME

END PROGRAM hrrr2arl

!##############################################################################
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM:  EXTRACT          MAIN ROUTINE FOR DECODING GFS GRIB DATA
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
!                 10 Jul 2002 (BJS) - add fields, levels
!                 25 Feb 2003 (RRD) - ichar function replacement
!                 05 Jul 2007 (BS)  - grib2
!                 09 Dec 2015 (BS)  - sref
!
! USAGE:  CALL XTRACT(GNAME,GNAMEINDX,,ZERO,GTYPE)
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

SUBROUTINE XTRACT(GNAME,GNAMEINDX,ZERO,GTYPE,VRES)

  USE GRIB_MOD

  IMPLICIT NONE

!--------------------------------------------------------------
! The number of levels created by vcgenerator and incorporated into
! the data statement of subroutine sigma. This number may differ
! from the actual number of levels contained within the input data.

  INTEGER, PARAMETER        :: MAXLEV=50

! selection mask for WRF-NMMB hybrid mid-point levels
  INTEGER      :: SMASK (MAXLEV) = 0

! argument list variables  
  CHARACTER(80),INTENT(IN) :: GNAME        ! grib2 filename
  CHARACTER(80),INTENT(IN) :: GNAMEINDX    ! grib2 index file filename
  INTEGER,INTENT(IN)       :: ZERO         ! initialize output file
  INTEGER,INTENT(IN)       :: VRES         ! vertical resolution

! input buffer 
  INTEGER :: NXP,NYP,NLVL

! output array limits
  INTEGER, PARAMETER   :: N2DV=18     ! number of 2D variables  
  INTEGER, PARAMETER   :: N3DV=8      ! number of 3D variables 

! temporary holding variable
  INTEGER      :: VGRIB
  CHARACTER(4) :: VCHAR
  REAL         :: CNVRT
  INTEGER      :: DISC
  INTEGER      :: CAT 

  INTEGER :: VER

! sfc arrays to hold grib, character, and level information
  INTEGER      :: VGRIB0(N2DV)        ! sfc grib ID          (ipdtmpl(2)) 
  INTEGER      :: STYP  (N2DV)        ! sfc variable code    (ipdtmpl(10)
  INTEGER      :: SIG0  (N2DV)        ! sfc level code       (ipdtmpl(12)
  INTEGER      :: DISC0 (N2DV)        ! sfc grib2 discipline (listsec0(1))
  INTEGER      :: CAT0  (N2DV)        ! sfc grib2 category   (ipdtmpl(1))
  CHARACTER(4) :: VCHAR0(N2DV)        ! arl variable string
  REAL         :: CNVRT0(N2DV)        ! units conversion factor

! 3d arrays to hold grib, character, and level information
  INTEGER      :: VGRIB1(N3DV)        ! 3d grib variable code
  CHARACTER(4) :: VCHAR1(N3DV)        ! arl variable string
  REAL         :: CNVRT1(N3DV)        ! units conversion factor
  INTEGER      :: DISC1 (N3DV)        ! grib2 discipline 
  INTEGER      :: CAT1  (N3DV)        ! grib2 category   
  INTEGER,ALLOCATABLE :: SIGLIN(:)    ! input levels
  REAL,   ALLOCATABLE :: SIGL(:)      ! output levels
  INTEGER,ALLOCATABLE :: NVAR(:)      ! variables each level

! inputs to getgb2
  INTEGER,DIMENSION(200) :: JIDS,JPDT,JGDT
  INTEGER      :: J,JDISC,JPDTN,JGDTN   

! for dumping "message number" vs. "record number"
       ! message number per wgrib2 (u and v are usually msg.1 and msg.2)
  INTEGER      :: JMSG              
  CHARACTER(2) :: SFX

! output from getgb2  (see module gribmod.f)
  TYPE(GRIBFIELD) :: GFLD
  INTEGER         :: IRET

! unpacked input and output array
  REAL,ALLOCATABLE         :: RVAR(:,:)
  REAL,ALLOCATABLE         :: XVAR(:,:)      

! packed output array
  CHARACTER(1),ALLOCATABLE :: CVAR(:)

! DIFW,DIFR
  REAL,ALLOCATABLE         :: DIFF(:,:),DUMW(:,:)

! for vertical velocity difference
  REAL    :: PREC,VAR1
  INTEGER :: NEXP,KSUM

! pass packing precision information (15 May 2014)
  COMMON / PAKVAL / PREC,NEXP,VAR1

! file information
  CHARACTER(80):: FNAME
  LOGICAL      :: FTEST,DONEQ,FOUNDQ

  INTEGER :: kvarb,kunit,ltype,ilevel,kdisc,kcat
  INTEGER :: lrec,nrec,nxy    
  INTEGER :: k,iyr,imo,ida,ihr,ifh,imn
  INTEGER :: iyrv,imov,idav,ihrv,imnv,macc0,macc1
  INTEGER :: nx,ny,nz,kl,kv,kret,krec
  REAL    :: lat1,lon1,lad,lov,gridkm,latin1,lo2
  INTEGER :: kvc

! diagnostic
!  integer :: ii,jj

! allow -m option on command line to specify model which is in output INDX record
  CHARACTER(4) :: gtype
  CHARACTER(9) :: fout
  CHARACTER(8) :: ftime

! remap array from one to two dimensions
  REAL, ALLOCATABLE ::  SVAR(:)
  INTEGER SHAPE(2)

! diagnostics
  REAL :: firstval,lastval,fldmax,fldmin

!-------------------------------------------------------------------------------
! note SOLM is in sref:  2,0,192,10,106,1.0
!      SOLW is in nam12: 2,0,  3, 0,106,1.0
!      SOLM is in nams:  2,0,  3. 0,106,1.0
! SURFACE VARIABLES
!    to get these values Barbara runs wgrib2 to find the message number,
!    then runs her "wgrib2_myinv" that prints listsec0, ipdtmpl, etc.
!    and gets the values for the given message number
!    instead probably could also use wgrib2 for this
!  discipline (listsec0(1))
  DATA DISC0/   0,     0,     0,     0,     0, &
                0,     0,     0,     0,     0, &
                0,     0,     2,     0,     0, & 
                0,     0,     0       /
!  category   (ipdtmpl(1))
  DATA CAT0/    3,     3,     1,     0,     0, &
                1,     2,     2,     3,     0, &
                0,     2,     0,     4,     3, &
                6,    16,     7       /
!  parameter (ipdtmpl(2))
  DATA VGRIB0/  5,   198,     8,     0,     0, &
                1,     2,     3,     0,    10, &
               11,    30,     1,     7,    18, &
                1,   196,     6       /
!  level id (ipdtmpl(12))
  DATA SIG0/    0,     0,     0,     0,     2, &
                2,    10,    10,     0,     0, &
                0,     0,     0,     0,     0, &
                0,     0, 25500       /
    ! NOTE initial ARL hrrr2arl program got last CAPE in file
    !      that is 255 to 0 mb above ground, so force this here
    !      other choices 90 or 180 with STYP=108, 
    !      or 0 with STYP=1
!  type (ipdtmpl(10)
  DATA STYP/    1,   101,     1,     0,   103, &
              103,   103,   103,     1,     1, &
                1,     1,     1,     1,     1, &
               10,    10,   108       /
!  arl id
  DATA VCHAR0  &
          /'SHGT','MSLP','TPP1','DIFR','T02M', &
           'RH2M','U10M','V10M','PRSS','LHTF', &
           'SHTF','USTR','RGHS','DSWF','PBLH', &
           'TCLD','REFC','CAPE'       /
!  wgrib2 (ncep) notation
          !   HGT, PRMSL,  APCP,          TMP, 
          !    RH,  UGRD,  VGRD,  PRES, LHTFL, 
          ! SHTFL, FRICV,  SFCR, DSWRF, HPBL,
          !  TCDC,  REFC,  CAPE

!  conversion factor
  DATA CNVRT0  &
           / 1.0 , 0.01 ,0.001 ,0.001,  1.0  , &
             1.0 ,  1.0 ,  1.0 , 0.01 , 1.0  , &
             1.0 ,  1.0 ,  1.0 , 1.0 ,  1.0  , &
             1.0 ,  1.0 ,  1.0        /

! UPPER LEVEL VARIABLES
  DATA DISC1/      0,     0,     0,     0,     0,     0,     0,     0/    ! DISCIPLINE
  DATA CAT1/       0,     2,     2,     2,     0,     1,    19,     3/    ! CATEGORY
  DATA VGRIB1/     0,     2,     3,     8,     0,     0,    11,     0/    ! PARAMETER ID
  DATA VCHAR1/'TEMP','UWND','VWND','WWND','DIFW','SPHU','TKEN','PRES'/    ! ARL ID
  DATA CNVRT1/   1.0,   1.0,   1.0,  0.01,  0.01,   1.0,   1.0,  0.01/    ! CONVERSION

! output record counter
  DATA KREC/0/
  SAVE KREC

!-------------------------------------------------------------------------------
! When dealing with some F90 compiler, replace ICHAR below with JCHAR function
! 8-29-14 comment out, per compiler, this not used
 !CHARACTER(1)        :: mychr    
 !INTEGER             :: jchar
 !JCHAR(MYCHR)=IAND(ICHAR(MYCHR),255)
!-------------------------------------------------------------------------------

  INTERFACE

  SUBROUTINE MAKNDX (N2DV,NVAR,VCHAR0,VCHAR1,LEVEL,NXP,NYP,NZP,         &
                     LAT1,LON1,LAD,LOV,GRIDKM,LATIN1,GTYPE,LO2,KVC)
  IMPLICIT NONE
  INTEGER,      INTENT(IN)   :: n2dv          ! number sfc variables 
  INTEGER,      INTENT(IN)   :: nvar   (:)    ! number of 3d variables per level
  CHARACTER(4), INTENT(IN)   :: vchar0 (:)    ! sfc variable id
  CHARACTER(4), INTENT(IN)   :: vchar1 (:)    ! 3d variable id
  REAL,         INTENT(IN)   :: level  (:)    ! level information
  INTEGER,      INTENT(IN)   :: nxp           ! x dimension
  INTEGER,      INTENT(IN)   :: nyp           ! y dimension
  INTEGER,      INTENT(IN)   :: nzp           ! z dimension
  REAL,         INTENT(IN)   :: lat1          ! latitude of 1st grid poin
  REAL,         INTENT(IN)   :: lon1          ! longitude of 1st grid point
  REAL,         INTENT(IN)   :: lad           ! Latitude where Dx and Dy are specified
  REAL,         INTENT(IN)   :: lov           ! longitude of meridian parallel to y-axis
  REAL,         INTENT(IN)   :: gridkm        ! grid spacing
  REAL,         INTENT(IN)   :: latin1        ! latitude at which the secant cone is cut
  CHARACTER(4), INTENT(IN)   :: gtype         ! grid identification (model)
  REAL,         INTENT(IN)   :: lo2           ! longitude of last grid point
  INTEGER,      INTENT(IN)   :: kvc           ! vertical coordinate index
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
 
  SUBROUTINE SIGMA (SIGL,NLVL,SMASK,VRES,KVC)
  REAL,    INTENT(OUT)   :: SIGL(:)     ! mid point sigma levels 1 = bottom
  INTEGER, INTENT(INOUT) :: NLVL        ! number of sigma levels to ouput
  INTEGER, INTENT(INOUT) :: SMASK(:)    ! number of sigma levels to ouput
  INTEGER, INTENT(IN)    :: VRES        ! vertical resolution
  INTEGER, INTENT(OUT)   :: kvc         ! vertical coordinate index
  END SUBROUTINE sigma

  END INTERFACE

!-------------------------------------------------------------------------------

! initialize hysplit library date-time routines
  CALL TMINIT

! set up the grid system (gdas2arl defined nxp,nyp,gridkm here)
!   read 1st record in file to read grid specs

!-------------------------------------------------------------------------------
! open grib2 file
    WRITE(*,*)'Opened file for input: ',GNAME
    !   open grib2 byte-addressable file for read-only
    !   /nwprod/lib/sorc/g2/grib2.doc
    KUNIT=10
    CALL BAOPENR(10,GNAME,IRET)
    IF(IRET.NE.0)THEN
       WRITE(*,*)'Error BAOPENR: ',IRET
       STOP 76
    END IF
    
! open grib2 index record file
    WRITE(*,*)'Opened file for input: ',GNAMEINDX
    !   open grib2 byte-addressable file for read-only
    !   /nwprod/lib/sorc/g2/grib2.doc
    KUNIT=11
    CALL BAOPENR(11,GNAMEINDX,IRET)
    IF(IRET.NE.0)THEN
       WRITE(*,*)'Error BAOPENR: ',IRET
       STOP 76
    END IF
    
!   set GRIB2 field identification values to search for 
      j=0		! number of fields to skip  (need this outside loop)
      jmsg=0            ! message number in wgrib2 terminology

!   set GRIB2 field identification values to search for 
      jdisc=-1		! grib2 discipline number (-1=accept any discipline)
      jids=-9999	! -9999 is wildcard
      jpdtn=-1		! product definition template number (-1=accept any)
      jpdt=-9999	! array ov values defining the product definition 
                        !   template 4.N of the field for which to search
      jgdtn=-1		! grid definition template number (M)
      jgdt=-9999	! array of values defining the grid definition template

!   -----------------------------------------
!   read grib2 record to get grid parameters
!   -----------------------------------------
!     apparently with all the defaults above, this just reads record by record
!     could re-do program to only (and directly) get the needed fields)
    CALL GETGB2(10,11,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT,.FALSE.,J,GFLD,IRET)
    IF(IRET.NE.0)THEN
       WRITE(*,*)'ERROR getgb2: ',IRET
       WRITE(*,*)'           j: ',J
     ! close if there is an error in GETGB2
       CALL BACLOSE(10,IRET)
       IF(IRET.NE.0)THEN
          WRITE(*,*)'ERROR baclose gribfile: ',IRET
       END IF
       CALL BACLOSE(11,IRET)
       IF(IRET.NE.0)THEN
          WRITE(*,*)'ERROR baclose grib index file: ',IRET
       END IF
       STOP 777
    END IF
 
! ARL-packing method cannot have a bitmap
  IF(GFLD%IBMAP.NE.255)THEN
    WRITE(*,*)'FATAL - This program does not support bitmap'
    STOP 277
  END IF

! reset j so next time run GETGB2, it starts at beginning of file
  j=0		! number of fields to skip  (need this outside loop)

! diagnostic
 !write(*,'(A,40I10)')'very first igdtmpl:',gfld%igdtmpl
 !write(*,'(A,40I6)')'very first ipdtmpl:',gfld%ipdtmpl

!-------------------------------------------------------------------------------
! determine number of sigma levels
!-------------------------------------------------------------------------------
  NLVL=0
! 1st call sigma, force nlvl=0
  CALL SIGMA (SIGL,NLVL,SMASK,VRES,KVC)
  ALLOCATE (SIGL(NLVL),STAT=kret)
  IF(KRET.NE.0)THEN
    WRITE(*,*)'FATAL ERROR - nmmb2arl.f - allocate SIGL:',NLVL
    STOP 790
  END IF
  ALLOCATE (SIGLIN(NLVL),STAT=kret)
  IF(KRET.NE.0)THEN
    WRITE(*,*)'FATAL ERROR - nmmb2arl.f - allocate SIGLIN:',NLVL
    STOP 791
  END IF
  WRITE(50,*)'Number of output sigma levels: ',NLVL
! now nlvl is known
  CALL SIGMA (SIGL,NLVL,SMASK,VRES,KVC)

  ALLOCATE (NVAR(NLVL),STAT=kret)
  IF(KRET.NE.0)THEN
    WRITE(*,*)'FATAL ERROR - nmmb2arl.f - allocate NVAR:',NLVL
    STOP 792
  END IF
  NVAR = N3DV
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! set up grid (run this only once)
!-------------------------------------------------------------------------------
  IF(JMSG.EQ.0)THEN

  ! lambert conformal nomenclature from
    ! http://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_temp3-30.shtml
  ! polar stereographic from
    ! http://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_temp3-20.shtml
  ! mercator 
    ! http://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_temp3-10.shtml
  ! Nx = number of points along the x-axis
    NXP=GFLD%IGDTMPL(8)
  ! Ny = number of points along the y-axis
    NYP=GFLD%IGDTMPL(9)
  ! Lat1 - latitude of 1st grid point
    LAT1=GFLD%IGDTMPL(10)/1000000.
  ! Lon1 - longitude of 1st grid point
    LON1=GFLD%IGDTMPL(11)/1000000.
  ! LaD - Latitude where Dx and Dy are specified  (lambert, merc, polar stereo)
  ! LaD - Latitude at which the Mercator projection intersects Earth
    LAD=GFLD%IGDTMPL(13)/1000000.
   
    IF(GFLD%IGDTLEN.EQ.18.OR.GFLD%IGDTLEN.EQ.22)THEN
    ! Polar stereographic and Lambert
     ! LoV - longitude of meridian parallel to y-axis along which latitude increases 
     !    as the y-coordinate increases 
       LOV=GFLD%IGDTMPL(14)/1000000.
    ELSE
    ! otherwise (Mercator), arbitrarily set 
       LOV=-99
    END IF

    IF(GFLD%IGDTLEN.EQ.19)THEN 
    ! Mercator
     ! Lo2 - longitude of last grid point
       Lo2=GFLD%IGDTMPL(15)/1000000.
    ELSE
    ! otherwise, arbitrarily set
       Lo2=-99
    END IF

  ! Dx - x-direction grid lingth (grid spacing) 
    IF(GFLD%IGDTLEN.EQ.19)THEN
    ! Mercator
    GRIDKM=GFLD%IGDTMPL(18)/1000000.
    ELSE
    ! Lambert and polar stereographic
    GRIDKM=GFLD%IGDTMPL(15)/1000000.
    END IF

    IF(GFLD%IGDTLEN.EQ.22)THEN
  !   Lambert
    ! Latin 1 - first latitude from the pole at which the secant cone cuts the sphere
      LATIN1=GFLD%IGDTMPL(19)/1000000.
    ELSE
    ! otherwise, arbitrarily set
      LATIN1=-99
    END IF

  ! create the configuration file if it doesn't exist
    CALL MAKNDX(N2DV,NVAR,VCHAR0,VCHAR1,SIGL,NXP,NYP,NLVL,           &
                LAT1,LON1,LAD,LOV,GRIDKM,LATIN1,GTYPE,LO2,KVC)

  ! configure the packing routines
    FNAME='CFG_'//GTYPE
    CALL PAKSET(20,FNAME,1,NX,NY,NZ)
    WRITE(*,*)'Set grid from pakset: ',nx,ny,nz
 
  END IF

  ALLOCATE (rvar(nxp,nyp),STAT=kret)
  IF(KRET.NE.0)THEN
    WRITE(*,*)'FATAL ERROR - nmmb2arl.f - allocate RVAR:',NXP,NYP
    STOP 793
  END IF
  ALLOCATE (xvar(nxp,nyp),STAT=kret)
  IF(KRET.NE.0)THEN
    WRITE(*,*)'FATAL ERROR - nmmb2arl.f - allocate XVAR:',NXP,NYP
    STOP 794
  END IF
  NXY=NXP*NYP
  ALLOCATE (cvar(nxy),STAT=kret)
  IF(KRET.NE.0)THEN
    WRITE(*,*)'FATAL ERROR - nmmb2arl.f - allocate CVAR:',NXY
    STOP 795
  END IF
  ALLOCATE (svar(nxy),STAT=kret)
  IF(KRET.NE.0)THEN
    WRITE(*,*)'FATAL ERROR - nmmb2arl.f - allocate SVAR:',NXY
    STOP 796
  END IF
  ALLOCATE (diff(nxp,nyp),STAT=kret)
  IF(KRET.NE.0)THEN
    WRITE(*,*)'FATAL ERROR - nmmb2arl.f - allocate DIFF:',NXP,NYP
    STOP 796
  END IF
  ALLOCATE (dumw(nxp,nyp),STAT=kret)
  IF(KRET.NE.0)THEN
    WRITE(*,*)'FATAL ERROR - nmmb2arl.f - allocate DUMW:',NXP,NYP
    STOP 796
  END IF
  SHAPE(1)=NXP
  SHAPE(2)=NYP

!    --------------------
!!!! end run GETGB2 once
!    --------------------

  DONEQ=.FALSE.
mloop: DO WHILE(.NOT.DONEQ)
     ! free memory when done with field
     CALL GF_FREE(GFLD)

     !write(*,*)
     !write(*,*)'------------------------------------------------------------------------------------'
     !write(*,*)'    Read next record (after 100 CONTINUE)'
     !write(*,*)'------------------------------------------------------------------------------------'

     ! set GRIB2 field identification values to search for
     jdisc=-1		! grib2 discipline number (-1=accept any discipline)
     jids=-9999	! -9999 is wildcard
     jpdtn=-1		! product definition template number (-1=accept any)
     jpdt=-9999	! array ov values defining the product definition
                  !   template 4.N of the field for which to search
     jgdtn=-1		! grid definition template number (M)
     jgdt=-9999	! array of values defining the grid definition template

     ! read grib2 record
     ! apparently with all the defaults above, this just reads record by record
     ! could re-do program to only (and directly) get the needed fields)
     CALL GETGB2(10,11,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT,.FALSE.,J,GFLD,IRET)
     IF(IRET.NE.0)THEN
        IF(IRET.EQ.99)THEN
           WRITE(*,*)'Notice - assume done reading grib2 file'
           DONEQ=.TRUE.
           EXIT mloop
        END IF
        WRITE(*,*)'ERROR getgb2: ',IRET
        WRITE(*,*)'           j: ',J
        ! close if there is an error in GETGB2
        CALL BACLOSE(10,IRET)
        IF(IRET.NE.0)THEN
           WRITE(*,*)'ERROR baclose: ',IRET
        END IF
        STOP 777
     END IF

     ! ARL-packing method cannot have a bitmap
     IF(GFLD%IBMAP.NE.255)THEN
        !WRITE(*,*)'FATAL - This program does not support bitmap'
        !STOP 377
        ! with hrrr, there are some bitmapped fields, but we are not getting them
        WRITE(*,*)'Skip this field it is a bitmap'
        CYCLE mloop
     END IF

     ! diagnostic
     !write(*,'(A,40I6)')'initial ipdtmpl:',gfld%ipdtmpl
     !write(*,'(A,40I10)')'initial igdtmpl:',gfld%igdtmpl

     !-------------------------------------------------------------------------------
     ! standard output name for packed data
     FOUT='DATA.'//GTYPE
     INQUIRE(FILE=FOUT,EXIST=FTEST)
     LREC=NXY+50
     OPEN(20,FILE=FOUT,RECL=LREC,ACCESS='DIRECT',FORM='UNFORMATTED')
     !-------------------------------------------------------------------------------

     ! message number as per wgrib2 (u is .1 v is .2 in the same message)
     sfx=''
     jmsg=jmsg+1
     if(gfld%ifldnum.ne.1)then
        jmsg=jmsg-1
        sfx='.2'
     end if

     ! indicator section
     KDISC=GFLD%DISCIPLINE
     VER=GFLD%VERSION
     IF(VER.NE.2)THEN
        WRITE(*,*)'ERROR - Input file is not GRIB version 2'
        STOP 101
     END IF

     ! product definition section
     KCAT= GFLD%IPDTMPL(1)
     KVARB=GFLD%IPDTMPL(2)
     LTYPE=GFLD%IPDTMPL(10)
     ILEVEL=GFLD%IPDTMPL(12)

     ! levels we don't want
     !   Code table 4.5, Fixed surface types and units
     !   http://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_table4-5.shtml
     ! exclude fields on potential vorticity levels
     IF(GFLD%IPDTMPL(10).EQ.109)THEN
        CYCLE mloop
     END IF

     ! reset a flag to indicate variable is to be found.
     FOUNDQ=.FALSE.

     ! check if 2d variable present in selection table
     KL=0
d2loop: DO KV=1,N2DV
        DISC=DISC0(KV)
        CAT=CAT0(KV)
        VGRIB=VGRIB0(KV)
        VCHAR=VCHAR0(KV)
        CNVRT=CNVRT0(KV)

        ! matches id and special level indicator
        !write(*,*)'kv,kdisc,disc,kcat,cat,kvarb,vgrib,ilevel,sig0(kv),styp(kv),ltype:',kv,kdisc,disc,kcat,cat,kvarb,vgrib,ilevel,sig0(kv),styp(kv),ltype
        !write(*,*)'kv,ilevel,sig0(kv),styp(kv),ltype:',kv,ilevel,sig0(kv),styp(kv),ltype
        IF(KDISC.EQ.DISC.AND.KCAT.EQ.CAT.AND.KVARB.EQ.VGRIB.AND.  &
           ILEVEL.EQ.SIG0(KV).AND.STYP(KV).EQ.LTYPE)THEN
           ! For these exclusions, MUST run this with -z0 so there is no initialization.
           ! When f00 is run, for these fields don't want to overwrite the fcst from
           ! previous cycle
           ! exclude precip accumulation other than 1-h
           IF(CAT.EQ.1.AND.VGRIB.EQ.8)THEN
              IF(GFLD%IPDTMPL(27).NE.1)THEN
                 !write(*,*)'skip precip not 1-hourly'
                 CYCLE mloop
              END IF
           END IF
           FOUNDQ=.TRUE.
           EXIT d2loop
        END IF
     END DO d2loop

     ! then check for 3d variable
     IF(.NOT.FOUNDQ)THEN
d3loop: DO KV=1,N3DV
           DISC=DISC1(KV)
           CAT=CAT1(KV)
           VGRIB=VGRIB1(KV)
           VCHAR=VCHAR1(KV)
           CNVRT=CNVRT1(KV)
           ! if this field is a match...
           ! must exclude surface parameters by requiring ILEVEL>0.
           IF(KDISC.EQ.DISC.AND.KCAT.EQ.CAT.AND.KVARB.EQ.VGRIB.AND.ILEVEL.GT.0) THEN
              ! skip 80 m above ground wind
              ! IPDTMPL not defined here
              !IF(DISC.EQ.0.AND.CAT.EQ.2.AND.VGRIB.EQ.2.AND.ILEVEL.eq.80.AND.IPDTMPL(10)
              !.EQ.103)CYCLE mloop
              !write(*,*)'disc,cat,vgrib,ilevel:',disc,cat,vgrib,ilevel
              IF(DISC.EQ.0.AND.CAT.EQ.2.AND.(VGRIB.EQ.2.OR.VGRIB.EQ.3).AND.ILEVEL.EQ.80)THEN
                 CYCLE mloop
              END IF
              !write(*,*)'kdisc,disc,kcat,cat,kvarb,vgrib:',kdisc,disc,kcat,cat,kvarb,vgrib

              ! check if 3d level is present in level selection mask
              KL=SMASK(MAXLEV-ILEVEL+1)
              IF(KL.NE.0)THEN
                 FOUNDQ=.TRUE.
              END IF

              EXIT d3loop
           END IF
        END DO d3loop
     END IF

     IF(.NOT.FOUNDQ)THEN
        ! if all tests fail go and read next grib record
        !WRITE(*,*)'Level match not found (go to next grb2 record): ',KREC,DISC,CAT,VCHAR,LEVEL
        CYCLE mloop
     END IF

     ! load the entire grib data record into the buffer

     !----------------------------------------------------------
     !  GRIB2
     ! possible could do above with unpack=.false.
     !   then unpack the data here once found match
     !----------------------------------------------------------

     KREC=KREC+1

     !write(*,*)
     !write(*,*)'*****************************************************************'
     !write(*,*)'300 get this record *********************************************'
     !write(*,*)'*****************************************************************'
     CALL GETGB2(10,11,J-1,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT,.TRUE.,J,GFLD,IRET)
     ! diagnostics
     !write(*,*)'listsec1:',gfld%idsect
     !write(*,*)'ipdtmpl:',gfld%ipdtmpl
     !write(*,*)'ipdtlen:',gfld%ipdtlen

     ! start of the forecast (yr-mo-da-hr-min)
     IF(GFLD%IDSECT(5).EQ.1)THEN
        IYR=GFLD%IDSECT(6)
        IMO=GFLD%IDSECT(7)
        IDA=GFLD%IDSECT(8)
        IHR=GFLD%IDSECT(9)
        IMN=GFLD%IDSECT(10)
     END IF

     ! start of forecast minutes since base time
     CALL TM2MIN(IYR,IMO,IDA,IHR,IMN,MACC0)

     ! diagnostic (dump product definition template values for every record getting
     !write(*,'(A,40I6)')'ipdtmpl:',gfld%ipdtmpl

     ! set the current time (ipdtmpl(8)=1 means the units are hours)
     IF(GFLD%IPDTMPL(8).EQ.1)THEN
        ! gfld%ipdtmpl(9) is the valid time for non-averaged/accumulation fields,
        ! but the start time for averaged/accumulated fields
        IFH=GFLD%IPDTMPL(9)
        !IF(GFLD%IPDTLEN.EQ.29)THEN
          ! sref change this to 32 from 29 
          ! http://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_temp4-8.shtml
        IF(GFLD%IPDTLEN.EQ.29)THEN
           !write(*,'(A,32I6)')'ipdtmpl:',gfld%ipdtmpl
           write(*,*)'notice: accumulation/average field'
           ! valid time
           IYRV=GFLD%IPDTMPL(16) ! sref is 19
           IMOV=GFLD%IPDTMPL(17) !         20
           IDAV=GFLD%IPDTMPL(18) !         21
           IHRV=GFLD%IPDTMPL(19) !         22
           IMNV=GFLD%IPDTMPL(20) !         23

           ! valid time in minutes since base time
           CALL TM2MIN(IYRV,IMOV,IDAV,IHRV,IMNV,MACC1)

           ! forecast time in hours
           IFH=(MACC1-MACC0)/60
           IF(MOD((MACC1-MACC0),60).NE.0)                              &
              WRITE(*,*)'NOTICE: Forecast time in not in whole hours'
        END IF
     ELSE
        WRITE(*,*)'ERROR - Forecast time unit is not hours. See GRIB2 Table 4.4'
        WRITE(*,*)'        Time unit code is',GFLD%IPDTMPL(8)
        STOP 102
     END IF
     CALL TMPLUS(IYR,IMO,IDA,IHR,IFH)

     ! for the the first record create an index record for pakrec
     IF(KREC.EQ.1)THEN
        ! write current output time to special file
        FTIME=TRIM(GTYPE)//'TIME'
        OPEN(40,FILE=FTIME)
        WRITE(40,'(I4,4I2.2)')IYR,IMO,IDA,IHR,IFH
        CLOSE (40)
     END IF

     ! remap input data from one- to two-dimensional array
     SVAR=GFLD%FLD
     RVAR=RESHAPE(SVAR,SHAPE)

     !diagnostics
     firstval=gfld%fld(1)
     lastval=gfld%fld(gfld%ndpts)
     fldmax=maxval(gfld%fld)
     fldmin=minval(gfld%fld)
     !write(*,*)'jmsg,(sfx),firstval,lastval,fldmax,fldmin:',jmsg,'(',sfx,')',firstval,lastval,fldmax,fldmin
     ! search 'firstval' below to dump unpacked vales to compare with these

     ! diagnostic check values against firstval,lastval,fldmax,fldmin
     !if(vchar.eq.'TPP1')then
     ! write(*,*)'firstval,lastval,fldmax,fldmin:',firstval,lastval,fldmax,fldmin
     !!write(*,*)'1,1, last,last, max, min:',rvar(1,1),rvar(614,428),maxval(rvar),minval(rvar)
     ! write(*,*)'1,1, last,last, max, min:',rvar(1,1),rvar(nxp,nyp),maxval(rvar),minval(rvar)
     !end if

     ! SPECIAL Case...LISD should be in degrees K, but are in degrees C
     !IF(VCHAR.EQ.'LISD') RVAR=RVAR+273.15

     IF(CNVRT.NE.1.0) RVAR=RVAR*CNVRT

     ! then pack into ARL format and continue
     WRITE(*,'(1X,3I5,1X,A,1X,2I8,1X,7I3)')                             &
             KREC,KV,KL,VCHAR,LTYPE,ILEVEL,IYR,IMO,IDA,IHR,IMN,IFH,KL+1
     CALL PAKREC(20,RVAR,CVAR,NXP,NYP,NXY,VCHAR,                        &
                 IYR,IMO,IDA,IHR,IMN,IFH,(KL+1),ZERO)

     ! for vertical velocities write DIFFerence field for greater precision
     ! previous variable written is assumed to have been the vertical
     ! velocity
     ! also for precipitation
     IF(VCHAR.EQ.'WWND'.OR.VCHAR.EQ.'TPP1')THEN
        write(*,*)'vchar:',vchar
        ! determine the packed values at each grid point
        CALL PAKINP(DUMW,CVAR,NXP,NYP,1,1,NXP,NYP,PREC,NEXP,VAR1,KSUM)
        ! compute the difference between the packed and original values
        DIFF = 0.0
        DIFF=RVAR-DUMW
        ! write the difference to the output file
        IF(VCHAR.EQ.'WWND') &
           CALL PAKREC(20,DIFF,CVAR,NXP,NYP,(NXP*NYP),'DIFW',IYR,IMO,IDA,IHR,IMN,IFH,(KL+1),ZERO)
        IF(VCHAR.EQ.'TPP1') &
           CALL PAKREC(20,DIFF,CVAR,NXP,NYP,(NXP*NYP),'DIFR',IYR,IMO,IDA,IHR,IMN,IFH,(KL+1),ZERO)
        KREC=KREC+1
     END IF
  END DO mloop

! compute number of records per time period in output file
  NREC=N2DV+1
  DO K=1,NLVL
     NREC=NREC+NVAR(K)
  END DO

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
!                 10 Jul 2002 (BS)  - add fields, levels
!                 12 Feb 2016 (BS)  - take out remnants of lat-lon grid
!
! USAGE:  CALL MAKNDX(N2DV,NVAR,VCHAR0,VCHAR1,LEVEL,NXP,NYP,NZP,
!                     NYP,NXP)
!   INPUT ARGUMENT LIST:    see below
!   OUTPUT ARGUMENT LIST:   see below
!   INPUT FILES:            NONE
!   OUTPUT FILES:           UNIT 30 CFG_GFS output file structure
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE:  IBM RS6000
!
!$$$

SUBROUTINE MAKNDX (N2DV,NVAR,VCHAR0,VCHAR1,LEVEL,NXP,NYP,NZP,           &
                   LAT1,LON1,LAD,LOV,GRIDKM,LATIN1,GTYPE,LO2,KVC)

  IMPLICIT NONE

  INTEGER,      INTENT(IN)   :: n2dv          ! number sfc variables 
  INTEGER,      INTENT(IN)   :: nvar   (:)    ! number of 3d variables per level
  CHARACTER(4), INTENT(IN)   :: vchar0 (:)    ! sfc variable id
  CHARACTER(4), INTENT(IN)   :: vchar1 (:)    ! 3d variable id
  REAL,         INTENT(IN)   :: level  (:)    ! level information
  INTEGER,      INTENT(IN)   :: nxp           ! x dimension
  INTEGER,      INTENT(IN)   :: nyp           ! y dimension
  INTEGER,      INTENT(IN)   :: nzp           ! z dimension
  REAL,         INTENT(IN)   :: lat1          ! latitude of 1st grid poin
  REAL,         INTENT(IN)   :: lon1          ! longitude of 1st grid point
  REAL,         INTENT(IN)   :: lad           ! Latitude where Dx and Dy are specified
  REAL,         INTENT(IN)   :: lov           ! longitude of meridian parallel to y-axis
  REAL,         INTENT(IN)   :: gridkm        ! grid spacing
  REAL,         INTENT(IN)   :: latin1        ! latitude at which the secant cone is cut
  CHARACTER(4), INTENT(IN)   :: gtype         ! grid identification (model)
  REAL,         INTENT(IN)   :: lo2           ! longitude of last grid point
  INTEGER,      INTENT(IN)   :: kvc           ! vertical coordinate index
  
  CHARACTER(4)  :: VCHAR(50) ! variable id
  CHARACTER(20) :: LABEL(18) ! optional field label

  CHARACTER(8)  :: FCFG      ! CFG filename

  INTEGER       :: I,N,NL,MVAR  
  REAL          :: SIG
  REAL          :: GRIDS(12), PARMAP(9)

  COMMON / SETUP / GRIDS, PARMAP

! optional field label string
  DATA LABEL/'Model Type:','Grid Numb:','Vert Coord:','Pole Lat:',         &
    'Pole Lon:','Ref Lat:','Ref Lon:','Grid Size:','Orientation:',         &
    'Cone Angle:','Sync X Pt:','Sync Y Pt:','Sync Lat:','Sync Lon:',       &
    'Reserved:','Numb X pt:','Numb Y pt:','Numb Levels:'/

! grid orientation
  GRIDS(6)=0.0
! delta=x grid size in km
  GRIDS(5)=GRIDKM
! variable reserved for future use
  GRIDS(12)=0.0

! sync x,y defines lower left grid point 
  GRIDS(8)=1.0 
  GRIDS(9)=1.0 
! lat/lon of lower left point
  GRIDS(10)=LAT1
  GRIDS(11)=LON1

! defines a lambert conformal projection

! pole lat/lon axis through pole (for sref force)
  GRIDS(1)=90.0
  GRIDS(2)=0.0 

! reference lat
  GRIDS(3)=LAD 

! reference lon (lambert and polar stereographic)
  GRIDS(4)=LOV
  ! mercator
    IF(LOV.EQ.-99)GRIDS(4)=LO2

! tangent latitude (lambert)
  GRIDS(7)=LATIN1
  ! polar stereographic
    IF(LO2.EQ.-99.AND.LATIN1.EQ.-99)GRIDS(7)=GRIDS(1)
  ! mercator
    IF(LOV.EQ.-99.AND.LATIN1.EQ.-99)GRIDS(7)=0.0

! write the packer configuration file
  FCFG='CFG_'//GTYPE

 !OPEN(30,FILE='CFG_GDAS')
  OPEN(30,FILE=FCFG)

! default grid number 99 (field not used)
  WRITE(30,'(A20,A4)')LABEL(1),GTYPE 
  WRITE(30,'(A20,A4)') LABEL(2),'  99'

! coordinate (1:sigma 2:pressure 3:terrain 4:hybrid)
  WRITE(30,'(A20,I4)') LABEL(3), KVC

! grid geolocation parameters and projection
  DO I=1,12
     WRITE(30,'(A20,F10.4)')LABEL(I+3),GRIDS(I) 
  END DO

! grid dimensions
  WRITE(30,'(A20,I4)') LABEL(16), NXP
  WRITE(30,'(A20,I4)') LABEL(17), NYP
  WRITE(30,'(A20,I4)') LABEL(18),(NZP+1)   

! upper level information
  DO NL=0,NZP   

     WRITE(LABEL(1),'(A6,I2,A1)')'Level ',NL,':'
     IF(NL.EQ.0)THEN
        SIG=1.0                 ! 9-7-16 change this to =1.0 from 0.0 as per prod 
        IF(KVC.EQ.2)SIG=0.0     ! 9-7-16 put this back in as per prod
        MVAR=N2DV
        VCHAR=VCHAR0
     ELSE
        SIG=LEVEL(NL)
        MVAR=NVAR(NL)
        VCHAR=VCHAR1
     END IF

!    assumes no more than 99 fields per level
     IF(SIG.LT.1.0)THEN
        WRITE(30,'(A20,F6.5,I3,99(1X,A4))')LABEL(1),   &
        SIG,MVAR,(VCHAR(N),N=1,MVAR)
     ELSEIF(SIG.GE.1.AND.SIG.LT.10.0)THEN
        WRITE(30,'(A20,F6.4,I3,99(1X,A4))')LABEL(1),   &
        SIG,MVAR,(VCHAR(N),N=1,MVAR)
     ELSEIF(SIG.GE.10.AND.SIG.LT.100.0)THEN
        WRITE(30,'(A20,F6.3,I3,99(1X,A4))')LABEL(1),   &
        SIG,MVAR,(VCHAR(N),N=1,MVAR)
     ELSEIF(SIG.GE.100.AND.SIG.LT.1000.0)THEN
        WRITE(30,'(A20,F6.2,I3,99(1X,A4))')LABEL(1),   &
        SIG,MVAR,(VCHAR(N),N=1,MVAR)
     ELSEIF(SIG.GE.1000)THEN
        WRITE(30,'(A20,F6.1,I3,99(1X,A4))')LABEL(1),   &
        SIG,MVAR,(VCHAR(N),N=1,MVAR)
     END IF

  END DO
  CLOSE (30) 

END SUBROUTINE makndx

!##############################################################################
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!$$$ 
!      created 3-16-17 from SIGMA (which is hybrid levels)

!##############################################################################
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM:  SIGMA            MAP MODEL LEVLES TO CHOSEN LEVELS             
!   PRGMMR:    BARBARA STUNDER  ORG: R/ARL       DATE:2017-05-10
!
! ABSTRACT:  THIS CODE WRITTEN AT THE AIR RESOURCES LABORATORY ...
!            MAP MODEL LEVLES TO CHOSEN LEVELS
!
! PROGRAM HISTORY LOG:
!   LAST REVISED: previously  {RRD) - in nmmb2arl.f
!                 10 May 2017 (BS)  - hrrr
!
! USAGE:  CALL SIGMA(SIGL,NLVL,SMASK,VRES,KVC)                    
!   INPUT ARGUMENT LIST:    see below
!   OUTPUT ARGUMENT LIST:   see below
!   INPUT FILES:            NONE
!   OUTPUT FILES:           NONE
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE:  IBM RS6000
!
!$$$

SUBROUTINE SIGMA (SIGL,NLVL,SMASK,VRES,KVC)
 
   REAL,    INTENT(OUT)   :: SIGL(:)     ! mid point sigma levels
   INTEGER, INTENT(INOUT) :: NLVL        ! number of sigma levels to output
   INTEGER, INTENT(INOUT) :: SMASK(:)    ! selection mask for output levels
   INTEGER, INTENT(IN)    :: VRES        ! vertical resolution (1-full,2-700,3-reduce,4-nams)
   INTEGER, INTENT(OUT)   :: kvc         ! vertical coordinate index

   INTEGER                :: K

   INTEGER, PARAMETER     :: MAXLEV=50  
   REAL,    PARAMETER     :: PDTOP=30000.0   ! change for hrrr?
   REAL,    PARAMETER     :: PT = 100.0      ! change for hrrr?

   INTEGER :: VMASK (MAXLEV) ! selection mask for sigma levels                     
   INTEGER :: VMASK1(MAXLEV) ! full                      
   INTEGER :: VMASK2(MAXLEV) ! <=700                     
   INTEGER :: VMASK3(MAXLEV) ! reduced resolution from NAMS    
   INTEGER :: VMASK4(MAXLEV) ! operational NAMS   
   REAL    :: WRFSIG(MAXLEV) ! number of sigma levels for WRF-NMMB

   SAVE VMASK, WRFSIG

!  full vertical extent, skipping fewer levels 
!    (1st 24 then every other, and near top, skip two)
!    for hrrr this is the only one tested
   DATA VMASK4/      0,      0,      1,      0,      0,  &
                     1,      0,      0,      1,      0,  &
                     1,      0,      1,      0,      1,  &
                     0,      1,      0,      1,      0,  &
                     1,      0,      1,      0,      1,  &
                     0,      1,      1,      1,      1,  &
                     1,      1,      1,      1,      1,  &
                     1,      1,      1,      1,      1,  &
                     1,      1,      1,      1,      1,  &
                     1,      1,      1,      1,      1/

!!! force all levels for testing
  !DATA VMASK4/      1,      1,      1,      1,      1,  &
  !                  1,      1,      1,      1,      1,  &
  !                  1,      1,      1,      1,      1,  &
  !                  1,      1,      1,      1,      1,  &
  !                  1,      1,      1,      1,      1,  &
  !                  1,      1,      1,      1,      1,  &
  !                  1,      1,      1,      1,      1,  &
  !                  1,      1,      1,      1,      1,  &
  !                  1,      1,      1,      1,      1,  &
  !                  1,      1,      1,      1,      1/

!  hrrr levels (4-digits) from Curtis Alexander, ESRL 3-15-17 
!      (who says later version will likely be hybrid levels)
!     5-digits per Glenn's runs at ARL
!  highest level to nearest ground
   DATA WRFSIG/                                     &
       0.00235, 0.00720, 0.01240, 0.01800, 0.02400, &
       0.03040, 0.03730, 0.04475, 0.05270, 0.06125, &
       0.07175, 0.08485, 0.09895, 0.11305, 0.12715, &
       0.14125, 0.15535, 0.16945, 0.18355, 0.19765, &
       0.21175, 0.22585, 0.24125, 0.25830, 0.27665, & 
       0.29705, 0.31975, 0.34495, 0.37295, 0.40410, &
       0.43850, 0.47675, 0.51950, 0.56700, 0.61600, &
       0.66250, 0.70600, 0.74650, 0.78400, 0.81850, &
       0.85000, 0.87850, 0.90400, 0.92750, 0.94900, &
       0.96700, 0.98100, 0.99050, 0.99600, 0.99900/ 

!   vertical coordinate system
!   kvc = 1(sigma); 2(pressure); 3(height); 4(hybrid)
!     could comfirm for upper-level fields ipdtmpl(10)=105
!     http://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_table4-5.shtml
    KVC=1

!   select vertical resolution
    IF(VRES.EQ.1) VMASK=VMASK1
    IF(VRES.EQ.2) VMASK=VMASK2
    IF(VRES.EQ.3) VMASK=VMASK3
    IF(VRES.EQ.4) VMASK=VMASK4

!   Determine the number of HYSPLIT output levels and compute the
!   array index to correspond GRIB file input level with output level.
!   First pass into this subroutine NLVL will be equal to zero.
    IF(NLVL.EQ.0)THEN
      !DO K=MAXLEV,1,-1
       DO K=MAXLEV,1,-1
          IF(VMASK(K).EQ.1)THEN
             NLVL=NLVL+1
!            The hysplit vertical index starts at one just above the surface
!            so does hrrr (unline nam)
             SMASK(K)=NLVL
          END IF
       END DO

       WRITE(50,*)'Using sigma levels from internal data statement'

!   map to output vertical distribution
    ELSE
       WRITE(50,*)' NMMB-level  HYSP-level  HYSP-sigma'
       DO K=1,MAXLEV
          IF(SMASK(K).NE.0)THEN
             SIGL(SMASK(K))=WRFSIG(K)
            !A=INT(WRFSIG(K))
            !B=WRFSIG(K)-A
            !WRITE(50,*) K,SMASK(K),SIGL(SMASK(K)),(A+1012.0*B)
             WRITE(50,*) K,SMASK(K),SIGL(SMASK(K))
          ELSE
             WRITE(50,*) K,SMASK(K),WRFSIG(K),'   skipped '
          END IF
       END DO
    END IF

END SUBROUTINE sigma
