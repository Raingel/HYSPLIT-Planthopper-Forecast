!$$$  MAIN PROGRAM DOCUMENTATION BLOCK
!
! MAIN PROGRAM: GRIB2ARL     DECODE GRIB MODEL FIELDS FOR HYSPLIT
! PRGMMR: DRAXLER            ORG: R/ARL       DATE: 1998-08-26
!
! ABSTRACT:  THIS CODE WRITTEN AT THE AIR RESOURCES LABORATORY ...
! Convert Latitude-Longitude GRIB data file to ARL format with the data 
! organized by time so that a consecutive group of records contains all
! the variables at the same time.  The ARL format consists of an index 
! record followed by data records.  One record per variable per level, 
! then followed by records for the next time period.  All records are of
! fixed length and packed one byte per variable.  Packing information is
! coded in the header portion of each record. 
!
! The program repacks the input data into ARL format in two different 
! modes. If a latitude-longitude output grid is defined (-g3 or -g4) then 
! the latlon input grid is repacked and written directly to ARL format.  
! A latlon input grid may be resized as a subgrid by setting the number of
! output grid points (-n{nxp:nyp}). 
!
! The latlon input data may also be interpolated to a conformal map
! projection (-g0, -g1, -g2) by specifying the output grid size (km),
! grid center latlon, and the number of grid points in each direction.  
! The projection is automatically defined depending upon the latitude 
! of the grid center, where Polar Sterographic is used for latitudes 
! above 60, Mercator for latitudes less than 30, and Lambert Conformal 
! for intermediate latitudes. The extract conformal map always defaults
! to a 100 km resolution projection of 100x100 grid points centered at 
! a lat/lon point set on the command line.
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
!                 20 Feb 2002 (RRD) - converted to ECMWF compatibility
!                 07 Jan 2003 (RRD) - implemented lat-lon subgrid option
!                 19 Feb 2003 (RRD) - additional diagnostic messages
!                 16 Mar 2004 (RRD) - major upgrade to handle ERA-40 grids
!                 02 Jul 2004 (RRD) - save configuration for merge
!                 09 Feb 2005 (RRD) - missing variable when using -q1
!                 27 Apr 2006 (RRD) - special CTBTO field (#134) of ecmwf data
!                 24 Oct 2006 (RRD) - added PBL height
!                 09 Apr 2007 (RRD) - FNMOC nogaps data support
!                 09 Jun 2008 (RRD) - bitmap variable (kbms) allocation
!                 03 Jul 2008 (RRD) - test for regional ecmwf grid
!                 08 Jul 2008 (RRD) - Argentina met service model
!                 14 Jan 2009 (RRD) - Missing variables aloft
!                 03 Apr 2009 (RRD) - pressure levels now any order
!                 20 Aug 2009 (RRD) - ecmwf count all levels
!                 23 Aug 2010 (RRD) - ukmet office center id=74
!                 11 Feb 2011 (RRD) - rainfall accumulation time
!                 11 Feb 2013 (RRD) - other options for ECMWF (disabled)
!                 20 Feb 2013 (RRD) - grid data ordered by variable then level
!                 12 Mar 2013 (RRD) - corrected memory overflow
!                 29 Oct 2013 (RRD) - from Met Eireann (analyze: sigp=sigl)
!                 02 May 2014 (BS)  - 0.125 deg grid resolution (F10.2 to F10.3)
!                 24 Oct 2017 (CPL) - handle large grib files
!
! USAGE:  GRIB2ARL [-options]
!   INPUT ARGUMENT LIST:  see below
!   OUTPUT ARGUMENT LIST: none 
!   INPUT FILES:          grib input data files 
!   OUTPUT FILES:
!      unit 20 DATA.ARL - ARL packed data output file
!      unit 30 CFG_ARL  - characteristics of output grid
!      unit 40 ARLTIME  - time indicator file
!      unit 50 MESSAGE  - diagnostic messages
!      unit 60 CFG_GRIB - grib information file
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE:  IBM RS6000
!
!$$$

PROGRAM GRIB2ARL

  IMPLICIT NONE

  INTEGER, PARAMETER :: MFILE = 3         ! maximum number of files

  INTEGER       :: KREC = 0               ! output record counter
  LOGICAL(1)    :: UVSET=.FALSE.          ! wind rotation flag
  INTEGER*8     :: KBYTE(MFILE)=(/0,0,0/) ! grib record byte counter

  LOGICAL       :: FTEST
  LOGICAL       :: SETGRIB = .FALSE.      ! read grib configuration file
  CHARACTER(80) :: FNAME(MFILE)
  INTEGER       :: FTYPE(MFILE)=(/0,0,0/) ! file type 1:3d 2:2d 3:const

  INTEGER       :: SFCP,RAIN,KF,KRET,NFILE,HANDLE(MFILE)
  INTEGER       :: IY0,IM0,ID0,IH0,IMN,IFH,IYR,IMO,IDA,IHR
  INTEGER       :: ZERO,KLVLS,FCOPEN,KGRID,NXP,NYP,KGRIB,NTIME
  REAL          :: CLAT,CLON,DIST

  COMMON /CDATES/ IYR,IMO,IDA,IHR,IMN,IFH,IY0,IM0,ID0,IH0

!------------------------------------------------------------------------
  INTERFACE
    SUBROUTINE COMMAND (kgrib,clat,clon,dist,kgrid,nxp,nyp,klvls, &
                        zero,sfcp,rain,nfile,ntime,ftype,fname)
    IMPLICIT NONE
    INTEGER,       INTENT(OUT) :: KGRIB       ! grib config file
    REAL,          INTENT(OUT) :: CLAT,CLON   ! extraction center 
    REAL,          INTENT(OUT) :: DIST        ! extraction grid resolution
    INTEGER,       INTENT(OUT) :: KGRID       ! output grid option
    INTEGER,       INTENT(OUT) :: NXP,NYP     ! output grid size
    INTEGER,       INTENT(OUT) :: KLVLS       ! number of output levels
    INTEGER,       INTENT(OUT) :: ZERO        ! initialization flag
    INTEGER,       INTENT(OUT) :: SFCP        ! use sfc pressure flag
    INTEGER,       INTENT(OUT) :: RAIN        ! precip accumulation hours
    INTEGER,       INTENT(OUT) :: NFILE       ! number of input files
    INTEGER,       INTENT(OUT) :: NTIME       ! number of time periods
    INTEGER,       INTENT(OUT) :: FTYPE(:)    ! file type
    CHARACTER(80), INTENT(OUT) :: FNAME(:)    ! input file names
    END SUBROUTINE command

    SUBROUTINE ANALYZE (handle,ftype,klvls,sfcp,rain,iend)
    IMPLICIT NONE
    INTEGER,INTENT(IN) :: HANDLE              ! IO handle
    INTEGER,INTENT(IN) :: FTYPE               ! input file type       
    INTEGER,INTENT(IN) :: KLVLS               ! max number of levels  
    INTEGER,INTENT(IN) :: SFCP                ! use sfc pressure flag
    INTEGER,INTENT(IN) :: RAIN                ! precip accumulation hours
    LOGICAL,INTENT(IN) :: IEND                ! flag for termination
    END SUBROUTINE analyze

    SUBROUTINE XTRACT (FTYPE,KREC,KBYTE,UVSET,HANDLE,KGRID,NXP,NYP,ZERO,KRET)
    IMPLICIT NONE
    INTEGER,INTENT(INOUT)    :: FTYPE         ! file type
    INTEGER,INTENT(INOUT)    :: KREC          ! output record counter
    INTEGER*8,INTENT(INOUT)  :: KBYTE         ! input file byte counter
    LOGICAL(1),INTENT(INOUT) :: UVSET         ! wind component rotation
    INTEGER,INTENT(IN)       :: HANDLE        ! IO handle
    INTEGER,INTENT(IN)       :: KGRID         ! projection type
    INTEGER,INTENT(IN)       :: NXP,NYP       ! extract grid size
    INTEGER,INTENT(IN)       :: ZERO          ! initialize output file
    INTEGER,INTENT(OUT)      :: KRET          ! termination code
    END SUBROUTINE xtract

    SUBROUTINE SETGRID (kgrid,kgrib,nxp,nyp,klvls,clat,clon,dist)
    IMPLICIT NONE
    INTEGER, INTENT(IN)    :: kgrid     ! output grid definition
    INTEGER, INTENT(IN)    :: kgrib     ! configuration file    
    INTEGER, INTENT(IN)    :: klvls     ! number of output levels
    INTEGER, INTENT(INOUT) :: nxp,nyp   ! output grid size
    REAL,    INTENT(IN)    :: clat,clon ! center of output grid
    REAL,    INTENT(IN)    :: dist      ! grid resolution       
    END SUBROUTINE setgrid

    SUBROUTINE WRTCON (krec,zero,nxp,nyp)
    IMPLICIT NONE
    INTEGER, INTENT(INOUT) :: KREC
    INTEGER, INTENT(IN)    :: ZERO          ! initialize output file
    INTEGER, INTENT(IN)    :: NXP,NYP       ! extract grid size
    END SUBROUTINE wrtcon
  END INTERFACE

!------------------------------------------------------------------------
! required for NCEP operational implementation
! CALL W3TAGB('GRIB2ARL ',1998,0238,0067,'R/ARL  ')

  CALL TMINIT

! open diagnostic message file
  OPEN(50,FILE='MESSAGE')

! process command line arguments or set default values
  CALL command(kgrib,clat,clon,dist,kgrid,nxp,nyp,klvls,zero, &
               sfcp,rain,nfile,ntime,ftype,fname)

! check for ANALYZE namelist file
  IF(KGRIB.EQ.1)THEN
    INQUIRE(FILE='CFG_GRIB',EXIST=SETGRIB)
    IF(SETGRIB) CALL SETPROG
  END IF

!------------------------------------------------------------------------
! open input data files

  DO KF=1,NFILE
     INQUIRE(FILE=FNAME(KF),EXIST=FTEST)
     IF(FTEST)THEN
!       direct IO subroutine
        HANDLE(KF)=FCOPEN(FNAME(KF),'r')
        WRITE(50,*)'Opened file: ',FNAME(KF)(1:40)
        WRITE(50,*)'-----------------------------------------------------'

!       analyze the data structure of the input file
        IF(.NOT.SETGRIB) & 
           CALL ANALYZE(handle(kf),ftype(kf),klvls,sfcp,rain,(kf.eq.nfile))
     ELSE
        WRITE(*,*) 'File not found:',FNAME(KF)
        WRITE(50,*)'File not found:',FNAME(KF)
        STOP 101
     END IF
  END DO

!------------------------------------------------------------------------
! define the output grid structure

  CALL SETGRID(kgrid,kgrib,nxp,nyp,klvls,clat,clon,dist)
  OPEN(40,FILE='ARLTIME')

!-------------------------------------------------------------------------------
! main decoding routine completes once per time period 

  DO WHILE (KRET.NE.2)
     DO KF=1,NFILE
        IF(FTYPE(KF).GT.0)THEN  ! skip constant files if already input
           CALL XTRACT(FTYPE(KF),KREC,KBYTE(KF),UVSET,HANDLE(KF),KGRID,NXP,NYP,ZERO,KRET)
        END IF
!       write any constant fields
        IF(FTYPE(KF).EQ.0)CALL wrtcon (krec,zero,nxp,nyp)
        WRITE(50,*)'Finished (kret,file): ',KRET,FNAME(KF)(1:40)  
        WRITE(50,*)'-----------------------------------------------------'
     END DO

!    write current output time to special file
     WRITE(50,*)'Finished time: ',IY0,IM0,ID0,IH0
     WRITE(40,'(4I2.2)')IY0,IM0,ID0,IH0

!    set time counter to next valid time
     IY0=IYR
     IM0=IMO
     ID0=IDA
     IH0=IHR

!    close out time period and write index record
     CALL PAKNDX(20)
     WRITE(50,*)'INDEX record written after record: ',KREC
     WRITE(50,*)'-------------------------------------------------------'

!    time period count
     NTIME=NTIME-1
     IF(NTIME.LE.0)KRET=2
  END DO

!-------------------------------------------------------------------------------
! rotate vector variables from true to grid orientation after processing all 
! the records because the u,v components were not written sequentially in 
! the grib file

  IF(.NOT.UVSET.AND.KGRID.LT.3) CALL ROTATE

!-------------------------------------------------------------------------------
! close out remaining open files

  DO KF=1,NFILE
     CALL FCCLOS(HANDLE(KF),*900)
  END DO
  900 CONTINUE

  CLOSE (20)
  CLOSE (40)
  CLOSE (50)

! required for NCEP operational implementation
! CALL W3TAGE('GRIB2ARL ')

END PROGRAM grib2arl

!#######################################################################

MODULE metdata

  INTEGER,     PARAMETER   :: m2dv = 13 ! maximum number of 2D variables
  INTEGER,     PARAMETER   :: m3dv =  7 ! maximum number of 3D variables
  INTEGER,     PARAMETER   :: mlev = 65 ! maximum number of levels  

! meteorological model identification
  CHARACTER(4)             :: MODEL

  CHARACTER(4)             :: KONST     ! constant fields id
  LOGICAL                  :: RTIME     ! records time ordered

! sfc arrays to hold grib, character, and conversion information
  INTEGER,     ALLOCATABLE :: VGRIB0(:) ! sfc grib ID  
  INTEGER,     ALLOCATABLE :: STYP  (:) ! sfc variable code 
  INTEGER,     ALLOCATABLE :: STYPS (:) ! sfc variable code (alternate) 
  INTEGER,     ALLOCATABLE :: SIG0  (:) ! sfc level code 
  CHARACTER(4),ALLOCATABLE :: VCHAR0(:) ! arl variable string
  REAL,        ALLOCATABLE :: CNVRT0(:) ! units conversion factor
  INTEGER,     ALLOCATABLE :: EXIST0(:) ! variable found in file

! 3d arrays to hold grib, character, and conversion information
  INTEGER,     ALLOCATABLE :: VGRIB1(:) ! 3d grib variable code
  CHARACTER(4),ALLOCATABLE :: VCHAR1(:) ! arl variable string
  REAL,        ALLOCATABLE :: CNVRT1(:) ! units conversion factor

! 3d variables exist by level 
  INTEGER,     ALLOCATABLE :: EXIST1(:,:) ! variable found in file

! 3d level information
  REAL,        ALLOCATABLE :: SIGL  (:) ! input/output levels
  INTEGER                  :: KVC       ! vertical coordinate flag
  INTEGER                  :: NLVL      ! number of levels excluding sfc

! arrays for grib description
  INTEGER      :: KPDS(25)              ! product definition
  INTEGER      :: KGDS(25)              ! grid definition
  INTEGER      :: KPTR(25)              ! section lengths
  INTEGER      :: SHAPE(2)              ! two dimensional shape

! define the input grid
  INTEGER      :: NLON,NLAT             ! number of points
  INTEGER      :: ILON1,JLAT1           ! I,J of corner point
  REAL         :: CLAT1,CLON1           ! corner point
  REAL         :: CLAT2,CLON2           ! corner point
  REAL         :: DLON,DLAT             ! delta grid

! input data buffer and bit map section
  CHARACTER(1),ALLOCATABLE :: BUFF(:)
  LOGICAL(1),  ALLOCATABLE :: KBMS(:)

! unpacked input array
  REAL,ALLOCATABLE         :: XVAR(:,:),RVAR(:)

! unpacked output array
  REAL,ALLOCATABLE         :: SVAR(:,:),PVAR(:,:)

! constant array of fixed data      
  REAL,ALLOCATABLE         :: QVAR(:,:)

! lat/lon grid conversion equivalence table
  REAL,ALLOCATABLE         :: TLAT(:,:)
  REAL,ALLOCATABLE         :: TLON(:,:)       

! packed output array
  CHARACTER(1),ALLOCATABLE :: CVAR(:)

  SAVE model,vgrib0,styp,styps,sig0,vchar0,cnvrt0,exist0,vgrib1,vchar1, &
       cnvrt1,exist1,sigl,kvc,nlvl,kpds,kgds,kptr,shape,nlon,nlat,      &
       ilon1,jlat1,clat1,clon1,dlon,dlat,clat2,clon2,buff,kbms,xvar,    &
       rvar,svar,pvar,qvar,tlat,tlon,cvar,konst,rtime

END MODULE metdata


!##############################################################################
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM:  EXTRACT          MAIN ROUTINE FOR DECODING ECM GRIB DATA
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
!                 20 Feb 2002 (RRD) - ecmwf compatitiblity
!                 08 Jan 2003 (RRD) - correct for initial precip
!
! USAGE:  CALL XTRACT( )
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

SUBROUTINE XTRACT(FTYPE,KREC,KBYTE,UVSET,HANDLE,KGRID,NXP,NYP,ZERO,KRET)

  USE metdata

  IMPLICIT NONE

! argument list variables
  INTEGER,INTENT(INOUT)    :: FTYPE        ! file type
  INTEGER,INTENT(INOUT)    :: KREC         ! output record counter
  INTEGER*8,INTENT(INOUT)  :: KBYTE        ! input file byte counter
  LOGICAL(1),INTENT(INOUT) :: UVSET        ! wind component rotation
  INTEGER,INTENT(IN)       :: HANDLE       ! IO handle
  INTEGER,INTENT(IN)       :: KGRID        ! projection type
  INTEGER,INTENT(IN)       :: NXP,NYP      ! extract grid size
  INTEGER,INTENT(IN)       :: ZERO         ! initialize output file
  INTEGER,INTENT(OUT)      :: KRET         ! termination code

! temporary holding variable
  INTEGER      :: VGRIB, PGRIB
  CHARACTER(4) :: VCHAR, PCHAR
  REAL         :: CNVRT

  INTEGER      :: kv,kl,klen,levels
  REAL         :: dummy(1),offset,sigma
  LOGICAL      :: setvarb 
  INTEGER      :: iyr,imo,ida,ihr,imn,ifh,iy0,im0,id0,ih0

  COMMON /CDATES/ IYR,IMO,IDA,IHR,IMN,IFH,IY0,IM0,ID0,IH0
  COMMON /VCOORD/ LEVELS, OFFSET(100), SIGMA(100)

!---------------------------------------------------------------------
! When dealing with some F90 compiler replace ICHAR with JCHAR function
  CHARACTER(1)        :: mychr    
  INTEGER             :: jchar
  JCHAR(MYCHR)=IAND(ICHAR(MYCHR),255)
!---------------------------------------------------------------------

  INTERFACE

  SUBROUTINE REGRID(KGRID,V1,NX1,NY1,V2,NX2,NY2)
  IMPLICIT NONE
  INTEGER, INTENT(IN)    :: KGRID           ! output grid definition 
  INTEGER, INTENT(IN)    :: NX1, NY1        ! input dimensions
  INTEGER, INTENT(IN)    :: NX2, NY2        ! output dimensions
  REAL,    INTENT(INOUT) :: V1(:,:)         ! input array
  REAL,    INTENT(OUT)   :: V2(:,:)         ! output array
  END SUBROUTINE regrid

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

  SUBROUTINE U2GRID(UU,VV,NX,NY)
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: NX, NY      ! array dimensions
  REAL, INTENT(INOUT) :: UU(:,:)     ! u component
  REAL, INTENT(INOUT) :: VV(:,:)     ! v component
  END SUBROUTINE u2grid

  END INTERFACE

!------------------------------------------------------------------------

  WRITE(50,*)'SUBROUTINE extract: buffer starting location: ',KBYTE

  KLEN=0
  newrec : DO WHILE (KBYTE.GE.0)

!   load the indicator section to determine record length
    CALL FCPTPD(HANDLE,KBYTE,*920)
    CALL FCREAD(HANDLE,BUFF,1,8,*920)
    IF(BUFF(1)//BUFF(2)//BUFF(3)//BUFF(4).NE.'GRIB')THEN
       KBYTE=KBYTE+1
       CYCLE newrec
    END IF
    KLEN=JCHAR(BUFF(7))+JCHAR(BUFF(6))*256+JCHAR(BUFF(5))*65536

!   read the entire data record
    CALL FCPTPD(HANDLE,KBYTE,*920)
    CALL FCREAD(HANDLE,BUFF,1,KLEN,*920)

!   call the nmc light grib unpacker
    CALL W3FI00(BUFF,KPDS,KGDS,KBMS,DUMMY,KPTR,KRET)
    IF(KRET.NE.0)THEN
       WRITE(50,*)'Error W3FI00: ',KRET
       STOP 102
    END IF

!------------------------------------------------------------------------
! determine if variable present in selection table   

! Construct total precip from special two variable precip. Sum fields
! 143 and 142, applicable for some ECMWF data sets
  IF(KPDS(5).EQ.143.AND.VGRIB.EQ.142) KPDS(5)=142

  setvarb=.FALSE.

! 2-dim surface variables
  DO kv=1,m2dv
     IF(KPDS(5).EQ.VGRIB0(KV).AND.                        &
       (KPDS(6).EQ.STYP(KV).OR.KPDS(6).EQ.STYPS(KV)).AND. &
        KPDS(7).EQ.SIG0(KV).AND.EXIST0(KV).EQ.1)THEN
        VGRIB=VGRIB0(KV)
        VCHAR=VCHAR0(KV)
        CNVRT=CNVRT0(KV)
        setvarb=.true.
        KL=0
     END IF
  END DO

! 3-dim upper air variables
  IF(.NOT.setvarb.AND.(KPDS(6).EQ.100.OR.KPDS(6).EQ.109))THEN
     DO kv=1,m3dv
     IF(KPDS(5).EQ.VGRIB1(KV).AND.EXIST1(KV,1).EQ.1)THEN
        VGRIB=VGRIB1(KV)
        VCHAR=VCHAR1(KV)
        CNVRT=CNVRT1(KV)

        IF(MODEL(1:3).EQ.'ECM'.AND.KPDS(6).EQ.109)THEN
!          reverse numbering for hybrid ecmwf, match index
           KL=LEVELS-KPDS(7)
           IF(KL.LE.NLVL) setvarb=.TRUE.
        ELSE
!          pressure grib data, find pressure level
           KL=1
           DO WHILE (KL.LE.NLVL.AND.KPDS(7).NE.INT(SIGL(KL)))
              KL=KL+1
           END DO
           IF(KL.LE.NLVL) setvarb=.TRUE.
        END IF
     END IF
     END DO
  END IF

  IF(.NOT.setvarb) GOTO 800

!------------------------------------------------------------------------
! variable accepted for processing, load the entire grib data record into
! the buffer and then interpolate to the output grid before writing

  CALL W3FI63(BUFF,KPDS,KGDS,KBMS,RVAR,KPTR,KRET)
  IF(KRET.NE.0)THEN
     WRITE(50,*)'Error W3FI63: ',KRET
     STOP 103
  END IF

!------------------------------------------------------------------------
! update time field, except for invariant files (ftype=3)

  IF(FTYPE.LE.2)THEN

!    century fix
     KPDS(8)=MOD(KPDS(8),100)

!    set the initialization time
     IYR=KPDS(8)
     IMO=KPDS(9)
     IDA=KPDS(10)
     IHR=KPDS(11)
     IMN=KPDS(12)

!    determine which forecast hour valid
     SELECT CASE (KPDS(16))
        CASE(0)
           IFH=0         ! +P1 when multi-time fields in their own file
           IF(KPDS(14).GT.0.AND.RTIME) IFH=KPDS(14) 
        CASE(1)          ! analysis
           IFH=0
        CASE(2,3,4)      ! valid from +P1 to +P2
           IFH=KPDS(15)
        CASE(10)
           IFH=KPDS(14)  ! valid at +P1
        CASE DEFAULT
           IFH=0
     END SELECT

!    compute the current time
     CALL TMPLUS(IYR,IMO,IDA,IHR,IFH)

!    save the time for testing
     IF(KREC.EQ.0)THEN
        IY0=IYR
        IM0=IMO
        ID0=IDA
        IH0=IHR
        WRITE(50,*)'Time initialized: ',IY0,IM0,ID0,IH0
     ELSE
!       new time found, exit to read next file, and write index record
        IF(IY0.NE.IYR.OR.IM0.NE.IMO.OR.ID0.NE.IDA.OR.IH0.NE.IHR)GOTO 910 
     END IF
  END IF

!-----------------------------------------------------------------------
! process data array

! remap input data from one- to two- dimensional array
  XVAR=RESHAPE(RVAR,SHAPE)
! interpolate from lat/lon to conformal grid
  CALL REGRID(KGRID,XVAR,NLON,NLAT,SVAR,NXP,NYP)

! units conversion
  IF(CNVRT.GE.0.0)THEN
     IF(CNVRT.NE.1.0) SVAR=SVAR*CNVRT
  ELSE
!    less than zero conversion indicates log of parameter
     IF(CNVRT.LT.0.0) SVAR=EXP(SVAR)*ABS(CNVRT)
  END IF

!------------------------------------------------------------------------
! special processing for time invariant fields

  IF(FTYPE.EQ.3) THEN
!    first time read, save field & ID, turn off read flag, clean exit
     QVAR=SVAR
     KONST=VCHAR
     FTYPE=0
     RETURN
  END IF

!----------------------------------------------------------------------
! special two component wind variables need rotation to grid coordinates
! when one of the conformal projections has been selected

  IF(KGRID.LE.2)THEN
     IF((VCHAR.EQ.'VWND'.AND.PCHAR.EQ.'UWND').OR.                            &
        (VCHAR.EQ.'V10M'.AND.PCHAR.EQ.'U10M'))THEN    

        IF(.NOT.UVSET.AND.VCHAR.EQ.'VWND'.AND.PCHAR.EQ.'UWND')THEN
!          only set flag if 3D components are adjacent
           UVSET=.TRUE.
           WRITE(50,*)'Wind rotation during read set true'
        END IF

        IF(UVSET)THEN
!          rotate winds in-place (faster than reading again to rotate)
           CALL U2GRID(PVAR,SVAR,NXP,NYP)

           WRITE(50,'(1X,I5,1X,A,1X,3I5)')KL,PCHAR,KPDS(7),KPDS(14),KPDS(15)
           CALL PAKREC(20,PVAR,CVAR,NXP,NYP,(NXP*NYP),PCHAR,               &
                       IYR,IMO,IDA,IHR,IMN,IFH,(KL+1),ZERO)

           WRITE(50,'(1X,I5,1X,A,1X,3I5)')KL,VCHAR,KPDS(7),KPDS(14),KPDS(15)
           CALL PAKREC(20,SVAR,CVAR,NXP,NYP,(NXP*NYP),VCHAR,               &
                       IYR,IMO,IDA,IHR,IMN,IFH,(KL+1),ZERO)

           KREC=KREC+2
           GOTO 800
        END IF

     ELSEIF(VCHAR(1:1).EQ.'U'.AND.UVSET)THEN
!       no need to write this record, wait for v-component
        GOTO 700
     END IF
  END IF

!-----------------------------------------------------------------------
! Precipitation field may require special processing if the totals are
! divided between convective and non-convective, for instance,         
! special ecmwf two component (previous 142 current 143=142) precip sum

  IF(PGRIB.EQ.142.AND.VGRIB.EQ.142)THEN
     SVAR=(SVAR+PVAR)
     WRITE(50,'(1X,I5,1X,A,1X,3I5)')KL,VCHAR,KPDS(7),KPDS(14),KPDS(15)
     CALL PAKREC(20,SVAR,CVAR,NXP,NYP,(NXP*NYP),VCHAR,               &
                 IYR,IMO,IDA,IHR,IMN,IFH,(KL+1),ZERO)
     KREC=KREC+1
!    special diagnostic to determine precipitation bucket times 
     WRITE(50,*)'Total field precipitation: ',SUM(SVAR)
     GOTO 800

  ELSEIF(PGRIB.NE.142.AND.VGRIB.EQ.142)THEN 
!    no need to write this record, wait for #143
     GOTO 700
  END IF

!-----------------------------------------------------------------------
! If no special processing was required then assume it was  normal record
! which is then packed into ARL format and continue on to next record

  WRITE(50,'(1X,I5,1X,A,1X,3I5,F10.4)')KL,VCHAR,KPDS(7),KPDS(14),KPDS(15)
  CALL PAKREC(20,SVAR,CVAR,NXP,NYP,(NXP*NYP),VCHAR,                     &
              IYR,IMO,IDA,IHR,IMN,IFH,(KL+1),ZERO)
  KREC=KREC+1

!   save data and then position to the next grib record
700 PVAR =SVAR  
    PCHAR=VCHAR
    PGRIB=VGRIB
    KBYTE=KBYTE+KLEN
    CYCLE newrec

!   all records written, no previous data required
800 PVAR =0.0  
    PCHAR='    '
    PGRIB=0     
    KBYTE=KBYTE+KLEN
    END DO newrec

!   end of time-period
910 KRET=1    
    RETURN   

!   end of file
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
!                 07 Jan 2003 (RRD) - lat/lon subgrid option
!
! USAGE:  CALL MAKNDX( )
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

SUBROUTINE MAKNDX (NXP,NYP,CLAT,CLON,DIST,KGRID,KGRIB)

  USE metdata

  IMPLICIT NONE

  INTEGER,      INTENT(IN)   :: nxp           ! x dimension
  INTEGER,      INTENT(IN)   :: nyp           ! y dimension
  REAL,         INTENT(IN)   :: clat          ! center grid lat
  REAL,         INTENT(IN)   :: clon          ! center grid lon
  REAL,         INTENT(IN)   :: dist          ! grid resolution
  INTEGER,      INTENT(IN)   :: kgrid         ! grid type 
  INTEGER,      INTENT(IN)   :: kgrib         ! configuration file

  CHARACTER(4)  :: VCHAR(50) ! variable id
  CHARACTER(4)  :: GTYPE     ! grid identification
  CHARACTER(20) :: LABEL(18) ! optional field label

  LOGICAL       :: FTEST
  INTEGER       :: I,N,NL,MVAR  
  REAL          :: SIG
  REAL          :: GRIDS(12), PARMAP(9)

  COMMON / SETUP / GRIDS, PARMAP

! optional field label string
  DATA LABEL/'Model Type:','Grid Numb:','Vert Coord:','Pole Lat:',             &
    'Pole Lon:','Ref Lat:','Ref Lon:','Grid Size:','Orientation:',             &
    'Cone Angle:','Sync X Pt:','Sync Y Pt:','Sync Lat:','Sync Lon:',           &
    'Reserved:','Numb X pt:','Numb Y pt:','Numb Levels:'/

! reserve space for output data arrays
  IF(KGRID.EQ.0)THEN
!    conformal extract
     GTYPE=MODEL(1:3)//'X'

!    defines a polar sterographic projection
     IF(ABS(CLAT).GT.60.0)THEN
        GRIDS(1)=90.0                 ! pole latitude    
        IF(CLAT.LT.0.0)GRIDS(1)=-90.0
        GRIDS(2)=CLON                 ! pole longtitude (+180 from cut)
        GRIDS(3)=CLAT                 ! grid size valid at ref lat/lon   
        GRIDS(4)=CLON
        GRIDS(5)=DIST                 ! grid size
        GRIDS(6)=0.0                  ! orientation
        GRIDS(7)=GRIDS(1)             ! tangent latitude

!    defines a mercator projection
     ELSEIF(ABS(CLAT).LT.30.0)THEN
        GRIDS(1)=0.0
        GRIDS(2)=CLON
        GRIDS(3)=CLAT
        GRIDS(4)=CLON
        GRIDS(5)=DIST  
        GRIDS(6)=0.0  
        GRIDS(7)=0.0

!    defines a lambert conformal projection
     ELSE
        GRIDS(1)=CLAT
        GRIDS(2)=CLON
        GRIDS(3)=CLAT
        GRIDS(4)=CLON
        GRIDS(5)=DIST  
        GRIDS(6)=0.0 
        GRIDS(7)=CLAT
     END IF

!    synch point in x,y coordintes
     GRIDS(8)=(NXP+1.0)/2.0
     GRIDS(9)=(NYP+1.0)/2.0
!    synch point in lat/lon coordinates
     GRIDS(10)=CLAT
     GRIDS(11)=CLON

  ELSEIF(KGRID.EQ.1)THEN
!    northern hemisphere polar stereographic
     GTYPE=MODEL(1:3)//'N'

     GRIDS(1)=90.0
     GRIDS(2)=0.0
     GRIDS(3)=60.0
     GRIDS(4)=-80.0
     GRIDS(5)=DIST   
     GRIDS(6)=0.0  
     GRIDS(7)=90.0
     GRIDS(8)=(NXP+1.0)/2.0
     GRIDS(9)=(NYP+1.0)/2.0
     GRIDS(10)=90.0
     GRIDS(11)=0.0

  ELSEIF(KGRID.EQ.2)THEN
!    southern hemisphere polar stereographic
     GTYPE=MODEL(1:3)//'S'

     GRIDS(1)=-90.0
     GRIDS(2)=0.0
     GRIDS(3)=-60.0
     GRIDS(4)=-80.0
     GRIDS(5)=DIST   
     GRIDS(6)=0.0  
     GRIDS(7)=-90.0
     GRIDS(8)=(NXP+1.0)/2.0
     GRIDS(9)=(NYP+1.0)/2.0
     GRIDS(10)=-90.0
     GRIDS(11)=0.0

  ELSEIF(KGRID.EQ.3)THEN
!    global latitude-longitude grid
     GTYPE=MODEL(1:3)//'G'

!    Set lat/lon of lower left point
     GRIDS(10)= CLAT1 
     GRIDS(11)= CLON1    

!    global grid output must always start at the south pole
!    if input data go from north to south, data are remapped in REGRID 
     IF(DLAT.LT.0.0) GRIDS(10)=CLAT1-(NLAT-1)*ABS(DLAT)

!    grid should be defined on a 0->360 coordinate
     IF(GRIDS(11).LT.0.0)GRIDS(11)=360.0+GRIDS(11)

!    Pole lat/lon is used to identify the latlon point of the max index
     GRIDS(1)=GRIDS(10)+ABS(DLAT)*(NYP-1)
     GRIDS(2)=GRIDS(11)+DLON*(NXP-1)
     GRIDS(2)=AMOD(GRIDS(2),360.0)

     GRIDS(3)=ABS(DLAT) ! ref lat defines grid spacing
     GRIDS(4)=DLON      ! ref lon defines grid spacing

     GRIDS(5)=0.0  ! grid size zero for lat/lom
     GRIDS(6)=0.0  ! orientation
     GRIDS(7)=0.0  ! tangent latitude

!    sync x,y defines lower left grid point 
     GRIDS(8)=1.0 
     GRIDS(9)=1.0
            
!    index value on input grid of lower left corner extract point
     ILON1=1
     JLAT1=1 

  ELSEIF(KGRID.EQ.4)THEN
!    regional latitude-longitude grid
     GTYPE=MODEL(1:3)//'R'

!    extract only valid if input grid global
     IF((NLAT-1)*ABS(DLAT).LT.180.OR.NLON*DLON.LT.360)THEN
        WRITE(50,*)'Extract option -g4 not tested for all input grids'
        WRITE(50,*)'Check output for consistency ...'
     END IF

     GRIDS(10)=CLAT-INT(NYP/2)*ABS(DLAT)
     GRIDS(11)=CLON-INT(NXP/2)*DLON
     IF(GRIDS(11).LT.0.0)GRIDS(11)=360.0+GRIDS(11)
     WRITE(50,*)'Extract grid corner lower left: ',GRIDS(10),GRIDS(11)

     GRIDS(1)=GRIDS(10)+ABS(DLAT)*(NYP-1)
     GRIDS(2)=GRIDS(11)+DLON*(NXP-1)
     WRITE(50,*)'Extract grid corner upper rght: ',GRIDS(1),GRIDS(2)
     GRIDS(2)=AMOD(GRIDS(2),360.0)
     GRIDS(3)=ABS(DLAT)  ! ref lat defines grid spacing
     GRIDS(4)=DLON ! ref lon defines grid spacing
     GRIDS(5)=0.0  ! grid size zero for lat/lom
     GRIDS(6)=0.0  ! orientation
     GRIDS(7)=0.0  ! tangent latitude
     GRIDS(8)=1.0 
     GRIDS(9)=1.0 

     ILON1=1+(GRIDS(11)-CLON1)/DLON
     JLAT1=1+(GRIDS(10)-CLAT1)/ABS(DLAT)
     IF(DLAT.LT.0.0)JLAT1=1+(GRIDS(10)-CLAT2)/ABS(DLAT)
     WRITE(50,*)'Extract grid corner lower left: ',JLAT1,ILON1 

  END IF
  GRIDS(12)=0.0  ! variable reserved for future use

!------------------------------------------------------------------------
! write the packer configuration file

  IF(KGRIB.EQ.1)THEN
     INQUIRE(FILE='CFG_ARL',EXIST=FTEST)
     IF(FTEST)RETURN
     WRITE(*,*)'Command line -q1 options requires both files ...'
     WRITE(*,*)' ascii: CFG_ARL'
     WRITE(*,*)'binary: CFG_GRIB' 
     STOP
  END IF
  OPEN(30,FILE='CFG_ARL')

! default grid number 99 (field not used)
  WRITE(30,'(A20,A4)')LABEL(1),GTYPE 
  WRITE(30,'(A20,A4)')LABEL(2),'  99'

! coordinate (1:sigma 2:pressure 3:terrain 4:hybrid)
  WRITE(30,'(A20,I4)') LABEL(3), KVC      

! grid geolocation parameters and projection
  DO I=1,12
   ! format change for 0.125 grid 5-2-14
     WRITE(30,'(A20,F10.3)')LABEL(I+3),GRIDS(I) 
     !# UKMET WRITE(30,'(A20,F10.3)')LABEL(I+3),GRIDS(I)
  END DO

! grid dimensions
  WRITE(30,'(A20,I4)') LABEL(16), NXP
  WRITE(30,'(A20,I4)') LABEL(17), NYP
  WRITE(30,'(A20,I4)') LABEL(18),(NLVL+1)   

! upper level information
  DO NL=0,NLVL  

     WRITE(LABEL(1),'(A6,I2,A1)')'Level ',NL,':'
     IF(NL.EQ.0)THEN
        SIG=1.0     
        IF(KVC.EQ.2)SIG=0.0

        MVAR=0
        DO N=1,M2DV
           IF(EXIST0(N).EQ.1)THEN
              MVAR=MVAR+1
              VCHAR(MVAR)=VCHAR0(N)
           END IF
        END DO

     ELSE
        SIG=SIGL(NL)
        MVAR=0
        DO N=1,M3DV
           IF(EXIST1(N,NL).EQ.1)THEN
              MVAR=MVAR+1
              VCHAR(MVAR)=VCHAR1(N)
           END IF
        END DO
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
! USAGE:  CALL MKGRID( )
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

SUBROUTINE MKGRID(NXP,NYP,TLAT,TLON)

  IMPLICIT NONE

  INTEGER, INTENT(IN)  :: NXP,NYP    ! output grid dimensions
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
  DO J=1,NYP
  DO I=1,NXP

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
!                 07 Jan 2003 (RRD) - lat/lon subgrid option
!
! USAGE:  CALL REGRID( )
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

SUBROUTINE REGRID(KGRID,V1,NX1,NY1,V2,NX2,NY2)

  USE metdata

  IMPLICIT NONE

  INTEGER, INTENT(IN)    :: KGRID           ! output grid type 
  INTEGER, INTENT(IN)    :: NX1, NY1        ! input dimensions
  INTEGER, INTENT(IN)    :: NX2, NY2        ! output dimensions
  REAL,    INTENT(INOUT) :: V1(:,:)         ! input array
  REAL,    INTENT(OUT)   :: V2(:,:)         ! output array

  LOGICAL :: first = .true. 
  INTEGER :: i,j,ii,jj,ilo,ihi,jlo,jhi
  REAL    :: xp,yp,fxi,fyj,top,bot,temp

  SAVE first

! just move data into output array if latlon grid 
  IF(KGRID.GE.3)THEN

     DO J=1,NY2  
        JJ=JLAT1+J-1
!       flip order of vertical array element
!       when input data ordered north to south
        IF(DLAT.LT.0.0)JJ=NY1-JLAT1-J+2

        DO I=1,NX2   
           II=ILON1+I-1
!          assume wrap if grid goes around dateline
           IF(II.GT.NX1)II=II-NX1
           V2(I,J)=V1(II,JJ)        
        END DO

     END DO
     RETURN
          
  END IF

! interpolate values to new grid
  DO I=1,NX2
  DO J=1,NY2

!    test for prime meridian
     TEMP=TLON(I,J)-CLON1
     IF(TEMP.LT.0.0)TEMP=TEMP+360.0 

!    compute adjacent index values on grid 1
     XP=1.0+TEMP/DLON
     YP=1.0+(CLAT1-TLAT(I,J))/ABS(DLAT)

!    test if all grid points fall within the input data domain
     IF(FIRST)THEN
        IF(XP.LT.1.0.OR.XP.GE.FLOAT(NX1+1).OR.YP.LT.1.0.OR.YP.GE.FLOAT(NY1+1)) &
        THEN
           WRITE(*,*)'ERROR: ARL grid beyond the input data domain'
           WRITE(*,*)'Output grid: ',I,J,TLAT(I,J),TLON(I,J)
           WRITE(*,*)'Input  grid: ',XP,YP,NX1,NY1
           STOP 104
        END IF
     END IF

!    compute indicies
     ILO=INT(XP)
     IHI=ILO+1
     JLO=INT(YP)
     JHI=JLO+1

!    interpolation fractions (off grid extrapolated)
     FXI=XP-ILO
     FYJ=YP-JLO

!    assume input grid is global then check for wrap at prime meridian
     IF(IHI.GT.NX1)IHI=1
     IF(JHI.GT.NY1)JHI=1

!    interpolate across at top and bottom
     TOP=(V1(IHI,JHI)-V1(ILO,JHI))*FXI+V1(ILO,JHI)
     BOT=(V1(IHI,JLO)-V1(ILO,JLO))*FXI+V1(ILO,JLO)

!    interpolate between top and bottom
     V2(I,J)=(TOP-BOT)*FYJ+BOT

  END DO
  END DO

  IF(FIRST)FIRST=.FALSE. 

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
!                 20 Feb 2002 (RRD) - multiple time periods
!
! USAGE:  CALL ROTATE( )
!   INPUT ARGUMENT LIST:  see below
!   OUTPUT ARGUMENT LIST: none
!   INPUT FILES:          UNIT 20 - DATA.ARL direct access
!   OUTPUT FILES:         UNIT 20 - DATA.ARL direct access
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE:  IBM RS6000
!
!$$$

SUBROUTINE ROTATE

  USE metdata

  IMPLICIT NONE

  CHARACTER(50)  :: LABEL
  CHARACTER(108) :: HEADER
  CHARACTER(4)   :: VARB, VARBX, VARBY
  INTEGER        :: IYR,IMO,IDA,IHR,IFH,IMN
  INTEGER        :: NXP,NYP,NXY,KL,KG,NEXP,KREC,KSUM
  REAL           :: PREC,VAR1

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

  SUBROUTINE PAKOUT(RVAR,CVAR,NX,NY,NXY,PREC,NEXP,VAR1,KSUM)
  IMPLICIT NONE
  INTEGER,   INTENT(IN)  :: nx,ny,nxy   ! dimension limits
  REAL,      INTENT(IN)  :: rvar(nx,ny) ! data array to be packed  
  CHARACTER, INTENT(OUT) :: cvar(nxy)   ! packed char*1 output array
  REAL,      INTENT(OUT) :: prec        ! precision of packed data array
  INTEGER,   INTENT(OUT) :: nexp        ! packing scaling exponent
  REAL,      INTENT(OUT) :: var1        ! value of real array at position (1,1)
  INTEGER,   INTENT(OUT) :: ksum        ! rotating checksum of packed data 
  END SUBROUTINE pakout

  SUBROUTINE U2GRID(UU,VV,NX,NY)
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: NX, NY      ! array dimensions
  REAL, INTENT(INOUT) :: UU(:,:)     ! u component
  REAL, INTENT(INOUT) :: VV(:,:)     ! v component
  END SUBROUTINE u2grid

  END INTERFACE
!---------------------------------------------------------------------

  WRITE(50,*)'SUBROUTINE rotate ...'

! decode the standard portion of the index record
  READ(20,REC=1)LABEL,HEADER
  READ(HEADER(94:),'(2I3)')NXP,NYP
  WRITE(50,*)'Allocating rotation variables: ',NXP,NYP

  IMN=0
  NXY=NXP*NYP

  100 FORMAT(7I2,A4,I4,2E14.7)

  KREC=0
  DO WHILE (KREC.GE.0)     

     KREC=KREC+1
     READ(20,REC=KREC,ERR=900)LABEL
     READ(LABEL,'(14X,A4)')VARB

     IF(VARB(1:1).EQ.'U')THEN

!       first record of record pair gives u-component variable
        READ(20,REC=KREC)LABEL,CVAR
        READ(LABEL,100)IYR,IMO,IDA,IHR,IFH,KL,KG,VARBX,NEXP,PREC,VAR1
        KSUM=-1
        CALL PAKINP(SVAR,CVAR,NXP,NYP,1,1,NXP,NYP,PREC,NEXP,VAR1,KSUM)

!       second record in pair is required to be the v-component 
        KREC=KREC+1
        READ(20,REC=KREC)LABEL,CVAR
        READ(LABEL,100)IYR,IMO,IDA,IHR,IFH,KL,KG,VARBY,NEXP,PREC,VAR1
        KSUM=-1
        CALL PAKINP(PVAR,CVAR,NXP,NYP,1,1,NXP,NYP,PREC,NEXP,VAR1,KSUM)

        IF(VARBY(1:1).EQ.'V')THEN
!          both components available then rotate
           CALL U2GRID(SVAR,PVAR,NXP,NYP)

!          convert real to packed character and write component
           CALL PAKOUT(SVAR,CVAR,NXP,NYP,NXY,PREC,NEXP,VAR1,KSUM)
           WRITE(LABEL,100)IYR,IMO,IDA,IHR,IFH,KL,KG,VARBX,NEXP,PREC,VAR1
           WRITE(20,REC=(KREC-1))LABEL,CVAR

           CALL PAKOUT(PVAR,CVAR,NXP,NYP,NXY,PREC,NEXP,VAR1,KSUM)
           WRITE(LABEL,100)IYR,IMO,IDA,IHR,IFH,KL,KG,VARBY,NEXP,PREC,VAR1
           WRITE(20,REC=KREC)LABEL,CVAR

           WRITE(50,*)'Rotate: ',KREC,KL,VARBX,' ',VARBY
        ELSE
           WRITE(50,*)'Error ROTATE: record pair not U,V'
           STOP 105
        END IF
     END IF

  END DO

900 CONTINUE
    WRITE(50,*)'Rotate terminated after records: ',(KREC-1) 
    WRITE(50,*)'--------------------------------------------------------'

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
! USAGE:  CALL U2GRID( )
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
! SUBPROGRAM:  COMMAND          PROCESS ARGUMENTS ON THE PROGRAM COMMAND LINE
!   PRGMMR:    ROLAND DRAXLER   ORG: R/ARL       DATE:04-03-10
!
! ABSTRACT:  THIS CODE WRITTEN AT THE AIR RESOURCES LABORATORY ...
!            PROCESSES THE COMMAND LINE ARGUMENTS OR OTHERWISE SETS THE
!            DEFAULT VALUES FOR CERTAIN PARAMETERS THAT DEFINE THE
!            DATA SET TO BE OUTPUT IN PACKED ARL FORMAT
!
! PROGRAM HISTORY LOG:
!   LAST REVISED: 10 Mar 2004 (RRD) - initial version from main code 
!                 09 Feb 2011 (RRD) - rainfall accumulation time
!
! USAGE:  CALL COMMAND(  )
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

SUBROUTINE command(kgrib,clat,clon,dist,kgrid,nxp,nyp,klvls,  &
                   zero,sfcp,rain,nfile,ntime,ftype,fname)

  IMPLICIT NONE

  INTEGER,       INTENT(OUT) :: KGRIB       ! grib config file
  REAL,          INTENT(OUT) :: CLAT,CLON   ! extraction center 
  REAL,          INTENT(OUT) :: DIST        ! extraction grid resolution
  INTEGER,       INTENT(OUT) :: KGRID       ! output grid option
  INTEGER,       INTENT(OUT) :: NXP,NYP     ! output grid size
  INTEGER,       INTENT(OUT) :: KLVLS       ! number of output levels
  INTEGER,       INTENT(OUT) :: ZERO        ! initialization flag
  INTEGER,       INTENT(OUT) :: SFCP        ! use sfc pressure flag
  INTEGER,       INTENT(OUT) :: RAIN        ! precip accumulation hours
  INTEGER,       INTENT(OUT) :: NFILE       ! number of input files
  INTEGER,       INTENT(OUT) :: NTIME       ! number of time periods
  INTEGER,       INTENT(OUT) :: FTYPE(:)    ! file type
  CHARACTER(80), INTENT(OUT) :: FNAME(:)    ! input file names

  INTEGER        :: IARGC,NARG,KRET
  CHARACTER(80)  :: LABEL

! check for command line arguments
  NARG=IARGC()

  IF(NARG.EQ.0)THEN
     WRITE(*,*)'Usage: grib2arl [-options]'
     WRITE(*,*)' -i[primary grib data: file name {required}]'
     WRITE(*,*)' -s[supplemental grib data: file name {optional}]'
     WRITE(*,*)' -c[constant grib data: file name {optional}]'
     WRITE(*,*)' -d[distance (km) extract grid resolution {100.0}]'
     WRITE(*,*)' -x[subgrid extract center longitude {-80.0}]'
     WRITE(*,*)' -y[subgrid extract center latitude {60.0}]'
     WRITE(*,*)' -g[output projection  0 :conformal extract'
     WRITE(*,*)'                       1 :fixed northern hemisphere polar'
     WRITE(*,*)'                       2 :fixed southern hemisphere polar'
     WRITE(*,*)'                      {3}:lat-lon global grid (as input)'
     WRITE(*,*)'                       4 :lat-lon extract grid'
     WRITE(*,*)' -n[number of (x:y) extract grid points {100}]'
     WRITE(*,*)' -k[number of output levels including sfc {20}]'
     WRITE(*,*)' -p[surface defined by {1}:pressure or 0:terrain height]'
     WRITE(*,*)' -q[analyze grib file {0} or use saved configuration: 1]'
     WRITE(*,*)' -r[rain fall accumulation time hours: {6}]'
     WRITE(*,*)' -t[the number of time periods to process: {744}]'
     WRITE(*,*)' -z[zero initialization of output file 0:no {1}:yes]'
     STOP
  END IF

! default values

  CLAT= 60.0
  CLON=-80.0
  DIST=100.0
  KGRID=3
  NXP=100
  NYP=100
  KLVLS=20
  ZERO=1
  SFCP=1
  RAIN=6   !# UKMET set =0 to use precipitation rate (if available) rather than accumulation
  NFILE=0
  KGRIB=0
  NTIME=744 

! go through each argument

  DO WHILE (NARG.GT.0)
     CALL GETARG(NARG,LABEL)
     SELECT CASE (LABEL(1:2))

!    main grib input data file name   
     CASE ('-i','-I')
        NFILE=NFILE+1
        READ(LABEL(3:),'(A)')FNAME(NFILE)
        FTYPE(NFILE)=1

!    supplemental grib input data file name   
     CASE ('-s','-S')
        NFILE=NFILE+1
        READ(LABEL(3:),'(A)')FNAME(NFILE)
        FTYPE(NFILE)=2

!    constant grib input data file name   
     CASE ('-c','-C')
        NFILE=NFILE+1
        READ(LABEL(3:),'(A)')FNAME(NFILE)
        FTYPE(NFILE)=3

!    extract grid resolution       
     CASE ('-d','-D')
        READ(LABEL(3:),'(F10.0)')DIST  

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
        KRET=INDEX(LABEL,':')
        IF(KRET.NE.0)THEN
           READ(LABEL(3:(KRET-1)),'(I3)')NXP
           READ(LABEL((KRET+1):) ,'(I3)')NYP
        ELSE
           READ(LABEL(3:),'(I3)')NXP     
           NYP=NXP
        END IF

!    number of 3d output levels
     CASE ('-k','-K')
        READ(LABEL(3:),'(I2)')KLVLS 

!    surface pressure available
     CASE ('-p','-P')
        READ(LABEL(3:),'(I1)')SFCP

!    hours for rainfall accumulation
     CASE ('-r','-R')
        READ(LABEL(3:),'(I2)')RAIN 

!    read cfg_grib file for initialization
     CASE ('-q','-Q')
        READ(LABEL(3:),'(I1)')KGRIB

!    number of time periods to process    
     CASE ('-t','-T')
        READ(LABEL(3:),'(I4)')NTIME

!    file initialization    
     CASE ('-z','-Z')
        READ(LABEL(3:),'(I1)')ZERO
     END SELECT

     NARG=NARG-1
  END DO

! insure that the 3d file is always defined first

  DO KRET=1,NFILE
!    find index of type=1
     IF(FTYPE(KRET).EQ.1) NARG=KRET
  END DO

  IF(NARG.NE.1)THEN
     FTYPE(NARG)=FTYPE(1)
     FTYPE(1)=1
     LABEL=FNAME(1)
     FNAME(1)=FNAME(NARG)
     FNAME(NARG)=LABEL
  END IF

  WRITE(50,*)'Command Line Arguments: '
  WRITE(50,*)'  Cntr - ',INT(CLAT),INT(CLON)
  WRITE(50,*)'  Grid - ',KGRID
  WRITE(50,*)'  Size - ',NXP,NYP,KLVLS
  WRITE(50,*)'  SfcP - ',SFCP
  WRITE(50,*)'  Rain - ',RAIN
  WRITE(50,*)'  Init - ',ZERO
  WRITE(50,*)'---------------------------------------------------------'

END SUBROUTINE command



!##############################################################################
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM:  ANALYZE          ANALYZE THE FIRST TIME PERIOD RECORDS
!   PRGMMR:    ROLAND DRAXLER   ORG: R/ARL       DATE:04-03-10
!
! ABSTRACT:  THIS CODE WRITTEN AT THE AIR RESOURCES LABORATORY ...
!            ANALYZES THE INPUT GRIB FILE TO DETERMINE THE PROCESSING
!            REQUIRED TO CREATE THE DESIRED OUTPUT FILE. ALL RECORDS
!            IN THE FIRST TIME PERIOD OF EACH INPUT FILE ARE EXAMINED.
!
! PROGRAM HISTORY LOG:
!   LAST REVISED: 10 Mar 2004 (RRD) - initial version from main code 
!                 06 Apr 2007 (RRD) - file type to arg list
!                 03 Apr 2009 (RRD) - pressure levels now any order
!                 09 Feb 2011 (RRD) - override precip accumulation 
!                 20 Feb 2013 (RRD) - pass through max levels
!
! USAGE:  CALL ANALYZE(  )
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

SUBROUTINE analyze (handle,ftype,klvls,sfcp,rain,iend)

  USE metdata

  IMPLICIT NONE

  INTEGER,INTENT(IN) :: HANDLE      ! IO handle
  INTEGER,INTENT(IN) :: FTYPE       ! input file type  
  INTEGER,INTENT(IN) :: KLVLS       ! max levels permitted
  INTEGER,INTENT(IN) :: SFCP        ! sfc pressure flag
  INTEGER,INTENT(IN) :: RAIN        ! precip accumulation hours
  LOGICAL,INTENT(IN) :: IEND        ! flag for termination

  REAL,    ALLOCATABLE :: sigp(:)   ! dummy variable to remap levels
  INTEGER, ALLOCATABLE :: existx(:,:)        

  REAL         :: dummy(1),offset,sigma,sigv
  INTEGER      :: jj,levels
  INTEGER      :: kk,kv,kval

  INTEGER*8    :: kbyte             ! file byte position 
  INTEGER      :: klen              ! byte length indicator
  INTEGER      :: kret              ! return code

  INTEGER      :: iyr,imo,ida,ihr,ifh
  INTEGER      :: mac0,mac1,mac2,dmac

  LOGICAL      :: setmdl = .FALSE.  ! model center
  LOGICAL      :: setsig = .FALSE.  ! assume pressure level data
  LOGICAL      :: setgrd = .FALSE.  ! define the input grid
  LOGICAL      :: setlvl = .FALSE.  ! level information
  LOGICAL      :: set2dv = .FALSE.  ! two dimensional variables
  LOGICAL      :: set3dv = .FALSE.  ! three dimensional variables
  LOGICAL      :: newlev            ! test for new data level

!---------------------------------------------------------------------
! values for hybrid grids set in w3fi00 (levels = kgds(19)/2)

  COMMON /VCOORD/ LEVELS, OFFSET(100), SIGMA(100)

!---------------------------------------------------------------------
! maintain flags for multiple iterations

  SAVE setmdl,setsig,setgrd,setlvl,set2dv,set3dv

!---------------------------------------------------------------------
! When dealing with some F90 compiler replace ICHAR with JCHAR function

  CHARACTER(1)        :: mychr    
  INTEGER             :: jchar
  JCHAR(MYCHR)=IAND(ICHAR(MYCHR),255)

!---------------------------------------------------------------------
  INTERFACE

  SUBROUTINE W3FI00(BUFF,KPDS,KGDS,KBMS,RVAR,KPTR,KRET)
  CHARACTER(1), INTENT(IN)  :: BUFF(:)
  LOGICAL(1),   INTENT(OUT) :: KBMS(:)
  INTEGER,      INTENT(OUT) :: KPDS(:)
  INTEGER,      INTENT(OUT) :: KGDS(:)
  REAL,         INTENT(OUT) :: RVAR(:)
  INTEGER,      INTENT(OUT) :: KPTR(:)
  INTEGER,      INTENT(OUT) :: KRET
  END SUBROUTINE w3fi00

  END INTERFACE

!---------------------------------------------------------------------

  WRITE(50,*)'SUBROUTINE analyze ...'

  IF(.NOT.ALLOCATED(BUFF)) THEN
     ALLOCATE (BUFF(8))
     ALLOCATE (KBMS(1))
     RTIME=.TRUE.       ! assume input data time ordered
  END IF

  DMAC=0
  MAC0=0
  KLEN=0
  KBYTE=0

  newbyt : DO WHILE (KBYTE.GE.0)

! find the start of the grib record
  CALL FCPTPD(HANDLE,KBYTE,*910)
  CALL FCREAD(HANDLE,BUFF,1,8,*910)
  IF(BUFF(1)//BUFF(2)//BUFF(3)//BUFF(4).NE.'GRIB')THEN
     KBYTE=KBYTE+1
     CYCLE newbyt
  END IF

! load the indicator section to determine record length
  KLEN=JCHAR(BUFF(7))+JCHAR(BUFF(6))*256+JCHAR(BUFF(5))*65536

! reallocate buffer if more array space required 
  IF(KLEN.GT.SIZE(BUFF,1))THEN
     DEALLOCATE (BUFF,STAT=KRET)
     ALLOCATE (BUFF(KLEN),STAT=KRET)
     IF(KRET.NE.0)THEN
        WRITE(50,*)'GRIB Buffer allocation error: ',KRET,KLEN
        STOP 107
     ELSE
        WRITE(50,*)'Input buffer allocation: ',KLEN
     END IF
  END IF

! read the entire data record
  CALL FCPTPD(HANDLE,KBYTE,*910)
  CALL FCREAD(HANDLE,BUFF,1,KLEN,*910)

! call the nmc light grib unpacker (only unpacks headers)
  CALL W3FI00(BUFF,KPDS,KGDS,KBMS,DUMMY,KPTR,KRET)
  IF(KRET.NE.0)THEN
     WRITE(50,*)'Error W3FI00: ',KRET
     STOP 108
  END IF

! check the valid time for the data record. This program is 
! designed to process only one time period

  KPDS(8)=MOD(KPDS(8),100) ! century fix
  IYR=KPDS(8)
  IMO=KPDS(9)
  IDA=KPDS(10)
  IHR=KPDS(11)

! determine which forecast hour valid
  SELECT CASE (KPDS(16))
     CASE(0)

!       Only apply +P1 adjustment when special multi-time accumulation
!       fields are defined in their own file. Assume that when fluxes
!       are mixed with 3D variables in the same file, they are valid at
!       the initial time rather than the termination time.

!       A forecast sfc flux file will have all records in proper time
!       sequence, aligned with the 3d file.  However, an analysis flux
!       file will have, for the same initialization time, some variables
!       valid at +P1 mixed in-between those valid at P1=0. 

        IFH=0
        IF(KPDS(14).GT.0) IFH=KPDS(14) 
     CASE(1)          ! analysis
        IFH=0
     CASE(2,3,4)      ! valid from +P1 to +P2
        IFH=KPDS(15)
     CASE(10)
        IFH=KPDS(14)  ! valid at +P1
     CASE DEFAULT
        IFH=0
  END SELECT

  IF(FTYPE.LE.2)THEN
!    compute the current time only for non-constant files
!    modified: 06 Apr 2007
     CALL TMPLUS(IYR,IMO,IDA,IHR,IFH)
     CALL TM2MIN(IYR,IMO,IDA,IHR,0,MAC2)

     IF(MAC0.EQ.0)THEN
        WRITE(50,*)'First time period found: ',IYR,IMO,IDA,IHR
        MAC0=MAC2
        MAC1=MAC2

     ELSE
!       time period analysis
        IF(MAC2.GT.MAC1) THEN
           WRITE(50,*)'New time period found: ',IYR,IMO,IDA,IHR
           DMAC=MAC2-MAC1

!          assume only one full time period needs to be analyzed (3D files)
!          2D files may have missing fluxes at time=0, therefore skip test
           IF(((MAC2-MAC0).GT.2*DMAC).AND.FTYPE.EQ.1) EXIT newbyt
           MAC1=MAC2

        ELSEIF(MAC2.LT.MAC1)THEN
           WRITE(50,*)'Data not time ordered: ',IYR,IMO,IDA,IHR
           RTIME=.FALSE. 
        END IF
     END IF
  END IF

!------------------------------------------------------------------
! Determine meteorological data source and set code tables for
! the maximum number of input variables supported by the converter.

  IF(.NOT.setmdl)THEN

     ALLOCATE (STYP(M2DV),SIG0(M2DV),STYPS(M2DV))
     ALLOCATE (VGRIB0(M2DV),VCHAR0(M2DV),CNVRT0(M2DV),EXIST0(M2DV))
     ALLOCATE (VGRIB1(M3DV),VCHAR1(M3DV),CNVRT1(M3DV),EXIST1(M3DV,MLEV))

     EXIST0=0  ! surface fields flag
     EXIST1=0  ! upper air fields flag

     SELECT CASE (KPDS(1))

        CASE(98) ! ECMWF Center (some fields disabled with 9 prefix due to accumulation issue)
           setmdl=.TRUE.
           MODEL='ECM'
           IF(KPDS(6).NE.100) setsig=.TRUE.

!# UKMET   VGRIB0 = (/   90 ,   90 ,  152 ,  134 ,  164 ,  165 ,  166 ,  167 ,  229 ,  230 ,  231 ,   106,  159 /)
!# UKMET   SIG0   = (/    1 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,     0,    0 /)
!# UKMET   STYPS  = (/  109 ,    1 ,    1 ,    1 ,    1 ,  109 ,  109 ,  109 ,    1 ,    1 ,    1 ,     1,    1 /)
!# UKMET   STYP   = (/  109 ,  109 ,    1 ,    1 ,    1 ,    1 ,    1 ,    1 ,    1 ,    1 ,    1 ,     1,    1 /)
!# UKMET   VCHAR0 = (/'SHGT','SHGT','PRSS','PRSS','TCLD','U10M','V10M','T02M','UMOF','VMOF','SHTF','PRT3','PBLH'/)
!# UKMET   CNVRT0 = (/0.102, 0.102 , -.01 , 0.01 ,100.0 ,  1.0 ,  1.0 ,  1.0 ,  1.0 ,  1.0 ,  1.0 , 0.06 ,  1.0 /)

           VGRIB0 = (/  129 ,  129 ,  152 ,  134 ,  228 ,  165 ,  166 ,  167 , 9180 , 9181 , 9146 ,   142,  159 /)
           SIG0   = (/    1 ,    0 ,    0 ,    1 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,    0 ,     0,    0 /)
           STYPS  = (/  109 ,    1 ,    1 ,    1 ,    1 ,  109 ,  109 ,  109 ,    1 ,    1 ,    1 ,     1,    1 /)
           STYP   = (/  109 ,  109 ,    1 ,  109 ,    1 ,    1 ,    1 ,    1 ,    1 ,    1 ,    1 ,     1,    1 /)
           VCHAR0 = (/'SHGT','SHGT','PRSS','PRSS','TPP6','U10M','V10M','T02M','UMOF','VMOF','SHTF','TPP6','PBLH'/)
           CNVRT0 = (/0.102, 0.102 , -.01 , 0.01 ,  1.0 ,  1.0 ,  1.0 ,  1.0 ,  1.0 ,  1.0 ,  1.0 ,  1.0 ,  1.0 /)

           VGRIB1 = (/  129 ,  130 ,  131 ,  132 ,  135 ,  133 ,  157 /)
           VCHAR1 = (/'HGTS','TEMP','UWND','VWND','WWND','SPHU','RELH'/)
           CNVRT1 = (/0.102 ,  1.0 ,  1.0 ,  1.0 , 0.01 ,  1.0 ,  1.0 /)

        CASE(7, 42, 58)  ! NOAA NCEP(7) or other centers (AR-42, FNMOC-58) running NCEP models
           setmdl=.TRUE.
           MODEL='GFS'
           IF(KPDS(1).EQ.42) MODEL='ARM'
           IF(KPDS(1).EQ.58) MODEL='NGP'
           IF(KPDS(6).NE.100) WRITE(50,*)'NOAA sigma not supported'

           VGRIB0 = (/    0 ,    7 ,    0 ,    1 ,   61 ,   33 ,   34,    11 ,  124 ,  125 ,  122 ,    67,   221 /)
           SIG0   = (/    0 ,    0 ,    0 ,    0 ,    0 ,   10 ,   10,     2 ,    0 ,    0 ,    0 ,     0,     0 /)
           STYPS  = (/    0 ,    1 ,    0 ,    1 ,    1 ,  105 ,  105 ,  105 ,    1 ,    1 ,    1 ,     1,     1 /)
           STYP   = (/    0 ,    1 ,    0 ,    1 ,    1 ,  105 ,  105 ,  105 ,    1 ,    1 ,    1 ,     1,     1 /)
           VCHAR0 = (/'    ','SHGT','    ','PRSS','TPP6','U10M','V10M','T02M','UMOF','VMOF','SHTF','MXHT','PBLH' /)
           CNVRT0 = (/  0.0 ,  1.0 ,  0.0 , 0.01 ,0.001 ,  1.0 ,  1.0 , 1.0  ,  1.0 ,  1.0 ,  1.0 ,   1.0,   1.0 /)

           VGRIB1 = (/    7 ,   11 ,   33 ,   34 ,   39 ,   51 ,   52 /)
           VCHAR1 = (/'HGTS','TEMP','UWND','VWND','WWND','SPHU','RELH'/)
           CNVRT1 = (/  1.0 ,  1.0 ,  1.0 ,  1.0 , 0.01 ,  1.0 ,  1.0 /)

        CASE(74)  ! UK Met Office (some issues remain regarding correct GRIB-ID code for surface pressure)
           setmdl=.TRUE.
           MODEL='UKMO'
           IF(KPDS(6).NE.100) WRITE(50,*)'UKMet sigma not supported'

           VGRIB0 = (/    0 ,    7 ,    0 ,    2 ,   61 ,   33 ,   34,    11 ,  124 ,  125 ,  122 ,    67,   221 /)
           SIG0   = (/    0 ,    0 ,    0 ,    0 ,    0 ,   10 ,   10,     2 ,    0 ,    0 ,    0 ,     0,     0 /)
           STYPS  = (/    0 ,    1 ,    0 ,    0 ,    1 ,  105 ,  105 ,  105 ,    1 ,    1 ,    1 ,     1,     1 /)
           STYP   = (/    0 ,    1 ,    0 ,    0 ,    1 ,  105 ,  105 ,  105 ,    1 ,    1 ,    1 ,     1,     1 /)
           VCHAR0 = (/'    ','SHGT','    ','PRSS','TPP6','U10M','V10M','T02M','UMOF','VMOF','SHTF','MXHT','PBLH' /)
           CNVRT0 = (/  0.0 ,  1.0 ,  0.0 , 0.01 ,0.001 ,  1.0 ,  1.0 , 1.0  ,  1.0 ,  1.0 ,  1.0 ,   1.0,   1.0 /)

           VGRIB1 = (/    7 ,   11 ,   33 ,   34 ,   39 ,   51 ,   52 /)
           VCHAR1 = (/'HGTS','TEMP','UWND','VWND','WWND','SPHU','RELH'/)
           CNVRT1 = (/  1.0 ,  1.0 ,  1.0 ,  1.0 , 0.01 ,  1.0 ,  1.0 /)

        CASE DEFAULT
           WRITE(50,*)'Unsupported meteorological center: ',KPDS(1)
           STOP 110

     END SELECT

!    Note that NOAA precip units kg/m2 while ECMWF units are meters
!    check for precipitation accumulation command line over-ride
     IF(RAIN.GT.24)THEN
        VCHAR0(5)='TPPA'
     ELSEIF(RAIN.EQ.24)THEN
        VCHAR0(5)='TPPD'
     ELSEIF(RAIN.EQ.12)THEN
        VCHAR0(5)='TPPT'
     ELSEIF(RAIN.EQ.6)THEN
        VCHAR0(5)='TPP6'
     ELSEIF(RAIN.EQ.3)THEN
        VCHAR0(5)='TPP3'
     ELSEIF(RAIN.EQ.1)THEN
        VCHAR0(5)='TPP1'
     ELSE
        CONTINUE
     END IF

     setmdl=.TRUE.
     WRITE(50,*)'GRIB code tables set for data center: ',KPDS(1),MODEL
  END IF

!------------------------------------------------------------------
! Determine number of levels

  IF(.NOT.ALLOCATED(SIGL))THEN
     WRITE(50,*)'Vertical levels in kgds(19): ',kgds(19)
     WRITE(50,*)'Vertical levels from levels: ',levels
     IF(KGDS(19).EQ.0)KGDS(19)=100
     ALLOCATE (SIGL(MAX(LEVELS,KGDS(19))))
     WRITE(50,*)'Vertical level allocation: ',SIZE(SIGL,1)
     NLVL=0
  END IF

  IF(.NOT.setlvl)THEN

     SELECT CASE (KPDS(6))

        CASE(100)  ! pressure level data (loop until array filled)
           KVC=2
           IF(NLVL.EQ.0)THEN
              NLVL=NLVL+1
              SIGL(NLVL)=KPDS(7)
              WRITE(50,*)'New level found: ',NLVL,SIGL(NLVL)
      
           ELSE
!             create in order of descending pressure         
!#            IF(KPDS(7).LT.INT(MINVAL(SIGL(1:NLVL))).OR.    &
!#               KPDS(7).GT.INT(MAXVAL(SIGL(1:NLVL))))THEN
!#               NLVL=NLVL+1
!#               SIGL(NLVL)=KPDS(7)
!#               WRITE(50,*)'New level found: ',NLVL,SIGL(NLVL)
!#            END IF 

!             revised test (03 Apr 2009)
              NEWLEV=.TRUE.  
              DO KK=1,NLVL
                 IF(KPDS(7).EQ.SIGL(KK))NEWLEV=.FALSE.
              END DO
              IF(NEWLEV)THEN
                 NLVL=NLVL+1
                 SIGL(NLVL)=KPDS(7)
                 WRITE(50,*)'New level found: ',NLVL,SIGL(NLVL)
              END IF
           END IF
           IF(NLVL.EQ.KGDS(19)) setlvl=.TRUE.

        CASE(109)  ! ecmwf hybrid data (all levels defined)
           KVC=4
!          Sigma levels represent the grid interface points, while the
!          output sigma levels are defined at the grid center. Note
!          values for LEVELS, OFFSET, SIGMA set in w3fi00. 

           WRITE(50,*)'ECMWF input sigma levels: ',LEVELS
           WRITE(50,*)' Grib  ARL    Offset     Sigma'
           DO KK=LEVELS,2,-1
              KVAL=INT(0.5*(OFFSET(KK) + OFFSET(KK-1)))
              SIGV=    0.5*( SIGMA(KK) +  SIGMA(KK-1))
              WRITE(50,'(2I5,I10,F10.4)')KK,(1+LEVELS-KK),KVAL,SIGV            
              SIGL(1+LEVELS-KK)=KVAL+SIGV 
           END DO
           NLVL=LEVELS-1
           setlvl=.TRUE.

     END SELECT
  END IF

!------------------------------------------------------------------
! Determine which variables exist in the file

  IF(.NOT.set2dv)THEN
     IF(SUM(exist0).EQ.m2dv)THEN
!       all 2-dim variables (sfc) found, stop search
        set2dv=.TRUE.
     ELSE
        DO kv=1,m2dv
           IF(KPDS(5).EQ.VGRIB0(KV).AND.                        &
             (KPDS(6).EQ.STYP(KV).OR.KPDS(6).EQ.STYPS(KV)).AND. &
              KPDS(7).EQ.SIG0(KV).AND.EXIST0(KV).EQ.0)THEN
              EXIST0(KV)=1
              WRITE(50,*)'Surface variable match: ',VCHAR0(KV)
           END IF
        END DO
     END IF
  END IF

  IF(.NOT.set3dv.AND.(KPDS(6).EQ.100.OR.KPDS(6).EQ.109))THEN
!#   IF(SUM(exist1).EQ.m3dv)THEN
!#      all 3-dim variables found, no need to continue search
!#      set3dv=.TRUE.
!#   ELSE
!       search all levels to see which variables present 
        DO kv=1,m3dv
           IF(KPDS(5).EQ.VGRIB1(KV).AND.EXIST1(KV,NLVL).EQ.0)THEN
              WRITE(50,*)'Upper level variable match: ',VCHAR1(KV)
              IF(NEWLEV)THEN
!                still counting new levels
                 EXIST1(KV,NLVL)=1
                 WRITE(50,*)'Variable applied to level: ',NLVL,'  Max=',KLVLS
              ELSE
!                all levels already identified
                 EXIST1(KV,:)=1
                 WRITE(50,*)'Variable applied to all levels!'
              END IF
!             special case of hybrid coordinate
              IF(KVC.EQ.4) EXIST1(KV,:)=1
           END IF
        END DO
!#   END IF
  END IF

!------------------------------------------------------------------
! Define the input grid

  IF(.NOT.setgrd)THEN
     NLON=KGDS(2)
     NLAT=KGDS(3)
     CLAT1=KGDS(4)/1000.0
     CLON1=KGDS(5)/1000.0
     CLAT2=KGDS(7)/1000.0
     CLON2=KGDS(8)/1000.0
     DLON=KGDS(9)/1000.0
     DLAT=KGDS(10)/1000.0
     SHAPE(1)=NLON
     SHAPE(2)=NLAT

!    determine y-grid direction as N->S or S->N and reverse 
!    the sign to indicate if the grid index increases with
!    decreasing latitude for the input data

     IF((CLAT1+(DLAT*(NLAT-1)).GT.90.0.AND.DLAT.GT.0.0).OR.  &
        (CLAT1.GT.CLAT2))THEN        
!       input data must be N->S, then reverse sign
!       output data will always be remapped to S->N
        DLAT=-DLAT
        WRITE(50,*)'Input data array from North to South'
     END IF

!    shift to 0 to 360 system
     IF(CLON1.LT.0.0)CLON1=360.0+CLON1 
   ! format change for 0.125 grid 5-2-14
     WRITE(50,'(1X,A,4F10.3)')   &
       'Input data corner and spacing: ',CLAT1,CLON1,DLAT,DLON

     ALLOCATE (XVAR(NLON,NLAT),RVAR(NLON*NLAT))
     WRITE(50,*)'Input data array allocation (nlon,nlat): ',NLON,NLAT
     setgrd=.TRUE.

!    allocate bitmap array space regardless of grib file content 
!    note that decoding the bit map section requires the NCEP w3lib
     DEALLOCATE (KBMS)
     ALLOCATE (KBMS(NLON*NLAT))
     WRITE(50,'(1X,A,I8)')'Bit map array allocation: ',(NLON*NLAT)
  END IF

! all records written, no previous data required
  KBYTE=KBYTE+KLEN
  END DO newbyt

! end of data
910 CONTINUE

!------------------------------------------------------------------
! Check results for consistency and modifiy accordingly 

! assume all upper level data are in the same file
  IF(NLVL.GT.0) THEN
     setlvl=.TRUE.
     set3dv=.TRUE.
  END IF

  WRITE(50,*)' '
  IF(.NOT.IEND) RETURN  ! only continue after last file processed

! make sure pressure levels such that index=1 is at the bottom
! revised (03 Apr 2009) to check all levels for proper order
  IF(KVC.EQ.2)THEN
!#   IF(SIGL(1).LT.SIGL(NLVL))THEN
!#      ALLOCATE (SIGP(NLVL),EXISTX(M3DV,MLEV))
!#      SIGP(NLVL:1:-1)=SIGL
!#      EXISTX(:,NLVL:1:-1)=EXIST1
!#      SIGL=SIGP          
!#      EXIST1=EXISTX
!#      DEALLOCATE (SIGP,EXISTX)
!#   END IF

     ALLOCATE (SIGP(SIZE(SIGL,1)),EXISTX(M3DV,MLEV))
!#   SIGP=SIGL
     DO JJ=1,NLVL
        SIGP(JJ)=SIGL(JJ)
     END DO
     EXISTX=EXIST1

     DO KK=1,NLVL
        KV=MAXLOC(SIGP,DIM=1)
        SIGL(KK)=SIGP(KV)
        EXIST1(:,KK)=EXISTX(:,KV)
        SIGP(KV)=0
     END DO
     DEALLOCATE (SIGP,EXISTX)
  END IF

! remove height field from 3d sigma-level ecmwf data
  IF(KVC.EQ.4) EXIST1(1,:)=0

! model requires either surface pressure or terrain height
  IF(EXIST0(1)+EXIST0(2)+EXIST0(3)+EXIST0(4).EQ.0)THEN
     WRITE(50,*)'Input terrain height or surface pressure required'
     STOP 113
  ELSEIF((EXIST0(3)+EXIST0(4)).EQ.0)THEN
!    surface pressure not available 
!    (passed first test, therefore terrain height available)
     CONTINUE  
  ELSEIF(SFCP.EQ.1)THEN
!    to get this far both fields must be available, select pressure
     EXIST0(1)=0
     EXIST0(2)=0
  ELSEIF(EXIST0(4).EQ.1.AND.KPDS(1).EQ.98)THEN
!    special CTBTO pressure field, always drop terrain
     EXIST0(1)=0
     EXIST0(2)=0
  ELSE
!    both fields available, terrain selected on command line
     EXIST0(3)=0
     EXIST0(4)=0
  END IF

! don't need both SPHU and RELH
  IF(EXIST1(6,1).EQ.1.AND.EXIST1(7,1).EQ.1) EXIST1(6,:)=0
  IF(EXIST1(6,1).EQ.0.AND.EXIST1(7,1).EQ.0)                     &
     WRITE(50,*)'WARNING: moisture fields missing (SPHU or RELH)'

! vertical motion desired but not required     
  IF(EXIST1(5,1).EQ.0)                                          &
     WRITE(50,*)'WARNING: vertical motion field missing (WWND)'

! when upper level on pressure surfaces the height field is required
  IF(KVC.EQ.2.AND.EXIST1(1,1).EQ.0)THEN
     WRITE(50,*)'ERROR: height field required with pressure levels'
     STOP 1131
  END IF

! set number of fields
  WRITE(50,*)'Number of 2d & 3d variables: ',SUM(exist0),SUM(exist1(:,1))
  WRITE(50,*)'---------------------------------------------------------'

! write binary configuration file to speed multi-data set conversions
  OPEN(60,FILE='CFG_GRIB',FORM='UNFORMATTED')
  WRITE(60) SIZE(BUFF,1)
  WRITE(60) NLVL   
  WRITE(60) MODEL
  WRITE(60) VGRIB0  
  WRITE(60) STYP
  WRITE(60) STYPS 
  WRITE(60) SIG0  
  WRITE(60) VCHAR0
  WRITE(60) CNVRT0
  WRITE(60) EXIST0
  WRITE(60) VGRIB1
  WRITE(60) VCHAR1
  WRITE(60) CNVRT1
  WRITE(60) EXIST1
  WRITE(60) SIZE(SIGL,1)
  WRITE(60) SIGL 
  WRITE(60) KVC    
  WRITE(60) KPDS
  WRITE(60) KGDS
  WRITE(60) KPTR
  WRITE(60) SHAPE
  WRITE(60) NLON,NLAT       
  WRITE(60) ILON1,JLAT1           
  WRITE(60) CLAT1,CLON1          
  WRITE(60) DLON,DLAT
  WRITE(60) LEVELS
  WRITE(60) OFFSET
  WRITE(60) SIGMA
  WRITE(60) RTIME          
  CLOSE(60)

END SUBROUTINE analyze


!##############################################################################
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM:  SETGRID          CREATES THE OUTPUT GRID
!   PRGMMR:    ROLAND DRAXLER   ORG: R/ARL       DATE:04-03-10
!
! ABSTRACT:  THIS CODE WRITTEN AT THE AIR RESOURCES LABORATORY ...
!            MERGES THE ANALYZED INPUT GRID INFORMATION WITH THE 
!            COMMAND LINE OPTIONS TO DEFINE THE OUTPUT GRID STRUCTURE
!
! PROGRAM HISTORY LOG:
!   LAST REVISED: 10 Mar 2004 (RRD) - initial version from main code 
!
! USAGE:  CALL SETGRID(  )
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

SUBROUTINE SETGRID (kgrid,kgrib,nxp,nyp,klvls,clat,clon,dist)

  USE metdata

  IMPLICIT NONE

  INTEGER, INTENT(IN)    :: kgrid     ! output grid definition
  INTEGER, INTENT(IN)    :: kgrib     ! configuation file 
  INTEGER, INTENT(IN)    :: klvls     ! number of output levels
  INTEGER, INTENT(INOUT) :: nxp,nyp   ! output grid size
  REAL,    INTENT(IN)    :: clat,clon ! center of output grid
  REAL,    INTENT(IN)    :: dist      ! grid resolution       

  INTEGER       :: nzp
  CHARACTER(80) :: fname

!--------------------------------------------------------------------
  INTERFACE

  SUBROUTINE PAKSET(LUNIT,FNAME,KREC1,NXP,NYP,NZP)
  IMPLICIT NONE
  INTEGER,       INTENT(IN)    :: lunit     ! output unit number
  CHARACTER(*),  INTENT(INOUT) :: fname     ! file name of METDATA.CFG
  INTEGER,       INTENT(IN)    :: krec1     ! position of index record at time-1
  INTEGER,       INTENT(OUT)   :: nxp, nyp  ! horizontal grid dimensions
  INTEGER,       INTENT(OUT)   :: nzp       ! vertical grid dimension (incl sfc)
  END SUBROUTINE pakset

  SUBROUTINE MKGRID(NXP,NYP,TLAT,TLON)
  IMPLICIT NONE
  INTEGER, INTENT(IN)  :: NXP,NYP    ! output grid dimensions
  REAL,    INTENT(OUT) :: TLAT(:,:)  ! position of nodes
  REAL,    INTENT(OUT) :: TLON(:,:)  ! position of nodes
  END SUBROUTINE mkgrid

  SUBROUTINE MAKNDX (NXP,NYP,CLAT,CLON,DIST,KGRID,KGRIB)
  IMPLICIT NONE
  INTEGER,      INTENT(IN)   :: nxp           ! x dimension
  INTEGER,      INTENT(IN)   :: nyp           ! y dimension
  REAL,         INTENT(IN)   :: clat          ! center grid lat
  REAL,         INTENT(IN)   :: clon          ! center grid lon
  REAL,         INTENT(IN)   :: dist          ! grid resolution 
  INTEGER,      INTENT(IN)   :: kgrid         ! grid type 
  INTEGER,      INTENT(IN)   :: kgrib         ! configuration file
  END SUBROUTINE makndx

  END INTERFACE
!--------------------------------------------------------------------

  WRITE(50,*)'SUBROUTINE setgrid ...'

  IF(KGRID.EQ.0)THEN
!    conformal extract  
     WRITE(50,*)'Configured conformal extract grid'

  ELSEIF(KGRID.EQ.1)THEN
!    northern hemisphere polar stereographic
     WRITE(50,*)'Configured northern hemisphere grid'

  ELSEIF(KGRID.EQ.2)THEN
!    southern hemisphere polar stereographic
     WRITE(50,*)'Configured southern hemisphere grid'

  ELSEIF(KGRID.EQ.3)THEN
!    global latitude-longitude grid (same as input)
     NXP=NLON
     NYP=NLAT
     WRITE(50,*)'Configured global lat/lon output grid'

  ELSEIF(KGRID.EQ.4)THEN
!    extract latitude-longitude grid 
!    check limits (output grid can't be larger than input)
     NXP=MIN(NXP,NLON)
     NYP=MIN(NYP,NLAT)
     WRITE(50,*)'Configured extract lat/lon grid'
  END IF

! reserve space for output data arrays
  ALLOCATE (CVAR(NXP*NYP))
  ALLOCATE (TLAT(NXP,NYP),TLON(NXP,NYP))
  ALLOCATE (SVAR(NXP,NYP),PVAR(NXP,NYP),QVAR(NXP,NYP))
  WRITE(50,*)'Output array allocation: ',NXP,NYP

! limit output to command line option
  NLVL=MIN(KLVLS,NLVL)

! create the configuration file 
  CALL MAKNDX(NXP,NYP,CLAT,CLON,DIST,KGRID,KGRIB)

! configure the packing routines
  FNAME='CFG_ARL'
  CALL PAKSET(20,FNAME,1,NXP,NYP,NZP)
  WRITE(50,*)'Set grid from pakset: ',nxp,nyp,nzp

! determine how the input data will be remapped 
  IF(KGRID.LE.2) CALL MKGRID(NXP,NYP,TLAT,TLON)

! standard output name for packed data
  OPEN(20,FILE='DATA.ARL',RECL=(50+NXP*NYP),ACCESS='DIRECT',FORM='UNFORMATTED')

  WRITE(50,*)'-----------------------------------------------------------'
END SUBROUTINE setgrid


!##############################################################################
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM:  SETPROG          READS NAMELIST FILE PRODUCED BY ANALYZE
!   PRGMMR:    ROLAND DRAXLER   ORG: R/ARL       DATE:04-03-10
!
! ABSTRACT:  THIS CODE WRITTEN AT THE AIR RESOURCES LABORATORY ...
!            USES THE RESULTS OF A PREVIOUS RUN NAMELIST FILE CREATED BY
!            ANALYZE THE INPUT GRIB FILE TO DETERMINE THE PROCESSING
!            REQUIRED TO CREATE THE DESIRED OUTPUT FILE.
!
! PROGRAM HISTORY LOG:
!   LAST REVISED: 10 Mar 2004 (RRD) - initial version from main code 
!
! USAGE:  CALL SETPROG(  )
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

SUBROUTINE setprog 

  USE metdata

  IMPLICIT NONE

  REAL         :: offset,sigma
  INTEGER      :: levels, klen 

  COMMON /VCOORD/ LEVELS, OFFSET(100), SIGMA(100)

  WRITE(50,*)'SUBROUTINE setprog ...'

  OPEN(60,FILE='CFG_GRIB',FORM='UNFORMATTED')

  READ(60) KLEN
  READ(60) NLVL   
  ALLOCATE (BUFF(KLEN))
  WRITE(50,*)'Buffer allocation: ',KLEN
     ALLOCATE (STYP(M2DV),SIG0(M2DV),STYPS(M2DV))
     ALLOCATE (VGRIB0(M2DV),VCHAR0(M2DV),CNVRT0(M2DV),EXIST0(M2DV))
     ALLOCATE (VGRIB1(M3DV),VCHAR1(M3DV),CNVRT1(M3DV),EXIST1(M3DV,MLEV))

  READ(60) MODEL
  READ(60) VGRIB0  
  READ(60) STYP
  READ(60) STYPS 
  READ(60) SIG0  
  READ(60) VCHAR0
  READ(60) CNVRT0
  READ(60) EXIST0
  READ(60) VGRIB1
  READ(60) VCHAR1
  READ(60) CNVRT1
  READ(60) EXIST1

  READ(60) KLEN
     ALLOCATE (SIGL(KLEN))
     WRITE(50,*)'Vertical array allocation: ',KLEN

  READ(60) SIGL 
  READ(60) KVC    
  READ(60) KPDS
  READ(60) KGDS
  READ(60) KPTR
  READ(60) SHAPE

  READ(60) NLON,NLAT 
     ALLOCATE (XVAR(NLON,NLAT),RVAR(NLON*NLAT))
     WRITE(50,*)'Input data array allocation (nlon,nlat): ',NLON,NLAT

  READ(60) ILON1,JLAT1           
  READ(60) CLAT1,CLON1          
  READ(60) DLON,DLAT
  READ(60) LEVELS
  READ(60) OFFSET
  READ(60) SIGMA
  READ(60) RTIME          
  CLOSE(60)
  WRITE(50,*)'---------------------------------------------------------'

END SUBROUTINE setprog


!##############################################################################
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM:  WRTCON           WRITES ANY CONSTANTS FIELDS
!   PRGMMR:    ROLAND DRAXLER   ORG: R/ARL       DATE:04-03-10
!
! ABSTRACT:  THIS CODE WRITTEN AT THE AIR RESOURCES LABORATORY ...
!            WRITES ANY CONSTANTS FIELDS PREVIOUSLY SAVED BY XTRACT
!
! PROGRAM HISTORY LOG:
!   LAST REVISED: 10 Mar 2004 (RRD) - initial version from main code 
!
! USAGE:  CALL SETPROG(  )
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

SUBROUTINE wrtcon (krec,zero,nxp,nyp)

  USE metdata

  INTEGER, INTENT(INOUT) :: KREC
  INTEGER, INTENT(IN)    :: ZERO          ! initialize output file
  INTEGER, INTENT(IN)    :: NXP,NYP       ! extract grid size


  INTEGER      :: IYR,IMO,IDA,IHR,IMN,IFH,IY0,IM0,ID0,IH0

  COMMON /CDATES/ IYR,IMO,IDA,IHR,IMN,IFH,IY0,IM0,ID0,IH0

  INTERFACE
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

  WRITE(50,*)'Constant field output: ', KONST
  CALL PAKREC(20,QVAR,CVAR,NXP,NYP,(NXP*NYP),KONST,                  &
              IY0,IM0,ID0,IH0,IMN,IFH,1,ZERO)
  KREC=KREC+1

END SUBROUTINE wrtcon

