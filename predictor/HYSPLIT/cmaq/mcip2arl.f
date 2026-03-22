PROGRAM mcip2arl

!-------------------------------------------------------------------------------
! Name:     MCIP2ARL
! Purpose:  Get MCIP output and convert to ARL one-byte packed format.  
! Revised:  21 Nov 2005 Converted from mm5toarl (RRD) 
!           05 Mar 2007 Generic input file suffix  
!-------------------------------------------------------------------------------

  IMPLICIT NONE

  INCLUDE 'PARMS3.EXT'      ! I/O API constants
  INCLUDE 'FDESC3.EXT'      ! I/O API file description data structure
  INCLUDE 'IODECL3.EXT'     ! I/O API function declarations

  CHARACTER(16)             :: INPFILE
  CHARACTER(80)             :: LABEL, OUTFILE, SUFFIX
  CHARACTER(5)              :: BASE(3) = (/'DOT3D','CRO3D','CRO2D'/)
  INTEGER                   :: LOGDEV
  LOGICAL                   :: FTEST

  REAL,         ALLOCATABLE :: dat_dot (:,:)
  REAL,         ALLOCATABLE :: tmp_dot (:,:)
  REAL,         ALLOCATABLE :: dat_crs (:,:)
  REAL,         ALLOCATABLE :: tmp_crs (:,:)
  REAL,         ALLOCATABLE :: sig  (:)       
  CHARACTER(1), ALLOCATABLE :: cvar (:)

! hysplit packed data configuration
  INTEGER, PARAMETER        :: nsfc =  4     ! number of surface variables
  INTEGER, PARAMETER        :: nvar = 10     ! total number of variables

  REAL,    PARAMETER        :: g    = 9.81   ! m/s**2; gravity

! arl packed variables names
  CHARACTER(4)  :: vchar(nvar) = (/ 'PRSS',            'SHTF',              &
                                    'USTR',            'MXHT',              &
                                    'PRES',            'UWND',              &
                                    'VWND',            'WWND',              &
                                    'TEMP',            'SPHU'              /)

! equivalent  mcip variable names
  CHARACTER(16) :: mchar(nvar) = (/ 'PRSFC           ','HFX             ',  &
                                    'USTAR           ','PBL             ',  &
                                    'PRES            ','UWIND           ',  &
                                    'VWIND           ','WWIND           ',  &
                                    'TA              ','QV              '  /)

! base file name index where each variable can be found 
  INTEGER       :: nbase(nvar) = (/  3,                 3,                  &
                                     3,                 3,                  &
                                     2,                 1,                  &
                                     1,                 2,                  &
                                     2,                 2                  /) 

  INTEGER       :: narg,iargc, klen
  INTEGER       :: j,k,n,nx,ny,nz,ntime
  INTEGER       :: iyr,imo,ida,ihr,ifh,imn
  INTEGER       :: jdate,jtime,jstep       
  REAL          :: clat1,clon1,clat2,clon2

!-------------------------------------------------------------------------------
! Configure subroutine interface argumment lists
!-------------------------------------------------------------------------------

  INTERFACE
     SUBROUTINE setgrid (nsfc,nvar,vchar,sig,clat1,clon1,clat2,clon2)
     INTEGER,       INTENT(IN)    :: nsfc, nvar
     CHARACTER(4),  INTENT(IN)    :: vchar ( : )
     REAL,          INTENT(IN)    :: sig   ( : )
     REAL,          INTENT(IN)    :: clat1,clon1,clat2,clon2
     END SUBROUTINE setgrid

    SUBROUTINE crs2dot (varcrs, vardot)
    REAL,           INTENT(IN)    :: varcrs ( : , : )
    REAL,           INTENT(OUT)   :: vardot ( : , : )
    END SUBROUTINE crs2dot
  END INTERFACE

!-------------------------------------------------------------------------------
! Check command line for file name suffix
!-------------------------------------------------------------------------------

! check for command line input/output file names
  NARG=IARGC()
  IF(NARG.LT.2)THEN
     WRITE(*,*)'Usage: mcip2arl [suffix] [outfile]'
     WRITE(*,*)'where {suffix} = input file METxxxxD_{suffix}'
     STOP
  END IF

  DO WHILE(NARG.GT.0)
     CALL GETARG(NARG,LABEL)
     IF(narg.EQ.1) THEN
        KLEN=INDEX(LABEL,' ')-1
        SUFFIX=LABEL(1:KLEN)
     END IF
     IF(narg.EQ.2) OUTFILE=LABEL
     NARG=NARG-1
  END DO

! MCIP input files
  DO N=1,3
     INPFILE='MET'//BASE(N)//'_'//SUFFIX(:KLEN)
     INQUIRE (FILE=INPFILE, EXIST=FTEST)
     IF(.NOT.FTEST) THEN
        WRITE(*,*) 'File not found:',N,' - ',INPFILE
     END IF
  END DO
  IF(.NOT.FTEST) STOP

!-------------------------------------------------------------------------------
! Loop through each MCIP file 
!-------------------------------------------------------------------------------

  LOGDEV  = INIT3()     !  initialization returns unit # for log
  IF(logdev.LT.0) STOP 'ERROR: Cannot open the IOAPI log file'

! allocate output file to reserve unit
  OPEN(10,FILE='MCIP.CFG')

! open and check all input files
  DO N=1,3
     INPFILE='MET'//BASE(N)//'_'//SUFFIX(:KLEN)

     IF(.NOT.OPEN3(INPFILE,FSREAD3,'mcip2arl'))THEN
        FTEST = SHUT3()
        WRITE(*,*) 'ERROR: cannot open the input file - ',INPFILE
        STOP
     END IF

!    load the descriptors 
     IF(.NOT.DESC3(INPFILE))THEN
        FTEST = SHUT3()
        WRITE(*,*) 'ERROR: cannot load descriptors - ',INPFILE
        STOP
     END IF

!    insure we have the right type of data file
     IF(FTYPE3D.NE.GRDDED3)THEN
        FTEST = SHUT3()
        WRITE(*,*) 'ERROR: file type not gridded data - ',FTYPE3D
        STOP
     END IF
  END DO

  CLOSE(10)

! First file (3D dot) allocate variables and configure HYSPLIT meteo file
  INPFILE='MET'//BASE(1)//'_'//SUFFIX(:KLEN)
  IF(.NOT.DESC3(INPFILE))THEN
     STOP 'Unable to get descriptor'

  ELSE
     ntime = MXREC3D
     nx    = NCOLS3D
     ny    = NROWS3D
     nz    = NLAYS3D 

     ALLOCATE (dat_dot (nx,   ny  ))
     ALLOCATE (tmp_dot (nx,   ny  ))
     ALLOCATE (dat_crs (nx-1, ny-1))
     ALLOCATE (tmp_crs (nx-1, ny-1))
     ALLOCATE (sig (nz))       
     ALLOCATE (cvar (nx*ny))

!    grid system corners for geo-reference
     INPFILE='GRIDDOT2D_'//SUFFIX(:KLEN)
     FTEST=OPEN3(INPFILE,FSREAD3,'mcip2arl')
     IF(.NOT.FTEST)THEN
        WRITE(*,*)'Error opening dot grid file: ',INPFILE
        STOP
     END IF
     FTEST=READ3(INPFILE,'LAT             ',1,JDATE,JTIME, dat_dot)
     CLAT1=dat_dot(1,1)
     CLAT2=dat_dot(nx,ny)
     FTEST=READ3(INPFILE,'LON             ',1,JDATE,JTIME, dat_dot)
     CLON1=dat_dot(1,1)
     CLON2=dat_dot(nx,ny)

!    vertical structure defined at half levels
     DO k = 1, nz
        sig(k) = 0.5*(vglvs3d(k+1)+vglvs3d(k))
     END DO

!    Set up ARL format variables and grid definitions
     CALL setgrid (nsfc,nvar,vchar,sig,clat1,clon1,clat2,clon2)
     CALL PAKSET(10,'MCIP.CFG',1,nx,ny,(nz+1))
     OPEN(10,FILE=OUTFILE,RECL=(50+nx*ny),ACCESS='DIRECT',FORM='UNFORMATTED')

!    Get the file starting date/time
     JDATE = SDATE3D       
     JTIME = STIME3D
     JSTEP = TSTEP3D 

!    Convert times to integer from character string
     ihr=JTIME/10000
     imn=JTIME-ihr*10000 
     iyr=MOD(JDATE/1000,100)
     CALL DAYMON(JDATE,imo,ida)
     ifh=0                        ! forecast hour always zero
  END IF 

!-------------------------------------------------------------------------------
! Loop over times 
!-------------------------------------------------------------------------------

  DO j = 1, ntime

!    process surface fields
     INPFILE='MET'//BASE(3)//'_'//SUFFIX(:KLEN)
     DO n = 1, nsfc
        FTEST=READ3(INPFILE,MCHAR(n),1,JDATE,JTIME, dat_crs)
        IF(.NOT.FTEST) STOP 'Unable to read surface file'

!       convert pressure from Pa to hPa
        IF(MCHAR(n)(1:5).EQ.'PRSFC') dat_crs = dat_crs /100.0

        CALL CRS2DOT(dat_crs, dat_dot)
!       dat_dot = TRANSPOSE (tmp_dot) 
        CALL PAKREC(10,dat_dot,cvar,nx,ny,(nx*ny),vchar(n),iyr,imo,ida,ihr,imn,ifh,1,0)  
     END DO

!    process all the cross point upper air fields                                 
     INPFILE='MET'//BASE(2)//'_'//SUFFIX(:KLEN)
     cloop : DO n = nsfc+1, nvar 
        IF(nbase(n).NE.2) CYCLE cloop
        DO k = 1, nz 
           FTEST=READ3(INPFILE,MCHAR(n),K,JDATE,JTIME, dat_crs)
           IF(.NOT.FTEST) STOP 'Unable to read cross file'

!          convert vertical velocity from m/s to hPa/s using omega = -W g rho
           IF(MCHAR(n)(1:5).EQ.'WWIND')THEN
              FTEST=READ3(INPFILE,'DENS            ',K,JDATE,JTIME, tmp_crs)
              IF(.NOT.FTEST) STOP 'Unable to read cross file'
              dat_crs = -dat_crs * g * tmp_crs /100.0 
           END IF

!          convert pressure from Pa to hPa
           IF(MCHAR(n)(1:4).EQ.'PRES') dat_crs = dat_crs /100.0

           CALL CRS2DOT(dat_crs, dat_dot)
!          dat_dot = TRANSPOSE (tmp_dot) 
           CALL PAKREC(10,dat_dot,cvar,nx,ny,(nx*ny),vchar(n),iyr,imo,ida,ihr,imn,ifh,k+1,0)  
        END DO
     END DO cloop

!    process all the dot point upper air fields                                 
     INPFILE='MET'//BASE(1)//'_'//SUFFIX(:KLEN)
     dloop : DO n = nsfc+1, nvar 
        IF(nbase(n).NE.1) CYCLE dloop
        DO k = 1, nz 
           FTEST=READ3(INPFILE,MCHAR(n),K,JDATE,JTIME, dat_dot)
           IF(.NOT.FTEST) STOP 'Unable to read dot file'
!          dat_dot = TRANSPOSE (tmp_dot) 
           CALL PAKREC(10,dat_dot,cvar,nx,ny,(nx*ny),vchar(n),iyr,imo,ida,ihr,imn,ifh,k+1,0)  
        END DO
     END DO dloop
     WRITE(*,*)'Finished: ',iyr,imo,ida,ihr

!    Close out time period by writing index record                      
     CALL PAKNDX(10)     
     CALL NEXTIME(JDATE,JTIME,JSTEP)

!    Convert times to integer from character string
     ihr=JTIME/10000
     imn=JTIME-ihr*10000 
     iyr=MOD(JDATE/1000,100)
     CALL DAYMON(JDATE,imo,ida)
  END DO 

!-------------------------------------------------------------------------------
! Deallocate variables.
!-------------------------------------------------------------------------------

  FTEST = SHUT3()
  IF(.NOT.FTEST) WRITE(*,*)'ERROR: shutdown of IOAPI'


END PROGRAM mcip2arl

!##############################################################################
!##############################################################################
!##############################################################################

SUBROUTINE setgrid (nsfc,nvar,vchar,sig,clat1,clon1,clat2,clon2)

!-------------------------------------------------------------------------------
! Name:     Setgrid      
! Purpose:  Defines mapping of latitude-longitude to grid units and initializes
!           ARL packing subroutines 
! Revised:  22 Nov 2005 
!-------------------------------------------------------------------------------

  IMPLICIT NONE

  INCLUDE 'PARMS3.EXT'          ! I/O API constants
  INCLUDE 'FDESC3.EXT'          ! I/O API file description data structure
  INCLUDE 'IODECL3.EXT'         ! I/O API function declarations

  INTEGER,       INTENT(IN)    :: nsfc, nvar 
  CHARACTER(4),  INTENT(IN)    :: vchar ( : )
  REAL,          INTENT(IN)    :: sig   ( : )
  REAL,          INTENT(IN)    :: clat1,clon1,clat2,clon2

  INTEGER                      :: k,n
  REAL                         :: parmap(9)
  REAL                         :: grids (12)
  REAL                         :: tru1, tru2   

  REAL,          EXTERNAL      :: eqvlat
  
!-------------------------------------------------------------------------------
! Create array that will be written to initialize ARL data packing
!-------------------------------------------------------------------------------

  IF( GDTYP3D == LAMGRD3 )THEN      ! lambert conformal

     tru1      = P_ALP3D            ! convert to real(8) 
     tru2      = P_BET3D  
     grids(3)  = eqvlat(tru1,tru2)  ! lat of grid spacing 

     grids(1)  = grids(3)           ! pole position
     grids(2)  = P_GAM3D            ! 180 from cut longitude

     CALL stlmbr(parmap,eqvlat(tru1,tru2),grids(2))
     CALL stcm2p(parmap,1.,1.,clat1,clon1,FLOAT(NCOLS3D),FLOAT(NROWS3D),clat2,clon2)

     grids(4)  = P_GAM3D            ! lon of grid spacing
     grids(5)  = YCELL3D/1000.0     ! grid size
     grids(6)  = 0.0                ! orientation
     grids(7)  = grids(3)           ! cone angle                         
     grids(8)  = (NCOLS3D+1)/2.0 
     grids(9)  = (NROWS3D+1)/2.0

!    grids(10) = YCENT3D
!    grids(11) = XCENT3D
!    use cmapf routines to determine grid center (mcip seems to be wrong)
     CALL cxy2ll(parmap,grids(8),grids(9),grids(10),grids(11))

  ELSEIF( GDTYP3D == POLGRD3 )THEN  ! polar sterographic

     grids(1)  = 90.0*P_ALP3D       ! pole position
     grids(2)  = 0.0                ! 180 from cut longitude
     grids(3)  = P_BET3D            ! lat of grid spacing 
     grids(4)  = P_GAM3D            ! lon of grid spacing
     grids(5)  =  YCELL3D/1000.0    ! grid size
     grids(6)  = 0.0                ! orientation
     grids(7)  = 90.0               ! cone angle
     grids(8)  = (NCOLS3D+1)/2.0 
     grids(9)  = (NROWS3D+1)/2.0
     grids(10) = YCENT3D
     grids(11) = XCENT3D

  ELSEIF( GDTYP3D == MERGRD3 )THEN  ! mercator projection 

     grids(1)  = 90.0               ! pole position
     grids(2)  =P_BET3D             ! 180 from cut longitude
     grids(3)  = P_ALP3D            ! lat of grid spacing 
     grids(4)  =P_BET3D             ! lon of grid spacing
     grids(5)  = YCELL3D/1000.0     ! grid size
     grids(6)  = 0.0                ! orientation
     grids(7)  = 90.0-P_GAM3D       ! cone angle
     grids(8)  = (NCOLS3D+1)/2.0 
     grids(9)  = (NROWS3D+1)/2.0
     grids(10) = YCENT3D
     grids(11) = XCENT3D

  ELSE
     WRITE(*,*)'MCIP grid not defined in program: ',GDTYP3D
     STOP

  END IF 
  grids(12) = 0.0                  ! unused

!-------------------------------------------------------------------------------
! Write ARL data packing configuration file                      
!-------------------------------------------------------------------------------

  OPEN(10,FILE='MCIP.CFG')      
  WRITE(10,'(20X,a4)') 'MCIP'                       ! abbreviated data set name
  WRITE(10,'(20X,i4)') 99,1                         ! grid & coordinate system
  WRITE(10,'(20X,f10.2)') grids                     ! grid orientation array
  WRITE(10,'(20X,i4)') ncols3d, nrows3d, (nlays3d+1)                       
 
! surface level information
  WRITE(10,'(20x,f6.0,i3,20(1x,a4))') 1.0, nsfc, (vchar(n),n=1,nsfc)

! upper level information
  DO k = 1, nlays3d
     WRITE(10,'(20x,f6.4,i3,20(1x,a4))') sig(k), (nvar-nsfc), (vchar(n),n=nsfc+1,nvar)
  END DO

  CLOSE (10)
  RETURN

END SUBROUTINE setgrid  

!##############################################################################
!##############################################################################
!##############################################################################

SUBROUTINE crs2dot (varcrs, vardot)

!-------------------------------------------------------------------------------
! Name:     Cross to Dot 
! Purpose:  Interpolates in horizontal from cross to dot points.
! Notes:    Based on PSU/NCAR model routine.
! Revised:  20 Apr 1999  Original version.  (TLO)
!           29 Oct 1999  Converted to free-form f90.  (TLO)
!           23 Nov 2005  Assume grid dimensions different by one for dot & crs
!-------------------------------------------------------------------------------

  IMPLICIT NONE

  REAL,            INTENT(IN)  :: varcrs ( : , : )
  REAL,            INTENT(OUT) :: vardot ( : , : )

  INTEGER                      :: ix, jx, ie, je, i, j

!-------------------------------------------------------------------------------
! Extract domain dimensions (vardot is always one larger in x and y)
!-------------------------------------------------------------------------------

  ix = SIZE(varcrs,1)
  jx = SIZE(varcrs,2)

  ie = ix - 1
  je = jx - 1

!-------------------------------------------------------------------------------
! For interior of grid, interpolate cross point values to dot points using
! four-point interpolation.
!-------------------------------------------------------------------------------

  DO j = 2, jx
    DO i = 2, ix
      vardot(i,j) = 0.25 * ( varcrs(i,j)   + varcrs(i-1,j)  &
                           + varcrs(i,j-1) + varcrs(i-1,j-1) )
    END DO
  END DO

!-------------------------------------------------------------------------------
! For outermost rows and columns, interpolate cross point values to dot points
! using two-point interpolation.  In row and column 1, there are no cross points
! below or to the left of the dow row that is being interpolated.  In row JX
! and column IX, cross points are not defined.
!-------------------------------------------------------------------------------

  DO i = 2, ix
    vardot(i,1)  = 0.5 * ( varcrs(i,1)  + varcrs(i-1,1)  )
    vardot(i,jx) = 0.5 * ( varcrs(i,jx) + varcrs(i-1,jx) )
  END DO

  DO j = 2, jx
    vardot(1, j) = 0.5 * ( varcrs(1, j) + varcrs(1, j-1) )
    vardot(ix,j) = 0.5 * ( varcrs(ix,j) + varcrs(ix,j-1) )
  END DO

!-------------------------------------------------------------------------------
! Define dot point corners and persist last row and column.
!-------------------------------------------------------------------------------

  vardot(1,   1   ) = varcrs(1, 1)
  vardot(ix+1,jx+1) = varcrs(ie,je)

  vardot(ix+1,1:jx) = varcrs(ix,:)
  vardot(1:ix,jx+1) = varcrs(:,jx)

  RETURN

END SUBROUTINE crs2dot

!##############################################################################
!##############################################################################
!##############################################################################

SUBROUTINE convert  ( kx, pres, ta, wa, qva)

!-------------------------------------------------------------------------------
! Name:     Convert      
! Purpose:  Compute surface and pressure profile from perturbation pressure and
!           converts vertical velocity (m/s) to omega units (hPa/s).
! Notes:    Based upon equations in TLO's sfclayer subroutine 
! Revised:  14 Aug 2000  Original version.  (RRD)
!           05 Sep 2000  Vertical velocity offset by one index. (RRD)
!           03 Sep 2002  Pressure computation correction (KAG)
!-------------------------------------------------------------------------------

  IMPLICIT NONE

  INTEGER,       INTENT(IN)    :: kx
  REAL,          INTENT(OUT)   :: pres       ( : , : , : )
  REAL,          INTENT(IN)    :: ta         ( : , : , : )
  REAL,          INTENT(INOUT) :: wa         ( : , : , : )
  REAL,          INTENT(IN)    :: qva        ( : , : , : )

  INTEGER                      :: k
  REAL,    PARAMETER :: g      =     9.81   ! m/s**2; gravity
  REAL,    PARAMETER :: r      =   287.04   ! J/kg/K; gas constant

  DO k=1,kx

!    convert vertical velocity from m/s to hPa/s using omega = -W g rho
     wa(:,:,k) = -wa(:,:,k+1) * g * pres(:,:,k) /  &
                 (r * ta(:,:,k) * ( 1.0 + 0.6077 * qva(:,:,k) ) )
  END DO

  RETURN

END SUBROUTINE convert 
