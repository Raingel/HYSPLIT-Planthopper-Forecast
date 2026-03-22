PROGRAM arw2arl

!---------------------------------------------------------------------------
! ADVANCED RESEARCH WRF TO ARL FORMAT
!---------------------------------------------------------------------------
! Last revised: 14 Apr 2006 (RRD) - Initial version
!               18 Aug 2006 (RRD) - revised command line prompts
!               25 Aug 2006 (RRD) - added time dimension to data arrays
!               13 Aug 2007 (RRD) - limit vertical levels
!               24 Sep 2008 (RRD) - handles multi-time period files
!               12 Jan 2010 (RRD) - added: tken,shtf,lhtf,ustr,rghs,dswf
!               21 Jan 2010 (RRD) - converted precip to hourly totals
!               03 Mar 2010 (RRD) - define variables through namelist
!               01 Jun 2011 (RRD) - fixed error processing user 3D variables
!               25 Feb 2013 (AFS) - added cfg files for wind flux and tke
!               12 Sep 2013 (RRD) - increased dimension of character strings
!               07 Apr 2014 (RRD) - treat TKE as a flux variable (nzp+1)
!               11 Apr 2014 (RRD) - correct precip totals for non-hourly
!               15 May 2014 (RRD) - enhanced packing usind difference field
!               17 Mar 2015 (RRD) - test for difference field
!               08 Mar 2018 (CPL) - fixed bug processing muliple user 3D vars
!               15 Mar 2018 (CPL) - created diff variable for TKE; output lower
!                                   interfaces of bottom_top_stag variables;
!                                   minimum TKE set to 0.1 
!               17 Apr 2018 (CPL) - remove minimum TKE
!               29 May 2018 (CPL) - account for TKE on either staggered or
!                                   unstaggered grid
!               08 Jun 2018 (CPL) - increase number of 3D variables output and
!                                   allow for DIF variable of AVGFLX_WWM
!               03 Aug 2018 (FN)  - Add DIFT for QKE 
!                                   Correct misplace of DIFT output section
!               24 Sep 2018 (CPL) - allow for potential temperature to be
!                                   output; move DIFT write; move DIFT and DIFW
!                                   if statements
!               28 Jul 2020 (CPL) - hybrid WRF vertical coordinates; adapted
!                                   from Thomas Nehrkorn's AER version of STILT
!               07 Aug 2020 (CPL) - update for WRF hybrid vertical coordinates
!               14 Oct 2021 (SYZ) - removed unused variables
!               01 Nov 2021 (CPL) - update for soil moisture and soil liquid 
!                                   water output
!               11 Sep 2023 (SYZ) - the -b option can take up to 4 digits
!---------------------------------------------------------------------------

  IMPLICIT NONE

  include 'netcdf.inc'

  REAL          :: delt,xini,xdel
  REAL          :: range(2)
  CHARACTER(256):: netcdf_file, varcfg_file, precip_file 
  CHARACTER(256):: label, arlcfg_file, arlbin_file
  CHARACTER(80) :: title
  LOGICAL       :: ftest, rainc, rainnc, diag
  LOGICAL       :: difr, difw, dift
  LOGICAL       :: skip
  LOGICAL       :: thetflg
  INTEGER       :: k, l, n, nt1, nt2, kcfg
  INTEGER       :: n3d, ndim, varid, narg, iargc, clen, mzp, maxl
  INTEGER       :: ntp, krec, nxp, nyp, nzp, nzs, nxy, kret, kinit
  INTEGER       :: dimlen(nf_max_var_dims)
  INTEGER       :: yy,mm,dd,hh,mn,fh
  INTEGER       :: hyb_opt
  INTEGER       :: vert_coord

  INTEGER       :: ncid        ! unit assigned for NetCDF I/O
  INTEGER       :: kunit = 90  ! unit number for ARL formatted files
  INTEGER       :: lunit = 80  ! accumulated precipitation file
  INTEGER       :: hunit = 70  ! hybrid WRF vertical coordinates file

  INTEGER       :: npar = 8    ! number of hybrid vertical coordinate variables

  REAL,  ALLOCATABLE :: p_top(:)           ! pressure top

! hybrid vertical coordinate variables
  REAL,  ALLOCATABLE :: c1h(:)
  REAL,  ALLOCATABLE :: c2h(:)
  REAL,  ALLOCATABLE :: c3h(:)
  REAL,  ALLOCATABLE :: c4h(:)
  REAL,  ALLOCATABLE :: c1f(:)
  REAL,  ALLOCATABLE :: c2f(:)
  REAL,  ALLOCATABLE :: c3f(:)
  REAL,  ALLOCATABLE :: c4f(:)

! temporary hybrid vertical coordinate variables
  REAL,  ALLOCATABLE :: hybverth(:,:)
  REAL,  ALLOCATABLE :: hybvertf(:,:)

  REAL,  ALLOCATABLE :: level(:,:)         ! vertical grid
  REAL,  ALLOCATABLE :: var2d(:,:)         ! generic 2D variable     

  REAL,  ALLOCATABLE :: tpct (:,:)         ! current total precip 
  REAL,  ALLOCATABLE :: tpcc (:,:)         ! current convective precip 
  REAL,  ALLOCATABLE :: tpcg (:,:)         ! current grid precip 
  REAL,  ALLOCATABLE :: tppt (:,:)         ! previous total precip
  REAL,  ALLOCATABLE :: tppc (:,:)         ! previous convective precip
  REAL,  ALLOCATABLE :: tppg (:,:)         ! previous grid precip

  REAL,  ALLOCATABLE :: var3ds(:,:,:)      ! soil 3D variable

  REAL,  ALLOCATABLE :: var3d(:,:,:)       ! generic 3D variable 
  REAL,  ALLOCATABLE :: uwnd (:,:,:)       ! west to east wind     
  REAL,  ALLOCATABLE :: vwnd (:,:,:)       ! south to north wind   
  REAL,  ALLOCATABLE :: wwnd (:,:,:)       ! vertical wind  
  REAL,  ALLOCATABLE :: temp (:,:,:)       ! ambient temperatue     
  REAL,  ALLOCATABLE :: pres (:,:,:)       ! pressure  
  REAL,  ALLOCATABLE :: sphu (:,:,:)       ! specific humidity  
  REAL,  ALLOCATABLE :: pert (:,:,:)       ! perturbation from base pressure

  CHARACTER(1), ALLOCATABLE :: cvar(:)     ! packed output array
  CHARACTER(19),ALLOCATABLE :: tvar(:)     ! date time string for records
  REAL,         ALLOCATABLE :: xmin(:)     ! minutes since simulation start
  REAL,         ALLOCATABLE :: base(:,:)   ! temporary 2D write variable   
  REAL,         ALLOCATABLE :: diff(:,:)   ! temporary difference  variable   

  REAL,    PARAMETER :: cp     =  1004.0   ! J/kg/K; specific heat
  REAL,    PARAMETER :: grav   =     9.81  ! m/s**2; gravity
  REAL,    PARAMETER :: rdry   =   287.04  ! J/kg/K; gas constant
  REAL,    PARAMETER :: rovcp  = rdry / cp ! constant for poisson's equation

  INTEGER, PARAMETER :: max2dv = 30        ! max number of 2D variables
  INTEGER, PARAMETER :: max3dv = 20        ! max number of 3D variables
  INTEGER            :: num2dv, num3dv     ! actual number of variables
  REAL               :: cnv2dv(max2dv)     ! conversion factor ARW->ARL
  REAL               :: cnv3dv(max3dv)
  CHARACTER(10)      :: arw2dv(max2dv)     ! ARW variable names
  CHARACTER(10)      :: arw3dv(max3dv)
  CHARACTER(4)       :: arl2dv(max2dv)     ! ARL variable names
  CHARACTER(4)       :: arl3dv(max3dv)

  REAL    :: PREC,VAR1
  INTEGER :: NEXP,KSUM

  NAMELIST/SETUP/ num3dv,arw3dv,cnv3dv,arl3dv,num2dv,arw2dv,cnv2dv,arl2dv

! pass packing precision information (15 May 2014)
  COMMON / PAKVAL / PREC,NEXP,VAR1,KSUM

!------------------------------------------------------------------------------
! subroutine interfaces

  INTERFACE   

  SUBROUTINE getdim (diag,ncid,ntp,nxp,nyp,nzp,nzs,clen)
  IMPLICIT NONE
  LOGICAL,       INTENT(IN)  :: diag
  INTEGER,       INTENT(IN)  :: ncid
  INTEGER,       INTENT(OUT) :: ntp
  INTEGER,       INTENT(OUT) :: nxp
  INTEGER,       INTENT(OUT) :: nyp
  INTEGER,       INTENT(OUT) :: nzp
  INTEGER,       INTENT(OUT) :: nzs
  INTEGER,       INTENT(OUT) :: clen
  END SUBROUTINE getdim

  SUBROUTINE setvar (diag,ncid,label,varid,n3d,ndim,dimlen,range)
  IMPLICIT NONE
  include 'netcdf.inc'
  LOGICAL,       INTENT(IN)  :: diag
  INTEGER,       INTENT(IN)  :: ncid
  CHARACTER(80), INTENT(IN)  :: label
  INTEGER,       INTENT(OUT) :: varid
  INTEGER,       INTENT(OUT) :: n3d
  INTEGER,       INTENT(OUT) :: ndim
  INTEGER,       INTENT(OUT) :: dimlen(nf_max_var_dims)
  REAL,          INTENT(OUT) :: range(2)
  END SUBROUTINE setvar

  SUBROUTINE setmap (diag,ncid,nxp,nyp,p_top)
  IMPLICIT NONE
  LOGICAL,       INTENT(IN)  :: diag
  INTEGER,       INTENT(IN)  :: ncid
  INTEGER,       INTENT(IN)  :: nxp,nyp
  REAL,          INTENT(IN)  :: p_top
  END SUBROUTINE setmap

  SUBROUTINE cfgrec (kunit,arlcfg_file,nxp,nyp,nzp,level,num2dv,arl2dv,num3dv,arl3dv,vert_coord)
  IMPLICIT NONE
  INTEGER,      INTENT(IN)   :: kunit
  CHARACTER(80),INTENT(IN)   :: arlcfg_file
  INTEGER,      INTENT(IN)   :: nxp,nyp,nzp
  REAL,         INTENT(IN)   :: level(:)
  INTEGER,      INTENT(IN)   :: num2dv
  CHARACTER(4), INTENT(IN)   :: arl2dv(:)
  INTEGER,      INTENT(IN)   :: num3dv 
  CHARACTER(4), INTENT(IN)   :: arl3dv(:)
  INTEGER,      INTENT(IN)   :: vert_coord
  END SUBROUTINE cfgrec

  SUBROUTINE get1dv (diag,ncid,varid,ntp,tvar)
  IMPLICIT NONE
  LOGICAL,       INTENT(IN)  :: diag
  INTEGER,       INTENT(IN)  :: ncid
  INTEGER,       INTENT(IN)  :: varid
  INTEGER,       INTENT(IN)  :: ntp
  CHARACTER(19), INTENT(OUT) :: tvar(:)
  END SUBROUTINE get1dv

  SUBROUTINE get2dv (diag,ncid,varid,ntp,rval)
  IMPLICIT NONE
  LOGICAL,       INTENT(IN)  :: diag
  INTEGER,       INTENT(IN)  :: ncid
  INTEGER,       INTENT(IN)  :: varid
  INTEGER,       INTENT(IN)  :: ntp
  REAL,          INTENT(OUT) :: rval(:)
  END SUBROUTINE get2dv

  SUBROUTINE get3dv (diag,ncid,varid,nx,ny,nz,nt,rval)
  IMPLICIT NONE
  LOGICAL,       INTENT(IN)  :: diag
  INTEGER,       INTENT(IN)  :: ncid
  INTEGER,       INTENT(IN)  :: varid
  INTEGER,       INTENT(IN)  :: nx,ny,nz,nt
  REAL,          INTENT(OUT) :: rval(:,:)
  END SUBROUTINE get3dv

  SUBROUTINE get4dv (diag,ncid,varid,nx,ny,nz,nt,rval)
  IMPLICIT NONE
  LOGICAL,       INTENT(IN)  :: diag
  INTEGER,       INTENT(IN)  :: ncid
  INTEGER,       INTENT(IN)  :: varid
  INTEGER,       INTENT(IN)  :: nx,ny,nz,nt
  REAL,          INTENT(OUT) :: rval(:,:,:)
  END SUBROUTINE get4dv

  SUBROUTINE PAKINP(RVAR,CVAR,NX,NY,NX1,NY1,LX,LY,PREC,NEXP,VAR1,KSUM)
  REAL,          INTENT(OUT)   :: rvar (:,:)  
  CHARACTER(1),  INTENT(IN)    :: cvar (:)  
  INTEGER,       INTENT(IN)    :: nx,ny     
  INTEGER,       INTENT(IN)    :: nx1,ny1   
  INTEGER,       INTENT(IN)    :: lx,ly   
  REAL,          INTENT(IN)    :: prec    
  INTEGER,       INTENT(IN)    :: nexp   
  REAL,          INTENT(IN)    :: var1  
  INTEGER,       INTENT(INOUT) :: ksum  
  END SUBROUTINE pakinp

  END INTERFACE

!------------------------------------------------------------------------------

  thetflg = .false. ! initialize thetflg

! define the command line options with default values

  NT1=1           ! index of starting time period
  NT2=9999        ! index of ending time period
  KINIT = 1       ! output file initialization 
  MAXL = 50       ! maximum number of levels
  DIAG = .false.  ! diagnostic output
  KCFG = 1        ! default configuration file for instantaneous wind

  NETCDF_FILE = 'WRFOUT.NC1'   ! NetCDF WRF-ARW input
  VARCFG_FILE = 'WRFDATA.CFG'  ! Define WRF and ARL variables
  PRECIP_FILE = 'WRFRAIN.BIN'  ! Save previous precipitation totals
  ARLCFG_FILE = 'ARLDATA.CFG'  ! ARL packing configuration file
  ARLBIN_FILE = 'ARLDATA.BIN'  ! ARL formatted output file

  NARG=IARGC()
  IF(NARG.EQ.0)THEN
     WRITE(*,*)'USAGE-1: arw2arl [netcdf data file name]'
     WRITE(*,*)' '
     WRITE(*,*)'USAGE-2: arw2arl -[options (default)]'
     WRITE(*,*)'  -b[beginning time period index (1)]'
     WRITE(*,*)'  -e[ending time period index (9999)]'
     WRITE(*,*)'  -t[time interval in minutes between outputs (60.0)]'
     WRITE(*,*)'  -s[create WRF variable namelist file for instantaneous winds]'
     WRITE(*,*)'  -a[create WRF variable namelist file for average fluxes]'
     WRITE(*,*)'  -k[create WRF variable namelist file for tke fields]'
     WRITE(*,*)'  -c[create and run with WRF variable namelist file (1)=inst, 2=avrg flux, 3=tke]'
     WRITE(*,*)'  -d[diagnostic output turned on]'
     WRITE(*,*)'  -i[input netcdf data file name (WRFOUT.NC)]'
     WRITE(*,*)'  -o[output ARL packed file name (ARLDATA.BIN)]'
     WRITE(*,*)'  -p[packing configuration file name (ARLDATA.CFG)]'
     WRITE(*,*)'  -v[variable namelist file name (WRFDATA.CFG)]'
     WRITE(*,*)'  -n[number of levels to extract from sfc (50)]'
     WRITE(*,*)'  -z[zero initialization each time 0=no (1)=yes]'
     WRITE(*,*)' '
     WRITE(*,*)'arw2arl - Advanced Research WRF to ARL format converts ARW'
     WRITE(*,*)'NetCDF files to a HYSPLIT compatible format. When the input'
     WRITE(*,*)'file contains data for a single time period, then the ARL format'
     WRITE(*,*)'output file from each execution should be appended (cat >>) to'
     WRITE(*,*)'the output file from the previous time periods execution. The'
     WRITE(*,*)'WRFDATA.CFG namelist file can be manually edited to select other'
     WRITE(*,*)'variables to output. X variables are not found in the WRF output'
     WRITE(*,*)'but are created by this program. All variables require a units'
     WRITE(*,*)'conversion factor be defined in the namelist file.'
     STOP
  END IF

  ! go through each argument
  DO WHILE (NARG.GT.0)

     CALL GETARG(NARG,LABEL)
     SELECT CASE (LABEL(1:2))

     ! beginning time period to extract
     CASE ('-b','-B')
        READ(LABEL(3:),'(I4)')NT1  

     ! end time period to extract   
     CASE ('-e','-E')
        READ(LABEL(3:),'(I4)')NT2  

     ! create WRF instantaneous configuration file
     CASE ('-s','-S')
        CALL cfgvar(varcfg_file,1)
        STOP

     ! create WRF average wind configureation file    
     CASE ('-a','-A')
        CALL cfgvar(varcfg_file,2) 
        STOP

     ! create WRF average wind configureation file    
     CASE ('-k','-K')
        CALL cfgvar(varcfg_file,3) 
        STOP       

     ! create WRF average wind configureation file    
     CASE ('-c','-C')
         READ(LABEL(3:),'(I1)')KCFG 

     ! turn on diagnostic output
     CASE ('-d','-D')
        diag=.TRUE.

     ! netcdf input data file name
     CASE ('-i','-I')
        CLEN=LEN_TRIM(LABEL)
        NETCDF_FILE=ADJUSTL(LABEL(3:CLEN))

     ! ARL format packed output file name
     CASE ('-o','-O')
        CLEN=LEN_TRIM(LABEL)
        ARLBIN_FILE=ADJUSTL(LABEL(3:CLEN))
        
     ! arl packing configuration file name
     CASE ('-p','-P')
        CLEN=LEN_TRIM(LABEL)
        ARLCFG_FILE=ADJUSTL(LABEL(3:CLEN))

     ! variable list file name
     CASE ('-v','-V')
        CLEN=LEN_TRIM(LABEL)
        VARCFG_FILE=ADJUSTL(LABEL(3:CLEN))

     ! maximum number of levels to extract
     CASE ('-n','-N')
        READ(LABEL(3:),'(I4)')MAXL 

     ! zero initialization each time period
     CASE ('-z','-Z')
        READ(LABEL(3:),'(I1)')KINIT

     ! simple command line argument with only file name
     CASE DEFAULT
        CLEN=LEN_TRIM(LABEL)
        NETCDF_FILE=ADJUSTL(LABEL(1:CLEN))

     END SELECT
     NARG=NARG-1

  END DO

!------------------------------------------------------------------------------
! Open and read the namelist that defines the WRF-ARW variables that will be
! extracted and converted to the HYSPLIT compatible ARL format. If it does not
! exist, the namelist will be created. Existing namelists may be user edited to 
! delete or define new variables.

  INQUIRE(FILE=TRIM(varcfg_file), EXIST=ftest)
  IF(.NOT.ftest)THEN
     CALL cfgvar(varcfg_file,kcfg)
  ELSE
     WRITE(*,*)'Using an existing decoding configuration: ',TRIM(varcfg_file)
  END IF

  OPEN (10,FILE=TRIM(varcfg_file))
  READ (10,SETUP)
  CLOSE(10)

  IF(num2dv.GT.max2dv)THEN
     WRITE(*,*)'Number of 2D variables',num2dv,' exceeding compiled limit',max2dv
     STOP
  END IF

  IF(num3dv.GT.max3dv)THEN
     WRITE(*,*)'Number of 3D variables',num3dv,' exceeding compiled limit',max3dv
     STOP
  END IF

!------------------------------------------------------------------------------
! open the NetCDF input file and obtain basic information to design output

  kret = NF_OPEN(netcdf_file, nf_nowrite, ncid)
  n=INDEX(netcdf_file,' ')-1
  IF(kret.NE.nf_noerr) THEN
     WRITE(*,*) netcdf_file(:n),' : ',NF_STRERROR(kret)
     STOP
  END IF
  IF(diag)WRITE(*,*)'Opened NetCDF input file on unit: ',ncid

  ! get dimensions for grid definitions
  CALL getdim (diag,ncid,ntp,nxp,nyp,nzp,nzs,clen)

  IF(diag.AND.ntp.GT.1)WRITE(*,*)'NetCDF file contains ',ntp,' time periods'
  NT1=MIN(NT1,NTP)
  NT2=MIN(NT2,NTP)

  ! limit the number of levels
  MZP=MIN(NZP,MAXL)

  ! determine the number of time periods within the file
  label='Times'
  CALL setvar(diag,ncid,label,varid,n3d,ndim,dimlen,range)
  IF(clen.EQ.19)THEN
     ALLOCATE (tvar(ntp),STAT=kret)
     IF(kret.NE.0)THEN
        WRITE(*,*)'Allocation error in time-1 array'
        STOP
     END IF
  ELSE
     WRITE(*,*)'Date String Length NE 19: ',clen
     STOP
  END IF
  CALL get1dv (diag,ncid,varid,ntp,tvar)

  ! get the valid time for each period 
  label='XTIME'
  CALL setvar(diag,ncid,label,varid,n3d,ndim,dimlen,range)
  ALLOCATE (xmin(ntp),STAT=kret)
  IF(kret.NE.0)THEN
     WRITE(*,*)'Allocation error in time-2 array'
     STOP
  END IF
  CALL get2dv (diag,ncid,varid,ntp,xmin)

  ! determine time interval between output periods
  ! values remain at zero if file only contains one time period
  DELT=0.0      
  DO n=1,ntp
     IF(diag) WRITE(*,*)xmin(n),' ',tvar(n)
     IF(n.EQ.1)THEN
        CONTINUE
     ELSEIF(n.EQ.2)THEN
        DELT=XMIN(2)-XMIN(1)
     ELSE
        IF(XMIN(n)-XMIN(n-1).NE.DELT)THEN
           WRITE(*,*)'Output time interval not constant'
           WRITE(*,*)'Initial delta (min): ',DELT
           WRITE(*,*)'Delta at period: ',(XMIN(n)-XMIN(n-1)),n
           STOP
        END IF
     END IF
  END DO
  IF(diag)WRITE(*,*)'Output time interval in minutes: ', DELT

  ! extract p_top
  label='P_TOP'
  CALL setvar(diag,ncid,label,varid,n3d,ndim,dimlen,range)
  ALLOCATE (p_top(ntp),STAT=kret)
  IF(kret.NE.0)THEN
     WRITE(*,*)'Allocation error in p_top array'
     STOP
  END IF

  kret = NF_GET_VAR_REAL(ncid, varid, p_top)
  IF(kret.NE.nf_noerr) THEN
     WRITE(*,*) NF_STRERROR(kret)
     STOP
  ELSEIF(diag)THEN
     WRITE(*,*) p_top(1)
  END IF

  ! set the global attributes for map projection 
  CALL setmap (diag,ncid,nxp,nyp,p_top(1))

  ! extract the level information based upon u-wind sigma levels
  label='ZNU'
  CALL setvar(diag,ncid,label,varid,n3d,ndim,dimlen,range)
  ALLOCATE (level(nzp,ntp),STAT=kret)
  IF(kret.NE.0)THEN
     WRITE(*,*)'Allocation error in level array'
     STOP
  END IF

  kret = NF_GET_VAR_REAL(ncid, varid, level)
  IF(kret.NE.nf_noerr) THEN
     WRITE(*,*) NF_STRERROR(kret)
     STOP
  ELSEIF(diag)THEN
     DO n=nzp,1,-1
        WRITE(*,*) n, level(n,1)
     END DO
  END IF

  FTEST=.FALSE.
  DIFR =.FALSE.
  ! check to insure all defined 2D variables exist in NetCDF file
  DO n=1,num2dv
     IF(TRIM(arw2dv(n)).EQ.'RAIN') CYCLE  
     IF(TRIM(arw2dv(n)).EQ.'X') THEN
!       rain difference variable created internally, does not exist
!       within NetCDF output file
        DIFR=.TRUE.
        CYCLE  
     END IF
     label=TRIM(arw2dv(n))
     CALL setvar(diag,ncid,label,varid,n3d,ndim,dimlen,range)
     IF(varid.EQ.0) THEN
        WRITE(*,*)'Variable not found: ',TRIM(label)
        FTEST=.TRUE.
     END IF
     IF(diag.AND.nxp*nyp.NE.dimlen(1)*dimlen(2))THEN
        WRITE(*,*)'Dimemsion error   :',TRIM(label)
        WRITE(*,*)'Dimensioned (x,y) :',nxp,nyp
        WRITE(*,*)'Variable (x,y)    :',dimlen(1),dimlen(2)
     END IF
  END DO  

  ! check to insure all defined 3D variables exist in NetCDF file
  DIFW=.FALSE.
  DIFT=.FALSE.
  DO n=1,num3dv
     IF(TRIM(arw3dv(n)).EQ.'X') THEN
!       difference variables created internally, does not exist
!       within NetCDF output file
        IF((arw3dv(n-1).EQ.'W').OR.(arw3dv(n-1).EQ.'AVGFLX_WWM')) THEN
           DIFW=.TRUE.
           CYCLE  
        ELSE IF((TRIM(arw3dv(n-1)(1:3)).EQ.'TKE').OR.(TRIM(arw3dv(n-1)(1:3)).EQ.'QKE')) THEN
           DIFT=.TRUE.
           CYCLE
        ELSE
           write(*,*)arw3dv(n-1),' DIFF VARIABLE REQUESTED'
           write(*,*)'CURRENTLY NOT SUPPORTED'
           write(*,*)'SUPPORT FOR NEW DIFF VARIABLE NEEDS'
           write(*,*)'TO BE CREATED INTERNALLY IN arw2arl.f'
           STOP
        END IF
     END IF
     label=TRIM(arw3dv(n))
     CALL setvar(diag,ncid,label,varid,n3d,ndim,dimlen,range)
     IF(varid.EQ.0) THEN
        WRITE(*,*)'Variable not found: ',TRIM(label)
        FTEST=.TRUE.
     END IF
     IF(diag.AND.nxp*nyp.NE.dimlen(1)*dimlen(2))THEN
        WRITE(*,*)'Dimemsion error   :',TRIM(label)
        WRITE(*,*)'Dimensioned (x,y) :',nxp,nyp
        WRITE(*,*)'Variable (x,y)    :',dimlen(1),dimlen(2)
     END IF
  END DO  

  ! if any variables missing then stop
  IF(FTEST)THEN
     WRITE(*,*)'Some defined variables not found in file: ',TRIM(netcdf_file)
     WRITE(*,*)'Edit ',TRIM(varcfg_file),' and try again ...'
     STOP
  END IF

! hybrid vertical coordinates
  hyb_opt=1
  label='HYBRID_OPT'
  kret = NF_GET_ATT_INT(ncid, nf_global, label, hyb_opt)
  IF(kret.NE.nf_noerr) THEN
     WRITE(*,*) label(:n),':',NF_STRERROR(kret)
     WRITE(*,*)'Use hyb_opt=1'
     hyb_opt=1
  END IF
  IF(diag)WRITE(*,*)label(:n),' = ',hyb_opt

  label='TITLE'
  title=' '
  kret = NF_GET_ATT_TEXT(ncid, nf_global, label, title)
  IF(kret.NE.nf_noerr) THEN
     WRITE(*,*) label(:),':',NF_STRERROR(kret)
     title=' '
  END IF
  IF(diag)WRITE(*,*)label(:),' = ',TRIM(title)

  IF(hyb_opt.EQ.2)THEN
     vert_coord = 5 ! WRF-hybrid

     ALLOCATE (c1h(nzp+1),c2h(nzp+1),c3h(nzp+1),c4h(nzp+1), &
      c1f(nzp+1),c2f(nzp+1),c3f(nzp+1),c4f(nzp+1),STAT=kret)
     IF(kret.NE.0)THEN
        WRITE(*,*)'Memory allocation error for hybrid variables'
        STOP
     END IF

     ALLOCATE (hybverth(nzp,ntp),STAT=kret)
     IF(kret.NE.0)THEN
        WRITE(*,*)'Memory allocation error for hybverth'
        STOP
     END IF

     ALLOCATE (hybvertf(nzp+1,ntp),STAT=kret)
     IF(kret.NE.0)THEN
        WRITE(*,*)'Memory allocation error for hybvertf'
        STOP
     END IF

     ! extract the level information based upon u-wind sigma levels
     label='C1H'
     CALL setvar(diag,ncid,label,varid,n3d,ndim,dimlen,range)
     kret = NF_GET_VAR_REAL(ncid, varid, hybverth)
     IF(kret.NE.nf_noerr) THEN
        WRITE(*,*) NF_STRERROR(kret)
        STOP
     ELSE
        DO n=1,nzp
           c1h(n)=hybverth(n,1)
        END DO
        c1h(nzp+1)=0.0
     END IF
     label='C2H'
     CALL setvar(diag,ncid,label,varid,n3d,ndim,dimlen,range)
     kret = NF_GET_VAR_REAL(ncid, varid, hybverth)
     IF(kret.NE.nf_noerr) THEN
        WRITE(*,*) NF_STRERROR(kret)
        STOP
     ELSE
        DO n=1,nzp
           c2h(n)=hybverth(n,1)
        END DO
        c2h(nzp+1)=0.0
     END IF
     label='C3H'
     CALL setvar(diag,ncid,label,varid,n3d,ndim,dimlen,range)
     kret = NF_GET_VAR_REAL(ncid, varid, hybverth)
     IF(kret.NE.nf_noerr) THEN
        WRITE(*,*) NF_STRERROR(kret)
        STOP
     ELSE
        DO n=1,nzp
           c3h(n)=hybverth(n,1)
        END DO
        c3h(nzp+1)=0.0
     END IF
     label='C4H'
     CALL setvar(diag,ncid,label,varid,n3d,ndim,dimlen,range)
     kret = NF_GET_VAR_REAL(ncid, varid, hybverth)
     IF(kret.NE.nf_noerr) THEN
        WRITE(*,*) NF_STRERROR(kret)
        STOP
     ELSE
        DO n=1,nzp
           c4h(n)=hybverth(n,1)
        END DO
        c4h(nzp+1)=0.0
     END IF
     label='C1F'
     CALL setvar(diag,ncid,label,varid,n3d,ndim,dimlen,range)
     kret = NF_GET_VAR_REAL(ncid, varid, hybvertf)
     IF(kret.NE.nf_noerr) THEN
        WRITE(*,*) NF_STRERROR(kret)
        STOP
     ELSE
        DO n=1,nzp+1
           c1f(n)=hybvertf(n,1)
        END DO
     END IF
     label='C2F'
     CALL setvar(diag,ncid,label,varid,n3d,ndim,dimlen,range)
     kret = NF_GET_VAR_REAL(ncid, varid, hybvertf)
     IF(kret.NE.nf_noerr) THEN
        WRITE(*,*) NF_STRERROR(kret)
        STOP
     ELSE
        DO n=1,nzp+1
           c2f(n)=hybvertf(n,1)
        END DO
     END IF
     label='C3F'
     CALL setvar(diag,ncid,label,varid,n3d,ndim,dimlen,range)
     kret = NF_GET_VAR_REAL(ncid, varid, hybvertf)
     IF(kret.NE.nf_noerr) THEN
        WRITE(*,*) NF_STRERROR(kret)
        STOP
     ELSE
        DO n=1,nzp+1
           c3f(n)=hybvertf(n,1)
        END DO
     END IF
     label='C4F'
     CALL setvar(diag,ncid,label,varid,n3d,ndim,dimlen,range)
     kret = NF_GET_VAR_REAL(ncid, varid, hybvertf)
     IF(kret.NE.nf_noerr) THEN
        WRITE(*,*) NF_STRERROR(kret)
        STOP
     ELSE
        DO n=1,nzp+1
           c4f(n)=hybvertf(n,1)
        END DO
     END IF
     IF(diag)THEN
        DO n=1,nzp+1
           WRITE(*,*) n,c1h(n),c2h(n),c3h(n),c4h(n),c1f(n),c2f(n),c3f(n),c4f(n)
        END DO
     END IF

     OPEN (hunit,FILE='hybrid_wrfvcoords.txt')
     WRITE(hunit,'(2I10)')nzp+1,npar
     WRITE(hunit,'(A)')'Output from arw2arl: WRF hybrid vertical coordinate parameters'
     WRITE(hunit,'(A)')'First line contains: nfull, npar'
     WRITE(hunit,'(A,I10)')'wrfout global attribute HYBRID_OPT:  ',hyb_opt
     WRITE(hunit,'(2A)')'wrfout global attribute TITLE:  ',TRIM(title)
     WRITE(hunit,'(A)')'Line following the ENDHEADER line contains the names of the npar parameters'
     WRITE(hunit,'(A)')'ENDHEADER'
     WRITE(hunit,'(A)')' C1H C2H C3H C4H C1F C2F C3F C4F'
     DO n=1,nzp+1
        WRITE(hunit,'(8G17.8)')c1h(n),c2h(n),c3h(n),c4h(n),c1f(n),c2f(n),c3f(n),c4f(n)
     END DO
     CLOSE(hunit)
  ELSE
     vert_coord = 1 ! sigma
  END IF

!------------------------------------------------------------------------------
! create the ARL packing information file 

  INQUIRE(FILE=TRIM(arlcfg_file), EXIST=ftest)
  IF(.NOT.ftest)THEN
     CALL cfgrec (kunit,arlcfg_file,nxp,nyp,mzp,level(:,1),num2dv,arl2dv,num3dv,arl3dv,vert_coord)
  ELSE
     WRITE(*,*)'Using an existing encoding configuration: ',TRIM(arlcfg_file)
  END IF

  ! initialize the packing routines
  KREC=1
  CALL PAKSET(kunit,TRIM(arlcfg_file),KREC,NXP,NYP,MZP)
  MZP=MZP-1

  ! configfure the ARL output data set
  nxy=nxp*nyp
  ALLOCATE (cvar(nxy),STAT=kret)
  IF(kret.NE.0)THEN
     WRITE(*,*)'Memory allocation error for packed data array'
     STOP
  END IF
  OPEN(kunit,FILE=TRIM(arlbin_file),RECL=(50+nxy),ACCESS='DIRECT',FORM='UNFORMATTED')


!------------------------------------------------------------------------------
! memory allocation 2D and 3D variables

  ftest=.FALSE.

  ALLOCATE (var2d(nxp,nyp),base(nxp,nyp),diff(nxp,nyp),STAT=kret) 
  IF(kret.NE.0)ftest=.TRUE.
  ALLOCATE (tpct(nxp,nyp),tpcc(nxp,nyp),tpcg(nxp,nyp),STAT=kret)
  IF(kret.NE.0)ftest=.TRUE.
  ALLOCATE (tppt(nxp,nyp),tppc(nxp,nyp),tppg(nxp,nyp),STAT=kret)
  IF(kret.NE.0)ftest=.TRUE.

  ALLOCATE (var3ds(nxp,nyp,nzs),STAT=kret)
  IF(kret.NE.0)ftest=.TRUE.

  ALLOCATE (var3d(nxp,nyp,nzp+1),pert(nxp,nyp,nzp),STAT=kret) 
  IF(kret.NE.0)ftest=.TRUE.
  ALLOCATE (uwnd(nxp+1,nyp,nzp),vwnd(nxp,nyp+1,nzp),wwnd(nxp,nyp,nzp+1),STAT=kret)
  IF(kret.NE.0)ftest=.TRUE.
  ALLOCATE (pres(nxp,nyp,nzp),temp(nxp,nyp,nzp),sphu(nxp,nyp,nzp),STAT=kret) 
  IF(kret.NE.0)ftest=.TRUE.

  IF(ftest)THEN
     WRITE(*,*)'Memory allocation error for data variables'
     STOP
  END IF

!------------------------------------------------------------------------------
! number of time periods loop

  DO L=NT1,NT2

     ! write the data fields for one time period in ARL format
     fh = NINT(XMIN(L)/60.0) ! forecast hour (minutes since start)
     READ(tvar(L),'(1x,5(1x,i2))')yy,mm,dd,hh,mn

     !-------------------------------------------------------------------------
     ! extract the 2-dimensional data fields

     rainc  =.FALSE.  ! set I/O flag to avoid duplicate reads
     rainnc =.FALSE.

     DO n=1,num2dv
        IF(TRIM(arw2dv(n)).EQ.'RAIN')THEN
           ! special processing for combined grid and convection precipitation
           IF(.NOT.rainc)THEN
              label='RAINC'
              CALL setvar(diag,ncid,label,varid,n3d,ndim,dimlen,range)
              CALL get3dv(diag,ncid,varid,nxp,nyp,nzp,L,var2d)
              tpcc=cnv2dv(n)*var2d        ! current convective total
              rainc =.TRUE.
           END IF

           IF(.NOT.rainnc)THEN
              label='RAINNC'
              CALL setvar(diag,ncid,label,varid,n3d,ndim,dimlen,range)
              CALL get3dv(diag,ncid,varid,nxp,nyp,nzp,L,var2d)
              tpcg=cnv2dv(n)*var2d        ! current grid scale total
              rainnc=.TRUE.
           END IF

        ELSEIF(TRIM(arw2dv(n)).EQ.'RAINC')THEN
           ! convective precipitation
           IF(.NOT.rainc)THEN
              label='RAINC'
              CALL setvar(diag,ncid,label,varid,n3d,ndim,dimlen,range)
              CALL get3dv(diag,ncid,varid,nxp,nyp,nzp,L,var2d)
              tpcc=cnv2dv(n)*var2d        ! current convective total
              rainc =.TRUE.
           END IF

        ELSEIF(TRIM(arw2dv(n)).EQ.'RAINNC')THEN
           ! non-convective grid scale precipitation
           IF(.NOT.rainnc)THEN
              label='RAINNC'
              CALL setvar(diag,ncid,label,varid,n3d,ndim,dimlen,range)
              CALL get3dv(diag,ncid,varid,nxp,nyp,nzp,L,var2d)
              tpcg=cnv2dv(n)*var2d        ! current grid scale total
              rainnc=.TRUE.
           END IF

        ELSEIF(TRIM(arw2dv(n)).EQ.'X')THEN
!          exclude because variable does not exist
           CONTINUE

        ELSEIF((TRIM(arw2dv(n)).EQ.'SMOIS').OR.(TRIM(arw2dv(n)).EQ.'SH2O'))THEN 
           label=TRIM(arw2dv(n))
           CALL setvar(diag,ncid,label,varid,n3d,ndim,dimlen,range)
           CALL get4dv(diag,ncid,varid,dimlen(1),dimlen(2),dimlen(3),L,var3ds)
           base=cnv2dv(n)*var3ds(:,:,1)
           CALL PAKREC(kunit,base,cvar,nxp,nyp,nxy,arl2dv(n),yy,mm,dd,hh,mn,fh,1,kinit)
        ELSE
!          all other 2D variables
           label=TRIM(arw2dv(n))
           CALL setvar(diag,ncid,label,varid,n3d,ndim,dimlen,range)
           CALL get3dv(diag,ncid,varid,nxp,nyp,nzp,L,var2d)
           base=cnv2dv(n)*var2d    
           CALL PAKREC(kunit,base,cvar,nxp,nyp,nxy,arl2dv(n),yy,mm,dd,hh,mn,fh,1,kinit) 
        END IF
     END DO

     !-------------------------------------------------------------------------
     ! Further processing is required to convert accumulated precipitation values 
     ! over the duration of the simulation into hourly totals. Within a multi-time
     ! period file this is done by computing the difference between the current and
     ! previous time accumulated values. When the simulation results span multiple
     ! files the values from the last record are written into file called WRFRAIN.BIN. 
     ! This file would be read upon opening the next time period output file.  When 
     ! this file does not exist or the forecast time associated with this precip
     ! file is not correct, then the accumulation for the first hour is set to zero.
     !
     ! Variable        Current-Total   Previous-Hour   
     ! -------         ------------    ------------
     ! Total           tpct            tppt            
     ! Convective      tpcc            tppc           
     ! Grid-Scale      tpcg            tppg          

     tpct=tpcc+tpcg  ! add convective and grid scale for current total 

     IF(L.EQ.1)THEN
        ! at first input record check if input file is available with the
        ! previous file's last precipitation accumulation value

        tppt=0.0 ! previous total 
        tppc=0.0 ! previous convective
        tppg=0.0 ! previous grid scale

        IF(XMIN(L).LE.0.0)THEN     
           ! force zero at forecast initialization time because even if
           ! the precip field exists at this time it cannot be correct 
           tpct=0.0
           tpcc=0.0
           tpcg=0.0

!          undefined because only one time period in file
!          and initialization hour has no precip anyway
           IF(DELT.EQ.0.0) DELT=60.0
           IF(diag)WRITE(*,*)'Forced output time interval in minutes: ', DELT
  
        ELSE
!          when the first record is at a non-zero forecast time check to see 
!          if there is a previous time period precipitation file to read
           INQUIRE(FILE=precip_file,EXIST=FTEST)
           IF(FTEST)THEN
              OPEN(LUNIT,FILE=precip_file,FORM='UNFORMATTED')
              READ(LUNIT) xini, xdel        
              READ(LUNIT) tppt, tppc, tppg
              CLOSE(LUNIT)
              WRITE(*,*)'Initialized accumulated precip from file: ',TRIM(precip_file)

              IF(DELT.EQ.0.0)THEN
!                time interval unknown when file only contains one time period
!                however, when a previous time period precip file exists, then
!                the time interval can be computed from the two forecast times
                 DELT=xmin(1)-xini

                 IF(NINT(DELT).LE.0)THEN
                    WRITE(*,*)'Accumulated precip file not from the same simulation'
!                   reset the current to zero because the previous accumulation is unknown
                    tppt=tpct
                    tppc=tpcc
                    tppg=tpcg
                    DELT=60.0
                    IF(diag)WRITE(*,*)'Forced output time interval in minutes: ', DELT

                 ELSEIF(NINT(DELT).NE.NINT(XDEL))THEN
                    WRITE(*,*)'Accumulated precip file not from the same simulation'
!                   dignostic test for consistency of this file and precip file
!                   reset previous to current and continue
                    tppt=tpct
                    tppc=tpcc
                    tppg=tpcg

                 ELSE
                    IF(diag)WRITE(*,*)'Set output interval from previous precip (min): ', DELT
                 END IF

              ELSE
!                time interval has been set because this is a multi-time file
                 IF(NINT(DELT).NE.NINT(XDEL).OR.        &
                    NINT(XMIN(1)-XINI).NE.NINT(DELT))THEN
                    WRITE(*,*)'Accumulated precip file not from the same simulation'
                    tppt=tpct
                    tppc=tpcc
                    tppg=tpcg
                 END IF
              END IF 

           ELSE
!             when there is no previous precipitation file and the first record
!             of this file is somewhere in the forecast, we have no previous
!             accumulation to compute the hourly totals, therefore we set the
!             previous total to the current accumulation value so that the rate 
!             for this time period becomes zero.
              WRITE(*,*)'Forecast time greater than zero, but no previous precip file found!'
              tppt=tpct
              tppc=tpcc
              tppg=tpcg

              IF(DELT.LE.0.0)THEN
!                if current file contains multiple time periods this value
!                should have been set earlier, otherwise set 60 min default
                 DELT=60.0
                 IF(diag)WRITE(*,*)'No previous precip file, set interval (min): ', DELT
              END IF
           END IF
        END IF

     ELSEIF(L.EQ.NT1)THEN
        ! when the first record processed is not the first record in the file,
        ! read the precip from the previous record to get the accumulated value

        label='RAINNC'
        CALL setvar(diag,ncid,label,varid,n3d,ndim,dimlen,range)
        CALL get3dv (diag,ncid,varid,nxp,nyp,nzp,(L-1),var2d)
        tppg=cnv2dv(n)*var2d

        label='RAINC'
        CALL setvar(diag,ncid,label,varid,n3d,ndim,dimlen,range)
        CALL get3dv (diag,ncid,varid,nxp,nyp,nzp,(L-1),var2d)
        tppc=cnv2dv(n)*var2d

        tppt=tppc+tppg
        WRITE(*,*)'Initialized accumulated precip from previous value in file'
     END IF

     ! write precipitation records to output file corrected to hourly totals
     ! regardless of the frequency of the output interval
     DO n=1,num2dv
        IF(TRIM(arw2dv(n)).EQ.'RAIN')THEN
           base=(tpct-tppt)*60.0/DELT
           CALL PAKREC(kunit,base,cvar,nxp,nyp,nxy,arl2dv(n),yy,mm,dd,hh,mn,fh,1,kinit) 

!          determine the packed values at each grid point (uses pakval common block)
           CALL PAKINP(var2d,cvar,nxp,nyp,1,1,nxp,nyp,prec,nexp,var1,ksum)
           IF(DIFR)THEN
!             compute the difference between the packed and original values
              diff=base-var2d 
!             write the difference to the output file 
              CALL PAKREC(kunit,diff,cvar,nxp,nyp,nxy,'DIFR',yy,mm,dd,hh,mn,fh,1,kinit)  
           END IF

        ELSEIF(TRIM(arw2dv(n)).EQ.'RAINC')THEN
           base=(tpcc-tppc)*60.0/DELT
           CALL PAKREC(kunit,base,cvar,nxp,nyp,nxy,arl2dv(n),yy,mm,dd,hh,mn,fh,1,kinit) 
        ELSEIF(TRIM(arw2dv(n)).EQ.'RAINNC')THEN
           base=(tpcg-tppg)*60.0/DELT
           CALL PAKREC(kunit,base,cvar,nxp,nyp,nxy,arl2dv(n),yy,mm,dd,hh,mn,fh,1,kinit) 
        END IF
     END DO

     ! reset accumulation bucket with every time period
     tppt=tpct
     tppc=tpcc
     tppg=tpcg

     ! at the last record in the file save accumulation
     IF(L.EQ.NT2)THEN
        IF(diag)WRITE(*,*)'Writing precip accumulation file: ', xmin(ntp),delt
        OPEN(LUNIT,FILE=precip_file,FORM='UNFORMATTED')
        WRITE(LUNIT) xmin(ntp),delt   
        WRITE(LUNIT) tppt, tppc, tppg
        CLOSE(LUNIT)
     END IF

     !-------------------------------------------------------------------------
     ! extract the 3-dimensional data fields

     DO n=1,num3dv
        IF(TRIM(arw3dv(n)).EQ.'X') CYCLE  
        label=TRIM(arw3dv(n))
        CALL setvar (diag,ncid,label,varid,n3d,ndim,dimlen,range)

        ! variables that may need to be saved for special processing
        IF(    TRIM(arw3dv(n)).EQ.'T')THEN
           CALL get4dv (diag,ncid,varid,dimlen(1),dimlen(2),dimlen(3),L,temp)
           IF(TRIM(arl3dv(n)).EQ.'THET')thetflg=.TRUE.
        ELSEIF(TRIM(arw3dv(n)).EQ.'U')THEN
           CALL get4dv (diag,ncid,varid,dimlen(1),dimlen(2),dimlen(3),L,uwnd)
        ELSEIF(TRIM(arw3dv(n)).EQ.'AVGFLX_RUM'.AND.kcfg.eq.2)THEN
           CALL get4dv (diag,ncid,varid,dimlen(1),dimlen(2),dimlen(3),L,uwnd)
        ELSEIF(TRIM(arw3dv(n)).EQ.'V')THEN
           CALL get4dv (diag,ncid,varid,dimlen(1),dimlen(2),dimlen(3),L,vwnd)
        ELSEIF(TRIM(arw3dv(n)).EQ.'AVGFLX_RVM'.AND.kcfg.eq.2)THEN
           CALL get4dv (diag,ncid,varid,dimlen(1),dimlen(2),dimlen(3),L,vwnd)
        ELSEIF(TRIM(arw3dv(n)).EQ.'W')THEN
           CALL get4dv (diag,ncid,varid,dimlen(1),dimlen(2),dimlen(3),L,wwnd)
        ELSEIF(TRIM(arw3dv(n)).EQ.'AVGFLX_WWM'.AND.kcfg.eq.2)THEN
           CALL get4dv (diag,ncid,varid,dimlen(1),dimlen(2),dimlen(3),L,wwnd)
        ELSEIF(TRIM(arw3dv(n)).EQ.'P')THEN
           CALL get4dv (diag,ncid,varid,dimlen(1),dimlen(2),dimlen(3),L,pres)
           label='PB'
           CALL setvar (diag,ncid,label,varid,n3d,ndim,dimlen,range)
           CALL get4dv (diag,ncid,varid,dimlen(1),dimlen(2),dimlen(3),L,pert)
        ELSEIF(TRIM(arw3dv(n)).EQ.'QVAPOR')THEN
           CALL get4dv (diag,ncid,varid,dimlen(1),dimlen(2),dimlen(3),L,sphu)
        ! all other variables can be written immediately
        ELSE
           CALL get4dv(diag,ncid,varid,dimlen(1),dimlen(2),dimlen(3),L,var3d)
           IF(TRIM(arl3dv(n)).EQ.'TKEN')THEN
              IF(dimlen(3).EQ.nzp)THEN
                 write(*,*)'ARW TKE variable on unstaggered grid'
                 write(*,*)'ARL requested TKE variable on staggered grid'
                 write(*,*)'TKEN = ARL staggered TKE variable'
                 write(*,*)'TKEU = ARL unstaggered TKE variable'
                 write(*,*)'Rename TKEN in WRFDATA.CFG to TKEU and try again'
                 STOP
              ENDIF
           ELSEIF(TRIM(arl3dv(n)).EQ.'TKEU')THEN
              IF(dimlen(3).EQ.nzp+1)THEN
                 write(*,*)'ARW TKE variable on staggered grid'
                 write(*,*)'ARL requested TKE variable on unstaggered grid'
                 write(*,*)'TKEN = ARL staggered TKE variable'
                 write(*,*)'TKEU = ARL unstaggered TKE variable'
                 write(*,*)'Rename TKEU in WRFDATA.CFG to TKEN and try again'
                 STOP
              ENDIF
           ENDIF
           DO k=1,MZP
              base=cnv3dv(n)*var3d(:,:,k)
              CALL PAKREC(kunit,base,cvar,nxp,nyp,nxy,arl3dv(n),yy,mm,dd,hh,mn,fh,k+1,kinit)

              IF(TRIM(arw3dv(n)(1:3)).EQ.'TKE'.OR.(TRIM(arw3dv(n)(1:3)).EQ.'QKE'))THEN
                 IF(DIFT)THEN
!                   determine the packed values at each grid point
                    CALL PAKINP(var2d,cvar,nxp,nyp,1,1,nxp,nyp,prec,nexp,var1,ksum)
!                   compute the difference between the packed and original
!                   values
                    diff=base-var2d
!                   write the difference to the output file 
                    CALL PAKREC(kunit,diff,cvar,nxp,nyp,nxy,'DIFT',yy,mm,dd,hh,mn,fh,k+1,kinit)
                 END IF
              END IF

           END DO
        END IF
     END DO

     DO k=1,MZP   
        ! pressure (Pa) is the sum of perturbation and base state pressure
        pres(:,:,k)=pres(:,:,k)+pert(:,:,k)
        if(thetflg)then
           ! add WRF offset to convert to potential temperature
           temp(:,:,k)=temp(:,:,k)+300.0
        else
           ! convert potential to ambient temperature
           temp(:,:,k)=(temp(:,:,k)+300.0)*(pres(:,:,k)/100000.0)**rovcp
        endif
        if(kcfg.ne.2)then 
        ! convert vertical velocity from m/s to hPa/s using omega = -W g rho
        wwnd(:,:,k)=-wwnd(:,:,k)*grav*pres(:,:,k)*0.01 /      &
                    (rdry*temp(:,:,k)*(1.0+0.6077*sphu(:,:,k)))
        endif
     END DO

     ! loop through all the levels and adjust output for grid stagger
     SKIP=.FALSE.
     DO n=1,num3dv
        IF(TRIM(arw3dv(n)).EQ.'X') CYCLE ! DIFF variable is computed
        DO k=1,MZP   
           IF(    TRIM(arw3dv(n)).EQ.'U')THEN
              base(:,:)=cnv3dv(n)*uwnd(1:nxp,:,k)
           ELSEIF(TRIM(arw3dv(n)).EQ.'AVGFLX_RUM'.and.kcfg.eq.2)THEN
              base(:,:)=cnv3dv(n)*uwnd(1:nxp,:,k)
           ELSEIF(TRIM(arw3dv(n)).EQ.'V')THEN
              base(:,:)=cnv3dv(n)*vwnd(:,1:nyp,k)
           ELSEIF(TRIM(arw3dv(n)).EQ.'AVGFLX_RVM'.and.kcfg.eq.2)THEN
              base(:,:)=cnv3dv(n)*vwnd(:,1:nyp,k)
           ELSEIF(TRIM(arw3dv(n)).EQ.'W')THEN
              base=cnv3dv(n)*wwnd(:,:,k)
           ELSEIF(TRIM(arw3dv(n)).EQ.'AVGFLX_WWM'.and.kcfg.eq.2)THEN
              base=cnv3dv(n)*wwnd(:,:,k)
           ELSEIF(TRIM(arw3dv(n)).EQ.'T')THEN
              base=cnv3dv(n)*temp(:,:,k)
           ELSEIF(TRIM(arw3dv(n)).EQ.'P')THEN
              base=cnv3dv(n)*pres(:,:,k)
           ELSEIF(TRIM(arw3dv(n)).EQ.'QVAPOR')THEN
              base=cnv3dv(n)*sphu(:,:,k)
           ELSEIF(TRIM(arw3dv(n)).EQ.'PB')THEN
              base=cnv3dv(n)*pert(:,:,k)
           ELSE
!             variable already written
              SKIP=.TRUE.
           END IF
           IF(.NOT.SKIP)THEN
              CALL PAKREC(kunit,base,cvar,nxp,nyp,nxy,arl3dv(n),yy,mm,dd,hh,mn,fh,k+1,kinit)  
           ELSE
              SKIP=.FALSE.
           END IF

!          for vertical velocities write DIFFerence field for greater precision
!          previous variable written is assumed to have been the vertical velocity
           IF((TRIM(arw3dv(n)).EQ.'W').OR.(TRIM(arw3dv(n)).EQ.'AVGFLX_WWM'))THEN
              IF(DIFW)THEN
!                determine the packed values at each grid point
                 CALL PAKINP(var2d,cvar,nxp,nyp,1,1,nxp,nyp,prec,nexp,var1,ksum)
!                compute the difference between the packed and original values
                 diff=base-var2d 
!                write the difference to the output file 
                 CALL PAKREC(kunit,diff,cvar,nxp,nyp,nxy,'DIFW',yy,mm,dd,hh,mn,fh,k+1,kinit)  
              END IF
           END IF
        END DO
     END DO

     ! write index record to output file
     CALL PAKNDX(kunit)

     ! time period loop
     WRITE(*,*)'Completed: ',yy,mm,dd,hh,mn
  END DO

  CLOSE (kunit)
  kret = NF_CLOSE(ncid)

END PROGRAM arw2arl
