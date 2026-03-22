PROGRAM cesm2arl

!---------------------------------------------------------------------------
! CESN data TO ARL FORMAT
!---------------------------------------------------------------------------
! Last revised: 19 Jun 2020 (CPL) - initial version 
!               09 Jul 2020 (CPL) - remove -f option
!
!---------------------------------------------------------------------------

  IMPLICIT NONE

  include 'netcdf.inc'

  CHARACTER(256):: netcdf_file, varcfg_file, arlcfg_file, arlbin_file
  CHARACTER(256):: f3dasm, f2dflx, f2dslv, f2dint
  CHARACTER(256):: label
  CHARACTER(256):: prefix
  CHARACTER(256):: logfile, grdfile, arlfile
  LOGICAL       :: diag
  INTEGER       :: l, nt1, nt2
  INTEGER       :: n3d, ndim, varid, narg, iargc, clen, mzp, maxl
  INTEGER       :: kret, kinit
  INTEGER       :: dimlen(nf_max_var_dims)

  REAL*8        :: p0                     ! P0

  REAL*8,  ALLOCATABLE :: hyam(:)           ! A
  REAL*8,  ALLOCATABLE :: hybm(:)           ! B
  REAL*8,  ALLOCATABLE :: lat(:)            ! latitude
  REAL*8,  ALLOCATABLE :: lon(:)            ! longitude

  INTEGER, ALLOCATABLE :: yyyymmdd(:)       ! current date
  INTEGER, ALLOCATABLE :: datesec(:)        ! seconds of current date
  INTEGER, ALLOCATABLE :: ndcur(:)          ! days since initialization
  INTEGER, ALLOCATABLE :: nscur(:)          ! seconds in days since initialization 

  REAL,  ALLOCATABLE :: level(:)
  REAL,  ALLOCATABLE :: var2d(:,:)      ! generic 2D variable     
  REAL,  ALLOCATABLE :: var3d(:,:,:)    ! generic 3D variable 
  REAL,  ALLOCATABLE :: base(:,:)       ! temporary 2D write variable

  INTEGER, PARAMETER :: kunit = 50      ! output unit for ARL packed data
  INTEGER, PARAMETER :: lunit = 60      ! log file unit

  INTEGER, PARAMETER :: nfile = 4       ! number of files

  INTEGER(2)         :: word
  CHARACTER(8)       :: cdate                ! process date for file name

  INTEGER            :: nxp, nyp, nzp        ! number of pts in output grid
  INTEGER            :: ntp                  ! number of time step
  INTEGER            :: nxy, lrec            ! product of pts and record length
  LOGICAL            :: ftest                ! file exist test result
  INTEGER            :: n, k, m, ii          ! dummy indicies
  INTEGER            :: idx                  ! index for x grid points in 0-360 system
  INTEGER            :: yy,mm,dd,hh,mn,fh    ! current obs time
  INTEGER            :: yyyy,temp            ! temporary time variables
  INTEGER            :: year, month, day     ! processing month
  INTEGER            :: mc0, mc3d, mc2d      ! accumulated minutes
  INTEGER            :: krec                 ! output record counter

! file dependent variables
  INTEGER        :: handle (nfile)      ! unit number for netcdf files
  INTEGER        :: kdim   (4,nfile)    ! number in x,y,z,t

  INTEGER, PARAMETER :: max2dv = 20        ! max number of 2D variables
  INTEGER, PARAMETER :: max3dv = 10        ! max number of 3D variables
  INTEGER            :: num2dv, num3dv     ! actual number of variables
  REAL               :: cnv2dv(max2dv)     ! conversion factor MERRA2->ARL
  REAL               :: cnv3dv(max3dv)
  CHARACTER(12)      :: cesm2dv(max2dv)     ! MERRA2 variable names
  CHARACTER(12)      :: cesm3dv(max3dv)
  CHARACTER(4)       :: arl2dv(max2dv)     ! ARL variable names
  CHARACTER(4)       :: arl3dv(max3dv)

  CHARACTER(1), ALLOCATABLE :: cvar(:)  ! packed output array

  REAL,         ALLOCATABLE :: tmflx(:), tmasm(:), tmslv(:), tmint(:)  ! time steps for 2d and 3d variables

  INTEGER        :: ncid, ixx, iyy
  INTEGER        :: iflx, islv, iint, tidx   
  LOGICAL        :: typ3d

!---------------------------------------------------------

  NAMELIST/SETUP/ num3dv,cesm3dv,cnv3dv,arl3dv,num2dv,cesm2dv,cnv2dv,arl2dv

!------------------------------------------------------------------------------
! subroutine interfaces

  INTERFACE   

  SUBROUTINE getdim (diag,ncid,ntp,nxp,nyp,nzp)
  IMPLICIT NONE
  LOGICAL,       INTENT(IN)  :: diag
  INTEGER,       INTENT(IN)  :: ncid
  INTEGER,       INTENT(OUT) :: ntp
  INTEGER,       INTENT(OUT) :: nxp
  INTEGER,       INTENT(OUT) :: nyp
  INTEGER,       INTENT(OUT) :: nzp
  END SUBROUTINE getdim

  SUBROUTINE setvar (diag,ncid,label,varid,n3d,ndim,dimlen)
  IMPLICIT NONE
  include 'netcdf.inc'
  LOGICAL,       INTENT(IN)  :: diag
  INTEGER,       INTENT(IN)  :: ncid
  CHARACTER(80), INTENT(IN)  :: label
  INTEGER,       INTENT(OUT) :: varid
  INTEGER,       INTENT(OUT) :: n3d
  INTEGER,       INTENT(OUT) :: ndim
  INTEGER,       INTENT(OUT) :: dimlen(nf_max_var_dims)
  END SUBROUTINE setvar

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

  SUBROUTINE cfgrec (kunit,arlcfg_file,nxp,nyp,nzp,level,num2dv,arl2dv,num3dv,arl3dv,lat,lon)
  IMPLICIT NONE
  INTEGER,      INTENT(IN)   :: kunit
  CHARACTER(80),INTENT(IN)   :: arlcfg_file
  INTEGER,      INTENT(IN)   :: nxp,nyp,nzp
  REAL,         INTENT(IN)   :: level(:)
  INTEGER,      INTENT(IN)   :: num2dv
  CHARACTER(4), INTENT(IN)   :: arl2dv(:)
  INTEGER,      INTENT(IN)   :: num3dv
  CHARACTER(4), INTENT(IN)   :: arl3dv(:)
  REAL*8,       INTENT(IN)   :: lat(:)
  REAL*8,       INTENT(IN)   :: lon(:)
  END SUBROUTINE cfgrec

  END INTERFACE

!------------------------------------------------------------------------------
! define the command line options with default values

  VARCFG_FILE = 'CESMDATA.CFG'
  NETCDF_FILE = 'CESMOUT.NC'   ! NetCDF CESM input
  ARLCFG_FILE = 'ARLDATA.CFG'  ! ARL packing configuration file
  ARLBIN_FILE = 'ARLDATA.BIN'  ! ARL formatted output file
  NT1=1           ! index of starting time period
  NT2=9999        ! index of ending time period
  KINIT = 1       ! output file initialization 
  MAXL = 50       ! maximum number of levels
  DIAG = .false.  ! diagnostic output

  NARG=IARGC()
  IF(NARG.EQ.0)THEN
     WRITE(*,*)'USAGE-1: cesm2arl [netcdf data file name]'
     WRITE(*,*)' '
     WRITE(*,*)'USAGE-2: cesm2arl -[options (default)]'
     WRITE(*,*)'  -b[beginning time period index (1)]'
     WRITE(*,*)'  -e[ending time period index (9999)]'
     WRITE(*,*)'  -d[diagnostic output turned on]'
     WRITE(*,*)'  -i[input netcdf data file name (CESMOUT.NC)]'
     WRITE(*,*)'  -o[output ARL packed file name (ARLDATA.BIN)]'
     WRITE(*,*)'  -p[packing configuration file name (ARLDATA.CFG)]'
     WRITE(*,*)'  -v[variable namelist file name (CESMDATA.CFG)]'
     WRITE(*,*)'  -n[number of levels to extract from sfc (50)]'
     STOP
  END IF

  ! go through each argument
  DO WHILE (NARG.GT.0)

     CALL GETARG(NARG,LABEL)
     SELECT CASE (LABEL(1:2))

     ! beginning time period to extract
     CASE ('-b','-B')
        READ(LABEL(3:),'(I1)')NT1

     ! end time period to extract   
     CASE ('-e','-E')
        READ(LABEL(3:),'(I4)')NT2

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

     END SELECT
     NARG=NARG-1

  END DO

!---------------------------------------------------------
! diagnostic output file

  logfile = 'MESSAGE'

  OPEN(LUNIT,FILE=logfile)

  WRITE(LUNIT,*) 'Log file: ',trim(logfile)

!---------------------------------------------------------
! Open and read the namelist that defines the CESM variables that will be
! extracted and converted to the HYSPLIT compatible ARL format. If it does not
! exist, the namelist will be created. Existing namelists may be user edited to
! delete or define new variables.

  INQUIRE(FILE=TRIM(varcfg_file), EXIST=ftest)
  IF(.NOT.ftest)THEN
     CALL cfgvar(varcfg_file)
  ELSE
     WRITE(LUNIT,*)'Using an existing decoding configuration: ',TRIM(varcfg_file)
  END IF

  OPEN (10,FILE=TRIM(varcfg_file))
  READ (10,SETUP)
  CLOSE(10)

  IF(num2dv.GT.max2dv)THEN
     WRITE(LUNIT,*)'Number of 2D variables',num2dv,' exceeding compiled limit',max2dv
     STOP
  END IF

  IF(num3dv.GT.max3dv)THEN
     WRITE(LUNIT,*)'Number of 3D variables',num3dv,' exceeding compiled limit',max3dv
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
  CALL getdim (diag,ncid,ntp,nxp,nyp,nzp)

  IF(diag.AND.ntp.GT.1)WRITE(*,*)'NetCDF file contains ',ntp,' time periods'
  NT1=MIN(NT1,NTP)
  NT2=MIN(NT2,NTP)

  ! limit the number of levels
  MZP=MIN(NZP,MAXL)

  ! get valid time
  label='date'
  CALL setvar(diag,ncid,label,varid,n3d,ndim,dimlen)
  ALLOCATE (yyyymmdd(ntp),STAT=kret)
  IF(kret.NE.0)THEN
     WRITE(*,*)'Allocation error in yyyymmdd array'
     STOP
  END IF

  kret = NF_GET_VAR_INT(ncid, varid, yyyymmdd)
  IF(kret.NE.nf_noerr) THEN
     WRITE(*,*) NF_STRERROR(kret)
     STOP
  ELSEIF(diag)THEN
     WRITE(*,*) yyyymmdd
  END IF

  label='datesec'
  CALL setvar(diag,ncid,label,varid,n3d,ndim,dimlen)
  ALLOCATE (datesec(ntp),STAT=kret)
  IF(kret.NE.0)THEN
     WRITE(*,*)'Allocation error in datesec array'
     STOP
  END IF

  kret = NF_GET_VAR_INT(ncid, varid, datesec)
  IF(kret.NE.nf_noerr) THEN
     WRITE(*,*) NF_STRERROR(kret)
     STOP
  ELSEIF(diag)THEN
     WRITE(*,*) datesec
  END IF

  ! get time since initialization
  label='ndcur'
  CALL setvar(diag,ncid,label,varid,n3d,ndim,dimlen)
  ALLOCATE (ndcur(ntp),STAT=kret)
  IF(kret.NE.0)THEN
     WRITE(*,*)'Allocation error in ndcur array'
     STOP
  END IF

  kret = NF_GET_VAR_INT(ncid, varid, ndcur)
  IF(kret.NE.nf_noerr) THEN
     WRITE(*,*) NF_STRERROR(kret)
     STOP
  ELSEIF(diag)THEN
     WRITE(*,*) ndcur
  END IF

  label='nscur'
  CALL setvar(diag,ncid,label,varid,n3d,ndim,dimlen)
  ALLOCATE (nscur(ntp),STAT=kret)
  IF(kret.NE.0)THEN
     WRITE(*,*)'Allocation error in datesec array'
     STOP
  END IF

  kret = NF_GET_VAR_INT(ncid, varid, nscur)
  IF(kret.NE.nf_noerr) THEN
     WRITE(*,*) NF_STRERROR(kret)
     STOP
  ELSEIF(diag)THEN
     WRITE(*,*) nscur
  END IF

  ! extract hyam
  label='hyam'
  CALL setvar(diag,ncid,label,varid,n3d,ndim,dimlen)
  ALLOCATE (hyam(nzp),STAT=kret)
  IF(kret.NE.0)THEN
     WRITE(*,*)'Allocation error in hyam array'
     STOP
  END IF

  kret = NF_GET_VAR_DOUBLE(ncid, varid, hyam)
  IF(kret.NE.nf_noerr) THEN
     WRITE(*,*) NF_STRERROR(kret)
     STOP
  ELSEIF(diag)THEN
     WRITE(*,*) hyam
  END IF

  ! extract hybm
  label='hybm'
  CALL setvar(diag,ncid,label,varid,n3d,ndim,dimlen)
  ALLOCATE (hybm(nzp),STAT=kret)
  IF(kret.NE.0)THEN
     WRITE(*,*)'Allocation error in hybm array'
     STOP
  END IF

  kret = NF_GET_VAR_DOUBLE(ncid, varid, hybm)
  IF(kret.NE.nf_noerr) THEN
     WRITE(*,*) NF_STRERROR(kret)
     STOP
  ELSEIF(diag)THEN
     WRITE(*,*) hybm
  END IF

  ! extract P0
  label='P0'
  CALL setvar(diag,ncid,label,varid,n3d,ndim,dimlen)

  kret = NF_GET_VAR_DOUBLE(ncid, varid, p0)
  IF(kret.NE.nf_noerr) THEN
     WRITE(*,*) NF_STRERROR(kret)
     STOP
  ELSEIF(diag)THEN
     WRITE(*,*) p0
  END IF

  ALLOCATE (level(nzp),STAT=kret)
  IF(kret.NE.0)THEN
     WRITE(*,*)'Allocation error in level array'
     STOP
  END IF

  DO n=1,nzp
     level(nzp+1-n) = NINT((p0 / 100.) * hyam(n)) + hybm(n)
  END DO

  IF(diag)THEN
     DO n=1,nzp
        WRITE(*,*) n, level(n), p0, hyam(n), hybm(n)
     END DO
  END IF

  ! extract lat
  label='lat'
  CALL setvar(diag,ncid,label,varid,n3d,ndim,dimlen)
  ALLOCATE (lat(nyp),STAT=kret)
  IF(kret.NE.0)THEN
     WRITE(*,*)'Allocation error in lat array'
     STOP
  END IF

  kret = NF_GET_VAR_DOUBLE(ncid, varid, lat)
  IF(kret.NE.nf_noerr) THEN
     WRITE(*,*) NF_STRERROR(kret)
     STOP
  ELSEIF(diag)THEN
     WRITE(*,*) lat
  END IF

  ! extract lon
  label='lon'
  CALL setvar(diag,ncid,label,varid,n3d,ndim,dimlen)
  ALLOCATE (lon(nxp),STAT=kret)
  IF(kret.NE.0)THEN
     WRITE(*,*)'Allocation error in lon array'
     STOP
  END IF

  kret = NF_GET_VAR_DOUBLE(ncid, varid, lon)
  IF(kret.NE.nf_noerr) THEN
     WRITE(*,*) NF_STRERROR(kret)
     STOP
  ELSEIF(diag)THEN
     WRITE(*,*) lon
  END IF

  FTEST=.FALSE.
  ! check to insure all defined 2D variables exist in NetCDF file
  DO n=1,num2dv
     label=TRIM(cesm2dv(n))
     CALL setvar(diag,ncid,label,varid,n3d,ndim,dimlen)
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
  DO n=1,num3dv
     label=TRIM(cesm3dv(n))
     CALL setvar(diag,ncid,label,varid,n3d,ndim,dimlen)
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

!------------------------------------------------------------------------------
! create the ARL packing information file 

  INQUIRE(FILE=TRIM(arlcfg_file), EXIST=ftest)
  IF(.NOT.ftest)THEN
     CALL cfgrec (kunit,arlcfg_file,nxp,nyp,mzp,level(:),num2dv,arl2dv,num3dv,arl3dv,lat,lon)
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

  ALLOCATE (var2d(nxp,nyp),base(nxp,nyp),STAT=kret)
  IF(kret.NE.0)ftest=.TRUE.

  ALLOCATE (var3d(nxp,nyp,nzp),STAT=kret)
  IF(kret.NE.0)ftest=.TRUE.

  IF(ftest)THEN
     WRITE(*,*)'Memory allocation error for data variables'
     STOP
  END IF

!------------------------------------------------------------------------------
! number of time periods loop

  DO L=NT1,NT2

     ! write the data fields for one time period in ARL format
     yyyy=yyyymmdd(L)/10000
     temp=yyyy/100
     yy=yyyy - temp*100
     mm=(yyyymmdd(L)/100) - yyyy*100
     dd=yyyymmdd(L) - yyyy*10000 - mm*100
     hh=datesec(L)/3600
     mn=datesec(L)/60 - hh*60

     fh=ndcur(L)*24.+nscur(L)/3600. ! forecast hour

     !-------------------------------------------------------------------------
     ! extract the 2-dimensional data fields

     DO n=1,num2dv

        label=TRIM(cesm2dv(n))
        CALL setvar(diag,ncid,label,varid,n3d,ndim,dimlen)
        CALL get3dv(diag,ncid,varid,nxp,nyp,nzp,L,var2d)
        base=cnv2dv(n)*var2d
        CALL PAKREC(kunit,base,cvar,nxp,nyp,nxy,arl2dv(n),yy,mm,dd,hh,mn,fh,1,kinit)
     END DO

     !-------------------------------------------------------------------------
     ! extract the 3-dimensional data fields

     DO n=1,num3dv
        label=TRIM(cesm3dv(n))
        CALL setvar (diag,ncid,label,varid,n3d,ndim,dimlen)
        CALL get4dv(diag,ncid,varid,dimlen(1),dimlen(2),dimlen(3),L,var3d)
        DO k=1,MZP
           base=cnv3dv(n)*var3d(:,:,nzp+1-k)
           CALL PAKREC(kunit,base,cvar,nxp,nyp,nxy,arl3dv(n),yy,mm,dd,hh,mn,fh,k+1,kinit)
        END DO
     END DO

     ! write index record to output file
     CALL PAKNDX(kunit)
     ! time period loop
     WRITE(*,*)'Completed: ',yy,mm,dd,hh,mn
  END DO

  CLOSE (kunit)
  kret = NF_CLOSE(ncid)

END PROGRAM cesm2arl
