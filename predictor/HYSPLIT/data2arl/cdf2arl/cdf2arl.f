PROGRAM cdf2arl

!---------------------------------------------------------------------------
! NCEP/NCAR Reanalysis TO ARL FORMAT
!---------------------------------------------------------------------------
! Last revised: 16 Mar 2015 (FN)  - modify original cdf2arl for reading 
!                                   input files in NetCDF4 format
!
! Input files from ftp://ftp.cdc.noaa.gov/Datasets/ncep.reanalysis/
!                        pres.sfc.2012.nc
!                        air.sig995.2012.nc
!                        uwnd.sig995.2012.nc
!                        vwnd.sig995.2012.nc
!                        prate.sfc.gauss.2012.nc
!                        hgt.2012.nc
!                        air.2012.nc
!                        uwnd.2012.nc
!                        vwnd.2012.nc
!                        omega.2012.nc
!                        rhum.2012.nc
!---------------------------------------------------------------------------

  IMPLICIT NONE

  include 'netcdf.inc'

  REAL          :: range(2)
  CHARACTER(256):: label
  CHARACTER(14) :: logfile
  LOGICAL       :: diag
  INTEGER       :: n3d, ndim, varid, clen
  INTEGER       :: nzp, kret
  INTEGER       :: dimlen(nf_max_var_dims)

  REAL,  ALLOCATABLE :: var2d(:,:)      ! generic 2D variable     
  REAL,  ALLOCATABLE :: var3d(:,:,:)    ! generic 3D variable 

  INTEGER, PARAMETER :: nvar  = 11      ! number of variables (and files)
  INTEGER, PARAMETER :: nsfc  = 5       ! number of surface variables
  INTEGER, PARAMETER :: kunit = 50      ! output unit for ARL packed data
  INTEGER, PARAMETER :: igmax = 192     ! number of gaussian grid longitudes
  INTEGER, PARAMETER :: jgmax = 94      ! number of gaussian grid latitudes
  INTEGER, PARAMETER :: lonpt = 144     ! number of longitude points
  INTEGER, PARAMETER :: latpt = 73      ! number of latitude points

  INTEGER(2)         :: word
  CHARACTER(2)       :: short

  LOGICAL            :: global          ! flag to output global latlon grid
  LOGICAL            :: little          ! lttile endian flag
  INTEGER            :: klvls           ! number of levels to output
  INTEGER(8)         :: ctime           ! netCDF time indicator
  REAL               :: gridkm          ! output grid size in km
  INTEGER            :: nxp, nyp        ! number of pts in output grid
  INTEGER            :: nxy, lrec       ! product of pts and record length
  REAL               :: clat, clon      ! center position of output grid
  LOGICAL            :: ftest           ! file exist test result
  CHARACTER(80)      :: fname           ! file name holder
  INTEGER            :: nz              ! maximum number of levels
  INTEGER            :: kv              ! variable with max numb of levels
  INTEGER            :: i1, j1          ! lower left index of input grid
  INTEGER            :: n, k, m, lev    ! dummy indicies
  INTEGER            :: iy,im,id,ih     ! current obs time
  INTEGER            :: ic, np          ! hours between obs, skip records
  INTEGER            :: julh            ! hours since start of year
  INTEGER            :: year, month     ! processing month
  INTEGER            :: klen            ! dummy byte counter
  INTEGER            :: krec            ! output record counter
  REAL               :: clat1, clon1    ! corner of input grid (1,1)
  REAL               :: dlat,  dlon     ! increment between points
  INTEGER            :: nlat,  nlon     ! number of input grid points

! gaussian and lat-lon grid arrays
  REAL           :: glat(jgmax),coa(jgmax),sia(jgmax),gw(jgmax)
  INTEGER(2)     :: mdata (igmax,jgmax) ! gaussian packed array
  REAL           :: gdata (igmax,jgmax) ! gaussian real array
  REAL           :: gxp   (igmax)       ! gaussian x points
  REAL           :: gyp   (jgmax)       ! gaussian y points
  REAL           :: ldata (lonpt,latpt) ! lat-lon interpolated
  REAL           :: lxp   (lonpt)       ! lat points
  REAL           :: lyp   (latpt)       ! lon points

! file dependent variables
  REAL           :: level  (20,nvar)    ! level values
  CHARACTER(8)   :: varb   (nvar)       ! data variable
  INTEGER        :: handle (nvar)       ! unit number
  INTEGER        :: kdate  (3,nvar)     ! starting year month day
  INTEGER        :: kdim   (4,nvar)     ! number in x,y,z,t
  CHARACTER(4)   :: vchar  (nvar)       ! ARL equivalents
  REAL           :: cnvrt  (nvar)       ! ARL unit conversion factors

! conformal grid variables
  REAL, ALLOCATABLE :: tlat (:,:)       ! lat position of each point
  REAL, ALLOCATABLE :: tlon (:,:)       ! lon position of each point
  REAL, ALLOCATABLE :: xvar (:,:)       ! dummy variable #1
  REAL, ALLOCATABLE :: yvar (:,:)       ! dummy variable #2

  CHARACTER(1), ALLOCATABLE :: cvar(:)  ! packed output array

  REAL,         ALLOCATABLE :: dtmp(:)  ! 1D array

  LOGICAL        :: typ3d  (nvar)       ! 3D input file

!---------------------------------------------------------
! 5 surface variables followed by 6 3D variables

! CDC netCDF variables as expressed in file name
  DATA varb/'pres&','air&','uwnd&','vwnd&','prate&',              &
            'hgt&', 'air&','uwnd&','vwnd&','omega&','rhum&'/

! ARL packed equivalent names and unit conversion factors
! precip kg/m/s to m over a 6h accumulation 0.001 * 6 * 3600.0
  DATA vchar/'PRSS','T02M','U10M','V10M','TPP6',                  &
             'HGTS','TEMP','UWND','VWND','WWND','RELH'/
  DATA cnvrt/ 0.01 , 1.0  , 1.0  , 1.0  , 21.6,                   &
               1.0 , 1.0  , 1.0  , 1.0  , 0.01 , 1.0  /

  COMMON /endian/ little

!------------------------------------------------------------------------------
! subroutine interfaces

  INTERFACE   

  SUBROUTINE getdim (diag,ncid,ntp,nxp,nyp,nzp,ftp3d)
  IMPLICIT NONE
  LOGICAL,       INTENT(IN)  :: diag
  INTEGER,       INTENT(IN)  :: ncid
  INTEGER,       INTENT(OUT) :: ntp
  INTEGER,       INTENT(OUT) :: nxp
  INTEGER,       INTENT(OUT) :: nyp
  INTEGER,       INTENT(OUT) :: nzp
  LOGICAL,       INTENT(IN)  :: ftp3d
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

  SUBROUTINE TMCOVT(inmn,iy,im,id,ih)
  IMPLICIT NONE
  INTEGER, INTENT(in)   :: inmn
  INTEGER, INTENT(out)  :: iy,im,id,ih
  END SUBROUTINE tmcovt

  SUBROUTINE MKGRID(NX,NY,TLAT,TLON)
  IMPLICIT NONE
  INTEGER, INTENT(IN)  :: NX,NY
  REAL,    INTENT(OUT) :: TLAT(:,:)
  REAL,    INTENT(OUT) :: TLON(:,:)
  END SUBROUTINE mkgrid

  SUBROUTINE SETNDX(GLOBAL,FTEST,KLVLS,GRIDKM,NXP,NYP,CLAT,CLON, &
                    NLAT,NLON,KUNIT)
  IMPLICIT NONE
  LOGICAL, INTENT(OUT)   :: GLOBAL
  LOGICAL, INTENT(OUT)   :: FTEST
  INTEGER, INTENT(INOUT) :: KLVLS
  REAL,    INTENT(OUT)   :: GRIDKM
  INTEGER, INTENT(OUT)   :: NXP
  INTEGER, INTENT(OUT)   :: NYP
  REAL,    INTENT(OUT)   :: CLAT
  REAL,    INTENT(OUT)   :: CLON
  INTEGER, INTENT(IN)    :: NLAT
  INTEGER, INTENT(IN)    :: NLON
  INTEGER, INTENT(IN)    :: KUNIT
  END SUBROUTINE setndx

  SUBROUTINE MAKNDX (nsfc,nvar,klvls,vchar,level,gridkm,nxp,nyp,clat,clon, &
                     global,clat1,clon1,dlat,dlon,nlat,nlon,kunit)
  IMPLICIT NONE
  INTEGER,      INTENT(IN)   :: nsfc, nvar
  INTEGER,      INTENT(IN)   :: klvls
  CHARACTER(4), INTENT(IN)   :: vchar (nvar)
  REAL,         INTENT(IN)   :: level (20,nvar)
  REAL,         INTENT(IN)   :: gridkm
  INTEGER,      INTENT(IN)   :: nxp
  INTEGER,      INTENT(IN)   :: nyp
  REAL,         INTENT(IN)   :: clat
  REAL,         INTENT(IN)   :: clon
  LOGICAL,      INTENT(IN)   :: global
  REAL,         INTENT(IN)   :: clat1
  REAL,         INTENT(IN)   :: clon1
  REAL,         INTENT(IN)   :: dlat
  REAL,         INTENT(IN)   :: dlon
  INTEGER,      INTENT(IN)   :: nlon
  INTEGER,      INTENT(IN)   :: nlat
  INTEGER,      INTENT(IN)   :: kunit
  END SUBROUTINE makndx

  SUBROUTINE REGRID(GLOBAL,I1,J1,V1,NX1,NY1,V2,NX2,NY2,TLAT,TLON,  &
                    CLAT1,CLON1,DLON,DLAT)
  IMPLICIT NONE
  LOGICAL, INTENT(IN)    :: GLOBAL
  INTEGER, INTENT(IN)    :: I1, J1
  INTEGER, INTENT(IN)    :: NX1, NY1
  INTEGER, INTENT(IN)    :: NX2, NY2
  REAL,    INTENT(INOUT) :: V1(NX1,NY1)
  REAL,    INTENT(OUT)   :: V2(NX2,NY2)
  REAL ,   INTENT(IN)    :: TLAT(NX2,NY2)
  REAL ,   INTENT(IN)    :: TLON(NX2,NY2)
  REAL,    INTENT(IN)    :: CLAT1, CLON1
  REAL,    INTENT(IN)    :: DLAT,  DLON
  END SUBROUTINE regrid

  END INTERFACE

!------------------------------------------------------------------------------
! define the command line options with default values

  DIAG = .false.  ! diagnostic output

!---------------------------------------------------------
! select the year and month for processing

  WRITE(*,*)'Enter four digit year (yyyy): '
  READ(*,*)year
  WRITE(*,*)'Enter two digit month (mm): '
  READ(*,*)month

!---------------------------------------------------------
! diagnostic output file

  if (month.lt.10) then
     write(logfile,"(a8,i4,a1,i1)") 'MESSAGE.',year,'0',month
  else
     write(logfile,"(a8,i4,i2)") 'MESSAGE.',year,month
  endif
  print *,'Log file: ',logfile

  OPEN(KUNIT+1,FILE=logfile)

  write(kunit+1,*) 'Log file: ',logfile

! determine if this is running on a big- or little-endian system
  short(1:1)=char(0)
  short(2:2)=char(1)
  READ(short,'(a2)')word
  IF(word.EQ.1)THEN
     little=.FALSE.
     WRITE(kunit+1,'(A,I4)')'Platform is big endian: ',word
  ELSE
!    word = 256
     little=.TRUE.
     WRITE(kunit+1,'(A,I4)')'Platform is little endian: ',word
  END IF
  WRITE(kunit+1,'(A)')' '

!---------------------------------------------------------
! construct and open files based upon the year selected

  typ3d=.false.

  DO K=1,nvar
     klen=INDEX(varb(k),'&')-1
     IF(K.EQ.1)THEN
!       surface pressure
        WRITE(fname,'(2a,i4,a3)')varb(k)(1:klen),'.sfc.',year,'.nc'
     ELSEIF(K.GE.2.AND.K.LE.4)THEN
!       first sigma level variables
        WRITE(fname,'(2a,i4,a3)')varb(k)(1:klen),'.sig995.',year,'.nc'
     ELSEIF(K.EQ.5)THEN
!       gaussian flux variables - precipitation
        WRITE(fname,'(2a,i4,a3)')varb(k)(1:klen),'.sfc.gauss.',year,'.nc'
     ELSE
!       remaining 3D variables
        WRITE(fname,'(2a,i4,a3)')varb(k)(1:klen),'.',year,'.nc'
        typ3d(k)=.true.
     END IF

     kret = NF_OPEN(fname, nf_nowrite, handle(k))
     n=INDEX(fname,' ')-1
     IF(kret.NE.nf_noerr) THEN
        WRITE(*,*) fname(:n),' : ',NF_STRERROR(kret)
        STOP
     END IF
     WRITE(KUNIT+1,*)'Opened NetCDF input file on unit: ',handle(k),fname

     ! get dimensions for grid definitions
      CALL getdim (diag,handle(k),kdim(4,k),kdim(1,k),kdim(2,k),kdim(3,k),typ3d(k))
      print *,varb(k),handle(k),kdim(:,k),typ3d(k)

  END DO

!---------------------------------------------------------
! construct level information

  WRITE(KUNIT+1,'(A)')' '
  WRITE(KUNIT+1,'(A)')'Vertical data -'

  nz=0
  level=0.0

  DO k=1,nvar
!    find variable with max # levels
     IF(kdim(3,k).GT.nz)THEN
        nz=kdim(3,k)
        kv=k
     END IF

     IF(kdim(3,k).gt.1)THEN
       label='level'
       ALLOCATE (dtmp(kdim(3,k)),STAT=kret)
       CALL setvar(diag,handle(k),label,varid,n3d,ndim,dimlen,range)
       CALL get2dv(diag,handle(k),varid,kdim(3,k),dtmp)

       do m=1,kdim(3,k)
          level(m,nvar)=dtmp(m)
       end do

      !print *,handle(k),level(:,nvar)
       DEALLOCATE(dtmp)
     END IF

  END DO

  k=nz
  DO WHILE (k.GT.0)
     write(KUNIT+1,'(i4,a,10i6)')k,' - ',(int(level(k,m)),m=nsfc+1,nvar)
     k=k-1
  END DO

  print *,'nz=',nz

!------------------------------------------------------------------------------
! open the NetCDF input file and obtain basic information to design output

  print *,'total time step in the input file=',kdim(4,kv)

! get start time of each file
  DO k=1,nvar

     label='time'
     ALLOCATE (dtmp(kdim(4,k)),STAT=kret)
     CALL setvar(diag,handle(k),label,varid,n3d,ndim,dimlen,range)
     CALL get2dv(diag,handle(k),varid,kdim(4,k),dtmp)

     CALL tmcovt(int(dtmp(1)),kdate(1,k),kdate(2,k),kdate(3,k),ih)

     print *,'file sttime=',dtmp(1),kdate(:,k),ih
     DEALLOCATE(dtmp)

  END DO

!---------------------------------------------------------
! set remaining input grid parameters

  nlon=kdim(1,kv)
  nlat=kdim(2,kv)
  WRITE(KUNIT+1,*)
  WRITE(KUNIT+1,*)'Input grid: ',nlon,nlat
! initialize max level selection variable
  klvls=nzp+1

! corner point and grid spacing of CDC global data
! note that CDC data starts at +90 but the data array is
! transformed from south to north in subroutine regrid
  clat1=-90.0
  clon1=0.0
  dlat=2.5
  dlon=2.5

! initialize gaussian grid to lat-lon conversion
  CALL SETXY(lxp,lonpt,lyp,latpt,0.0,dlon,-90.0,dlat)
  CALL SETXY(gxp,igmax,gyp,jgmax,0.0,(360.0/FLOAT(igmax)),0.,0.)
  CALL GAUSSL(sia,coa,glat,gw,jgmax)
  gyp = glat * 180.0/3.14159265

!---------------------------------------------------------
! configure the ARL packed data

! read parameters for the output grid
  CALL SETNDX(global,ftest,klvls,gridkm,nxp,nyp,clat,clon,  &
              nlat,nlon,kunit)

  print *,'global=',global,ftest,klvls
  print *,'nxp=',nxp,nyp,clat,clon

! create the configuration file if it doesn't exist
  IF(.NOT.FTEST) CALL MAKNDX                                &
     (nsfc,nvar,klvls,vchar,level,gridkm,nxp,nyp,clat,clon, &
      global,clat1,clon1,dlat,dlon,nlat,nlon,kunit)

! configure the packing routines
  KREC=1
  CALL PAKSET(KUNIT,'METDATA.CFG',KREC,NXP,NYP,KLVLS)

! open output file (RS - reanalysis sigma; RP - reanalysis pressure)
  NXY =NXP*NYP
  LREC=NXY+50
  IF(GLOBAL)THEN
     WRITE(FNAME,'(a2,i4,i2.2,a4)')'RP',year,month,'.gbl'
  ELSE
     WRITE(FNAME,'(a2,i4,i2.2,a4)')'RP',year,month,'.arl'
  END IF
  OPEN(KUNIT,FILE=FNAME,RECL=LREC,ACCESS='DIRECT',   &
       FORM='UNFORMATTED')
  ALLOCATE (cvar(nxp*nyp))

! set up the grid system
  ALLOCATE (tlat(nxp,nyp),tlon(nxp,nyp))
  ALLOCATE (xvar(nxp,nyp),yvar(nxp,nyp))

! determine how data will be remapped
  IF(GLOBAL)THEN
!    lower left corner of global grid
     i1=1
     j1=1
     IF(nxp.lt.nlon)i1=1+(clon-clon1)/dlon
     IF(nyp.lt.nlat)j1=1+(clat-clat1)/dlat
     WRITE(KUNIT+1,*)'Dim of latlon output grid: ',nxp,nyp
     WRITE(KUNIT+1,*)'I,J of lower left corner : ',i1,j1
  ELSE
     CALL MKGRID(nxp,nyp,tlat,tlon)
     WRITE(KUNIT+1,*)'Dim of conformal output grid: ',nxp,nyp
  END IF

!---------------------------------------------------------
! construct time loop

! assume that at least one file has non-zero values for base date
! problems with missing base date started with 2005
  iy=MAXVAL(kdate(1,:))
  im=MAXVAL(kdate(2,:))
  id=MAXVAL(kdate(3,:))

  IF(iy.EQ.0.AND.im.EQ.0.AND.id.EQ.0)THEN
     WRITE(*,*)'All input files missing base date ... assume 1 Jan!'
     iy=year
     im=1
     id=1
  END IF
  ih=0
  ic=6

! compute number of time periods to skip
  CALL NCJULH (iy,month,id,ih,0,julh)
  NP=(julh/ic)+1

  WRITE(KUNIT+1,*)
  tloop : DO n=1,kdim(4,kv)

     vloop : DO k=1,nvar

        label='time'
        ALLOCATE (dtmp(kdim(4,k)),STAT=kret)
        CALL setvar(diag,handle(k),label,varid,n3d,ndim,dimlen,range)
        CALL get2dv(diag,handle(k),varid,kdim(4,k),dtmp)

        CALL tmcovt(int(dtmp(np)),iy,im,id,ih)

        if (im .ne. month) goto 900

        klen=INDEX(varb(k),'&')-1
        label=varb(k)(1:klen)

        WRITE(KUNIT+1,*)'Processing: ',varb(k)(1:klen),np,iy,im,id,ih
        print *,'Processing: ',varb(k)(1:klen),np,iy,im,id,ih

        if (typ3d(k)) then 

           ALLOCATE (var3d(kdim(1,k),kdim(2,k),kdim(3,k)),STAT=kret)
           CALL setvar (diag,handle(k),label,varid,n3d,ndim,dimlen,range)
           CALL get4dv (diag,handle(k),varid,kdim(1,k),kdim(2,k),kdim(3,k),np,var3d)

          !print *,trim(label),np,var3d(51,51,:)
           write(KUNIT+1,*)'   ',varb(k)(1:klen),' - ',var3d(51,51,:)

        else

           ALLOCATE (var2d(kdim(1,k),kdim(2,k)),STAT=kret)
           CALL setvar(diag,handle(k),label,varid,n3d,ndim,dimlen,range)
           CALL get3dv(diag,handle(k),varid,kdim(1,k),kdim(2,k),kdim(3,k),np,var2d)

          !print *,trim(label),np,var2d(1,1),var2d(51,51)
           write(KUNIT+1,*)'   ',varb(k)(1:klen),' - ',var2d(1,1),var2d(51,51)

        end if

!       write one record for each data level
        mloop : DO m=1,kdim(3,k)

!          ARL packed data level count includes surface
           lev=m
           if(kdim(3,k).gt.1)lev=m+1
           if(lev.gt.klvls) CYCLE mloop

          !if (typ3d(k)) print *,'lev=',lev

!          special processing for Gaussian precipitation field
           IF(vchar(k).EQ.'TPP6')THEN
              print *,igmax,jgmax,lonpt,latpt
              CALL INTP2D(var2d,igmax,jgmax,gxp,gyp,ldata,lonpt,latpt,lxp,lyp,0)
!             convert to conformal grid
              CALL REGRID(global,i1,j1,ldata(1,1),nlon,nlat,yvar,nxp,nyp,      &
                          tlat,tlon,clat1,clon1,dlon,dlat)
           ELSE
!             convert to conformal grid
              if (typ3d(k)) then 
                CALL REGRID(global,i1,j1,var3d(1,1,m),nlon,nlat,yvar,nxp,nyp,    &
                            tlat,tlon,clat1,clon1,dlon,dlat)
              else
                CALL REGRID(global,i1,j1,var2d(1,1),nlon,nlat,yvar,nxp,nyp,    &
                            tlat,tlon,clat1,clon1,dlon,dlat)
              endif
           END IF

!          convert to ARL standard units
           yvar = yvar * cnvrt(k)

           CALL PAKREC(kunit,yvar,cvar,nxp,nyp,nxy,vchar(k),                &
                       mod(iy,100),im,id,ih,0,0,lev,0)

        END DO mloop

        if (allocated(var2d)) deallocate(var2d)
        if (allocated(var3d)) deallocate(var3d)

     END DO vloop

!    write index record to output file
     CALL PAKNDX(KUNIT)
     WRITE(KUNIT+1,*)'Index record written'
     WRITE(KUNIT+1,*)

     np = np+1

  END DO tloop

!---------------------------------------------------------
! close all open files

  900 CONTINUE

  DO K=1,nvar
     kret = NF_CLOSE(handle(k))
     WRITE(KUNIT+1,*)'Closing input files ',handle(k)
     print *,'closing...',handle(k),kret
  END DO

  CLOSE(KUNIT)
  CLOSE(KUNIT+1)

END PROGRAM cdf2arl
