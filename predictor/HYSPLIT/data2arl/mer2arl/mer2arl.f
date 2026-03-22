PROGRAM mer2arl

!---------------------------------------------------------------------------
! MERRA2 data TO ARL FORMAT
!---------------------------------------------------------------------------
! Last revised: 10 May 2017 (FN)  - initial version 
!                                   input files in NetCDF4 format
!               12 Jul 2017 (FN)  - switch longitude to 0-360 system
!               19 Jul 2017 (FN)  - add prefix to MERRA2 filename
!               29 Jan 2021 (FN)  - fix hybrid coordinate (see METLAYS.INC)
!                                   add suffix to MERRA2 filename
!                                   modify script for MERRA2 download
!
! MERRA2 download website:
!   https://goldsmr4.gesdisc.eosdis.nasa.gov/data/MERRA2
!   https://goldsmr5.gesdisc.eosdis.nasa.gov/data/MERRA2
!
! MERRA2 files:
!   MERRA2_100.tavg3_3d_asm_Nv.19830925.nc4 => f3dasm (file#1)
!   ----------                          ---
!     prefix                            suffix
!   MERRA2_100.tavg1_2d_flx_Nx.19830925.nc4 => f2dflx (file#2)
!   MERRA2_100.tavg1_2d_slv_Nx.19830925.nc4 => f2dslv (file#3)
!   MERRA2_100.tavg1_2d_int_Nx.19830925.nc4 => f2dint (file#4)
!
!---------------------------------------------------------------------------

  IMPLICIT NONE

  include 'netcdf.inc'
  include 'METLAYS.INC' 

  REAL          :: range(2)
  CHARACTER(256):: f3dasm, f2dflx, f2dslv, f2dint
  CHARACTER(256):: label
  CHARACTER(256):: prefix,suffix
  CHARACTER(256):: logfile, varfile, grdfile, arlfile
  LOGICAL       :: diag
  INTEGER       :: n3d, ndim, varid, clen
  INTEGER       :: kret, kinit
  INTEGER       :: dimlen(nf_max_var_dims)

  REAL,  ALLOCATABLE :: var2d(:,:)      ! generic 2D variable     
  REAL,  ALLOCATABLE :: var3d(:,:,:)    ! generic 3D variable 
  REAL,  ALLOCATABLE :: base(:,:)       ! temporary 2D write variable

  INTEGER, PARAMETER :: kunit = 50      ! output unit for ARL packed data
  INTEGER, PARAMETER :: lunit = 60      ! log file unit

  INTEGER, PARAMETER :: nfile = 4       ! number of files

  INTEGER(2)         :: word
  CHARACTER(2)       :: short
  CHARACTER(8)       :: cdate                ! process date for file name

  LOGICAL            :: little               ! lttile endian flag
  INTEGER            :: nxp, nyp, nzp        ! number of pts in output grid
  INTEGER            :: ntp                  ! number of time step
  INTEGER            :: nxy, lrec            ! product of pts and record length
  LOGICAL            :: ftest                ! file exist test result
  INTEGER            :: n, k, m, ii          ! dummy indicies
  INTEGER            :: idx                  ! index for x grid points in 0-360 system
  INTEGER            :: iy,im,id,ih,imn, fh  ! current obs time
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
  CHARACTER(12)      :: mer2dv(max2dv)     ! MERRA2 variable names
  CHARACTER(12)      :: mer3dv(max3dv)
  CHARACTER(4)       :: arl2dv(max2dv)     ! ARL variable names
  CHARACTER(4)       :: arl3dv(max3dv)

  REAL               :: a,b

  CHARACTER(1), ALLOCATABLE :: cvar(:)  ! packed output array

  REAL,         ALLOCATABLE :: tmflx(:), tmasm(:), tmslv(:), tmint(:)  ! time steps for 2d and 3d variables

  INTEGER        :: ncid, ixx, iyy
  INTEGER        :: iflx, islv, iint, tidx   
  LOGICAL        :: typ3d

!---------------------------------------------------------

  NAMELIST/SETUP/ num3dv,mer3dv,cnv3dv,arl3dv,num2dv,mer2dv,cnv2dv,arl2dv

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

  SUBROUTINE cfgrec (kunit,arlcfg_file,nxp,nyp,nzp,level,num2dv,arl2dv,num3dv,arl3dv)
  IMPLICIT NONE
  INTEGER,      INTENT(IN)   :: kunit
  CHARACTER(80),INTENT(IN)   :: arlcfg_file
  INTEGER,      INTENT(IN)   :: nxp,nyp,nzp
  REAL,         INTENT(IN)   :: level(:)
  INTEGER,      INTENT(IN)   :: num2dv
  CHARACTER(4), INTENT(IN)   :: arl2dv(:)
  INTEGER,      INTENT(IN)   :: num3dv
  CHARACTER(4), INTENT(IN)   :: arl3dv(:)
  END SUBROUTINE cfgrec

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
  WRITE(*,*)'Enter two digit day (dd): '
  READ(*,*)day
  WRITE(*,*)'Enter prefix for MERRA2 filename (ex:MERRA2_400): '
  READ(*,*)prefix
  WRITE(*,*)'Enter suffix for MERRA2 filename (ex:.nc4 or SUB.nc4): '
  READ(*,*)suffix

  CALL TMINIT
  CALL TM2MIN(year,month,day,0,0,mc0)
  print *,mc0

  CALL TM2DAY(mc0,iy,im,id,ih,imn)
  print *,'processing date: ',iy,im,id,ih,imn

  WRITE(cdate,'(i4,i2,i2)') year,month,day
  IF (im .LT. 10) cdate(5:5) = '0'
  IF (id .LT. 10) cdate(7:7) = '0'

  print *,cdate

!---------------------------------------------------------
! diagnostic output file

  logfile = 'MESSAGE'

  OPEN(LUNIT,FILE=logfile)

  WRITE(LUNIT,*) 'Log file: ',trim(logfile)

! determine if this is running on a big- or little-endian system
  short(1:1)=char(0)
  short(2:2)=char(1)
  READ(short,'(a2)')word
  IF(word.EQ.1)THEN
     little=.FALSE.
     WRITE(LUNIT,'(A,I4)')'Platform is big endian: ',word
  ELSE
!    word = 256
     little=.TRUE.
     WRITE(LUNIT,'(A,I4)')'Platform is little endian: ',word
  END IF
  WRITE(LUNIT,'(A)')' '

!---------------------------------------------------------
! hybrib sigma-pressure vertical cooridnate 
! follow gfs0p25 converter (global2arl.f)
! convert a and b parameters to 'sigma' for all levels (a is interger part; b is decimal)
!
! Hybrid Ap and Bp parameter are listed in METLAY.INC
! http://wiki.seas.harvard.edu/geos-chem/index.php/GEOS-Chem_vertical_grids#72-layer_vertical_grid

  WRITE(LUNIT,'(I3,A,1X,A)') mlvl,' vertical levels in file METLAY.INC'

  DO K=1,mlvl
     A=nint(ap(k))
     B=bp(k)
   ! for 1st level a=0 b=1.0
   ! force it to be a sigma value instead of a pressure value
     IF(K.EQ.1.AND.A.EQ.0.0.AND.B.EQ.1.0) B=0.9999

   ! 5 significant digits if a(hPa) > 1.0, else 4
     IF(A.GT.100.)THEN
        B=NINT(B*100.)/100.0
     ELSEIF(A.GT.10.)THEN
        B=NINT(B*1000.)/1000.0
     ELSEIF(A.GT.1.)THEN
        B=NINT(B*10000.)/10000.0
     ELSE
        B=NINT(B*10000.)/10000.0
     END IF

     SIGL(K)=A+B
     WRITE(LUNIT,'(A,1X,I2,1X,F9.4)')'k,sigl:',k,sigl(k)
  END DO
  CLOSE(20)

!---------------------------------------------------------
! Open and read the namelist that defines the MERRA2 variables that will be
! extracted and converted to the HYSPLIT compatible ARL format. If it does not
! exist, the namelist will be created. Existing namelists may be user edited to
! delete or define new variables.

  varfile = 'MERDATA.CFG'

  INQUIRE(FILE=TRIM(varfile), EXIST=ftest)
  IF(.NOT.ftest)THEN
     CALL cfgvar(varfile)
  ELSE
     WRITE(LUNIT,*)'Using an existing decoding configuration: ',TRIM(varfile)
  END IF

  OPEN (10,FILE=TRIM(varfile))
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

!---------------------------------------------------------
! construct and open files based upon the year selected

     !time-average 3D data in the native vertical grid, 3-hourly
     f3dasm = trim(prefix)//'.tavg3_3d_asm_Nv.'//cdate//'.'//trim(suffix)
     typ3d  =.true.
     k = 1                  ! file #1
     kret  = NF_OPEN(f3dasm, nf_nowrite, handle(k))
     n=INDEX(f3dasm,' ')-1
     IF(kret.NE.nf_noerr) THEN
        print *,f3dasm(:n),' : ',NF_STRERROR(kret)
        STOP
     END IF
     WRITE(LUNIT,*)'Opened file on unit: ',handle(k),trim(f3dasm)

     CALL getdim (diag,handle(k),kdim(4,k),kdim(1,k),kdim(2,k),kdim(3,k),typ3d)
     WRITE(LUNIT,*)'file #',k,kdim(:,k)

     !time-average 2D flux data, hourly
     f2dflx = trim(prefix)//'.tavg1_2d_flx_Nx.'//cdate//'.'//trim(suffix)
     typ3d  =.false.
     k = 2                  ! file #2
     kret  = NF_OPEN(f2dflx, nf_nowrite, handle(k))
     n=INDEX(f2dflx,' ')-1
     IF(kret.NE.nf_noerr) THEN
        print *,f2dflx(:n),' : ',NF_STRERROR(kret)
        STOP
     END IF
     WRITE(LUNIT,*)'Opened file on unit: ',handle(k),trim(f2dflx)

     CALL getdim (diag,handle(k),kdim(4,k),kdim(1,k),kdim(2,k),kdim(3,k),typ3d)
     WRITE(LUNIT,*)'file #',k,kdim(:,k)

     !time-average t2m, u10m and v10m, hourly
     f2dslv = trim(prefix)//'.tavg1_2d_slv_Nx.'//cdate//'.'//trim(suffix)
     typ3d  =.false.
     k = 3                  ! file #3
     kret  = NF_OPEN(f2dslv, nf_nowrite, handle(k))
     n=INDEX(f2dslv,' ')-1
     IF(kret.NE.nf_noerr) THEN
        print *,f2dslv(:n),' : ',NF_STRERROR(kret)
        STOP
     END IF
     WRITE(LUNIT,*)'Opened file on unit: ',handle(k),trim(f2dslv)

     CALL getdim (diag,handle(k),kdim(4,k),kdim(1,k),kdim(2,k),kdim(3,k),typ3d)
     WRITE(LUNIT,*)'file #',k,kdim(:,k)

     !time-average shortwave flux, hourly
     f2dint = trim(prefix)//'.tavg1_2d_int_Nx.'//cdate//'.'//trim(suffix)
     typ3d  =.false.
     k = 4                  ! file #4
     kret  = NF_OPEN(f2dint, nf_nowrite, handle(k))
     n=INDEX(f2dint,' ')-1
     IF(kret.NE.nf_noerr) THEN
        print *,f2dint(:n),' : ',NF_STRERROR(kret)
        STOP
     END IF
     WRITE(LUNIT,*)'Opened file on unit: ',handle(k),trim(f2dint)

     CALL getdim (diag,handle(k),kdim(4,k),kdim(1,k),kdim(2,k),kdim(3,k),typ3d)
     WRITE(LUNIT,*)'file #',k,kdim(:,k)

!------------------------------------------------------------------------------
! memory allocation 2D and 3D variables

      nxp = kdim(1,1)   ! from f3dasm (file #1)
      nyp = kdim(2,1)
      nzp = kdim(3,1)
      ntp = kdim(4,1)   ! 3-hourly data for 3d variable file
      WRITE(LUNIT,*) 'nxp=',nxp,nyp,nzp,ntp

      ftest=.FALSE.

      ALLOCATE (var2d(nxp,nyp),base(nxp,nyp),STAT=kret)
      IF(kret.NE.0)ftest=.TRUE.
      ALLOCATE (var3d(nxp,nyp,nzp),STAT=kret)
      IF(kret.NE.0)ftest=.TRUE.

      IF(ftest)THEN
         print *,'Memory allocation error for data variables'
         STOP
      END IF

!------------------------------------------------------------------------------
! check variables

! check to insure all defined 2D variables exist in NetCDF file
  DO n=1,num2dv
     label=TRIM(mer2dv(n))

     IF(TRIM(label) .eq. 'T2M'  .or. TRIM(label) .eq. 'U10M' .or. & 
        TRIM(label) .eq. 'V10M' .or. TRIM(label) .eq. 'PS' ) THEN
        ncid=handle(3)  ! 2d data file for t2m, u10m and v10m
     ELSE IF(TRIM(label) .eq. 'SWNETSRF') THEN
        ncid=handle(4)  ! 2d data file for shortwave flux
     ELSE IF(TRIM(label) .eq. 'HGT') THEN
        ncid=handle(1)
        label='PHIS'
     ELSE
        ncid=handle(2)  ! 2d flux file
     ENDIF

     CALL setvar(diag,ncid,label,varid,n3d,ndim,dimlen,range)
     IF(varid.EQ.0) THEN
        WRITE(LUNIT,*)'Variable not found: ',TRIM(label)
        ftest=.TRUE.
     END IF
  END DO

! check to insure all defined 3D variables exist in NetCDF file
  ncid=handle(1)        ! 3d file
  DO n=1,num3dv
     label=TRIM(mer3dv(n))
     CALL setvar(diag,ncid,label,varid,n3d,ndim,dimlen,range)
     IF(varid.EQ.0) THEN
        WRITE(LUNIT,*)'Variable not found: ',TRIM(label)
        ftest=.TRUE.
     END IF
  END DO

! if any variables missing then stop
  IF(ftest)THEN
     print *,'Some defined variables not found in netcdf files'
     print *,'Edit ',TRIM(varfile),' and try again ...'
     STOP
  END IF

!---------------------------------------------------------
! get time steps of each file

     label='time'
     k=1
     ncid=handle(k)
     ALLOCATE (tmasm(kdim(4,k)),STAT=kret)
     CALL setvar(diag,ncid,label,varid,n3d,ndim,dimlen,range)
     CALL get2dv(diag,ncid,varid,kdim(4,k),tmasm)
    !WRITE(LUNIT,*) '3d time step=',tmasm

     k=2
     ncid=handle(k)
     ALLOCATE (tmflx(kdim(4,k)),STAT=kret)
     CALL setvar(diag,ncid,label,varid,n3d,ndim,dimlen,range)
     CALL get2dv(diag,ncid,varid,kdim(4,k),tmflx)
    !WRITE(LUNIT,*) '2d flx time step=',tmflx

     k=3
     ncid=handle(k)
     ALLOCATE (tmslv(kdim(4,k)),STAT=kret)
     CALL setvar(diag,ncid,label,varid,n3d,ndim,dimlen,range)
     CALL get2dv(diag,ncid,varid,kdim(4,k),tmslv)
    !WRITE(LUNIT,*) '2d slv time step=',tmslv

     k=4
     ncid=handle(k)
     ALLOCATE (tmint(kdim(4,k)),STAT=kret)
     CALL setvar(diag,ncid,label,varid,n3d,ndim,dimlen,range)
     CALL get2dv(diag,ncid,varid,kdim(4,k),tmint)
    !WRITE(LUNIT,*) '2d int time step=',tmint

!---------------------------------------------------------
! create the ARL packing information file

  grdfile = 'METGRID.CFG'
  arlfile = 'ARLDATA.BIN'

  INQUIRE(FILE=TRIM(grdfile), EXIST=ftest)
  IF(.NOT.ftest)THEN
     CALL cfgrec (kunit,grdfile,nxp,nyp,nzp,sigl(:),num2dv,arl2dv,num3dv,arl3dv)
  ELSE
     WRITE(LUNIT,*)'Using an existing encoding configuration: ',TRIM(grdfile)
  END IF

! initialize the packing routines
  KREC=1
  CALL PAKSET(kunit,TRIM(grdfile),krec,nxp,nyp,nzp)
  nzp=nzp-1
  WRITE(LUNIT,*) 'Set grid from pakset: ',nxp,nyp,nzp

! configfure the ARL output data set
  nxy=nxp*nyp
  ALLOCATE (cvar(nxy),STAT=kret)
  IF(kret.NE.0)THEN
     WRITE(*,*)'Memory allocation error for packed data array'
     STOP
  END IF
  OPEN(kunit,FILE=TRIM(arlfile),RECL=(50+nxy),ACCESS='DIRECT',FORM='UNFORMATTED')

!---------------------------------------------------------
! number of time periods loop

  WRITE(LUNIT,*) '==================='
  ixx=153-289+1+nxp
  iyy=301
  WRITE(LUNIT,*) 'check point:',ixx,iyy

  tloop : DO m=1,ntp                  ! loop through each time step

     mc3d = mc0+90+int(tmasm(m))      ! file #1 starting at 01:30

     CALL TM2DAY(mc3d,iy,im,id,ih,imn)
     WRITE(LUNIT,*) 'Processing: ',iy,im,id,ih,imn

     do k=1,kdim(4,2)                 ! hourly data for flux variables
        mc2d = mc0+30+int(tmflx(k))   ! file #2 starting at 00:30
        if (mc2d .eq. mc3d .or. mc2d .gt. mc3d) goto 800
     end do
  800 continue

     iflx=k                           ! index for variables in flux file
     WRITE(LUNIT,*) 'index flx file: ',iflx

     do k=1,kdim(4,3)                 ! hourly data for 2d variables
        mc2d = mc0+30+int(tmslv(k))   ! file #3 starting at 00:30
        if (mc2d .eq. mc3d .or. mc2d .gt. mc3d) goto 810
     end do
  810 continue

     islv=k                           ! index for variables in flux file
     WRITE(LUNIT,*) 'index slv file: ',islv

     iint=k                           ! index for variables in int file
     WRITE(LUNIT,*) 'index int file: ',iint

     fh = 0
     kinit = 1

     ! extract 2d variables
     DO n=1,num2dv

        label=TRIM(mer2dv(n))

        IF(TRIM(label) .eq. 'T2M'  .or. TRIM(label) .eq. 'U10M' .or. &
           TRIM(label) .eq. 'V10M' .or. TRIM(label) .eq. 'PS' ) THEN
           ncid=handle(3)  ! 2d data file for t2m, u10m and v10m
           tidx=islv
        ELSE IF(TRIM(label) .eq. 'SWNETSRF') THEN
           ncid=handle(4)  ! 2d data for shortwave flux
           tidx=iint
        ELSE IF(TRIM(label) .eq. 'HGT') THEN
           ncid=handle(1)
           tidx=m
           label='PHIS'
        ELSE
           ncid=handle(2)  ! 2d flux file
           tidx=iflx
        ENDIF

        CALL setvar(diag,ncid,label,varid,n3d,ndim,dimlen,range)
        CALL get3dv(diag,ncid,varid,nxp,nyp,nzp,tidx,var2d)

        IF(TRIM(label) .eq. 'PRECTOTCORR') THEN ! special treatment to get 3-hrly precip rate
          base=var2d                            ! temporary storing the data
         !print *,tidx,var2d(ixx,iyy)
          CALL get3dv(diag,ncid,varid,nxp,nyp,nzp,tidx-1,var2d)
          base=base+var2d                       ! adding data at previous hour
         !print *,tidx-1,var2d(ixx,iyy)

          CALL get3dv(diag,ncid,varid,nxp,nyp,nzp,tidx+1,var2d)
          base=base+var2d                       ! adding data at next hour
         !print *,tidx+1,var2d(ixx,iyy)

          var2d=base/3.0
        ENDIF

       !base=cnv2dv(n)*var2d
        do ii=1,nxp 
           idx=ii-((nxp/2)+1)+1
           if (idx .le. 0) idx=nxp+idx
           base(ii,:)=cnv2dv(n)*var2d(idx,:)     ! switch to 0-360 system, ii=289 at a grid point longitude=0
          !if (TRIM(label) .eq. 'PHIS') WRITE(LUNIT,*) ii,idx
        end do
        CALL PAKREC(kunit,base,cvar,nxp,nyp,nxy,arl2dv(n),iy,im,id,ih,imn,fh,1,kinit)
        WRITE(LUNIT,*) n,TRIM(label),' ',TRIM(arl2dv(n)),base(ixx,iyy),var2d(ixx,iyy)

     END DO

     ! extract 3d variables
     DO n=1, num3dv

        label=TRIM(mer3dv(n))
        ncid=handle(1)     ! 3d data file

        CALL setvar (diag,ncid,label,varid,n3d,ndim,dimlen,range)
        CALL get4dv (diag,ncid,varid,nxp,nyp,nzp,m,var3d)

        kloop : DO k=1,nzp        ! loop through each data level
          !base(:,:)=cnv3dv(n)*var3d(:,:,nzp-k+1)
           do ii=1,nxp
              idx=ii-289+1
              if (idx .le. 0) idx=nxp+idx
              base(ii,:)=cnv3dv(n)*var3d(idx,:,nzp-k+1)
           end do
           CALL PAKREC(kunit,base,cvar,nxp,nyp,nxy,arl3dv(n),iy,im,id,ih,imn,fh,k+1,kinit)
          !if (label .eq. 'T') WRITE(LUNIT,*) n,nzp-k+1,TRIM(label),' ',TRIM(arl3dv(n)),base(ixx,iyy)
           if (k .eq. 1) WRITE(LUNIT,*) n,k,TRIM(label),' ',TRIM(arl3dv(n)),base(ixx,iyy),var3d(ixx,iyy,nzp-k+1)
        END DO kloop

     END DO

     ! write index record to output file
     CALL PAKNDX(kunit)

     ! time period loop
     WRITE(*,*)'Completed: ',iy,im,id,ih,imn

  END DO tloop

!---------------------------------------------------------
! close all open files

  900 CONTINUE

  WRITE(LUNIT,*)'==================='
  DO K=1,nfile
     kret = NF_CLOSE(handle(k))
     WRITE(LUNIT,*)'Closing input files ',handle(k)
  END DO

  DEALLOCATE(var2d,var3d,base)
  DEALLOCATE(tmflx,tmasm,tmslv,tmint)
  DEALLOCATE(cvar)

  CLOSE(kunit)
  CLOSE(LUNIT)

  888 CONTINUE

END PROGRAM mer2arl
