
PROGRAM testadviec
!======================================================================
! testing for adviec function
!----------------------------------------------------------------------
! Last Revision: 04 oct 2020 (AMC) test one contant velocity case.
!=======================================================================

!  USE funits

  IMPLICIT NONE

  INCLUDE 'DEFARG2.INC'
  INCLUDE 'TESTMET.INC'
  INCLUDE 'utests.inc'
!------------------------------------------------------------------------------
  INTEGER    :: kret, nxt, nyt
  REAL       :: xres, yres   ! grid resolution in degrees. 
  INTEGER    :: KL, KX, KY
  INTEGER    :: vx, vy, vz   ! velocity in grid cell / minute
  LOGICAL :: teststatus      ! keeps track of whether test has passed or failed. 
  REAL EPS
  PARAMETER (EPS = 2.0e-3)   ! error allowed in output position. 

!-------------------------------------------------------------------------------
! argument list variables
!-------------------------------------------------------------------------------
  REAL, ALLOCATABLE   :: uuu(:,:,:,:)      ! u wind
  REAL, ALLOCATABLE   :: vvv(:,:,:,:)      ! v wind
  REAL, ALLOCATABLE   :: www(:,:,:,:)      ! w wind
  REAL, ALLOCATABLE   :: gx(:,:)      ! grid size (m)
  REAL, ALLOCATABLE   :: gy(:,:)      ! grid size (m)
  INTEGER :: k1,k2           ! last and next time index    
  INTEGER :: nlvl            ! number of levels to process
  INTEGER :: mtime(2)        ! time of meteo observations   
  INTEGER :: jet             ! current elapsed time (minutes)
  REAL    :: zmdl            ! vertical model domain top (m)
  REAL    :: dt              ! integration step (minutes)
  REAL    :: tratio          ! time step stability criterion
  LOGICAL :: tavrg           ! velocity fields temporally averaged
  LOGICAL :: back            ! flag to indicate direction
  LOGICAL :: global          ! global cyclic boundary conditions
  INTEGER :: nxp,nyp         ! global boundaries           
  REAL, ALLOCATABLE  :: zsg(:)

  REAL    :: xx,yy,zz        ! old (t) and new (t+dt) position 
  REAL    :: zx              ! last estimate of vertical index
  REAL    :: umn,vmn         ! output velocity

! INOUT
! xx, yy, zz
! OUT
! zx


! Target variables -------------------------------------------------
  REAL     :: txx, tyy, tzz, tzx
! keep track of original values ------------------------------------
  REAL     :: pxx, pyy, pzz, pzx

!COMMON BLOCK----------------------------------------------------

  REAL       :: aa,bb,cc          ! vertical grid polynomial
  REAL       :: aa2,bb2,cc2      
  REAL       :: sux, suy, svx, svy
  REAL       :: sux2, suy2, svx2, svy2

  COMMON /ZZTOKK/ AA,BB,CC
  COMMON /STAGGER/ SUX, SUY, SVX, SVY
!------------------------------------------------------------------------------

  teststatus = .TRUE.
! setup input variables 
  xres=0.25
  yres=0.25
  k1  = 1
  k2  = 2
  nlvl = 20
  mtime(1) = 0
  mtime(2) = 60
  jet = 10
  zmdl = 10000
  dt   = 1
  tratio = 1
  tavrg = .FALSE.
  back = .FALSE.
  global = .FALSE.
  nxt = 10
  nyt = 10
  nxp = 10
  nyp = 10

  xx = 2
  yy = 2
  zz = 2
  zx = 4 

  pxx = xx
  pyy = yy
  pzz = zz
  pzx = zx
!---------------------------------------------
! setup common block variables (TO DO pick values)

  AA   = 1
  BB   = 1
  CC   = 1

! velocity offsets for staggered grids
  SUX = 0
  SUY = 0
  SVX = 0
  SVY = 0

! create copies of common block variables for testing
  aa2 = aa
  bb2 = bb
  cc2 = cc
  sux2 = sux
  suy2 = suy
  svx2 = svx
  svy2 = svy

! Allocate arrays
  ALLOCATE (uuu(nxt,nyt,nlvl,2), &
            vvv(nxt,nyt,nlvl,2), &
            www(nxt,nyt,nlvl,2),STAT=kret)

  ALLOCATE (gx(nxt,nyt), &
            gy(nxt,nyt), STAT=kret)

  ALLOCATE (zsg(nlvl), STAT=kret)

!-------------------------------------------------
!---------------------------------------------
! constant velocity case
  DO KL=1,NLVL
     zsg(kl) = KL
  END DO
! SETUP target values

! set constant velocity values in grid cell / minute
  vx = 3. 
  vy = 5.
  vz = 1.

! set target values for constant velocity field.
  txx = xx + vx*dt 
  tyy = yy + vy*dt
  tzz = zz + vz*dt
  tzx = 1

! convert to m/s for new adviec.f routine
  vx = vx * 111.0e3 * xres / 60.0
  vy = vy * 111.0e3 * xres / 60.0

  DO KL=1,NLVL
     zsg(kl) = KL
  END DO
  CALL VELCASE1(NXT,NYT,NLVL,VX,VY,VZ,UUU,VVV,WWW)
  CALL GRIDCASE1(NXT,NYT,XRES,YRES,GX,GY,111.0e3)
  CALL ADVIEC(uuu,vvv,www,  &
             K1,K2,NLVL,MTIME,JET,ZMDL,XX,YY,ZZ,          &
             ZX,DT,TRATIO,                                &
             TAVRG,BACK,                                  &  
             GLOBAL,NXP,NYP,ZSG,GX,GY)

! -----------------------------------------------
! test that no common block variables are modified.
  IF(aa.ne.aa2)STOP 2
  IF(cc.ne.cc2)STOP 2
  IF(bb.ne.bb2)STOP 2

! -----------------------------------------------
! Test values that should be modified
  WRITE(*,*) tyy-yy
  CALL ASSERT_FLOAT_EQ(__FILE__, __LINE__, xx, txx, EPS)
  CALL ASSERT_FLOAT_EQ(__FILE__, __LINE__, yy, tyy, EPS)
  CALL ASSERT_FLOAT_EQ(__FILE__, __LINE__, zz, tzz, EPS)
  CALL ASSERT_FLOAT_EQ(__FILE__, __LINE__, zx, tzx, EPS)
  WRITE(*,*) 'xpos', pxx, xx, txx
  WRITE(*,*) 'ypos', pyy, yy, tyy
  WRITE(*,*) 'zpos', pzz, zz, tzz
  WRITE(*,*) 'zindex', pzx, zx, tzx

!  IF(XX.NE.TXX)THEN
!     WRITE(*,*) xx, 'xpos not as expected', txx
!     teststatus = .FALSE.
!  END IF
!  IF(YY.NE.TYY)THEN
!     WRITE(*,*) yy, 'ypos not as expected',  tyy
!     teststatus = .FALSE.
!  END IF
!  IF(ZZ.NE.TZZ)THEN
!     WRITE(*,*) zz, 'ypos not as expected', tyy
!     teststatus = .FALSE.
!  END IF
!  IF(YY.NE.TYY)THEN
!     WRITE(*,*) zx, 'z index not as expceted',  tzx
!     teststatus = .FALSE.
!  END IF
!  If(.NOT. teststatus) STOP 2

! END constant velocity case.
! -----------------------------------------------
! -----------------------------------------------

  DEALLOCATE(uuu,vvv,www,zsg)


END PROGRAM testadviec
