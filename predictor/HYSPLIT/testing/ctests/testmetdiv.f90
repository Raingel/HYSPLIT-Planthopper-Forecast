
PROGRAM testmetdiv
!======================================================================
! testing for metdivmod function
!----------------------------------------------------------------------
! Last Revision: 07 Jul 2023 (AMC) Fantine Ngan found bug in metdiv.
!                                  metdiv changed to a module
!                                  some unit tests found here.
!=======================================================================

!  USE funits
  USE metdivmod

  IMPLICIT NONE

  INCLUDE 'DEFARG2.INC'
  INCLUDE 'TESTMET.INC'
  INCLUDE 'utests.inc'
!------------------------------------------------------------------------------
  INTEGER    :: kret, nxt, nyt
  REAL       :: xres, yres    ! grid resolution in degrees. 
  INTEGER    :: CCC 
  REAL       :: EPS
  PARAMETER (EPS = 2.0e-3)   ! error allowed in output position. 
!-------------------------------------------------------------------------------
! argument list variables
!-------------------------------------------------------------------------------
  REAL, ALLOCATABLE   :: uuu(:,:,:)      ! u wind
  REAL, ALLOCATABLE   :: vvv(:,:,:)      ! v wind
  REAL, ALLOCATABLE   :: gx(:,:)      ! grid size (m)
  REAL, ALLOCATABLE   :: gy(:,:)      ! grid size (m)
  INTEGER :: nlvl            ! number of levels to process
  REAL, ALLOCATABLE   :: www(:,:,:)      ! w wind (output)
  REAL, ALLOCATABLE   :: uu1(:),uu2(:),vv1(:),vv2(:),wwt(:),answer(:)

  REAL, ALLOCATABLE  :: zsg(:)
  INTEGER :: i,j


! Target variables -------------------------------------------------
!------------------------------------------------------------------------------

! setup input variables 
  xres=0.25
  yres=0.25
  nlvl = 20
  nxt = 20
  nyt = 20

!---------------------------------------------

!-------------------------------------------------
!---------------------------------------------

  ALLOCATE (uuu(4,4,nlvl), &
            vvv(4,4,nlvl), &
            www(4,4,nlvl), STAT=kret)

  ALLOCATE( gx(4,4),    &
            gy(4,4), STAT=kret )

  nlvl=5
  www=0
  ccc=6
  ALLOCATE(uu1(nlvl),uu2(nlvl),vv1(nlvl),vv2(nlvl),wwt(nlvl), stat=kret)
  ALLOCAtE(answer(nlvl),stat=kret)
  ALLOCATE (zsg(nlvl), STAT=kret)

! zsg goes from 1 to 0.
  IF(CCC.EQ.6)zsg=[0.9,0.8,0.7,0.5,0.3]

  CALL VELCASEDIV(NXT,NYT,NLVL,UUU,VVV,CCC)
  CALL GRIDCASE1(NXT,NYT,XRES,YRES,GX,GY,111.0e3)
  CALL metdiv(nxt,nyt,nlvl,zsg,uuu,vvv,www,gx,gy)

  ! not testing the smoothing function here.
  CALL SMOOTH((/ 1.25e-3, 3.75e-3, 5e-3, 2.5e-3, -2.5e-3 /),nlvl,answer)

  j=2
  DO I=1,NLVL
     write(*,*) www(j,2,i), uuu(j-1,2,i), uuu(j+1,2,i), -1*answer(i)
     CALL ASSERT_FLOAT_EQ(__FILE__,__LINE__,-1*answer(i),www(j,2,i),eps)
  END DO

  CALL SMOOTH((/ -2.5e-3, -7.5e-3, -1e-2, -5e-3, 5e-3 /),nlvl,answer)
  
  j=3
  ! mutiply answer by -1 because upward velocities in HYSPLIT are negative.
  DO I=1,NLVL
     write(*,*) www(j,2,i), uuu(j-1,2,i), uuu(j+1,2,i)
     CALL ASSERT_FLOAT_EQ(__FILE__,__LINE__,-1*answer(i),www(j,2,i),eps)
  END DO

! ---------------------------------------------------
  ! velocities all diverging
  vv1=0.
  vv2=0.
  uu1 = (/ -10.,-10.,-10.,-10.,-10. /)
  uu2 = (/ 10.,10.,10.,10.,10. /)
  zsg = (/ 0.9,0.8,0.7,0.6,0.5/)
  gx = 10e3 ! 10 km grid
  gy = 10e3 ! 10 km grid
  CALL CALCDIV(uu1,uu2,vv1,vv2,&
               wwt, NLVL,ZSG,2*GX(1,1),2*GY(1,1))
  answer = -1 * (/ 0.003,0.009,0.015,0.021,0.027 /)
  DO I=1,NLVL
     write(*,*) 'Z', I,  answer(i), wwt(i)
     !CALL ASSERT_FLOAT_EQ(__FILE__,__LINE__,answer(i),wwt(i),eps)
  END DO

! ---------------------------------------------------
  ! velocities start out diverging and then converge
  vv1=0.
  vv2=0.
  uu1 = (/ -10.,-10.,10.,10.,10. /)
  uu2 = (/ 10.,10.,-10.,-10.,-10. /)
  zsg = (/ 0.9,0.8,0.7,0.6,0.5/)
  gx = 10e3 ! 10 km grid
  gy = 10e3 ! 10 km grid
  CALL CALCDIV(uu1,uu2,vv1,vv2,&
               wwt, NLVL,ZSG,2*GX(1,1),2*GY(1,1))
  answer = (/ -0.003,-0.009,-0.009,-0.003,0.003 /)
  DO I=1,NLVL
     CALL ASSERT_FLOAT_EQ(__FILE__,__LINE__,answer(i),wwt(i),eps)
  END DO


  ! spacing increases  0.05, 0.1, 0.15, 0.2, 0.25
  ! to keep the same velocity differences must be

  ! first half level. (0.1/0.05*5)= -10, 10. diverging
  ! first full level u= -20, 20              diverging

  ! second half level. (0.1/0.1*10)= -10, 10 diverging
  ! second full level  u=0,0                 neighter

  ! third half level. (0.1/0.15*10)=6.66,-6.66 converging
  ! third full level. 0-6.666*2 = 13.333,-13.333 converging

  ! fourth half level (0.1/0.2*10)=5, -5 converging
  ! fourth full level 13.333-5*2 = -3.33,3.33 diverging

  ! fifth half level (0.1/0.25*10)=4, -4  converging
  ! fifth full level 3.33+4*2 = 11.333, -11.333 converging

  ! 6.66 (0.1/0.15*10) at third half level so 13.332 at third full level.
  ! (0.1/0.2)*10= 5 AT FOURTH Half LEVEL. 3.33 = 13.332 - 5*2
  ! (0.2/0.25)*10=4 at fifth half level. 4.67 = 3.33-4*2
  zsg = (/ 0.95,0.85,0.70,0.50,0.25 /)
  uu1 = (/ -20.,0.,13.332,-3.33,11.333 /)
  uu2 = (/ 20.,0.,-13.332,3.33,-11.333/)
  
  CALL CALCDIV(uu1,uu2,vv1,vv2,&
               wwt, NLVL,ZSG,2*GX(1,1),2*GY(1,1))
  answer = (/ -0.003,-0.009,-0.003,0.003,0.009 /)

  DO I=1,NLVL
     CALL ASSERT_FLOAT_EQ(__FILE__,__LINE__,answer(i),wwt(i),eps)
  END DO

  !CALL stagmetdiv(nxt,nyt,nlvl,zsg,uuu,vvv,www,gx,gy)



END PROGRAM testmetdiv
