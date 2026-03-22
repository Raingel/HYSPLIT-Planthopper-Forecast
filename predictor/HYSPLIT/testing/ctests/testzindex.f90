
PROGRAM testzindex
!======================================================================
! testing for zindex function
! tests for both user defined and using the quadratic equation.
! tests that returned index for sigma below first level is 1.
! tests that returned index is sigma above last level is nlvl
! tests for level between first and second level
! tests for level near the middle
! test is for one set of a,b,c and nlvl values.
!----------------------------------------------------------------------
! Last Revision: 09 Sept 2023 (AMC)
!=======================================================================

!  USE funits

  IMPLICIT NONE

  INCLUDE 'zindex.inc'
  INCLUDE 'TESTMET.INC'
  INCLUDE 'utests.inc'
!------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! argument list variables
!-------------------------------------------------------------------------------

  REAL    :: zmdl            ! vertical model domain top (m)
  INTEGER :: nlvl            ! number of levels to process
  REAL    :: zz        ! old (t) and new (t+dt) position 
  REAL    :: zx        ! index of zz position  in zsg array
  REAL    :: zsg(71)

! internal variables
  INTEGER :: kret,n
  INTEGER :: tn1, tn2
  REAL    :: target_zx, eps

!COMMON BLOCK----------------------------------------------------
! may no longer need if using the zsg loop.
  REAL       :: aa,bb,cc          ! vertical grid polynomial
  COMMON /ZZTOKK/ AA,BB,CC
!------------------------------------------------------------------------------
  zmdl = 25000.
  nlvl = 71
  AA   = 5.
  BB   = 5.
  CC   = 0.

  zx = 0.     ! initialize
  eps = 0.001 ! how much error expected for vx?
!----------------------------------------------------------------
! zsg is defined like this in hymodelc and hymodelt
  DO N=1,NLVL
     ZSG(N) = 1 - (AA*FLOAT(N*N) + BB*FLOAT(N) + CC)/ZMDL
  END DO
!----------------------------------------------------------------

! Check for zz near the middle somewhere in the middle
  ZZ = ZSG(50) + 0.75*(ZSG(49)-ZSG(50))
  target_zx = 49.25

  write(*,*) 'description  zz   zx  target_zx'
  AA=5
  CALL ZINDEX(NLVL,ZMDL,ZZ,ZSG,ZX)
  write(*,*) 'ZX equation', ZZ, ZX, target_zx
  CALL ASSERT_FLOAT_EQ(__FILE__, __LINE__, ZX, target_zx, EPS)

  AA=-999999.0
  CALL ZINDEX(NLVL,ZMDL,ZZ,ZSG,ZX)
  write(*,*) 'ZX user defined', ZZ, ZX, target_zx
  CALL ASSERT_FLOAT_EQ(__FILE__, __LINE__, ZX, target_zx, EPS)

! -------------------------------------------
! Check for ZZ close to  first layer.
  ZZ = ZSG(2) + 0.5*(ZSG(1)-ZSG(2))
  target_zx = 1.5
  write(*,*) '-------------------------------------'
  write(*,*) 'Checking for zz between first and second level', ZZ, ZSG(1)

  AA=5
  CALL ZINDEX(NLVL,ZMDL,ZZ,ZSG,ZX)
  write(*,*) 'ZX equation', ZZ, ZX, target_zx
  CALL ASSERT_FLOAT_EQ(__FILE__, __LINE__, ZX, target_zx, EPS)

  AA=-99999.0
  CALL ZINDEX(NLVL,ZMDL,ZZ,ZSG,ZX)
  write(*,*) 'ZX user defined', ZZ, ZX, target_zx
  CALL ASSERT_FLOAT_EQ(__FILE__, __LINE__, ZX, target_zx, EPS)

! -------------------------------------------
! Check for ZZ between surface and first layer.
  ZZ = ZSG(1) + 0.5*(1-ZSG(1))
  target_zx = 0.5 ! use if allowing zx to be less than 0.
  target_zx = 1.0
  write(*,*) '-------------------------------------'
  write(*,*) 'Checking for zz below first level', ZZ, ZSG(1)

  AA=5
  CALL ZINDEX(NLVL,ZMDL,ZZ,ZSG,ZX)
  write(*,*) 'ZX equation', ZZ, ZX, target_zx
  CALL ASSERT_FLOAT_EQ(__FILE__, __LINE__, ZX, target_zx, EPS)

  AA=-99999.0
  CALL ZINDEX(NLVL,ZMDL,ZZ,ZSG,ZX)
  write(*,*) 'ZX user defined', ZZ, ZX, target_zx
  CALL ASSERT_FLOAT_EQ(__FILE__, __LINE__, ZX, target_zx, EPS)

!  AA=-99.5
!  CALL ZINDEX(NLVL,ZMDL,ZZ,ZSG,ZX)
!  write(*,*) 'ZX computed with user dedfined', ZX, target_zx

!-----------------------------------------
! Check for ZZ above top layer?
  ZZ = ZSG(NLVL) - 1.50*(ZSG(NLVL-1)-ZSG(NLVL))
  target_zx = 72.5
  target_zx = 71.   ! use if not allowing zx greater than nlvl
  write(*,*) '-------------------------------------'
  write(*,*) 'Checking for zz above last level zz, zsg(nlvl)', ZZ, ZSG(NLVL)
  write(*,*) 'HEIGHTS MSL', ZMDL*(1-ZZ), ZMDL*(1-ZSG(NLVL))
  AA=5
  CALL ZINDEX(NLVL,ZMDL,ZZ,ZSG,ZX)
  write(*,*) 'ZX equation', ZZ, ZX, target_zx
  CALL ASSERT_FLOAT_EQ(__FILE__, __LINE__, ZX, target_zx, EPS)

  AA=-99999.0
  CALL ZINDEX(NLVL,ZMDL,ZZ,ZSG,ZX)
  write(*,*) 'ZX user defined', ZZ, ZX, target_zx
  CALL ASSERT_FLOAT_EQ(__FILE__, __LINE__, ZX, target_zx, EPS)

!  AA=-99.5
!  CALL ZINDEX(NLVL,ZMDL,ZZ,ZSG,ZX)
!  write(*,*) 'ZX computed with user dedfined', ZX, target_zx

!  DEALLOCATE(zsg)


END PROGRAM testzindex
