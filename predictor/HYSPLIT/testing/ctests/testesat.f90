
PROGRAM testesat
!======================================================================
! testing for saturated vapor pressure calculation
!----------------------------------------------------------------------
! Last Revision: 21 May 2023 (AMC) 
!=======================================================================

  USE prfmod
  USE frtmod
  IMPLICIT NONE
  INCLUDE 'utests.inc'

!------------------------------------------------------------------------------
  REAL EPS
  PARAMETER (EPS = 2.0e-3)   ! error allowed in output position. 
!-------------------------------------------------------------------------------
! argument list variables
!-------------------------------------------------------------------------------
  REAL    :: t1           ! temperature (K)  
  REAL    :: esat         ! saturated vapor pressure
  REAL    :: esat_target  ! saturated vapor pressure
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
! additional argument list variables for prfb
!-------------------------------------------------------------------------------

! Target variables -------------------------------------------------


! test saturated vapor pressure calculation

! target values are from Huang, "A simple accurate formula for calculating 
! saturation vapor pressure of water and ice" June 2018 American Meteorological Society 
! DOI: 0.175/JAMC-D-17-034.1
! Temperatures below 0 C are not tested because WMO guidelines state
! to calculate RH with respect to water and not ice.
! the reference values in Huang are for calculation with respect to ice.

!----------------------------------------------------------------------------------------------
  T1 = 0.01 + 273.15
  esat_target = 611.655/100.0
  esat = calcesat(T1)
  write(*,*) 'saturated vapor pressure with calcesat', T1, esat, esat_target, esat-esat_target
  CALL ASSERT_FLOAT_EQ(__FILE__, __LINE__, esat, esat_target, 0.5)

  esat = 10*esat_pr(T1)
  write(*,*) 'saturated vapor pressure with esat_pr ', T1, esat, esat_target, esat-esat_target
!  CALL ASSERT_FLOAT_EQ(__FILE__, __LINE__, esat, esat_target, 0.5)
write (*,*) '---------------------------------------'

!----------------------------------------------------------------------------------------------
  T1 = -40 + 273.15
  esat_target = 12.8412/100.0
  esat = calcesat(T1)
  write(*,*) 'saturated vapor pressure with calcesat', T1, esat, esat_target, esat-esat_target
!  CALL ASSERT_FLOAT_EQ(__FILE__, __LINE__, esat, esat_target, 0.5)

  esat = 10*esat_pr(T1)
  write(*,*) 'saturated vapor pressure with esat_pr ', T1, esat, esat_target, esat-esat_target
!  CALL ASSERT_FLOAT_EQ(__FILE__, __LINE__, esat, esat_target, 0.5)
write (*,*) '---------------------------------------'

!----------------------------------------------------------------------------------------------
  T1 = -20 + 273.15
  esat_target = 103.239/100.0
  esat = calcesat(T1)
  write(*,*) 'saturated vapor pressure with calcesat', T1, esat, esat_target, esat-esat_target
!  CALL ASSERT_FLOAT_EQ(__FILE__, __LINE__, esat, esat_target, 0.5)

  esat = 10*esat_pr(T1)
  write(*,*) 'saturated vapor pressure with esat_pr ', T1, esat, esat_target, esat-esat_target
!  CALL ASSERT_FLOAT_EQ(__FILE__, __LINE__, esat, esat_target, 0.5)
write (*,*) '---------------------------------------'

!----------------------------------------------------------------------------------------------
  T1 = -10 + 273.15
  esat_target = 2339.32/100.0
  esat = calcesat(T1)
  write(*,*) 'saturated vapor pressure with calcesat', T1, esat
!  CALL ASSERT_FLOAT_EQ(__FILE__, __LINE__, esat, esat_target, 0.5)

  esat = 10*esat_pr(T1)
  write(*,*) 'saturated vapor pressure with esat_pr ', T1, esat
!  CALL ASSERT_FLOAT_EQ(__FILE__, __LINE__, esat, esat_target, 0.5)

write (*,*) '---------------------------------------'

!----------------------------------------------------------------------------------------------
  T1 = 20 + 273.15
  esat_target = 2339.32/100.0
  esat = calcesat(T1)
  write(*,*) 'saturated vapor pressure with calcesat', T1, esat, esat_target, esat-esat_target
  CALL ASSERT_FLOAT_EQ(__FILE__, __LINE__, esat, esat_target, 0.5)

  esat = 10*esat_pr(T1)
  write(*,*) 'saturated vapor pressure with esat_pr ', T1, esat, esat_target, esat-esat_target
!  CALL ASSERT_FLOAT_EQ(__FILE__, __LINE__, esat, esat_target, 0.5)
write (*,*) '---------------------------------------'
  
!----------------------------------------------------------------------------------------------
  T1 = 40 + 273.15
  esat_target = 7384.94/100.0
  esat = calcesat(T1)
  write(*,*) 'saturated vapor pressure with calcesat', T1, esat, esat_target, esat-esat_target
  CALL ASSERT_FLOAT_EQ(__FILE__, __LINE__, esat, esat_target, 1.0)

  esat = 10*esat_pr(T1)
  write(*,*) 'saturated vapor pressure with esat_pr ', T1, esat, esat_target, esat-esat_target
!  CALL ASSERT_FLOAT_EQ(__FILE__, __LINE__, esat, esat_target, 0.5)
write (*,*) '---------------------------------------'
  
!----------------------------------------------------------------------------------------------
!----------------------------------------------------------------------------------------------
!----------------------------------------------------------------------------------------------
  


END PROGRAM testesat
