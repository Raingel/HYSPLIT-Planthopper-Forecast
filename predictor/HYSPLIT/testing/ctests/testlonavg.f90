PROGRAM testlonavg
!======================================================================
! testing for computing an weighted longitude average
!----------------------------------------------------------------------
! Change History:
! 06 Oct 2021 (SYZ) initial
!=======================================================================

! macros to save keystrokes
#include "utests.h"

  IMPLICIT NONE
  INCLUDE 'lonavg.inc'
  INCLUDE 'utests.inc'
  REAL LONS(10), WGTS(10), AVG, VAR
  INTEGER NPTS

!----------------------------------------------------------------------
! Test case 1 - positive longitudes only
!----------------------------------------------------------------------
  ! set up
  LONS(1) = 10.0
  LONS(2) = 11.0
  LONS(3) = 12.0
  WGTS(1) =  1.0
  WGTS(2) =  1.0
  WGTS(3) =  3.0
  NPTS    =  3

  CALL LONAVG(LONS, WGTS, NPTS, AVG, VAR)

  ! check the average and variance
  CALL ASSERT_FLOAT_EQ2(AVG, 11.40)
  CALL ASSERT_FLOAT_EQ2(VAR,  0.64)

!----------------------------------------------------------------------
! Test case 2 - negative longitudes only
!----------------------------------------------------------------------
  ! set up
  LONS(1) = -10.0
  LONS(2) = -11.0
  LONS(3) = -12.0
  WGTS(1) =  1.0
  WGTS(2) =  1.0
  WGTS(3) =  3.0
  NPTS    =  3

  CALL LONAVG(LONS, WGTS, NPTS, AVG, VAR)

  ! check the average and variance
  CALL ASSERT_FLOAT_EQ2(AVG,-11.40)
  CALL ASSERT_FLOAT_EQ2(VAR,  0.64)

!----------------------------------------------------------------------
! Test case 3 - mix of postive and negative longitudes.
!               values near zero.  positive average value.
!----------------------------------------------------------------------
  ! set up
  LONS(1) =  10.0
  LONS(2) =  11.0
  LONS(3) =  12.0
  LONS(4) =  -4.0
  LONS(5) =  -5.0
  LONS(6) =  -6.0
  WGTS    =   1.0  ! same weights
  NPTS    =   6

  CALL LONAVG(LONS, WGTS, NPTS, AVG, VAR)

  ! check the average and variance
  CALL ASSERT_FLOAT_EQ2(AVG,  3.0   )
  CALL ASSERT_FLOAT_EQ3(VAR, 64.6667, 1.0E-04)

!----------------------------------------------------------------------
! Test case 4 - mix of postive and negative longitudes.
!               values near zero.  negative average value.
!----------------------------------------------------------------------
  ! set up
  LONS(1) = -10.0
  LONS(2) = -11.0
  LONS(3) = -12.0
  LONS(4) =   4.0
  LONS(5) =   5.0
  LONS(6) =   6.0
  WGTS    =   1.0  ! same weights
  NPTS    =   6

  CALL LONAVG(LONS, WGTS, NPTS, AVG, VAR)

  ! check the average and variance
  CALL ASSERT_FLOAT_EQ2(AVG, -3.0   )
  CALL ASSERT_FLOAT_EQ3(VAR, 64.6667, 1.0E-04)

!----------------------------------------------------------------------
! Test case 5 - mix of postive and negative longitudes.
!               values near 180/-180.  positive average value.
!----------------------------------------------------------------------
  ! set up
  LONS(1) =  170.0
  LONS(2) =  169.0
  LONS(3) =  168.0
  LONS(4) = -176.0
  LONS(5) = -175.0
  LONS(6) = -174.0
  WGTS    =   1.0  ! same weights
  NPTS    =   6

  CALL LONAVG(LONS, WGTS, NPTS, AVG, VAR)

  ! check the average and variance
  CALL ASSERT_FLOAT_EQ2(AVG, 177.0   )
  CALL ASSERT_FLOAT_EQ3(VAR,  64.6667, 1.0E-04)

!----------------------------------------------------------------------
! Test case 6 - mix of postive and negative longitudes.
!               values near 180/-180.  negative average value.
!----------------------------------------------------------------------
  ! set up
  LONS(1) = -170.0
  LONS(2) = -169.0
  LONS(3) = -168.0
  LONS(4) =  176.0
  LONS(5) =  175.0
  LONS(6) =  174.0
  WGTS    =   1.0  ! same weights
  NPTS    =   6

  CALL LONAVG(LONS, WGTS, NPTS, AVG, VAR)

  ! check the average and variance
  CALL ASSERT_FLOAT_EQ2(AVG, -177.0    )
  CALL ASSERT_FLOAT_EQ3(VAR,    64.6667, 1.0E-04)

END PROGRAM
