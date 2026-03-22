PROGRAM testcnllxy
!======================================================================
! testing for coordinate mapping function
!----------------------------------------------------------------------
! Change History:
! 04 Oct 2021 (SYZ) initial
!=======================================================================

! macros to save keystrokes
#include "utests.h"

  IMPLICIT NONE
  INCLUDE 'cnllxy.inc'
  INCLUDE 'utests.inc'
  REAL PARMAP(9), TNGLAT, CLONG
  REAL XI, ETA

!----------------------------------------------------------------------
! Test case 1 - away from a pole.
!----------------------------------------------------------------------
  ! set up
  TNGLAT =  40.0
  CLONG  = -90.0
  CALL STLMBR(PARMAP, TNGLAT, CLONG)

  ! check if GDLONG in CNLLXY() will be zero when XLONG = CLONG.
  CALL ASSERT_FLOAT_EQ2(PARMAP(2), CLONG)

  ! when GDLONG in CNLLXY() is near zero
  CALL CNLLXY(PARMAP, 40.0, -90.0, XI, ETA) 
  CALL ASSERT_FLOAT_EQ2(XI,  0.0     )
  CALL ASSERT_FLOAT_EQ2(ETA, 0.603017)

  ! when GDLONG in CNLLXY() is away from zero
  CALL CNLLXY(PARMAP, 40.0, 0.0, XI, ETA) 
  CALL ASSERT_FLOAT_EQ2(XI,  0.806625)
  CALL ASSERT_FLOAT_EQ2(ETA, 1.048765)

  ! near the north pole
  CALL CNLLXY(PARMAP, 89.5, -90.0, XI, ETA) 
  CALL ASSERT_FLOAT_EQ2(XI,  0.0     )
  CALL ASSERT_FLOAT_EQ2(ETA, 1.508426)

  ! near the south pole
  CALL CNLLXY(PARMAP, -89.5, -90.0, XI, ETA) 
  CALL ASSERT_FLOAT_EQ2(XI,    0.0     )
  CALL ASSERT_FLOAT_EQ2(ETA, -49.615574)

!----------------------------------------------------------------------
! Test case 2 - polar projection around the north pole
!----------------------------------------------------------------------
  ! set up
  TNGLAT =  90.0
  CLONG  = -90.0
  CALL STLMBR(PARMAP, TNGLAT, CLONG)

  ! when GDLONG in CNLLXY() is near zero
  CALL CNLLXY(PARMAP, 87.0, -90.0, XI, ETA) 
  CALL ASSERT_FLOAT_EQ2(XI,  0.0     )
  CALL ASSERT_FLOAT_EQ2(ETA, 0.973814)

  ! when GDLONG in CNLLXY() is away from zero
  CALL CNLLXY(PARMAP, 87.0, 0.0, XI, ETA) 
  CALL ASSERT_FLOAT_EQ2(XI,  2.618593e-02)
  CALL ASSERT_FLOAT_EQ2(ETA, 1.0         )

  ! near the north pole
  CALL CNLLXY(PARMAP, 89.5, -90.0, XI, ETA) 
  CALL ASSERT_FLOAT_EQ2(XI,  0.0     )
  CALL ASSERT_FLOAT_EQ2(ETA, 0.995637)

  CALL CNLLXY(PARMAP, 89.7, -90.0, XI, ETA) 
  CALL ASSERT_FLOAT_EQ2(XI,  0.0     )
  CALL ASSERT_FLOAT_EQ2(ETA, 0.997382)

  CALL CNLLXY(PARMAP, 89.8, -90.0, XI, ETA) 
  CALL ASSERT_FLOAT_EQ2(XI,  0.0     )
  CALL ASSERT_FLOAT_EQ2(ETA, 0.998255)

  CALL CNLLXY(PARMAP, 89.9, -90.0, XI, ETA) 
  CALL ASSERT_FLOAT_EQ2(XI,  0.0     )
  CALL ASSERT_FLOAT_EQ2(ETA, 0.999127)

  CALL CNLLXY(PARMAP, 90.0, -90.0, XI, ETA) 
  CALL ASSERT_FLOAT_EQ2(XI,  0.0     )
  CALL ASSERT_FLOAT_EQ2(ETA, 1.000000)

  ! near the south pole
  CALL CNLLXY(PARMAP, -89.5, -90.0, XI, ETA) 
  CALL ASSERT_FLOAT_EQ2(XI,     0.0  )
  CALL ASSERT_FLOAT_EQ3(ETA, -228.181, 1.0e-3)

!----------------------------------------------------------------------
! Test case 3 - polar projection around the south pole
!----------------------------------------------------------------------
  ! set up
  TNGLAT = -90.0
  CLONG  = -90.0
  CALL STLMBR(PARMAP, TNGLAT, CLONG)

  ! when GDLONG in CNLLXY() is near zero
  CALL CNLLXY(PARMAP, -87.0, -90.0, XI, ETA) 
  CALL ASSERT_FLOAT_EQ2(XI,   0.0     )
  CALL ASSERT_FLOAT_EQ2(ETA, -0.973814)

  ! when GDLONG in CNLLXY() is away from zero
  CALL CNLLXY(PARMAP, -87.0, 0.0, XI, ETA) 
  CALL ASSERT_FLOAT_EQ2(XI,   2.618592e-02)
  CALL ASSERT_FLOAT_EQ2(ETA, -1.000000)

  ! near the south pole
  CALL CNLLXY(PARMAP, -89.5, -90.0, XI, ETA) 
  CALL ASSERT_FLOAT_EQ2(XI,   0.0     )
  CALL ASSERT_FLOAT_EQ2(ETA, -0.995637)

  CALL CNLLXY(PARMAP, -89.7, -90.0, XI, ETA) 
  CALL ASSERT_FLOAT_EQ2(XI,   0.0     )
  CALL ASSERT_FLOAT_EQ2(ETA, -0.997382)

  CALL CNLLXY(PARMAP, -89.8, -90.0, XI, ETA) 
  CALL ASSERT_FLOAT_EQ2(XI,   0.0     )
  CALL ASSERT_FLOAT_EQ2(ETA, -0.998255)

  CALL CNLLXY(PARMAP, -89.9, -90.0, XI, ETA) 
  CALL ASSERT_FLOAT_EQ2(XI,   0.0     )
  CALL ASSERT_FLOAT_EQ2(ETA, -0.999127)

  CALL CNLLXY(PARMAP, -90.0, -90.0, XI, ETA) 
  CALL ASSERT_FLOAT_EQ2(XI,   0.0     )
  CALL ASSERT_FLOAT_EQ2(ETA, -1.000000)

  ! near the north pole
  CALL CNLLXY(PARMAP, 89.5, -90.0, XI, ETA) 
  CALL ASSERT_FLOAT_EQ2(XI,    0.0    )
  CALL ASSERT_FLOAT_EQ3(ETA, 228.181, 1.0e-03)

END PROGRAM
