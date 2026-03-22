PROGRAM testmapbox
!======================================================================
! unit test the CMAPBOX class and its methods.
!----------------------------------------------------------------------
! PROGRAM HISTORY LOG:
!   LAST REVISED: 06 Oct 2021 (SYZ) - initial
!                 18 Oct 2022 (SYZ) - update after mapbox.f revision
!                                     for placing the invisible ring
!                                     at the map-hold location
!=======================================================================

! macros to save keystrokes
#include "utests.h"

  IMPLICIT NONE

  INTERFACE
     SUBROUTINE TEST_CTOR()
     END SUBROUTINE

     SUBROUTINE TEST_INITQ()
     END SUBROUTINE

     SUBROUTINE TEST_ALLOC()
     END SUBROUTINE

     SUBROUTINE TEST_CLEAR()
     END SUBROUTINE

     SUBROUTINE TEST_HOLDAT()
     END SUBROUTINE

     SUBROUTINE TEST_REGLON()
     END SUBROUTINE

     SUBROUTINE TEST_DIFLON()
     END SUBROUTINE

     SUBROUTINE TEST_ADDCON()
     END SUBROUTINE

     SUBROUTINE TEST_ADDPNT()
     END SUBROUTINE

     SUBROUTINE TEST_SETRNG()
     END SUBROUTINE

     SUBROUTINE TEST_GETEXT()
     END SUBROUTINE

     SUBROUTINE TEST_CALCBX()
     END SUBROUTINE

     SUBROUTINE TEST_FREE()
     END SUBROUTINE
  END INTERFACE

!----------------------------------------------------------------------
! Test the CMAPBOX class constructor
!----------------------------------------------------------------------
  CALL TEST_CTOR()

!----------------------------------------------------------------------
! Test each methods in the CMAPBOX class
!----------------------------------------------------------------------
  CALL TEST_INITQ()
  CALL TEST_ALLOC()
  CALL TEST_CLEAR()
  CALL TEST_HOLDAT()
  CALL TEST_REGLON()
  CALL TEST_DIFLON()
  CALL TEST_ADDCON()
  CALL TEST_ADDPNT()
  CALL TEST_SETRNG()
  CALL TEST_GETEXT()
  CALL TEST_CALCBX()
  CALL TEST_FREE()

END PROGRAM

!----------------------------------------------------------------------
! Test CMAPBOX constructor
!----------------------------------------------------------------------
SUBROUTINE TEST_CTOR()
  USE mapbox
  IMPLICIT NONE
  INCLUDE 'utests.inc'

  TYPE(CMAPBOX)      :: MBOX

  ! check the initial values of class member variables.
  CALL ASSERT_EQ2(MBOX%KLAT, 181)
  CALL ASSERT_EQ2(MBOX%KLON, 360)
  CALL ASSERT_FLOAT_EQ2(MBOX%GINC,    1.0)
  CALL ASSERT_FLOAT_EQ2(MBOX%GCLAT, -90.0)
  CALL ASSERT_FLOAT_EQ2(MBOX%GCLON,   0.0)
  CALL ASSERT_FALSE1(MBOX%HOLDQ)
  CALL ASSERT_EQ2(MBOX%NPTS, 0)
END SUBROUTINE

!----------------------------------------------------------------------
! Test INITQ()
!----------------------------------------------------------------------
SUBROUTINE TEST_INITQ()
  USE mapbox
  IMPLICIT NONE
  INCLUDE 'utests.inc'

  TYPE(CMAPBOX)      :: MBOX

  ! The LATLON array is not initialized
  CALL ASSERT_FALSE1(MBOX%INITQ())

  ! Allocate LATLON and retry
  ALLOCATE(MBOX%LATLON(181, 360))
  CALL ASSERT_TRUE1(MBOX%INITQ())
  DEALLOCATE(MBOX%LATLON)
END SUBROUTINE

!----------------------------------------------------------------------
! Test ALLOC()
!----------------------------------------------------------------------
SUBROUTINE TEST_ALLOC()
  USE mapbox
  IMPLICIT NONE
  INCLUDE 'utests.inc'

  TYPE(CMAPBOX)      :: MBOX

  ! allocate using a longitude range [-180, 180) instead of [0, 360).
  CALL MBOX%ALLOC(-90.0, -180.0, 181.0, 360.0, 1.0, 'alloc failed')

  ! check
  CALL ASSERT_TRUE1(MBOX%INITQ())
  CALL ASSERT_EQ2(MBOX%KLAT, 181)
  CALL ASSERT_EQ2(MBOX%KLON, 360)
  CALL ASSERT_FLOAT_EQ2(MBOX%GCLAT,  -90.0)
  CALL ASSERT_FLOAT_EQ2(MBOX%GCLON, -180.0)
  CALL ASSERT_FLOAT_EQ2(MBOX%GINC ,    1.0)

  ! cleanup
  CALL MBOX%FREE()
END SUBROUTINE

!----------------------------------------------------------------------
! Test CLEAR()
!----------------------------------------------------------------------
SUBROUTINE TEST_CLEAR()
  USE mapbox
  IMPLICIT NONE
  INCLUDE 'utests.inc'

  TYPE(CMAPBOX)      :: MBOX

  ! set up
  CALL MBOX%ALLOC(-90.0, 0.0, 181.0, 360.0, 1.0, 'alloc failed')
  MBOX%LATLON(1,1) = 1

  ! test
  CALL MBOX%CLEAR()
  CALL ASSERT_EQ2(MBOX%LATLON(1,1), 0)

  ! cleanup
  CALL MBOX%FREE()
END SUBROUTINE

!----------------------------------------------------------------------
! Test REGLON()
!----------------------------------------------------------------------
SUBROUTINE TEST_REGLON()
  USE mapbox
  IMPLICIT NONE
  INCLUDE 'utests.inc'

  TYPE(CMAPBOX)      :: MBOX

  ! set up
  CALL MBOX%ALLOC(-90.0, 0.0, 181.0, 360.0, 1.0, 'alloc failed')

  ! longtidue values are normalized to [0, 360) for the set up.
  CALL ASSERT_FLOAT_EQ2(MBOX%REGLON(-180.0), 180.0)
  CALL ASSERT_FLOAT_EQ2(MBOX%REGLON( -10.0), 350.0)
  CALL ASSERT_FLOAT_EQ2(MBOX%REGLON(   0.0),   0.0)
  CALL ASSERT_FLOAT_EQ2(MBOX%REGLON( 180.0), 180.0)
  CALL ASSERT_FLOAT_EQ2(MBOX%REGLON( 360.0),   0.0)
  CALL ASSERT_FLOAT_EQ2(MBOX%REGLON( 365.0),   5.0)

  ! cleanup
  CALL MBOX%FREE()
END SUBROUTINE

!----------------------------------------------------------------------
! Test DIFLON()
!----------------------------------------------------------------------
SUBROUTINE TEST_DIFLON()
  USE mapbox
  IMPLICIT NONE
  INCLUDE 'utests.inc'

  TYPE(CMAPBOX)      :: MBOX

  ! set up
  CALL MBOX%ALLOC(-90.0, 0.0, 181.0, 360.0, 1.0, 'alloc failed')

  ! tests
  CALL ASSERT_FLOAT_EQ2(MBOX%DIFLON(  0.0,   0.0), 0.0)
  ! positive values
  CALL ASSERT_FLOAT_EQ2(MBOX%DIFLON( 10.0,   8.0), 2.0)
  CALL ASSERT_FLOAT_EQ2(MBOX%DIFLON(  8.0,  10.0), 2.0)
  ! negative values
  CALL ASSERT_FLOAT_EQ2(MBOX%DIFLON(-10.0,  -8.0), 2.0)
  CALL ASSERT_FLOAT_EQ2(MBOX%DIFLON( -8.0, -10.0), 2.0)
  ! values near zero
  CALL ASSERT_FLOAT_EQ2(MBOX%DIFLON(-10.0,   8.0), 18.0)
  CALL ASSERT_FLOAT_EQ2(MBOX%DIFLON(  8.0, -10.0), 18.0)
  ! values near -180 and/or +180
  CALL ASSERT_FLOAT_EQ2(MBOX%DIFLON(-175.0,  178.0), 7.0)
  CALL ASSERT_FLOAT_EQ2(MBOX%DIFLON( 178.0, -175.0), 7.0)

  ! cleanup
  CALL MBOX%FREE()
END SUBROUTINE

!----------------------------------------------------------------------
! Test HOLDAT()
!----------------------------------------------------------------------
SUBROUTINE TEST_HOLDAT()
  USE mapbox
  IMPLICIT NONE
  INCLUDE 'utests.inc'

  TYPE(CMAPBOX)      :: MBOX

  ! set up
  CALL MBOX%ALLOC(-90.0, 0.0, 181.0, 360.0, 1.0, 'alloc failed')

  ! test
  CALL ASSERT_FALSE1(MBOX%HOLDQ)

  CALL MBOX%HOLDAT(40.0, -90.0)

  CALL ASSERT_TRUE1(MBOX%HOLDQ)
  CALL ASSERT_FLOAT_EQ2(MBOX%HLAT,  40.0)
  CALL ASSERT_FLOAT_EQ2(MBOX%HLON, 270.0)

  ! cleanup
  CALL MBOX%FREE()
END SUBROUTINE

!----------------------------------------------------------------------
! Test ADDCON()
!----------------------------------------------------------------------
SUBROUTINE TEST_ADDCON()
  USE mapbox
  IMPLICIT NONE
  INCLUDE 'utests.inc'

  TYPE(CMAPBOX)      :: MBOX
  REAL               :: CONC(4, 6)
  INTEGER            :: NHIT

  ! set up
  CALL MBOX%ALLOC(-90.0, 0.0, 181.0, 360.0, 1.0, 'alloc failed')
  CALL MBOX%CLEAR()
  CONC = 1.0

  ! test
  CALL MBOX%ADDCON(CONC, 0.0, 0.0, 0.5, 0.5, 4, 6, NHIT)

  CALL ASSERT_EQ2(NHIT, 24)
  CALL ASSERT_EQ2(MBOX%LATLON(91, 1), 4)
  CALL ASSERT_EQ2(MBOX%LATLON(91, 2), 4)
  CALL ASSERT_EQ2(MBOX%LATLON(91, 3), 4)
  CALL ASSERT_EQ2(MBOX%LATLON(91, 4), 0)
  CALL ASSERT_EQ2(MBOX%LATLON(92, 1), 4)
  CALL ASSERT_EQ2(MBOX%LATLON(92, 2), 4)
  CALL ASSERT_EQ2(MBOX%LATLON(92, 3), 4)
  CALL ASSERT_EQ2(MBOX%LATLON(92, 4), 0)

  ! cleanup
  CALL MBOX%FREE()
END SUBROUTINE

!----------------------------------------------------------------------
! Test ADDPNT()
!----------------------------------------------------------------------
SUBROUTINE TEST_ADDPNT()
  USE mapbox
  IMPLICIT NONE
  INCLUDE 'utests.inc'

  TYPE(CMAPBOX)      :: MBOX

  ! set up
  CALL MBOX%ALLOC(-90.0, 0.0, 181.0, 360.0, 1.0, 'alloc failed')
  CALL MBOX%CLEAR()

  ! test with a location
  CALL MBOX%ADDPNT(40.0, -90.0)
  CALL ASSERT_EQ2(MBOX%LATLON(131, 271), 1)

  ! cross the north pole
  CALL MBOX%ADDPNT(89.0, -90.0)
  CALL ASSERT_EQ2(MBOX%LATLON(180, 271), 1)
  CALL MBOX%ADDPNT(91.0, -90.0)
  CALL ASSERT_EQ2(MBOX%LATLON(180,  91), 1)

  ! cross the south pole
  CALL MBOX%ADDPNT(-89.0, -90.0)
  CALL ASSERT_EQ2(MBOX%LATLON(  2, 271), 1)
  CALL MBOX%ADDPNT(-91.0, -90.0)
  CALL ASSERT_EQ2(MBOX%LATLON(  2,  91), 1)

  ! cleanup
  CALL MBOX%FREE()
END SUBROUTINE

!----------------------------------------------------------------------
! Test SETRNG()
!----------------------------------------------------------------------
SUBROUTINE TEST_SETRNG()
  USE mapbox
  IMPLICIT NONE
  INCLUDE 'utests.inc'

  TYPE(CMAPBOX)      :: MBOX
  REAL               :: DIST

  ! set up
  CALL MBOX%ALLOC(-90.0, 0.0, 181.0, 360.0, 1.0, 'alloc failed')
  CALL MBOX%CLEAR()

  ! four concentric rings 111 km apart
  DIST = 111.0
  CALL MBOX%SETRNG(40.0, -90.0, 4, DIST)

  CALL ASSERT_FLOAT_EQ2(DIST, 100.0)

  CALL ASSERT_EQ2(MBOX%LATLON(136, 271), 0)
  CALL ASSERT_EQ2(MBOX%LATLON(135, 271), 0)
  CALL ASSERT_GE2(MBOX%LATLON(134, 271), 1)
  CALL ASSERT_GE2(MBOX%LATLON(133, 271), 1)
  CALL ASSERT_GE2(MBOX%LATLON(132, 271), 1)
  CALL ASSERT_GE2(MBOX%LATLON(131, 271), 1)  ! lat = 40, lon = -90
  CALL ASSERT_GE2(MBOX%LATLON(130, 271), 1)
  CALL ASSERT_GE2(MBOX%LATLON(129, 271), 1)
  CALL ASSERT_GE2(MBOX%LATLON(128, 271), 1)
  CALL ASSERT_GE2(MBOX%LATLON(127, 271), 1)
  CALL ASSERT_EQ2(MBOX%LATLON(126, 271), 0)

  ! repeat with automatic determination.
  CALL MBOX%CLEAR()
  DIST = 0.0

  ! with no hit, the ring distance will be based on the grid size.
  CALL MBOX%SETRNG(40.0, -90.0, 4, DIST)

  CALL ASSERT_FLOAT_EQ2(DIST, 20.0)

  CALL ASSERT_EQ2(MBOX%LATLON(133, 271), 0)
  CALL ASSERT_EQ2(MBOX%LATLON(132, 271), 0)
  CALL ASSERT_GE2(MBOX%LATLON(131, 271), 1)  ! lat = 40, lon = -90
  CALL ASSERT_GE2(MBOX%LATLON(130, 271), 1)
  CALL ASSERT_EQ2(MBOX%LATLON(129, 271), 0)

  ! cleanup
  CALL MBOX%FREE()
END SUBROUTINE

!----------------------------------------------------------------------
! Test GETEXT()
!----------------------------------------------------------------------
SUBROUTINE TEST_GETEXT()
  USE mapbox
  IMPLICIT NONE
  INCLUDE 'utests.inc'

  TYPE(CMAPBOX)      :: MBOX
  REAL               :: PLAT, PLON, XLAT, XLON

  ! set up
  CALL MBOX%ALLOC(-90.0, 0.0, 181.0, 360.0, 1.0, 'alloc failed')
  CALL MBOX%CLEAR()

  ! when no hit is set
  CALL MBOX%GETEXT(PLAT, PLON, XLAT, XLON)

  CALL ASSERT_FLOAT_EQ2(PLAT, -90.0)
  CALL ASSERT_FLOAT_EQ2(PLON,   0.0)
  CALL ASSERT_FLOAT_EQ2(XLAT,   1.0)
  CALL ASSERT_FLOAT_EQ2(XLON,   1.0)

  ! two hits
  CALL MBOX%CLEAR()

  CALL MBOX%ADDPNT(40.0, -90.0)
  CALL MBOX%ADDPNT(41.0, -89.0)

  CALL MBOX%GETEXT(PLAT, PLON, XLAT, XLON)

  CALL ASSERT_FLOAT_EQ2(PLAT,  40.0)
  CALL ASSERT_FLOAT_EQ2(PLON, 270.0)
  CALL ASSERT_FLOAT_EQ2(XLAT,   2.0)
  CALL ASSERT_FLOAT_EQ2(XLON,   2.0)

  ! two hits across longitude bounds
  CALL MBOX%CLEAR()

  CALL MBOX%ADDPNT(40.0,  179.0)
  CALL MBOX%ADDPNT(40.0, -179.0)

  CALL MBOX%GETEXT(PLAT, PLON, XLAT, XLON)

  CALL ASSERT_FLOAT_EQ2(PLAT,  40.0)
  CALL ASSERT_FLOAT_EQ2(PLON, 179.0)
  CALL ASSERT_FLOAT_EQ2(XLAT,   1.0)
  CALL ASSERT_FLOAT_EQ2(XLON,   3.0)

  ! cleanup
  CALL MBOX%FREE()
END SUBROUTINE

!----------------------------------------------------------------------
! Test CALCBX()
!----------------------------------------------------------------------
SUBROUTINE TEST_CALCBX()
  USE mapbox
  IMPLICIT NONE
  INCLUDE 'utests.inc'

  TYPE(CMAPBOX)      :: MBOX

  ! set up
  CALL MBOX%ALLOC(-90.0, 0.0, 181.0, 360.0, 1.0, 'alloc failed')
  CALL MBOX%CLEAR()

  ! 4 x 6 plume whose left-bottom corner is at lat = 40.0, lon = -90.0
  MBOX%LATLON(131:134, 271:276) = 1

  CALL MBOX%CALCBX()

  CALL ASSERT_EQ2(MBOX%NPTS, 5)
  CALL ASSERT_FLOAT_EQ2(MBOX%PTS(1,1),  42.0        )
  CALL ASSERT_FLOAT_EQ2(MBOX%PTS(2,1), -87.0 + 360.0)
  CALL ASSERT_FLOAT_EQ2(MBOX%PTS(1,2),  42.0        )
  CALL ASSERT_FLOAT_EQ2(MBOX%PTS(2,2), -90.0 + 360.0)
  CALL ASSERT_FLOAT_EQ2(MBOX%PTS(1,3),  42.0        )
  CALL ASSERT_FLOAT_EQ2(MBOX%PTS(2,3), -84.0 + 360.0)
  CALL ASSERT_FLOAT_EQ2(MBOX%PTS(1,4),  40.0        )
  CALL ASSERT_FLOAT_EQ2(MBOX%PTS(2,4), -87.0 + 360.0)
  CALL ASSERT_FLOAT_EQ2(MBOX%PTS(1,5),  44.0        )
  CALL ASSERT_FLOAT_EQ2(MBOX%PTS(2,5), -87.0 + 360.0)

  ! set a map hold at the upper right corner of the plume.
  ! the size of the resulting bounding box is 8 x 12.
  CALL MBOX%HOLDAT(44.0, -84.0)

  CALL MBOX%CALCBX()

  CALL ASSERT_EQ2(MBOX%NPTS, 5)
  CALL ASSERT_FLOAT_EQ2(MBOX%PTS(1,1),  44.0        )
  CALL ASSERT_FLOAT_EQ2(MBOX%PTS(2,1), -84.0 + 360.0)
  CALL ASSERT_FLOAT_EQ2(MBOX%PTS(1,2),  44.0        )
  CALL ASSERT_FLOAT_EQ2(MBOX%PTS(2,2), -90.0 + 360.0)
  CALL ASSERT_FLOAT_EQ2(MBOX%PTS(1,3),  44.0        )
  CALL ASSERT_FLOAT_EQ2(MBOX%PTS(2,3), -78.0 + 360.0)
  CALL ASSERT_FLOAT_EQ2(MBOX%PTS(1,4),  40.0        )
  CALL ASSERT_FLOAT_EQ2(MBOX%PTS(2,4), -84.0 + 360.0)
  CALL ASSERT_FLOAT_EQ2(MBOX%PTS(1,5),  48.0        )
  CALL ASSERT_FLOAT_EQ2(MBOX%PTS(2,5), -84.0 + 360.0)

  ! cleanup
  CALL MBOX%FREE()
END SUBROUTINE

!----------------------------------------------------------------------
! Test FREE()
!----------------------------------------------------------------------
SUBROUTINE TEST_FREE()
  USE mapbox
  IMPLICIT NONE
  INCLUDE 'utests.inc'

  TYPE(CMAPBOX)      :: MBOX

  ! set up
  CALL MBOX%ALLOC(-90.0, 0.0, 181.0, 360.0, 1.0, 'alloc failed')

  ! test
  CALL ASSERT_TRUE1(MBOX%INITQ())

  CALL MBOX%FREE()

  CALL ASSERT_FALSE1(MBOX%INITQ())

  ! cleanup is not necessary
END SUBROUTINE
