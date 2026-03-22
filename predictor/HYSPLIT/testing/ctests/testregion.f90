PROGRAM testregion
!======================================================================
! unit test the CREGION class and its methods.
!----------------------------------------------------------------------
! PROGRAM HISTORY LOG:
!   LAST REVISED: 15 Jun 2023 (SYZ) - initial
!=======================================================================

! macros to save keystrokes
#include "utests.h"

   IMPLICIT NONE

   INTERFACE
      SUBROUTINE TEST_CTOR()
      END SUBROUTINE

      SUBROUTINE TEST_UNION()
      END SUBROUTINE

      SUBROUTINE TEST_OFFSET()
      END SUBROUTINE
   END INTERFACE

!----------------------------------------------------------------------
! Test the CREGION class constructor
!----------------------------------------------------------------------
   CALL TEST_CTOR()

!----------------------------------------------------------------------
! Test each methods in the CREGION class
!----------------------------------------------------------------------
   CALL TEST_UNION()
   CALL TEST_OFFSET()

END PROGRAM

!----------------------------------------------------------------------
! Test CREGION constructor
!----------------------------------------------------------------------
SUBROUTINE TEST_CTOR()
   USE region
   IMPLICIT NONE
   INCLUDE 'utests.inc'

   TYPE(CREGION)      :: RGN

   ! check the initial values of class member variables.
   CALL ASSERT_EQ2(RGN%ILAT, 0)
   CALL ASSERT_EQ2(RGN%ILON, 0)
END SUBROUTINE

!----------------------------------------------------------------------
! Test UNION()
!----------------------------------------------------------------------
SUBROUTINE TEST_UNION()
   USE region
   IMPLICIT NONE
   INCLUDE 'utests.inc'

   TYPE(CREGION)      :: RGN,OTH

   ! two regions of the same size but offset by (1.5, 1.0) degrees
   RGN%NLAT=50
   RGN%NLON=60
   RGN%DLAT=0.5
   RGN%DLON=0.5
   RGN%CLAT=25.0
   RGN%CLON=-80.0
   OTH%NLAT=50
   OTH%NLON=60
   OTH%DLAT=0.5
   OTH%DLON=0.5
   OTH%CLAT=26.0
   OTH%CLON=-78.5

   CALL RGN%UNION(OTH)

   CALL ASSERT_EQ2(RGN%NLAT, 52)
   CALL ASSERT_EQ2(RGN%NLON, 63)
   CALL ASSERT_FLOAT_EQ2(RGN%DLAT, 0.5)
   CALL ASSERT_FLOAT_EQ2(RGN%DLON, 0.5)
   CALL ASSERT_FLOAT_EQ2(RGN%CLAT, 25.0)
   CALL ASSERT_FLOAT_EQ2(RGN%CLON, -80.0)

   ! two regions that wrap around longtidue=180.0
   RGN%NLAT=50
   RGN%NLON=60
   RGN%DLAT=0.5
   RGN%DLON=0.5
   RGN%CLAT=25.0
   RGN%CLON=150.0
   OTH%NLAT=50
   OTH%NLON=60
   OTH%DLAT=0.5
   OTH%DLON=0.5
   OTH%CLAT=26.0
   OTH%CLON=-179.0

   CALL RGN%UNION(OTH)

   CALL ASSERT_EQ2(RGN%NLAT, 52)
   CALL ASSERT_EQ2(RGN%NLON, 122)
   CALL ASSERT_FLOAT_EQ2(RGN%DLAT, 0.5)
   CALL ASSERT_FLOAT_EQ2(RGN%DLON, 0.5)
   CALL ASSERT_FLOAT_EQ2(RGN%CLAT, 25.0)
   CALL ASSERT_FLOAT_EQ2(RGN%CLON, 150.0)
END SUBROUTINE

!----------------------------------------------------------------------
! Test OFFSET()
!----------------------------------------------------------------------
SUBROUTINE TEST_OFFSET()
   USE region
   IMPLICIT NONE
   INCLUDE 'utests.inc'

   TYPE(CREGION)      :: RGN,OTH

   ! two regions of the same size but offset by (1.5, 1.0) degrees
   RGN%NLAT=50
   RGN%NLON=60
   RGN%DLAT=0.5
   RGN%DLON=0.5
   RGN%CLAT=25.0
   RGN%CLON=-80.0
   OTH%NLAT=50
   OTH%NLON=60
   OTH%DLAT=0.5
   OTH%DLON=0.5
   OTH%CLAT=26.0
   OTH%CLON=-78.5

   CALL RGN%OFFSET(OTH)

   CALL ASSERT_EQ2(RGN%ILAT, -2)
   CALL ASSERT_EQ2(RGN%ILON, -3)

END SUBROUTINE
