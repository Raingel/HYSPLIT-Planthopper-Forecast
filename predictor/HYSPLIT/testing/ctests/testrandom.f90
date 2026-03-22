PROGRAM testrandom
!======================================================================
! testing random.f
!----------------------------------------------------------------------
! Change History:
! 11 Aug 2023 (SYZ) initial
! 30 Oct 2023 (SYZ) checks numbers
!=======================================================================

! macros to save keystrokes
#include "utests.h"

   IMPLICIT NONE
   INCLUDE 'random.inc'
   INCLUDE 'utests.inc'

   INTERFACE
      SUBROUTINE TEST_GASDEV1()
      END SUBROUTINE
   END INTERFACE

   INTEGER ISEED,K
   REAL V1,V2
   INTEGER TIME

   !----------------------------------------------------------------------
   ! Test GASDEV1() - sanity check
   !----------------------------------------------------------------------
   ISEED=TIME()
   DO K=1,1000
      CALL GASDEV1(ISEED,V1,V2)
      !PRINT *,'DEBUG:V1,V1,V1**2+V2**2:',V1,V2,V1*V1+V2*V2
      CALL ASSERT_FALSE1(V1.EQ.0 .AND. V2.EQ.0)
   ENDDO

   !----------------------------------------------------------------------
   ! Test RAN3()
   !----------------------------------------------------------------------
   ISEED=TIME()
   DO K=1,1000
      V1=RAN3(ISEED)
      CALL ASSERT_GE2(V1,0.0)
      CALL ASSERT_LE2(V1,1.0)
   ENDDO

   !----------------------------------------------------------------------
   ! Test RAN3B()
   !----------------------------------------------------------------------
   ISEED=TIME()
   DO K=1,1000
      V1=RAN3B(ISEED)
      !PRINT *,'DEBUG:V1:',V1
      CALL ASSERT_GE2(V1,0.0)
      CALL ASSERT_LE2(V1,1.0)
   ENDDO

   !----------------------------------------------------------------------
   ! More tests follows
   !----------------------------------------------------------------------
   CALL TEST_GASDEV1()
END PROGRAM

!----------------------------------------------------------------------
! Test GASDEV1() to see if an expected sequence of numbers is generated
!----------------------------------------------------------------------
SUBROUTINE TEST_GASDEV1()
   IMPLICIT NONE
   INCLUDE 'utests.inc'
   INCLUDE 'random.inc'

   INTEGER IDUM
   REAL R1,R2

   IDUM=-1
   CALL GASDEV1(IDUM,R1,R2)
   CALL ASSERT_FLOAT_EQ2(R1,-0.994063)
   CALL ASSERT_FLOAT_EQ2(R2, 1.059816)
   CALL ASSERT_EQ2(IDUM,1)

   CALL GASDEV1(IDUM,R1,R2)
   CALL ASSERT_FLOAT_EQ2(R1, 0.574032)
   CALL ASSERT_FLOAT_EQ2(R2, 2.20898 )
   CALL ASSERT_EQ2(IDUM,1)

   CALL GASDEV1(IDUM,R1,R2)
   CALL ASSERT_FLOAT_EQ2(R1, 0.490218)
   CALL ASSERT_FLOAT_EQ2(R2,-0.303952)
   CALL ASSERT_EQ2(IDUM,1)
END SUBROUTINE
