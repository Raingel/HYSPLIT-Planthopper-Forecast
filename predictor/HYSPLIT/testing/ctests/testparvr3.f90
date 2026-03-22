PROGRAM testparvr3
!======================================================================
! testing parvr3.f
!----------------------------------------------------------------------
! Change History:
! 11 Aug 2023 (SYZ) initial
!=======================================================================

! macros to save keystrokes
#include "utests.h"

   IMPLICIT NONE
   INCLUDE 'parvr3.inc'
   INCLUDE 'utests.inc'

   INTEGER ISEED,K
   REAL V1,V2,SIGMA,VELOC,V2SR

   !----------------------------------------------------------------------
   ! Test PARVR3() - optional argument check
   !----------------------------------------------------------------------
   ISEED=TIME()
   SIGMA=10.0
   V2SR=-999.0
   CALL PARVR3(SIGMA,VELOC,ISEED,V2SR)
   CALL ASSERT_FLOAT_EQ2(VELOC, SIGMA*V2SR)

   !----------------------------------------------------------------------
   ! Test GASDV3() - sanity check
   !----------------------------------------------------------------------
   ISEED=TIME()
   DO K=1,1000
      ! GASDV3() internally generates two numbers. So call it twice.
      ! Number pairs cannot be zero at the same time.
      V1=GASDV3(ISEED)
      V2=GASDV3(ISEED)
      !PRINT *,'DEBUG:V1,V1,V1**2+V2**2:',V1,V2,V1*V1+V2*V2
      CALL ASSERT_FALSE1(V1.EQ.0 .AND. V2.EQ.0)
   ENDDO
  
END PROGRAM
