PROGRAM testrdwhyb
!======================================================================
! testing rdwhyb.f
!----------------------------------------------------------------------
! Change History:
! 11 Aug 2023 (SYZ) initial
!=======================================================================

! macros to save keystrokes
#include "utests.h"

   IMPLICIT NONE
   INCLUDE 'rdwhyb.inc'
   INCLUDE 'utests.inc'

   INTEGER nz,npar,ret_code
   REAL wrfvcoords(100,3)

   !----------------------------------------------------------------------
   ! Test a happy path.
   !----------------------------------------------------------------------
   OPEN(20,FILE='rdwhyb.txt',STATUS='UNKNOWN')
   WRITE(20,'(A)')'5 3'  ! 5 levels, 3 params
   WRITE(20,'(A)')'ENDHEADER'
   WRITE(20,'(A)')'P1,P2,P3'  ! parameter names
   WRITE(20,'(A)')'1.0 1.1 1.2'
   WRITE(20,'(A)')'2.0 2.1 2.2'
   WRITE(20,'(A)')'3.0 3.1 3.2'
   WRITE(20,'(A)')'4.0 4.1 4.2'
   WRITE(20,'(A)')'5.0 5.1 5.2'
   CLOSE(20)
  
   NZ=5
   NPAR=3
   WRFVCOORDS=0.0
   RET_CODE=999
   CALL RDWHYB('rdwhyb.txt',NZ,NPAR,WRFVCOORDS,RET_CODE)
   CALL ASSERT_EQ2(RET_CODE,0)
   CALL ASSERT_FLOAT_EQ2(WRFVCOORDS(1,1),1.0)
   CALL ASSERT_FLOAT_EQ2(WRFVCOORDS(1,2),1.1)
   CALL ASSERT_FLOAT_EQ2(WRFVCOORDS(1,3),1.2)

   CALL ASSERT_FLOAT_EQ2(WRFVCOORDS(5,1),5.0)
   CALL ASSERT_FLOAT_EQ2(WRFVCOORDS(5,2),5.1)
   CALL ASSERT_FLOAT_EQ2(WRFVCOORDS(5,3),5.2)

END PROGRAM
