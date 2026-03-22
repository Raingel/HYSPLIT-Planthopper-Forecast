PROGRAM testrdizsg
!======================================================================
! testing rdizsg.f
!----------------------------------------------------------------------
! Change History:
! 11 Aug 2023 (SYZ) initial
!=======================================================================

! macros to save keystrokes
#include "utests.h"

   IMPLICIT NONE
   INCLUDE 'rdizsg.inc'
   INCLUDE 'utests.inc'

   INTEGER iu,nzm,nlvl,ksfc,ret_code
   REAL zmdl,zsg(100),sfcl,aa,bb,cc

   !----------------------------------------------------------------------
   ! Test a happy path.
   !----------------------------------------------------------------------
   OPEN(20,FILE='rdizsg.txt',STATUS='UNKNOWN')
   WRITE(20,'(A)')'5 2 14000.0'  ! 5 levels, sfc index 2, ztop
   WRITE(20,'(A)')'ENDHEADER'
   WRITE(20,'(A)')'100.0'        ! height(1)
   WRITE(20,'(A)')'200.0'        ! height(2): sfc height
   WRITE(20,'(A)')'300.0'
   WRITE(20,'(A)')'400.0'
   WRITE(20,'(A)')'500.0'
   CLOSE(20)
  
   IU=10
   NZM=100
   NLVL=20
   ZSG=0.314
   KSFC=1               ! surface layer index
   ZMDL=10000.0         ! model top
   SFCL=10.0            ! surface layer top
   AA=0.1
   BB=0.2
   CC=0.3
   CALL RDIZSG('rdizsg.txt',IU,ZMDL,ZSG,NZM,NLVL,KSFC,SFCL,AA,BB,CC,RET_CODE)
   CALL ASSERT_EQ2(RET_CODE,0)
   CALL ASSERT_EQ2(NLVL,5)
   CALL ASSERT_EQ2(KSFC,2)
   CALL ASSERT_FLOAT_EQ2(ZMDL,14000.0)
   CALL ASSERT_FLOAT_EQ2(SFCL,200.0)
   CALL ASSERT_FLOAT_EQ2(ZSG(1),1.0-100.0/14000.0)
   CALL ASSERT_FLOAT_EQ2(ZSG(2),1.0-200.0/14000.0)
   CALL ASSERT_FLOAT_EQ2(ZSG(3),1.0-300.0/14000.0)
   CALL ASSERT_FLOAT_EQ2(ZSG(4),1.0-400.0/14000.0)
   CALL ASSERT_FLOAT_EQ2(ZSG(5),1.0-500.0/14000.0)

END PROGRAM
