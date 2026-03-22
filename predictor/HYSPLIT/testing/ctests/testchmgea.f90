PROGRAM testchmgea
!======================================================================
! test the CHMGEA() subroutine
!----------------------------------------------------------------------
! Change History:
! 18 May 2022 (SYZ) initial
!=======================================================================

! macros to save keystrokes
#include "utests.h"

  IMPLICIT NONE
  INCLUDE 'limits.inc'
  INCLUDE 'DEFCONC.INC'
  INCLUDE 'utests.inc'
  INCLUDE 'chmgea.inc'

  TYPE(pset)   :: dirt(3)
  INTEGER      :: numtyp,kret,k
  REAL*8       :: ddt,concin(3),spcltemp,concout(3),rrk,rkj(3,3)
  INTEGER      :: klm

  CALL TMINIT()

  NUMTYP = 3

  DO K = 1, NUMTYP
     DIRT(k)%QRATE=1.0        ! emission rate (per hour)
     DIRT(k)%QHRS=1.0         ! hours of emission
     DIRT(k)%START%YR=22
     DIRT(k)%START%MO= 5
     DIRT(k)%START%DA=18
     DIRT(k)%START%HR= 6
     DIRT(k)%START%MN= 0
  ENDDO

!----------------------------------------------------------------------
! Test case 1 - a simple reaction: CHMA -> 1.5 CHMB + 2 CHMC.
!----------------------------------------------------------------------
  ! set up
  DIRT(1)%IDENT='CHMA'
  DIRT(2)%IDENT='CHMB'
  DIRT(3)%IDENT='CHMC'

  DDT = 1.0 ! minutes
  CONCIN(1) = 1000.0
  CONCIN(2) = 0.0
  CONCIN(3) = 0.0
  SPCLTEMP = 273.15 + 30.0
  KLM = 0
  RRK = 1.0
  RKJ = 1.0
  CONCOUT = 0

  CALL CHMGEA(DIRT,DDT,CONCIN,SPCLTEMP,NUMTYP,CONCOUT,KLM,RRK,RKJ)

  CALL ASSERT_DOUBLE_EQ2(CONCOUT(1), 1.3562634893737541D+02)
  CALL ASSERT_DOUBLE_EQ2(CONCOUT(2), 3.4588528320337321D+02)
  CALL ASSERT_DOUBLE_EQ2(CONCOUT(3), 4.6118037760449744D+02)
  CALL ASSERT_EQ2(KLM, 1)

END PROGRAM
