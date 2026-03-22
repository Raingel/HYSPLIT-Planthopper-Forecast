PROGRAM teststrutl
!======================================================================
! unit tests for string utilities in strutl.f
!----------------------------------------------------------------------
! Change History:
! 08 Oct 2021 (SYZ) initial
!=======================================================================

! macros to save keystrokes
#include "utests.h"

  IMPLICIT NONE
  INCLUDE 'strutl.inc'
  INCLUDE 'utests.inc'

  CHARACTER*(80) BUF80

!------------------------------------------------------------------------------
! Test FRMNOS()
!------------------------------------------------------------------------------
  CALL ASSERT_STR_EQ2(FRMNOS(   1),   '01')
  CALL ASSERT_STR_EQ2(FRMNOS(   9),   '09')
  CALL ASSERT_STR_EQ2(FRMNOS(  10),   '10')
  CALL ASSERT_STR_EQ2(FRMNOS(  99),   '99')
  CALL ASSERT_STR_EQ2(FRMNOS( 100),  '100')
  CALL ASSERT_STR_EQ2(FRMNOS( 999),  '999')
  CALL ASSERT_STR_EQ2(FRMNOS(1000), '1000')
  CALL ASSERT_STR_EQ2(FRMNOS(9999), '9999')

  ! Tests with optional argument
  CALL ASSERT_STR_EQ2(FRMNOS(   1, 2),   '01')
  CALL ASSERT_STR_EQ2(FRMNOS(   1, 3),  '001')
  CALL ASSERT_STR_EQ2(FRMNOS(   1, 4), '0001')
  CALL ASSERT_STR_EQ2(FRMNOS(  10, 2),   '10')
  CALL ASSERT_STR_EQ2(FRMNOS(  10, 3),  '010')
  CALL ASSERT_STR_EQ2(FRMNOS(  10, 4), '0010')
  CALL ASSERT_STR_EQ2(FRMNOS( 100, 3),  '100')
  CALL ASSERT_STR_EQ2(FRMNOS( 100, 4), '0100')

!------------------------------------------------------------------------------
! Test LENSTR()
!------------------------------------------------------------------------------
  BUF80 = 'TEST'
  CALL ASSERT_EQ2(LEN(BUF80), 80)
  CALL ASSERT_EQ2(LENSTR(BUF80, LEN(BUF80)), 4)

  BUF80 = '  TEST    '
  CALL ASSERT_EQ2(LEN(BUF80), 80)
  CALL ASSERT_EQ2(LENSTR(BUF80, LEN(BUF80)), 6)

  BUF80 = '    '
  CALL ASSERT_EQ2(LEN(BUF80), 80)
  CALL ASSERT_EQ2(LENSTR(BUF80, LEN(BUF80)), 0)

END PROGRAM
