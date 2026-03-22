!======================================================================
! helper subroutines for unit tests
!----------------------------------------------------------------------
! ASSERT_EQ() - integer-integer or real-real equaliy test.
! ASSERT_GT()
! ASSERT_LT()
! ASSERT_GE()
! ASSERT_LE()
! ASSERT_FLOAT_EQ()
! ASSERT_TRUE()
! ASSERT_FALSE()
!======================================================================

!----------------------------------------------------------------------
! compare two integer values
!----------------------------------------------------------------------
SUBROUTINE ASSERT_EQ_II(FNAME, LINENO, VALU, XPECT)
  IMPLICIT NONE
  CHARACTER*(*),  INTENT(IN) :: FNAME
  INTEGER,        INTENT(IN) :: LINENO
  INTEGER,        INTENT(IN) :: VALU, XPECT

  IF (VALU .NE. XPECT) THEN
     PRINT *, FNAME, ', line', LINENO, 'ASSERT_EQ failed: ', &
              'VALUE', VALU, ', EXPECTED', XPECT
     STOP 2
  END IF
END SUBROUTINE

!----------------------------------------------------------------------
! compare two real values
!----------------------------------------------------------------------
SUBROUTINE ASSERT_EQ_RR(FNAME, LINENO, VALU, XPECT)
  IMPLICIT NONE
  CHARACTER*(*),  INTENT(IN) :: FNAME
  REAL,           INTENT(IN) :: LINENO
  REAL,           INTENT(IN) :: VALU, XPECT

  IF (VALU .NE. XPECT) THEN
     PRINT *, FNAME, ', line', LINENO, 'ASSERT_EQ failed: ', &
              'VALUE', VALU, ', EXPECTED', XPECT
     STOP 2
  END IF
END SUBROUTINE

!----------------------------------------------------------------------
! compare two integer values
!----------------------------------------------------------------------
SUBROUTINE ASSERT_GT_II(FNAME, LINENO, A, B)
  IMPLICIT NONE
  CHARACTER*(*),  INTENT(IN) :: FNAME
  INTEGER,        INTENT(IN) :: LINENO
  INTEGER,        INTENT(IN) :: A, B

  IF (A .LE. B) THEN
     PRINT *, FNAME, ', line', LINENO, 'ASSERT_GT failed: ', &
              A, '> ', B
     STOP 2
  END IF
END SUBROUTINE

!----------------------------------------------------------------------
! compare two real values
!----------------------------------------------------------------------
SUBROUTINE ASSERT_GT_RR(FNAME, LINENO, A, B)
  IMPLICIT NONE
  CHARACTER*(*),  INTENT(IN) :: FNAME
  REAL,           INTENT(IN) :: LINENO
  REAL,           INTENT(IN) :: A, B

  IF (A .LE. B) THEN
     PRINT *, FNAME, ', line', LINENO, 'ASSERT_GT failed: ', &
              A, '> ', B
     STOP 2
  END IF
END SUBROUTINE

!----------------------------------------------------------------------
! compare two integer values
!----------------------------------------------------------------------
SUBROUTINE ASSERT_LT_II(FNAME, LINENO, A, B)
  IMPLICIT NONE
  CHARACTER*(*),  INTENT(IN) :: FNAME
  INTEGER,        INTENT(IN) :: LINENO
  INTEGER,        INTENT(IN) :: A, B

  IF (A .GE. B) THEN
     PRINT *, FNAME, ', line', LINENO, 'ASSERT_LT failed: ', &
              A, '< ', B
     STOP 2
  END IF
END SUBROUTINE

!----------------------------------------------------------------------
! compare two real values
!----------------------------------------------------------------------
SUBROUTINE ASSERT_LT_RR(FNAME, LINENO, A, B)
  IMPLICIT NONE
  CHARACTER*(*),  INTENT(IN) :: FNAME
  REAL,           INTENT(IN) :: LINENO
  REAL,           INTENT(IN) :: A, B

  IF (A .GE. B) THEN
     PRINT *, FNAME, ', line', LINENO, 'ASSERT_LT failed: ', &
              A, '< ', B
     STOP 2
  END IF
END SUBROUTINE

!----------------------------------------------------------------------
! compare two integer values
!----------------------------------------------------------------------
SUBROUTINE ASSERT_GE_II(FNAME, LINENO, A, B)
  IMPLICIT NONE
  CHARACTER*(*),  INTENT(IN) :: FNAME
  INTEGER,        INTENT(IN) :: LINENO
  INTEGER,        INTENT(IN) :: A, B

  IF (A .LT. B) THEN
     PRINT *, FNAME, ', line', LINENO, 'ASSERT_GE failed: ', &
              A, '>= ', B
     STOP 2
  END IF
END SUBROUTINE

!----------------------------------------------------------------------
! compare two real values
!----------------------------------------------------------------------
SUBROUTINE ASSERT_GE_RR(FNAME, LINENO, A, B)
  IMPLICIT NONE
  CHARACTER*(*),  INTENT(IN) :: FNAME
  REAL,           INTENT(IN) :: LINENO
  REAL,           INTENT(IN) :: A, B

  IF (A .LT. B) THEN
     PRINT *, FNAME, ', line', LINENO, 'ASSERT_GE failed: ', &
              A, '>= ', B
     STOP 2
  END IF
END SUBROUTINE

!----------------------------------------------------------------------
! compare two integer values
!----------------------------------------------------------------------
SUBROUTINE ASSERT_LE_II(FNAME, LINENO, A, B)
  IMPLICIT NONE
  CHARACTER*(*),  INTENT(IN) :: FNAME
  INTEGER,        INTENT(IN) :: LINENO
  INTEGER,        INTENT(IN) :: A, B

  IF (A .GT. B) THEN
     PRINT *, FNAME, ', line', LINENO, 'ASSERT_LE failed: ', &
              A, '<= ', B
     STOP 2
  END IF
END SUBROUTINE

!----------------------------------------------------------------------
! compare two real values
!----------------------------------------------------------------------
SUBROUTINE ASSERT_LE_RR(FNAME, LINENO, A, B)
  IMPLICIT NONE
  CHARACTER*(*),  INTENT(IN) :: FNAME
  REAL,           INTENT(IN) :: LINENO
  REAL,           INTENT(IN) :: A, B

  IF (A .GT. B) THEN
     PRINT *, FNAME, ', line', LINENO, 'ASSERT_LE failed: ', &
              A, '<= ', B
     STOP 2
  END IF
END SUBROUTINE

!----------------------------------------------------------------------
! test if a logical value is false
!----------------------------------------------------------------------
SUBROUTINE ASSERT_FALSE(FNAME, LINENO, VALU)
  IMPLICIT NONE
  CHARACTER*(*),  INTENT(IN) :: FNAME
  INTEGER,        INTENT(IN) :: LINENO
  LOGICAL,        INTENT(IN) :: VALU

  IF (VALU) THEN
     PRINT *, FNAME, ', line', LINENO, 'ASSERT_FALSE failed: ', &
              'VALUE', VALU
     STOP 2
  END IF
END SUBROUTINE

!----------------------------------------------------------------------
! test if a logical value is true
!----------------------------------------------------------------------
SUBROUTINE ASSERT_TRUE(FNAME, LINENO, VALU)
  IMPLICIT NONE
  CHARACTER*(*),  INTENT(IN) :: FNAME
  INTEGER,        INTENT(IN) :: LINENO
  LOGICAL,        INTENT(IN) :: VALU

  IF (.NOT. VALU) THEN
     PRINT *, FNAME, ', line', LINENO, 'ASSERT_TRUE failed: ', &
              'VALUE', VALU
     STOP 2
  END IF
END SUBROUTINE

!----------------------------------------------------------------------
! compare two real values within a tolerance
!----------------------------------------------------------------------
SUBROUTINE ASSERT_FLOAT_EQ(FNAME, LINENO, VALU, XPECT, EPS)
  IMPLICIT NONE
  CHARACTER*(*),  INTENT(IN) :: FNAME
  INTEGER,        INTENT(IN) :: LINENO
  REAL,           INTENT(IN) :: VALU, XPECT
  REAL, OPTIONAL, INTENT(IN) :: EPS

  REAL            :: TOL = 1.0e-5

  IF (PRESENT(EPS)) THEN
     TOL = EPS
  END IF

  IF (ABS(VALU - XPECT) .GT. TOL) THEN
     PRINT *, FNAME, ', line', LINENO, 'ASSERT_FLOAT_EQ failed: ', &
              'VALUE', VALU, ', EXPECTED', XPECT, ', TOLERANCE', TOL
     STOP 2
  END IF
END SUBROUTINE

!----------------------------------------------------------------------
! compare two double-precision values within a tolerance
!----------------------------------------------------------------------
SUBROUTINE ASSERT_DOUBLE_EQ(FNAME, LINENO, VALU, XPECT, EPS)
  IMPLICIT NONE
  CHARACTER*(*),              INTENT(IN) :: FNAME
  INTEGER,                    INTENT(IN) :: LINENO
  DOUBLE PRECISION,           INTENT(IN) :: VALU, XPECT
  DOUBLE PRECISION, OPTIONAL, INTENT(IN) :: EPS

  DOUBLE PRECISION                       :: TOL = 1.0e-10

  IF (PRESENT(EPS)) THEN
     TOL = EPS
  END IF

  IF (ABS(VALU - XPECT) .GT. TOL) THEN
     PRINT *, FNAME, ', line', LINENO, 'ASSERT_DOUBLE_EQ failed: ', &
              'VALUE', VALU, ', EXPECTED', XPECT, ', TOLERANCE', TOL
     STOP 2
  END IF
END SUBROUTINE

!----------------------------------------------------------------------
! compare two strings
!----------------------------------------------------------------------
SUBROUTINE ASSERT_STR_EQ(FNAME, LINENO, VALU, XPECT)
  IMPLICIT NONE
  CHARACTER*(*),  INTENT(IN) :: FNAME
  INTEGER,        INTENT(IN) :: LINENO
  CHARACTER*(*),  INTENT(IN) :: VALU, XPECT

  ! match the string length too.
  IF (LEN(VALU).NE.LEN(XPECT) .OR. VALU.NE.XPECT) THEN
     PRINT *, FNAME, ', line', LINENO, 'ASSERT_STR_EQ failed: ', &
              'VALUE<' // VALU // '> len', LEN(VALU),            &
              ', EXPECTED<' // XPECT // '> len', LEN(XPECT)
     STOP 2
  END IF
END SUBROUTINE
