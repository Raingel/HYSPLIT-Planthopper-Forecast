!----------------------------------------------------------------------
! named interface ASSERT_EQ for function overloading
!----------------------------------------------------------------------
INTERFACE ASSERT_EQ

  SUBROUTINE ASSERT_EQ_II(FNAME, LINENO, VALU, XPECT)
     IMPLICIT NONE
     CHARACTER*(*),  INTENT(IN) :: FNAME
     INTEGER,        INTENT(IN) :: LINENO
     INTEGER,        INTENT(IN) :: VALU, XPECT
  END SUBROUTINE

  SUBROUTINE ASSERT_EQ_RR(FNAME, LINENO, VALU, XPECT)
     IMPLICIT NONE
     CHARACTER*(*),  INTENT(IN) :: FNAME
     INTEGER,        INTENT(IN) :: LINENO
     REAL,           INTENT(IN) :: VALU, XPECT
  END SUBROUTINE

END INTERFACE

!----------------------------------------------------------------------
! named interface ASSERT_GT for function overloading
!----------------------------------------------------------------------
INTERFACE ASSERT_GT

  SUBROUTINE ASSERT_GT_II(FNAME, LINENO, A, B)
     IMPLICIT NONE
     CHARACTER*(*),  INTENT(IN) :: FNAME
     INTEGER,        INTENT(IN) :: LINENO
     INTEGER,        INTENT(IN) :: A, B
  END SUBROUTINE

  SUBROUTINE ASSERT_GT_RR(FNAME, LINENO, A, B)
     IMPLICIT NONE
     CHARACTER*(*),  INTENT(IN) :: FNAME
     INTEGER,        INTENT(IN) :: LINENO
     REAL,           INTENT(IN) :: A, B
  END SUBROUTINE

END INTERFACE

!----------------------------------------------------------------------
! named interface ASSERT_LT for function overloading
!----------------------------------------------------------------------
INTERFACE ASSERT_LT

  SUBROUTINE ASSERT_LT_II(FNAME, LINENO, A, B)
     IMPLICIT NONE
     CHARACTER*(*),  INTENT(IN) :: FNAME
     INTEGER,        INTENT(IN) :: LINENO
     INTEGER,        INTENT(IN) :: A, B
  END SUBROUTINE

  SUBROUTINE ASSERT_LT_RR(FNAME, LINENO, A, B)
     IMPLICIT NONE
     CHARACTER*(*),  INTENT(IN) :: FNAME
     INTEGER,        INTENT(IN) :: LINENO
     REAL,           INTENT(IN) :: A, B
  END SUBROUTINE

END INTERFACE

!----------------------------------------------------------------------
! named interface ASSERT_GE for function overloading
!----------------------------------------------------------------------
INTERFACE ASSERT_GE

  SUBROUTINE ASSERT_GE_II(FNAME, LINENO, A, B)
     IMPLICIT NONE
     CHARACTER*(*),  INTENT(IN) :: FNAME
     INTEGER,        INTENT(IN) :: LINENO
     INTEGER,        INTENT(IN) :: A, B
  END SUBROUTINE

  SUBROUTINE ASSERT_GE_RR(FNAME, LINENO, A, B)
     IMPLICIT NONE
     CHARACTER*(*),  INTENT(IN) :: FNAME
     INTEGER,        INTENT(IN) :: LINENO
     REAL,           INTENT(IN) :: A, B
  END SUBROUTINE

END INTERFACE

!----------------------------------------------------------------------
! named interface ASSERT_LE for function overloading
!----------------------------------------------------------------------
INTERFACE ASSERT_LE

  SUBROUTINE ASSERT_LE_II(FNAME, LINENO, A, B)
     IMPLICIT NONE
     CHARACTER*(*),  INTENT(IN) :: FNAME
     INTEGER,        INTENT(IN) :: LINENO
     INTEGER,        INTENT(IN) :: A, B
  END SUBROUTINE

  SUBROUTINE ASSERT_LE_RR(FNAME, LINENO, A, B)
     IMPLICIT NONE
     CHARACTER*(*),  INTENT(IN) :: FNAME
     INTEGER,        INTENT(IN) :: LINENO
     REAL,           INTENT(IN) :: A, B
  END SUBROUTINE

END INTERFACE

!----------------------------------------------------------------------
! unnamed interface
!----------------------------------------------------------------------
INTERFACE

  SUBROUTINE ASSERT_FALSE(FNAME, LINENO, VALU)
     IMPLICIT NONE
     CHARACTER*(*),  INTENT(IN) :: FNAME
     INTEGER,        INTENT(IN) :: LINENO
     LOGICAL,        INTENT(IN) :: VALU
  END SUBROUTINE

  SUBROUTINE ASSERT_TRUE(FNAME, LINENO, VALU)
     IMPLICIT NONE
     CHARACTER*(*),  INTENT(IN) :: FNAME
     INTEGER,        INTENT(IN) :: LINENO
     LOGICAL,        INTENT(IN) :: VALU
  END SUBROUTINE

  SUBROUTINE ASSERT_FLOAT_EQ(FNAME, LINENO, VALU, XPECT, EPS)
     IMPLICIT NONE
     CHARACTER*(*),  INTENT(IN) :: FNAME
     INTEGER,        INTENT(IN) :: LINENO
     REAL,           INTENT(IN) :: VALU, XPECT
     REAL, OPTIONAL, INTENT(IN) :: EPS
  END SUBROUTINE

  SUBROUTINE ASSERT_DOUBLE_EQ(FNAME, LINENO, VALU, XPECT, EPS)
     IMPLICIT NONE
     CHARACTER*(*),              INTENT(IN) :: FNAME
     INTEGER,                    INTENT(IN) :: LINENO
     DOUBLE PRECISION,           INTENT(IN) :: VALU, XPECT
     DOUBLE PRECISION, OPTIONAL, INTENT(IN) :: EPS
  END SUBROUTINE
 
  SUBROUTINE ASSERT_STR_EQ(FNAME, LINENO, VALU, XPECT)
     IMPLICIT NONE
     CHARACTER*(*),  INTENT(IN) :: FNAME
     INTEGER,        INTENT(IN) :: LINENO
     CHARACTER*(*),  INTENT(IN) :: VALU, XPECT
  END SUBROUTINE

END INTERFACE
