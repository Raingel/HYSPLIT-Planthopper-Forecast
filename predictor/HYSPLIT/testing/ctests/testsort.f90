PROGRAM testsort
!======================================================================
! unit test the sort() and xsort() subroutines.
!----------------------------------------------------------------------
! PROGRAM HISTORY LOG:
!   LAST REVISED: 09 Aug 2023 (SYZ) - initial
!=======================================================================

! macros to save keystrokes
#include "utests.h"

   IMPLICIT NONE

   INTERFACE
      SUBROUTINE TEST_XSORT()
      END SUBROUTINE

      SUBROUTINE TEST_XSORT_COMP()
      END SUBROUTINE

      SUBROUTINE TEST_SORT_COMP()
      END SUBROUTINE
   END INTERFACE

   CALL TEST_XSORT()
   CALL TEST_XSORT_COMP()

   CALL TEST_SORT_COMP()

END PROGRAM

!----------------------------------------------------------------------
! Test XSORT()
!----------------------------------------------------------------------
SUBROUTINE TEST_XSORT()
   IMPLICIT NONE
   INCLUDE 'utests.inc'
   INCLUDE 'sort.inc'

   INTEGER NPTS
   INTEGER IDX(1000)
   REAL    VAL(1000)

   !----------------------------------------------------------------------
   ! two values
   NPTS=2
   IDX=0
   VAL(1)=2.0
   VAL(2)=1.0
   CALL XSORT(NPTS,VAL,IDX)
   ! check the output
   CALL ASSERT_EQ2(IDX(1), 2)
   CALL ASSERT_EQ2(IDX(2), 1)
   CALL ASSERT_FLOAT_EQ2(VAL(1), 2.0)
   CALL ASSERT_FLOAT_EQ2(VAL(2), 1.0)
END SUBROUTINE

!----------------------------------------------------------------------
! Test XSORT() by comparing its outputs with those from the original
! XSORT() that used GOTO statements.
!----------------------------------------------------------------------
SUBROUTINE TEST_XSORT_COMP()
   IMPLICIT NONE
   INCLUDE 'utests.inc'
   INCLUDE 'sort.inc'

   INTEGER NPTS,K,T
   INTEGER IDX(1000),IDX2(1000),TEMP(1000)
   REAL    VAL(1000),VAL2(1000)

   !----------------------------------------------------------------------
   INTERFACE
   SUBROUTINE XSORT_GOTO(NPTS,CVAL,IDX,TMP)
      IMPLICIT NONE

      INTEGER,  INTENT(IN)  :: NPTS
      REAL,     INTENT(IN)  :: CVAL(:)
      INTEGER,  INTENT(OUT) :: IDX(:)
      INTEGER,  INTENT(OUT) :: TMP(:)
   END SUBROUTINE
   END INTERFACE

   !----------------------------------------------------------------------
   T=TIME()
   CALL SRAND(T)

   DO NPTS=2,1000
      PRINT *,'Running xsort() comparison with NPTS',NPTS
      DO K=1,NPTS
         VAL(K)=RAND(0)*100.0  ! between 0 and 100.0
         VAL2(K)=VAL(K)
      ENDDO
      !WRITE(*,'(3X,5E9.2)') (VAL(K),K=1,MIN(5,NPTS))

      CALL XSORT(NPTS, VAL, IDX)
      CALL XSORT_GOTO(NPTS, VAL2, IDX2, TEMP)

      DO K=1,NPTS
         CALL ASSERT_EQ2(IDX(K), IDX2(K))
         CALL ASSERT_FLOAT_EQ2(VAL(IDX(K)), VAL2(IDX2(K)))
      ENDDO
   ENDDO
END SUBROUTINE

!----------------------------------------------------------------------
! Test SORT() by comparing its outputs with those from the original
! SORT() that used GOTO statements.
!----------------------------------------------------------------------
SUBROUTINE TEST_SORT_COMP()
   IMPLICIT NONE
   INCLUDE 'utests.inc'
   INCLUDE 'sort.inc'

   INTEGER NPTS,K,T
   REAL    VAL(1000),VAL2(1000)
   REAL    DSORT(99),DSORT2(99)

   !----------------------------------------------------------------------
   INTERFACE
   SUBROUTINE SORT_GOTO(NPTS,CVAL,DSORT)
      IMPLICIT NONE

      INTEGER,  INTENT(IN)  :: NPTS
      REAL,     INTENT(IN)  :: CVAL(:)
      REAL,     INTENT(OUT) :: DSORT(99)
   END SUBROUTINE
   END INTERFACE

   !----------------------------------------------------------------------
   T=TIME()
   CALL SRAND(T)

   DO NPTS=2,1000
      PRINT *,'Running sort() comparison with NPTS',NPTS
      DO K=1,NPTS
         VAL(K)=RAND(0)*100.0  ! between 0 and 100.0
         VAL2(K)=VAL(K)
      ENDDO
      !WRITE(*,'(3X,5E9.2)') (VAL(K),K=1,MIN(5,NPTS))

      CALL SORT(NPTS, VAL, DSORT)
      CALL SORT_GOTO(NPTS, VAL2, DSORT2)

      DO K=1,99
         CALL ASSERT_FLOAT_EQ2(DSORT(K), DSORT2(K))
      ENDDO
   ENDDO
END SUBROUTINE


!------------------------------------------------------------------------------
! binary sortting routine xsort does not re-arrange data in the cval array
! but sorts the index array 
!------------------------------------------------------------------------------
SUBROUTINE XSORT_GOTO(NPTS,CVAL,NDEX,TEMP)

    IMPLICIT NONE

    INTEGER, INTENT(IN)    :: NPTS
    REAL,    INTENT(IN)    :: CVAL(:)
    INTEGER, INTENT(OUT)   :: NDEX(:)
    INTEGER, INTENT(OUT)   :: TEMP(:)

    INTEGER :: nlen,len1,len2,indx1,indx2
    INTEGER :: i,i1,i2,i3,l1,l2

!   initialize the index array before sort
    DO I=1,NPTS
       NDEX(I)=I
    END DO

    NLEN=1
100 IF(NLEN.GE.NPTS)GO TO 800

       I1=1
       I2=I1+NLEN
       I3=1
200       L1=0
          L2=0
          LEN1=NPTS-I1+1
          IF(LEN1.GT.NLEN)LEN1=NLEN
          LEN2=NPTS-I2+1
          IF(LEN2.GT.NLEN)LEN2=NLEN
250       INDX1=NDEX(I1+L1)
          INDX2=NDEX(I2+L2)

          IF(CVAL(INDX1).GT.CVAL(INDX2))GO TO 400

300       TEMP(I3)=NDEX(I1+L1)
          L1=L1+1
          I3=I3+1
          GO TO 500

400       CONTINUE
          TEMP(I3)=NDEX(I2+L2)
          L2=L2+1
          I3=I3+1

500       IF(I3.GT.NPTS)GO TO 550
          IF(L1.LT.LEN1.AND.L2.LT.LEN2)GO TO 250
          IF(L1.EQ.LEN1.AND.L2.LT.LEN2)GO TO 400
          IF(L1.LT.LEN1.AND.L2.EQ.LEN2)GO TO 300
          I1=I3
          I2=I1+NLEN
          IF(I2.LE.NPTS)GO TO 200
          L1=0
          GO TO 300

550    DO I=1,NPTS
          NDEX(I)=TEMP(I)
       END DO
       NLEN=NLEN*2
       GO TO 100

800 RETURN

END SUBROUTINE xsort_goto


!-------------------------------------------------------------------- 
! Ascending order binary sort which returns the sorted values in
! array DSORT by cumulative percent from 1 to 99.  For instance, 
! DSORT(50) returns the median value, while DSORT(25 & 75) return the
! quartile values. Routine requres 2**N passes through the data, 
! where N equals the number of data points.
!--------------------------------------------------------------------
! Last Revised: 18 Jul 2001
!               14 Aug 2002
!--------------------------------------------------------------------

SUBROUTINE SORT_GOTO(NUMPTS,VALUE,DSORT)

  IMPLICIT NONE

  INTEGER,  INTENT(IN)  :: NUMPTS
  REAL,     INTENT(IN)  :: VALUE(:)
  REAL,     INTENT(OUT) :: DSORT(99)

  INTEGER, ALLOCATABLE :: TEMP (:)
  INTEGER, ALLOCATABLE :: INDX (:)

  REAL    :: FRAC
  INTEGER :: I,I1,I2,I3,L1,L2,KRET,NLEN,LEN1,LEN2,INDX1,INDX2

  IF(NUMPTS.LE.1)THEN
     WRITE(*,*)'Too few points to sort: ',NUMPTS
     RETURN
  END IF

  ALLOCATE (TEMP(NUMPTS),SOURCE=0,STAT=KRET)
  ALLOCATE (INDX(NUMPTS),SOURCE=0,STAT=KRET)

! initialize index array
  DO I=1,NUMPTS
     INDX(I)=I
  END DO

    NLEN=1
100 IF(NLEN.GE.NUMPTS)GO TO 800

    I1=1
    I2=I1+NLEN
    I3=1

200 L1=0
    L2=0
    LEN1=NUMPTS-I1+1
    IF(LEN1.GT.NLEN)LEN1=NLEN
    LEN2=NUMPTS-I2+1
    IF(LEN2.GT.NLEN)LEN2=NLEN

250 INDX1=INDX(I1+L1)
    INDX2=INDX(I2+L2)
    IF(VALUE(INDX1).GT.VALUE(INDX2))GO TO 400

300 TEMP(I3)=INDX(I1+L1)
    L1=L1+1
    I3=I3+1
    GO TO 500

400 CONTINUE
    TEMP(I3)=INDX(I2+L2)
    L2=L2+1
    I3=I3+1

500 IF(I3.GT.NUMPTS)GO TO 550
    IF(L1.LT.LEN1.AND.L2.LT.LEN2)GO TO 250
    IF(L1.EQ.LEN1.AND.L2.LT.LEN2)GO TO 400
    IF(L1.LT.LEN1.AND.L2.EQ.LEN2)GO TO 300
    I1=I3
    I2=I1+NLEN
    IF(I2.LE.NUMPTS)GO TO 200
    L1=0
    GO TO 300

550 DO I=1,NUMPTS
       INDX(I)=TEMP(I)
    END DO
    NLEN=NLEN*2
    GO TO 100

800 CONTINUE

! return percentile values at 1% increments
  DO I=1,99
     FRAC=I/100.0
     DSORT(I)=VALUE(INDX(MAX(1,INT(FRAC*NUMPTS))))
  END DO

DEALLOCATE (temp,indx)

END SUBROUTINE SORT_GOTO
