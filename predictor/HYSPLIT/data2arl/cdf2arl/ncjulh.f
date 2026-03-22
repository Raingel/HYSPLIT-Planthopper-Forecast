!===============================================================================
! Convert time plus delta time to time and return julian hour

SUBROUTINE ncjulh (iy,im,id,ih,ic,julh)

  IMPLICIT NONE

  INTEGER, INTENT(INOUT) :: iy, im, id, ih
  INTEGER, INTENT(IN)    :: ic
  INTEGER, INTENT(OUT)   :: julh

  INTEGER  :: NDM(12),NDT(12), NDAYS

  DATA NDM/31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31/
  DATA NDT/0 , 31, 59, 90,120,151,181,212,243,273,304,334/

! check for leap year
  NDM(2)=28

  IF(MOD(IY,100).EQ.0)THEN
!    leap year century test
     IF(MOD(IY,400).EQ.0)NDM(2)=29
  ELSE
!    standard leap year test
     IF(MOD(IY,4).EQ.0)NDM(2)=29
  END IF

! current hour incremented by IC
  IF(IC.GT.0)THEN
     IH=IH+IC
     DO WHILE (IH.GE.24)
        IH=IH-24
        ID=ID+1
        IF(ID.GT.NDM(IM))THEN
           ID=1
           IM=IM+1
           IF(IM.GT.12)THEN
              IM=1
              IY=IY+1
           END IF
        END IF
     END DO
  END IF

! accumulated hours
  NDAYS=NDT(IM)+ID
  IF(IM.GT.2.AND.NDM(2).EQ.29)NDAYS=NDAYS+1
  JULH=(NDAYS-1)*24+IH

END SUBROUTINE ncjulh
