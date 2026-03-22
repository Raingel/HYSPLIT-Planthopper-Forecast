SUBROUTINE TMCOVT(inmn,iy,im,id,ih)

  IMPLICIT NONE

!-------------------------------------------------------------------------------

  INTEGER, INTENT(in)   :: inmn
  INTEGER, INTENT(out)  :: iy,im,id,ih

  INTEGER               :: mn       ! date and time

  INTEGER  :: macm,kk,macc,mhr,mda,mmo,idt

! default number of accumulated days in each month (non leap year)
  INTEGER  :: NADPM (12) = (/0,31,59,90,120,151,181,212,243,273,304,334/)

  INTEGER  :: K, IYEAR, NDPY, NDTOT
  INTEGER  :: NADPY (300)     ! accumulated days table from Jan 1, 1800
                              ! where the first value is for Jan 1, 1801
! COMMON /TMVALS/ NADPY


!-------------------------------------------------------------------------------

  NDTOT=0
  DO IYEAR=1800,2099

     K=IYEAR-1800+1
     NDPY=365

     IF(MOD(IYEAR,100).NE.0)THEN
!       regular year
        IF(MOD(IYEAR,4).EQ.0)   NDPY=366 ! leap year
     ELSE
!       century
        IF(MOD(IYEAR,400).EQ.0) NDPY=366 ! leap century
     END IF

     NDTOT=NDTOT+NDPY
     NADPY(K)=NDTOT

  END DO

 !print *,nadpy

macm=inmn*60

! compute four digit year
  KK=1
  DO WHILE (NADPY(KK)*1440.LT.MACM)
     KK=KK+1
  END DO
  IYEAR=1800+(KK-1)

! compute minutes in this year
  MACC=MACM-NADPY(KK-1)*1440

! convert back to two digit year
  IY=MOD(IYEAR-1800,100)

! current minute
  MHR=MACC/60
  MN=MACC-MHR*60

! current hour
  MDA=MHR/24
  IH=MHR-MDA*24

! current month and day
  DO K=1,12
     IDT=NADPM(K)

!    adjust accumulated days for leap year
     IF(MOD(IYEAR,4).EQ.0.AND.K.GT.2)IDT=IDT+1

     IF(IDT.LE.MDA)THEN
        MMO=IDT
        IM=K
     END IF
  END DO
  ID=MDA-MMO+1

! check for end-of-year
  IF(ID.GT.31)THEN
     ID=ID-31
     IM=1
     IY=MOD(IY+1,100)
  END IF

! print *,iyear,iy,im,id,ih,mn

END SUBROUTINE tmcovt
