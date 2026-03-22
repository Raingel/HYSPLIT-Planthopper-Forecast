SUBROUTINE REGRID(GLOBAL,I1,J1,V1,NX1,NY1,V2,NX2,NY2,TLAT,TLON,    &
                  CLAT1,CLON1,DLON,DLAT)

   IMPLICIT NONE

   LOGICAL, INTENT(IN)    :: GLOBAL
   INTEGER, INTENT(IN)    :: I1, J1
   INTEGER, INTENT(IN)    :: NX1, NY1
   INTEGER, INTENT(IN)    :: NX2, NY2
   REAL,    INTENT(INOUT) :: V1(NX1,NY1)
   REAL,    INTENT(OUT)   :: V2(NX2,NY2)
   REAL ,   INTENT(IN)    :: TLAT(NX2,NY2)
   REAL ,   INTENT(IN)    :: TLON(NX2,NY2)
   REAL,    INTENT(IN)    :: CLAT1, CLON1
   REAL,    INTENT(IN)    :: DLAT,  DLON

   INTEGER :: i,j,ii,jj,ilo,ihi,jlo,jhi
   REAL    :: xp,yp,fxi,fyj,top,bot,temp

!  reverse the direction of the north-south component
   DO I=1,NX1
      JLO=1
      JHI=NY1
      DO WHILE (JLO.LT.JHI)
         TEMP=V1(I,JHI)
         V1(I,JHI)=V1(I,JLO)
         V1(I,JLO)=TEMP
         JLO=JLO+1
         JHI=JHI-1
      END DO
   END DO

! just move data into output array if latlon grid
  IF(GLOBAL)THEN
     DO J=1,NY2
        JJ=J1+J-1
!       assume bad definition if above 90
        JJ=MIN(JJ,NY1)
        DO I=1,NX2
           II=I1+I-1
!          assume wrap if grid goes around dateline
           IF(II.GT.NX1)II=II-NX1
           V2(I,J)=V1(II,JJ)
        END DO
     END DO
     RETURN
  END IF

! interpolate values to new grid
  DO I=1,NX2
  DO J=1,NY2

!    compute adjacent index values on grid 1
     XP=1.0+(TLON(I,J)-CLON1)/DLON
     YP=1.0+(TLAT(I,J)-CLAT1)/DLAT

!    compute indicies
     ILO=INT(XP)
     IHI=ILO+1
     JLO=INT(YP)
     JHI=JLO+1

!    interpolation fractions (off grid extrapolated)
     FXI=XP-ILO
     FYJ=YP-JLO

!    global grids check for wrap at prime meridian
     IF(IHI.GT.NX1)IHI=1
     IF(JHI.GT.NY1)JHI=1

!    interpolate across at top and bottom
     TOP=(V1(IHI,JHI)-V1(ILO,JHI))*FXI+V1(ILO,JHI)
     BOT=(V1(IHI,JLO)-V1(ILO,JLO))*FXI+V1(ILO,JLO)

!    interpolate between top and bottom
     V2(I,J)=(TOP-BOT)*FYJ+BOT

  END DO
  END DO

END SUBROUTINE regrid
