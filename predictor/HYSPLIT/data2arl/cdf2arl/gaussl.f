      SUBROUTINE GAUSSL(SIA,COA,GLAT,GW,JGMAX)
!
!---- THIS ROUTINE CALCULATES THE GAUSSIAN LATITUDES
!
      REAL   SIA(JGMAX),COA(JGMAX),GLAT(JGMAX),GW(JGMAX)
      REAL   PI2,X0,X1,X2,X3,P0,P1,P2,D0,D1,D2,DFN

!     X0=3.1415926535897932D0/FLOAT(4*JGMAX+2)
      X0=3.1415926535897932/FLOAT(4*JGMAX+2)

      JH=(JGMAX+1)/2
      PI2=ASIN(1.0)
      DO J=1,JH
         X1=COS(FLOAT(4*J-1)*X0)

   10    P0=X1
         P1=1.500*P0*P0-0.50
         D0=1.00
         D1=3.00*P0

         DO N=3,JGMAX
            DFN=FLOAT(N)
            X2=(2.00*DFN-1.00)*P1
            P2=(X1*X2-(DFN-1.00)*P0)/DFN
            D2=X2+D0
            P0=P1
            P1=P2
            D0=D1
            D1=D2
         END DO

         X3=P2/D2
         X1=X1-X3
         IF(ABS(X3).GE.1.E-5 ) GOTO 10  
         COA(J)=X1
         GW(J)=2.000/((1.000-X1*X1)*D2*D2)
      END DO

      DO J=1,JH
         JJ=JGMAX-J+1
         GLAT(JJ)=PI2-ACOS(COA(J))
         GLAT(J)=-GLAT(JJ)
         SIA(JJ)=COA(J)
         SIA(J)=-COA(J)
         COA(J)=SQRT(1.000-COA(J)*COA(J))
         COA(JJ)=COA(J)
         GW(JJ)=GW(J)
      END DO

      RETURN
      END
