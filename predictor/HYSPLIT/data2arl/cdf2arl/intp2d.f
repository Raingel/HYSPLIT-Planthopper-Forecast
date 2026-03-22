!
! TWO DIMENSIONAL LINEAR INTERPOLATION.
! MFLG((+/-)1,0) --- INTEGER FLAG OF (USE,NOT.USE) MASK.
!    if mflg.lt.0 ==> do not set default value in array B.
!  SETUP MASK IN CALLER AND PUT INTO COMMON BLOCK /COMASK/DEFLT,MASK
!  MASK(I,J) = (1,0) --- (USE, VOID) DATA POINT (I,J)
!  SET DEFAULT VALUE FOR MASKED OUT POINTS IN DEFLT (R*4)
!  IERR,JERR --- NO. OF XB,YB POINTS OUTSIDE DOMAIN OF XA AND YA.
!
      SUBROUTINE INTP2D(A,IMX,IMY,XA,YA,B,JMX,JMY,XB,YB,MFLG)
      PARAMETER(MAX=361,MAXP=361*181)
      COMMON /COMASK/ DEFLT,MASK(maxp)
      COMMON /COMWRK/ IERR,JERR,DXP(MAX),DYP(MAX),DXM(MAX),DYM(MAX), &
                      IPTR(MAX),JPTR(MAX),WGT(4)
      DIMENSION A(IMX,*),B(JMX,*),XA(*),YA(*),XB(*),YB(*)
      CALL SETPTR(XA,IMX,XB,JMX,IPTR,DXP,DXM,IERR)
      CALL SETPTR(YA,IMY,YB,JMY,JPTR,DYP,DYM,JERR)
      DO I = 1,4
      WGT(I) = 1.0
      END DO
      IF (MFLG.EQ.0) DEFLT = 0.00
      if (mflg.ge.0) then
        DO J = 1,JMY
        DO I = 1,JMX
        B(I,J) = DEFLT
        END DO
        END DO
      endif
 
      DO 20 J = 1,JMY
        JM = JPTR(J)
        IF (JM.LT.0) GOTO 20
        JP = JM + 1
      DO 10 I = 1,JMX
        IM = IPTR(I)
        IF (IM.LT.0) GOTO 10
        IP = IM + 1
        IF(mflg.ne.0) CALL MSKWGT(MASK,IMX,IMY,IM,IP,JM,JP,WGT,mflg,*10)
        D1 = DXM(I)*DYM(J)*WGT(1)
        D2 = DXM(I)*DYP(J)*WGT(2)
        D3 = DXP(I)*DYM(J)*WGT(3)
        D4 = DXP(I)*DYP(J)*WGT(4)
        DD = D1 + D2 +D3 + D4
        IF (DD.EQ.0.0) GOTO 10
        B(I,J) = (D4*A(IM,JM)+D3*A(IM,JP)+D2*A(IP,JM)+D1*A(IP,JP))/DD
   10 CONTINUE
   20 CONTINUE
      if(yb(1).le.ya(1)) then
      do 30 i=1,jmx
  30  b(i,1)=b(i,2)*0.65+b(i,3)*0.35
      endif
      if(yb(jmy).ge.ya(imy)) then
      do 40 i=1,jmx
  40  b(i,jmy)=b(i,jmy-1)*0.65+b(i,jmy-2)*0.35
      endif
      if(xb(1).le.xa(1)) then
      do 50 j=1,jmy
  50  b(1,j)=b(2,j)*0.65+b(3,j)*0.35
      endif
      if(xb(jmx).ge.xa(imx)) then
      do 60 j=1,jmy
  60  b(jmx,j)=b(jmx-1,j)*0.65+b(jmx-2,j)*0.35
      endif
      RETURN
      END
!
!  id --- flag for extrapolation over land:
!         0 --- interpolate over land points;
!        -1 --- ignore land points.
!
      SUBROUTINE MSKWGT(MASK,IMX,IMY,IM,IP,JM,JP,WGT,id,*)
      INTEGER   MASK(IMX,IMY)
      DIMENSION WGT(4)
      isum = 0
      WGT(1) = MASK(IP,JP)
      WGT(2) = MASK(IP,JM)
      WGT(3) = MASK(IM,JP)
      WGT(4) = MASK(IM,JM)
      isum = mask(ip,jp)+mask(ip,jm)+mask(im,jp)+mask(im,jm)
      if (id.lt.0.and.isum.lt.4) return 1
      RETURN
      END
 
      SUBROUTINE SETPTR(X,M,Y,N,IPTR,DP,DM,IERR)
      DIMENSION X(*),Y(*),IPTR(*),DP(*),DM(*)
      IERR = 0
      DO 10 J = 1,N
        YL = Y(J)
        IF (YL.LT.X(1)) THEN
          IERR = IERR + 1
          IPTR(J) = -1
          GOTO 10
        ELSEIF (YL.GT.X(M)) THEN
          IERR = IERR + 1
          IPTR(J) = -1
          GOTO 10
        ENDIF
        DO 20 I = 1,M-1
          IF (YL.GT.X(I+1)) GOTO 20
          DM(J) = YL-X(I)
          DP(J) = X(I+1)-YL
          IPTR(J) = I
          GOTO 10
  20    CONTINUE
  10  CONTINUE
      RETURN
      END
