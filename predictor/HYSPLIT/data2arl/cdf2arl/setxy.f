      SUBROUTINE SETXY(X,NX,Y,NY,XMN,DLX,YMN,DLY)

      DIMENSION X(NX),Y(NY)

      DO I = 1,NX
         X(I) = XMN + DLX*FLOAT(I-1)
      END DO
      IF (NY.EQ.0) RETURN

      DO J = 1,NY
         Y(J) = YMN + DLY*FLOAT(J-1)
      END DO
      RETURN
      END
