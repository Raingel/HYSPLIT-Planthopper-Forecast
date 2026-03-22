  SUBROUTINE GRIDCASE1(NXT,NYT,XRES,YRES,GX,GY,G2M)
  ! regular grid
  INTEGER, INTENT(IN) :: nxt,nyt      ! size of arrays
  REAL, INTENT(IN)    :: xres, yres   ! resolution of grid in degrees.
  REAL, INTENT(OUT)   :: gx(:,:)      ! grid size (m)
  REAL, INTENT(OUT)   :: gy(:,:)      ! grid size (m)
  REAL, INTENT(IN)    :: G2M          ! meters per degree
  write(*,*) 'GX, GY', xres, yres, G2M, xres*G2M

  DO KX=1,NXT
     DO KY=1,NYT
        gx(kx,ky) = xres*G2M
        gy(kx,ky)  = yres*G2M
     END DO
  END DO
  END SUBROUTINE

  SUBROUTINE VELCASEDIV(NXT,NYT,NLVL,UUU,VVV,SET)
  ! Used by testmetdiv.

  IMPLICIT NONE
  REAL, INTENT(OUT)   :: uuu(4,4,5)      ! u wind
  REAL, INTENT(OUT)   :: vvv(4,4,5)      ! v wind
  INTEGER, INTENT(OUT) :: nlvl,nxt,nyt ! size of arrays
  INTEGER, INTENT(IN) :: set           ! which case to return

  REAL :: u1(4,4), u2(4,4), u3(4,4)
  REAL :: v1(4,4), v2(4,4), v3(4,4)
  REAL :: crnr, zer, yvel, vel

  ! 23.125 m/s is chosen because
  ! 23.125 m/s * (60 s/min) * (1/111,000*0.25 grid square/m) = 0.05 grid square / minute.
  ! and that is the output unit of metdiv 

  vel = 23.125  ! m/s
  yvel = 0.0

  nxt = 4
  nyt = 4
  nlvl = 5
  zer = 0.0 
  crnr = 0.707106 * vel

  SELECT CASE(SET)
  CASE (2)
     yvel = 0
     nlvl = 5
     ! converging in all levels. with 0 x velocity is at a grid cell.
     ! convergence occurs at a grid cell 
     u1 = reshape((/(/vel,zer,-vel,-vel /),(/vel,zer,-vel,-vel /),(/vel,zer,-vel,-vel /),(/vel,zer,-vel,-vel /)/),(/4,4/))
     v1 = reshape((/(/yvel,yvel,yvel,yvel/),(/yvel,yvel,yvel,yvel/),(/yvel,yvel,yvel,yvel/),(/yvel,yvel,yvel,yvel/)/),(/4,4/))


     uuu = reshape((/u1,u1,u1,u1,u1/),(/4,4,5/))
     vvv = reshape((/v1,v1,v1,v1,v1/),(/4,4,5/))

  CASE (3)
     yvel = 0
     nlvl = 5
     ! converging in all levels. but non zero x velocity in center. 
     ! convergence occurs between two grids.
     u1 = reshape((/(/vel,vel,-vel,-vel/),(/vel,vel,-vel,-vel/),(/vel,vel,-vel,-vel/) ,(/vel,vel,-vel,-vel/)/),(/4,4/))
     v1 = reshape((/(/yvel,yvel,yvel,yvel/),(/yvel,yvel,yvel,yvel/),(/yvel,yvel,yvel,yvel/),(/yvel,yvel,yvel,yvel/)/),(/4,4/))


     uuu = reshape((/u1,u1,u1,u1,u1/),(/4,4,5/))
     vvv = reshape((/v1,v1,v1,v1,v1/),(/4,4,5/))

  CASE (4)
     yvel = 0
     nlvl = 5
     u1 = reshape((/(/vel,zer,-vel,-vel/),(/vel,zer,-vel,-vel/),(/vel,zer,-vel,-vel /),(/vel,zer,-vel,-vel/) /),(/4,4/))
     v1 = reshape((/(/yvel,yvel,yvel,yvel/),(/yvel,yvel,yvel,yvel/),(/yvel,yvel,yvel,yvel/),(/yvel,yvel,yvel,yvel/)/),(/4,4/))

     u2 = reshape((/(/-vel,zer,vel,vel/),(/-vel,zer,vel,vel/),(/-vel,zer,vel,vel/),(/-vel,zer,vel,vel/)/),(/4,4/))

     ! converging in first to levels. 0 in middle level, diverging in second two levels.
     uuu = reshape((/u1,u1,v1,u2,u2/),(/4,4,5/))
     vvv = reshape((/v1,v1,v1,v1,v1/),(/4,4,5/))

  CASE (5)
     yvel = 0
     nlvl = 5
     u1 = reshape((/(/vel,zer,-vel,-vel/),(/vel,zer,-vel,-vel/),(/vel,zer,-vel,-vel /),(/vel,zer,-vel,-vel/) /),(/4,4/))
     v1 = reshape((/(/yvel,yvel,yvel,yvel/),(/yvel,yvel,yvel,yvel/),(/yvel,yvel,yvel,yvel/),(/yvel,yvel,yvel,yvel/)/),(/4,4/))

     u2 = reshape((/(/-vel,zer,vel,vel/),(/-vel,zer,vel,vel/),(/-vel,zer,vel,vel/),(/-vel,zer,vel,vel/)/),(/4,4/))

     ! diverging in first to levels. 0 in middle level, converging in second two levels.
     uuu = reshape((/u2,u2,v1,u1,u1/),(/4,4,5/))
     vvv = reshape((/v1,v1,v1,v1,v1/),(/4,4,5/))

  CASE (6)
     yvel = 0
     nlvl = 5
     u1 = reshape((/(/vel,-vel,zer,vel/),(/vel,-vel,zer,vel/),(/vel,-vel,zer,vel /),(/vel,-vel,zer,vel/) /),(/4,4/))
     v1 = reshape((/(/yvel,yvel,yvel,yvel/),(/yvel,yvel,yvel,yvel/),(/yvel,yvel,yvel,yvel/),(/yvel,yvel,yvel,yvel/)/),(/4,4/))

     u2 = reshape((/(/-vel,vel,zer,-vel/),(/-vel,vel,zer,-vel/),(/-vel,vel,zer,-vel /),(/-vel,vel,zer,-vel/) /),(/4,4/))

     ! similar to a convection cell.
     ! <- -> 0 <-
     ! <- -> 0 <-
     ! 0   0 0  0
     ! -> <- 0 ->
     ! -> <- 0 ->     
     uuu = reshape((/u1,u1,v1,u2,u2/),(/4,4,5/))
     vvv = reshape((/v1,v1,v1,v1,v1/),(/4,4,5/))



  END SELECT

  write(*,*) 'UUU', uuu
  write(*,*) 'VVV', vvv

  END SUBROUTINE


  SUBROUTINE VELCASE1A(NXT,NYT,NLVL,VX,VY,VZ,UUU,VVV,WWW)
  ! constant velocity field
  IMPLICIT NONE
  REAL, INTENT(OUT)   :: uuu(:,:,:)      ! u wind
  REAL, INTENT(OUT)   :: vvv(:,:,:)      ! v wind
  REAL, INTENT(OUT)   :: www(:,:,:)      ! w wind
  REAL, INTENT(IN) :: vx, vy, vz   ! velocity (m/s)
  INTEGER, INTENT(IN) :: nlvl,nxt,nyt ! size of arrays

  INTEGER    :: kret,kl,kx,ky
  INTEGER    :: k1, k2

  DO KL=1,NLVL
   DO KX=1,NXT
      DO KY=1,NYT
        uuu(kx,ky,KL) = vx
        vvv(kx,ky,KL) = vy
        www(kx,ky,KL) = vz
      END DO
   END DO
  END DO
  END SUBROUTINE


  SUBROUTINE VELCASE1(NXT,NYT,NLVL,VX,VY,VZ,UUU,VVV,WWW)
  IMPLICIT NONE
! constant velocity field

  REAL, INTENT(OUT)   :: uuu(:,:,:,:)      ! u wind
  REAL, INTENT(OUT)   :: vvv(:,:,:,:)      ! v wind
  REAL, INTENT(OUT)   :: www(:,:,:,:)      ! w wind
  INTEGER, INTENT(IN) :: vx, vy, vz   ! velocity (m/s)
  INTEGER, INTENT(IN) :: nlvl,nxt,nyt ! size of arrays
 
  INTEGER    :: kret,kl,kx,ky
  INTEGER    :: k1, k2
! Allocate and fill arrays
! velocity variables

  k1 = 1
  k2 = 2

  !DO KL=1,NLVL
  !   zsg(kl) = KL
  !END DO

! velocity defined as constant in space and time.
  DO KL=1,NLVL
     DO KX=1,NXT
        DO KY=1,NYT
          uuu(kx,ky,KL,k1) = vx
          vvv(kx,ky,KL,k1) = vy
          www(kx,ky,KL,k1) = vz
          uuu(kx,ky,KL,k2) = vx
          vvv(kx,ky,KL,k2) = vy
          www(kx,ky,KL,k2) = vz
        END DO
     END DO
  END DO
  END SUBROUTINE
