
INTERFACE

  SUBROUTINE GRIDCASE1(NXT,NYT,XRES,YRES,GX,GY,G2M)
  INTEGER, INTENT(IN) :: nxt,nyt      ! size of arrays
  REAL, INTENT(IN)    :: xres, yres   ! resolution of grid in degrees.
  REAL, INTENT(OUT)   :: gx(:,:)      ! grid size (m)
  REAL, INTENT(OUT)   :: gy(:,:)      ! grid size (m)
  REAL, INTENT(IN)    :: g2m          ! meters per degree
  END SUBROUTINE gridcase1



  SUBROUTINE VELCASE1(NXT,NYT,NLVL,VX,VY,VZ,UUU,VVV,WWW)
  IMPLICIT NONE
! constant velocity field
  REAL, INTENT(OUT)   :: uuu(:,:,:,:)      ! u wind
  REAL, INTENT(OUT)   :: vvv(:,:,:,:)      ! v wind
  REAL, INTENT(OUT)   :: www(:,:,:,:)      ! w wind
  INTEGER, INTENT(IN) :: vx, vy, vz   ! velocity in grid cell / minute
  INTEGER, INTENT(IN) :: nlvl,nxt,nyt ! size of arrays
  END SUBROUTINE velcase1

END INTERFACE 
 
