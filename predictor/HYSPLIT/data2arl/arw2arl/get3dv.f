SUBROUTINE get3dv (diag,ncid,varid,nx,ny,nz,nt,rval)

  IMPLICIT NONE

  include 'netcdf.inc'

  LOGICAL,       INTENT(IN)  :: diag
  INTEGER,       INTENT(IN)  :: ncid
  INTEGER,       INTENT(IN)  :: varid
  INTEGER,       INTENT(IN)  :: nx,ny,nz,nt
  REAL,          INTENT(OUT) :: rval(:,:)   

  INTEGER       :: status
  INTEGER       :: start(3),count(3)

  start(1) = 1
  start(2) = 1
  start(3) = nt

  count(1) = nx
  count(2) = ny
  count(3) = 1

  status=nz  ! statement to suppress unused variable message
  status = NF_GET_VARA_REAL(ncid, varid, start, count, rval)

  IF(status.NE.nf_noerr) THEN
     WRITE(*,*) NF_STRERROR(status)
  ELSEIF(diag)THEN
     WRITE(*,*)'2D value: ',rval(nx/2,ny/2)
  ELSE
     CONTINUE
  END IF

END SUBROUTINE get3dv 
