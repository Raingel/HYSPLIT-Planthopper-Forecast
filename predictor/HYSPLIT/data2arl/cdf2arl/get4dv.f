SUBROUTINE get4dv (diag,ncid,varid,nx,ny,nz,nt,rval)

  IMPLICIT NONE

  include 'netcdf.inc'

  LOGICAL,       INTENT(IN)  :: diag
  INTEGER,       INTENT(IN)  :: ncid
  INTEGER,       INTENT(IN)  :: varid
  INTEGER,       INTENT(IN)  :: nx,ny,nz,nt
  REAL,          INTENT(OUT) :: rval(:,:,:) 

  INTEGER       :: n,status
  INTEGER       :: start(4),count(4)

  start(1) = 1
  start(2) = 1
  start(3) = 1
  start(4) = nt

  count(1) = nx
  count(2) = ny
  count(3) = nz
  count(4) = 1

  status = NF_GET_VARA_REAL(ncid, varid, start, count, rval)
  IF(status.NE.nf_noerr) THEN
     WRITE(*,*) NF_STRERROR(status)
  ELSEIF(diag)THEN
     DO n=nz,1,-1
        WRITE(*,*)'3d value: ',n,rval(nx/2,ny/2,n)
     END DO
  ELSE
     CONTINUE
  END IF

END SUBROUTINE get4dv 
