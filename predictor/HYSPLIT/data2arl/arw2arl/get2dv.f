SUBROUTINE get2dv (diag,ncid,varid,ntp,rval)

  IMPLICIT NONE

  include 'netcdf.inc'

  LOGICAL,       INTENT(IN)  :: diag
  INTEGER,       INTENT(IN)  :: ncid
  INTEGER,       INTENT(IN)  :: varid
  INTEGER,       INTENT(IN)  :: ntp
  REAL,          INTENT(OUT) :: rval(:) 

  INTEGER       :: n,status

  status = NF_GET_VAR_REAL(ncid, varid, rval)
  IF(status.NE.nf_noerr) THEN
     WRITE(*,*) NF_STRERROR(status)
  ELSEIF(diag)THEN
     DO n=1,ntp
        WRITE(*,*) rval(n)
     END DO 
  ELSE
     CONTINUE
  END IF

END SUBROUTINE get2dv 
