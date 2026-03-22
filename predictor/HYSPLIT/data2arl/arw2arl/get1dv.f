SUBROUTINE get1dv (diag,ncid,varid,ntp,tvar)

  IMPLICIT NONE

  include 'netcdf.inc'

  LOGICAL,       INTENT(IN)  :: diag
  INTEGER,       INTENT(IN)  :: ncid
  INTEGER,       INTENT(IN)  :: varid
  INTEGER,       INTENT(IN)  :: ntp
  CHARACTER(19), INTENT(OUT) :: tvar(:) 

  INTEGER       :: n,status

  status = NF_GET_VAR_TEXT(ncid, varid, tvar)
  IF(status.NE.nf_noerr) THEN
     WRITE(*,*) NF_STRERROR(status)
  ELSEIF(diag)THEN
     DO n=1,ntp
        WRITE(*,*) tvar(n)
     END DO 
  ELSE
     CONTINUE
  END IF

END SUBROUTINE get1dv 
