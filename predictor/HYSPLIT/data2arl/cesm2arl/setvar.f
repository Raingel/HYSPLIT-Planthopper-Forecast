SUBROUTINE setvar (diag,ncid,label,varid,n3d,ndim,dimlen)

  IMPLICIT NONE

  include 'netcdf.inc'

  LOGICAL,       INTENT(IN)  :: diag
  INTEGER,       INTENT(IN)  :: ncid
  CHARACTER(80), INTENT(IN)  :: label 
  INTEGER,       INTENT(OUT) :: varid
  INTEGER,       INTENT(OUT) :: n3d  
  INTEGER,       INTENT(OUT) :: ndim
  INTEGER,       INTENT(OUT) :: dimlen(nf_max_var_dims)

  INTEGER       :: n,kl,status
  INTEGER       :: dimids(nf_max_var_dims) 

  dimlen = 0
  varid  = 0
  n3d    = 0
  ndim   = 0

  kl=INDEX(label,' ')

  status = NF_INQ_VARID(ncid, label, varid)
  IF(status.NE.nf_noerr) THEN
     WRITE(*,*) NF_STRERROR(status)
     WRITE(*,*) label(:kl)
     varid=0
     RETURN
  ELSE
     IF(diag)WRITE(*,*)label(:kl),'= ',varid
  END IF

  status = NF_INQ_VARNDIMS(ncid, varid, ndim)
  IF(status.NE.nf_noerr) THEN
     WRITE(*,*) NF_STRERROR(status)
  ELSE
     IF(diag)WRITE(*,*)' Number of dimensions = ',ndim
  END IF

  status = NF_INQ_VARDIMID(ncid, varid, dimids)
  IF(status.NE.nf_noerr) THEN
     WRITE(*,*) NF_STRERROR(status)
  ELSE
     IF(diag)WRITE(*,*)'Dimension IDs = ',dimids(:ndim)
  END IF

  DO n=1,ndim
     status = NF_INQ_DIMLEN(ncid, dimids(n), dimlen(n))
     IF(status.NE.nf_noerr) THEN
        WRITE(*,*) NF_STRERROR(status)
     ELSE
        IF(diag)WRITE(*,*)'Length = ',dimlen(n)
     END IF
  END DO

END SUBROUTINE setvar 
