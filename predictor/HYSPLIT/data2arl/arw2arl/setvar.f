SUBROUTINE setvar (diag,ncid,label,varid,n3d,ndim,dimlen,range)

  IMPLICIT NONE

  include 'netcdf.inc'

  LOGICAL,       INTENT(IN)  :: diag
  INTEGER,       INTENT(IN)  :: ncid
  CHARACTER(80), INTENT(IN)  :: label 
  INTEGER,       INTENT(OUT) :: varid
  INTEGER,       INTENT(OUT) :: n3d  
  INTEGER,       INTENT(OUT) :: ndim
  INTEGER,       INTENT(OUT) :: dimlen(nf_max_var_dims)
  REAL,          INTENT(OUT) :: range(2) 

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

! additional information for 3D variables
  IF(ndim.GT.2)THEN
     status = NF_GET_ATT_INT(ncid, varid, '_n3D', n3d)
     IF(status.NE.nf_noerr) THEN
        IF(diag)WRITE(*,*) '_n3D: ',NF_STRERROR(status)
        n3d=0     
     ELSE
        IF(diag)WRITE(*,*)'_n3D = ',n3d
     END IF

     status = NF_GET_ATT_REAL(ncid, varid, 'valid_range', range)
     IF(status.NE.nf_noerr) THEN
        IF(diag)WRITE(*,*) 'valid_range: ',NF_STRERROR(status)
        range(1)=-1.0E+20
        range(2)=+1.0E+20
     ELSE
        IF(diag)WRITE(*,*)'Valid Range = ',range
     END IF
  END IF

END SUBROUTINE setvar 
