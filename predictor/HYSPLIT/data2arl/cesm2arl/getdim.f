SUBROUTINE getdim (diag,ncid,ntp,nxp,nyp,nzp)

  IMPLICIT NONE

  include 'netcdf.inc'

  LOGICAL,       INTENT(IN)  :: diag
  INTEGER,       INTENT(IN)  :: ncid
  INTEGER,       INTENT(OUT) :: ntp
  INTEGER,       INTENT(OUT) :: nxp  
  INTEGER,       INTENT(OUT) :: nyp  
  INTEGER,       INTENT(OUT) :: nzp  

  CHARACTER(80)              :: label 
  INTEGER                    :: n,status,dimid

  label='time'
  n=INDEX(label,' ')
  status = NF_INQ_DIMID(ncid, label, dimid)
  IF(status.NE.nf_noerr) THEN
     WRITE(*,*) label(:n),' : ',NF_STRERROR(status)
  ELSE
     IF(diag)WRITE(*,*)label(:n),' = ',dimid
     status = NF_INQ_DIMLEN(ncid, dimid,ntp)
     IF(status.NE.nf_noerr) THEN
        WRITE(*,*) NF_STRERROR(status)
     ELSE
        IF(diag)WRITE(*,*)'Number of times = ',ntp
     END IF
  END IF

  label='lon'
  n=INDEX(label,' ')
  status = NF_INQ_DIMID(ncid, label, dimid)
  IF(status.NE.nf_noerr) THEN
     WRITE(*,*) label(:n),' : ',NF_STRERROR(status)
  ELSE
     IF(diag)WRITE(*,*)label(:n),' = ',dimid
     status = NF_INQ_DIMLEN(ncid, dimid,nxp)
     IF(status.NE.nf_noerr) THEN
        WRITE(*,*) NF_STRERROR(status)
     ELSE
        IF(diag)WRITE(*,*)'Number of x points = ',nxp 
     END IF
  END IF

  label='lat'
  n=INDEX(label,' ')
  status = NF_INQ_DIMID(ncid, label, dimid)
  IF(status.NE.nf_noerr) THEN
     WRITE(*,*) label(:n),' : ',NF_STRERROR(status)
  ELSE
     IF(diag)WRITE(*,*)label(:n),' = ',dimid
     status = NF_INQ_DIMLEN(ncid, dimid, nyp)
     IF(status.NE.nf_noerr) THEN
        WRITE(*,*) NF_STRERROR(status)
     ELSE
        IF(diag)WRITE(*,*)'Number of y points = ',nyp 
     END IF
  END IF

  label='lev'
  n=INDEX(label,' ')
  status = NF_INQ_DIMID(ncid, label, dimid)
  IF(status.NE.nf_noerr) THEN
     WRITE(*,*) label(:n),' : ',NF_STRERROR(status)
  ELSE
     IF(diag)WRITE(*,*)label(:n),' = ',dimid
     status = NF_INQ_DIMLEN(ncid, dimid, nzp)
     IF(status.NE.nf_noerr) THEN
        WRITE(*,*) NF_STRERROR(status)
     ELSE
        IF(diag)WRITE(*,*)'Number of z points = ',nzp 
     END IF
  END IF

END SUBROUTINE getdim 
