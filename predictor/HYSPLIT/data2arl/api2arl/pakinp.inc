  INTERFACE

  SUBROUTINE PAKINP(RVAR,CVAR,NX,NY,NX1,NY1,LX,LY,PREC,NEXP,VAR1,KSUM)
  IMPLICIT NONE
  REAL,          INTENT(OUT)   :: rvar (:,:)     ! real data unpacked
  CHARACTER(1),  INTENT(IN)    :: cvar (:)       ! packed input of NX*NY
  INTEGER,       INTENT(IN)    :: nx,ny          ! size of input array  
  INTEGER,       INTENT(IN)    :: nx1,ny1        ! optional sub-grid left edge 
  INTEGER,       INTENT(IN)    :: lx,ly          ! length of sub-grid
  REAL,          INTENT(IN)    :: prec           ! precision of packed data 
  INTEGER,       INTENT(IN)    :: nexp           ! packing scaling exponent
  REAL,          INTENT(IN)    :: var1           ! value of array at (1,1)
  INTEGER,       INTENT(INOUT) :: ksum           ! rotating checksum 
  END SUBROUTINE pakinp

  END INTERFACE

