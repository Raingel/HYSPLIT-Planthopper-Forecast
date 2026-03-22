INTERFACE

SUBROUTINE PAKSET(LUNIT,FNAME,KREC1,NXP,NYP,NZP)
  IMPLICIT NONE
  INTEGER,       INTENT(IN)    :: lunit     ! output unit number
  CHARACTER(*),  INTENT(INOUT) :: fname     ! file name of METDATA.CFG
  INTEGER,       INTENT(IN)    :: krec1     ! position of index record at time-1
  INTEGER,       INTENT(OUT)   :: nxp, nyp  ! horizontal grid dimensions
  INTEGER,       INTENT(OUT)   :: nzp       ! vertical grid dimension (incl sfc)
END SUBROUTINE pakset

END INTERFACE
