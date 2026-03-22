!===============================================================================
! Read METGRID.CFG configuration file for packing if required

SUBROUTINE SETNDX(GLOBAL,FTEST,KLVLS,GRIDKM,NXP,NYP,CLAT,CLON,NLAT,NLON,KUNIT)

  IMPLICIT NONE

  LOGICAL, INTENT(OUT)   :: GLOBAL
  LOGICAL, INTENT(OUT)   :: FTEST
  INTEGER, INTENT(INOUT) :: KLVLS
  REAL,    INTENT(OUT)   :: GRIDKM
  INTEGER, INTENT(OUT)   :: NXP 
  INTEGER, INTENT(OUT)   :: NYP
  REAL,    INTENT(OUT)   :: CLAT  
  REAL,    INTENT(OUT)   :: CLON 
  INTEGER, INTENT(IN)    :: NLAT
  INTEGER, INTENT(IN)    :: NLON
  INTEGER, INTENT(IN)    :: KUNIT

  INTEGER                :: K
  CHARACTER(80)          :: LABEL
  REAL                   :: GRIDS(12), PARMAP(9)

  COMMON / SETUP / GRIDS, PARMAP

! default is to map data to conformal projection
  GLOBAL=.FALSE.

! if configuration exists exit
  INQUIRE(FILE='METGRID.CFG',EXIST=FTEST)

  IF(FTEST)THEN
     OPEN(KUNIT,FILE='METGRID.CFG')
     READ(KUNIT,'(A)')LABEL
     READ(KUNIT,'(A)')LABEL
     READ(KUNIT,'(A)')LABEL
     DO K=1,12
        READ(KUNIT,'(A20,F10.2)')LABEL,GRIDS(K)
     END DO
     READ(KUNIT,'(A20,I4)')LABEL,NXP
     READ(KUNIT,'(A20,I4)')LABEL,NYP
     READ(KUNIT,'(A20,I4)')LABEL,KLVLS
     DO K=1,KLVLS
        READ(KUNIT,'(A)')LABEL
     END DO
     CLOSE (KUNIT)
     WRITE(*,*)'Using existing METGRID.CFG ... '
     WRITE(*,*)'Delete file and rerun program if new grid required'

!    definition of a global latlon grid (spacing=0.0)
     IF(GRIDS(5).LE.0.0)THEN
        GLOBAL=.TRUE.
        CLAT=GRIDS(10)
        CLON=GRIDS(11)
     END IF

  ELSE
     WRITE(*,*)'Output grid resolution (km or 0 for latlon)'
     READ(*,*)GRIDKM

     IF(GRIDKM.LE.0.0)THEN
!       defines that the output grid should be latlon 
        GLOBAL=.TRUE.

        WRITE(*,*)'Output grid dimensions (nlon,nlat or 0,0 if global)'
        READ(*,*)NXP,NYP

        IF(NXP.EQ.0.AND.NYP.EQ.0)THEN
           CLAT=0.0
           CLON=0.0
        ELSE 
           WRITE(*,*)'Output grid lower left corner (lat,lon)'
           READ(*,*)CLAT,CLON
!          use 0-360 system  
           IF(CLON.LT.0.0)CLON=360.0+CLON
        END IF
        IF(NXP.EQ.0)NXP=NLON
        IF(NYP.EQ.0)NYP=NLAT

        WRITE(*,*)'Number of levels required (incl sfc) - ',klvls
        READ(*,*)KLVLS

     ELSE
!       defines that the latlon grid is remapped to conformal for output
        WRITE(*,*)'Output grid dimensions (nx,ny)'
        READ(*,*)NXP,NYP
        WRITE(*,*)'Output grid center (lat,lon)'
        READ(*,*)CLAT,CLON
        WRITE(*,*)'Number of levels required (incl sfc) - ',klvls
        READ(*,*)KLVLS

     END IF
  END IF

END SUBROUTINE setndx
