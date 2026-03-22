!===============================================================================
! Create METDATA.CFG configuration file for packing if required

SUBROUTINE MAKNDX (nsfc,nvar,klvls,vchar,level,gridkm,nxp,nyp,clat,clon, &
                   global,clat1,clon1,dlat,dlon,nlat,nlon,kunit)

  IMPLICIT NONE

  INTEGER,      INTENT(IN)   :: nsfc, nvar  
  INTEGER,      INTENT(IN)   :: klvls
  CHARACTER(4), INTENT(IN)   :: vchar (nvar)    
  REAL,         INTENT(IN)   :: level (20,nvar)
  REAL,         INTENT(IN)   :: gridkm
  INTEGER,      INTENT(IN)   :: nxp 
  INTEGER,      INTENT(IN)   :: nyp 
  REAL,         INTENT(IN)   :: clat  
  REAL,         INTENT(IN)   :: clon 
  LOGICAL,      INTENT(IN)   :: global
  REAL,         INTENT(IN)   :: clat1 
  REAL,         INTENT(IN)   :: clon1
  REAL,         INTENT(IN)   :: dlat  
  REAL,         INTENT(IN)   :: dlon 
  INTEGER,      INTENT(IN)   :: nlon
  INTEGER,      INTENT(IN)   :: nlat
  INTEGER,      INTENT(IN)   :: kunit
  
  CHARACTER(20) :: LABEL(18)                ! optional field label
  INTEGER       :: i,n,ig,nl,nvb,nve,kvc,mvar 
  REAL          :: sig
  REAL          :: GRIDS(12), PARMAP(9)

  COMMON / SETUP / GRIDS, PARMAP

! optional field label string
  DATA LABEL/'Model Type:','Grid Numb:','Vert Coord:','Pole Lat:',         &
    'Pole Lon:','Ref Lat:','Ref Lon:','Grid Size:','Orientation:',         &
    'Cone Angle:','Sync X Pt:','Sync Y Pt:','Sync Lat:','Sync Lon:',       &
    'Reserved:','Numb X pt:','Numb Y pt:','Numb Levels:'/

! grid orientation
  GRIDS(6)=0.0
! delta=x grid size in km
  GRIDS(5)=GRIDKM
! variable reserved for future use
  GRIDS(12)=0.0

  IF(GLOBAL)THEN
!    sync x,y defines lower left grid point 
     GRIDS(8)=1.0 
     GRIDS(9)=1.0 
!    lat/lon of lower left point
     GRIDS(10)=CLAT1
     GRIDS(11)=CLON1
!    latlon grid not global
     IF(NYP.LT.NLAT)GRIDS(10)=CLAT
     IF(NXP.LT.NLON)GRIDS(11)=CLON
  ELSE
!    synch point in x,y coordintes
     GRIDS(8)=(NXP+1.0)/2.0
     GRIDS(9)=(NYP+1.0)/2.0
!    synch point in lat/lon coordinates
     GRIDS(10)=CLAT
     GRIDS(11)=CLON
  END IF

! defines a global latlon grid
  IF(GLOBAL)THEN

!    pole lat/lon is used to identify the 
!    latlon point of the maximum index
     GRIDS(1)=GRIDS(10)+DLAT*(NYP-1)
     GRIDS(2)=GRIDS(11)+DLON*(NXP-1)
     GRIDS(7)=0.0  

!    the reference lat/lon defines grid spacing  
     GRIDS(3)=DLAT
     GRIDS(4)=DLON

! defines a polar sterographic projection
  ELSEIF(ABS(CLAT).GT.60.0)THEN

!    set the pole position and reference lat/lon
     IF(CLAT.GT.0.0)THEN
        GRIDS(1)=90.0
     ELSE
        GRIDS(1)=-90.0
     END IF

!    pole longtitude (+180 from cut)
     GRIDS(2)=CLON

!    reference lat/lon (at which grid size specified)
     GRIDS(3)=CLAT
!    reference longitude and grid alignment
     GRIDS(4)=CLON

!    tangent latitude
     GRIDS(7)=GRIDS(1)

! defines a mercator projection
  ELSEIF(ABS(CLAT).LT.30.0)THEN

!    pole lat/lon axis through pole
     GRIDS(1)=0.0
     GRIDS(2)=CLON

!    reference lat
     GRIDS(3)=CLAT
!    reference lon
     GRIDS(4)=CLON

!    tangent latitude
     GRIDS(7)=0.0

! defines a lambert conformal projection
  ELSE

!    pole lat/lon axis through pole
     GRIDS(1)=CLAT
     GRIDS(2)=CLON

!    reference lat
     GRIDS(3)=CLAT
!    reference lon
     GRIDS(4)=CLON

!    tangent latitude
     GRIDS(7)=CLAT

  END IF

! write the packer configuration file
  OPEN(KUNIT,FILE='METDATA.CFG')
  WRITE(KUNIT,'(A20,A4)')LABEL(1),'CDC1'

! default grid number 99 (field not used)
  IG=99
  WRITE(KUNIT,'(A20,I4)') LABEL(2),IG

! coordinate- 1:sigma  2:pressure  3:terrain  4:hybrid
  KVC=2
  WRITE(KUNIT,'(A20,I4)') LABEL(3), KVC

! grid geolocation parameters and projection
  DO I=1,12
     WRITE(KUNIT,'(A20,F10.2)')LABEL(I+3),GRIDS(I)
  END DO

! grid dimensions
  WRITE(KUNIT,'(A20,I4)') LABEL(16),NXP
  WRITE(KUNIT,'(A20,I4)') LABEL(17),NYP
  WRITE(KUNIT,'(A20,I4)') LABEL(18),KLVLS

! upper level information
  DO NL=1,KLVLS

     WRITE(LABEL(1),'(A6,I2,A1)')'Level ',NL,':'
     IF(NL.EQ.1)THEN
        MVAR=NSFC
        NVB=1
        NVE=MVAR
        SIG=0.0     
     ELSE
!       all variables available
        NVB=NSFC+1
        NVE=NVAR

!       count actual number
        MVAR=0
        DO N=NVB,NVE
           IF(LEVEL(NL-1,N).GT.0.0)MVAR=MVAR+1
        END DO
        NVE=NVB+MVAR-1

!       first upper level always complete
        SIG=LEVEL(NL-1,NSFC+1)
     END IF

     IF(SIG.LT.1.0)THEN
        WRITE(KUNIT,'(A20,F8.5,I3,10(1X,A4))')LABEL(1),                 &
              SIG,MVAR,(VCHAR(N),N=NVB,NVE)

     ELSEIF(SIG.GE.1.AND.SIG.LT.10.0)THEN
        WRITE(KUNIT,'(A20,F8.4,I3,10(1X,A4))')LABEL(1),                 &
              SIG,MVAR,(VCHAR(N),N=NVB,NVE)

     ELSEIF(SIG.GE.10.AND.SIG.LT.100.0)THEN
        WRITE(KUNIT,'(A20,F6.3,I3,10(1X,A4))')LABEL(1),                 &
              SIG,MVAR,(VCHAR(N),N=NVB,NVE)

     ELSEIF(SIG.GE.100.AND.SIG.LT.1000.0)THEN
        WRITE(KUNIT,'(A20,F6.2,I3,10(1X,A4))')LABEL(1),                 &
              SIG,MVAR,(VCHAR(N),N=NVB,NVE)

     ELSEIF(SIG.GE.1000)THEN
        WRITE(KUNIT,'(A20,F6.1,I3,10(1X,A4))')LABEL(1),                 &
              SIG,MVAR,(VCHAR(N),N=NVB,NVE)
     END IF

  END DO
  CLOSE (KUNIT)

END SUBROUTINE makndx
