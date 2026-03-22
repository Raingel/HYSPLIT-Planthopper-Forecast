SUBROUTINE CFGREC (kunit,arlcfg_file,nxp,nyp,nzp,level,nsfc,vsfc,n3dm,v3dm,vert_coord)

  IMPLICIT NONE

  INTEGER,      INTENT(IN)   :: kunit
  CHARACTER(80),INTENT(IN)   :: arlcfg_file
  INTEGER,      INTENT(IN)   :: nxp,nyp,nzp  
  REAL,         INTENT(IN)   :: level(:)
  INTEGER,      INTENT(IN)   :: nsfc
  CHARACTER(4), INTENT(IN)   :: vsfc(:)
  INTEGER,      INTENT(IN)   :: n3dm
  CHARACTER(4), INTENT(IN)   :: v3dm(:)
  INTEGER,      INTENT(IN)   :: vert_coord

  CHARACTER(20) :: label(18) 
  INTEGER       :: k,n
  REAL          :: sig

  REAL          :: GRIDS(12), PARMAP(9)
  COMMON / SETUP / GRIDS, PARMAP

! optional field label string
  DATA LABEL/'Model Type:','Grid Numb:','Vert Coord:','Pole Lat:',         &
    'Pole Lon:','Ref Lat:','Ref Lon:','Grid Size:','Orientation:',         &
    'Cone Angle:','Sync X Pt:','Sync Y Pt:','Sync Lat:','Sync Lon:',       &
    'Reserved:','Numb X pt:','Numb Y pt:','Numb Levels:'/

! write the packer configuration file 
  OPEN(KUNIT,FILE=TRIM(arlcfg_file))

! four character model label triggers grid stagger switch in HYSPLIT
  WRITE(KUNIT,'(A20,A4)')LABEL(1),'AWRF'    ! no not change

! default grid number 99 (field not used)
  WRITE(KUNIT,'(A20,I4)') LABEL(2),99 

! vertical coordinate- 1:sigma  2:pressure  3:terrain  4:ecmwf-hybrid  5:WRF-hybrid
!  WRITE(KUNIT,'(A20,I4)') LABEL(3), 1   
  WRITE(KUNIT,'(A20,I4)') LABEL(3), vert_coord

! grid geolocation parameters and projection
  DO N=1,12
     IF(GRIDS(N).LE.-100.)THEN
        WRITE(KUNIT,'(A20,F10.3)')LABEL(N+3),GRIDS(N)
     ELSEIF(GRIDS(N).LE.-10.)THEN
        WRITE(KUNIT,'(A20,F10.4)')LABEL(N+3),GRIDS(N)
     ELSEIF(GRIDS(N).LT.0.)THEN
        WRITE(KUNIT,'(A20,F10.5)')LABEL(N+3),GRIDS(N)
     ELSEIF(GRIDS(N).LT.10.)THEN
        WRITE(KUNIT,'(A20,F10.6)')LABEL(N+3),GRIDS(N)
     ELSEIF(GRIDS(N).LT.100.)THEN
        WRITE(KUNIT,'(A20,F10.5)')LABEL(N+3),GRIDS(N)
     ELSE
        WRITE(KUNIT,'(A20,F10.4)')LABEL(N+3),GRIDS(N)
     END IF
  END DO

! grid dimensions
  WRITE(KUNIT,'(A20,I4)') LABEL(16),NXP
  WRITE(KUNIT,'(A20,I4)') LABEL(17),NYP
  WRITE(KUNIT,'(A20,I4)') LABEL(18),(NZP+1)

! upper level information
  DO K=1,(NZP+1)  
     WRITE(LABEL(1),'(A6,I2,A1)')'Level ',K,':'

     IF(K.EQ.1)THEN
        SIG=1.0     
        WRITE(KUNIT,'(A20,F6.4,I3,99(1X,A4))')LABEL(1),                 &
              SIG,NSFC,(VSFC(N),N=1,NSFC)
     ELSE
        SIG=LEVEL(K-1)
        IF(SIG.LT.1.0)THEN
           WRITE(KUNIT,'(A20,F6.5,I3,99(1X,A4))')LABEL(1),              &
                 SIG,N3DM,(V3DM(N),N=1,N3DM)
        ELSEIF(SIG.GE.1.AND.SIG.LT.10.0)THEN
           WRITE(KUNIT,'(A20,F6.4,I3,99(1X,A4))')LABEL(1),              &
                 SIG,N3DM,(V3DM(N),N=1,N3DM)
        ELSEIF(SIG.GE.10.AND.SIG.LT.100.0)THEN
           WRITE(KUNIT,'(A20,F6.3,I3,99(1X,A4))')LABEL(1),              &
                 SIG,N3DM,(V3DM(N),N=1,N3DM)
        ELSEIF(SIG.GE.100.AND.SIG.LT.1000.0)THEN
           WRITE(KUNIT,'(A20,F6.2,I3,99(1X,A4))')LABEL(1),              &
                 SIG,N3DM,(V3DM(N),N=1,N3DM)
        ELSEIF(SIG.GE.1000)THEN
           WRITE(KUNIT,'(A20,F6.1,I3,99(1X,A4))')LABEL(1),              &
                 SIG,N3DM,(V3DM(N),N=1,N3DM)
        END IF
     END IF

  END DO
  CLOSE (KUNIT)

END SUBROUTINE cfgrec
