!----------------------------------------------------------------------
! convert mapping from WRF to HYSPLIT parameters
! Last Revised: 08 Aug 2013 (RRD) - mercator tangent latitude correction
!		24 Mar 2014 (AFS) - mercator projection corrected 
!               26 Sep 2014 (RRD) - added lat/lon grid (iproj=6)
!               10 Oct 2017 (CPL) - Corrected projection for iproj=6
!               14 Dec 2017 (CPL) - Corrected REF_LAT for iproj=1
!               12 Feb 2018 (CPL) - set DUMMY to WRF-ARW pressure top
!               14 Oct 2021 (SYZ) - removed unused variables
!----------------------------------------------------------------------

SUBROUTINE setmap (diag,ncid,nxp,nyp,p_top)

  IMPLICIT NONE
  include 'netcdf.inc'

  LOGICAL,       INTENT(IN)  :: diag
  INTEGER,       INTENT(IN)  :: ncid
  INTEGER,       INTENT(IN)  :: nxp,nyp 
  REAL,          INTENT(IN)  :: p_top

  CHARACTER(80) :: label
  INTEGER       :: n,status,iproj
  REAL          :: kmpdeg
  REAL          :: clat,clon,orient,plat,tlat,tlon,dxkm,dykm
  REAL          :: GRIDS(12), PARMAP(9), EQVLAT

  COMMON / SETUP / GRIDS, PARMAP

  label='MAP_PROJ'
  n=INDEX(label,' ')-1
  status = NF_GET_ATT_INT(ncid, nf_global, label, iproj)
  IF(status.NE.nf_noerr) THEN
     WRITE(*,*) label(:n),':',NF_STRERROR(status)
  ELSE
     IF(diag)WRITE(*,*)label(:n),' = ',iproj
  END IF

  label='CEN_LAT'
  n=INDEX(label,' ')-1
  status = NF_GET_ATT_REAL(ncid, nf_global, label, clat)
  IF(status.NE.nf_noerr) THEN
     WRITE(*,*) label(:n),':',NF_STRERROR(status)
  ELSE
     IF(diag)WRITE(*,*)label(:n),' = ',clat
  END IF

  label='CEN_LON'
  n=INDEX(label,' ')-1
  status = NF_GET_ATT_REAL(ncid, nf_global, label, clon)
  IF(status.NE.nf_noerr) THEN
     WRITE(*,*) label(:n),':',NF_STRERROR(status)
  ELSE
     IF(diag)WRITE(*,*)label(:n),' = ',clon
  END IF

  label='STAND_LON'
  n=INDEX(label,' ')-1
  status = NF_GET_ATT_REAL(ncid, nf_global, label, orient)
  IF(status.NE.nf_noerr) THEN
     WRITE(*,*) label(:n),':',NF_STRERROR(status)
  ELSE
     IF(diag)WRITE(*,*)label(:n),' = ',orient
  END IF

  label='DX'
  n=INDEX(label,' ')-1
  status = NF_GET_ATT_REAL(ncid, nf_global, label, dxkm)
  dxkm=dxkm/1000.0
  IF(status.NE.nf_noerr) THEN
     WRITE(*,*) label(:n),':',NF_STRERROR(status)
  ELSE
     IF(diag)WRITE(*,*)label(:n),' = ',dxkm
  END IF

  label='DY'
  n=INDEX(label,' ')-1
  status = NF_GET_ATT_REAL(ncid, nf_global, label, dykm)
  dykm=dykm/1000.0
  IF(status.NE.nf_noerr) THEN
     WRITE(*,*) label(:n),':',NF_STRERROR(status)
  ELSE
     IF(diag)WRITE(*,*)label(:n),' = ',dykm
  END IF

  label='TRUELAT1'
  n=INDEX(label,' ')-1
  status = NF_GET_ATT_REAL(ncid, nf_global, label, tlat)
  IF(status.NE.nf_noerr) THEN
     WRITE(*,*) label(:n),':',NF_STRERROR(status)
  ELSE
     IF(diag)WRITE(*,*)label(:n),' = ',tlat
  END IF

  label='TRUELAT2'
  n=INDEX(label,' ')-1
  status = NF_GET_ATT_REAL(ncid, nf_global, label, plat)
  IF(status.NE.nf_noerr) THEN
     WRITE(*,*) label(:n),':',NF_STRERROR(status)
  ELSE
     IF(diag)WRITE(*,*)label(:n),' = ',plat
  END IF


!--------------------------------------------------------
! HYSPLIT packed data configuration


! grid orientation
  GRIDS(6)=0.0     

! delta=x grid size in km
  GRIDS(5)=0.5*(dxkm+dykm)

! synch point in x,y coordintes
  GRIDS(8)=0.5*(NXP+1)
  GRIDS(9)=0.5*(NYP+1)

! synch point in lat/lon coordinates
  GRIDS(10)=CLAT 
  GRIDS(11)=CLON 

! variable reserved for future use
!  GRIDS(12)=0.0
! WRF model top pressure
  GRIDS(12)=p_top/100.

! defines a polar sterographic projection
  IF(iproj.EQ.2)THEN

     IF(TLAT.NE.PLAT)THEN
        WRITE(*,*)'ERROR: incorrectly defined polar projection in WRF!'
        WRITE(*,*)'TRUELAT1 does not equal TRUELAT2: ',TLAT,PLAT
        WRITE(*,*)'TRUELAT1 defines the latitude at which the grid resolution is defined.'
        STOP 900
     END IF

!    set the pole position and reference lat/lon
     GRIDS(1)=90.0  
     IF(CLAT.LT.0.0) GRIDS(1)=-90.0

!    pole longtitude (+180 from cut)
     GRIDS(2)=ORIENT

!    reference lat (at which grid size specified)
     GRIDS(3)=TLAT

!    reference longitude and grid alignment
     GRIDS(4)=ORIENT

!    cone angle / tangent latitude
     GRIDS(7)=GRIDS(1)

! defines a mercator projection
  ELSEIF(iproj.EQ.3)THEN

     if(plat.gt.90.0.or.plat.lt.-90.0)plat=0.0
     if(orient.gt.180.0.or.orient.lt.-180.0)orient=clon

!    pole lat/lon axis through pole
     if(clat.lt.0.0)GRIDS(1)=-90.0
     GRIDS(2)=ORIENT

!    reference lat
!    GRIDS(3)=eqvlat(TLAT,PLAT)
     GRIDS(3)=TLAT

!    reference lon
     GRIDS(4)=ORIENT

!    cone angle / tangent latitude
!    GRIDS(7)=GRIDS(1)
     GRIDS(7)=0.0

! defines a lambert conformal projection
  ELSEIF(iproj.EQ.1)THEN

!    pole lat/lon axis through pole
     GRIDS(1)=eqvlat(TLAT,PLAT)
     GRIDS(2)=ORIENT

!    reference lat
     GRIDS(3)=TLAT

!    reference lon
     GRIDS(4)=ORIENT

!    cone angle / tangent latitude
     GRIDS(7)=GRIDS(1)

! defines a latitude-longitude grid (use 0->360) 
  ELSEIF(iproj.EQ.6)THEN

!    kilometers per degree latitude (radius earth = 6370 km)
!    define only to the nearest 0.1 degree
!    note HYSPLIT assumes earth = 6371.2 km
     KMPDEG=2.0*3.14159265*6370.0/360.0
     GRIDS(3)=dykm/kmpdeg
     GRIDS(4)=dxkm/kmpdeg
     IF(GRIDS(4).LT.0.0) GRIDS(4)=360.0+GRIDS(4)

     GRIDS(5)=0.0
     GRIDS(6)=0.0
     GRIDS(7)=0.0
     GRIDS(8)=1.0
     GRIDS(9)=1.0

     GRIDS(10)=CLAT-FLOAT(NYP-1)/2.*GRIDS(3)
     GRIDS(11)=CLON-FLOAT(NXP-1)/2.*GRIDS(4)

     IF(GRIDS(11).LT.0.0) GRIDS(11)=360.0+GRIDS(11)
     
     GRIDS(1)=GRIDS(10)+GRIDS(3)*(NYP-1)
     GRIDS(2)=GRIDS(11)+GRIDS(4)*(NXP-1) 

  ELSE
     WRITE(*,*)'Undefined projection: ',iproj
     STOP
  END IF


!------------------------------------------------------
! test projection with ARL grid conversion routines
! doesn't work for cyclindrical equidistant or lat-lon grids
  IF(IPROJ.EQ.0.OR.IPROJ.EQ.6) RETURN

! define the tangent latitude and reference longitude
  CALL STLMBR(PARMAP,GRIDS(7),GRIDS(4))

! define the grid by a one-point specification
  CALL STCM1P(PARMAP,GRIDS(8),GRIDS(9),GRIDS(10),GRIDS(11),                &
                     GRIDS(3),GRIDS(4),GRIDS(5),GRIDS(6))

! determine the corner lat/lon at the grid locations
  CALL CXY2LL(PARMAP,1.0,1.0,TLAT,TLON)
  IF(diag) WRITE(*,*) 'SW corner point: ',tlat,tlon
  CALL CXY2LL(PARMAP,FLOAT(nxp+1),FLOAT(nyp+1),TLAT,TLON)
  IF(diag) WRITE(*,*) 'NE corner point: ',tlat,tlon

END SUBROUTINE setmap 

