 PROGRAM BDY2BIN

! Converts ASCII boundary files to packed concentration format which
! can then be plotted using any of the HYSPLIT concentration plot programs.
! Note that the first data point on the first record is assumed to be
! centered at 180W and 90N, so that from the northwest corner the
! the data goes eastward and then southward.
! Last Revised: 16 Aug 2010

  IMPLICIT NONE

  CHARACTER(11) :: FNAME
  INTEGER(4)    :: NLAT=181
  INTEGER(4)    :: NLON=360
  REAL(4)       :: DLAT=1.0
  REAL(4)       :: DLON=1.0
  REAL(4)       :: CLAT=-90.0 
  REAL(4)       :: CLON=-180.0
  REAL(4)       :: RVAL(360,181)  
  INTEGER(4)    :: j,kret,numb

  CHARACTER(7)  ::  LABEL(3)
  INTEGER(4)    ::  DVAL(360) 

  DATA LABEL/'LANDUSE','ROUGLEN','TERRAIN'/

  WRITE(*,*)'1 - landuse.asc'
  WRITE(*,*)'2 - rouglen.asc'
  WRITE(*,*)'3 - terrain.asc'
  WRITE(*,*)'Select Input File Number: '      
  READ(*,*)  NUMB 

  OPEN(10,FILE=LABEL(NUMB)//'.ASC')

  J=181
  KRET=0
  DO WHILE (KRET.EQ.0)
     READ(10,'(360I4)',IOSTAT=KRET )DVAL
     RVAL(:,J)=DVAL
     J=J-1
  END DO

  CALL conwrite(LABEL(NUMB)//'.BIN',LABEL(NUMB)(1:4),nlat,nlon,dlat,dlon,clat,clon,rval)

 END PROGRAM BDY2BIN

!----------------------------------------------------------

SUBROUTINE CONWRITE (FNAME,VARB,NLAT,NLON,DLAT,DLON,CLAT,CLON,RVAL)

  IMPLICIT NONE

  CHARACTER(11), INTENT(IN) :: FNAME
  CHARACTER(4),  INTENT(IN) :: VARB
  INTEGER(4),    INTENT(IN) :: NLAT,NLON
  REAL(4),       INTENT(IN) :: DLAT,DLON,CLAT,CLON
  REAL(4),       INTENT(IN) :: RVAL(NLON,NLAT)
  INTEGER(4)                :: i,j

  OPEN(50,FILE=FNAME,FORM='UNFORMATTED',ACCESS='SEQUENTIAL')
  WRITE(50)VARB,0,0,0,0,0,1,0             ! MODEL,IYR,IMO,IDA,IHR,ICH,NLOC,CPACK
  WRITE(50)0,0,0,0,0.0,0.0,0.0            ! IBYR,IBMO,IBDA,IBHR,OLAT,OLON,OLVL
  WRITE(50)NLAT,NLON,DLAT,DLON,CLAT,CLON
  WRITE(50)1,0                            ! NLVL,(HEIGHT(KK),KK=1,NLVL)
  WRITE(50)1,VARB                         ! NTYP,(IDENT(KK),KK=1,NTYP)
  WRITE(50)0,0,0,0,0,0                    ! IYR,IMO,IDA,IHR,IMN,IFH
  WRITE(50)0,0,0,0,0,0
  WRITE(50) VARB,0,((RVAL(I,J),I=1,NLON),J=1,NLAT)
  CLOSE(50)

END SUBROUTINE conwrite
