!======================================================================
! Converts HYSPLIT binary gridded concentration file to NetCDF Version 3
! Last Revised: 29 Oct 2010 (RRD) - initial version
!               31 Jul 2012 (SL)  - CF compliant
!               27 Aug 2012 (SL)  - date correction (-1 day)
!               25 Jan 2018 (CPL) - renamed conc2cdf.f to con2cdf3.f,
!                                   fixed origin outputs, and
!                                   applied Matthew Krupcale's patches: 
!                                   improved memory efficiency for IO
!                                   and fixed gettime subroutine
!               23 Feb 2021 (CPL) - changee 60. to DBLE(60) in IC eqn
!               14 Oct 2021 (SYZ) - removed unused variables
!======================================================================

PROGRAM CON2CDF3

  IMPLICIT NONE

  CHARACTER(256)            :: LABEL, INPFILE, OUTFILE
  CHARACTER(4)              :: PTYPE, MODEL
  CHARACTER(4), ALLOCATABLE :: IDENT (:)

  REAL,         ALLOCATABLE :: CSUM  (:,:,:)

  REAL,         ALLOCATABLE :: LONS(:), LATS(:)
  INTEGER,      ALLOCATABLE :: HEIGHT(:)
  INTEGER,      ALLOCATABLE :: MTIME(:)
  REAL,         ALLOCATABLE :: OLAT(:),OLON(:),OLVL(:)
  REAL*8,       ALLOCATABLE :: starttime(:), endtime(:)
  REAL*8,       ALLOCATABLE :: bounds(:,:)

  LOGICAL    :: ftest
  INTEGER(2) :: ip,jp
  INTEGER    :: ich,ii,jj,kk,kl,kp,n,nloc,nlon,nlat,cpack,iargc
  INTEGER    :: np,nxyp,kret,krec,ntim,ntyp,nlvl,level,narg,karg 
  INTEGER    :: iyr,imo,ida,ihr,imn,ifh,ibyr,ibmo,ibda,ibhr,ibmn
  REAL       :: clon,clat,dlon,dlat,ilon,ilat,ilvl

  INTEGER, PARAMETER :: century=20

! NetCDF definitions

  CHARACTER(26)             :: LONG_NAME

  INTEGER :: ncid,condim(4),idolon,idolat,idolvl,idotim
  INTEGER :: idhgts
  INTEGER :: idetim
  INTEGER :: idlon,idlat
  INTEGER :: ibnds,ibndsdim,idbnds
  INTEGER :: ioriginsdim

  INTEGER, ALLOCATABLE :: idconc(:)

  INCLUDE 'netcdf.inc'

!--------------------------------

  INPFILE='cdump'
  OUTFILE='cdump.nc'

  KARG=IARGC()
  IF(KARG.GE.1)THEN
     DO NARG=1,KARG
        CALL GETARG(NARG,LABEL)
        IF(LABEL(1:2).EQ.'-I'.OR.LABEL(1:2).EQ.'-i') INPFILE=LABEL(3:)
        IF(LABEL(1:2).EQ.'-O'.OR.LABEL(1:2).EQ.'-o') OUTFILE=LABEL(3:)
     END DO
  ELSE
     WRITE(*,*)'Usage: con2cdf3 -i{input binary file name} -o{output NetCDF file name}'
     WRITE(*,*)'       convert HYSPLIT binary to NetCDF: default = -icdump -ocdump.nc'
     WRITE(*,*)' '
     STOP
  END IF

  INQUIRE (FILE=INPFILE, EXIST=FTEST)
  IF(FTEST)THEN
     OPEN(10,FILE=INPFILE,FORM='UNFORMATTED',ACCESS='SEQUENTIAL')
  ELSE
     WRITE(*,*)'File not found: ',INPFILE
     STOP
  END IF

!-------------------------------
! meteo file information and calculation start date

  READ(10,IOSTAT=kret)MODEL,IYR,IMO,IDA,IHR,ICH, NLOC, CPACK
  IF(kret.ne.0)CPACK=0   ! old format files don't have cpack
  IF(cpack.GT.1) STOP 9999

  ALLOCATE (olat(nloc), STAT=kret)
  ALLOCATE (olon(nloc), STAT=kret)
  ALLOCATE (olvl(nloc), STAT=kret)
  ALLOCATE (mtime(nloc), STAT=kret)

  DO N=1,NLOC
     READ(10,IOSTAT=kret)IBYR,IBMO,IBDA,IBHR,ILAT,ILON,ILVL,IBMN
     OLAT(N)=ilat
     OLON(N)=ilon
     OLVL(N)=ilvl
     MTIME(N)=ibyr*1000000+ibmo*10000+ibda*100+ibhr
     IF(kret.ne.0)IBMN=0
  END DO

! horizontal grid index record
  READ(10) NLAT, NLON, DLAT, DLON, CLAT, CLON
  ALLOCATE (LONS(nlon), STAT=kret)
  ALLOCATE (LATS(nlat), STAT=kret)

! vertical grid index record
  READ(10) NLVL
  BACKSPACE(10)
  ALLOCATE (height(nlvl), STAT=kret)
  READ(10) NLVL, (HEIGHT(KK),KK=1,NLVL)

! pollutant identification record
  READ(10) NTYP
  BACKSPACE(10)
  ALLOCATE (IDENT(ntyp), IDCONC(ntyp), STAT=kret)
  READ(10) NTYP, (IDENT(KK),KK=1,NTYP)

! number of time periods
  KRET=0
  NTIM=0
  tloop : DO WHILE (KRET.EQ.0)
     READ(10,IOSTAT=kret)
     IF(kret.NE.0) EXIT tloop 
     READ(10)  
     NTIM=NTIM+1
     DO KP=1,NTYP
     DO KL=1,NLVL
        READ(10)
     END DO
     END DO
  END DO tloop

! rewind and reposition to data records
  REWIND(10)
  KREC=4+NLOC
  DO KK=1,KREC
     READ(10) 
  END DO

! main array allocation
  ALLOCATE (csum(nlon,nlat,nlvl), STAT=kret)
  ALLOCATE (starttime(ntim), endtime(ntim), STAT=kret)
  ALLOCATE (bounds(2,ntim), STAT=kret)

! define NetCDF file  

  kret=NF_CREATE(OUTFILE, NF_CLOBBER, ncid)
  IF(kret.NE.NF_NOERR) WRITE(*,*)'create',NF_STRERROR(kret)

! Add global attributes
  kret=NF_PUT_ATT_TEXT(ncid, NF_GLOBAL, 'title', 34, 'HYSPLIT Model Concentration Output')
  IF(kret.NE.NF_NOERR) WRITE(*,*)'title', NF_STRERROR(kret)
  kret=NF_PUT_ATT_TEXT(ncid, NF_GLOBAL, 'Conventions', 6, 'CF-1.5')
  IF(kret.NE.NF_NOERR) WRITE(*,*)'conventions', NF_STRERROR(kret)
! define dimensions

  kret=NF_DEF_DIM(ncid, 'longitude', nlon, condim(1)) 
  IF(kret.NE.NF_NOERR) WRITE(*,*)'longitude',NF_STRERROR(kret)
  kret=NF_DEF_DIM(ncid, 'latitude', nlat, condim(2)) 
  IF(kret.NE.NF_NOERR) WRITE(*,*)'latitude',NF_STRERROR(kret)
  kret=NF_DEF_DIM(ncid, 'levels', nlvl, condim(3))
  IF(kret.NE.NF_NOERR) WRITE(*,*)'levels',NF_STRERROR(kret)
  kret=NF_DEF_DIM(ncid, 'time', NF_UNLIMITED, condim(4)) 
  IF(kret.NE.NF_NOERR) WRITE(*,*)'time',NF_STRERROR(kret)
  ibnds=2
  kret=NF_DEF_DIM(ncid, 'bnds', ibnds, ibndsdim) 
  IF(kret.NE.NF_NOERR) WRITE(*,*)'bnds',NF_STRERROR(kret)
  kret=NF_DEF_DIM(ncid, 'origins', nloc, ioriginsdim)
  IF(kret.NE.NF_NOERR) WRITE(*,*)'origins',NF_STRERROR(kret)

! define grid variables
  kret=NF_DEF_VAR(ncid, 'latitude', NF_FLOAT, 1, condim(2), idlat)
  IF(kret.NE.NF_NOERR) WRITE(*,*)'latitude',NF_STRERROR(kret)
  kret=NF_PUT_ATT_TEXT(ncid, idlat, 'long_name', 39, 'latitude degrees north from the equator')
  IF(kret.NE.NF_NOERR) WRITE(*,*)'long_name',NF_STRERROR(kret)
  kret=NF_PUT_ATT_TEXT(ncid, idlat, 'units', 13, 'degrees_north')
  IF(kret.NE.NF_NOERR) WRITE(*,*)'units',NF_STRERROR(kret)
  kret=NF_PUT_ATT_TEXT(ncid, idlat, 'point_spacing', 4, 'even')
  IF(kret.NE.NF_NOERR) WRITE(*,*)'point_spacing',NF_STRERROR(kret)

  kret=NF_DEF_VAR(ncid, 'longitude', NF_FLOAT, 1, condim(1), idlon)
  IF(kret.NE.NF_NOERR) WRITE(*,*)'longitude',NF_STRERROR(kret)
  kret=NF_PUT_ATT_TEXT(ncid, idlon, 'long_name', 50, 'longitude degrees east from the greenwich meridian')
  IF(kret.NE.NF_NOERR) WRITE(*,*)'long_name',NF_STRERROR(kret)
  kret=NF_PUT_ATT_TEXT(ncid, idlon, 'units', 12, 'degrees_east')
  IF(kret.NE.NF_NOERR) WRITE(*,*)'units',NF_STRERROR(kret)
  kret=NF_PUT_ATT_TEXT(ncid, idlon, 'point_spacing', 4, 'even')
  IF(kret.NE.NF_NOERR) WRITE(*,*)'point_spacing',NF_STRERROR(kret)

  kret=NF_DEF_VAR(ncid, 'levels', NF_INT, 1, condim(3), idhgts)
  IF(kret.NE.NF_NOERR) WRITE(*,*)'levels',NF_STRERROR(kret)
  kret=NF_PUT_ATT_TEXT(ncid, idhgts, 'long_name', 24, 'Top height of each layer')
  IF(kret.NE.NF_NOERR) WRITE(*,*)'long_name',NF_STRERROR(kret)
  kret=NF_PUT_ATT_TEXT(ncid, idhgts, 'units', 1, 'm')
  IF(kret.NE.NF_NOERR) WRITE(*,*)'units',NF_STRERROR(kret)

! define time grid
  
  kret=NF_DEF_VAR(ncid, 'time', NF_DOUBLE , 1, condim(4), idetim)
  IF(kret.NE.NF_NOERR) WRITE(*,*)'time',NF_STRERROR(kret)
  kret=NF_PUT_ATT_TEXT(ncid, idetim, 'units', 30, 'days since 1970-01-01 00:00:00')
  IF(kret.NE.NF_NOERR) WRITE(*,*)'units',NF_STRERROR(kret)
  kret=NF_PUT_ATT_TEXT(ncid, idetim, 'bounds', 9, 'time_bnds')
  IF(kret.NE.NF_NOERR) WRITE(*,*)'bounds',NF_STRERROR(kret)
  kret=NF_PUT_ATT_TEXT(ncid, idetim, 'calendar', 9, 'gregorian')
  IF(kret.NE.NF_NOERR) WRITE(*,*)'calendar',NF_STRERROR(kret)
  kret=NF_PUT_ATT_TEXT(ncid, idetim, 'standard_name', 4, 'time')
  IF(kret.NE.NF_NOERR) WRITE(*,*)'standard_name',NF_STRERROR(kret)

! define simulation origin variables
  kret=NF_DEF_VAR(ncid, 'olat', NF_FLOAT, 1, ioriginsdim, idolat)
  IF(kret.NE.NF_NOERR) WRITE(*,*)'olat',NF_STRERROR(kret)
  kret=NF_PUT_ATT_TEXT(ncid, idolat, 'long_name', 26, 'Simulation origin latitude')
  IF(kret.NE.NF_NOERR) WRITE(*,*)'long_name',NF_STRERROR(kret)

  kret=NF_DEF_VAR(ncid, 'olon', NF_FLOAT, 1, ioriginsdim, idolon)
  IF(kret.NE.NF_NOERR) WRITE(*,*)NF_STRERROR(kret)
  kret=NF_PUT_ATT_TEXT(ncid, idolon, 'long_name', 27, 'Simulation origin longitude')
  IF(kret.NE.NF_NOERR) WRITE(*,*)'long_name',NF_STRERROR(kret)

  kret=NF_DEF_VAR(ncid, 'olvl', NF_FLOAT, 1, ioriginsdim, idolvl)
  IF(kret.NE.NF_NOERR) WRITE(*,*)'olvl',NF_STRERROR(kret)
  kret=NF_PUT_ATT_TEXT(ncid, idolvl, 'long_name', 23, 'Simulation origin level')
  IF(kret.NE.NF_NOERR) WRITE(*,*)'long_name',NF_STRERROR(kret)

  kret=NF_DEF_VAR(ncid, 'otim', NF_INT , 1, ioriginsdim, idotim)
  IF(kret.NE.NF_NOERR) WRITE(*,*)'otim',NF_STRERROR(kret)
  kret=NF_PUT_ATT_TEXT(ncid, idotim, 'long_name', 37, 'Simulation origin start time YYMMDDHH')
  IF(kret.NE.NF_NOERR) WRITE(*,*)'long_name',NF_STRERROR(kret)

! define concentration array structure

  DO KP=1,NTYP
     kret=NF_DEF_VAR(ncid,ident(KP), NF_FLOAT, 4, condim, idconc(KP))
     IF(kret.NE.NF_NOERR) WRITE(*,*)ident(KP),NF_STRERROR(kret)
     LONG_NAME='Concentration Array - '//ident(KP)
     kret=NF_PUT_ATT_TEXT(ncid, idconc(KP), 'long_name', 26, LONG_NAME)
     IF(kret.NE.NF_NOERR) WRITE(*,*)'long_name',NF_STRERROR(kret)
  ENDDO

! define time bounds

  kret=NF_DEF_VAR(ncid, 'time_bnds', NF_DOUBLE, 2, (/ ibndsdim, condim(4) /), idbnds)
  IF(kret.NE.NF_NOERR) WRITE(*,*)'time_bnds',NF_STRERROR(kret)

! close attribute definition and write data

  kret=NF_ENDDEF(ncid)
  IF(kret.NE.NF_NOERR) WRITE(*,*)ncid, NF_STRERROR(kret)

! build latitude and longitude grid
  DO II=1,NLON
     LONS(II)=CLON+(II-1)*DLON
  ENDDO
  DO JJ=1,NLAT
     LATS(JJ)=CLAT+(JJ-1)*DLAT
  ENDDO

  kret=NF_PUT_VAR_REAL(ncid, idlon, lons) 
  IF(kret.NE.NF_NOERR) WRITE(*,*)idlon, NF_STRERROR(kret)
  kret=NF_PUT_VAR_REAL(ncid, idlat, lats) 
  IF(kret.NE.NF_NOERR) WRITE(*,*)idlat, NF_STRERROR(kret)
  kret=NF_PUT_VAR_INT (ncid, idhgts, height)
  IF(kret.NE.NF_NOERR) WRITE(*,*)idhgts, NF_STRERROR(kret)
  
  kret=NF_PUT_VAR_REAL(ncid, idolon, olon)
  IF(kret.NE.NF_NOERR) WRITE(*,*)idolon, NF_STRERROR(kret)
  kret=NF_PUT_VAR_REAL(ncid, idolat, olat)
  IF(kret.NE.NF_NOERR) WRITE(*,*)idolat, NF_STRERROR(kret)
  kret=NF_PUT_VAR_REAL(ncid, idolvl, olvl)
  IF(kret.NE.NF_NOERR) WRITE(*,*)idolvl, NF_STRERROR(kret)
  
  kret=NF_PUT_VAR_INT (ncid, idotim, mtime)
  IF(kret.NE.NF_NOERR) WRITE(*,*)idotim, NF_STRERROR(kret)

! read and output time and concentration arrays
  DO KK=1,NTIM
     NXYP=0

     READ(10)IYR,IMO,IDA,IHR,IMN,IFH
     call gettime(century,iyr,imo,ida,ihr,imn,starttime(KK))
     READ(10)IYR,IMO,IDA,IHR,IMN,IFH
     call gettime(century,iyr,imo,ida,ihr,imn,endtime(KK))

     DO KP=1,NTYP
     CSUM = 0.0
     DO KL=1,NLVL
        IF(CPACK.EQ.1)THEN
           READ(10)PTYPE,LEVEL,NXYP,(IP,JP,CSUM(IP,JP,KL),NP=1,NXYP)
        ELSE
           READ(10)PTYPE,LEVEL,((CSUM(II,JJ,KL),II=1,NLON),JJ=1,NLAT)
        END IF
     END DO
     kret=NF_PUT_VARA_REAL(ncid, idconc(KP), (/ 1, 1, 1, KK /), &
                           (/ NLON, NLAT, NLVL, 1 /), csum(:,:,:))
     IF(kret.NE.NF_NOERR) WRITE(*,*)idconc(KP), NF_STRERROR(kret)
     END DO
  END DO

  kret=NF_PUT_VAR_DOUBLE (ncid, idetim, endtime) 
  IF(kret.NE.NF_NOERR) WRITE(*,*)idetim, NF_STRERROR(kret)

  bounds(1,:) = starttime
  bounds(2,:) = endtime
  kret=NF_PUT_VAR_DOUBLE (ncid, idbnds, bounds) 
  IF(kret.NE.NF_NOERR) WRITE(*,*)idbnds, NF_STRERROR(kret)

  kret=NF_CLOSE(ncid)
  IF(kret.NE.NF_NOERR) WRITE(*,*)ncid, NF_STRERROR(kret)

  DEALLOCATE (lons,lats,height,STAT=kret)
  DEALLOCATE (ident,idconc,starttime,endtime,bounds,STAT=kret)
  DEALLOCATE (csum,STAT=kret)
  DEALLOCATE (olat,olon,olvl,mtime)

END PROGRAM con2cdf3

!**********************************************************************************************
FUNCTION days_from_civil(Iyear, Imonth, Iday)
! Returns number of days since civil 1970-01-01.  Negative values indicate
!   days prior to 1970-01-01.
! Preconditions:  y-m-d represents a date in the civil (Gregorian) calendar
! Adapted from algorithm by Howard Hinnant[1]
! [1] http://howardhinnant.github.io/date_algorithms.html#days_from_civil
  Implicit None
  Integer, Intent(In)::         Iyear,Imonth,Iday
  Integer::                     days_from_civil
  Integer::                     y, era, yoe, doy, doe
  IF (Imonth.le.2) THEN
     y = Iyear - 1
  ELSE
     y = Iyear
  END IF
  IF (y.ge.0) THEN
     era = y / 400
  ELSE
     era = (y-399) / 400
  END IF
  yoe = y - era * 400                          ! [0, 399]
  IF (Imonth.gt.2) THEN
     doy = (153*(Imonth - 3) + 2)/5 + Iday-1   ! [0, 365]
  ELSE
     doy = (153*(Imonth + 9) + 2)/5 + Iday-1   ! [0, 365]
  END IF
  doe = yoe * 365 + yoe/4 - yoe/100 + doy      ! [0, 146096]
  days_from_civil = era * 146097 + doe - 719468
END FUNCTION days_from_civil

!**********************************************************************************************
SUBROUTINE gettime(Icentury,Iyear,Imonth,Iday,Ihour,Iminute,IC)
!Subroutine to compute days since 1970-1-1 0:0:0 from date and time in file

  Implicit None
  Integer, Intent(In)::         Iday,Imonth,Iyear,Icentury,Ihour,Iminute
  Real*8, Intent(Out)::         IC
  Integer::                     y, days_from_civil
  IF (Iyear.lt.100) THEN
     y = Iyear + Icentury*100
  ELSE
     y = Iyear
  END IF
  IC = days_from_civil(y, Imonth, Iday) + (Ihour +(Iminute)/DBLE(60))/24.
END SUBROUTINE gettime
