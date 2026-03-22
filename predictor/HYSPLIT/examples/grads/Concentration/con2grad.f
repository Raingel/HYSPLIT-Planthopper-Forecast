PROGRAM CON2GRADS

!-------------------------------------------------------------------------------
! converts binary hysplit concentration file to grads binary format
! first edition: Jan H Vermeulen 24 Febr. 97
!-------------------------------------------------------------------------------
! revised (rrd): 14 Jan 2000
!              : 18 Dec 2000 - fortran90 upgrade
!              : 01 Mar 2001 - GrADS interface upgrade
!              : 21 May 2002 - ctl file format change (shm)
!              : 23 Dec 2004 - concentration packing test
!-------------------------------------------------------------------------------

  LOGICAL       :: FTEST,depos
  REAL          :: CINT(8)
  INTEGER(2)    :: ip,jp
  INTEGER       :: cpack 
  CHARACTER(4)  :: PTYPE, MODEL
  CHARACTER(40) :: LABEL
      
  REAL,         ALLOCATABLE :: CSUM  (:,:)
  CHARACTER(4), ALLOCATABLE :: IDENT (:) 
  INTEGER,      ALLOCATABLE :: HEIGHT(:)

      character fname*80,dummy*80,months(12)*3
      data months/'JAN','FEB','MAR','APR','MAY','JUN',                         &
                  'JUL','AUG','SEP','OCT','NOV','DEC'/
      data depos/.false./

! input file information
! WIN32:NARGS()=arguments+command    UNIX:IARGC()=arguments
      narg=iargc()
!     narg=nargs()-1

      if(narg.EQ.0)then
         write(*,*)'Usage: con2grads [filename]'
         stop
      else
         call getarg(1,fname)
         inquire (file=fname, exist=ftest)
         if(ftest)then
            open(10,file=fname,form='unformatted',access='sequential')
         else
            write(*,*)'File not found:',fname
            stop
         end if
      end if

! file initialization and array allocation

  CALL CONSET(nloc,nlon,nlat,nlvl,ntyp)
  ALLOCATE (csum(nlon,nlat), STAT=kret)
  ALLOCATE (height(nlvl), STAT=kret)
  ALLOCATE (IDENT(ntyp), STAT=kret) 

! input file information

!     meteo file information and calculation start data
      READ(10,IOSTAT=KRET)MODEL,IYR,IMO,IDA,IHR,ICH, NLOC, CPACK
      IF(KRET.NE.0)CPACK=0
      IF(CPACK.GT.1)THEN
         WRITE(*,*)'*ERROR* unsupported concentration packing: ',CPACK
         STOP
      END IF
      write(*,'(a,5i4)')model,iyr,imo,ida,ihr,ich
           
      DO N=1,NLOC
         READ(10)IBYR,IBMO,IBDA,IBHR,OLAT,OLON,OLVL
         write(*,'(4i3,3f7.2)')ibyr,ibmo,ibda,ibhr,olat,olon,olvl
      END DO

!     horizontal grid index record
      READ(10) NLAT, NLON, DLAT, DLON, CLAT, CLON
      write(*,'(A20,2I10)')  ' Numb points lat/lon:',nlat,nlon
      write(*,'(A20,2F10.2)')'   Increment lat/lon:',dlat,dlon
      write(*,'(A20,2F10.2)')'      Corner lat/lon:',clat,clon

!     open grads file
      LREC=4*NLAT*NLON
      OPEN(25,FILE='concen.grd',RECL=LREC,ACCESS='DIRECT',FORM='UNFORMATTED')

!     mapping limits
      alatb=clat+dlat*nlat
      alonl=clon+dlon*nlon
      alatt=clat
      alonr=clon
      dmax=0.0
      dmin=1.0E+25
      cmax=0.0
      cmin=1.0E+25

!     time interval of data
      kdel=0

!     vertical grid index record
      READ(10) NLVL, (HEIGHT(KK),KK=1,NLVL)
      write(*,'(A20,20I5)')' Levels and heights:', nlvl,(height(kk),kk=1,nlvl)

!     pollutant identification record
      READ(10) NTYP, (IDENT(KK),KK=1,NTYP)
      write(*,'(A20,I5,20A5)')' Number and ID:', ntyp,(ident(kk),kk=1,ntyp)

      NREC=0
      NTIM=0
      DO WHILE (NTIM.GE.0)

         READ(10,END=900)KYR,KMO,KDA,KHR,KMN,KFH
         write(*,'(A20,5I5)')' Sample start:',kyr,kmo,kda,khr,kfh
         READ(10)IYR,IMO,IDA,IHR,IMN,IFH
         write(*,'(A20,5I5)')' Sample stop: ',iyr,imo,ida,ihr,ifh

!        save initial dump time for GrADS control file
         IF(NTIM.EQ.0)THEN
           if(iyr.lt.50)then
	      kyear=2000+iyr
           else
              kyear=1900+iyr
           end if
           imog=imo
           idag=ida
           ihrg=ihr
         END IF

!        set sampling data time interval
         if(kdel.eq.0)then
            kdel=(ida*24+ihr)-(kda*24+khr)
         end if

         DO KP=1,NTYP
         DO KL=1,NLVL

            IF(CPACK.EQ.1)THEN
               CSUM = 0.0
               READ(10)PTYPE,LEVEL,NXYP,(IP,JP,CSUM(IP,JP),NP=1,NXYP)
            ELSE
               READ(10)PTYPE,LEVEL,((CSUM(II,JJ),II=1,NLON),JJ=1,NLAT)
            END IF

            write(*,'(A20,A5,I5)')' P-Ident and Level:',ptype,level
            IF(PTYPE.NE.IDENT(KP))THEN
               WRITE(*,*)'Variables does not match:',PTYPE,IDENT(KP)
               STOP
            END IF

!           check for deposition
            if(level.eq.0)depos=.true.

!           check limits and max values
            do j=1,nlat
            do i=1,nlon
               if(csum(i,j).gt.0.0)then
                  if(depos.and.level.eq.0)then
                    dmax=max(dmax,csum(i,j))
                    dmin=min(dmin,csum(i,j))
                  else
                    cmax=max(cmax,csum(i,j))
                    cmin=min(cmin,csum(i,j))
                  end if
                  plat=clat+(j-1)*dlat
                  plon=clon+(i-1)*dlon
                  alatt=max(alatt,plat)
                  alonr=max(alonr,plon)
                  alatb=min(alatb,plat)
                  alonl=min(alonl,plon)
               end if
            end do
            end do

!           output to grads file
            nrec=nrec+1
            write(25,rec=nrec)((csum(ii,jj),ii=1,nlon),jj=1,nlat)

         END DO
         END DO
         NTIM=NTIM+1

      END DO
  900 CONTINUE

! create grads control script files 
      open(30,file='concen.ctl')
      write(30,'(a)')'dset ^concen.grd'
      write(30,'(a)')'title 99 Hysplit Model Data'
      write(30,'(a)')'undef -9.99E33'
      write(30,'(a)')'options big_endian'
      write(30,'(a,i3,a,1x,f8.2,1x,f6.3)')'xdef ',nlon,' linear ',clon,dlon
      write(30,'(a,i3,a,1x,f8.2,1x,f6.3)')'ydef ',nlat,' linear ',clat,dlat
      if(depos)then
         write(30,'(a,i2,a,10i6)')     'zdef ',nlvl-1,' levels ',(height(k),k=2,nlvl)
         write(30,'(a,i2,a,i2.2,a1,i2.2,a3,2i4,a)')                               &
              'tdef ',ntim,' linear ',ihrg,'z',idag,months(imog),kyear,kdel,'hr'
         write(30,'(a,i3)')'vars ',ntyp+1
         write(30,'(a7,i3,a)')ident(1)//'dep',0,' 99 deposition'
         do k=1,ntyp
            write(30,'(a7,i3,a)')ident(k)//'con',nlvl-1,' 99 concentration'
         end do
      else
         write(30,'(a,i2,a,10i6)')     'zdef ',nlvl,' levels ',(height(k),k=1,nlvl)
         write(30,'(a,i2,a,i2.2,a1,i2.2,a3,2i4,a)')                               &
              'tdef ',ntim,' linear ',ihrg,'z',idag,months(imog),kyear,kdel,'hr'
         write(30,'(a,i3)')'vars ',ntyp
         do k=1,ntyp
            write(30,'(a7,i3,a)')ident(k)//'con',nlvl,' 99 concentration'
         end do
      end if
      write(30,'(a)')'endvars'
      close(30)

! initial default mapping
      jrng=10
      if(alatb.lt.0.0)alatb=alatb-jrng
      if(alatt.gt.0.0)alatt=alatt+jrng
      if(alonl.lt.0.0)alonl=alonl-jrng
      if(alonr.gt.0.0)alonr=alonr+jrng
      lat1=int(alatb/jrng)*jrng
      lat2=int(alatt/jrng)*jrng
      lon1=int(alonl/jrng)*jrng
      lon2=int(alonr/jrng)*jrng

      if(ibyr.lt.50)then
          kyear=2000+ibyr
      else
          kyear=1900+ibyr
      end if
      if(depos)then
!       contour levels
        nmax=nint(alog10(dmax))
        nmin=nint(alog10(dmin))
        ncnt=1+(iabs(nmax-nmin)-1)/8
        cint(8)=10.0**nmax
        do k=7,1,-1
           cint(k)=cint(k+1)/10.0**ncnt
        end do

!       create initial grads display script
        open(30,file='species.dep')
        write(30,'(a)')ident(1)//'dep'
        write(dummy,'(2a)')ident(1)//' deposition'
        write(30,'(a)')dummy(1:15)
        write(30,'(8e8.1)')cint
        write(30,'(a)')'0 9 4 5 3 7 8 2 0'
        close(30)
      end if

! concentration contour levels
      nmax=nint(alog10(cmax))
      nmin=nint(alog10(cmin))
      ncnt=1+(iabs(nmax-nmin)-1)/8
      cint(8)=10.0**nmax
      do k=7,1,-1
         cint(k)=cint(k+1)/10.0**ncnt
      end do

! create initial grads display script
      open(30,file='species.con')
      write(30,'(a)')ident(1)//'con'
      write(dummy,'(2a)')ident(1)//' concentration'
      write(30,'(a)')dummy(1:18)
      write(30,'(8e8.1)')cint
      write(30,'(a)')'0 9 4 5 3 7 8 2 0'

      close(30)
      close(25)

END PROGRAM con2grads

!-------------------------------------------------------------------------------

SUBROUTINE CONSET(nloc,nlon,nlat,nlvl,ntyp)

  INTEGER(2)   :: ip,jp
  INTEGER      :: cpack, height 
  CHARACTER(4) :: IDENT, PTYPE, MODEL

! meteo file information and calculation start data
  READ(10,IOSTAT=kret)MODEL,IYR,IMO,IDA,IHR,ICH, NLOC, CPACK
  IF(kret.ne.0)CPACK=0   ! old format files don't have cpack

! calculation start for each location
  DO N=1,NLOC
     READ(10)IBYR,IBMO,IBDA,IBHR,OLAT,OLON,OLVL
  END DO

! horizontal grid index record
  READ(10) NLAT, NLON, DLAT, DLON, CLAT, CLON

! vertical grid index record
  READ(10) NLVL, (HEIGHT,KK=1,NLVL)

! pollutant identification record
  READ(10) NTYP, (IDENT,KK=1,NTYP)

  REWIND(10)

END SUBROUTINE conset  
