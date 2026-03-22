!$$$  PROGRAM DOCUMENTATION BLOCK
!
! PROGRAM:     ETA04ARL          DECODE GRIB ETA (165) MODEL FIELD FOR HYSPLIT
!   PRGMMR:    GLENN ROLPH      ORG: REAR       DATE:02-01-30
!
! ABSTRACT:  THIS CODE WRITTEN AT THE AIR RESOURCES LABORATORY ...
!     converts 4 km eta model pressure grib file to arl packed format.
!     Processes only one time period per execution.  Multiple input files
!     may be specified on the command line, each containing different
!     variables required for the specific time period.  Input data are
!     assumed to be already on a conformal map projection on model levels.
!     Only packing to ARL format and units conversion is required.
!
! PROGRAM HISTORY LOG:
! LAST REVISED    30 Jan 2002 (GDR) - converted to eta 4 km
!
! USAGE: ETA04ARL [GRIB_FILE_NAME] [GRIB_FILE_NAME2] [...]
!   INPUT ARGUMENT LIST:
!     GRIB_FILE_NAME - multiple grib files may be defined
!   OUTPUT ARGUMENT LIST:
!     NONE
!   INPUT FILES:
!     GRIB_DATA - grib input data files use special directIO routines
!   OUTPUT FILES:
!     unit 20 DATA.ETA - ARL packed data output file
!     unit 30 CFG_ETA - temp file area
!     unit 40 ETATIME - time indicator file
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE:  IBM/RS6000
!
!$$$

      PROGRAM ETA4ARL


      LOGICAL   ftest
      CHARACTER FNAME*80
      INTEGER*4 handle, fcopen

!     check for command line arguments
      narg=IARGC()
      if(narg.eq.0)then
         write(*,*)'Converts 4 km eta model pressure grib file to arl format'
         write(*,*)' '
         write(*,*)'Usage: eta04arl [file_1] [file_2] [...]'
         write(*,*)'Convert pressure 4 km eta grib file to arl format'
         stop
      end if

!     process each data file
      do while (narg.gt.0)
         CALL GETARG(narg,fname)
         INQUIRE(file=fname,exist=ftest)
         if(ftest)then
            HANDLE=FCOPEN(fname,'r')
            write(*,*)'Started processing: ',fname
            call xtract(handle)
            CALL FCCLOS(handle,*900)
  900       continue
         else
            write(*,*)'File not found:',fname
         end if
         narg=narg-1
      end do

!     close out time period and write index record
      call pakndx(20)
      CLOSE (20)
      END


!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM:  EXTRACT          MAIN ROUTINE FOR DECODING ETA GRIB DATA
!   PRGMMR:    GLENN ROLPH      ORG: R/ARL       DATE:02-01-30
!
! ABSTRACT:  THIS CODE WRITTEN AT THE AIR RESOURCES LABORATORY ...
!            EXTRACTS ONE RECORD FROM INPUT GRIB FILE AND ADDS ONE
!            RECORD TO THE OUTPUT FILE
!
! PROGRAM HISTORY LOG:
!   LAST REVISED: 30 Jan 2002 - GDR
!                 25 Feb 2003 (RRD) - ichar function replacement
!
! USAGE:  CALL XTRACT(HANDLE)
!   INPUT ARGUMENT LIST:
!     HANDLE - defines input file to directIO routines
!   OUTPUT ARGUMENT LIST:
!     NONE
!   INPUT FILES:
!     HANDLE defines input data file
!   OUTPUT FILES:
!     NONE
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE:  IBM/RS6000
!
!$$$

      SUBROUTINE xtract(handle)

!     number of levels, variables, and array limits
!       /com/eta/para/eta.YYYYMMDD/eta.tXXz.AWPMSOXX.tm00

!     PARAMETER (NLVL=27, MVAR=14, MAXX=331, MAXY=368, MAXB=350000)
      PARAMETER (NLVL=27, MVAR=14, MAXX=400, MAXY=400, MAXB=350000)
      PARAMETER (MAXC=350000)

!     arrays to hold grib, character, and level variable information
      INTEGER     SIGL(NLVL), NVAR(NLVL), HANDLE
      INTEGER     VGRIB, VGRIB0(MVAR), VGRIB1(MVAR),                           &
                  SIG0(MVAR), STYP(MVAR)
      CHARACTER*4 VCHAR, VCHARP, VCHAR0(MVAR), VCHAR1(MVAR)
      REAL        CNVRT, CNVRT0(MVAR), CNVRT1(MVAR)

!     define arrays for product definition section, grid description,
!     and other grib information
      INTEGER   KPDS(25),KGDS(25),KPTR(25)

!     input data buffer and bit map section
      CHARACTER*1 BUFF(MAXB)
      LOGICAL*1   KBMS(MAXB)
      LOGICAL     FTEST

!     unpacked output array
      REAL   RVAR(MAXX,MAXY),SVAR(MAXX,MAXY),SGVAR(MAXX,MAXY)

!     packed output array
      CHARACTER*1 CVAR(MAXC)
      CHARACTER   FNAME*80

!     default information (grid id, output records)
      DATA IG/99/, KREC/0/

!--------------------------------------------------------------------
!     predefine number of variables surface and aloft
!     first dimension is number of surface variables,
!     second dimension is number of levels above surface * number of upper fields
      DATA NVAR / 14, 26*7 /

!     set output structure in ARL character format
      DATA VCHAR0/ 'SHGT','MSLP','TPP1','CPP1','T02M',           &
                   'RH2M','U10M','V10M','PRSS','LHTF',           &
                   'SHTF','USTR','RGHS','DSWF'/
      DATA VCHAR1/ 'UWND','VWND','HGTS','TEMP','WWND','RELH','TKEN',           &
                    7*'    '/

!     set output structure by defining GRIB variables (sfc + level 1)
      DATA VGRIB0/ 7,  2, 61, 63, 11,                            &
                  52, 33, 34,  1, 121,                           &
                  122,253,83, 204/
      DATA VGRIB1/33, 34,  7, 11, 39, 52, 158, 7*0/

!     special surface variables level identification
      DATA SIG0/  0,   0,   0,   0,   2,                         &
                  2,  10,  10,   0,   0,                         &
                  0,   0,   0,   0/

      DATA STYP/  1,   102,   1,   1, 105,                       &
                  105, 105, 105, 1,  1,                          &
                  1,   1,   1,   1/

!     units conversion factors before converting to ARL format
      DATA CNVRT0/  1.0, 0.01, 0.001,0.001, 1.0,                 &
                    1.0,   1.0,  1.0, 0.01, 1.0,                 &
                    1.0,  1.0,  1.0, 1.0/
      DATA CNVRT1/  1.0,   1.0,  1.0,  1.0, 0.01,  1.0, 1.0, 7*0.0/

!     set requested pressure levels for input and output
      DATA SIGL/   0,1000,975,950,925,900,875,850,825,800,775,750,725,         &
                700,650,600,550,500,450,400,350,300,250,200,150,100,50/

      SAVE krec

!-------------------------------------------------------------------------------
! When dealing with some F90 compilers, replace ICHAR below with JCHAR function
  CHARACTER(1)        :: mychr
  INTEGER             :: jchar
  JCHAR(MYCHR)=IAND(ICHAR(MYCHR),255)
!-------------------------------------------------------------------------------

!     input grib record byte counter
      kbyte=0

!     read the indicator section
  100 call fcptps(handle,kbyte,*900)
      call fcread(handle,buff,1,8,*900)

!     determine the length of the entire grib record
      klen=jchar(buff(7))+jchar(buff(6))*256+jchar(buff(5))*65536
      if(klen.gt.maxb)then
         write(*,*)'Grib record: ',klen
         write(*,*)'Exceedes buffer: ',maxb
         stop
      end if

!     load the indicator section and pds segment into the buffer
      call fcptps(handle,kbyte,*900)
      call fcread(handle,buff,1,36,*900)

!     product definition section (+8 byte indicator section offset)
      koff=8
      kvarb=jchar(buff(koff+9))
      ktype=jchar(buff(koff+10))
      level=jchar(buff(koff+12))+jchar(buff(koff+11))*256
!     check the hours at octet 19 and 20 for precipitation summing
      ktim1=jchar(buff(koff+19))
      ktim2=jchar(buff(koff+20))
!     check the octect 21 to remove all averaged fields
      ktim3=jchar(buff(koff+21))

!     check if 2d variable present in selection table
      kl=1
      do kv=1,nvar(kl)
         vgrib=vgrib0(kv)
         vchar=vchar0(kv)
         cnvrt=cnvrt0(kv)
!        matches id and special level indicator
         IF(KVARB.EQ.VGRIB.AND.LEVEL.EQ.SIG0(KV).AND.KTYPE.EQ.STYP(KV))        &
         THEN
!          Test for averaging period (want no averaging)
!          Precipitation (61,63) is totaled for 1 hour and has ktim3 ne 0, so don't check it
            IF(KTIM3.EQ.0.OR.VGRIB.EQ.61.OR.VGRIB.EQ.63)GO TO 300
         END IF
      end do

!     then check for 3d variable
      kl=2
      do kv=1,nvar(kl)
         vgrib=vgrib1(kv)
         vchar=vchar1(kv)
         cnvrt=cnvrt1(kv)
         if(kvarb.eq.vgrib)go to 200
      end do
      go to 800

!     check if 3d level is present in selection table
  200 do kl=2,nlvl
         if(level.eq.sigl(kl))go to 300
      end do

!     if all tests fail go and read next grib record
      go to 800

!     load the entire grib data record into the buffer
  300 krec=krec+1
      call fcptps(handle,kbyte,*900)
      call fcread(handle,buff,1,klen,*900)

!     call the nmc grib unpacker
      call W3FI63(buff,kpds,kgds,kbms,rvar,kptr,kret)

      if(kret.ne.0)then
         write(*,*)'Error W3FI63: ',kret
         stop
      end if

!     correct for Y2K
      kpds(8)=mod(kpds(8),100)

!     set the current time
      iyr=kpds(8)
      imo=kpds(9)
      ida=kpds(10)
      ihr=kpds(11)
      imn=kpds(12)
      ifh=max(kpds(14),kpds(15))
      call tmplus(iyr,imo,ida,ihr,ifh)

!     for the the first record create an index record for pakrec
      if(krec.eq.1)then

!        the number of grid points in original grid
         nxp=kgds(2)
         nyp=kgds(3)
         nxy=nxp*nyp

!        decode grib information and create index record structure
         call makndx(kgds,kbms,maxb,svar,nxp,nyp,vchar0,vchar1,mvar,         &
                     nlvl,nvar,sigl,ig,newgrd,i1,j1,i2,j2,nsxp,nsyp)

         if(newgrd.eq.0)then
            if(nxp.gt.maxx.or.nyp.gt.maxy)then
               write(*,*)'Real array size: ',nxp,nyp
               write(*,*)'Exceedes dimensions: ',maxx,maxy
               stop
            end if

            nxy=nxp*nyp
            if(nxy.gt.maxc)then
               write(*,*)'Packed output array: ',nxy
               write(*,*)'Exceeds dimensions: ',maxc
               stop
            end if
            call pakset(20,'CFG_ETA',1,nxp,nyp,nzp)
         else
            if(nsxp.gt.maxx.or.nsyp.gt.maxy)then
               write(*,*)'Real array size: ',nsxp,nsyp
               write(*,*)'Exceedes dimensions: ',maxx,maxy
               stop
            end if

            nxy=nsxp*nsyp
            if(nxy.gt.maxc)then
               write(*,*)'Packed output array: ',nxy
               write(*,*)'Exceeds dimensions: ',maxc
               stop
            end if
            call pakset(20,'CFG_ETA',1,nsxp,nsyp,nzp)
         end if

!        write control file with date
         OPEN(40,FILE='ETATIME')
         WRITE(40,'(5I2)')IYR,IMO,IDA,IHR,IFH
         CLOSE (40)

!        standard file name for output
         FNAME='DATA.ETA'
         INQUIRE(FILE=FNAME,EXIST=FTEST)

!        open data file
         LREC=NXY+50
         OPEN(20,FILE=FNAME,RECL=LREC,                                         &
            ACCESS='DIRECT',FORM='UNFORMATTED')

         IF(.NOT.FTEST)THEN
!           output data set initialized to missing
            KINI=1
         ELSE
!           output data set created previously, fill in data          
            KINI=0
         END IF
         write(*,*)'Initialized output data set: ',fname
      end if

!     create new subgrid array and perform any required units conversion
      if(newgrd.eq.1)then
         call subgrd(rvar,nxp,nyp,i1,j1,i2,j2,sgvar,nsxp,nsyp,cnvrt)

!        then pack into ARL format and continue
         call pakrec(20,sgvar,cvar,nsxp,nsyp,nxy,vchar,                              &
                     iyr,imo,ida,ihr,imn,ifh,kl,kini)
      else

!        perform any required units conversion
         if(cnvrt.ne.1.0) call datcnv(rvar,nxp,nyp,cnvrt)

!        then pack into ARL format and continue
         call pakrec(20,rvar,cvar,nxp,nyp,nxy,vchar,                              &
                     iyr,imo,ida,ihr,imn,ifh,kl,kini)
      end if


  800 kbyte=kbyte+klen
      GO TO 100

  900 RETURN
      END



!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM:  MAKNDX           CREATE THE INDEX RECORD CONFIGURATION FILE
!   PRGMMR:    GLENN ROLPH      ORG: R/ARL      DATE:01-11-15
!
! ABSTRACT:  THIS CODE WRITTEN AT THE AIR RESOURCES LABORATORY ...
!            CREATES THE CONFIGURATION FILE FOR THE OUTPUT DATA WHICH
!            DEFINES THE GRID SYSTEM AS WELL AS ALL VARIABLES THAT ARE
!            TO BE WRITTEN TO THE OUTPUT FILE.
!
! PROGRAM HISTORY LOG:
!   LAST REVISED: 22 Feb 2002 - GDR added KBMS and MAXB to argument list
!
! USAGE:  CALL MAKNDX(KGDS,KBMS,MAXB,SVAR,NXP,NYP,VCHAR0,VCHAR1,MVAR,
!              NLVL,NVAR,SIGL,IG,NEWGRD,I1,J1,I2,J2,NSXP,NSYP)
!   INPUT ARGUMENT LIST:
!     KGDS - grid definitions from w3lib decoder
!     KBMS - logical Bitmap array
!     MAXB - maximum dimensions of bitmap array
!     SVAR - bitmap 2-dimensional array
!     NXP,NYP - SVAR bitmap dimensions
!     VCHAR0 - character array of surface field identification
!     VCHAR1 - character array of upper level field identifications
!     NLVL - number of data levels in output file
!     MVAR - maximum dimension for number of variables
!     SIGL - height of each output level
!     IG - grid identification number
!   OUTPUT ARGUMENT LIST:
!     NEWGRD - subgrid defined (yes=1, no=0)
!     I1,J1,I2,J2 - subgrid dimension limits from original grid
!     NSXP,NSYP - subgrid dimensions
!   INPUT FILES:
!     NONE
!   OUTPUT FILES:
!     UNIT 30 - CFG_ETA defines the output file grid and structure
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE:  IBM/RS6000
!
!$$$

      SUBROUTINE MAKNDX(kgds,kbms,maxb,svar,nxp,nyp,vchar0,vchar1,mvar,      &
                        nlvl,nvar,sigl,ig,newgrd,i1,j1,i2,j2,nsxp,nsyp)

      LOGICAL FTEST
      INTEGER   kgds(25)
      REAL   grids(12)
      REAL  PARMAP(9), SUBGRD(9)

!     bitmap array
      REAL   SVAR(NXP,NYP)

!     input data bit map section
      LOGICAL*1 KBMS(MAXB)

!     arrays to hold variable selection information
      INTEGER   sigl(nlvl), nvar(nlvl)
      CHARACTER*4 VCHAR0(MVAR), VCHAR1(MVAR)

!     DO II=1,13
!       WRITE(6,*)II,KGDS(II)
!     END DO
!     STOP

!     determine missing grid points from the bitmap
      ii=0
      do j=1,nyp
      do i=1,nxp
         ii=ii+1
         if(kbms(ii))then
           svar(i,j)=1.0
         else
           svar(i,j)=0.0
         end if
      end do
      end do

!     find center point 
      ictr=(nxp+1)/2
      jctr=(nyp+1)/2
      icnt=0 
      j1=1
      
   50 icnt=icnt+1
      i1=ictr-icnt
      i2=ictr+icnt
      if(i1.eq.0.or.i2.gt.nxp)then
!         write(6,*)'no missing I values found'
          i1=1
          i2=nxp
          go to 100
      end if
      do ii=i1,i2
        jj=jctr-icnt
        if(jj.le.1)jj=1
        if(svar(ii,jj).eq.0.0)go to 100
        jj=jctr+icnt
        if(jj.gt.nyp)jj=nyp
        if(svar(ii,jj).eq.0.0)go to 100
      end do
      go to 50

  100 icnt=0 
  150 icnt=icnt+1
      j1=jctr-icnt
      j2=jctr+icnt
      if(j1.eq.0.or.j2.gt.nyp)then
!         write(6,*)'no missing J values found'
          j1=1
          j2=nyp
          go to 250
      end if
      do jj=j1,j2
        ii=ictr-icnt
!       check to see if ii is less than the just defined left edge (i1) 
        if(ii.le.i1)ii=i1+1
        if(svar(ii,jj).eq.0.0)go to 200
        ii=ictr+icnt
!       check to see if ii is greater than the just defined right edge (i2) 
        if(ii.ge.i2)ii=i2-1
        if(svar(ii,jj).eq.0.0)go to 200
      end do
      go to 150

  200 icnt=icnt-1
      i1=i1+1
      j1=j1+1
      i2=i2-1
      j2=j2-1

  250 if(i1.eq.1.and.j1.eq.1.and.i2.eq.nxp.and.j2.eq.nyp)then
         write(6,*)'Original grid:   i1,j1,i2,j2 ',1,1,nxp,nyp
         write(6,*)'No grid reduction taken'
         newgrd=0
      else
         write(6,*)'Original grid:   i1,j1,i2,j2 ',1,1,nxp,nyp
         write(6,*)'New Grid subset: i1,j1,i2,j2 ',i1,j1,i2,j2
         newgrd=1
      end if
      
!     Original Grid Information
!     grid orientation
      grids(6)=0.0

!     sync x,y defines lower left grid point
      grids(8)=1.0
      grids(9)=1.0
!     lat/lon of lower left point 
      grids(10)=kgds(4)/1000.0
      grids(11)=kgds(5)/1000.0
!     variable reserved for future use
      grids(12)=0.0

      if(kgds(1).eq.3)then
!     defines a lambert conformal projection

!        set the pole position and reference lat/lon
         grids(1)=90.0
         grids(2)=0.0

!        reference latitude
         grids(3)=kgds(12)/1000.0

!        reference longitude = grid orientation
         grids(4)=kgds(7)/1000.0

!        delta=x grid size in km
         grids(5)=kgds(8)/1000.0

!        tangent latitude
         grids(7)=kgds(12)/1000.0

      elseif(kgds(1).eq.5)then
!     defines a northern hemispheric polar stereographic projection

!        set the pole position and reference lat/lon
         grids(1)=90.0
         grids(2)=0.0

!        reference latitude
         grids(3)=60.0
 
!        reference longitude = grid orientation
         grids(4)=kgds(7)/1000.0

!        delta=x grid size in km
         grids(5)=kgds(8)/1000.0

!        tangent latitude
         grids(7)=grids(1)

      else
         write(*,*)'Undefined projection: ',kgds(1)
         stop
      end if

      if(newgrd.eq.1)then
!       initialize original grid
        CALL STLMBR(PARMAP,GRIDS(7),GRIDS(4))

!       use single point grid definition
        CALL STCM1P(PARMAP,GRIDS(8),GRIDS(9),GRIDS(10),GRIDS(11),               &
                  GRIDS(3),GRIDS(4),GRIDS(5),GRIDS(2))

!       number of grid points of new grid
        nsxp=i2-i1+1
        nsyp=j2-j1+1
        call cxy2ll(PARMAP,float(i1),float(j1),xslat1,xslon1)
        call cxy2ll(PARMAP,float(i2),float(j2),xslat2,xslon2)
        grids(10)=xslat1
        grids(11)=xslon1

      end if

!     write the packer configuration file
      OPEN(30,file='CFG_ETA')
      WRITE(30,'(20X,A4)')'ETA4'

!     grid number 99 and 2 for pressure coordinate system
      WRITE(30,'(20X,I4)') IG, 2

      WRITE(30,'(20X,F10.4)')(GRIDS(I),I=1,12)
      if(newgrd.eq.1)then
        WRITE(30,'(20X,I4)')nsxp,nsyp,nlvl
      else 
        WRITE(30,'(20X,I4)')nxp,nyp,nlvl
      end if

!     upper level information
      DO NL=1,NLVL
         sigma=sigl(nl)
         if(nl.eq.1)then
            WRITE(30,'(20X,F6.1,I3,32(1X,A4))')                                &
            sigma,nvar(nl),(vchar0(nv),nv=1,nvar(nl))
         else
            WRITE(30,'(20X,F6.1,I3,32(1X,A4))')                                &
            sigma,nvar(nl),(vchar1(nv),nv=1,nvar(nl))
         end if
      END DO
      CLOSE (30)

      RETURN
      END


!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM:  DATCNV           CONVERT UNITS FOR ALL ELEMENTS OF THE DATA
!   PRGMMR:    GLENN ROLPH      ORG: R/ARL       DATE:01-11-15
!
! ABSTRACT:  THIS CODE WRITTEN AT THE AIR RESOURCES LABORATORY ...
!            IS USED TO CONVERT EACH ELEMENT OF THE OUTPUT DATA GRID
!            TO CONFORM WITH UNIT CONVENTIONS USED IN OTHER APPLICATIONS
!
! PROGRAM HISTORY LOG:
!   LAST REVISED: 15 Nov 2001 - GDR
!
! USAGE:  CALL DATCNV(RVAR,NXP,NYP,CNVRT)
!   INPUT ARGUMENT LIST:
!     RVAR - real data array
!     NXP,NYP - dimensions of the array
!     CNVRT - conversion factor
!   OUTPUT ARGUMENT LIST:
!     RVAR - real data array after conversion
!   INPUT FILES:
!     NONE
!   OUTPUT FILES:
!     NONE
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE:  IBM/RS6000
!
!$$$
      SUBROUTINE datcnv(rvar,nxp,nyp,cnvrt)

      REAL   RVAR(NXP,NYP)

      DO J=1,NYP
      DO I=1,NXP

         rvar(i,j)=rvar(i,j)*cnvrt

      END DO
      END DO
      RETURN
      END

!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM:  SUBGRD           FILL SUB GRID ARRAY
!   PRGMMR:    GLENN ROLPH      ORG: R/ARL       DATE:02-03-04
!
! ABSTRACT:  THIS CODE WRITTEN AT THE AIR RESOURCES LABORATORY ...
!            IS USED TO CREATE A SUBGRID ARRAY FROM THE ORIGINAL ARRAY
!            AND TO CONVERT EACH ELEMENT OF THE OUTPUT DATA GRID
!            TO CONFORM WITH UNIT CONVENTIONS USED IN OTHER APPLICATIONS
!
! PROGRAM HISTORY LOG:
!   LAST REVISED: 04 Mar 2002 - GDR
!
! USAGE:  CALL SUBGRD(RVAR,NXP,NYP,I1,J1,I2,J2,SGVAR,NSXP,NSYP,CNVRT)
!   INPUT ARGUMENT LIST:
!     RVAR - real original data array
!     NXP,NYP - dimensions of the original array
!     I1,J1,I2,J2 - GRID LIMITS OF SUBGRID FROM ORIGINAL GRID
!     NSXP,NSYP - dimensions of the subgrid array
!     CNVRT - conversion factor
!   OUTPUT ARGUMENT LIST:
!     SGVAR - real subgrid output data array after conversion
!   INPUT FILES:
!     NONE
!   OUTPUT FILES:
!     NONE
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE:  IBM/RS6000
!
!$$$
      SUBROUTINE subgrd(rvar,nxp,nyp,i1,j1,i2,j2,sgvar,nsxp,nsyp,cnvrt)

      REAL   RVAR(NXP,NYP),SGVAR(NSXP,NSYP)

      jj=0
      DO J=J1,J2
      jj=jj+1
      ii=0
        DO I=I1,I2
           ii=ii+1
           sgvar(ii,jj)=rvar(i,j)*cnvrt
        END DO
      END DO
      RETURN
      END

