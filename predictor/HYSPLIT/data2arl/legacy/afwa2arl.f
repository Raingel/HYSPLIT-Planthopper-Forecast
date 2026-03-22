!$$$  PROGRAM DOCUMENTATION BLOCK
!
! PROGRAM:     AFWA2ARL         DECODE AFWA GRIB DATA MODEL FOR HYSPLIT
!   PRGMMR:    GLENN ROLPH      ORG: R/ARL       DATE:99-01-29
!
! ABSTRACT:  THIS CODE WRITTEN AT THE AIR RESOURCES LABORATORY ...
!     converts an eta model pressure grib file to arl packed format
!     processes only one time period per execution.  Multiple input files
!     may be specified on the command line, each containing different
!     variables required for the specific time period.  Input data are
!     assumed to be already on a conformal map projection on model levels.
!     Only packing to ARL format and units conversion is required.
!
! PROGRAM HISTORY LOG:
!   LAST REVISED: 30 Mar 2000 (GDR) - changed to 3hr precip
!                 29 Jan 1999 (GDR)
!                 11 Oct 2001 (RRD) - conversion to f90
!                 18 Oct 2001 (RRD) - large grid domains
!                 28 Oct 2001 (RRD) - modified for AFWA fields
!                 05 Mar 2002 (RRD) - new library consistency
!                 09 Apr 2002 (RRD) - grib test
!                 06 Jul 2010 (RRD) - support polar sterographic
!                 24 Aug 2010 (RRD) - revised Lambert and added Mercator
!
! USAGE: AFWA2ARL [GRIB_FILE_NAME] [GRIB_FILE_NAME2] [...]
!   INPUT ARGUMENT LIST:
!     GRIB_FILE_NAME - multiple grib files may be defined
!   OUTPUT ARGUMENT LIST:
!     NONE
!   INPUT FILES:
!     GRIB_DATA - grib input data files use special directIO routines
!   OUTPUT FILES:
!     unit 20 DATA.ARL - ARL packed data output file
!     unit 30 CFG_ARL - temp file area
!     unit 40 ARLTIME - time indicator file
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE:  IBM/RS6000
!
!$$$

      PROGRAM AFWA2ARL

      LOGICAL   ftest,pdata  
      CHARACTER fname*80
      INTEGER*4 handle, fcopen

!     check for command line arguments
      narg=IARGC()
      if(narg.eq.0)then
         write(*,*)'Converts an AFWA model sigma or pressure grib file to arl packed format.'
         write(*,*)'Processes only one time period per execution. Multiple input files'
         write(*,*)'may be specified on the command line, each containing different'
         write(*,*)'variables required for the specific time period.  Input data are'
         write(*,*)'assumed to be already on a conformal map projection on model levels.'
         write(*,*)'Only packing to ARL format and units conversion is required.'
         write(*,*)'The default is to extract the sigma level data records only, except'
         write(*,*)'when the -p flag set to extract the pressure level records.'
         write(*,*)' '
         write(*,*)'Usage: afwa2arl [-p] [file_1] [file_2] [...]'
         stop
      end if

!     check for the pressure level data flag
      pdata=.FALSE.
      do while (narg.gt.0)
         CALL GETARG(narg,fname)
         IF(fname(1:2).EQ.'-p'.OR.fname(1:2).EQ.'-P') pdata=.TRUE.          
         narg=narg-1
      end do

!     process each data file
      narg=IARGC()
      do while (narg.gt.0)
         CALL GETARG(narg,fname)
         IF(fname(1:2).EQ.'-p'.OR.fname(1:2).EQ.'-P') go to 990

         INQUIRE(file=fname,exist=ftest)
         if(ftest)then
            HANDLE=FCOPEN(fname,'r')
            write(*,*)'Started processing: ',fname
            call xtract(handle,pdata)
            CALL FCCLOS(handle,*900)
  900       continue
         else
            write(*,*)'File not found:',fname
         end if
  990    narg=narg-1
      end do

!     close out time period and write index record
      call pakndx(20)
      CLOSE (20)
      END


!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM:  EXTRACT          MAIN ROUTINE FOR DECODING GRIB DATA
!   PRGMMR:    GLENN ROLPH      ORG: R/E/AR      DATE:99-01-29
!
! ABSTRACT:  THIS CODE WRITTEN AT THE AIR RESOURCES LABORATORY ...
!            EXTRACTS ONE RECORD FROM INPUT GRIB FILE AND ADDS ONE
!            RECORD TO THE OUTPUT FILE
!
! PROGRAM HISTORY LOG:
!   LAST REVISED: 29 Jan 1999 (GDR)
!                 25 Feb 2003 (RRD) - ichar function replacement
!
! USAGE:  CALL XTRACT(HANDLE,PDATA)
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

      SUBROUTINE xtract(handle,pdata)

!     number of levels, variables, and array limits
      PARAMETER (MLEV=60, MVAR=6, MAXX=200, MAXY=150, MAXB=90000)
      PARAMETER (MAXC=90000)

!     data array with upper level height information (press and sigma)
      INTEGER     PREL(26), SIGL(52), WVEL(52)

!     arrays to hold grib, character, and level variable information
      INTEGER     ZLEV(MLEV), NVAR(MLEV), HANDLE
      INTEGER     VGRIB, VGRIB0(MVAR), VGRIB1(MVAR),                           &
                  SIG0(MVAR), STYP(MVAR)
      CHARACTER*4 VCHAR, VCHARP, VCHAR0(MVAR), VCHAR1(MVAR)
      REAL        CNVRT, CNVRT0(MVAR), CNVRT1(MVAR)

!     define arrays for product definition section, grid description,
!     and other grib information
      INTEGER   KPDS(25),KGDS(25),KPTR(25)

!     input data buffer and bit map section
      CHARACTER BUFF(MAXB)*1
      LOGICAL*1 KBMS(MAXB)
      LOGICAL   FTEST,PDATA

!     unpacked output array
      REAL   RVAR(MAXX,MAXY),PVAR(MAXX,MAXY)

!     packed output array
      CHARACTER CVAR(MAXC)*1, FNAME*80

!     array remapping
      REAL SVAR(MAXX*MAXY)
      INTEGER SHAPE(2)
      DATA SHAPE/MAXX,MAXY/ 

!     default information (grid id, output records,
      DATA IG/18/, KREC/0/

!--------------------------------------------------------------------
!     predefine number of variables surface and aloft
      DATA NVAR / 6, 59*6 /

!     special surface variables level identification
      DATA SIG0/  0,   2,   2,  10,  10,  0/
      DATA STYP/  1, 105, 105, 105, 105,  1/ 

!     set output structure by defining GRIB variables (sfc + level 1)
      DATA VGRIB0/ 61, 11, 52, 33, 34,  1/
      DATA VGRIB1/ 33, 34,  7, 11, 39, 52/

!     set output structure in ARL character format
      DATA VCHAR0/ 'TPP3','T02M','RH2M','U10M','V10M','PRSS'/
      DATA VCHAR1/ 'UWND','VWND','HGTS','TEMP','WWND','RELH'/

!     set requested pressure levels for input and output
      DATA PREL/ 1000,975,950,925,900,875,850,800,750,700,650,600,    &
                  550,500,450,400,350,300,250,200,150,100,75,50,25,10 /

!     set requested sigma levels for input and output
      DATA SIGL/  9985,9945,9885,9815,9735,9645,9550,9440,9315,9175,9020, & 
                  8850,8665,8460,8235,7995,7735,7455,7155,6840,6515,6175, & 
                  5825,5475,5120,4760,4405,4055,3715,3390,3080,2785,2505, &
                  2245,2000,1775,1570,1385,1220,1065, 925, 800, 690, 590, &
                    500, 425, 355, 295, 245, 195, 150, 115 /

!     sigma level interface associated with vertical velocity fields
      DATA WVEL/  9970,9920,9850,9780,9690,9600,9500,9380,9250,9100,8940, &  
                  8760,8570,8350,8120,7870,7600,7310,7000,6680,6350,6000, &  
                  5650,5300,4940,4580,4230,3880,3550,3230,2930,2640,2370, &  
                  2120,1880,1670,1470,1300,1140, 990, 860, 740, 640, 540, &
                   460, 390, 320, 270, 220, 170, 130, 100 / 

!     units conversion factors before converting to ARL format
      DATA CNVRT0/0.001, 1.0, 1.0, 1.0,  1.0, 0.01/
      DATA CNVRT1/  1.0, 1.0, 1.0, 1.0, 0.01,  1.0/
      SAVE krec

!-------------------------------------------------------------------------------
! When dealing with some F90 compilers, replace ICHAR below with JCHAR function
  CHARACTER(1)        :: mychr
  INTEGER             :: jchar
  JCHAR(MYCHR)=IAND(ICHAR(MYCHR),255)
!-------------------------------------------------------------------------------

  INTERFACE
  SUBROUTINE W3FI63(BUFF,KPDS,KGDS,KBMS,RVAR,KPTR,KRET)
  CHARACTER(1), INTENT(IN)  :: BUFF(:)
  LOGICAL(1),   INTENT(OUT) :: KBMS(:)
  INTEGER,      INTENT(OUT) :: KPDS(:)
  INTEGER,      INTENT(OUT) :: KGDS(:)
  REAL,         INTENT(OUT) :: RVAR(:)
  INTEGER,      INTENT(OUT) :: KPTR(:)
  INTEGER,      INTENT(OUT) :: KRET
  END SUBROUTINE w3fi63
  END INTERFACE

!--------------------------------------------------------------------

      IF(pdata)THEN
!        pressure level data
         nlvl=26
         DO kl=1,nlvl
            zlev(kl+1)=prel(kl) 
         END DO
         zlev(1)=0.0
      ELSE
!        sigma level data
         nlvl=52
         DO kl=1,nlvl
            zlev(kl+1)=sigl(kl)
         END DO
         zlev(1)=1.0

!        replace with geometric vertical velocity
         VCHAR1(5)='DZDT'
         VGRIB1(5)=40         
         CNVRT1(5)=1.0
      END IF
      nlvl=nlvl+1

!     input grib record byte counter
      kbyte=0

!     read the indicator section
  100 call fcptps(handle,kbyte,*900)
      call fcread(handle,buff,1,8,*900)
      IF(BUFF(1)//BUFF(2)//BUFF(3)//BUFF(4).NE.'GRIB')THEN
         KBYTE=KBYTE+1
         GOTO 100
      END IF

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
         IF(KVARB.EQ.VGRIB.AND.LEVEL.EQ.SIG0(KV).AND.KTYPE.EQ.STYP(KV)) THEN 

!           precipitation (61) is accumulated for 3 hours
            IF(VGRIB.EQ.61)THEN
               KTIMD=KTIM2-KTIM1
               IF(KTIMD.EQ.3.OR.KTIMD.EQ.0)GO TO 300
               WRITE(*,*)'Precip unexpected time interval:',ktim1,ktim2,ktim3

!           all remaining surface variables
            ELSE
               GO TO 300
            END IF
         END IF
      end do

!     check for pressure or sigma level
      IF(pdata)THEN
!        require pressure but found sigma level then skip
         IF(ktype.EQ.107) go to 800
      ELSE
!        require sigma but found pressure level then skip
         IF(ktype.EQ.100) go to 800
      END IF

!     then check for 3d variable match
      kl=2
      do kv=1,nvar(kl)
         vgrib=vgrib1(kv)
         vchar=vchar1(kv)
         cnvrt=cnvrt1(kv)
         if(kvarb.eq.vgrib)go to 200
      end do
      go to 800

!     check if 3d level found is present in selection table
  200 do kl=2,nlvl
         if(level.eq.zlev(kl))go to 300
!        special test for geometric vertical velocity
         if(vgrib.eq.40.and.level.eq.wvel(kl-1))go to 300
      end do

!     if all tests fail go and read next grib record
      go to 800

!     load the entire grib data record into the buffer
  300 krec=krec+1
      call fcptps(handle,kbyte,*900)
      call fcread(handle,buff,1,klen,*900)

!     call the nmc grib unpacker
      call W3FI63(buff,kpds,kgds,kbms,svar,kptr,kret)
!     remap input data from one- to two-dimensional array
      RVAR=RESHAPE(SVAR,SHAPE)

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
      IF(krec.eq.1)then

!        decode grib information and create index record structure
         call makndx(pdata,kgds,vchar0,vchar1,mvar,nlvl,nvar,zlev,ig,nxp,nyp)

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
         call pakset(20,'CFG_ARL',1,nxp,nyp,nzp)

!        write control file with date
         OPEN(40,FILE='ARLTIME')
         WRITE(40,'(5I2)')IYR,IMO,IDA,IHR,IFH
         CLOSE (40)

!        standard file name for output
         FNAME='DATA.ARL'
         INQUIRE(FILE=FNAME,EXIST=FTEST)

!        open data file
         LREC=NXY+50
         OPEN(20,FILE=FNAME,RECL=LREC,ACCESS='DIRECT',FORM='UNFORMATTED')

         IF(.NOT.FTEST)THEN
!           output data set initialized to missing
            KINI=1
         ELSE
!           output data set created previously, fill in data          
            KINI=0
         END IF
         write(*,*)'Initialized output data set: ',fname
      END IF

!     perform any required units conversion
      if(cnvrt.ne.1.0) call datcnv(rvar,nxp,nyp,cnvrt)

!     then pack into ARL format and continue
!###  WRITE(*,*)'Record: ',krec,kv,kl,ktype,vchar,zlev(kl),rvar(1,1),rvar(nxp,nyp)
      call pakrec(20,rvar,cvar,nxp,nyp,nxy,vchar,IYR,IMO,IDA,IHR,IMN,IFH,KL,KINI)

  800 kbyte=kbyte+klen
      GO TO 100

  900 RETURN
      END



!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM:  MAKNDX           CREATE THE INDEX RECORD CONFIGURATION FILE
!   PRGMMR:    GLENN ROLPH      ORG: R/E/AR      DATE:99-01-29
!
! ABSTRACT:  THIS CODE WRITTEN AT THE AIR RESOURCES LABORATORY ...
!            CREATES THE CONFIGURATION FILE FOR THE OUTPUT DATA WHICH
!            DEFINES THE GRID SYSTEM AS WELL AS ALL VARIABLES THAT ARE
!            TO BE WRITTEN TO THE OUTPUT FILE.
!
! PROGRAM HISTORY LOG:
!   LAST REVISED: 29 Jan 1999 - GDR
!
! USAGE:  CALL MAKNDX(PDATA,KGDS,VCHAR0,VCHAR1,MVAR,NLVL,NVAR,
!              SIGL,IG,NXP,NYP)
!   INPUT ARGUMENT LIST:
!     PDATA - logical flag for pressure level data
!     KGDS - grid definitions from w3lib decoder
!     VCHAR0 - character array of surface field identification
!     VCHAR1 - character array of upper level field identifications
!     MVAR - maximum dimension for number of variables
!     NLVL - number of data levels in output file
!     SIGL - height of each output level
!     IG - grid identification number
!   OUTPUT ARGUMENT LIST:
!     NXP,NYP - output grid dimensions
!   INPUT FILES:
!     NONE
!   OUTPUT FILES:
!     UNIT 30 - CFG_ARL defines the output file grid and structure
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE:  CRAY
!
!$$$

      SUBROUTINE MAKNDX(pdata,kgds,vchar0,vchar1,mvar,nlvl,nvar,zlev,ig,nxp,nyp)

      LOGICAL FTEST,PDATA
      INTEGER kgds(25)
      REAL    grids(12),parmap(9)

!     arrays to hold variable selection information
      INTEGER     zlev(nlvl), nvar(nlvl)
      CHARACTER*4 VCHAR0(MVAR), VCHAR1(MVAR)

!     the number of grid points
      nxp=kgds(2)
      nyp=kgds(3)

!     grid orientation at ref point with respect to north
!     almost always set to zero
      grids(6)=0.0

!     lat/lon of grid point 1,1
      grids(8)=1.0
      grids(9)=1.0
      grids(10)=kgds(4)/1000.0
      grids(11)=kgds(5)/1000.0
      grids(12)=0.0

!     defines a lambert conformal projection
      if(kgds(1).eq.3)then

!        set the pole position (lat/lon)
         grids(1)=EQVLAT(kgds(12)/1000.0,kgds(13)/1000.0)
         grids(2)=kgds(7)/1000.0

!        reference latitude and longitude (where grid spacing defined)
         grids(3)=kgds(12)/1000.0
         grids(4)=kgds(7)/1000.0

!        delta=x grid size (km)
         grids(5)=kgds(8)/1000.0

!        tangent latitude
         grids(7)=grids(1) 
write(*,*) grids

!     defines a polar sterographic projection
      elseif(kgds(1).eq.5)then

!        set the pole latitude
         grids(1)=90.0
         if(kgds(10).EQ.1) grids(1)=-90.0 ! projection center flag
         grids(2)=0.0

!        reference latitude (grid size valid at this lat)
         grids(3)=60.0 
         if(kgds(10).EQ.1) grids(3)=-60.0 ! projection center flag
         grids(4)=kgds(7)/1000.0

!        delta=x grid size
         grids(5)=kgds(8)/1000.0
!        tangent latitude (either north or south pole)
         grids(7)=grids(1) 

!     defines a mercator projection
      elseif(kgds(1).eq.1)then

!        set the pole position lat/lon
         grids(1)=kgds(9)/1000.0
         grids(2)=0.0  

!        reference latitude (grid size valid at this lat)
         grids(3)=kgds(9)/1000.0
         grids(4)=0.0  

!        delta=x grid size
         grids(5)=kgds(12)/1000.0

!        tangent latitude
         grids(7)=grids(1)

!        define as 2-point projection           
         endlat=kgds(7)/1000.0
         endlon=kgds(8)/1000.0
         CALL STLMBR(PARMAP,grids(1),grids(2))
         CALL STCM2P(PARMAP,grids(8),grids(9),grids(10),grids(11),float(nxp),float(nyp),endlat,endlon)  

!        find center lat-lon to convert to 1-point projection
         xc=0.5*(1+nxp)
         yc=0.5*(1+nyp)
         CALL CXY2LL(parmap,xc,yc,clat,clon) 
         grids(2)=clat

      else
         write(*,*)'Undefined projection: ',kgds(1)
         stop
      end if

!     write the packer configuration file
      OPEN(30,file='CFG_ARL')
      WRITE(30,'(20X,A4)')'ARL'

!     grid number 99 and 1 for sigma coordinate system
!     grid number 99 and 2 for pressure coordinate system
      IF(pdata)THEN
         WRITE(30,'(20X,I4)') IG, 2
      ELSE
         WRITE(30,'(20X,I4)') IG, 1
      END IF

      WRITE(30,'(20X,F10.4)')(GRIDS(I),I=1,12)
      WRITE(30,'(20X,I4)')nxp,nyp,nlvl

!     upper level information
      DO NL=1,NLVL
         IF(PDATA)THEN
            sigma=zlev(nl)
         ELSE
            sigma=zlev(nl)/10000.0
         END IF
         IF(nl.EQ.1)THEN
            WRITE(30,'(20X,F6.1,I3,32(1X,A4))') sigma,nvar(nl),(vchar0(nv),nv=1,nvar(nl))
         ELSE
            IF(PDATA)THEN
               WRITE(30,'(20X,F6.1,I3,32(1X,A4))') sigma,nvar(nl),(vchar1(nv),nv=1,nvar(nl))
            ELSE
               WRITE(30,'(20X,F6.4,I3,32(1X,A4))') sigma,nvar(nl),(vchar1(nv),nv=1,nvar(nl))
            END IF
         END IF
      END DO
      CLOSE (30)
      RETURN
      END


!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM:  DATCNV           CONVERT UNITS FOR ALL ELEMENTS OF THE DATA
!   PRGMMR:    GLENN ROLPH      ORG: R/E/AR      DATE:99-01-29
!
! ABSTRACT:  THIS CODE WRITTEN AT THE AIR RESOURCES LABORATORY ...
!            IS USED TO CONVERT EACH ELEMENT OF THE OUTPUT DATA GRID
!            TO CONFORM WITH UNIT CONVENTIONS USED IN OTHER APPLICATIONS
!
! PROGRAM HISTORY LOG:
!   LAST REVISED: 29 Jan 1999 - GDR
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
!   MACHINE:  CRAY
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
