!$$$  PROGRAM DOCUMENTATION BLOCK
!
! PROGRAM:     PNA05            DECODE GRIB MM5 MODEL FIELD FOR HYSPLIT
!   PRGMMR:    GLENN ROLPH      ORG: R/E/AR      DATE:01-01-26
!
! ABSTRACT:  THIS CODE WRITTEN AT THE AIR RESOURCES LABORATORY ...
!     converts an AFWA MM5 model pressure grib file to arl packed format
!     processes only one time period per execution.  Multiple input files
!     may be specified on the command line, each containing different
!     variables required for the specific time period.  Input data are
!     assumed to be already on a conformal map projection on model levels.
!     Only packing to ARL format and units conversion is required.
!
! PROGRAM HISTORY LOG:
!     26 Jan 01 (GDR) - original version
!     10 Sep 02 (RRD) - upgrade to match revised w3lib
!     29 Jan 03 (RRD) - converted to handle 5 km grids
!
! USAGE: PNA05    [GRIB_FILE_NAME] [GRIB_FILE_NAME2] [...]
!   INPUT ARGUMENT LIST:
!     GRIB_FILE_NAME - multiple grib files may be defined
!   OUTPUT ARGUMENT LIST:
!     NONE
!   INPUT FILES:
!     GRIB_DATA - grib input data files use special directIO routines
!   OUTPUT FILES:
!     unit 20 DATA.MM5 - ARL packed data output file
!     unit 30 CFG_MM5 - temp file area
!     unit 40 MM5TIME - time indicator file
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE:  IBM/RS6000
!
!$$$

      PROGRAM PNA05    

      LOGICAL   ftest
      CHARACTER FNAME*80
      INTEGER*4 handle, fcopen

!     check for command line arguments
      narg=IARGC()
      if(narg.eq.0)then
         write(*,*)'Usage: pmm52arl [file_1] [file_2] [...]'
         write(*,*)'Convert 15 km pressure mm5 grib file to arl format'
         write(*,*)'File name: us057g1010t02v${cyc}000${fhr}00.gz'
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
      END PROGRAM pna05


!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM:  EXTRACT          MAIN ROUTINE FOR DECODING MM5 GRIB DATA
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
      PARAMETER (NLVL=25, MVAR=17, MAXX=267, MAXY=243, MAXB=350000)
      PARAMETER (MAXC=200000)

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
      CHARACTER BUFF(MAXB)*1
      LOGICAL*1 KBMS(MAXB)
      LOGICAL FTEST

!     unpacked output array
      REAL   RVAR(MAXX,MAXY),PVAR(MAXX,MAXY)

!     packed output array
      CHARACTER CVAR(MAXC)*1, FNAME*80

!     array remapping
      REAL SVAR(MAXX*MAXY)
      INTEGER SHAPE(2)
      DATA SHAPE/MAXX,MAXY/

!     default information (grid id, output records,
      DATA IG/70/, KREC/0/

!--------------------------------------------------------------------
!     predefine number of variables surface and aloft
      DATA NVAR / 17, 24*6 /

!     special surface variables level identification
      DATA SIG0/  0,   2,    0,   2, 10,  10,   0,                             &
                  0,   0,   0,    0,   2,  0,   0,                             &
                  0,   0,   0/

      DATA STYP/  102, 105,  1, 105, 105, 105,  1,                             &
                    1,   1,   1,  1, 105,  1, 200,                             &
                    1,   1,   1/

!     set output structure by defining GRIB variables (sfc + level 1)
      DATA VGRIB0/ 2,  11,  61,  52, 33,  34,   1,                             &
                 170, 171, 173, 167, 20, 244,  71,                             &
                 233, 137, 122/

      DATA VGRIB1/33, 34,  7, 11, 39, 52, 11*0/

!     set output structure in ARL character format
      DATA VCHAR0/ 'MSLP','T02M','TPP1','RH2M','U10M','V10M','PRSS',           &
                   'CSNO','CICE','CFZR','CRAI','VSBY','PTYP','TCLD',           &
                   'SHGT','PBLH','SHTF'/

      DATA VCHAR1/ 'UWND','VWND','HGTS','TEMP','WWND','RELH',                  &
                    11*'    '/

!     set requested pressure levels for input and output
      DATA SIGL/   0,1025,1000,975,950,925,900,875,850,800,750,                &
                700,650,600,550,500,450,400,350,300,250,200,150,100,50/

!     units conversion factors before converting to ARL format
      DATA CNVRT0/ 0.01,   1.0,  0.001, 1.0,  1.0,  1.0, 0.01,                 &
                    1.0,   1.0,   1.0,  1.0,  1.0,  1.0,  1.0,                 &
                    1.0,   1.0,   1.0/
      DATA CNVRT1/  1.0,   1.0,  1.0,  1.0,  0.01,  1.0, 11*0.0/

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
         IF(KVARB.EQ.VGRIB.AND.LEVEL.EQ.SIG0(KV).AND.STYP(KV).EQ.KTYPE)        &
         THEN
!        Test for averaging period (want no averaging)
!        Precipitation (61,63) is totaled for 1 hour, so don't check it
!           IF(KTIM1.EQ.0.OR.VGRIB.EQ.61.OR.VGRIB.EQ.63)GO TO 300
            IF(VGRIB.NE.61.AND.VGRIB.NE.63)THEN
               IF(KTIM3.EQ.0)GO TO 300
            ELSE
               KTIMD=KTIM2-KTIM1
               write(6,*)'ktimd,ktim1,ktim2',ktimd,ktim1,ktim2
               IF(KTIMD.EQ.1.OR.KTIMD.EQ.0)GO TO 300
            END IF
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
      call W3FI63(buff,kpds,kgds,kbms,svar,kptr,kret)
      if(kret.ne.0)then
         write(*,*)'Error W3FI63: ',kret
         stop
      end if
!     remap input data from one- to two-dimensional array
      RVAR=RESHAPE(SVAR,SHAPE)

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
         call makndx(kgds,vchar0,vchar1,mvar,nlvl,nvar,                        &
                     sigl,ig,nxp,nyp)

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
         call pakset(20,'CFG_MM5',1,nxp,nyp,nzp)

!        write control file with date
         OPEN(40,FILE='MM5TIME')
         WRITE(40,'(5I2)')IYR,IMO,IDA,IHR,IFH
         CLOSE (40)

!        standard file name for output
         FNAME='DATA.MM5'
         INQUIRE(FILE=FNAME,EXIST=FTEST)

!        open data file
         LREC=NXY+50
         OPEN(20,FILE=FNAME,RECL=LREC,                                         &
            ACCESS='DIRECT',FORM='UNFORMATTED')

         IF(.NOT.FTEST)THEN
!           open output data set and initialize to missing
            call datini(20,CVAR,NXY,NLVL,NVAR,IG,IYR,IMO,IDA,IHR,              &
                        VCHAR0,MVAR)
            write(*,*)'Initialized output data set: ',fname
         END IF
      END IF

!     perform any required units conversion
      if(cnvrt.ne.1.0) call datcnv(rvar,nxp,nyp,cnvrt)

!     then pack into ARL format and continue
      write(*,*)'Record: ',krec,kv,kl,ktype,vchar,sigl(kl),IYR,IMO,IDA,        &
                 IHR,IMN,IFH
      call pakrec(20,rvar,cvar,nxp,nyp,nxy,vchar,                              &
        IYR,IMO,IDA,IHR,IMN,IFH,KL,0)


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
! USAGE:  CALL MAKNDX(KGDS,VCHAR0,VCHAR1,MVAR,NLVL,NVAR,
!              SIGL,IG,NXP,NYP)
!   INPUT ARGUMENT LIST:
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
!     UNIT 30 - CFG_MM5 defines the output file grid and structure
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE:  CRAY
!
!$$$

      SUBROUTINE MAKNDX(kgds,vchar0,vchar1,mvar,nlvl,nvar,                     &
                        sigl,ig,nxp,nyp)

      LOGICAL FTEST
      INTEGER   kgds(25)
      REAL   grids(12)

!     arrays to hold variable selection information
      INTEGER   sigl(nlvl), nvar(nlvl)
      CHARACTER*4 VCHAR0(MVAR), VCHAR1(MVAR)

      WRITE(6,*)'VALUES OF KGDS 1 THROUGH 13'
      DO II=1,13
        WRITE(6,*)II,KGDS(II)
      END DO

!     the number of grid points
      nxp=kgds(2)
      nyp=kgds(3)

!     if configuration exists exit
!     INQUIRE(FILE='CFG_MM5',EXIST=FTEST)
!     IF(FTEST)RETURN

!     grid orientation
      grids(6)=0.0

!     lat/lon of grid point 1,1
      grids(8)=1.0
      grids(9)=1.0
      grids(10)=kgds(4)/1000.0
      grids(11)=kgds(5)/1000.0
      grids(12)=0.0

!     defines a lambert conformal projection
      if(kgds(1).eq.3)then

!        set the pole position
         grids(1)=90.0
         grids(2)=0.0

!        reference latitude
         grids(3)=kgds(13)/1000.0
!        reference longitude = grid orientation
         grids(4)=kgds(7)/1000.0

!        delta=x grid size
         grids(5)=kgds(8)/1000.0

!        tangent latitude
         grids(7)=kgds(12)/1000.0
         grids(7)=EQVLAT(grids(3),grids(7))
      else
         write(*,*)'Undefined projection: ',kgds(1)
         stop
      end if

!     write the packer configuration file
      OPEN(30,file='CFG_MM5')
      WRITE(30,'(20X,A4)')'MM5'

!     grid number 99 and 1 for sigma coordinate system
!     grid number 99 and 2 for pressure coordinate system
      WRITE(30,'(20X,I4)') IG, 2

      WRITE(30,'(20X,F10.4)')(GRIDS(I),I=1,12)
      WRITE(30,'(20X,I4)')nxp,nyp,nlvl

!     upper level information
      DO NL=1,NLVL
!        sigma=sigl(nl)/10000.0
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
! SUBPROGRAM:  DATINI           INITIALIZE OUTPUT FILE FOR ONE TIME PERIOD
!   PRGMMR:    GLENN ROLPH      ORG: R/E/AR      DATE:99-01-29
!
! ABSTRACT:  THIS CODE WRITTEN AT THE AIR RESOURCES LABORATORY ...
!            DUMMY ROUTINE TO FILL IN HEADER BYTES ON ALL RECORS OF THE
!            OUTPUT DATA FILE WITH CORRECT TIME BUT NULL DATA FIELDS.
!
! PROGRAM HISTORY LOG:
!   LAST REVISED: 29 Jan 1999 - GDR
!
! USAGE:  CALL DATINI(KUNIT,CVAR,NXY,NLVL,NVAR,IG,IY,IM,ID,IH,VCHAR0,
!                     MVAR)
!   INPUT ARGUMENT LIST:
!     KUNIT - unit number of the output file
!     CVAR - dummy character string to represent the gridded data
!     NXY - the length of the gridded data field in bytes
!     NLVL - number of data levels in output file
!     NVAR - array defining the number of variables on each level
!     IG - grid identification number
!     IY,IM,ID,IH - current date/time
!     VCHAR0 - character array of surface field identification
!     MVAR - maximum dimension for number of variables
!   OUTPUT ARGUMENT LIST:
!     NONE
!   INPUT FILES:
!     NONE
!   OUTPUT FILES:
!     UNIT 20 - DATA.MM5 is the output data file
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE:  CRAY
!
!$$$

      SUBROUTINE DATINI(KUNIT,CVAR,NXY,NLVL,NVAR,IG,IY,IM,ID,IH,               &
                        VCHAR0,MVAR)
      CHARACTER CVAR(NXY)*1, LABEL*50, VCHAR0(MVAR)*4
      INTEGER   NVAR(NLVL)

      IC=-1
      NEXP=0
      PREC=0.0
      VAR1=0.0
      MREC=0

!     initialize packed data array
      DO K=1,NXY
         CVAR(K)=' '
      END DO
      CVAR(NXY)=CHAR(13)

!     header label output format
  100 FORMAT(7I2,A4,I4,2E14.7)

!     index record
      WRITE(LABEL,100)IY,IM,ID,IH,IC,0,IG,'NULL',NEXP,PREC,VAR1
      MREC=MREC+1
      WRITE(KUNIT,REC=MREC)LABEL,CVAR

      DO NL=1,NLVL
      DO NV=1,NVAR(NL)

       IL=NL-1
!      write ID's for surface fields as some are missing
       IF(NL.EQ.1)THEN
         WRITE(LABEL,100)IY,IM,ID,IH,IC,IL,IG,VCHAR0(NV),NEXP,PREC,VAR1
       ELSE
         WRITE(LABEL,100)IY,IM,ID,IH,IC,IL,IG,'NULL',NEXP,PREC,VAR1
       END IF
       MREC=MREC+1
       WRITE(KUNIT,REC=MREC)LABEL,CVAR

      END DO
      END DO

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

