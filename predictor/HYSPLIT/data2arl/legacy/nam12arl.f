!$$$  PROGRAM DOCUMENTATION BLOCK
!
! PROGRAM:     NAM12ARL          DECODE GRIB NAM (218) MODEL FIELD FOR HYSPLIT
!   PRGMMR:    GLENN ROLPH      ORG: REAR       DATE:01-11-15
!
! ABSTRACT:  THIS CODE WRITTEN AT THE AIR RESOURCES LABORATORY ...
!     converts eta model pressure grib file (12 km, 218) to arl packed format.
!     Processes only one time period per execution.  Multiple input files
!     may be specified on the command line, each containing different
!     variables required for the specific time period.  Input data are
!     assumed to be already on a conformal map projection on model levels.
!     Only packing to ARL format and units conversion is required.
!
! PROGRAM HISTORY LOG:
!                 11 Oct 2001 (RRD) - conversion to f90
!                 18 Oct 2001 (RRD) - large grid domains
! LAST REVISED    15 Nov 2001 (GDR) - converted to eta 12 km
!                 05 Mar 2002 (RRD) - improved w3lib consistency
!                 09 Apr 2002 (RRD) - grib test
!                 03 Feb 2005 (GDR) - changed TPPT/CPPT to TPP3/CPP3
!                                     renamed CFG_ETA to CFG_NAM
!                 29 Dec 2005 (BS)  - exclude precip at +0
!                 04 Jan 2012 (BS)  - add sfc fields 
!                 25 Jul 2013 (BS)  - allocatable BUFF, stop error codes
!
! USAGE: NAM12ARL [GRIB_FILE_NAME] [GRIB_FILE_NAME2] [...]
!   INPUT ARGUMENT LIST:
!     GRIB_FILE_NAME - multiple grib files may be defined
!   OUTPUT ARGUMENT LIST:
!     NONE
!   INPUT FILES:
!     GRIB_DATA - grib input data files use special directIO routines
!   OUTPUT FILES:
!     unit 20 DATA.NAM - ARL packed data output file
!     unit 30 CFG_NAM - temp file area
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE:  IBM/RS6000
!
!$$$

      PROGRAM NAM12ARL

      LOGICAL   ftest
      CHARACTER FNAME*80
      INTEGER*4 handle, fcopen

!     check for command line arguments
      narg=IARGC()
      if(narg.eq.0)then
         write(*,*)'Usage: nam12arl [file_1] [file_2] [...]'
         write(*,*)'Convert pressure 12 km NAM grib (grid 218) file to arl format'
         stop 20
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
! SUBPROGRAM:  EXTRACT          MAIN ROUTINE FOR DECODING NAM GRIB DATA
!   PRGMMR:    GLENN ROLPH      ORG: R/ARL       DATE:01-11-15
!
! ABSTRACT:  THIS CODE WRITTEN AT THE AIR RESOURCES LABORATORY ...
!            EXTRACTS ONE RECORD FROM INPUT GRIB FILE AND ADDS ONE
!            RECORD TO THE OUTPUT FILE
!
! PROGRAM HISTORY LOG:
!   LAST REVISED: 15 Nov 2001 - GDR
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
!     dimensions of eta 12 km grid 218 
!       /com/eta/para/eta.YYYYMMDD/eta.tXXz.awphysXX.tm00

      PARAMETER (NLVL=27, MVAR=29, MAXX=614, MAXY=428)
      PARAMETER (MAXC=550000)

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
      CHARACTER(1), ALLOCATABLE :: BUFF(:)    
      LOGICAL*1,    ALLOCATABLE :: KBMS(:)    
      LOGICAL   FTEST

!     unpacked output array
      REAL   RVAR(MAXX,MAXY),PVAR(MAXX,MAXY)

!     packed output array
      CHARACTER CVAR(MAXC)*1, FNAME*80

!     remap array from one to two dimensions
      REAL SVAR(MAXX*MAXY)
      INTEGER SHAPE(2)
      DATA SHAPE/MAXX,MAXY/

!     default information (grid id, output records)
      DATA IG/99/, KREC/0/

!--------------------------------------------------------------------
!     predefine number of variables surface and aloft
!     first dimension is number of surface variables,
!     second dimension is number of levels above surface * number of upper fields
      DATA NVAR / 29, 26*7 /

!     set output structure in ARL character format
      DATA VCHAR0/ 'SHGT','MSLP','TPP3','CPP3','T02M',           &
                   'RH2M','U10M','V10M','PRSS','LHTF',           &
                   'SHTF','USTR','RGHS','DSWF','PBLH',           &
                   'TMPS','SOLT','SOLW','WESD','CSNO',           &
                   'CICE','CFZR','CRAI','VSBY','TCLD',           &
                   'CAPE','CINH','LISD','LIB4'/
      DATA VCHAR1/ 'UWND','VWND','HGTS','TEMP','WWND','RELH','TKEN',           &
                    22*'    '/

!     set output structure by defining GRIB variables (sfc + level 1)
      DATA VGRIB0/ 7,  2, 61, 63, 11,                            &
                  52, 33, 34,  1, 121,                           &
                  122,253,83, 204,221,                           &
                   11, 85, 86, 65, 143,                          &
                  142,141,140, 20,  71,                          &
                  157,156,131,132/
      DATA VGRIB1/33, 34,  7, 11, 39, 52, 158,22*0/

!     special surface variables level identification
      DATA SIG0/  0,   0,   0,   0,   2,                         &
                  2,  10,  10,   0,   0,                         &
                  0,   0,   0,   0,   0,                         &
                  0,  10, 200,   0,   0,                         &
                  0,  0,    0,   0,   0,                         &
                  0,  0,12900,46080/

      DATA STYP/  1,   102,   1,   1, 105,                       &
                  105, 105, 105, 1,  1,                          &
                  1,   1,   1,   1,  1,                          &
                  1, 112, 112,   1,   1,                         &
                  1,   1,   1,   1, 200,                         &
                  1,   1, 101, 116/

!     units conversion factors before converting to ARL format
      DATA CNVRT0/  1.0, 0.01, 0.001,0.001, 1.0,                 &
                    1.0,   1.0,  1.0, 0.01, 1.0,                 &
                    1.0,  1.0,  1.0, 1.0,   1.0,                 &
                    1.0,  1.0,  1.0, 1.0,   1.0,                 &
                    1.0,  1.0,  1.0, 1.0,   1.0,                 &
                    1.0,  1.0,  1.0, 1.0/
      DATA CNVRT1/  1.0,   1.0,  1.0,  1.0, 0.01,  1.0, 1.0,22*0.0/

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
! COMMENT OUT BY JPHUANG 12/28/2012
!  INTERFACE
! SUBROUTINE W3FI63(BUFF,KPDS,KGDS,KBMS,RVAR,KPTR,KRET)
!  CHARACTER(1), INTENT(IN)  :: BUFF(:)
!  LOGICAL(1),   INTENT(OUT) :: KBMS(:)
!  INTEGER,      INTENT(OUT) :: KPDS(:)
!  INTEGER,      INTENT(OUT) :: KGDS(:)
!  REAL,         INTENT(OUT) :: RVAR(:)
!  INTEGER,      INTENT(OUT) :: KPTR(:)
!  INTEGER,      INTENT(OUT) :: KRET
!  END SUBROUTINE w3fi63
!  END INTERFACE
!-------------------------------------------------------------------

!     input grib record byte counter
      kbyte=0

!     read the indicator section
    ! section from nams2arl.f to make buffer allocatable 
! inital dummy allocation to permit reading of grib record length
  IF(.NOT.ALLOCATED(BUFF)) ALLOCATE (BUFF(8),STAT=KRET)
  IF(.NOT.ALLOCATED(KBMS)) ALLOCATE (KBMS(8),STAT=KRET)

100 CONTINUE

!   load the indicator section to determine record length
    CALL FCPTPS(HANDLE,KBYTE,*920)
    CALL FCREAD(HANDLE,BUFF,1,8,*900)
    IF(BUFF(1)//BUFF(2)//BUFF(3)//BUFF(4).NE.'GRIB')THEN
       KBYTE=KBYTE+1
       GOTO 100
    END IF
    KLEN=ICHAR(BUFF(7))+ICHAR(BUFF(6))*256+ICHAR(BUFF(5))*65536

!   reallocate buffer and kbms if necessary
    IF(KLEN.GT.SIZE(BUFF,1))THEN
       DEALLOCATE (BUFF,STAT=KRET)
       ALLOCATE (BUFF(KLEN),STAT=KRET)
       IF(KRET.NE.0) WRITE(*,*)'Buffer allocation error: ',KRET,KLEN
       WRITE(*,*)'Buffer allocation: ',KLEN
       DEALLOCATE (KBMS,STAT=KRET)
       ALLOCATE (KBMS(KLEN),STAT=KRET)
       IF(KRET.NE.0) WRITE(*,*)'kbms   allocation error: ',KRET,KLEN
    END IF

!   read the entire data record
    ! end section from nams2arl.f
      call fcptps(handle,kbyte,*900)
      call fcread(handle,buff,1,8,*900)
      IF(BUFF(1)//BUFF(2)//BUFF(3)//BUFF(4).NE.'GRIB')THEN
         KBYTE=KBYTE+1
         GOTO 100
      END IF

!     determine the length of the entire grib record
      klen=jchar(buff(7))+jchar(buff(6))*256+jchar(buff(5))*65536
     ! since now re-allocate, do not need this
     !if(klen.gt.maxb)then
     !   write(*,*)'Grib record: ',klen
     !   write(*,*)'Exceedes buffer: ',maxb
     !   stop
     !end if

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
            IF(KTIM3.EQ.0)GO TO 300
!          Test to get TPP3/CPP3 not TPPT/CPPT - both 61/63
            if((vgrib.eq.61.or.vgrib.eq.63).and.                              &
                ktim2-ktim1.le.3.and.ktim2-ktim1.gt.0)go to 300
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
!     remap input data from one- to two-dimensional array
      RVAR=RESHAPE(SVAR,SHAPE)
      if(kret.ne.0)then
         write(*,*)'Error W3FI63: ',kret
         stop 40
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
         call makndx(kgds,vchar0,vchar1,mvar,nlvl,nvar,                        &
                     sigl,ig,nxp,nyp)

         if(nxp.gt.maxx.or.nyp.gt.maxy)then
            write(*,*)'Real array size: ',nxp,nyp
            write(*,*)'Exceedes dimensions: ',maxx,maxy
            stop 50
         end if

         nxy=nxp*nyp
         if(nxy.gt.maxc)then
            write(*,*)'Packed output array: ',nxy
            write(*,*)'Exceeds dimensions: ',maxc
            stop 60
         end if
         call pakset(20,'CFG_NAM',1,nxp,nyp,nzp)

!        standard file name for output
         FNAME='DATA.NAM'
         INQUIRE(FILE=FNAME,EXIST=FTEST)

!        open data file
         LREC=NXY+50
         OPEN(20,FILE=FNAME,RECL=LREC,                                         &
            ACCESS='DIRECT',FORM='UNFORMATTED')

         IF(.NOT.FTEST)THEN
!           output data set initialized to missing
            KINI=1
            write(*,*)'Initialized output data set: ',fname
         ELSE
!           output data set created previously, fill in data          
            KINI=0
         END IF
      END IF

!     perform any required units conversion
      if(cnvrt.ne.1.0) call datcnv(rvar,nxp,nyp,cnvrt)

!     then pack into ARL format and continue
!     write(*,*)'Record: ',krec,kv,kl,ktype,vchar,sigl(kl)
      call pakrec(20,rvar,cvar,nxp,nyp,nxy,vchar,                              &
                  IYR,IMO,IDA,IHR,IMN,IFH,KL,KINI)


  800 kbyte=kbyte+klen
      GO TO 100

    ! from nams2arl.f (6-14-12)
!   normal end of file
900 KRET=0
    RETURN

!   next-time period
910 KRET=1
    RETURN

!   end-of-input
920 KRET=2
    ! end from nams2arl.f

      RETURN
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
!   LAST REVISED: 15 Nov 2001 - GDR
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
!     UNIT 30 - CFG_NAM defines the output file grid and structure
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE:  IBM/RS6000
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

!     DO II=1,13
!       WRITE(6,*)II,KGDS(II)
!     END DO
!     STOP

!     the number of grid points
      nxp=kgds(2)
      nyp=kgds(3)

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

!        set the pole position and reference lat/lon
         grids(1)=90.0
         grids(2)=0.0
         grids(3)=kgds(12)/1000.0

!        reference longitude = grid orientation
         grids(4)=kgds(7)/1000.0

!        delta=x grid size
         grids(5)=kgds(8)/1000.0

!        tangent latitude
         grids(7)=kgds(12)/1000.0
      else
         write(*,*)'Undefined projection: ',kgds(1)
         stop 70
      end if

!     write the packer configuration file
      OPEN(30,file='CFG_NAM')
      WRITE(30,'(20X,A4)')'NAM'

!     grid number 99 and 1 for sigma coordinate system
!     grid number 99 and 2 for pressure coordinate system
      WRITE(30,'(20X,I4)') IG, 2

      WRITE(30,'(20X,F10.4)')(GRIDS(I),I=1,12)
      WRITE(30,'(20X,I4)')nxp,nyp,nlvl

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
