!$$$  PROGRAM DOCUMENTATION BLOCK
!
! PROGRAM:     narr2arl.f   DECODE GRIB NAM MODEL FIELD FOR HYSPLIT
!   PRGMMR:    GLENN ROLPH      ORG: R/ARL       DATE:99-01-29
!
! ABSTRACT:  THIS CODE WRITTEN AT THE AIR RESOURCES LABORATORY ...
!     converts an narr model pressure grib file to arl packed format
!     processes only one time period per execution.  Multiple input files
!     may be specified on the command line, each containing different
!     variables required for the specific time period.  Input data are
!     assumed to be already on a conformal map projection on model levels.
!     However, wind vectors need to be rotated from true to the projection!
!     Only packing to ARL format and units conversion is required.
!
! PROGRAM HISTORY LOG:
!   LAST REVISED: 30 Mar 2000 (GDR) - changed to 3hr precip
!                 29 Jan 1999 (GDR)
!                 11 Oct 2001 (RRD) - conversion to f90
!                 18 Oct 2001 (RRD) - large grid domains
!                 05 Mar 2002 (RRD) - improved w3lib consistency
!                 09 Apr 2002 (RRD) - grib test
!                 30 May 2006 (JLH) - for NARR
!                 11 Feb 2008 (RRD) - wind rotation
!                 05 Mar 2008 (RRD) - friction velocity correction
!                 24 Feb 2010 (RRD) - pressure level test
!                 28 Feb 2012 (RRD) - fixed missing TKE field
!                 27 Mar 2014 (RRD) - auto grid reduction for bit maps
!                 15 Jan 2015 (AFS) - reads existing CFG file
!
! USAGE: NARR2ARL [GRIB_FILE_NAME] [GRIB_FILE_NAME2] [...]
!   INPUT ARGUMENT LIST:
!     GRIB_FILE_NAME - multiple grib files may be defined
!   OUTPUT ARGUMENT LIST:
!     NONE
!   INPUT FILES:
!     GRIB_DATA - grib input data files use special directIO routines
!     unit 50 CFG.OVER  - overriding cfg file
!   OUTPUT FILES:
!     unit 20 DATA.NARR - ARL packed data output file
!     unit 30 CFG.NARR  - temp file area
!     unit 40 TIME.NARR - time indicator file
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE:  IBM/RS6000
!
!$$$

      PROGRAM NARR2ARL

      LOGICAL   ftest
      CHARACTER FNAME*80
      INTEGER*4 handle, fcopen

!     check for command line arguments
      narg=IARGC()
      if(narg.eq.0)then
         write(*,*)'Convert pressure narr grib1 file to arl format'
         write(*,*)'Usage: narr2arl [file_1] [file_2] [...]'
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
      END PROGRAM narr2arl


!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM:  EXTRACT          MAIN ROUTINE FOR DECODING NAM GRIB DATA
!   PRGMMR:    GLENN ROLPH      ORG: R/E/AR      DATE:99-01-29
!
! ABSTRACT:  THIS CODE WRITTEN AT THE AIR RESOURCES LABORATORY ...
!            EXTRACTS ONE RECORD FROM INPUT GRIB FILE AND ADDS ONE
!            RECORD TO THE OUTPUT FILE
!
! PROGRAM HISTORY LOG:
!   LAST REVISED: 28 Jan 2005 - GDR
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
      PARAMETER (NLVL=24, MVAR=12, MAXX=500, MAXY=500, MAXB=900000)
      PARAMETER (MAXC=900000)

!     arrays to hold grib, character, and level variable information
      INTEGER     SIGL(NLVL), NVAR(NLVL), HANDLE
      INTEGER     VGRIB, VGRIB0(MVAR), VGRIB1(MVAR),                           &
                  SIG0(MVAR), STYP(MVAR)
      CHARACTER*4 VCHAR, PCHAR, VCHAR0(MVAR), VCHAR1(MVAR)
      REAL        CNVRT, CNVRT0(MVAR), CNVRT1(MVAR)

!     define arrays for product definition section, grid description,
!     and other grib information
      INTEGER   KPDS(25),KGDS(25),KPTR(25)

!     input data buffer and bit map section
      CHARACTER BUFF(MAXB)*1
      LOGICAL*1 KBMS(MAXB), KBIT(MAXX,MAXY)
      LOGICAL*1 NBIT
      LOGICAL   FTEST

!     mapping arrays
      REAL grids(12),   parmap(9),  gridsp(12)

!     unpacked output array
      REAL RVAR(MAXX,MAXY)

!     bit map reducted array
      REAL, ALLOCATABLE :: PVAR(:,:),TVAR(:,:)

!     packed output array
      CHARACTER CVAR(MAXC)*1, FNAME*80

!     remap array from one to two dimensions
      REAL SVAR(MAXX*MAXY)
      INTEGER SHAPE(2)
      DATA SHAPE/0,0/

!     default information (grid id, output records,
      DATA IG/18/, KREC/0/

!--------------------------------------------------------------------
!     predefine number of variables surface and aloft
      DATA NVAR /12,15*7,8*6/

!     surface variables
      DATA VGRIB0/   61  , 204  , 253  , 221  ,                                &
                    121  ,  1   ,  2   , 122  ,                                &
                     71  ,  11  ,  33  ,  34  /

      DATA STYP/     1   ,  1   ,  1   ,  1   ,                                &
                     1   ,  1   , 102  ,  1   ,                                &
                    200  , 105  , 105  , 105  /

      DATA SIG0/     0   ,  0   ,  0   ,  0   ,                                &
                     0   ,  0   ,  0   ,  0   ,                                &
                     0   ,  2   ,  10  ,  10  /

      DATA VCHAR0/ 'TPP3','DSWF','USTR','HPBL',                                &
                   'LHTF','PRSS','MSLP','SHTF',                                &
                   'TCLD','T02M','U10M','V10M'/

      DATA CNVRT0/ 0.001 , 1.0  , 1.0  , 1.0  ,                                &
                    1.0  , 0.01 , 0.01 , 1.0  ,                                &
                    1.0  , 1.0  , 1.0  , 1.0  /

!     upper air variables
      DATA VGRIB1/   33  ,  34  ,  7   ,  11  ,  39  ,  51  , 158,             &
                    5*0/

      DATA VCHAR1/ 'UWND','VWND','HGTS','TEMP','WWND','SPHU','TKEN',           &
                    5*'    '/

      DATA CNVRT1/  1.0  , 1.0  , 1.0  , 1.0  , 0.01 , 1.0  , 1.0,             &
                    5*0.0/

!     sigma levels for input and output
      DATA SIGL/   0,1000,975,950,925,900,875,850,825,800,775,750,725,         &
                          700,650,600,550,500,450,400,350,300,200,100/

      SAVE krec,nbit,shape
  
      COMMON / SETUP / GRIDS, PARMAP

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

  SUBROUTINE PAKREC(LUNIT,RVAR,CVAR,NX,NY,NXY,KVAR,IY,IM,ID,IH,MN,IC,LL,KINI)
  INTEGER,      INTENT(IN)    :: LUNIT       ! output unit number
  INTEGER,      INTENT(IN)    :: NX,NY       ! dimensions of RVAR
  INTEGER,      INTENT(IN)    :: NXY         ! dimensions of CVAR
  REAL,         INTENT(IN)    :: RVAR(NX,NY) ! input data to be packed
  CHARACTER(1), INTENT(INOUT) :: CVAR(NXY)   ! packed data array
  CHARACTER(4), INTENT(IN)    :: KVAR        ! descriptor of variable written
  INTEGER,      INTENT(IN)    :: IY,IM,ID    ! date identification
  INTEGER,      INTENT(IN)    :: IH,MN       ! time identification (MN-minutes)
  INTEGER,      INTENT(IN)    :: IC          ! forecast hour, ICX hour for >99
  INTEGER,      INTENT(IN)    :: LL          ! level indicator 
  INTEGER,      INTENT(IN)    :: KINI        ! initialization (0-no 1-yes)
  END SUBROUTINE pakrec

  SUBROUTINE ROTATE (UU,VV,NX,NY)
  REAL, INTENT(INOUT) :: UU(:,:)
  REAL, INTENT(INOUT) :: VV(:,:)
  INTEGER, INTENT(IN) :: NX,NY
  END SUBROUTINE rotate
  END INTERFACE

!-------------------------------------------------------------------

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
         IF(KVARB.EQ.VGRIB.AND.LEVEL.EQ.SIG0(KV).AND.STYP(KV).EQ.KTYPE)goto 300
      end do

!     then check for 3d variable
      kl=2
      do kv=1,nvar(kl)
         vgrib=vgrib1(kv)
         vchar=vchar1(kv)
         cnvrt=cnvrt1(kv)
         if(kvarb.eq.vgrib.and.ktype.eq.100)go to 200
      end do
      go to 800

!     check if 3d level is present in selection table
  200 do kl=2,nlvl
         if(kvarb.eq.158.and.kl.gt.16) go to 800 ! ignore TKE above 600 hPA
         if(level.eq.sigl(kl))go to 300          ! get height index value
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
      if(shape(1).EQ.0.OR.shape(2).EQ.0)THEN
!        use the full-grid dimensions
         shape(1)=kgds(2)
         shape(2)=kgds(3)
      end if
      RVAR=RESHAPE(SVAR,SHAPE)
      KBIT=RESHAPE(KBMS,SHAPE)

!     correct for Y2K
      kpds(8)=mod(kpds(8),100)

!     set the current time
      iyr=kpds(8)
      imo=kpds(9)
      ida=kpds(10)
      ihr=kpds(11)
      imn=kpds(12)
      ifh=max(kpds(14),kpds(15))
      if(ifh.ne.0) then
!       force fcst hr to 0 for precip (61) & cloud cover (71) so pakrec works
        ifh=0
      end if
      call tmplus(iyr,imo,ida,ihr,ifh)

!     for the the first record create an index record for pakrec
      IF(krec.eq.1)then
!        decode grib information and create index record structure
         call makndx(kgds,vchar0,vchar1,mvar,nlvl,nvar,sigl,ig,nxp,nyp)
!       check if overriding CFG file exists
          INQUIRE(FILE='CFG.OVER', EXIST=ftest) 
          if(ftest)then
            OPEN(50,file='CFG.OVER')
            READ(50,'(20X,A4)')MODEL
            READ(50,'(20X,I4)') IG, N
            READ(50,'(20X,F10.4)')(GRIDSP(I),I=1,12)
            CLOSE(50)

!         get i1,j1 for overrriding domain
          CALL CLL2XY(PARMAP,gridsp(10),gridsp(11),x1p,y1p)
          i1p=NINT(x1p)
          j1p=NINT(y1p)
          end if

!        quick array limits test for recompilation
         if(nxp.gt.maxx.or.nyp.gt.maxy)then
            write(*,*)'Real array size x,y: ',nxp,nyp
            write(*,*)'Exceedes dimensions: ',maxx,maxy
            write(*,*)'RECOMPILE!'
            stop
         end if

         nxy=nxp*nyp
         if(nxy.gt.maxc)then
            write(*,*)'Packed output array: ',nxy
            write(*,*)'Exceeds dimensions : ',maxc
            write(*,*)'RECOMPILE!'
            stop
         end if

!        check if map reduction required to eliminate bit-map masking
         i1=1
         i2=nxp
         j1=1
         j2=nyp
         nbit=.true.

         do while (nbit)
            nbit=.false.
            do j=j1,j2
            do i=i1,i2
!              when any one point true, reduce grid and retest
               if(kbit(i,j)) nbit=.true.
            end do
            end do
            if(nbit)then
               j1=j1+1
               j2=j2-1
               i1=i1+1
               i2=i2-1
            end if
         end do

         IF(I1.EQ.1.AND.I2.EQ.NXP.AND.J1.EQ.1.AND.J2.EQ.NYP)THEN
!           full grid output, no bitmap defined
            nbit=.false.
         ELSE
            nbit=.true.
!           define new corner lat-lon
            CALL CXY2LL(PARMAP,FLOAT(I1),FLOAT(J1),CLAT,CLON)
            KGDS(4)=CLAT*1000.0
            KGDS(5)=CLON*1000.0
      
!           new number of grid points
            KGDS(2)=I2-I1+1
            KGDS(3)=J2-J1+1

            write(*,*)'Full domain grid: ',nxp,nyp
            call makndx(kgds,vchar0,vchar1,mvar,nlvl,nvar,sigl,ig,nxp,nyp)
            write(*,*)'Bitmap reduction: ',nxp,nyp
            nxy=nxp*nyp
          
            INQUIRE(FILE='CFG.OVER', EXIST=ftest)
            if(ftest)then
            if(I1.le.I1P.and.J1.le.J1P)then
            I1=I1P
            J1=J1P
            else
            write(*,*)'ERROR: overriding cfg domain is bigger than bitmap extracted domain'
            stop
            endif 
            endif

         END IF
         ALLOCATE (PVAR(nxp,nyp),TVAR(nxp,nyp))
!        configure packing subroutines
            INQUIRE(FILE='CFG.OVER', EXIST=ftest)
            if(ftest)then
             call pakset(20,'CFG.OVER',1,nxp,nyp,nzp)
            else
             call pakset(20,'CFG.NARR',1,nxp,nyp,nzp)
            endif
!        write control file with date
         OPEN(40,FILE='TIME.NARR')
         WRITE(40,'(5I2)')IYR,IMO,IDA,IHR,IFH
         CLOSE (40)

!        standard file name for output
         FNAME='DATA.NARR'
         INQUIRE(FILE=FNAME,EXIST=FTEST)

!        open data file
         LREC=NXY+50
         OPEN(20,FILE=FNAME,RECL=LREC,ACCESS='DIRECT',FORM='UNFORMATTED')
         IF(.NOT.FTEST)THEN
!           output data set initialized to missing
            KINI=1
            write(*,*)'Output initialized ... filled with missing'
         ELSE
!           output data set created previously, fill in data          
            KINI=0
            write(*,*)'Output un-initialized ... filling with data'
         END IF
         
      END IF

!     perform any required units conversion
      if(cnvrt.ne.1.0) rvar=rvar*cnvrt
!     remap into final array space
      DO I=1,NXP   
      DO J=1,NYP
         TVAR(I,J)=RVAR(I+I1-1,J+J1-1)
      END DO
      END DO

!#### then pack into ARL format and continue
!#### write(*,*)'Record: ',krec,kv,kl,ktype,vchar,sigl(kl)

      IF(VCHAR(1:1).EQ.'U'.AND.VCHAR.NE.'USTR')THEN
!        save values for rotation
         PVAR=TVAR
         PCHAR=VCHAR

      ELSEIF(VCHAR(1:1).EQ.'V')THEN    
!        convert compass winds to grid-orientation
         CALL ROTATE(PVAR,TVAR,NXP,NYP)
         call pakrec(20,pvar,cvar,nxp,nyp,nxy,pchar,       &
                     IYR,IMO,IDA,IHR,IMN,IFH,KL,KINI)
         call pakrec(20,tvar,cvar,nxp,nyp,nxy,vchar,       &
                     IYR,IMO,IDA,IHR,IMN,IFH,KL,KINI)

      ELSE
!        all other variables
         call pakrec(20,tvar,cvar,nxp,nyp,nxy,vchar,       &
                     IYR,IMO,IDA,IHR,IMN,IFH,KL,KINI)
      END IF

  800 kbyte=kbyte+klen
      GO TO 100

  900 RETURN
      END

      SUBROUTINE ROTATE (UU,VV,NX,NY)
      REAL, INTENT(INOUT) :: UU(:,:)
      REAL, INTENT(INOUT) :: VV(:,:)
      INTEGER, INTENT(IN) :: NX,NY
      REAL GRIDS(12), PARMAP(9)
      COMMON/ SETUP / GRIDS, PARMAP
      DO I=1,NX
      DO J=1,NY
         CALL CC2GXY(PARMAP,FLOAT(I),FLOAT(J),UU(I,J),VV(I,J),UG,VG)
         UU(I,J)=UG
         VV(I,J)=VG
      END DO
      END DO
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
!     UNIT 30 - CFG.NARR defines the output file grid and structure
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE:  CRAY
!
!$$$

      SUBROUTINE MAKNDX(kgds,vchar0,vchar1,mvar,nlvl,nvar,sigl,ig,nxp,nyp)

      LOGICAL FTEST
      INTEGER kgds(25)
      REAL    grids(12),parmap(9)
      
      COMMON / SETUP / GRIDS, PARMAP
      DATA KONCE/0/ 
!     arrays to hold variable selection information
      INTEGER   sigl(nlvl), nvar(nlvl)
      CHARACTER*4 VCHAR0(MVAR), VCHAR1(MVAR)

      CHARACTER*4 MODEL
      INTEGER  N,L

     
     INQUIRE(FILE='CFG.OVER', EXIST=ftest)
      IF((ftest).and.(konce.eq.1))THEN
      WRITE(*,*)'WARNING: Using an overriding encoding configuration: ','CFG.OVER'
      OPEN(50,file='CFG.OVER')

      READ(50,'(20X,A4)')MODEL

!     grid number 99 and 1 for sigma coordinate system
!     grid number 99 and 2 for pressure coordinate system
      READ(50,'(20X,I4)') IG, N
      READ(50,'(20X,F10.4)')(GRIDS(I),I=1,12)
      READ(50,'(20X,I4)')nxp,nyp,l
      DO NL=1,l
         if(nl.eq.1)then
            READ(50,'(20X,F6.1,I3,32(1X,A4))')                                &
            sigma,nvar(nl),(vchar0(nv),nv=1,nvar(nl))
         else
            READ(50,'(20X,F6.1,I3,32(1X,A4))')                                &
            sigma,nvar(nl),(vchar1(nv),nv=1,nvar(nl))
         end if
      END DO

      CLOSE(50)

!     initialize the lat-lon conversion routines (rrd - 2/11/2008)
!     define the tangent latitude and reference longitude
      CALL STLMBR(PARMAP,GRIDS(7),GRIDS(4))
!     define the grid by a one-point specification
      CALL STCM1P(PARMAP,GRIDS(8),GRIDS(9),GRIDS(10),GRIDS(11),                &
                         GRIDS(3),GRIDS(4),GRIDS(5),GRIDS(6))
!    no overriding cfg file 
!    normal run
     ELSE
!    first pass through subroutine
      konce=1

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
         stop
      end if

!     initialize the lat-lon conversion routines (rrd - 2/11/2008)
!     define the tangent latitude and reference longitude
      CALL STLMBR(PARMAP,GRIDS(7),GRIDS(4))
!     define the grid by a one-point specification
      CALL STCM1P(PARMAP,GRIDS(8),GRIDS(9),GRIDS(10),GRIDS(11),                &
                         GRIDS(3),GRIDS(4),GRIDS(5),GRIDS(6))

!     write the packer configuration file
      OPEN(30,file='CFG.NARR')
      WRITE(30,'(20X,A4)')'NARR'

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
     END IF
     END
