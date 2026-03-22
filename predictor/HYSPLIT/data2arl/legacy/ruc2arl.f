!$$$  PROGRAM DOCUMENTATION BLOCK
!
! PROGRAM:     RUC2ARL         DECODE GRIB RUC MODEL FIELD FOR HYSPLIT     
!   PRGMMR:    GLENN ROLPH      ORG: R/ARL      DATE:08-05-14
!
! ABSTRACT:  THIS CODE WRITTEN AT THE AIR RESOURCES LABORATORY ...
!     converts an ruc model pressure grib file to arl packed format
!     processes only one time period per execution.  Multiple input files
!     may be specified on the command line, each containing different
!     variables required for the specific time period.  Input data are
!     assumed to be already on a conformal map projection on model levels.
!     Only packing to ARL format and units conversion is required.  
!
! PROGRAM HISTORY LOG:
!   LAST REVISED: 14 May 2008 - GDR                                 
!                 27 Mar 2002 - GDR                                 
!
! USAGE: RUC2ARL [GRIB_FILE_NAME] [GRIB_FILE_NAME2] [...]
!   INPUT ARGUMENT LIST:
!     GRIB_FILE_NAME - multiple grib files may be defined 
!   OUTPUT ARGUMENT LIST:
!     NONE
!   INPUT FILES:
!     GRIB_DATA - grib input data files use special directIO routines
!   OUTPUT FILES:
!     unit 20 DATA.RUC - ARL packed data output file
!     unit 30 CFG_RUC - temp file area
!     unit 40 RUCTIME - time indicator file
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE:  IBM RS/6000
!
!$$$

      PROGRAM RUC2ARL

      LOGICAL   ftest
      CHARACTER FNAME*80 
      INTEGER*4 handle, fcopen  

!     check for command line arguments
      narg=IARGC()
      if(narg.eq.0)then
         write(*,*)'Usage: ruc20arl [file_1] [file_2] [...]'      
         write(*,*)'Convert 20 km pressure ruc grib file to arl format'
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
! SUBPROGRAM:  EXTRACT          MAIN ROUTINE FOR DECODING RUC GRIB DATA     
!   PRGMMR:    GLENN ROLPH      ORG: R/ARL     DATE:02-03-27
!
! ABSTRACT:  THIS CODE WRITTEN AT THE AIR RESOURCES LABORATORY ...
!            EXTRACTS ONE RECORD FROM INPUT GRIB FILE AND ADDS ONE 
!            RECORD TO THE OUTPUT FILE 
!
! PROGRAM HISTORY LOG:
!   LAST REVISED: 27 Mar 2002 - GDR                                 
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
!   MACHINE:  IBM RS/6000
!
!$$$

      SUBROUTINE xtract(handle)

!     number of levels, variables, and array limits 
      PARAMETER (NLVL=26, MVAR=21, MAXX=301, MAXY=225, MAXB=280000)
      PARAMETER (MAXC=280000)      

!     arrays to hold grib, character, and level variable information
      INTEGER     SIGL(NLVL), NVAR(NLVL), HANDLE 
      INTEGER     VGRIB, VGRIB0(MVAR), VGRIB1(MVAR), SIG0(MVAR), STYP(MVAR) 
      CHARACTER*4 VCHAR, VCHARP, VCHAR0(MVAR), VCHAR1(MVAR) 
      REAL        CNVRT, CNVRT0(MVAR), CNVRT1(MVAR)
      
!     define arrays for product definition section, grid description,
!     and other grib information
      INTEGER   KPDS(25),KGDS(25),KPTR(25)

!     input data buffer and bit map section
      CHARACTER BUFF(MAXB)*1 
      LOGICAL*1   KBMS(MAXB)
      LOGICAL     FTEST, APCPCHK, CPCPCHK

!     unpacked output array
      REAL   RVAR(MAXX,MAXY),PVAR(MAXX,MAXY) 

!     packed output array
      CHARACTER CVAR(MAXC)*1 
      CHARACTER   FNAME*80

!     remap array from one to two dimensions
      REAL SVAR(MAXX*MAXY)
      INTEGER SHAPE(2)
      DATA SHAPE/MAXX,MAXY/

!     default information (grid id, output records,
      DATA IG/21/, KREC/0/

!--------------------------------------------------------------------
!     predefine number of variables surface and aloft 
      DATA NVAR / 21, 25*6 /               

!     special surface variables level identification
      DATA SIG0/  0,   0,   0,  10,  10,   2,   0,   0,   2,   2,     &
                  0,   0,   0,   0,   0,   0,   0,   0,   0,   0,     &
                  0/
      DATA STYP/  1, 102,   1,  105, 105, 105,  1,   1, 105,   105,   &
                  1,   1,   1,   1,   1,   1,   1,   1,   1,     1,   &
                  1/

!     set output structure by defining GRIB variables (sfc + level 1)
      DATA VGRIB0/ 1,129,  7, 33, 34,            &
                  11, 62, 63, 52, 17, 66, 65,    &
                  140,141,142,143,157,156,131,   &
                  221,20/
      DATA VGRIB1/ 7, 11, 33, 34, 39, 52, 15*0/

!     set output structure in ARL character format
      DATA VCHAR0/ 'PRSS','MSLP','SHGT','U10M','V10M',                &
                   'T02M','TPP1','CPP1','RH2M','DP2M','WESD','SNWD',  &
                   'CRAI','CFZR','CICE','CSNO','CAPE','CINH','LISD',  &
                   'MXHT','VSBY'/             
      DATA VCHAR1/ 'HGTS','TEMP','UWND','VWND','WWND','RELH',         &
                    15*'    '/ 
  
!     set requested sigma levels for input and output
      DATA SIGL/   0,1000,975,950,925,900,875,850,825,800,775,750,725,  &
                700,650,600,550,500,450,400,350,300,250,200,150,100/ 

!     units conversion factors before converting to ARL format
      DATA CNVRT0/ 0.01,  0.01,  1.0,  1.0,  1.0,                     &
                    1.0, 0.001,0.001,  1.0,  1.0,  1.0,               &
                    1.0,   1.0,   1.0,  1.0,  1.0, 1.0, 1.0,273.15,   &
                    1.0,   1.0/
      DATA CNVRT1/  1.0,   1.0,  1.0,  1.0, 0.01,  1.0, 15*0.0/
!--------------------------------------------------------------------

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
!-------------------------------------------------------------------

      APCPCHK=.FALSE.
      CPCPCHK=.FALSE.

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
!     check the precipitation to for hourly accumulation at octet 19 and 20 
      ktim1=jchar(buff(koff+19))
      ktim2=jchar(buff(koff+20))
      ktim3=jchar(buff(koff+21))
!     check if 2d variable present in selection table
      kl=1
      do kv=1,nvar(kl)
         vgrib=vgrib0(kv)
         vchar=vchar0(kv)
         cnvrt=cnvrt0(kv)
!        matches id and special level indicator
         IF(KVARB.EQ.VGRIB.AND.LEVEL.EQ.SIG0(KV).AND.STYP(KV).EQ.KTYPE) THEN
!      Test for precipitation (want hourly)
            kdiff=ktim2-ktim1
            IF(VGRIB.NE.62.AND.VGRIB.NE.63)THEN
               GO TO 300
            ELSE IF(VGRIB.EQ.62.AND.KDIFF.LE.1)THEN
               IF(.not.apcpchk)THEN
                  APCPCHK=.TRUE.
                  GO TO 300
               END IF
            ELSE IF(VGRIB.EQ.63.AND.KDIFF.LE.1)THEN
               IF(.not.cpcpchk)THEN
                  CPCPCHK=.TRUE.
                  GO TO 300
               END IF
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
         call makndx(kgds,vchar0,vchar1,mvar,nlvl,nvar,     &
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
         call pakset(20,'CFG_RUC',1,nxp,nyp,nzp)
      
!        write control file with date
         OPEN(40,FILE='RUCTIME')              
         WRITE(40,'(5I2)')IYR,IMO,IDA,IHR,IFH 
         CLOSE (40)                       
    
!        standard file name for output
         FNAME='DATA.RUC'
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
!        write(*,*)'Initialized output data set: ',fname

!           open output data set and initialize to missing
!           call datini(20,CVAR,NXY,NLVL,NVAR,IG,IYR,IMO,IDA,IHR)
!           write(*,*)'Initialized output data set: ',fname
!        END IF
      END IF

!     perform any required units conversion
      if(cnvrt.ne.1.0) call datcnv(rvar,nxp,nyp,cnvrt)

!     save stratoform precipitation to add to convective precip
      IF(VGRIB.EQ.62)THEN
         CALL PCPSUM(RVAR,PVAR,NXP,NYP,VGRIB)
         krecp=krec
         kvp=kv
         klp=kl
         ktypep=ktype
         vcharp=vchar
         sglp=sigl(kl)
         GO TO 800
      ELSE IF(VGRIB.EQ.63)THEN
         CALL PCPSUM(RVAR,PVAR,NXP,NYP,VGRIB)
!        write total precipitation field
!        write(*,*)'Record: ',krecp,kvp,klp,ktypep,vcharp,sglp 
         call pakrec(20,pvar,cvar,nxp,nyp,nxy,vcharp,            &
           IYR,IMO,IDA,IHR,IMN,IFH,KLP,kini)                                 
      END IF
 
!     then pack into ARL format and continue
!     write(*,*)'Record: ',krec,kv,kl,ktype,vchar,sigl(kl) 
      call pakrec(20,rvar,cvar,nxp,nyp,nxy,vchar,                & 
        IYR,IMO,IDA,IHR,IMN,IFH,KL,kini)                                 


  800 kbyte=kbyte+klen
      GO TO 100

  900 RETURN
      END


!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM:  MAKNDX           CREATE THE INDEX RECORD CONFIGURATION FILE  
!   PRGMMR:    GLENN ROLPH      ORG: R/ARL       DATE:02-03-27
!
! ABSTRACT:  THIS CODE WRITTEN AT THE AIR RESOURCES LABORATORY ...
!            CREATES THE CONFIGURATION FILE FOR THE OUTPUT DATA WHICH
!            DEFINES THE GRID SYSTEM AS WELL AS ALL VARIABLES THAT ARE
!            TO BE WRITTEN TO THE OUTPUT FILE.
!
! PROGRAM HISTORY LOG:
!   LAST REVISED: 27 Mar 2002 - GDR                                 
  
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
!     UNIT 30 - CFG_RUC defines the output file grid and structure
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE:  CRAY
!
!$$$

      SUBROUTINE MAKNDX(kgds,vchar0,vchar1,mvar,nlvl,nvar,sigl,ig,nxp,nyp)

      LOGICAL FTEST
      INTEGER   kgds(25) 
      REAL   grids(12)

!     arrays to hold variable selection information
      INTEGER   sigl(nlvl), nvar(nlvl)
      CHARACTER*4 VCHAR0(MVAR), VCHAR1(MVAR)

!     the number of grid points
      nxp=kgds(2)
      nyp=kgds(3)

!     if configuration exists exit
!     INQUIRE(FILE='CFG_RUC',EXIST=FTEST)
!     IF(FTEST)RETURN

!     grid orientation
      grids(6)=0.0

!     lat/lon of grid point 1,1
      grids(8)=1.0
      grids(9)=1.0
!     lat/lon of lower left point
      grids(10)=kgds(4)/1000.0
      grids(11)=kgds(5)/1000.0-360.0
      grids(12)=0.0

!     defines a lambert conformal projection
      if(kgds(1).eq.3)then

!        set the pole position and reference lat/lon 
         grids(1)=90.0
         grids(2)=0.0

!        reference latitude 
         grids(3)=kgds(12)/1000.0

!        reference longitude = grid orientation
         grids(4)=kgds(7)/1000.0-360.0

!        delta=x grid size in km
         grids(5)=kgds(8)/1000.0

!        tangent latitude
         grids(7)=kgds(12)/1000.0
      else
         write(*,*)'Undefined projection: ',kgds(1)
         stop
      end if
    
!     write the packer configuration file
      OPEN(30,file='CFG_RUC') 
      WRITE(30,'(20X,A4)')'RUC'  

!     grid number 99 and 2 for pressure coordinate system
      WRITE(30,'(20X,I4)') IG, 2

      WRITE(30,'(20X,F10.4)')(GRIDS(I),I=1,12)
      WRITE(30,'(20X,I4)')nxp,nyp,nlvl

!     upper level information
      DO NL=1,NLVL
         sigma=sigl(nl)
         if(nl.eq.1)then
            WRITE(30,'(20X,F6.1,I3,21(1X,A4))') sigma,nvar(nl),   &
                        (vchar0(nv),nv=1,nvar(nl))
         else
            WRITE(30,'(20X,F6.1,I3,21(1X,A4))') sigma,nvar(nl),   &
                        (vchar1(nv),nv=1,nvar(nl))
         end if
      END DO
      CLOSE (30)
      RETURN
      END


!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM:  DATCNV           CONVERT UNITS FOR ALL ELEMENTS OF THE DATA  
!   PRGMMR:    GLENN ROLPH      ORG: R/ARL       DATE:02-03-27
!
! ABSTRACT:  THIS CODE WRITTEN AT THE AIR RESOURCES LABORATORY ...
!            IS USED TO CONVERT EACH ELEMENT OF THE OUTPUT DATA GRID
!            TO CONFORM WITH UNIT CONVENTIONS USED IN OTHER APPLICATIONS
!
! PROGRAM HISTORY LOG:
!   LAST REVISED: 27 Mar 2002 - GDR                                 
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

        if(cnvrt.eq.273.15)then
         rvar(i,j)=rvar(i,j)+cnvrt
        else
         rvar(i,j)=rvar(i,j)*cnvrt
        end if

      END DO
      END DO
      RETURN
      END

!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM:  PCPSUM           SAVE STRATOFORM PRECIPITATION TO ARRAY  
!   PRGMMR:    GLENN ROLPH      ORG: R/ARL       DATE:02-03-27
!
! ABSTRACT:  THIS CODE WRITTEN AT THE AIR RESOURCES LABORATORY ...
!            IS USED TO SAVE THE STRATOFORM PRECIPITATION SO THAT IT
!            CAN BE ADDED TO THE CONVECTIVE PRECIPITATION FOR TOTAL 
!            PRECIPITATION    
!
! PROGRAM HISTORY LOG:
!   LAST REVISED: 02 Mar 2002 - GDR                                 
!
! USAGE:  CALL DATCNV(RVAR,PVAR,NXP,NYP,VGRIB)
!   INPUT ARGUMENT LIST:
!     RVAR - incoming precipitation
!     PVAR - total precipitation (stratoform+convective)
!     NXP,NYP - dimensions of the array
!     VGRIB - precipitation type (62=stratoform, 63=convective)
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

      SUBROUTINE pcpsum(rvar,pvar,nxp,nyp,vgrib)

      REAL   RVAR(NXP,NYP),PVAR(NXP,NYP)

      IF(VGRIB.EQ.62)THEN
         DO J=1,NYP
         DO I=1,NXP
            if(rvar(i,j).lt.0.0)rvar(i,j)=0.0
            pvar(i,j)=rvar(i,j)
         END DO
         END DO
      ELSE
         DO J=1,NYP
         DO I=1,NXP
            if(rvar(i,j).lt.0.0)rvar(i,j)=0.0
            pvar(i,j)=pvar(i,j)+rvar(i,j)
         END DO
         END DO
      END IF
      RETURN
      END
 
