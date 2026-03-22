!-------------------------------------------------------------
! Copyright 2005-2007 ECMWF
!
! Licensed under the GNU Lesser General Public License which
! incorporates the terms and conditions of version 3 of the GNU
! General Public License.
! See LICENSE and gpl-3.0.txt for details.
!
!
!  Description: count messages before processing
!
!  Author: Enrico Fucile
!
!
!-------------------------------------------------------------
! API2ARL - Converts GRIB2 messages using ECMWF grib_api library
! routines to HYSPLIT packed (ARL) data format. The initial
! version of this program is based upon ECMWF count_messages.f
!-------------------------------------------------------------
! Last Revised: 18 Feb 2010       - Roland.Draxler@noaa.gov
!               22 Feb 2010 (RRD) - added support for Lambert
!               27 Oct 2017 (AMC) - replace grib_api library with eccodes
!-------------------------------------------------------------

PROGRAM api2arl 
  use eccodes
  implicit none

  integer                            ::  iargc,narg
  logical                            ::  ftest
  logical                            ::  invert = .false.
  integer                            ::  ifile
  integer                            ::  iret
  integer                            ::  num_msg
  integer                            ::  i,j,m,n
  integer                            ::  k,kl,kv
  integer                            ::  grid_base
  integer,dimension(:),allocatable   ::  igrib

  real                               ::  units  
  real                               ::  rlat ,rlon 
  real                               ::  clat ,clon 
  real                               ::  clat2,clon2 
  real                               ::  tlat1,tlat2 
  integer                            ::  nxp,nyp,nzp
  real                               ::  dlat,dlon
  real, dimension(:),   allocatable  ::  values
  real, dimension(:,:), allocatable  ::  rvalue
  character(len=4),     allocatable  ::  cvar(:) 
  integer                            ::  numberOfValues

  character(len=4)   :: gtype
  character(len=80)  :: message             
  character(len=80)  :: apicfg_name ! define grib variables
  character(len=80)  :: arlcfg_name ! define arl strucure 
  character(len=80)  :: grib_name   ! grib input file name
  character(len=80)  :: data_name   ! arl output data file
  character(len=256) :: value = 'not_set'

  integer            :: iyr,imo,ida,ihr,imn,ifh,if1,if2
  integer            :: zero,top

  integer, parameter                 :: maxvar = 25
  integer, parameter                 :: maxlev = 50
  integer                            :: numlev = 0
  integer                            :: levhgt
  integer, dimension(maxlev)         :: levels
  integer, dimension(:),allocatable  :: msglev
  integer, dimension(:),allocatable  :: msgvar

  integer                            :: numsfc, numatm
  real            ,dimension(maxvar) :: atmcnv, sfccnv 
  character(len=4),dimension(maxvar) :: atmgrb, sfcgrb 
  character(len=4),dimension(maxvar) :: atmarl, sfcarl 

  integer, dimension(maxvar)         :: sfcvar 
  integer, dimension(maxvar,maxlev)  :: atmvar 
  integer, dimension(maxvar,maxlev)  :: wrkvar 

  NAMELIST/SETUP/ numatm, atmgrb, atmcnv, atmarl,   &
                  numsfc, sfcgrb, sfccnv, sfcarl

!------------------------------------------------------------
! Interface to ARL packing routines found in the HYSPLIT
! library: ./hysplit/library/libhysplit.a

  INTERFACE
  SUBROUTINE MAKNDX (FILE_NAME,GTYPE,NXP,NYP,NZP,CLAT,CLON,DLAT,DLON,   &
                     RLAT,RLON,TLAT1,TLAT2,NUMSFC,NUMATM,LEVELS,        &
                     SFCVAR,ATMVAR,ATMARL,SFCARL)
  IMPLICIT NONE
  CHARACTER(80),INTENT(IN)   :: file_name     ! configuration file  
  CHARACTER(4), INTENT(IN)   :: gtype         ! grid identification
  INTEGER,      INTENT(IN)   :: nxp           ! x dimension
  INTEGER,      INTENT(IN)   :: nyp           ! y dimension
  INTEGER,      INTENT(IN)   :: nzp           ! z dimension
  REAL,         INTENT(IN)   :: clat,clon     ! lower left corner
  REAL,         INTENT(IN)   :: dlat,dlon     ! grid spacing 
  REAL,         INTENT(IN)   :: rlat,rlon     ! reference point
  REAL,         INTENT(IN)   :: tlat1,tlat2   ! tangent point   
  INTEGER,      INTENT(IN)   :: numsfc        ! numb sfc var in cfg
  INTEGER,      INTENT(IN)   :: numatm        ! numb atm var in cfg
  INTEGER,      INTENT(IN)   :: levels(:)     ! level value each atm
  INTEGER,      INTENT(IN)   :: sfcvar(:)     ! mark each var found 
  INTEGER,      INTENT(IN)   :: atmvar(:,:)   ! mark each var by level
  CHARACTER(4), INTENT(IN)   :: atmarl(:)     ! output character ID
  CHARACTER(4), INTENT(IN)   :: sfcarl(:)     ! output character ID 
  END SUBROUTINE MAKNDX

  SUBROUTINE PAKREC(LUNIT,RVAR,CVAR,NX,NY,NXY,KVAR,IY,IM,ID,IH,MN,IC,LL,KINI)
  IMPLICIT NONE
  INTEGER,      INTENT(IN)  :: LUNIT       ! output unit number
  INTEGER,      INTENT(IN)  :: NX,NY       ! dimensions of RVAR
  INTEGER,      INTENT(IN)  :: NXY         ! dimensions of CVAR
  REAL,         INTENT(IN)  :: RVAR(NX,NY) ! input data to be packed
  CHARACTER(1), INTENT(OUT) :: CVAR(NXY)   ! packed data array
  CHARACTER(4), INTENT(IN)  :: KVAR        ! descriptor of variable written
  INTEGER,      INTENT(IN)  :: IY,IM,ID    ! date identification
  INTEGER,      INTENT(IN)  :: IH,MN       ! time identification (MN-minutes)
  INTEGER,      INTENT(IN)  :: IC          ! forecast hour, ICX hour for >99
  INTEGER,      INTENT(IN)  :: LL          ! level indicator
  INTEGER,      INTENT(IN)  :: KINI        ! initialization (0-no 1-yes)
  END SUBROUTINE pakrec

  SUBROUTINE PAKSET(LUNIT,FNAME,KREC1,NXP,NYP,NZP)
  IMPLICIT NONE
  INTEGER,       INTENT(IN)    :: lunit     ! output unit number
  CHARACTER(*),  INTENT(INOUT) :: fname     ! file name of METDATA.CFG
  INTEGER,       INTENT(IN)    :: krec1     ! position of index record at time-1
  INTEGER,       INTENT(OUT)   :: nxp, nyp  ! horizontal grid dimensions
  INTEGER,       INTENT(OUT)   :: nzp       ! vertical grid dimension (incl sfc)
  END SUBROUTINE pakset
  END INTERFACE

!------------------------------------------------------------

  ! check for command line arguments
  NARG=IARGC()

  IF(NARG.EQ.0)THEN
     WRITE(*,*)'Usage: api2arl [-options]'
     WRITE(*,*)' -h[help information with extended discussion]'
     WRITE(*,*)' -e[encoding configuration file {name | create arldata.cfg}]'
     WRITE(*,*)' -d[decoding configuration file {name | create api2arl.cfg}]'
     WRITE(*,*)' -i[input grib data file name {DATA.GRIB2}]'
     WRITE(*,*)' -o[output data file name {DATA.ARL}]'
     WRITE(*,*)' -g[grid name (4 char) default = {center ID}]'
     WRITE(*,*)' -t[top pressure height (hPa) for processing {20}]'
     WRITE(*,*)' -z[zero fields (no=0 yes=1)initialization flag {1}]'
     STOP
  END IF

  apicfg_name='api2arl.cfg'
  arlcfg_name='arldata.cfg'
  grib_name='DATA.GRIB2'
  data_name='DATA.ARL'
  gtype=''
  zero=1
  top=20

  DO WHILE (NARG.GT.0)
     CALL GETARG(NARG,message)
     SELECT CASE (message(1:2))
     CASE ('-h','-H')
        CALL help 
        STOP
     CASE ('-d','-D')
        READ(message(3:),'(A)' )apicfg_name  
     CASE ('-e','-E')
        READ(message(3:),'(A)' )arlcfg_name  
     CASE ('-i','-I')
        READ(message(3:),'(A)' )grib_name    
     CASE ('-o','-O')
        READ(message(3:),'(A)' )data_name  
     CASE ('-g','-G')
        READ(message(3:),'(A)' )gtype      
     CASE ('-t','-T')
        READ(message(3:),'(I3)')top 
     CASE ('-z','-Z')
        READ(message(3:),'(I1)')zero
     END SELECT
     NARG=NARG-1
  END DO

  INQUIRE(FILE=TRIM(apicfg_name),EXIST=ftest)
  IF(.NOT.ftest)THEN
     apicfg_name='api2arl.cfg'
     CALL makapi(apicfg_name)
  ELSE
     WRITE(*,*)'Existing decoding configuration:',TRIM(apicfg_name)
  END IF
  OPEN (10,FILE=TRIM(apicfg_name))
  READ (10,SETUP)
  CLOSE(10)

!------------------------------------------------------------

  ! support multiple fields in a single message
  call grib_multi_support_on(iret)
  IF (iret.NE.grib_success) GOTO 900

  ! open the grib file
  call grib_open_file(ifile,TRIM(grib_name),'r',iret)
  IF (iret.NE.grib_success) GOTO 900

  ! count the messages in the file
  call grib_count_in_file(ifile,num_msg,iret)
  IF (iret.NE.grib_success) GOTO 900
  allocate(igrib (num_msg))
  allocate(msglev(num_msg))
  allocate(msgvar(num_msg))
  igrib =-1
  msglev=-1
  msgvar=-1

  ! Load the messages into memory from the file.
  DO i=1,num_msg
     call grib_new_from_file(ifile,igrib(i), iret)
     IF (iret.NE.grib_success) GOTO 900
  END DO

  ! close the file
  call grib_close_file(ifile,iret)
  IF (iret.NE.grib_success) GOTO 900

!------------------------------------------------------------
! Loop on all the messages in memory to establish the
! variable and level structure of the output data set
! according to the variables defined in the namelist

  levels=-1 ! height of each defined level
  sfcvar= 0 ! surface variable counter     
  atmvar= 0 ! atmospheric variable counter by level    

  DO i=1,num_msg
     call grib_get(igrib(i),'levelType',value) 

     ! surface variable (200 for TCLD)
     IF(trim(value).EQ.'sfc'.OR.trim(value).EQ.'200')THEN
        call grib_get(igrib(i),'shortName',value)
        kv=-1
        DO k=1,numsfc
           ! test restricted to the first four chars
           IF(trim(value(1:4)).EQ.sfcgrb(k))kv=k
        END DO

        IF(kv.NE.-1)THEN
           ! set level and 2D (sfc) variable indicies
           ! call grib_get(igrib(i),'level',levhgt)
           msglev(i)=0
           msgvar(i)=kv
           sfcvar(kv)=1  
        END IF

     ! atmospheric variable (currently pressure levels)
     ELSEIF(trim(value).EQ.'pl')THEN
        call grib_get(igrib(i),'shortName',value)
        kv=-1
        DO k=1,numatm
           ! define variable index
           IF(trim(value).EQ.atmgrb(k))kv=k
        END DO

        IF(kv.NE.-1)THEN
           call grib_get(igrib(i),'level',levhgt)

           IF(levhgt.GE.top)THEN
           IF(numlev.EQ.0)THEN
              ! save message number used to define grid
              grid_base=i
              ! first upper variable when numlev still equals zero
              numlev=numlev+1
              IF(numlev.GT.maxlev) THEN
                 WRITE(*,*)'Too many input levels, recompile with maxlev!'
                 STOP 
              END IF
              kl=numlev
              levels(kl)=levhgt

              ! define the remainder of the grid 
              call grib_get(igrib(i),'centre',value)
              IF(gtype.EQ.'') gtype=value(1:4)

              ! currently only handles regular lat-lon grids
              call grib_get(igrib(i),'gridType',value)
              rlat=0.0
              rlon=0.0

              IF(trim(value).EQ.'regular_ll')THEN
                 call grib_get(igrib(i),'latitudeOfFirstGridPointInDegrees', clat)
                 call grib_get(igrib(i),'longitudeOfFirstGridPointInDegrees',clon)
                 call grib_get(igrib(i), 'latitudeOfLastGridPointInDegrees', clat2) 
                 call grib_get(igrib(i), 'longitudeOfLastGridPointInDegrees',clon2)
                 call grib_get(igrib(i),'iDirectionIncrementInDegrees', dlon)
                 call grib_get(igrib(i),'jDirectionIncrementInDegrees', dlat) 
                 call grib_get(igrib(i),'numberOfPointsAlongAParallel', nxp)
                 call grib_get(igrib(i),'numberOfPointsAlongAMeridian', nyp)
                 ! check for data order (prefer south to north, otherwise invert)
                 IF(clat2.LT.clat)THEN
                    clat=clat2
                    invert=.true.
                 END IF
              ELSEIF(trim(value).EQ.'lambert')THEN
                 call grib_get(igrib(i),'latitudeOfFirstGridPointInDegrees', clat)
                 call grib_get(igrib(i),'longitudeOfFirstGridPointInDegrees',clon)
                 call grib_get(igrib(i), 'LaDInDegrees',rlat)
                 call grib_get(igrib(i), 'LoVInDegrees',rlon)
                 call grib_get(igrib(i),'DxInMetres', dlon)
                 call grib_get(igrib(i),'DyInMetres', dlat)
                 call grib_get(igrib(i),'Ni', nxp)
                 call grib_get(igrib(i),'Nj', nyp)
                 call grib_get(igrib(i), 'Latin1InDegrees',tlat1)
                 call grib_get(igrib(i), 'Latin2InDegrees',tlat2)
              ELSE
                 WRITE(*,*)'Current version only handles Lambert or Lat-Lon grids!'
                 WRITE(*,*)'This grid: ',trim(value)
                 STOP 
              END IF


           ELSE
              ! find match to existing level height
              kl=-1
              DO k=1,numlev
                 IF(levels(k).EQ.levhgt)kl=k
              END DO
              ! define a new level              
              IF(kl.EQ.-1)THEN
                 numlev=numlev+1
                 IF(numlev.GT.maxlev) THEN
                    WRITE(*,*)'Too many input levels, recompile with maxlev!'
                    STOP 
                 END IF
                 kl=numlev
                 levels(kl)=levhgt
              END IF

           END IF

           ! set level and 3D variable indicies 
           msglev(i)=kl
           msgvar(i)=kv
           atmvar(kv,kl)=1      
           END IF
        END IF
     END IF
  END DO 

!------------------------------------------------------------
! create HYSPLIT packing configuration file

  ! insure that level one is at the bottom 
  IF(levels(1).LT.levels(numlev))THEN
     DO k=1,numlev
        wrkvar(1,k)=levels(numlev+1-k)
     END DO
     levels=wrkvar(1,:)

     DO k=1,numlev
        wrkvar(:,k)=atmvar(:,numlev+1-k)
     END DO
     atmvar=wrkvar

     DO i=1,num_msg
        k=msglev(i)
        IF(k.NE.0) msglev(i)=numlev+1-k
     END DO  
  END IF

  ! Creates the ARL packing configuration file if it doesn't exist.
  ! An old file may be used if the current file does not contain all
  ! the fields (e.g. diagnostic) and those records are to be filled
  ! during another pass through the program with a different input file.
  ! Note that all variables passed to the packing routines must be 
  ! defined in this file, because it determines the record structure
  ! of the output. However, not all defined variables need to be 
  ! provided during any one pass through the program. 

  INQUIRE(FILE=TRIM(arlcfg_name),EXIST=ftest)
  IF(.NOT.ftest)THEN
     CALL MAKNDX (ARLCFG_NAME,GTYPE,NXP,NYP,NUMLEV,CLAT,CLON,DLAT,DLON,    &
                  RLAT,RLON,TLAT1,TLAT2,NUMSFC,NUMATM,LEVELS,              &
                  SFCVAR,ATMVAR,ATMARL,SFCARL)
  ELSE
     WRITE(*,*)'Existing encoding configuration:',TRIM(arlcfg_name)
  END IF

! initialize the packing routine common block and open the output file
  CALL PAKSET(20,ARLCFG_NAME,1,NXP,NYP,NZP)
  OPEN(20,FILE=TRIM(data_name),RECL=(50+NXP*NYP),ACCESS='DIRECT',  &
       FORM='UNFORMATTED')

!------------------------------------------------------------
! LOOP through all messages reading selected variables and
! writing data to the packed HYSPLIT output format

  ! get the size of the values array
  call grib_get_size(igrib(grid_base),'values',numberOfValues)
  allocate(values(numberOfValues), stat=iret)
  allocate( cvar (numberOfValues), stat=iret)

  IF(numberOfValues.NE.nxp*nyp)THEN
     WRITE(*,*)'Inconsistent 1D and 2D array size!'
     WRITE(*,*)'1D array: ',numberOfValues
     WRITE(*,*)'2D array: ',nxp,nyp
     STOP
  END IF
  allocate(rvalue(nxp,nyp), stat=iret)

  DO i=1,num_msg
     IF(msgvar(i).LT.0)CYCLE

     ! define the variable string by the variable and level
     ! index values saved for each message number
     kl=msglev(i)
     kv=msgvar(i)
     IF(kl.EQ.0)THEN 
        gtype=sfcarl(kv)
        units=sfccnv(kv)
     ELSE
        gtype=atmarl(kv)
        units=atmcnv(kv)
     END IF

     ! determine the forecast hour, for accumulated 
     ! variables use the maximum time 
     call grib_get(igrib(i),'forecastTime',ifh)
     call grib_get(igrib(i),'startStep',if1)
     call grib_get(igrib(i),'endStep',if2)
     ifh=MAX(ifh,if1,if2)

     ! set the date variables to correspond with the valid time
     call grib_get(igrib(i),'validityDate',value)
     READ(value,'(2X,3I2)') iyr,imo,ida
     call grib_get(igrib(i),'validityTime',imn)
     ihr=imn/100
     imn=imn-ihr*100

     ! get data values in a one dimensional array
     call grib_get(igrib(i),'values',values)
  
     ! place data into two dimensional array insuring that the 
     ! J(Y) index increases with latitude (S to N)
     ! input GRIB data from N to S when invert is true
     k=0
     DO j=1,nyp
        n=j
        IF(invert)n=nyp+1-j
     DO m=1,nxp
        k=k+1
        rvalue(m,n)=values(k)*units 
     END DO
     END DO 

!    write the data record to the output file
     CALL PAKREC(20,RVALUE,CVAR,NXP,NYP,(NXP*NYP),GTYPE,     &
                 IYR,IMO,IDA,IHR,IMN,IFH,(KL+1),ZERO)
  END DO

! complete the output by writing the index record 
  CALL PAKNDX(20)
  WRITE(*,*)'Finished: ',IYR,IMO,IDA,IHR

  DO i=1,num_msg
    call grib_release(igrib(i))
  END DO
  CLOSE(20)

! deallocate(values)
  deallocate(cvar)
  deallocate(igrib)
  deallocate(values)
  deallocate(rvalue)
  STOP

  900 CONTINUE
  CALL grib_get_error_string(iret,message)
  WRITE(*,*) message
  STOP 900

END PROGRAM api2arl  


!-------------------------------------------------------------
! Create the configuration file for HYSPLIT library
! packing subroutines                                 

SUBROUTINE MAKNDX (FILE_NAME,GTYPE,NXP,NYP,NZP,CLAT,CLON,DLAT,DLON,   &
                   RLAT,RLON,TLAT1,TLAT2,NUMSFC,NUMATM,LEVELS,        &
                   SFCVAR,ATMVAR,ATMARL,SFCARL)
  IMPLICIT NONE

  CHARACTER(80),INTENT(IN)   :: file_name     ! configuration file  
  CHARACTER(4), INTENT(IN)   :: gtype         ! grid identification
  INTEGER,      INTENT(IN)   :: nxp           ! x dimension
  INTEGER,      INTENT(IN)   :: nyp           ! y dimension
  INTEGER,      INTENT(IN)   :: nzp           ! y dimension
  REAL,         INTENT(IN)   :: clat,clon     ! lower left corner
  REAL,         INTENT(IN)   :: dlat,dlon     ! grid spacing 
  REAL,         INTENT(IN)   :: rlat,rlon     ! reference point
  REAL,         INTENT(IN)   :: tlat1,tlat2   ! tangent point   
  INTEGER,      INTENT(IN)   :: numsfc        ! numb sfc var in cfg
  INTEGER,      INTENT(IN)   :: numatm        ! numb atm var in cfg
  INTEGER,      INTENT(IN)   :: levels(:)     ! level value each atm
  INTEGER,      INTENT(IN)   :: sfcvar(:)     ! mark each var found 
  INTEGER,      INTENT(IN)   :: atmvar(:,:)   ! mark each var by level
  CHARACTER(4), INTENT(IN)   :: atmarl(:)     ! output character ID
  CHARACTER(4), INTENT(IN)   :: sfcarl(:)     ! output character ID 

  CHARACTER(4)  :: VCHAR(50) ! variable id
  CHARACTER(20) :: LABEL(18) ! optional field label

  INTEGER       :: N,NL,MVAR  
  REAL          :: SIG
  REAL          :: GRIDS(12)

! optional field label string
  DATA LABEL/'Model Type:','Grid Numb:','Vert Coord:','Pole Lat:',      &
    'Pole Lon:','Ref Lat:','Ref Lon:','Grid Size:','Orientation:',      &
    'Cone Angle:','Sync X Pt:','Sync Y Pt:','Sync Lat:','Sync Lon:',    &
    'Reserved:','Numb X pt:','Numb Y pt:','Numb Levels:'/

! Set lat/lon of lower left point
  GRIDS(10)= CLAT
  GRIDS(11)= CLON

! grid should be defined on a 0->360 coordinate
  IF(GRIDS(11).LT.0.0)GRIDS(11)=360.0+GRIDS(11)

  IF(RLAT.EQ.0.0.AND.RLON.EQ.0.0)THEN
!    defines a regular lat-lon grid
!    Pole lat/lon is used to identify the latlon point of the max index
     GRIDS(1)=GRIDS(10)+DLAT*(NYP-1)
     GRIDS(2)=GRIDS(11)+DLON*(NXP-1)
     GRIDS(2)=AMOD(GRIDS(2),360.0)

     GRIDS(3)=DLAT ! ref lat defines grid spacing
     GRIDS(4)=DLON ! ref lon defines grid spacing

     GRIDS(5)=0.0  ! grid size zero for lat/lom
     GRIDS(6)=0.0  ! orientation
     GRIDS(7)=0.0  ! tangent latitude

  ELSE
!    defines a lmmbert conformal grid
     GRIDS(1)=90.0  ! pole position   
     GRIDS(2)=0.0                               

     GRIDS(3)=RLAT  ! resolution defined at this latitude 
     GRIDS(4)=RLON  ! ref lon defines grid spacing

     GRIDS(5)=SQRT(DLAT*DLON)/1000.0 ! grid size km
     GRIDS(6)=0.0   ! grid orientation
     GRIDS(7)=TLAT1 ! tangent latitude
  END IF

! sync x,y defines lower left grid point 
  GRIDS(8)=1.0 
  GRIDS(9)=1.0

! variable reserved for future use
  GRIDS(12)=0.0  

! write the packer configuration file
  OPEN(30,FILE=FILE_NAME)

! default grid number 99 (field not used)
  WRITE(30,'(A20,A4)')LABEL(1),GTYPE 
  WRITE(30,'(A20,A4)')LABEL(2),'  99'

! coordinate (1:sigma 2:pressure 3:terrain 4:hybrid)
  WRITE(30,'(A20,I4)') LABEL(3), 2        

! grid geolocation parameters and projection
  DO N=1,12
     WRITE(30,'(A20,F10.2)')LABEL(N+3),GRIDS(N)
  END DO

! grid dimensions
  WRITE(30,'(A20,I4)') LABEL(16), NXP
  WRITE(30,'(A20,I4)') LABEL(17), NYP
  WRITE(30,'(A20,I4)') LABEL(18), NZP+1

! upper level information
  DO nl=0,nzp   

     WRITE(LABEL(1),'(A6,I2,A1)')'Level ',NL,':'

     IF(NL.EQ.0)THEN
        SIG=0.0     
        MVAR=0

        DO n=1,numsfc
           IF(sfcvar(n).EQ.1)THEN
              MVAR=MVAR+1
              VCHAR(MVAR)=sfcarl(n)  
           END IF
        END DO

     ELSE
        SIG=LEVELS(NL)
        MVAR=0

        DO n=1,numatm 
           IF(atmvar(n,nl).EQ.1)THEN
              MVAR=MVAR+1
              VCHAR(MVAR)=atmarl(n)  
           END IF
        END DO
     END IF

     IF(SIG.LT.1.0)THEN
        WRITE(30,'(A20,F6.5,I3,99(1X,A4))')LABEL(1),SIG,MVAR,(VCHAR(N),N=1,MVAR)
     ELSEIF(SIG.GE.1.AND.SIG.LT.10.0)THEN
        WRITE(30,'(A20,F6.4,I3,99(1X,A4))')LABEL(1),SIG,MVAR,(VCHAR(N),N=1,MVAR)
     ELSEIF(SIG.GE.10.AND.SIG.LT.100.0)THEN
        WRITE(30,'(A20,F6.3,I3,99(1X,A4))')LABEL(1),SIG,MVAR,(VCHAR(N),N=1,MVAR)
     ELSEIF(SIG.GE.100.AND.SIG.LT.1000.0)THEN
        WRITE(30,'(A20,F6.2,I3,99(1X,A4))')LABEL(1),SIG,MVAR,(VCHAR(N),N=1,MVAR)
     ELSEIF(SIG.GE.1000)THEN
        WRITE(30,'(A20,F6.1,I3,99(1X,A4))')LABEL(1),SIG,MVAR,(VCHAR(N),N=1,MVAR)
     END IF

  END DO
  CLOSE (30) 

END SUBROUTINE makndx

!-------------------------------------------------------------
! Create the GRIB_API configuration file if the file 
! does not already exist.Use only the first four characters
! of the GRIB shortName! 

SUBROUTINE makapi(apicfg_name)

  implicit none
  character(len=1)   :: a,c 
  character(len=3)   :: d 
  character(len=80)  :: apicfg_name ! define grib variables

  a = CHAR(39) ! apostrophe 
  c = CHAR(44) ! comma   
  d(1:1) = a  
  d(2:2) = c
  d(3:3) = a

  open(30,file=TRIM(apicfg_name))
  write(30,'(a)')'&SETUP'
  write(30,'(a)')' numatm = 6,'
  write(30,'(a)')' atmgrb = '//a//'gh'//d//'t'//d//'u'//d//'v'//d//'w'//d//'r'//a//c
  write(30,'(a)')' atmcnv =   1.0 ,  1.0 ,  1.0 ,  1.0 , 0.01 ,  1.0 ,'
  write(30,'(a)')' atmarl = '//a//'HGTS'//d//'TEMP'//d//'UWND'//d//'VWND'//d//'WWND'//d//'RELH'//a//c
  write(30,'(a)')' numsfc = 14,'
  write(30,'(a)')' sfcgrb = '//a//'orog'//d//'sp'//d//'tp'//d//'tcc'//d//'hpbl'//d//'uflx'//d//'vflx'  &
                             //d//'dswr'//d//'shtf'//d//'lhtf'//d//'2t'//d//'r'//d//'10u'//d//'10v'//a//c
  write(30,'(a)')' sfccnv = 1.0, 0.01,0.001, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,' 
  write(30,'(a)')' sfcarl = '//a//'SHGT'//d//'PRSS'//d//'TPP6'//d//'TCLD'//d//'PBLH'//d//'UMOF'//d//'VMOF &
                            '//d//'DSWF'//d//'SHTF'//d//'LHTF'//d//'T02M'//d//'RH2M'//d//'U10M'//d//'V10M'//a//c
  write(30,'(a)')'/'
  close(30)

END SUBROUTINE makapi

!-------------------------------------------------------------
! Extended HELP discussion                           

SUBROUTINE help

  WRITE(*,*)'Usage: api2arl [-options]'
  WRITE(*,*)' -h[help information with extended discussion]'
  WRITE(*,*)' -d[decoding configuration file {name | create api2arl.cfg}]'
  WRITE(*,*)' -e[encoding configuration file {name | create arldata.cfg}]'
  WRITE(*,*)' -i[input grib data file name {DATA.GRIB2}]'
  WRITE(*,*)' -o[output data file name {DATA.ARL}]'
  WRITE(*,*)' -g[grid name (4 char) default = {center ID}]'
  WRITE(*,*)' -t[top pressure height (hPa) for processing {20}]'
  WRITE(*,*)' -z[zero fields (no=0 yes=1)initialization flag {1}]'
  WRITE(*,*)' '
  WRITE(*,*)'The API2ARL program converts model output data in GRIB2 format'
  WRITE(*,*)'to the ARL packed format required for input to HYSPLIT. The GRIB'
  WRITE(*,*)'data must be a global latitude-longitude or regional Lambert grid'
  WRITE(*,*)'defined on pressure surfaces. The program will only convert one'
  WRITE(*,*)'time period in any one input file. Multiple time period output'
  WRITE(*,*)'files can be created by appending each time period processed using'
  WRITE(*,*)'the cat command (e.g. cat DATA.ARL_t2 >> DATA.ARL_t1).'
  WRITE(*,*)' '
  WRITE(*,*)'The GRIB conversion is defined by the decoding configuration file'
  WRITE(*,*)'which defines the relationship between the grib file variable names,' 
  WRITE(*,*)'the ARL packed format variable names, and the units conversion factors.'
  WRITE(*,*)'The default name for this file is api2arl.cfg and if it does not exist,' 
  WRITE(*,*)'the file will be created. It can be subsequently edited to define a'
  WRITE(*,*)'customized set of variables. Each user must know which variables are' 
  WRITE(*,*)'available in the GRIB input file. The current default configuration'
  WRITE(*,*)'defines the following GRIB variables:'
  WRITE(*,*)'levelType  level shortName'
  WRITE(*,*)'pl         {hPa} gh t u v r w'
  WRITE(*,*)'sfc        0     sp orog tp lhtfl shtfl uflx vflx dswrf hpbl tcc'
  WRITE(*,*)'sfc        2     2t r'
  WRITE(*,*)' '
  WRITE(*,*)'The GRIB messages are scanned and an encoding configuration file is'
  WRITE(*,*)'created (arldata.cfg) that defines the record structure for packing'
  WRITE(*,*)'into the ARL format. Variables defined in api2arl.cfg that are not' 
  WRITE(*,*)'found in the GRIB file will not be defined in arldata.cfg. An old file'
  WRITE(*,*)'may be used if the current configuration does not contain all the fields' 
  WRITE(*,*)'(e.g. fluxes) and those records are to be filled during another pass' 
  WRITE(*,*)'through the program with a different GRIB file. Note that all variables'
  WRITE(*,*)'passed to the packing routines must be defined in this file, but not all' 
  WRITE(*,*)'defined variables have to be written during any one pass.'  
  WRITE(*,*)' '
  WRITE(*,*)'Two pass processing is accomplished by turning off the initialization'
  WRITE(*,*)'flag (-z0) during the second pass. In this way missing fields are not'
  WRITE(*,*)'written into an existing output file (DATA.ARL). The example below shows'
  WRITE(*,*)'how this process can be scripted assuming a new forecast is available'
  WRITE(*,*)'every six hours and flux fields are not available at the initial time.'
  WRITE(*,*)' '
  WRITE(*,*)'rm -f DATA.ARL ARLDATA.BIN arldata.cfg api2arl.cfg' 
  WRITE(*,*)'./api2arl -z1 -iCYCLE06Z_FCST06.GRIB'
  WRITE(*,*)'./api2arl -z0 -iCYCLE12Z_FCST00.GRIB'
  WRITE(*,*)'mv DATA.ARL ARLDATA.BIN'
  WRITE(*,*)'for HH in 03 06 09 12; do'
  WRITE(*,*)'./api2arl -z1 -iCYCLE12Z_FCST${HH}.GRIB'
  WRITE(*,*)'cat DATA.ARL >>ARLDATA.BIN'
  WRITE(*,*)'rm -f DATA.ARL'
  WRITE(*,*)'done'

END SUBROUTINE help
