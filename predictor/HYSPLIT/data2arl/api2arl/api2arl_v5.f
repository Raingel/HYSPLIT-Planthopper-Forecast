!-------------------------------------------------------------
!  Description: create ARL files from grib2 using eccodes library.
!
!  Author: Alice Crawford
!
!-------------------------------------------------------------
! API2ARL_v5 - Converts GRIB2 messages using ECMWF eccodes library
! routines to HYSPLIT packed (ARL) data format. 
! 
! Initially created for some basic DWD ICON data which has sigma levels.

!-------------------------------------------------------------
! Last Revised:                   - modified from api2arl_v4 code for ICON
!               3  Oct 2022 (AMC) - added difw field.
!-------------------------------------------------------------

PROGRAM api2arl 
  use eccodes
  use arlmod

  implicit none
  INCLUDE 'pakset.inc'

  integer                            ::  iargc,narg
  logical                            ::  ftest
  logical                            ::  udif = .false. ! use the diff fields
  logical                            ::  invert = .false.
  integer                            ::  ifile
  integer                            ::  is_missing, iret
  integer                            ::  num_msg
  integer                            ::  i,j,k,l,m,n,g
  integer                            ::  kl,kv
  integer                            ::  grid_base
  integer,dimension(:),allocatable   ::  igrib

  real                               ::  sigmav
  real                               ::  sfcavg,lvlavg
  real                               ::  units  
  type(agrid)                        ::  metgrid
  integer                            ::  nxp,nyp,nzp
  integer                            ::  pcat,pnum    
  real, dimension(:),   allocatable  ::  values
  real, dimension(:,:), allocatable  ::  rvalue
  real, dimension(:,:), allocatable  ::  var2d  ! for computing dif fields
  character(len=1),     allocatable  ::  cvar(:) 
  integer                            ::  numberOfValues
  integer                            ::  numberOfLevels

  character(len=4)   :: model       ! meteorological model
  character(len=4)   :: param       ! parameter name
  character(len=8)   :: ltype       ! level type
  character(len=80)  :: message             
  character(len=80)  :: apicfg_name ! define grib variables
  character(len=80)  :: arlcfg_name ! define arl strucure 
  character(len=80)  :: grib_name   ! grib input file name
  character(len=80)  :: data_name   ! arl output data file
  character(len=256) :: value = 'not_set'

!  integer            :: iyr,imo,ida,imn,ifh,if1,if2
  integer            :: sigma,zero,top,ptop,ktop

  integer, parameter                 :: kunit = 50  ! log file unit
  integer, parameter                 :: lunit = 60  ! ARL file unit

!  integer, parameter                 :: maxvar = 25
!  integer, parameter                 :: maxlev = 100
  integer                            :: tnum = 0  ! number of time periods in grib file(s)
  integer                            :: numlev = 0
  integer                            :: numavg = 0
  integer                            :: levhgt
  integer, dimension(maxlev)         :: levels
  integer, dimension(:),allocatable  :: msglev  ! level of each grib message
  integer, dimension(:),allocatable  :: msgvar  ! variable index of each grib message
  real,    dimension(maxlev)         :: sigmaz  ! needed for hybrid levels.

  integer                            :: numsfc, numatm
  integer         ,dimension(maxvar) :: atmcat, sfccat ! grib parameterCategory
  integer         ,dimension(maxvar) :: atmnum, sfcnum ! grib ParameterNumber
  real            ,dimension(maxvar) :: atmcnv, sfccnv ! conversion factor
  character(len=4),dimension(maxvar) :: atmgrb, sfcgrb ! grib shortName
  character(len=4),dimension(maxvar) :: atmarl, sfcarl ! ARL 4character ID

  integer, dimension(maxvar)         :: sfcvar  ! 1 if exits in file. 0 otherwise
  integer, dimension(maxvar,maxlev)  :: atmvar  ! 1 if exists in file. 0 otherwise
  real, dimension(maxvar,maxlev)  :: wrkvar 


  NAMELIST/SETUP/ numatm, atmgrb, atmcnv, atmarl,   &
                  numsfc, sfcgrb, sfccnv, sfcarl,   &
                  atmcat, atmnum, sfccat, sfcnum   


!------------------------------------------------------------
! Interface to ARL packing routines found in the HYSPLIT
! library: ./hysplit/library/libhysplit.a

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
     WRITE(*,*)' -g[model grid name (4 char) default = {center ID}]'
     WRITE(*,*)' -s[sigma compute=1 or read=2 or ignore=(0)]'
     WRITE(*,*)' -t[top pressure (hPa) or number {50} of levels from bottom]'
     WRITE(*,*)' -a[averge vertical velocity no=(0) or yes=numpts radius]'
     WRITE(*,*)' -z[zero fields (no=0 yes=1)initialization flag {1}]'
     STOP
  END IF

! Set defaults
  apicfg_name='api2arl.cfg'
  arlcfg_name='arldata.cfg'
  grib_name='DATA.GRIB2'
  data_name='DATA.ARL'
  model=''
  sigma=0
  zero=1
  top=50


! Read command line inputs
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
        READ(message(3:),'(A)' )model      
     CASE ('-s','-S')
        READ(message(3:),'(I1)')sigma
     CASE ('-t','-T')
        READ(message(3:),'(I3)')top 
     CASE ('-a','-A')
        READ(message(3:),'(I1)')numavg
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

! add diff fields
  if(udif)then
     numatm = numatm+1
     atmarl(numatm) = 'DIFW' 
  end if

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
  write(*,*) 'Getting structure'
  call gtstrct(num_msg, igrib, udif, &
               numsfc, sfccat, sfcnum, sfcgrb, sfcarl, sfcvar, &
               numatm, atmcat, atmnum, atmgrb, atmarl, atmvar, &
               metgrid, model, invert, grid_base, &
               msglev, msgvar, ltype, levels,numlev,tnum)
!------------------------------------------------------------
  
  IF(ltype.eq.'hl')THEN
      write(*,*) 'Getting sigmaz'
      CALL GETSIG(numlev, sigmaz)
      sigma=2
  END IF 
!------------------------------------------------------------
! This block can be used if need to be re-ordered.

! create HYSPLIT packing configuration file
  ! insure that pressure level one is at the bottom 
!  IF(trim(ltype).EQ.'pl'.AND.levels(1).LT.levels(numlev))THEN 
!  IF(levels(1).LT.levels(numlev))THEN 

  !write(*,*) sigmaz(1), sigmaz(2), sigmaz(3)
  !DO k=1,numlev
  !  wrkvar(1,k)=sigmaz(numlev+1-k)
  !END DO
  !sigmaz=wrkvar(1,:)
  !write(*,*) sigmaz(numlev), sigmaz(numlev-1), sigmaz(numlev-2)
  !write(*,*) '0000000000000000000000000'

  IF(ltype.eq.'none')THEN 
     write(*,*) 'Reordering levels'
!    disable this section for gdas data
    DO k=1,numlev
       wrkvar(1,k)=sigmaz(numlev+1-k)
    END DO
    sigmaz=wrkvar(1,:)

    DO k=1,numlev
       wrkvar(1,k)=levels(numlev+1-k)
    END DO
    levels=wrkvar(1,:)

    DO k=1,numlev
       wrkvar(:,k)=atmvar(:,numlev+1-k)
    END DO
    atmvar=wrkvar
!
    DO i=1,num_msg
       k=msglev(i)
       IF(k.NE.0) msglev(i)=numlev+1-k
    END DO  
  END IF


  ! Creates the ARL packing configuration file 
  ! other versions allow to use a pre-computed one.
  ftest=.FALSE.
!  INQUIRE(FILE=TRIM(arlcfg_name),EXIST=ftest)

  IF(.NOT.ftest)THEN
     CALL MAKNDX (ARLCFG_NAME,NUMLEV,METGRID,  &
                  NUMSFC,NUMATM,LEVELS,SIGMAZ, &
                  SFCVAR,ATMVAR,ATMARL,SFCARL, &
                  INVERT,ltype)
  ELSE
     WRITE(*,*)'Existing encoding configuration:',TRIM(arlcfg_name)
  END IF

! initialize the packing routine common block and open the output file
! pakset will read the arldata.cfg file.
  CALL PAKSET(punit,ARLCFG_NAME,1,NXP,NYP,NZP)
  OPEN(punit,FILE=TRIM(data_name),RECL=(50+NXP*NYP),ACCESS='DIRECT',  &
       FORM='UNFORMATTED')

!------------------------------------------------------------
! Allocate arrays for holding the data from the grib file.

  call codes_is_missing(igrib(grid_base),'numberOfValues',is_missing)
  IF(is_missing.eq.1)THEN
     write(*,*) 'NO numberOfValues  in grib file'
     ! try using get_size to get the numberOfValues instead
     call grib_get_size(igrib(grid_base),'values',numberOfValues,iret)
  ELSE                 
     call grib_get(igrib(grid_base),'numberOfValues', numberOfValues)
  END IF

  allocate(values(numberOfValues), stat=iret)
  allocate(cvar(numberOfValues), stat=iret)

  IF(numberOfValues.NE.nxp*nyp)THEN
     WRITE(*,*)'Inconsistent 1D and 2D array size!'
     WRITE(*,*)'1D array: ',numberOfValues
     WRITE(*,*)'2D array: ',nxp,nyp
     STOP
  END IF
  allocate(rvalue(nxp,nyp), var2d(nxp,nyp),  stat=iret)

!------------------------------------------------------------

!------------------------------------------------------------
! LOOP through all messages reading selected variables and
! writing data to the packed HYSPLIT output format
! 
  DO i=1,num_msg
     ! check if message holds data we want to extract.
     IF(msgvar(i).LT.0)CYCLE
     ! define the variable string by the variable and level
     ! index values saved for each message number
     kl=msglev(i)
     kv=msgvar(i)
     IF(kl.EQ.0)THEN 
        param=sfcarl(kv)
        units=sfccnv(kv)
     ELSE
        param=atmarl(kv)
        units=atmcnv(kv)
     END IF
     call pakm(igrib(i),param,units,kl,nxp,nyp,cvar, &
          numlev,values,rvalue,var2d,invert,zero,udif)
  END DO

! complete the output by writing the index record 
  CALL PAKNDX(punit)

  DO i=1,num_msg
    call grib_release(igrib(i))
  END DO
  CLOSE(punit)

  IF(sigma.GT.0)THEN
     WRITE(*,*)'Computed equivalent sigma levels in sigma.txt'
     CLOSE(90)
  END IF

  deallocate(cvar)
  deallocate(igrib)
  deallocate(values)
  deallocate(rvalue)
  deallocate(msglev)
  deallocate(msgvar)
  STOP

  900 CONTINUE
  CALL grib_get_error_string(iret,message)
  WRITE(*,*) message
  STOP 900

END PROGRAM api2arl  


!-------------------------------------------------------------
! get the grid information from the grib message
!                                  
SUBROUTINE help

  WRITE(*,*)'Usage: api2arl [-options]'
  WRITE(*,*)' -h[help information with extended discussion]'
  WRITE(*,*)' -d[decoding configuration file {name | create api2arl.cfg}]'
  WRITE(*,*)' -e[encoding configuration file {name | create arldata.cfg}]'
  WRITE(*,*)' -i[input grib data file name {DATA.GRIB2}]'
  WRITE(*,*)' -o[output data file name {DATA.ARL}]'
  WRITE(*,*)' -g[grid name (4 char) default = {center ID}]'
  WRITE(*,*)' -s[sigma compute=1 or read=2 or ignore=(0)]'
  WRITE(*,*)' -t[top pressure (hPa) or level number for processing {20}]'
  WRITE(*,*)' -a[averge vertical velocity no=(0) or yes=numpts radius]'
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

END SUBROUTINE help
