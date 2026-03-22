!-------------------------------------------------------------
!  Author: Alice Crawford
!
!
!-------------------------------------------------------------
! ARLMOD - 
! subroutines to converts GRIB2 messages using ECMWF eccodes library
! routines to HYSPLIT packed (ARL) data format. 

! GETGRD
! MAKNDX
! GETSIG
! MATCH 
! PAKM
! GTSTRCT 
! makapi

!-------------------------------------------------------------
! Last Revised: 24 Sep 2022       - 
!-------------------------------------------------------------
MODULE arlmod
   implicit none

   integer, parameter :: maxvar = 25
   integer, parameter :: maxlev = 100
   integer, parameter :: punit = 20    ! for accessing ARL file
   
   TYPE AGRID
        real :: rlat, rlon
        real :: dlat, dlon
        real :: clat, clon
        real :: clat2, clon2
        real :: tlat1, tlat2
        integer :: nxp, nyp
        character(len=4) :: model 
   END TYPE

CONTAINS

   !-------------------------------------------------------------
   ! get the grid information from the grib message
   !-------------------------------------------------------------
   SUBROUTINE GETGRD(igrib,metgrid,model,invert) 
     use eccodes
     IMPLICIT NONE

     INTEGER,      INTENT(IN)           :: igrib   ! grib message
     TYPE(AGRID),  INTENT(OUT)          :: metgrid
     character(len=4), INTENT(OUT)      :: model   ! meteorological model
     LOGICAL, INTENT(OUT)               :: invert

     ! local variables
     character(len=80)                  :: project ! mapping projection
     character(len=256)                 :: center  ! mapping projection
     real                               :: eqvlat 

     call grib_get(igrib,'centre',center)
     metgrid%model=model
     IF(model.EQ.'') metgrid%model=center(1:4)

     ! grid projection information  
     call grib_get(igrib,'gridType',project)
     metgrid%rlat=0.0
     metgrid%rlon=0.0

     IF(trim(project).EQ.'regular_ll')THEN
        call grib_get(igrib,'latitudeOfFirstGridPointInDegrees', metgrid%clat)
        call grib_get(igrib,'longitudeOfFirstGridPointInDegrees',metgrid%clon)
        call grib_get(igrib, 'latitudeOfLastGridPointInDegrees', metgrid%clat2) 
        call grib_get(igrib, 'longitudeOfLastGridPointInDegrees',metgrid%clon2)
        call grib_get(igrib,'iDirectionIncrementInDegrees', metgrid%dlon)
        call grib_get(igrib,'jDirectionIncrementInDegrees', metgrid%dlat) 
        call grib_get(igrib,'numberOfPointsAlongAParallel', metgrid%nxp)
        call grib_get(igrib,'numberOfPointsAlongAMeridian', metgrid%nyp)
        ! check for data order (prefer south to north, otherwise invert)
        IF(metgrid%clat2.LT.metgrid%clat)THEN
           metgrid%clat=metgrid%clat2
           invert=.true.
        END IF

     ELSEIF(trim(project).EQ.'lambert')THEN
        call grib_get(igrib,'latitudeOfFirstGridPointInDegrees', metgrid%clat)
        call grib_get(igrib,'longitudeOfFirstGridPointInDegrees',metgrid%clon)
        call grib_get(igrib, 'LaDInDegrees',metgrid%rlat)
        call grib_get(igrib, 'LoVInDegrees',metgrid%rlon)
        call grib_get(igrib,'DxInMetres', metgrid%dlon)
        call grib_get(igrib,'DyInMetres', metgrid%dlat)
        call grib_get(igrib,'Ni', metgrid%nxp)
        call grib_get(igrib,'Nj', metgrid%nyp)
        call grib_get(igrib, 'Latin1InDegrees',metgrid%tlat1)
        call grib_get(igrib, 'Latin2InDegrees',metgrid%tlat2)
        ! check for data order (prefer south to north, otherwise invert)
        IF(metgrid%tlat2.LT.metgrid%tlat1)THEN
           invert=.true.
        END IF
        metgrid%TLAT1=EQVLAT(metgrid%tlat1,metgrid%tlat2)

     ELSE
        WRITE(*,*)'Current version only handles Lambert or Lat-Lon grids!'
        WRITE(*,*)'This grid: ',trim(center)
        STOP 
     END IF
   END SUBROUTINE GETGRD

   !-------------------------------------------------------------
   ! getting information for the hybrid model levels.
   !-------------------------------------------------------------

   SUBROUTINE GETSIG (numlev,sigmaz)
     INTEGER,      INTENT(IN)    :: numlev 
     REAL,         INTENT(OUT)   :: sigmaz(:)     ! sigma at each level  

     integer                     :: k
     real                        :: dum1, dum2
     logical                     ::  ftest

   ! getting information for the hybrid model levels.
   ! pressure will be computed in prfecm.f using the following
   ! a = int(psg)
   ! b = real(a) - psg
   ! pressure = a + b * sp 
   ! ps = surface pressure 
   ! the third column in the sigma.txt needs to be
   ! int(a) + b
   ! this will become the psg variable used by prfecm.f

   ! The a(n) and b(n) are usually for computing pressure a half
   ! model level above and below the model level.
   ! Thus pressure at the model level is actually 
   ! p(n) = 0.5 * (a(n) + b(n)sp + a(n-1)+b(n-1)sp)
   ! An example following the ERA5 with 137 model levels
   ! a(137)=0 b(137)=1.0 meaning this half level is at the surface.
   ! a(136)=0 b(136) = 0.997630
   ! p(137) = 0.5 * (b(137)+b(136)) sp 
   ! only the third column is utilized here.
   ! Read in data about hybrid levels
     INQUIRE(FILE=TRIM('sigma.txt'),EXIST=ftest)
     IF(.NOT.ftest)THEN
        write(*,*) 'sigma.txt not found. required for hybrid levels'
     END IF
     OPEN(90,FILE='sigma.txt')
     DO k=1,numlev
        READ(90,*) dum1, dum2, sigmaz(k)
     END DO
     CLOSE(90)

   END SUBROUTINE GETSIG



   !------------------------------------------------------------------
   ! Find if data in grib message matches that specified by cfg file.
   !------------------------------------------------------------------

   SUBROUTINE MATCH (num,tmpcat,tmpnum,tmpgrb,igrib,kv,verbose)
     use eccodes
     IMPLICIT NONE
     INTEGER,      INTENT(IN)      :: tmpcat(:)     ! for matching parameter Category 
     INTEGER,      INTENT(IN)      :: tmpnum(:)     ! for matching parameter Number 
     CHARACTER(4), INTENT(IN)      :: tmpgrb(:)     ! for matching shortName
     INTEGER,      INTENT(IN)      :: igrib         ! grib message 
     INTEGER,      INTENT(IN)      :: num           ! number of things to try to match
     INTEGER,      INTENT(OUT)     :: kv            ! -1 if no match
     LOGICAL,      INTENT(IN)      :: verbose       ! print write statements

     INTEGER       :: is_missing, iret, k
     INTEGER       :: pcat, pnum 
     character(len=256) :: gname = 'not_set'
           IF(verbose)write(*,*) 'Extracting from grib messages'
        !  First try to use shortName for matching.
           call codes_is_missing(igrib,'shortName',is_missing,iret)
           IF(is_missing.ne.1)THEN
             call grib_get(igrib,'shortName',gname)
             IF(trim(gname).NE.'unknown')THEN
                kv=-1
                DO k=1,num
                  ! test restricted to the first four chars
                  IF(trim(gname(1:4)).EQ.tmpgrb(k))THEN
                     kv=k
                     !IF(VERBOSE)write(*,*) gname
                  END IF
                END DO
             ELSE
                ! if shortname is unknown then need to activate next block.
                is_missing=1
             END IF
           END IF

           IF(is_missing.eq.1)THEN
   !        Use parameterCategory and parameterNumber to match.
              call grib_get(igrib,'parameterCategory',pcat)
              call grib_get(igrib,'parameterNumber',pnum)
              kv=-1
              DO k=1,num
                 IF(pcat.EQ.tmpcat(k).AND.pnum.EQ.tmpnum(k))kv=k
              END DO
           END IF

   END SUBROUTINE MATCH


   !-------------------------------------------------------------
   ! Create the configuration file for HYSPLIT library
   ! packing subroutines                                 
   !-------------------------------------------------------------

   SUBROUTINE MAKNDX (FILE_NAME,NZP,metgrid,   &
                      NUMSFC,NUMATM,LEVELS,SIGMAZ, &
                      SFCVAR,ATMVAR,ATMARL,SFCARL,INVERT,VCORD)
     IMPLICIT NONE

     CHARACTER(80),INTENT(IN)   :: file_name     ! configuration file  
     TYPE(AGRID),  INTENT(IN)   :: metgrid       ! grid information
     INTEGER,      INTENT(IN)   :: nzp           ! y dimension
     INTEGER,      INTENT(IN)   :: numsfc        ! numb sfc var in cfg
     INTEGER,      INTENT(IN)   :: numatm        ! numb atm var in cfg
     INTEGER,      INTENT(IN)   :: levels(:)     ! level value each atm
     REAL,         INTENT(IN)   :: sigmaz(:)     ! sigma at each level  
     INTEGER,      INTENT(IN)   :: sfcvar(:)     ! mark each var found 
     INTEGER,      INTENT(IN)   :: atmvar(:,:)   ! mark each var by level
     CHARACTER(4), INTENT(IN)   :: atmarl(:)     ! output character ID
     CHARACTER(4), INTENT(IN)   :: sfcarl(:)     ! output character ID 
     LOGICAL,      INTENT(IN)   :: invert        ! data N to S
     CHARACTER(8), INTENT(IN)   :: vcord         ! vertical coordinate 

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


   ! sync x,y defines lower left grid point 
     GRIDS(8)=1.0 
     GRIDS(9)=1.0


   ! Set lat/lon of lower left point
     GRIDS(10)= metgrid%CLAT
     GRIDS(11)= metgrid%CLON

   ! grid should be defined on a 0->360 coordinate
     IF(GRIDS(11).LT.0.0)GRIDS(11)=360.0+GRIDS(11)

     IF(metgrid%RLAT.EQ.0.0.AND.metgrid%RLON.EQ.0.0)THEN
   !    defines a regular lat-lon grid
   !    Pole lat/lon is used to identify the latlon point of the max index
        GRIDS(1)=GRIDS(10)+metgrid%DLAT*(metgrid%NYP-1)
        GRIDS(2)=GRIDS(11)+metgrid%DLON*(metgrid%NXP-1)
        GRIDS(2)=AMOD(GRIDS(2),360.0)

        GRIDS(3)=metgrid%DLAT ! ref lat defines grid spacing

        GRIDS(4)=metgrid%DLON ! ref lon defines grid spacing

        GRIDS(5)=0.0  ! grid size zero for lat/lom
        GRIDS(6)=0.0  ! orientation
        GRIDS(7)=0.0  ! tangent latitude

     ELSE
   !    defines a lambert conformal grid
        GRIDS(1)=metgrid%TLAT1                   
        GRIDS(2)=metgrid%RLON                               

        GRIDS(3)=metgrid%TLAT2 ! resolution defined at this latitude 
        GRIDS(4)=metgrid%RLON  ! ref lon defines grid spacing

        GRIDS(5)=SQRT(metgrid%DLAT*metgrid%DLON)/1000.0 ! grid size km
        GRIDS(6)=0.0   ! grid orientation
        GRIDS(7)=metgrid%TLAT1 ! tangent latitude

        IF(invert) GRIDS(9)=metgrid%NYP
     END IF
   ! variable reserved for future use
     GRIDS(12)=0.0  

   ! write the packer configuration file
     OPEN(30,FILE=FILE_NAME)

   ! default grid number 99 (field not used)
     WRITE(30,'(A20,A4)')LABEL(1),metgrid%MODEL 
     WRITE(30,'(A20,A4)')LABEL(2),'  99'

   ! coordinate (1:sigma 2:pressure 3:terrain 4:hybrid)
     IF(trim(vcord).EQ.'pl')THEN
        WRITE(30,'(A20,I4)') LABEL(3), 2        
     ELSEIF(trim(vcord).EQ.'ml')THEN
        WRITE(30,'(A20,I4)') LABEL(3), 1        
     ELSEIF(trim(vcord).EQ.'hl')THEN
        WRITE(30,'(A20,I4)') LABEL(3), 4        
     ELSE
        WRITE(30,'(A20,I4)') LABEL(3), -1       
     END IF

   ! grid geolocation parameters and projection
     DO N=1,12
        WRITE(30,'(A20,F10.2)')LABEL(N+3),GRIDS(N)
     END DO

   ! grid dimensions
     WRITE(30,'(A20,I4)') LABEL(16), metgrid%NXP
     WRITE(30,'(A20,I4)') LABEL(17), metgrid%NYP
     WRITE(30,'(A20,I4)') LABEL(18), NZP+1

   ! upper level information
     DO nl=0,nzp   
        WRITE(LABEL(1),'(A6,I2,A1)')'Level ',NL,':'

        IF(NL.EQ.0)THEN
           IF(trim(vcord).EQ.'pl')THEN
              SIG=0.0     
           ELSE
              SIG=1.0
           END IF
           MVAR=0

           DO n=1,numsfc
              IF(sfcvar(n).EQ.1)THEN
                 MVAR=MVAR+1
                 VCHAR(MVAR)=sfcarl(n)  
              END IF
           END DO

        ELSE
           IF(trim(vcord).EQ.'pl')THEN
   !####      SIG=SIGMAZ(NL)         
              SIG=LEVELS(NL)  ! gdas
           ELSEIF(trim(vcord).EQ.'ml')THEN
              !SIG=LEVELS(NL)/1.0E+05
              SIG=LEVELS(NL)
           ELSEIF(trim(vcord).EQ.'hl')THEN
              !SIG=LEVELS(NL)
              SIG=SIGMAZ(NL)
           ELSE
              SIG=SIGMAZ(NL)
           END IF
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

   ! ECMWF variables
   ! msl	- mean sea level pressure
   ! lcc	- low cloud cover (fraction)
   ! tcc	- total cloud cover (fraction)
   ! lsp	- stratiform precipitation (m)
   ! cp	- convective precipitation
   ! sshf	- surface sensible heat flux (W m**-2 s)
   ! ewss  - east west sfc stress (N m**-2 s)
   ! ssr	- surface solar radiation (W m**-2 s)
   ! z	- geopotential (m**2 s**-2)
   ! cvl	- low vegetation cover (fraction)
   ! sr	- surface roughness (m)
   ! etadot- eta coordinate vertical velocity

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

     write(30,'(a)')' numatm = 12,'
     write(30,'(a)')' atmgrb = '//a//'gh'//d//'pres'//d// 't'//d//'pt'//d//'u'//d//'v'//d//'w'//d//'z' &
                                //d// 'r'//d//'qv'//d//'q'//d//'etad'//a//c
     write(30,'(a)')' atmcat =     3 ,   3 ,    0 ,   0 ,   2 ,    2 ,    2 ,   2 ,   1 ,   1 ,    1,   2,'  
     write(30,'(a)')' atmnum =     5 ,   0 ,    0 ,   2 ,   2 ,    3 ,    8 ,   9 ,   1 ,   2 ,    2,   8,'
     write(30,'(a)')' atmcnv =   1.0 ,0.01 ,  1.0 , 1.0 , 1.0 ,  1.0 , 0.01 , 1.0 , 1.0 , 1.0 ,  1.0, 0.01'
     write(30,'(a)')' atmarl = '//a//'HGTS'//d//'PRES'//d//'TEMP'//d//'THET'//d//'UWND'  &
                                //d//'VWND'//d//'WWND'//d//'DZDT'//d//'RELH'//d//'SPHU'//d//'SPHU'//d//'WWND'//a//c

     write(30,'(a)')' numsfc = 21,'
     write(30,'(a)')' sfcgrb = '//a//'orog'//d//  'sp'//d//  'tp'//d// 'tcc'//d//'hpbl'//d//'uflx'//d//'vflx'  &
                                //d//'dswr'//d//'shtf'//d//'lhtf'//d//  '2t'//d//   'r'//d// '10u'//d// '10v'  &
                                //d//'ewss'//d//'nsss'//d//   'z'//d//'sshf'//d// 'ssr'//d// 'lsp'//d//  'cp'//a//c
     write(30,'(a)')' sfccat =   3,   3,   1,  6,  19,  2, 17,  4,  0,  0,  0,  1,  2,  2,  2,  2,  3,  0,  4,   1,   1' 
     write(30,'(a)')' sfcnum =   5,   0,   8,  1,   3, 17, 18,  0, 11, 10,  0,  1,  2,  3,  2,  3,  5, 11,  0,   8,   8' 
     write(30,'(a)')' sfccnv = 1.0,0.01,.001,1.0, 1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,0.1,1.0,1.0,.001,.001' 
     write(30,'(a)')' sfcarl = '//a//'SHGT'//d//'PRSS'//d//'TPP6'//d//'TCLD'//d//'PBLH'//d//'UMOF'//d//'VMOF' &
                                //d//'DSWF'//d//'SHTF'//d//'LHTF'//d//'T02M'//d//'RH2M'//d//'U10M'//d//'V10M' &
                                //d//'UMOF'//d//'VMOF'//d//'SHGT'//d//'SHTF'//d//'DSWF'//d//'TPPA'//d//'TPPA'//a//c

     write(30,'(a)')'/'
     close(30)

   END SUBROUTINE makapi

!-------------------------------------------------------------
! get structure
! Loop on all the messages in memory to establish the
! variable and level structure of the output data set
! according to the variables defined in the namelist


  SUBROUTINE gtstrct(num_msg, igrib, udif,   &
                   numsfc, sfccat, sfcnum, sfcgrb, sfcarl, sfcvar, &
                   numatm, atmcat, atmnum, atmgrb, atmarl, atmvar, &
                   metgrid, model, invert, grid_base, &
                   msglev, msgvar, ltype, levels, numlev, tnum)

  use eccodes
  implicit none

  INTEGER, INTENT(IN)           :: num_msg    ! number of messages in grib file
  INTEGER, INTENT(IN)           :: igrib (:)  ! preloaded grib messages
  LOGICAL, INTENT(IN)           :: udif
  CHARACTER(4), INTENT(IN)     :: atmarl(:)     ! ARL character ID
  CHARACTER(4), INTENT(IN)     :: sfcarl(:)     ! ARL character ID
  INTEGER, INTENT(IN)          :: numsfc,numatm ! number of variables to look for
  INTEGER, dimension(maxvar), INTENT(IN)     :: atmnum,atmcat   ! 
  INTEGER, dimension(maxvar), INTENT(IN)     :: sfcnum,sfccat   ! 
  CHARACTER(len=4), dimension(maxvar), INTENT(IN)        :: atmgrb  ! grib shortname
  CHARACTER(len=4), dimension(maxvar), INTENT(IN)        :: sfcgrb  ! grib shortname

  TYPE(AGRID),      INTENT(OUT)    :: metgrid
  INTEGER,          INTENT(OUT)    :: grid_base
  LOGICAL,          INTENT(OUT)    :: invert
  CHARACTER(len=8), INTENT(OUT)    :: ltype      ! level type
  CHARACTER(len=4), INTENT(OUT)    :: model      ! model id
  integer,  INTENT(OUT)  :: levels(:)
  integer,  INTENT(OUT)  :: numlev


  INTEGER, INTENT(INOUT) :: msglev(:), msgvar(:) 
  INTEGER, INTENT(OUT) :: atmvar(:,:) 
  INTEGER, INTENT(OUT) :: sfcvar(:) 
  INTEGER, INTENT(OUT) :: tnum   ! number of time periods found in file.


  integer :: i, k, kl, kv, levhgt 
  logical :: ftest
  character(len=256) :: vdate = 'not set'
  character(len=256) :: pdate = 'no value'
  integer :: vtime, ptime


  levels=-1
  sfcvar=0
  atmvar=0

  DO i=1,num_msg
     ! pl should be for pressure levels
     ! ml should be for model levels.
     call grib_get(igrib(i),'levelType',ltype) 
     IF(ltype.EQ.'150')ltype='hl'   ! hybrid levels
     IF(ltype.EQ.'200')ltype='sfc'  ! 200 is for TCLD
!    -----------------------------------------------------------------------------
     IF(trim(ltype).EQ.'sfc')THEN
        call  MATCH(numsfc,sfccat,sfcnum,sfcgrb,igrib(i),kv,.FALSE.)
        IF(kv.NE.-1)THEN
           ! set level and 2D (sfc) variable indicies
           msglev(i)=0
           msgvar(i)=kv
           sfcvar(kv)=1
        END IF

!    -----------------------------------------------------------------------------
     ! atmospheric variable (pressure or model levels or hybrid)
     ELSEIF(trim(ltype).EQ.'pl'.OR.trim(ltype).EQ.'ml'.OR.trim(ltype).EQ.'hl')THEN

        ! first time through get the grid information.
        IF(numlev.eq.0)THEN
             CALL GETGRD(igrib(i),metgrid,model,invert)
        END IF

        ! see if message variable matches one in the config file.
        call  MATCH(numatm,atmcat,atmnum,atmgrb,igrib(i), kv,.FALSE.)

        ! if a match was found 
        IF(kv.NE.-1)THEN
           call grib_get(igrib(i),'validityDate',vdate)
           call grib_get(igrib(i),'validityTime',vtime)
           IF(i.eq.1)THEN
              tnum=1
           ELSE IF((pdate.ne.vdate).or.(ptime.ne.vtime))THEN
              tnum=tnum+1
           END IF
           pdate = vdate
           ptime = vtime

           ! need to use one message later that has atmospheric data in it.
           grid_base = i
           ! get the level.
           call grib_get(igrib(i),'level',levhgt)
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
         
           ! atmvar is an array which keeps track
           msglev(i)=kl
           msgvar(i)=kv
           atmvar(kv,kl)=1     
           IF(UDIF.and.(atmarl(kv).eq.'WWND'))THEN
             atmvar(numatm,kl) = 1 ! set difw field for each level   
           END IF
        END IF

     ELSE
!       level type is undefined 
        WRITE(*,*)'levelType not defined: ', ltype
!##     STOP
        CONTINUE
     END IF
  END DO 
  write(*,*) 'Number of time periods found in file ', tnum
END SUBROUTINE gtstrct
! end of looping through the messages.
!------------------------------------------------------------
  
subroutine pakm(igrib, param,units,kl,nxp,nyp,cvar, &
                numlev, values, rvalue, var2d, invert, zero, udif)
     use eccodes
     implicit none
     include 'pakrec.inc'
     include 'pakinp.inc'

     INTEGER, INTENT(IN)  :: igrib
     INTEGER, INTENT(IN)  :: kl,nxp,nyp,numlev
     CHARACTER(len=1), INTENT(OUT)  :: cvar(:)
     REAL, INTENT(INOUT)  :: values(:)
     REAL, INTENT(INOUT)  :: rvalue(:,:)
     REAL, INTENT(INOUT)  :: var2d(:,:)
     REAL, INTENT(IN)     :: units
     CHARACTER(len=4), INTENT(IN) :: param 
     LOGICAL, INTENT(IN)  :: invert, udif   
     INTEGER, INTENT(IN)  :: zero 
 
     integer :: j,k,m,n 
     integer :: if1,if2,iyr,imo,ida,ifh,ihr,imn 
     integer :: iret
     character(len=256) :: vdate

     ! this is for the diff fields
     ! comes from pakrec and is used for pakinp
     integer :: nexp, ksum
     real    :: prec, var1
     COMMON / PAKVAL / PREC, NEXP, VAR1, KSUM

     call grib_get(igrib,'forecastTime',ifh,iret)
!#   IF(iret.NE.GRIB_SUCCESS)                    &
!#      WRITE(*,*)'Missing forecastTime in message #: ',i
     call grib_get(igrib,'startStep',if1)
     call grib_get(igrib,'endStep',if2)
     ifh=MAX(ifh,if1,if2)

     IF(kl.GT.-1)THEN
        ! set the date variables to correspond with the valid time
        call grib_get(igrib,'validityDate',vdate)
        READ(vdate,'(2X,3I2)') iyr,imo,ida
        call grib_get(igrib,'validityTime',imn)
        ihr=imn/100
        imn=imn-ihr*100
     END IF

     ! get data values in a one dimensional array
     call grib_get(igrib,'values',values)
  
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
     IF(kl.EQ.0)THEN
        ! surface variables level indicator is 1 (KL+1)
        CALL PAKREC(punit,RVALUE,CVAR,NXP,NYP,(NXP*NYP),PARAM,     &
             IYR,IMO,IDA,IHR,IMN,IFH,(KL+1),ZERO)
     ELSE
        CALL PAKREC(punit,RVALUE,CVAR,NXP,NYP,(NXP*NYP),PARAM,     &
             IYR,IMO,IDA,IHR,IMN,IFH,(NUMLEV-KL+2),ZERO)

        IF((UDIF).and.(param.eq.'UWND'))then
           call pakinp(var2d,cvar,nxp,nyp,1,1,nxp,nyp,prec,nexp,var1,ksum)
           rvalue = rvalue-var2d  
           CALL PAKREC(punit,RVALUE,CVAR,NXP,NYP,(NXP*NYP),'DIFW',     &
                IYR,IMO,IDA,IHR,IMN,IFH,(NUMLEV-KL+2),ZERO)
        END IF

     END IF

end subroutine pakm



END MODULE arlmod
