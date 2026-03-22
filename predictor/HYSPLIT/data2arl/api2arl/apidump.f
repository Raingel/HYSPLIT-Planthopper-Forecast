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
! Initial version based upon count_messages.f 
! Last Revised: 12 Feb 2010 - Roland.Draxler@noaa.gov
! 27 Oct 2017 - replaced use grib_api with use eccodes library.

program apidump
  use eccodes
  implicit none

  integer                            ::  status
  integer                            ::  ifile
  integer                            ::  iret
  integer                            ::  num_msg
  integer                            ::  Ni,Nj
  integer                            ::  i
  integer,dimension(:),allocatable   ::  igrib
  real                               ::  latitudeOfFirstPointInDegrees
  real                               ::  longitudeOfFirstPointInDegrees
  real                               ::  latitudeOfLastPointInDegrees
  real                               ::  longitudeOfLastPointInDegrees
  integer                            ::  numberOfPointsAlongAParallel
  integer                            ::  numberOfPointsAlongAMeridian
  real                               ::  iDirectionIncrementInDegrees
  real                               ::  jDirectionIncrementInDegrees
  real, dimension(:), allocatable    ::  values
  integer                            ::  numberOfValues
  real                               ::  average,min_val, max_val

  character(len=20)  :: name_space
  character(len=80)  :: message 
  character(len=256) :: key
  character(len=256) :: value
  character(len=512) :: all
  integer            :: grib_count
  integer            :: kiter

  integer            :: iyr,imo,ida,ihr

  name_space='ls'
  value='not_set'

  !------------------------------------------------------------

! support multiple fields in a single message
  call grib_multi_support_on(status)
  IF (status.NE.grib_success) GOTO 900

  ! open the grib file
  call grib_open_file(ifile,'data.grib2','r',status)
  IF (status.NE.grib_success) GOTO 900

  ! count the messages in the file
  call grib_count_in_file(ifile,num_msg)
  IF (status.NE.grib_success) GOTO 900
  allocate(igrib(num_msg))
  igrib=-1

  ! Load the messages into memory from the file.
  DO i=1,num_msg
     call grib_new_from_file(ifile,igrib(i), status)
     IF (status.NE.grib_success) GOTO 900
  END DO

  ! we can close the file
  call grib_close_file(ifile,status)
  IF (status.NE.grib_success) GOTO 900

  !------------------------------------------------------------
  ! Loop on all the messages in memory

  DO i=1,num_msg
     write(*,*) 'processing message number ',i

!    call grib_keys_iterator_new(igrib(i),kiter,name_space)
!    do
!       call grib_keys_iterator_next(kiter, iret) 
!       if (iret .ne. 1) exit
!       call grib_keys_iterator_get_name(kiter,key)
!       call grib_get(igrib(i),trim(key),value)
!       all=trim(key)// ' = ' // trim(value)
!       write(*,*) trim(all)
!    end do
!    call grib_keys_iterator_delete(kiter)

     key='date'
        call grib_get(igrib(i),trim(key),value)
        all=trim(key)// ' = ' // trim(value)
        write(*,*) trim(all)
     key='gridType' 
        call grib_get(igrib(i),trim(key),value)
        all=trim(key)// ' = ' // trim(value)
        write(*,*) trim(all)
     key='levelType' 
        call grib_get(igrib(i),trim(key),value)
        all=trim(key)// ' = ' // trim(value)
        write(*,*) trim(all)
     key='level' 
        call grib_get(igrib(i),trim(key),value)
        all=trim(key)// ' = ' // trim(value)
        write(*,*) trim(all)
     key='shortName'
        call grib_get(igrib(i),trim(key),value)
        all=trim(key)// ' = ' // trim(value)
        write(*,*) trim(all)
     key='pressureUnits' 
        call grib_get(igrib(i),trim(key),value)
        all=trim(key)// ' = ' // trim(value)
        write(*,*) trim(all)
     key='shortName' 
        call grib_get(igrib(i),trim(key),value)
        all=trim(key)// ' = ' // trim(value)
        write(*,*) trim(all)
     key='cfName' 
        call grib_get(igrib(i),trim(key),value)
        all=trim(key)// ' = ' // trim(value)
        write(*,*) trim(all)
     key='name' 
        call grib_get(igrib(i),trim(key),value)
        all=trim(key)// ' = ' // trim(value)
        write(*,*) trim(all)
     key='units' 
        call grib_get(igrib(i),trim(key),value)
        all=trim(key)// ' = ' // trim(value)
        write(*,*) trim(all)

     call grib_get(igrib(i),'year',  iyr)
     call grib_get(igrib(i),'month', imo)
     call grib_get(igrib(i),'day',   ida)
     call grib_get(igrib(i),'hour',  ihr)
     write(*,*) 'year,month,day,hour=',iyr,imo,ida,ihr 

     !     get as a integer
     call grib_get(igrib(i),'numberOfPointsAlongAParallel', &
          numberOfPointsAlongAParallel)
     write(*,*) 'numberOfPointsAlongAParallel=', &
          numberOfPointsAlongAParallel

     !     get as a integer
     call grib_get(igrib(i),'numberOfPointsAlongAMeridian', &
          numberOfPointsAlongAMeridian)
     write(*,*) 'numberOfPointsAlongAMeridian=', &
          numberOfPointsAlongAMeridian

     !     get as a real
     call grib_get(igrib(i), 'latitudeOfFirstGridPointInDegrees', &
          latitudeOfFirstPointInDegrees)
     write(*,*) 'latitudeOfFirstGridPointInDegrees=', &
          latitudeOfFirstPointInDegrees

     !     get as a real
     call grib_get(igrib(i), 'longitudeOfFirstGridPointInDegrees', &
          longitudeOfFirstPointInDegrees)
     write(*,*) 'longitudeOfFirstGridPointInDegrees=', &
          longitudeOfFirstPointInDegrees

     !     get as a real
     call grib_get(igrib(i), 'latitudeOfLastGridPointInDegrees', &
          latitudeOfLastPointInDegrees)
     write(*,*) 'latitudeOfLastGridPointInDegrees=', &
          latitudeOfLastPointInDegrees

     !     get as a real
     call grib_get(igrib(i), 'longitudeOfLastGridPointInDegrees', &
          longitudeOfLastPointInDegrees)
     write(*,*) 'longitudeOfLastGridPointInDegrees=', &
          longitudeOfLastPointInDegrees

     !     get as a real
     call grib_get(igrib(i), 'iDirectionIncrementInDegrees', &
          iDirectionIncrementInDegrees)
     write(*,*) 'iDirectionIncrementInDegrees=', &
          iDirectionIncrementInDegrees 

     !     get as a real
     call grib_get(igrib(i), 'jDirectionIncrementInDegrees', &
          jDirectionIncrementInDegrees)
     write(*,*) 'jDirectionIncrementInDegrees=', &
          jDirectionIncrementInDegrees 

     !     get as a integer
     call grib_get(igrib(i),'Ni', Ni)
     write(*,*) 'Ni=', Ni
     call grib_get(igrib(i),'Nj', Nj)
     write(*,*) 'Nj=', Nj

     !     get the size of the values array
     call grib_get_size(igrib(i),'values',numberOfValues)
     write(*,*) 'numberOfValues=',numberOfValues

     !     get data values
     allocate(values(numberOfValues), stat=iret)
     call grib_get(igrib(i),'values',values)

     call grib_get(igrib(i),'min',min_val) ! can also be obtained through minval(values)
     call grib_get(igrib(i),'max',max_val) ! can also be obtained through maxval(values)
     call grib_get(igrib(i),'average',average) ! can also be obtained through maxval(values)

     write(*,*)'There are ',numberOfValues, &
          ' average is ',average, &
          ' min is ',  min_val, &
          ' max is ',  max_val
     write(*,*) '---------------------'

  END DO

  DO i=1,num_msg
    call grib_release(igrib(i))
  END DO

  deallocate(values)
  deallocate(igrib)
  STOP

  900 CONTINUE
  CALL grib_get_error_string(status,message)
  WRITE(*,*) message
  STOP 900

end program apidump 
