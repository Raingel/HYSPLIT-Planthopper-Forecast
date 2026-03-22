program asc2grad

!-------------------------------------------------------------------------------
! trajectory model output data in ascii format is converted to 
! GrADS format - multiple trajectory version
! programmer : Jan H. Vermeulen  : 19 Febr. 97
!-------------------------------------------------------------------------------
! revised    : Roland Draxler    : 11 Jan 2000
!                                : 12 Dec 2000 - fortran90 upgrade 
!                                : 31 May 2005 - convert to file format #1
!-------------------------------------------------------------------------------

! program attributes : Fortran 77

      real ptrajlat,ptrajlon,time
      real trajh, trajp

      character*3 months(12)
      character*8 trajid
      logical ftest
      character*80 fname,dummy
      data months/'jan','feb','mar','apr','may','jun',                         &
                  'jul','aug','sep','oct','nov','dec'/

! input file information
! WIN32:NARGS()=arguments+command    UNIX:IARGC()=arguments
      narg=iargc()
!     narg=nargs()-1

      if(narg.EQ.0)then
         write(*,*)'Usage: asc2bin [file name]'
         stop
      else
         call getarg(1,fname)
         inquire (file=fname, exist=ftest)
         if(ftest)then
            open(10,file=fname)
         else
            write(*,*)'File not found:',fname
            stop
         end if
      end if

! call assign is Cray specific such that a stream data set can be created
!     call assign("assign -Nieee -Fnull u:20")
      open(20,file='grads.bin',form='unformatted')

      iflag = 0
      irec = 0
! Reposition to begining of trajectory data
! first 2 dummy read, then read the number of trajectories (ino)  and lastly 
! ino+1 dummy reads
      read(10,'(a80)')dummy
      read(10,'(a80)')dummy
      read(10,'(i6)')ino
      do l=1,ino
         read(10,'(4i6,2f9.3,f8.1)')iyr,imo,ida,ihr,slat,slon,slvl
      end do
      read(10,'(a80)')dummy
      if(iyr.lt.50)then
         iyear=2000+iyr
      else
         iyear=1900+iyr
      end if

! check domain size for map optimization
      alatt=slat
      alatb=slat
      alonl=slon
      alonr=slon

! Read and Write
      do 50 i=1,2000

      read(10,'(6x,i6,30x,a8,6x,2f9.3,f8.1,1x,f8.1)',end=60)ih,trajid,    &
           trajlat,trajlon,trajh,trajp
!     test for blank record at end
      if(trajlat.eq.0.0.and.trajlon.eq.0.0)goto 50
      if(iflag.eq.0)then
        iflag = 1
        ihold = ih
      endif

! map limits
      alatt=max(trajlat,alatt)
      alatb=min(trajlat,alatb)
      alonl=min(trajlon,alonl)
      alonr=max(trajlon,alonr)

! if new time group, write time group terminator.
      if(ihold.ne.ih)then
      ij=0
      write(20)trajid,ptrajlat,ptrajlon,time,ij,ij
      write(*,'(a8,2f8.3,f4.1,2i2)')trajid,ptrajlat,ptrajlon,time,0,0
      endif
      ihold = ih

! write this report
      time = 0.0
      ij=1
      write(20)trajid,trajlat,trajlon,time,ij,ij
      write(20)trajh,trajp
      write(*,'(a8,2f8.3,f4.1,2i2)')trajid,trajlat,trajlon,time,1,1
      write(*,'(2f8.1)')trajh,trajp
      irec=irec+1
      ptrajlat = trajlat
      ptrajlon = trajlon
50    continue

! On end of file write last time group terminator
60    continue 
      ij=0
      write(20)trajid,trajlat,trajlon,time,ij,ij
      write(*,'(a8,2f8.3,f4.1,2i2)')trajid,trajlat,trajlon,time,0,0

      write(*,*)'Number of ascii records converted to GrADS = ',irec
      write(*,*)'Number of trajectories = ',ino
      close(10)
      close(20)

! create various grads control and script files according to plot
      open(30,file='grads.ctl')
      write(30,'(a)')'dset grads.bin'
      write(30,'(a)')'dtype station'
      write(30,'(a)')'format sequential'
      write(30,'(a)')'stnmap grads.map'
      write(30,'(a)')'undef -999.0'
      write(30,'(a,i2.2,a1,i2.2,a3,i4,a)')'tdef 1 linear ',ihr,'z',            &
                                          ida,months(imo),iyear,' 1hr'
      write(30,'(a)')'vars 2'
      write(30,'(a)')'z 0 99 height agl'
      write(30,'(a)')'p 0 99 pressure height'
      write(30,'(a)')'endvars'
      close(30)

      if(alatb.lt.0.0)alatb=alatb-5.0
      if(alatt.gt.0.0)alatt=alatt+5.0
      if(alonl.lt.0.0)alonl=alonl-5.0
      if(alonr.gt.0.0)alonr=alonr+5.0
      lat1=int(alatb/5.0)*5
      lat2=int(alatt/5.0)*5
      lon1=int(alonl/5.0)*5
      lon2=int(alonr/5.0)*5
  
      open(30,file='grads.gs')
      write(dummy,'(2i4)')lat1,lat2
      write(30,'(3a)')'''set lat',dummy(1:8),''''
      write(dummy,'(2i5)')lon1,lon2
      write(30,'(3a)')'''set lon',dummy(1:10),''''
      write(30,'(a)')'''set grads off'''
      write(30,'(a)')'''d p'''
      close(30)

end program asc2grad
