      character * 80 fname
      character *80 buf
      integer*4 handle
      integer*4 fcopen,fclen,fcgtps
      data fname/'testfile.txt'/
      data buf/' '/
      handle = fcopen(fname,'r')
      write(*,'(a,i6)')'Numerical value of handle is:',handle
      if (handle.lt.0) then
        write(*,*) 'File :',fname,' not found'
      else
        lenfil = fclen(handle)
        write(*,'(a,i6)') 'Length of file is :',lenfil
        call fcread(handle,buf,1,6)
        write(*,*) buf(1:6)
        write(*,'(i6)') fcgtps(handle)
        call fcptps(handle,4)
        call fcread(handle,buf,1,6)
        write(*,*) buf(1:6)
        call fcread(handle,buf,-3,2)
        write(*,*) buf(1:6)
        call fcclos(handle)

        handle= fcopen('test.out','w')
        call fcwrit(handle,'lmnop',5,1)
        call fctrnc(handle,4)
        call fcclos(handle)
      endif
      stop
      end
