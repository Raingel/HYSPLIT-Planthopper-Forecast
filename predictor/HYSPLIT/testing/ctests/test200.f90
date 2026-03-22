program test200
  implicit none

  character(len=5) :: version_in

  print *, 'Testing version string...'

  ! Open the VERSION file and get the value.
  open(20, file = 'VERSION', status = 'old')
  read(20,'(A5)') version_in
  close(20)
  print *, 'This test is meant to fail'

  ! make this test fail?
  STOP 2
    
  print *, 'SUCCESS!'
end program test200
