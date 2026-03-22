SUBROUTINE cfgvar(cfg_name)

  implicit none
  character(len=1)   :: a,c
  character(len=3)   :: d
  character(len=80)  :: cfg_name 
  integer            :: k

  a = CHAR(39) ! apostrophe
  c = CHAR(44) ! comma
  d(1:1) = a
  d(2:2) = c
  d(3:3) = a

  open (30,file=TRIM(cfg_name))
  write(30,'(a)')'&SETUP'

  write(30,'(a)')' num3dv = 5,'
  write(30,'(a)')' cesm3dv = '//a//'T'//d//'U'//d//'V'//d//'OMEGA'//d//'Q'//a//c
  write(30,'(a)')' cnv3dv =  1.0, 1.0, 1.0, 0.01, 1.0,'
  write(30,'(a)')' arl3dv = '//a//'TEMP'//d//'UWND'//d//'VWND'//d//'WWND'//d//'SPHU'//a//c

  write(30,'(a)')' num2dv = 8,'
  write(30,'(a)')' cesm2dv = '//a//'PS'//d//'PRECT'//d//'FSDS'//d//'CLDTOT'     &
                             //d//'PBLH'//d//'SHFLX'//d//'LHFLX'//d//'TREFHT'//a//c
  write(30,'(a)')' cnv2dv = 0.01, 1.0, 1.0, 100.0, 1.0, 1.0, 1.0, 1.0,'
  write(30,'(a)')' arl2dv = '//a//'PRSS'//d//'TPP3'//d//'DSWF'//d//'TCLD' &
                             //d//'PBLH'//d//'SHTF'//d//'LHTF'//d//'T02M'//a//c

  write(30,'(a)')'/'
  close(30)

END SUBROUTINE cfgvar
