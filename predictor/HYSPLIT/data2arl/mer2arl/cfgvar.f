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

  write(30,'(a)')' num3dv = 6,'
  write(30,'(a)')' mer3dv = '//a//'PL'//d//'T'//d//'U'//d//'V'//d//'OMEGA'//d//'QV'//a//c
  write(30,'(a)')' cnv3dv =  0.01, 1.0, 1.0, 1.0, 0.01, 1.0,'
  write(30,'(a)')' arl3dv = '//a//'PRES'//d//'TEMP'//d//'UWND'//d//'VWND'//d//'WWND'//d//'SPHU'//a//c

  write(30,'(a)')' num2dv = 12,'
  write(30,'(a)')' mer2dv = '//a//'HGT'//d//'PS'//d//'PRECTOTCORR'//d//'PBLH'//d//'USTAR'//d//'Z0M'     &
                             //d//'SWNETSRF'//d//'HFLUX'//d//'EFLUX'//d//'T2M'//d//'U10M'//d//'V10M'//a//c
  write(30,'(a)')' cnv2dv = 0.102, 0.01, 0.06, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,'
  write(30,'(a)')' arl2dv = '//a//'SHGT'//d//'PRSS'//d//'PRT3'//d//'PBLH'//d//'USTR'//d//'RGHS' &
                             //d//'DSWF'//d//'SHTF'//d//'LHTF'//d//'T02M'//d//'U10M'//d//'V10M'//a//c

  write(30,'(a)')'/'
  close(30)

END SUBROUTINE cfgvar
