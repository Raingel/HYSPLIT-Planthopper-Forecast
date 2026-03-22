SUBROUTINE cfgvar(cfg_name,k)

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

!  instantaneous wind configuration file
  if(k.eq.1)then

  write(30,'(a)')' num3dv = 7,'
  write(30,'(a)')' arw3dv = '//a//'P'//d//'T'//d//'U'//d//'V'//d//'W'//d//'X'//d//'QVAPOR'//a//c
  write(30,'(a)')' cnv3dv =  0.01, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,'
  write(30,'(a)')' arl3dv = '//a//'PRES'//d//'TEMP'//d//'UWND'//d//'VWND'//d//'WWND'//d//'DIFW'//d//'SPHU'//a//c

  write(30,'(a)')' num2dv = 12,'
  write(30,'(a)')' arw2dv = '//a//'HGT'//d//'PSFC'//d//'RAIN'//d//'X'//d//'PBLH'//d//'UST'     &
                             //d//'SWDOWN'//d//'HFX'//d//'LH'//d//'T2'//d//'U10'//d//'V10'//a//c
  write(30,'(a)')' cnv2dv = 1.0, 0.01, 0.001, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,'
  write(30,'(a)')' arl2dv = '//a//'SHGT'//d//'PRSS'//d//'TPP1'//d//'DIFR'//d//'PBLH'//d//'USTR &
                            '//d//'DSWF'//d//'SHTF'//d//'LHTF'//d//'T02M'//d//'U10M'//d//'V10M'//a//c

! averaged wind flux configuration file
  elseif(k.eq.2)then

  write(30,'(a)')' num3dv = 6,'
  write(30,'(a)')' arw3dv = '//a//'P'//d//'T'//d//'AVGFLX_RUM'//d//'AVGFLX_RVM'//d//'AVGFLX_WWM'//d//'QVAPOR'//a//c
  write(30,'(a)')' cnv3dv =  0.01, 1.0, 1.0, 1.0, 1.0, 1.0,'
  write(30,'(a)')' arl3dv = '//a//'PRES'//d//'TEMP'//d//'UWND'//d//'VWND'//d//'WWND'//d//'SPHU'//a//c

  write(30,'(a)')' num2dv = 18,'
  write(30,'(a)')' arw2dv = '//a//'HGT'//d//'PSFC'//d//'RAIN'//d//'X'//d//'PBLH'//d//'UST'//d//'ZNT'     &
                             //d//'SWDOWN'//d//'HFX'//d//'LH'//d//'T2'//d//'U10'//d//'V10'       &
                             //d//'MUU'//d//'MUV'//d//'MAPFAC_M'//d//'MAPFAC_U'//d//'MAPFAC_V'//a//c
  write(30,'(a)')' cnv2dv = 1.0, 0.01, 0.001, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 , &
                               1.0 , 1.0 , 1.0 , 1.0 ,'
  write(30,'(a)')' arl2dv = '//a//'SHGT'//d//'PRSS'//d//'TPP1'//d//'DIFR'//d//'PBLH'//d//'USTR'//d//'RGHS &
                            '//d//'DSWF'//d//'SHTF'//d//'LHTF'//d//'T02M'//d//'U10M'//d//'V10M &
                            '//d//'MUU0'//d//'MUV0'//d//'MSFT'//d//'MSFU'//d//'MSFV'//a//c
! tke configuration file
  elseif(k.eq.3)then
  write(30,'(a)')' num3dv = 8,'
  write(30,'(a)')' arw3dv = '//a//'P'//d//'T'//d//'U'//d//'V'//d//'W'//d//'X'//d//'QVAPOR'//d//'TKE_PBL'//a//c
  write(30,'(a)')' cnv3dv =  0.01, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,'
  write(30,'(a)')' arl3dv = '//a//'PRES'//d//'TEMP'//d//'UWND'//d//'VWND'//d//'WWND'//d//'DIFW'//d//'SPHU'//d//'TKEN'//a//c

  write(30,'(a)')' num2dv = 12,'
  write(30,'(a)')' arw2dv = '//a//'HGT'//d//'PSFC'//d//'RAIN'//d//'X'//d//'PBLH'//d//'UST'    &
                             //d//'SWDOWN'//d//'HFX'//d//'LH'//d//'T2'//d//'U10'//d//'V10'//a//c
  write(30,'(a)')' cnv2dv = 1.0, 0.01, 0.001, 1.0,  1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,'
  write(30,'(a)')' arl2dv = '//a//'SHGT'//d//'PRSS'//d//'TPP1'//d//'DIFR'//d//'PBLH'//d//'USTR &
                            '//d//'DSWF'//d//'SHTF'//d//'LHTF'//d//'T02M'//d//'U10M'//d//'V10M'//a//c
  
  endif

  write(30,'(a)')'/'
  close(30)

END SUBROUTINE cfgvar
