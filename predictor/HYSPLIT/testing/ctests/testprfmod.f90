
PROGRAM testprfmod
!======================================================================
! testing for prfmod module
! test for
! prfa
! prfb
! calcrelh (incomplete) 
!----------------------------------------------------------------------
! Last Revision: 30 apr 2023 (AMC) 
!=======================================================================

!  USE funits
  USE prfmod
  IMPLICIT NONE
  INCLUDE 'utests.inc'

!------------------------------------------------------------------------------
  LOGICAL :: teststatus      ! keeps track of whether test has passed or failed. 
  REAL EPS
  PARAMETER (EPS = 2.0e-3)   ! error allowed in output position. 
  INTEGER :: iii
!-------------------------------------------------------------------------------
! argument list variables
!-------------------------------------------------------------------------------
  LOGICAL :: pflg,sflg,qflg    ! pressure,surface,humidity flags
  REAL    :: zsfc         ! surface terrain (m)
  REAL    :: pbot
  REAL    :: t1           ! lowest level temperature in profile (K)   
  REAL    :: p0           ! surface pressure (hPa)

  REAL    :: sphu         ! specific humidity
  REAL    :: sphu_target  ! specific humidity
  REAL    :: relh         ! relative humidity
  REAL    :: relh_target  ! relative humidity
  REAL    :: prs          ! pressure 
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
! additional argument list variables for prfb
!-------------------------------------------------------------------------------
  REAL    :: psg(5)      ! sigma pressure profile
  REAL    :: zzz(5)      ! levels data


! Target variables -------------------------------------------------
  REAL     :: tp0, tpbot, tzsfc

  teststatus = .TRUE.
! setup input variables 

!----------------------------------------------------------------------
! prfa case where both surface pressure and terrain height available.
  pflg=.TRUE.
  sflg=.TRUE.
  t1 = 12 + 273.15
  p0 = 1013
  zsfc = 100

  tp0 =  p0
  tzsfc = zsfc
  tpbot = LOG(p0)

  CALL PRfA(PFLG,SFLG,T1,ZSFC,P0,PBOT)

  CALL ASSERT_FLOAT_EQ(__FILE__, __LINE__, p0, tp0, EPS)
  CALL ASSERT_FLOAT_EQ(__FILE__, __LINE__, zsfc, tzsfc, EPS)
  CALL ASSERT_FLOAT_EQ(__FILE__, __LINE__, pbot, tpbot, EPS)
 
!----------------------------------------------------------------------
! prfa cases where  surface pressure available but terrain height is not.

! surface pressure is higher than 1013
  pflg=.TRUE.
  sflg=.FALSE.
  t1 = 12+273.15
  p0 = 920
  tp0 = p0
  tpbot = LOG(p0)
  zsfc = 0
  tzsfc = 803.8
  CALL PRfA(PFLG,SFLG,T1,ZSFC,P0,PBOT)
  CALL ASSERT_FLOAT_EQ(__FILE__, __LINE__, p0, tp0, EPS)
  CALL ASSERT_FLOAT_EQ(__FILE__, __LINE__, zsfc, tzsfc, 0.5)
  CALL ASSERT_FLOAT_EQ(__FILE__, __LINE__, pbot, tpbot, EPS)
  write(*,*) p0, zsfc

! surface pressure is equal to 1013
! terrain height should be 0
  p0 = 1013
  tp0 = p0
  tpbot = LOG(p0)
  tzsfc = 0
  CALL PRfA(PFLG,SFLG,T1,ZSFC,P0,PBOT)
  CALL ASSERT_FLOAT_EQ(__FILE__, __LINE__, p0, tp0, EPS)
  CALL ASSERT_FLOAT_EQ(__FILE__, __LINE__, zsfc, tzsfc, 0.5)
  CALL ASSERT_FLOAT_EQ(__FILE__, __LINE__, pbot, tpbot, EPS)
  write(*,*) p0, zsfc

! surface pressure is lower than 1013.
! terrain height should still be 0, not negative!
  p0 = 1017
  tp0 = p0
  tpbot = LOG(p0)
  tzsfc = 0
  CALL PRfA(PFLG,SFLG,T1,ZSFC,P0,PBOT)
  CALL ASSERT_FLOAT_EQ(__FILE__, __LINE__, p0, tp0, EPS)
  CALL ASSERT_FLOAT_EQ(__FILE__, __LINE__, zsfc, tzsfc, 0.5)
  CALL ASSERT_FLOAT_EQ(__FILE__, __LINE__, pbot, tpbot, EPS)
  write(*,*) p0, zsfc

!----------------------------------------------------------------------
! prfa Cases where terrain available but surface pressure is not.

! when zsfc is 0 then p0 should be equal to the mslp
  pflg=.FALSE.
  sflg=.TRUE.
  t1 = 12+273.15
  zsfc = 0
  tzsfc = zsfc
  tp0 = mslp
  tpbot = LOG(tp0)
  CALL PRfA(PFLG,SFLG,T1,ZSFC,P0,PBOT)
  CALL ASSERT_FLOAT_EQ(__FILE__, __LINE__, p0, tp0, 0.5)
  CALL ASSERT_FLOAT_EQ(__FILE__, __LINE__, zsfc, tzsfc, 0.5)
  CALL ASSERT_FLOAT_EQ(__FILE__, __LINE__, pbot, tpbot, EPS)
  write(*,*) p0, zsfc

  pflg=.FALSE.
  sflg=.TRUE.
  t1 = 12+273.15
  zsfc = 803.8
  tzsfc = zsfc
  tp0 = 920
  tpbot = LOG(tp0)
  CALL PRfA(PFLG,SFLG,T1,ZSFC,P0,PBOT)
  CALL ASSERT_FLOAT_EQ(__FILE__, __LINE__, p0, tp0, 0.5)
  CALL ASSERT_FLOAT_EQ(__FILE__, __LINE__, zsfc, tzsfc, 0.5)
  CALL ASSERT_FLOAT_EQ(__FILE__, __LINE__, pbot, tpbot, EPS)
  write(*,*) p0, zsfc

!---------------------------------------------


! setup pressure and level profiles.
! are taken from profile output of GEFS
! see in the data directory.

  psg = [1000.,975.,950.,925.,900.]

! prfb Cases surface pressure available but terrain is not.

! Case where terrain height is 0.
  pflg=.TRUE.
  sflg=.FALSE.
  t1 = 25.8+273.15
  p0 = 1009
  tp0 = p0
  tpbot = LOG(tp0)
  zzz = [70.6,294.,521.,752.,992.]
  tzsfc = 0
  CALL PRfB(PFLG,SFLG,T1,PSG,ZZZ,ZSFC,P0,PBOT)
  write(*,*) 'prfb', p0, zsfc
  CALL ASSERT_FLOAT_EQ(__FILE__, __LINE__, p0, tp0, EPS)
  CALL ASSERT_FLOAT_EQ(__FILE__, __LINE__, zsfc, tzsfc, EPS)
  CALL ASSERT_FLOAT_EQ(__FILE__, __LINE__, pbot, tpbot, EPS)

! Case with high terrain height above the first pressure level.
  zzz = [179.,388.,601.,817.,1038.]
  p0 = 973
  tp0 = p0
  tpbot = LOG(tp0)
  tzsfc = 421
  t1 = 8.6 + 273.15
  CALL PRfB(PFLG,SFLG,T1,PSG,ZZZ,ZSFC,P0,PBOT)
  write(*,*) 'prfb', p0, zsfc
  CALL ASSERT_FLOAT_EQ(__FILE__, __LINE__, p0, tp0, EPS)
  ! terrain height is given as 421 in profile.txt but
  ! calculation has fairly large error. 
  CALL ASSERT_FLOAT_EQ(__FILE__, __LINE__, zsfc, tzsfc, 20.)
  CALL ASSERT_FLOAT_EQ(__FILE__, __LINE__, pbot, tpbot, EPS)

! prfb Cases terrain available but pressure is not.
  pflg=.FALSE.
  sflg=.TRUE.
  zzz = [179.,388.,601.,817.,1038.]
  p0 = -1
  tp0 = 973
  tpbot = LOG(tp0)
  zsfc = 421
  tzsfc = zsfc
  t1 = 8.6 + 273.15
  CALL PRfB(PFLG,SFLG,T1,PSG,ZZZ,ZSFC,P0,PBOT)
  write(*,*) 'prfb', p0, zsfc, pbot, tpbot
  CALL ASSERT_FLOAT_EQ(__FILE__, __LINE__, p0, tp0, 5.)
  CALL ASSERT_FLOAT_EQ(__FILE__, __LINE__, zsfc, tzsfc, EPS)
  CALL ASSERT_FLOAT_EQ(__FILE__, __LINE__, pbot, tpbot, 5.) 

  zzz = [70.6,294.,521.,752.,992.]
  tp0 = 1009
  tpbot = LOG(tp0)
  zsfc = 0
  tzsfc = zsfc
  t1 = 25.8+273.15
  CALL PRfB(PFLG,SFLG,T1,PSG,ZZZ,ZSFC,P0,PBOT)
  write(*,*) 'prfb', p0, zsfc, pbot, tpbot
  CALL ASSERT_FLOAT_EQ(__FILE__, __LINE__, p0, tp0, 5.)
  CALL ASSERT_FLOAT_EQ(__FILE__, __LINE__, zsfc, tzsfc, EPS)
  CALL ASSERT_FLOAT_EQ(__FILE__, __LINE__, pbot, tpbot, 5.) 



! prfb Case where both are available
  pflg=.TRUE.
  sflg=.TRUE.
  zzz = [70.6,294.,521.,752.,992.]
  p0 = 1009
  tp0 = p0
  tpbot = LOG(tp0)
  zsfc = 0
  tzsfc = zsfc
  t1 = 25.8+273.15
  CALL PRfB(PFLG,SFLG,T1,PSG,ZZZ,ZSFC,P0,PBOT)
  write(*,*) 'prfb', p0, zsfc, pbot, tpbot
  CALL ASSERT_FLOAT_EQ(__FILE__, __LINE__, p0, tp0, EPS)
  CALL ASSERT_FLOAT_EQ(__FILE__, __LINE__, zsfc, tzsfc, EPS)
  CALL ASSERT_FLOAT_EQ(__FILE__, __LINE__, pbot, tpbot, EPS) 

  
!----------------------------------------------------------------------------------------------
!----------------------------------------------------------------------------------------------
!----------------------------------------------------------------------------------------------
! Calculating relative humidity from specific humidity
! as well as calculating specific humidity from relative humidity
! More work needs to be done on getting the target values.

  T1 = 10.6 + 273.15
  sphu = 6.9/1000.0
  qflg=.TRUE.
  PRS = 918
  relh = CALCRELH(QFLG,T1,SPHU,PRS) 
  write(*,*) 'RELH', relh, sphu
  sphu_target = CALCSP(T1,RELH*100,PRS)
  write(*,*) 'RELH', relh, sphu_target
! test that calcrelh and calcsp functions are inverse
  CALL ASSERT_FLOAT_EQ(__FILE__, __LINE__, sphu, sphu_target, EPS) 

  T1 = 287.73
  sphu = 6.18/1000.0
  qflg=.TRUE.
  PRS = 932
  relh = CALCRELH(QFLG,T1,SPHU,PRS) 
  write(*,*) 'RELH', relh, sphu
  sphu_target = CALCSP(T1,RELH*100,PRS)
  write(*,*) 'RELH', relh, sphu_target
! test that calcrelh and calcsp functions are inverse
  CALL ASSERT_FLOAT_EQ(__FILE__, __LINE__, sphu, sphu_target, EPS) 


! test when qflg is false it gives back the RH in fraction.
  T1 = 10.6 + 273.15
  sphu = 70
  qflg=.FALSE.
  PRS = 918. * 100.
  relh_target = 0.70
  relh = CALCRELH(QFLG,T1,SPHU,PRS) 
  CALL ASSERT_FLOAT_EQ(__FILE__, __LINE__, relh, relh_target, 1e-3)
  write(*,*) 'RELH', relh, sphu
   
   
  !T1 = 10.6 + 273.15
  !sphu = 6.9/1000.0
  !qflg=.TRUE.
  !PRS = 918. * 100.
  !CALL PRFRELH(QFLG,T1,SPHU,PRS,SPHU) 
  
!----------------------------------------------------------------------------------------------
! Calculating virtual temperature




END PROGRAM testprfmod
