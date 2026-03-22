
PROGRAM testpardsp
!======================================================================
! testing for dispersion function
! Possible framework for unit test.
!----------------------------------------------------------------------
! Last Revision: 30 Sep 2020 (AMC) based on old modified version of pardsp.f
!=======================================================================

!  USE funits

  IMPLICIT NONE

  INCLUDE 'DEFARG1.INC'

!------------------------------------------------------------------------------
  INTEGER    :: kret, KL

! PARDSP INPUTS----------------------------------------------------------------
!  TYPE(aset)                   :: meto        ! surface advection variables

  REAL       :: umix            ! u-component turbulence (m2/s2)
  REAL       :: vmix            ! v-component turbulence (m2/s2)
  REAL       :: gdisx           ! grid spacing (m)
  REAL       :: gdisy           ! grid spacing (m)
  REAL       :: zndx            ! vertical fractional index position
  REAL       :: tl              ! lagrangian time scale

!  TYPE(bset),      ALLOCATABLE :: metz (:)    ! profile advection variables
   REAL,      ALLOCATABLE :: wmix(:)  ! w-vertical turbulence profile (m2/s2)

! PARDSP INPUTS--------------------------------------------------------------
  REAL             :: umn, vmn ! velocity components at particle position
  REAL             :: dt,zmdl,zsfc
  INTEGER          :: nlvl, seed_id, krand,hdwpx
  REAL,            ALLOCATABLE :: zsg(:)      ! sigmas
  REAL,            ALLOCATABLE :: rannumb(:)  ! random number generator variables.
  INTEGER, PARAMETER :: MAXRAND = 20 

  REAL                 :: sigu, sigv, sigw, xpos, ypos, zpos

! Target variables -------------------------------------------------
! INOUT of PARDSP
! xpos
! ypos
! zpos
! vprime / sigv
! wratio / sigw
! uprime / sigu
! iseed  / seed_id

  REAL                 :: tsigu, tsigv, tsigw, txpos, typos, tzpos
  INTEGER              :: tseed_id

!COMMON BLOCK----------------------------------------------------
! just to get zmix
!  REAL       :: fvel         ! scalar friction velocity (m/s)
!  REAL       :: ustr         ! u- friction velocity (m/s)
!  REAL       :: vstr         ! v- friction velocity (m/s)
!  REAL       :: tstr         ! friction temperature (deg K)
!  REAL       :: wstr         ! convective velocity scale (m/s)
!  REAL       :: slen         ! Obukhov stability length (m)
!  REAL       :: zmix         ! mixed layer depth (m)
!  REAL       :: psi          ! integrated stability function heat

  REAL       :: aa,bb,cc          ! vertical grid polynomial
  REAL       :: hscale            ! horz time scale (sec)
  REAL       :: vscale            ! vert time scale (sec)

  REAL       :: aa2, bb2, cc2
  REAL       :: hscale2
  REAL       :: vscale2

! NOT SURE THIS IS USED AT ALL.
! CODE that used it is commented out.
! COMMON /stbcom/ fvel,ustr,vstr,tstr,wstr,slen,zmix,psi
  COMMON /ZZTOKK/ AA,BB,CC
  COMMON /stblen/ vscale,hscale
!------------------------------------------------------------------------------

! setup input variables (TO DO pick values)

  umn  = 1
  vmn  = 1
  umix = 1
  vmix = 1
  gdisx = 1
  gdisy = 1
  dt   = 1
  zmdl = 1
  zsfc = 1
  nlvl = 5
  xpos = 1
  ypos = 1
  zpos = 1

  sigu = 1
  sigv = 1
  sigw = 1

  krand = 1
 
!---------------------------------------------
! setup common block variables (TO DO pick values)

  AA   = 1
  BB   = 1
  CC   = 1
  vscale = 1
  hscale = 1

! create copies of common block variables for testing
  aa2 = aa
  bb2 = bb
  cc2 = cc
  hscale2 = hscale
  vscale2 = vscale

!---------------------------------------------
! SETUP target values (TO DO pick values)
  txpos = 1
  typos = 1
  tzpos = 1

  tsigu = 1
  tsigv = 1
  tsigw = 1
  tseed_id = 1
!---------------------------------------------
! Allocate and fill arrays (TO DO  pick values)
  ALLOCATE (wmix(nlvl),STAT=kret)
  ALLOCATE (zsg(nlvl),STAT=kret)
  ALLOCATE (rannumb(maxrand),STAT=kret)
 
  DO KL=1,NLVL
     WMIX(KL) = KL/10.0
     ZSG(KL) = KL
  END DO
  DO KL=1,maxrand
     rannumb(KL) = KL
  END DO
!---------------------------------------------
  CALL PARDSP(UMN,VMN,UMIX,VMIX,GDISX,GDISY,DT,ZMDL,ZSFC,    &
              NLVL,WMIX,ZSG,XPOS,YPOS,ZPOS,        &
              SIGV,SIGW,SIGU,HDWPX,ZNDX,TL,seed_id,&
              RANNUMB,MAXRAND,KRAND)

! INOUT
! xpos
! ypos
! zpos
! vprime / sigv
! wratio / sigw
! uprime / sigu
! iseed  / seed_id

! -----------------------------------------------
! test that no common block variables are modified.
  IF(hscale.ne.hscale2)STOP 2
  IF(vscale.ne.vscale2)STOP 2
  IF(aa.ne.aa2)STOP 2
  IF(cc.ne.cc2)STOP 2
  IF(bb.ne.bb2)STOP 2

! -----------------------------------------------
! Test values that should be modified
  WRITE(*,*) xpos
  WRITE(*,*) ypos
  WRITE(*,*) zpos
  WRITE(*,*) sigu, sigv, sigw
  IF(SIGU.NE.TSIGU) STOP2
  IF(SIGV.NE.TSIGV) STOP2
  IF(SIGW.NE.TSIGW) STOP2
  IF(XPOS.NE.TXPOS) STOP2
  IF(YPOS.NE.TYPOS) STOP2
  IF(ZPOS.NE.TZPOS) STOP2
  IF(SEED_ID.NE.TSEED_ID) STOP2

! -----------------------------------------------

  DEALLOCATE(rannumb, wmix, zsg)


END PROGRAM testpardsp
