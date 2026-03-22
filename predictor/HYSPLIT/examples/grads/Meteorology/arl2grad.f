PROGRAM ARL2GRAD

!-------------------------------------------------------------------------------
! convert ARL packed meteorological data to Grads format using
! hysplit4 source code library routines
!-------------------------------------------------------------------------------
! Last Revision: 14 Jan 2000 (RRD)
!                24 Nov 2000 (RRD) - correction on lcc test
!                29 Nov 2000 (RRD) - vertical structure change
!                18 Dec 2000 (RRD) - fortran90 upgrade
!                18 Oct 2001 (RRD) - defgrid.inc modification
!                10 Nov 2003 (RRD) - modified argument list metini
!                10 Dec 2005 (GDR) - added u/v wind rotation to LCC grids
!-------------------------------------------------------------------------------

  INCLUDE 'DEFGRID.INC' 
	              
  LOGICAL       :: BACK,CNTL,WNDTRU
  CHARACTER(4)  :: VARB,VARB2,AVARB(40,30)
  CHARACTER(50) :: LABEL
  INTEGER       :: NFLDS(40)
  REAL          :: HGT(30)
  CHARACTER(1), ALLOCATABLE :: CDATA(:)
  REAL,         ALLOCATABLE :: RVAR(:,:)     
  REAL,         ALLOCATABLE :: WIND(:,:)     

  COMMON /GBLGRD/ GRID, DREC, FILE

  character fname*80,dummy*80,months(12)*3, proj*3, model*4, outnam*20, cdef*4
  character varbi*4,grdid*2
  integer nvar
  data months/'jan','feb','mar','apr','may','jun',                             &
              'jul','aug','sep','oct','nov','dec'/
  data back/.false./,kvel/0/
  data outnam/'dummy'/
  data wndtru/.false./

!---------------------------------------------------------------------
  INTERFACE
  SUBROUTINE PAKINP(RVAR,CVAR,NX,NY,NX1,NY1,LX,LY,PREC,NEXP,VAR1,KSUM)
  REAL,          INTENT(OUT)   :: rvar (:,:)     ! real data unpacked
  CHARACTER(1),  INTENT(IN)    :: cvar (:)       ! packed input of NX*NY
  INTEGER,       INTENT(IN)    :: nx,ny          ! size of input array  
  INTEGER,       INTENT(IN)    :: nx1,ny1        ! optional sub-grid left edge 
  INTEGER,       INTENT(IN)    :: lx,ly          ! length of sub-grid
  REAL,          INTENT(IN)    :: prec           ! precision of packed data 
  INTEGER,       INTENT(IN)    :: nexp           ! packing scaling exponent
  REAL,          INTENT(IN)    :: var1           ! value of array at (1,1)
  INTEGER,       INTENT(INOUT) :: ksum           ! rotating checksum 
  END SUBROUTINE pakinp
  END INTERFACE
!---------------------------------------------------------------------

  100 FORMAT(6I2,2X,A4,I4,2E14.7)

! check for command line arguments

      narg=iargc()

      if(narg.EQ.0)then
         WRITE(*,*)'Usage: arl2grad {/meteo_dir_name/} {arl_filename} {optional output filename}'
         STOP
      END IF
      CALL GETARG(1,fname)
      FILE(1,1)%DIR=fname
      CALL GETARG(2,fname)
      FILE(1,1)%METEO=fname
      IF(NARG.EQ.3)then
        CALL GETARG(3,outnam)
        ibout=0
        ieout=0
        do ii=1,20
          if(outnam(ii:ii).ne.' ')then
            if(ibout.eq.0)ibout=ii
            ieout=ii
!         else if(ibout.ne.0)then
!           ieout=ii-1
          end if 
        end do
      END IF

! check for grads data control file in local directory
      INQUIRE(FILE='GRADS.CTL',EXIST=CNTL)
      NTIME=-1
      IF(CNTL)THEN
         OPEN(29,FILE='GRADS.CTL')
!        number of time periods to process
         READ(29,'(i3,a)')NTIME,label
         NVAR=30
         NL=1
 120     READ(29,'(F6.0,I3,99(1X,A4))',END=140)HGT(NL),NVAR,(AVARB(NV,NL),NV=1,NVAR)
         NFLDS(NL)=NVAR
         NL=NL+1
         GO TO 120
 140     ICLEV=NL-2
         CLOSE(29)
      END IF

!=>read and initialize data file

      ISOT=0
      NGRD=1
      NTIM=1
      OLAT=0.0
      BACK=.FALSE.
      CALL TMINIT
      CALL METINI(ISOT,NGRD,NTIM,OLAT,IBYR,IBMO,IBDA,IBHR,BACK,KVEL) 
!=>input file time information

      kyr=FILE(1,1)%FIRST%YR
      kmo=FILE(1,1)%FIRST%MO
      kda=FILE(1,1)%FIRST%DA
      khr=FILE(1,1)%FIRST%HR
      if(kyr.lt.50)then
         kyear=2000+kyr
      else
         kyear=1900+kyr
      end if
      write(6,*)kyear,kmo,kda,khr
      ntim=FILE(1,1)%ENDREC/DREC(1,1)%REC_PER
      kdel=DREC(1,1)%DELTA/60

!=>input file grid projection information

      pollat = GRID(1,1)%POLE_LAT
      pollon = GRID(1,1)%POLE_LON
      reflat = GRID(1,1)%REF_LAT
      reflon = GRID(1,1)%REF_LON
      gsize  = GRID(1,1)%SIZE
      orient = GRID(1,1)%ORIENT
      tanlat = GRID(1,1)%TANG_LAT
      model  = GRID(1,1)%MODEL_ID
      ib=0
      ie=0
      do ii=1,4
        if(model(ii:ii).ne.' ')then
          if(ib.eq.0)ib=ii
          ie=ii
        else if(ib.ne.0)then
          ie=ii-1
        end if 
      end do

!     Special case for GFS NPS and SPS grids (gsize must be true at 60N/60S for GrADS?)
!     and we defined GFSN and GFSS to be true at 90N/90S
!     if(model(ib:ie).eq.'GFSN'.or.model(ib:ie).eq.'GFSS')then
!        if(gsize.lt.100.0)then
!            gsize=88.869
!        else
!            gsize=177.739
!        end if
!     end if 

!     north polar sterographic
      IF(TANLAT.EQ. 90.0)THEN
        proj='nps'
        dlat=gsize/111.0
        dlon=dlat
        wndtru=.false.

!     south polar sterographic
      ELSEIF(TANLAT.EQ.-90.0)THEN
        proj='sps'
        dlat=gsize/111.0
        dlon=dlat
        wndtru=.false.

!     lambert conformal
      ELSE
        proj='lcc'
        gsize=gsize*1000.0
        dlat=gsize/111000.0
        dlon=dlat
        wndtru=.true.
      END IF

!     number of grid points
      NXP=GRID(1,1)%NX
      NYP=GRID(1,1)%NY
      NZP=GRID(1,1)%NZ
      NXYP=NXP*NYP

!     allocate space
      ALLOCATE (CDATA(NXYP), STAT=KRET)
      ALLOCATE (RVAR(NXP,NYP), STAT=KRET)     
      ALLOCATE (WIND(NXP,NYP), STAT=KRET)     

!     determine maximum bounds of lat/lon
      clat=90.0
      tlat=0.0
      clon=360.0
      tlon=0.0
      DO JJ=1,NYP
      DO II=1,NXP
        CALL CXY2LL(GRID(1,1)%GBASE,FLOAT(II),FLOAT(JJ),XLAT,XLON)
        CLAT=MIN(CLAT,XLAT) 
        TLAT=MAX(TLAT,XLAT)
        CLON=MIN(CLON,XLON+180.0)
        TLON=MAX(TLON,XLON+180.0)
      END DO
      END DO
      cxp=0.5*(1+nxp)
      cyp=0.5*(1+nyp)
      CALL CXY2LL(GRID(1,1)%GBASE,CXP,CYP,XLAT,XLON)
      lat1=nint(clat)
      lat2=nint(tlat)
      lon1=nint(clon)
      lon2=nint(tlon)
      nlat=(lat2-lat1)/dlat
      nlon=(lon2-lon1)/dlon
      CLON=CLON-180.0

!=>define grads interpolated grid
      if(model(ib:ie).eq.'AVN'.or.model(ib:ie).eq.'FNL')then
        nlat=181
        nlon=720
        clat=0.
        clon=0.
!     special case for LAT/LON GFS grids
      else if(model(ib:ie).eq.'GFSG')then
        wndtru=.false.
        if(reflat.eq.1.0)then
!          1 degree data
           dlat=1.0
           dlon=1.0
           nlat=181
           nlon=360
           clat=-90.
           clon=0.
        else
!          2.5 degree data
           dlat=2.5
           dlon=2.5
           nlat=73
           nlon=144
           clat=-90.
           clon=0.
        end if
      end if

!     open grads file
      LREC=4*(FILE(1,1)%REC_LEN-50)
      IF(OUTNAM(1:5).EQ.'dummy')THEN
        OPEN(25,FILE=model(ib:ie)//'.grd',RECL=LREC,ACCESS='DIRECT',FORM='UNFORMATTED')
        OPEN(30,FILE=model(ib:ie)//'.ctl')
        WRITE(30,'(a)')'dset ^'//model(ib:ie)//'.grd'
      ELSE
        OPEN(25,FILE=outnam(ibout:ieout)//'.grd',RECL=LREC,ACCESS='DIRECT',FORM='UNFORMATTED')
        OPEN(30,FILE=outnam(ibout:ieout)//'.ctl')
        WRITE(30,'(a)')'dset ^'//outnam(ibout:ieout)//'.grd'
      END IF

!=>go through data by record

      IREC=0
      NREC=0
      VARB='INDX'
      
      IF(NTIME.GT.0.AND.NTIME.LE.NTIM)NTIM=NTIME
      DO KT=1,NTIM
!        read the index record for each time period
!        skip any extra index records (due to smaller grid than is needed for 
!        fields/levels in index record
         DO WHILE (VARB.EQ.'INDX')
           IREC=IREC+1
           READ(FILE(1,1)%KUNIT,REC=IREC)LABEL,(CDATA(K),K=1,NXYP)
           READ(LABEL,100)IY,IM,ID,IH,IC,LL,VARB,NEXP,PREC,VAR1
         END DO
         IREC=IREC-1

!=>convert surface data first

         NVAR=DREC(1,1)%NUM_VARB(1)
!        loop through the number of variables
         DO NN=1,NVAR
            IREC=IREC+1
            READ(FILE(1,1)%KUNIT,REC=IREC)LABEL,(CDATA(K),K=1,NXYP)
            READ(LABEL,100)IY,IM,ID,IH,IC,LL,VARB,NEXP,PREC,VAR1
            KSUM=-1

!           output to grads file
            IF(NTIME.GE.0)THEN
               DO NF1=1,NFLDS(1)
                 IF(VARB.EQ.AVARB(NF1,1))THEN
                   CALL PAKINP(RVAR,CDATA,NXP,NYP,1,1,NXP,NYP,PREC,NEXP,VAR1,KSUM)
!                  rotate winds to true
                   IF(WNDTRU)THEN
                     IF(VARB.EQ.'U10M')THEN
                         READ(FILE(1,1)%KUNIT,REC=IREC+1)LABEL,(CDATA(K),K=1,NXYP)
                         READ(LABEL,100)IY,IM,ID,IH,IC,LL,VARB2,NEXP,PREC,VAR1
                         IF(VARB2.EQ.'V10M')THEN 
                            CALL PAKINP(WIND,CDATA,NXP,NYP,1,1,NXP,NYP,PREC,NEXP,VAR1,KSUM)
                            CALL WINDU(RVAR,WIND,NXP,NYP)
                         END IF
                     ELSEIF(VARB.EQ.'V10M')THEN
                         READ(FILE(1,1)%KUNIT,REC=IREC-1)LABEL,(CDATA(K),K=1,NXYP)
                         READ(LABEL,100)IY,IM,ID,IH,IC,LL,VARB2,NEXP,PREC,VAR1
                         IF(VARB2.EQ.'U10M')THEN 
                            CALL PAKINP(WIND,CDATA,NXP,NYP,1,1,NXP,NYP,PREC,NEXP,VAR1,KSUM)
                            CALL WINDV(RVAR,WIND,NXP,NYP)
                         END IF
                     END IF
                   END IF
                   nrec=nrec+1
                   IF(IC.EQ.-1.OR.VARB.EQ.'NULL')THEN
                      CALL MGRADS(RVAR,NREC,NXP,NYP)
                   ELSE
                      CALL WGRADS(RVAR,NREC,NXP,NYP)
                   END IF
                 END IF
               END DO
            ELSE
               CALL PAKINP(RVAR,CDATA,NXP,NYP,1,1,NXP,NYP,PREC,NEXP,VAR1,KSUM)
!              rotate winds to true
               IF(WNDTRU)THEN
                 IF(VARB.EQ.'U10M')THEN
                    READ(FILE(1,1)%KUNIT,REC=IREC+1)LABEL,(CDATA(K),K=1,NXYP)
                    READ(LABEL,100)IY,IM,ID,IH,IC,LL,VARB2,NEXP,PREC,VAR1
                    IF(VARB2.EQ.'V10M')THEN 
                       CALL PAKINP(WIND,CDATA,NXP,NYP,1,1,NXP,NYP,PREC,NEXP,VAR1,KSUM)
                       CALL WINDU(RVAR,WIND,NXP,NYP)
                    END IF
                 ELSEIF(VARB.EQ.'V10M')THEN
                    READ(FILE(1,1)%KUNIT,REC=IREC-1)LABEL,(CDATA(K),K=1,NXYP)
                    READ(LABEL,100)IY,IM,ID,IH,IC,LL,VARB2,NEXP,PREC,VAR1
                    IF(VARB2.EQ.'U10M')THEN 
                       CALL PAKINP(WIND,CDATA,NXP,NYP,1,1,NXP,NYP,PREC,NEXP,VAR1,KSUM)
                       CALL WINDV(RVAR,WIND,NXP,NYP)
                    END IF
                 END IF
               END IF
               nrec=nrec+1
               IF(IC.EQ.-1.OR.VARB.EQ.'NULL')THEN
                  CALL MGRADS(RVAR,NREC,NXP,NYP)
               ELSE
                  CALL WGRADS(RVAR,NREC,NXP,NYP)
               END IF
            END IF
         END DO

!=>convert upper level variables

!        all levels assumed to have same number of variables
!     ***changed to allow less fields with increasing level***
         NVAR=DREC(1,1)%NUM_VARB(2)

         DO NN=1,NVAR
!           shift pointer to next variable first level
            IREC=IREC+1
            JREC=IREC
            READ(FILE(1,1)%KUNIT,REC=JREC)LABEL,(CDATA(K),K=1,NXYP)
            READ(LABEL,100)IY,IM,ID,IH,IC,LL,VARB,NEXP,PREC,VAR1
            VARBI=VARB
            IHI=IH
            KSUM=-1
!           output to grads file
            IF(NTIME.GE.0)THEN
              IF(HGT(2).EQ.DREC(1,1)%HEIGHT(2))THEN
                DO NF1=1,NFLDS(2)
                  IF(VARB.EQ.AVARB(NF1,2))THEN
                    CALL PAKINP(RVAR,CDATA,NXP,NYP,1,1,NXP,NYP,PREC,NEXP,VAR1,KSUM)
!                   rotate winds to true
                    IF(WNDTRU)THEN
                      IF(VARB.EQ.'UWND')THEN
                         READ(FILE(1,1)%KUNIT,REC=JREC+1)LABEL,(CDATA(K),K=1,NXYP)
                         READ(LABEL,100)IY,IM,ID,IH,IC,LL,VARB2,NEXP,PREC,VAR1
                         IF(VARB2.EQ.'VWND')THEN 
                            CALL PAKINP(WIND,CDATA,NXP,NYP,1,1,NXP,NYP,PREC,NEXP,VAR1,KSUM)
                            CALL WINDU(RVAR,WIND,NXP,NYP)
                         END IF
                      ELSEIF(VARB.EQ.'VWND')THEN
                         READ(FILE(1,1)%KUNIT,REC=JREC-1)LABEL,(CDATA(K),K=1,NXYP)
                         READ(LABEL,100)IY,IM,ID,IH,IC,LL,VARB2,NEXP,PREC,VAR1
                         IF(VARB2.EQ.'UWND')THEN 
                            CALL PAKINP(WIND,CDATA,NXP,NYP,1,1,NXP,NYP,PREC,NEXP,VAR1,KSUM)
                            CALL WINDV(RVAR,WIND,NXP,NYP)
                         END IF
                      END IF
                    END IF
                    nrec=nrec+1
                    IF(IC.EQ.-1.OR.VARB.EQ.'NULL')THEN
                        write(6,*)'missing ',VARBI,' at level 1'
                        CALL MGRADS(RVAR,NREC,NXP,NYP)
                     ELSE
                        CALL WGRADS(RVAR,NREC,NXP,NYP)
                     END IF
                  END IF
                END DO
              END IF
            ELSE
              CALL PAKINP(RVAR,CDATA,NXP,NYP,1,1,NXP,NYP,PREC,NEXP,VAR1,KSUM)
!             rotate winds to true
              IF(WNDTRU)THEN
                IF(VARB.EQ.'UWND')THEN
                    READ(FILE(1,1)%KUNIT,REC=JREC+1)LABEL,(CDATA(K),K=1,NXYP)
                    READ(LABEL,100)IY,IM,ID,IH,IC,LL,VARB2,NEXP,PREC,VAR1
                    IF(VARB2.EQ.'VWND')THEN 
                       CALL PAKINP(WIND,CDATA,NXP,NYP,1,1,NXP,NYP,PREC,NEXP,VAR1,KSUM)
                       CALL WINDU(RVAR,WIND,NXP,NYP)
                    END IF
                ELSEIF(VARB.EQ.'VWND')THEN
                    READ(FILE(1,1)%KUNIT,REC=JREC-1)LABEL,(CDATA(K),K=1,NXYP)
                    READ(LABEL,100)IY,IM,ID,IH,IC,LL,VARB2,NEXP,PREC,VAR1
                    IF(VARB2.EQ.'UWND')THEN 
                       CALL PAKINP(WIND,CDATA,NXP,NYP,1,1,NXP,NYP,PREC,NEXP,VAR1,KSUM)
                       CALL WINDV(RVAR,WIND,NXP,NYP)
                    END IF
                END IF
              END IF
              nrec=nrec+1
              IF(IC.EQ.-1.OR.VARB.EQ.'NULL')THEN
                 write(6,*)'missing ',VARBI,' at level 1'
                 CALL MGRADS(RVAR,NREC,NXP,NYP)
              ELSE
                 CALL WGRADS(RVAR,NREC,NXP,NYP)
              END IF
            END IF
            KLV=3
            DO KK=2,NZP-1
             IFND=0
             DO WHILE(LL.LE.KK.AND.IH.EQ.IHI)
               JREC=JREC+1
               READ(FILE(1,1)%KUNIT,REC=JREC,ERR=999)LABEL,(CDATA(K),K=1,NXYP)
               READ(LABEL,100)IY,IM,ID,IH,IC,LL,VARB,NEXP,PREC,VAR1
               IF(LL.EQ.KK.AND.VARB.EQ.VARBI)THEN
                 IFND=1
                 KSUM=-1
!                output to grads file
                 IF(NTIME.GE.0)THEN
                   IF(HGT(KLV).EQ.DREC(1,1)%HEIGHT(KK+1))THEN
                     DO NF1=1,NFLDS(KLV)
                       IF(VARB.EQ.AVARB(NF1,KLV))THEN
                          CALL PAKINP(RVAR,CDATA,NXP,NYP,1,1,NXP,NYP,PREC,NEXP,VAR1,KSUM)
!                         rotate winds to true
                          IF(WNDTRU)THEN
                            IF(VARB.EQ.'UWND')THEN
                               READ(FILE(1,1)%KUNIT,REC=JREC+1)LABEL,(CDATA(K),K=1,NXYP)
                               READ(LABEL,100)IY,IM,ID,IH,IC,LL,VARB2,NEXP,PREC,VAR1
                               IF(VARB2.EQ.'VWND')THEN 
                                  CALL PAKINP(WIND,CDATA,NXP,NYP,1,1,NXP,NYP,PREC,NEXP,VAR1,KSUM)
                                  CALL WINDU(RVAR,WIND,NXP,NYP)
                               END IF
                            ELSEIF(VARB.EQ.'VWND')THEN
                               READ(FILE(1,1)%KUNIT,REC=JREC-1)LABEL,(CDATA(K),K=1,NXYP)
                               READ(LABEL,100)IY,IM,ID,IH,IC,LL,VARB2,NEXP,PREC,VAR1
                               IF(VARB2.EQ.'UWND')THEN 
                                  CALL PAKINP(WIND,CDATA,NXP,NYP,1,1,NXP,NYP,PREC,NEXP,VAR1,KSUM)
                                  CALL WINDV(RVAR,WIND,NXP,NYP)
                               END IF
                            END IF
                          END IF
                          nrec=nrec+1
                          IF(IC.EQ.-1.OR.VARB.EQ.'NULL')THEN
                            write(6,*)'missing ',VARBI,' at level ',kk
                            CALL MGRADS(RVAR,NREC,NXP,NYP)
                          ELSE 
                            CALL WGRADS(RVAR,NREC,NXP,NYP)
                          END IF
                       END IF
                     END DO
                     KLV=KLV+1
                   END IF
                 ELSE
                   CALL PAKINP(RVAR,CDATA,NXP,NYP,1,1,NXP,NYP,PREC,NEXP,VAR1,KSUM)
!                  rotate winds to true
                   IF(WNDTRU)THEN
                     IF(VARB.EQ.'UWND')THEN
                         READ(FILE(1,1)%KUNIT,REC=JREC+1)LABEL,(CDATA(K),K=1,NXYP)
                         READ(LABEL,100)IY,IM,ID,IH,IC,LL,VARB2,NEXP,PREC,VAR1
                         IF(VARB2.EQ.'VWND')THEN 
                            CALL PAKINP(WIND,CDATA,NXP,NYP,1,1,NXP,NYP,PREC,NEXP,VAR1,KSUM)
                            CALL WINDU(RVAR,WIND,NXP,NYP)
                         END IF
                     ELSEIF(VARB.EQ.'VWND')THEN
                         READ(FILE(1,1)%KUNIT,REC=JREC-1)LABEL,(CDATA(K),K=1,NXYP)
                         READ(LABEL,100)IY,IM,ID,IH,IC,LL,VARB2,NEXP,PREC,VAR1
                         IF(VARB2.EQ.'UWND')THEN 
                            CALL PAKINP(WIND,CDATA,NXP,NYP,1,1,NXP,NYP,PREC,NEXP,VAR1,KSUM)
                            CALL WINDV(RVAR,WIND,NXP,NYP)
                         END IF
                     END IF
                   END IF
                   nrec=nrec+1
                   IF(IC.EQ.-1.OR.VARB.EQ.'NULL')THEN
                      write(6,*)'missing ',VARBI,' at level ',kk
                      CALL MGRADS(RVAR,NREC,NXP,NYP)
                   ELSE 
                      CALL WGRADS(RVAR,NREC,NXP,NYP)
                   END IF
                 END IF
               END IF
             END DO
999          IF(IFND.EQ.0)then
               IF(NTIME.GE.0)THEN
                 IF(HGT(KLV).EQ.DREC(1,1)%HEIGHT(KK+1))THEN
                     DO NF1=1,NFLDS(KLV)
                       IF(VARBI.EQ.AVARB(NF1,KLV))THEN
                         nrec=nrec+1
                         write(6,*)'missing ',VARBI,' at level ',kk
                         CALL MGRADS(RVAR,NREC,NXP,NYP)
                       END IF
                     END DO
                 END IF
               ELSE
                 nrec=nrec+1
                 write(6,*)'missing ',VARBI,' at level ',kk
                 CALL MGRADS(RVAR,NREC,NXP,NYP)
               END IF
             end if
             JREC=JREC-1
            END DO
         END DO
!        shift pointer to last record of time period
         IREC=JREC

      END DO
      CLOSE(25)

!=>create grads control script files 
!     define grid number for GrADS toolbox
      if(model(ib:ie).eq.'AVN')then
          grdid=' 1'
      elseif(model(ib:ie).eq.'NAM'.or.model(ib:ie).eq.'ETA')then
          grdid=' 2'
      elseif(model(ib:ie).eq.'RUC')then
          grdid=' 3'
      elseif(model(ib:ie).eq.'NGM')then
          grdid=' 4'
      elseif(model(ib:ie).eq.'MM5')then
          grdid=' 5'
      elseif(model(ib:ie).eq.'MRF')then
          grdid=' 6'
      elseif(model(ib:ie).eq.'EDAS')then
          grdid=' 7'
      elseif(model(ib:ie).eq.'FNL')then
          grdid=' 8'
      elseif(model(ib:ie).eq.'GFSG'.or.model(ib:ie).eq.'GFSN'.or.model(ib:ie).eq.'GFSS')then
          grdid=' 9'
      else
          grdid='50'
      end if
      write(30,'(a)')'title '//grdid//' '//model//' Meteorological Data'
      write(30,'(a)')'undef -9.99E33'
      write(30,'(a)')'options big_endian'

      IF(PROJ.EQ.'nps'.OR.PROJ.EQ.'sps')THEN
!        find x,y position of pole
         CALL CLL2XY(GRID(1,1)%GBASE,POLLAT,POLLON,XP,YP)
         write(30,'(a)')'* pdef isize jsize projtype ipole jpole lonref gridinc'
         write(30,'(a,2i4,a4,2(1x,f6.1),1x,f6.0,1x,f6.2)')                     &
              'pdef ',nxp,nyp,proj,xp,yp,reflon,gsize
      ELSEIF(model.ne.'GFSG')THEN
         write(30,'(a)')'* pdef isize jsize projtype reflat reflon iref jref tanlatS tanlatN lonref gridincX gridincY'
         write(30,'(a,2i4,a4,4(1x,f6.1),3(1x,f6.0),2(1x,f8.2))')               &
              'pdef ',nxp,nyp,proj,xlat,xlon,cxp,cyp,                          &
               tanlat,tanlat,reflon,gsize,gsize
      END IF

      write(30,'(a,i3,a,1x,f8.2,1x,f5.2)')'xdef ',nlon,' linear ',clon,dlon
      write(30,'(a,i3,a,2(1x,f6.2))')    'ydef ',nlat,' linear ',clat,dlat

      IF(NTIME.GT.0)THEN
         write(30,'(a,i2,a,30f7.0)')'zdef ',iclev,' levels ',(hgt(k),k=2,iclev+1)
      ELSEIF(DREC(1,1)%HEIGHT(1).LT.0.0)THEN
         write(30,'(a,i2,a,30f7.4)')'zdef ',(nzp-1),' levels ',                &
              (DREC(1,1)%HEIGHT(k),k=2,nzp)
      ELSE
         write(30,'(a,i2,a,30f7.0)')'zdef ',(nzp-1),' levels ',                &
              (DREC(1,1)%HEIGHT(k),k=2,nzp)
      END IF

      write(30,'(a,i3,a,i2.2,a1,i2.2,a3,2i4,a)')'tdef ',ntim,' linear ',       &
            khr,'z',kda,months(kmo),kyear,kdel,'hr'
      IF(NTIME.GT.0)THEN
        ntyp=NFLDS(1)+NFLDS(2)
        write(30,'(a,i3)')'vars ',ntyp
        ntyp=NFLDS(1)
      ELSE
        ntyp=DREC(1,1)%NUM_VARB(1)+DREC(1,1)%NUM_VARB(2)
        write(30,'(a,i3)')'vars ',ntyp
        ntyp=DREC(1,1)%NUM_VARB(1)
      END IF
      do k=1,ntyp
         IF(NTIME.GT.0)THEN
           cdef=avarb(k,1)
         ELSE
           cdef=drec(1,1)%varb_id(k,1)
         END IF
         if(cdef.eq.'U10M')then
           write(30,'(a4,a)')cdef,' 0 33,105 u-wind at 10 m'
         elseif(cdef.eq.'V10M')then
           write(30,'(a4,a)')cdef,' 0 34,105 v-wind at 10 m'
         else
           write(30,'(a4,a)')cdef,' 0 99 surface'
         end if
      end do
      IF(NTIME.GT.0)THEN
        ntyp=NFLDS(2)
      ELSE
        ntyp=DREC(1,1)%NUM_VARB(2)
      END IF
      do k=1,ntyp
         IF(NTIME.GT.0)THEN
           cdef=avarb(k,2)
           nlvl1=iclev
         ELSE
           cdef=drec(1,1)%varb_id(k,2)
           nlvl1=nzp-1
         END IF
         if(cdef.eq.'UWND')then
           write(30,'(a4,i3,a)')cdef,nlvl1,' 33,100 u-wind'
         elseif(cdef.eq.'VWND')then
           write(30,'(a4,i3,a)')cdef,nlvl1,' 34,100 v-wind'
         else
           write(30,'(a4,i3,a)')cdef,nlvl1,' 99 upper'
         end if
      end do
      write(30,'(a)')'endvars'
      close(30)

!=>create initial grads display script

!     open(30,file=model(ib:ie)//'.gs')
!     write(dummy,'(2i4)')lat1,lat2
!     write(30,'(3a)')'''set lat',dummy(1:8),''''
!     write(dummy,'(2i5)')lon1,lon2
!     write(30,'(3a)')'''set lon',dummy(1:10),''''
!     write(30,'(a)')'''set mpdset hires'''
!     write(30,'(a)')'''set z 1'''
!     write(30,'(a)')'''set cthick 10'''
!     write(30,'(a)')'''set t 1'''
!     write(30,'(a)')'''set grads off'''
!     write(30,'(3a)')'''d TEMP'''
!     close(30)

      STOP
      END

!-------------------------------------------------------------------------------
!=>subroutine to write out properly formatted grads record

      SUBROUTINE WGRADS(RVAR,NREC,NXP,NYP)
      REAL RVAR(NXP,NYP)
      write(25,rec=nrec)((rvar(ii,jj),ii=1,nxp),jj=1,nyp)
      return
      END
!-------------------------------------------------------------------------------
!=>subroutine to write out missing formatted grads record

      SUBROUTINE MGRADS(RVAR,NREC,NXP,NYP)
      REAL RVAR(NXP,NYP)
      rvar(1,1)=-9.99E+33
      write(25,rec=nrec)((rvar(1,1),ii=1,nxp),jj=1,nyp)
      return
      END
!-------------------------------------------------------------------------------
!=>subroutine to convert winds from grid to true (RVAR=UWNDS)

      SUBROUTINE WINDU(RVAR,WIND,NXP,NYP)

      INCLUDE 'DEFGRID.INC'

      REAL RVAR(NXP,NYP),WIND(NXP,NYP)

      COMMON /GBLGRD/ GRID, DREC, FILE

      write(*,*)'converting u & v wind to true N'
      DO JJ=1,NYP
      DO II=1,NXP
          XCP=FLOAT(II)
          YCP=FLOAT(JJ)
          CALL CG2CXY(GRID(1,1)%GBASE,XCP,YCP,RVAR(II,JJ),WIND(II,JJ),UT,VT)
          RVAR(II,JJ)=UT
          WIND(II,JJ)=VT
      END DO
      END DO
      return
      END
!-------------------------------------------------------------------------------
!=>subroutine to convert winds from grid to true (RVAR=VWNDS)

      SUBROUTINE WINDV(RVAR,WIND,NXP,NYP)

     INCLUDE 'DEFGRID.INC' 

      REAL RVAR(NXP,NYP),WIND(NXP,NYP)

      COMMON /GBLGRD/ GRID, DREC, FILE

      DO JJ=1,NYP
      DO II=1,NXP
          XCP=FLOAT(II)
          YCP=FLOAT(JJ)
          CALL CG2CXY(GRID(1,1)%GBASE,XCP,YCP,WIND(II,JJ),RVAR(II,JJ),UT,VT)
          WIND(II,JJ)=UT
          RVAR(II,JJ)=VT
      END DO
      END DO
      return
      END

