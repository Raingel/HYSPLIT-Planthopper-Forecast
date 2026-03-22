      PROGRAM DAT2ARL
!=========================================================
! Sample program to convert meterological data to standard
! packed form for input into ARL programs: Hysplit4, etc.
! Assume that data are provided on a lat/lon grid on
! pressure surfaces.  Interpolation to conformal grid.
! Data are generated in a sample subroutine that needs to
! be replaced with customized I/O routines for actual data
! See subroutines gettime and getdata
! File Unit Numbers used by this program:
!     20 - packed data output file
!     30 - characteristics of output grid (DATA_CFG)
!=========================================================

      LOGICAL FTEST
      CHARACTER FNAME*80,LABEL*80

!=>input data array limit
      PARAMETER (NLON=360,NLAT=181)
      REAL RVAR(NLON,NLAT)

!=>output data array size (number of grid points)
      PARAMETER (NXP=100, NYP=100)
      PARAMETER (NXY=NXP*NYP)
!     unpacked input and output array
      REAL XVAR(NXP,NYP), YVAR(NXP,NYP)
!     lat/lon grid conversion equivalence table
      REAL TLAT(NXP,NYP),TLON(NXP,NYP)
!     packed output array
      CHARACTER CVAR(NXY)*1

!=>variable descriptor tables
!     number of levels, variables at sfc, variables aloft
      PARAMETER (NLVL=12, MVAR0=2, MVAR1=6)
      CHARACTER*4 VCHAR, VCHAR0(MVAR0), VCHAR1(MVAR1)
!     unit conversion factors
      REAL SIGL(NLVL),   CNVRT0(MVAR0), CNVRT1(MVAR1)

!=>output variables name and conversion factors to ARL format
      DATA VCHAR0 /'PRSS','TPP6'/
      DATA CNVRT0/ 0.01, 0.001 /
      DATA VCHAR1 /'HGTS','TEMP','UWND','VWND','WWND','RELH'/
      DATA CNVRT1/  1.0,   1.0,  1.0,  1.0, 0.01,  1.0 /

!=>set requested sigma levels for input and output
      DATA SIGL/ 0.,1000.,925.,850.,700.,500.,                                 &
                400.,300.,250.,200.,150.,100./
!     input data corner and grid increment in deg
      DATA CLAT1/90.0/, CLON1/0.0/, DLAT/1.0/, DLON/1.0/
!     output grid size in kilometers
      DATA GRIDKM/100.0/

!=>check for command line arguments
      NARG=IARGC()
      IF(NARG.NE.3)THEN
         WRITE(*,*)'Usage: dat2arl [file] [clat] [clon]'
         STOP
      ELSE
         CALL GETARG(1,FNAME)
         CALL GETARG(2,LABEL)
         READ(LABEL,'(F10.0)')CLAT
         CALL GETARG(3,LABEL)
         READ(LABEL,'(F10.0)')CLON
      END IF

!=>create index record structure and initialize packing routines
      CALL MAKNDX(VCHAR0,VCHAR1,NLVL,MVAR0,MVAR1,SIGL,                         &
                  NXP,NYP,CLAT,CLON,GRIDKM)
!     returns configuration from file (last 3 arguments not required)
      CALL PAKSET(20,'DATA_CFG',1,NXT,NYT,NZT)
!     set up the grid to lat/lon equivalence array
      CALL MKGRID(NXP,NYP,TLAT,TLON)

!=>open output data file and initilize to missing
      LREC=NXY+50
      OPEN(20,FILE=FNAME,RECL=LREC,ACCESS='DIRECT',FORM='UNFORMATTED')
      WRITE(*,*)'Initialized output data set: ',FNAME

!=>loop through all data periods until finished
      FTEST=.TRUE.
      DO WHILE(FTEST)

!        check availablility and date of next obs
         CALL GETTIME(FTEST,IY,IM,ID,IH,IN,IF)
         WRITE(*,*)'Processing: ',IY,IM,ID,IH

!        loop through each level
         DO L=1,NLVL

            NVAR=MVAR0
            IF(L.GT.1)NVAR=MVAR1
!           number of winds counter (2=vector)
            NUMW=0

!           loop through variables at that level
            DO K=1,NVAR

!              get data for that level and variable
               VCHAR=VCHAR0(K)
               IF(L.GT.1)VCHAR=VCHAR1(K)
               CALL GETDATA(VCHAR,L,RVAR,NLAT,NLON)

!              process data according to variable type
               CNVRT=CNVRT0(K)
               IF(L.GT.1)CNVRT=CNVRT1(K)

               IF(VCHAR1(K).EQ.'UWND')THEN
!                 interpolate from lat/lon to conformal grid
                  CALL REGRID(RVAR,NLON,NLAT,XVAR,NXP,NYP,TLAT,TLON,           &
                       CLAT1,CLON1,DLAT,DLON)
                  IF(CNVRT.NE.1.0)CALL DATCNV(XVAR,NXP,NYP,CNVRT)
!                 wind component processing order does not matter
                  NUMW=NUMW+1
!                 when both wind vectors have been processed rotate
                  IF(NUMW.EQ.2)THEN
                     CALL U2GRID(XVAR,YVAR,NXP,NYP)
!                    then pack into ARL format and continue
                     CALL PAKREC(20,XVAR,CVAR,NXP,NYP,NXY,'UWND',              &
                          IY,IM,ID,IH,IN,IF,L,0)
                     CALL PAKREC(20,YVAR,CVAR,NXP,NYP,NXY,'VWND',              &
                          IY,IM,ID,IH,IN,IF,L,0)
                     NUMW=0
                  END IF

               ELSEIF(VCHAR1(K).EQ.'VWND')THEN
!                 interpolate from lat/lon to conformal grid
                  CALL REGRID(RVAR,NLON,NLAT,YVAR,NXP,NYP,TLAT,TLON,           &
                       CLAT1,CLON1,DLAT,DLON)
                  IF(CNVRT.NE.1.0)CALL DATCNV(YVAR,NXP,NYP,CNVRT)
!                 wind component processing order does not matter
                  NUMW=NUMW+1
                  IF(NUMW.EQ.2)THEN
                     CALL U2GRID(XVAR,YVAR,NXP,NYP)
!                    then pack into ARL format and continue
                     CALL PAKREC(20,XVAR,CVAR,NXP,NYP,NXY,'UWND',              &
                          IY,IM,ID,IH,IN,IF,L,0)
                     CALL PAKREC(20,YVAR,CVAR,NXP,NYP,NXY,'VWND',              &
                          IY,IM,ID,IH,IN,IF,L,0)
                     NUMW=0
                  END IF

               ELSE
!                 normal single scalar variable
!                 interpolate from lat/lon to conformal grid
                  CALL REGRID(RVAR,NLON,NLAT,XVAR,NXP,NYP,TLAT,TLON,           &
                       CLAT1,CLON1,DLAT,DLON)
                  IF(CNVRT.NE.1.0)CALL DATCNV(XVAR,NXP,NYP,CNVRT)
!                 then pack into ARL format and continue
                  CALL PAKREC(20,XVAR,CVAR,NXP,NYP,NXY,VCHAR,                  &
                       IY,IM,ID,IH,IN,IF,L,0)
               END IF

!           variable loop
            END DO

!        level loop
         END DO

!        close out time period and write index record
         CALL PAKNDX(20)

!     time period loop
      END DO

      CLOSE (20)
      END


!#####################################################################

      SUBROUTINE MAKNDX(VCHAR0,VCHAR1,NLVL,MVAR0,MVAR1,                        &
         SIGL,NXP,NYP,CLAT,CLON,GRIDKM)

!     creates configuration file that describes the output
!     data grid and will be used by packing programs to write
!     header information into data records

      REAL SIGL(NLVL)
!     arrays to hold variable selection information
      CHARACTER*4 VCHAR0(MVAR0), VCHAR1(MVAR1)

      COMMON / SETUP / GRIDS(12), PARMAP(9)

!     grid orientation
      GRIDS(6)=0.0
!     delta=x grid size in km
      GRIDS(5)=GRIDKM

!     synch point in x,y coordintes
      GRIDS(8)=(NXP+1.0)/2.0
      GRIDS(9)=(NYP+1.0)/2.0
!     synch point in lat/lon coordinates
      GRIDS(10)=CLAT
      GRIDS(11)=CLON

!     variable reserved for future use
      GRIDS(12)=0.0

!     defines a polar sterographic projection
      IF(ABS(CLAT).GT.45.0)THEN

!        set the pole position and reference lat/lon
         IF(CLAT.GT.0.0)THEN
            GRIDS(1)=90.0
         ELSE
            GRIDS(1)=-90.0
         END IF

!        pole longtitude (+180 from cut)
         GRIDS(2)=0.0
         GRIDS(3)=CLAT

!        reference longitude or grid alignment
         GRIDS(4)=CLON

!        tangent latitude
         GRIDS(7)=90.0

!     defines a mercator projection
      ELSE

!        pole lat/lon axis through pole
         GRIDS(1)=90.0
         GRIDS(2)=0.0

!        reference lat
         GRIDS(3)=CLAT
!        reference lon
         GRIDS(4)=CLON

!        tangent latitude
         GRIDS(7)=0.0
      END IF

!     write the packer configuration file
      OPEN(30,FILE='DATA_CFG')
      WRITE(30,'(20X,A4)')'ARL1'

!     grid number 99 and 1 for pressure coordinate system
      WRITE(30,'(20X,I4)') 99, 2

      WRITE(30,'(20X,F10.2)')(GRIDS(I),I=1,12)
      WRITE(30,'(20X,I4)')NXP,NYP,NLVL

!     variable information
      DO L=1,NLVL
         IF(L.EQ.1)THEN
            WRITE(30,'(20X,F6.1,I3,20(1X,A4))')                                &
                SIGL(L),MVAR0,(VCHAR0(N),N=1,MVAR0)
         ELSE
            WRITE(30,'(20X,F6.1,I3,20(1X,A4))')                                &
                SIGL(L),MVAR1,(VCHAR1(N),N=1,MVAR1)
         END IF
      END DO

      CLOSE (30)
      RETURN
      END

!#####################################################################

      SUBROUTINE DATCNV(RVAR,NXP,NYP,CNVRT)

!     converts data array from input data units
!     to output file units

      REAL RVAR(NXP,NYP)

      DO J=1,NYP
      DO I=1,NXP
         RVAR(I,J)=RVAR(I,J)*CNVRT
      END DO
      END DO

      RETURN
      END

!#####################################################################

      SUBROUTINE MKGRID(NX,NY,TLAT,TLON)

!     generates the lat/lon position of each node on the conformal
!     output data grid used to interpolate from input lat/lon grid

      REAL TLAT(NX,NY),TLON(NX,NY)
      COMMON / SETUP / GRIDS(12), PARMAP(9)

!     define the tangent latitude and reference longitude
      CALL STLMBR(PARMAP,GRIDS(1),GRIDS(2))

!     define the grid by a one-point specification
      CALL STCM1P(PARMAP,GRIDS(8),GRIDS(9),GRIDS(10),GRIDS(11),                &
                         GRIDS(3),GRIDS(4),GRIDS(5),GRIDS(6))

!     determine the lat/lon at the grid locations
      DO I=1,NX
      DO J=1,NY
         CALL CXY2LL(PARMAP,FLOAT(I),FLOAT(J),TLAT(I,J),CLON)

!        ==>use -180 to +180 system
!        if(clon.gt. 180.0)clon=clon-360.0
!        if(clon.lt.-180.0)clon=clon+360.0
!        ==>use 0 to 360 system
         if(clon.gt. 360.0)clon=clon-360.0
         if(clon.lt.   0.0)clon=clon+360.0

         TLON(I,J)=CLON
      END DO
      END DO

      RETURN
      END

!#####################################################################

      SUBROUTINE REGRID(V1,NX1,NY1,V2,NX2,NY2,TLAT,TLON,                       &
         CLAT1,CLON1,DLAT,DLON)

!     linear interpolation from lat/lon to conformal grid

!     old data and new data
      REAL V1(NX1,NY1), V2(NX2,NY2)

!     grid conversion array
      REAL TLAT(NX2,NY2), TLON(NX2,NY2)

!     interpolate values to new grid
      DO I=1,NX2
      DO J=1,NY2

!        compute adjacent index values on grid 1
       XP=1.0+(TLON(I,J)-CLON1)/DLON
       YP=1.0+(CLAT1-TLAT(I,J))/DLAT

!        check limits
       IF(XP.LT.1.0)XP=1.0
       IF(XP.GT.FLOAT(NX1))XP=NX1-1
       IF(YP.LT.1.0)YP=1.0
       IF(YP.GT.FLOAT(NY1))YP=NY1-1

!        compute indicies
         ILO=INT(XP)
         IHI=ILO+1
         JLO=INT(YP)
         JHI=JLO+1

!        interpolation fractions (off grid extrapolated)
         FXI=XP-ILO
         FYJ=YP-JLO

!        interpolate across at top and bottom
         TOP=(V1(IHI,JHI)-V1(ILO,JHI))*FXI+V1(ILO,JHI)
         BOT=(V1(IHI,JLO)-V1(ILO,JLO))*FXI+V1(ILO,JLO)

!        interpolate between top and bottom
         V2(I,J)=(TOP-BOT)*FYJ+BOT

      END DO
      END DO

      RETURN
      END


!#####################################################################


      SUBROUTINE U2GRID(UU,VV,NX,NY)

!     rotates winds already interpolated to conformal grid from
!     true orientation to grid orientation

      REAL   UU(NX,NY), VV(NX,NY)
      COMMON/ SETUP / GRIDS(12), PARMAP(9)

!     convert compass winds to grid-orientation
      DO I=1,NX
      DO J=1,NY
         CALL CC2GXY(PARMAP,FLOAT(I),FLOAT(J),UU(I,J),VV(I,J),UG,VG)
         UU(I,J)=UG
         VV(I,J)=VG
      END DO
      END DO

      RETURN
      END


!#####################################################################

      SUBROUTINE GETTIME(FTEST,IY,IM,ID,IH,IN,IF)

!     this routine should be replaced by real data access

      PARAMETER (NT=9)
      LOGICAL FTEST
      INTEGER IYR(NT),IMO(NT),IDA(NT),IHR(NT),IMN(NT),IFH(NT)
      DATA NTIME/0/

      DATA IYR/ 98, 98, 98, 98, 98, 98, 98, 98, 98 /
      DATA IMO/  5,  5,  5,  5,  5,  5,  5,  5,  5 /
      DATA IDA/ 15, 15, 15, 15, 16, 16, 16, 16, 17 /
      DATA IHR/  0,  6, 12, 18,  0,  6, 12, 18,  0 /
      DATA IMN/  0,  0,  0,  0,  0,  0,  0,  0,  0 /
      DATA IFH/  0,  6, 12, 18, 24, 30, 36, 42, 48 /

      SAVE NTIME

      NTIME=NTIME+1
      FTEST=.TRUE.

      IY=IYR(NTIME)
      IM=IMO(NTIME)
      ID=IDA(NTIME)
      IH=IHR(NTIME)
      IN=IMN(NTIME)
      IF=IFH(NTIME)

      IF(NTIME.EQ.NT)FTEST=.FALSE.
      RETURN
      END

!#####################################################################

      SUBROUTINE GETDATA(VCHAR,LVL,RVAR,NLAT,NLON)

!     this routine should be replaced by real data access

      REAL RVAR(NLON,NLAT), HGT(12), TMP(12)
      CHARACTER*4 VCHAR

!     standard atmosphere values for the following pressure surfaces:
!     1013 1000 925 850 700 500 400 300 250 200 150 100

      DATA HGT/  0.,100.,750.,1450.,3000.,5550.,7200.,9150.,10350.,            &
        11800.,13600.,16200./

      DATA TMP/288.,287.,283., 279., 269., 252., 241., 229.,221.,              &
        217.,217.,217./

      IF(VCHAR.EQ.'PRSS')VALUE=101300.0
      IF(VCHAR.EQ.'TPP6')VALUE=0.0
      IF(VCHAR.EQ.'HGTS')VALUE=HGT(LVL)
      IF(VCHAR.EQ.'TEMP')VALUE=TMP(LVL)
      IF(VCHAR.EQ.'UWND')VALUE=1.0*LVL
      IF(VCHAR.EQ.'VWND')VALUE=1.0*LVL
      IF(VCHAR.EQ.'WWND')VALUE=0.0
      IF(VCHAR.EQ.'RELH')VALUE=50.0

      DO J=1,NLAT
      DO I=1,NLON
         RVAR(I,J)=VALUE
      END DO
      END DO

      RETURN
      END
