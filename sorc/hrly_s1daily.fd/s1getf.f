       SUBROUTINE S1GETF(MODEL,ID,LBL1,IANL,IVER,ifhr,FMesh,fmesh2,
     1                   HEMISP,hemisp2,IERR)
C SUBPROGRAM:    S1GETF
C   PRGMMR: ROBERT HIRANO    ORG: W/NMC42    DATE: 88-12-14
C
C ABSTRACT: RETRIEVES REQEUESTED FIELD FROM GRIB ARCHIVE.
C
C PROGRAM HISTORY LOG:
C   88-12-14  ROBERT HIRANO
C   91-03-13  CHANGED SUBROUTINES GETX TO GETARC.
C
C USAGE:    CALL S1GETF(MODEL,ID,LBL1,IANL,IVER,ifhr,FMesh,fmesh2,HEMISP,hemisp2,IERR)
C   INPUT ARGUMENT LIST:
C     MODEL    - ID (1=GFS,2=AVN,3=NGM,4=ETA)  =MDANL FOR ANL
C     ID       - LABEL OF FIELD
C     LBL1     - FIRST WORD OF LABEL
C     IANL     - NON-ZERO FOR ANALYSIS
C     IVER     - NON-ZERO FOR FIELDS VERIFYING AT DESIRED DATE
C
C   OUTPUT ARGUMENT LIST:
C     FMesh    - grib grid 6 (53x45, 2385-point, oriented 105W,
c                pole at (27,49)
c     FMesh2   - 38x17 646-point 2.5deg lon-lat grid interpolated in getgrib
C     HEMISP   - grib grid 29 (145x37, 5365-point, for 0N to 90N,
c                (1,1) at (0E,90N), prime meridian not duplicated
c     hemisp2  - grib grid 27 (65x65, 4225-point, oriented 80W, pole at (33,33)
C     IERR     - ERROR MESSAGE FROM getgrib SUBROUTINE
C
C   SUBPROGRAM CALLED:
C     UNIQUE:   - AFILL, getgrib
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN-77
C   MACHINE:  NAS
C
C$$$
C
       PARAMETER   (MXMDL=4)
       CHARACTER*4 RUNMDL(MXMDL),RUN
       INTEGER  ID(12)
       REAL     FMesh(53,45),HEMISP(145,37)
       real     fmesh2(38,17),hemisp2(65,65)
       DATA RUNMDL/'GFSS','GAFS','RAFS','ETAX'/
C
C...................................................................
C      RETRIEVE REQUESTED MODEL FIELD
C        THERE ARE NO GFS AND AVN MSLP FIELDS, NEED TO INTERPOLATE
C        USING 1000MB AND 500MB HEIGHT FIELDS.
c  no longer true  gfs and avn mslp fields are available in GRIB
c  calls to HT2MSL removed
C...................................................................
C
       CALL AFILL(FMesh,53,45,1,.0)
       call afill(fmesh2,38,17,1,.0)
       CALL AFILL(HEMISP,145,37,1,.0)
       call afill(hemisp2,65,65,1,.0)
C.....
       RUN = RUNMDL(MODEL)
c
       if(model.ge.3) then
         ilon=53
         ilat=45
         ilon2=38
         ilat2=17
         ipts=ilon*ilat
       else
         ilon=145
         ilat=37
         ilon2=65
         ilat2=65
         ipts=65160
       endif
c
c  retrieve the global analysis and forecasts
c
       LOOP90: DO MX=1,1
       if (model .le. 2) then
          CALL getgrib(model,ID,IANL,IVER,ipts,ilon,ilat,ilon2,ilat2,
     1                 ifhr,HEMISP,hemisp2,IERR)
          EXIT LOOP90
C.....
C.....ETA VFCN USE ngmANL...............  c
          else
c          IF(IANL  .NE.  0) THEN
            CALL getgrib(model,ID,IANL,IVER,ipts,ilon,ilat,ilon2,ilat2,
     1                   ifhr,FMesh,fmesh2,IERR)
       LOOP10: DO J = 1,45
       LOOP12: DO I = 1,53
       END DO LOOP12
       END DO LOOP10
       ENDIF
C
       END DO LOOP90
C
       RETURN
       END
c

