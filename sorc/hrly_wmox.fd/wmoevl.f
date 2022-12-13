       SUBROUTINE WMOEVL(MODEL,MDLNAM,LVL,WGT,IRGNB,IRGNE,MXCYL,
     1                   MXFCT,SX)
C SUBPROGRAM:    WMOEVL
C   PRGMMR: LILLY            ORG: NP12        DATE: 2007-07-22
C
C ABSTRACT: MEAN ERROR, RMS VEC ERR, AVG AND ABS MAX FCST AND ANL WIND
C FOR NORTH AND SOUTH HEMISPHERE AND TROPICS PRESSURE LEVELS 850, 500,
C AND 250MB.
C
C PROGRAM HISTORY LOG:
C 2007-07-22 STEVEN G. LILLY -- UPDATING SOURCE CODES
C 2013-12-05 STEVEN G. LILLY -- Fixed variables imod and jmod in
C                               Subroutine getgrib. These variables
C                               were being reset to zero
C                               
C
C USAGE:    CALL WMOEVL(MODEL,MDLNAM,LVL,WGT,IRGNB,IRGNE,MXCYL,
C                       MXFCT,SX)
C
C   INPUT ARGUMENT LIST:
C
C   OUTPUT ARGUMENT LIST:
C
C   SUBPROGRAM CALLED:
C     LIBRARY:    (NONE)
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN-90
C   MACHINE:  IBM RS6000
C
C$$$
C
C
       CHARACTER(13) NVGRD(2)
       CHARACTER(10)  ITRUTH
       CHARACTER(8)  PARM
       CHARACTER(8)  PLVL
       CHARACTER(3)  MDLNAM
       REAL     SX(15,MXCYL,MXFCT,2), X(5,2), WGT(37,3),
     1          ANLNHM(145,37),ANLSHM(145,37),
     2          HEMFCT(145,37),HEMAIT(145,37),
     3          TRPANL(145,37),TRPFCT(145,37),TRPAIT(145,37)
       INTEGER  LABEL(6),LABELF(6),LABELA(6)
       LOGICAL(1) IVFCT,IVLVL
       DATA  NVGRD/'2.5DEG LATLON','5.0DEG LATLON'/
       DATA  NTMAX/ 1/
       DATA  IGRDNH,IGRDSH /29,30/
C
       COMMON /XDATA/IDATE(1),IDS(4,4),IVFCT(10,2),IVT(4,1),
     1         IVLVL(4),PLVL(4)
       COMMON /YDATA/ NBEGIN,NEND,ICYB,ICYE,ICYI,IVFLD,IFTINC,
     1         NICR,JLVLS,JFCTS,MDANL,ITRUTH
C
C
       CALL IFILL(LABEL,6,1,1,0)
       CALL IFILL(LABELF,6,1,1,0)
       CALL IFILL(LABELA,6,1,1,0)
       CALL AFILL(TRPANL,145,37,1,0.0)
       CALL AFILL(anlnhm,145,37,1,0.0)
       CALL AFILL(anlshm,145,37,1,0.0)
       CALL AFILL(hemfct,145,37,1,0.0)
       CALL AFILL(hemait,145,37,1,0.0)
       CALL AFILL(TRPFCT,145,37,1,0.0)
       CALL AFILL(TRPAIT,145,37,1,0.0)
C
C..................................................................
C...   FOR EACH REQUESTED CYCLE PERIOD..........
C...     1. SPECIFY LABEL VARIABLES AND CALL SUBROUTINE getgrib to
C...        RETRIEVE VERIFYING FIELD.
C..................................................................
C
C
 100   FORMAT(4X,'MDANL,IVFLD,IANL,IVER= ',4I2,2X,'LABEL= ',4i9,
     +        i11,i9)
 200    FORMAT(' ERROR NUMBER ',I3,' OCCURRED FOR MDANL= ',I2,
     1         ' ...IANL= ',I2,' ...IVER= ',I2,' *************',/,
     2         5X,'LABEL= ', 4i9, i11,' ... DO NEXT CYCLE ...')
c 300   FORMAT('1')
C
C..... . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C
c       PRINT 300
       NTIMES = 0
       LMSL = 0
       IF(IDS(1,LVL)  .EQ.  2) LMSL = 1
C
       do 15 iparm=1,2
        if ((lvl .eq. 1) .and. (iparm .eq. 2)) then
          goto 15
        endif
       DO 10 ICYL = ICYB,ICYE,ICYI
       IVFDTE = IDATE(ICYL)
       IF(IVFDTE  .EQ.  0) GO TO 10
       NTIMES = NTIMES + 1
C
C
       IF(IVFLD  .EQ.  1) THEN
        IANL = 1
        IVER = 0
       ELSE
        IANL = 0
        IVER = 1
       ENDIF
C
       DO 12 L = 1,4
       LABEL(L) = IDS(L,LVL)
  12   CONTINUE
       LABEL(5) = IVFDTE
       LABEL(6) = IGRDNH
       LBLONE = IDS(1,LVL)
C
       PRINT 100, MDANL, IVFLD,IANL,IVER,(LABEL(L),L=1,6)
C
c  get current analysis for northern hemisphere
c
       ifhr=0
       call getgrib(MDANL,LABEL,IANL,IVER,iparm,ifhr,ANLNHM,IERR)
C
       IF(IERR  .NE. 0) THEN
        PRINT 200,IERR,MDANL,IANL,IVER,(LABEL(L),L=1,6)
        GO TO 10
       ENDIF
C.....
       DO 14  L = 1,4
       LABEL(L) = IDS(L,LVL)
  14   CONTINUE
       LABEL(5) = IVFDTE
       LABEL(6) = IGRDSH
C
c  get current analysis for southern hemisphere
c
C
       CALL getgrib(MDANL,LABEL,IANL,IVER,iparm,ifhr,ANLSHM,IERR)
C
       IF(IERR  .NE. 0) THEN
        PRINT 200,IERR,MDANL,IANL,IVER,(LABEL(L),L=1,6)
        GO TO 10
       ENDIF
C.....
       DO 16  L = 1,4
       LABEL(L) = IDS(L,LVL)
  16   CONTINUE
       LABEL(5) = IVFDTE
       LABEL(6) = IGRDNH
C
C...   SAVE ANL FOR TROPICS (20S-20N) .. STORE IN 1-17 ....
C
       JSH =28
       JNH =1
       DO 50 J = 1,9
        JSH = JSH + 1
       DO 52 I = 1,145
        TRPANL(I,J) = ANLSHM(I,JSH)
  52   CONTINUE
  50   CONTINUE
       DO 54 J = 10,17
        JNH = JNH + 1
       DO 56 I = 1,145
        TRPANL(I,J) = ANLNHM(I,JNH)
  56   CONTINUE
  54   CONTINUE
C
C
C..................................................................
C...     2. SPECIFY LABEL VARIABLES AND CALL SUBROUTINE getgrib TO
C...        RETRIEVE VERIFYING FORECAST HOUR.  
C..................................................................
C
C
 210    FORMAT('0 ERROR NUMBER ',I3,' OCCURRED FOR MODEL= ',I2,
     1         ' ...IANL= ',I2,' ...IVER= ',I2,' *************',/,
     2         5X,'LABEL= ', 5i9,' ... DO NEXT FCST HR ...')
C
C..... . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C
       IVER = 1
       IANL = 0
       IF(IFTINC  .NE.  0) GO TO 18
        IF(MODEL  .NE.  MDANL) THEN
         IF(IVFLD  .EQ.  1) THEN
          IANL = 1
          IVER = 0
         ENDIF
        ENDIF
  18   CONTINUE
C.....
       DO 20 IFHR = 1,JFCTS
c        IF(.NOT. IVFCT(IFHR,MODEL)) GO TO 20
C.....
       CALL AFILL(hemfct,145,37,1,0.0)
       CALL AFILL(hemait,145,37,1,0.0)
       CALL AFILL(TRPFCT,145,37,1,0.0)
       CALL AFILL(TRPAIT,145,37,1,0.0)
c
       DO 30 IRGN = IRGNB,IRGNE
        IF(IRGN  .EQ.  1) THEN
         IGRDF = IGRDNH
        ELSE
         IF(IRGN  .EQ.  2) THEN
          IGRDF = IGRDSH
         ELSE
          GO TO 32
         ENDIF
        ENDIF
C.....
        JFT = IFTINC * IFHR
C
       DO 22 L = 1,4
        LABELF(L) = IDS(L,LVL)
        LABELA(L) = IDS(L,LVL)
  22   CONTINUE
        LABELF(5) = IVFDTE
        LABELF(6) = IGRDF
        LABELA(6) = IGRDF
C.....
c        IF(NTIMES  .LE.  NTMAX) THEN
c         PRINT  110,LABEL(1),LABELF(1),LABELF(5),LABELF(6)
c        ENDIF
C.....
c  get forecast
c
c
       CALL getgrib(MODEL,LABELF,IANL,IVER,iparm,ifhr,HEMFCT,IERR)
C
       IF(IERR  .NE. 0) THEN
        PRINT 210,IERR,MODEL,IANL,IVER,(LABELF(L),L=1,5)
        GO TO 20
       ENDIF
C
C.....
       LABELA(4) = LABELF(4)
c
c  get analysis valid ifhr hours before current analysis
c
       CALL getgrib(MDANL,LABELA,1,0,iparm,ifhr,HEMAIT,IERR)
C
C.....
       IF(IERR  .NE. 0) THEN
        IAX = 1
        IVX = 0
        PRINT 210,IERR,MDANL,IAX,IVX,(LABELA(L),L=1,6)
        GO TO 20
       ENDIF
C.....
c       IF(NTIMES  .LE.  NTMAX) THEN
c        PRINT  120,MODEL,JFT,IANL,IVER,(LABELF(L),L=1,6),
c     1         (LABELA(L),L=1,6)
c       ENDIF
C
       DO 24 L = 1,4
        LABELF(L) = IDS(L,LVL)
        LABELA(L) = IDS(L,LVL)
  24   CONTINUE
       LABELF(5) = IVFDTE
       LABELF(6) = IGRDF
       LABELA(6) = IGRDF
C
C...   SAVE TROPICS SECTION FROM NHM AND SHM FIELDS ......
C
C
       IF(IRGN  .EQ.  1) THEN
        J1 = 10
        J2 = 17
        J3 = 1
        DO 34 J = J1,J2
         J3 = J3 + 1
        DO 36 I = 1,145
         TRPFCT(I,J) = HEMFCT(I,J3)
         TRPAIT(I,J) = HEMAIT(I,J3)
  36    CONTINUE
  34    CONTINUE
C
       ELSE
        IF(IRGN  .EQ.  2) THEN
         J1 = 1
         J2 = 9
         J3 = 28
         DO 38 J = J1,J2
          J3 = J3 + 1
         DO 40 I = 1,145
          TRPFCT(I,J) = HEMFCT(I,J3)
          TRPAIT(I,J) = HEMAIT(I,J3)
   40    CONTINUE
   38    CONTINUE
        ENDIF
       ENDIF
C
C..................................................................
C...     3. CALCULATE WMO VERIFICATION SCORES
C..................................................................
C.....
       IF(IRGN .EQ. 1) THEN
c
c
         CALL WMOVZT( X,WGT(1,IRGN),HEMFCT,HEMAIT,ANLNHM,
     1               IRGN,IFHR,LMSL,NICR)
       ELSE
c
c
         CALL WMOVZT( X,WGT(1,IRGN),HEMFCT,HEMAIT,ANLSHM,
     1               IRGN,IFHR,LMSL,NICR)
       ENDIF
       GO TO 42
C
 32    CONTINUE
c
c
        CALL WMOVZT( X,WGT(1,IRGN),TRPFCT,TRPAIT,TRPANL,
     1               IRGN,IFHR,LMSL,NICR)
 42    CONTINUE
C.....
       DO 26  J = 1,2
       DO 26  I = 1,5
        II = 5*(IRGN - 1) + I
        SX(II,ICYL,IFHR,J) = X(I,J)
  26   CONTINUE
C
C..... END OF REGION LOOP  ......
  30   CONTINUE
C
C..... END OF FORECAST LOOP .....
  20   CONTINUE
C
C
C..... END OF CYCLE LOOP .....
  10   CONTINUE
C
C
C..................................................................
C...     4. PRINT DAILY VERIFICATION SCORES
C..................................................................
C
C
 310   FORMAT(//,5X,'..... FOR MODEL = ',A3,' ... AT LEVEL = ',
     1        A8,1x,a3,' ... VERIFICATION GRID= ',A13,' .....',//)
 320   FORMAT(5X,'FCST HOUR = ',I5,4X,'FROM  ',I10,' TO ',I10,' ...',
     1        '..................',/,T18,'NORTH HEMISPHERE ...',
     2        55X,'SOUTH HEMISPHERE ...',92X,'TROPICS ...',/,
     3        5X,'DAY  CYL     MERR   RMSE   CORC   S1MX   S1TR',
     4        55X,'MERR   RMSE   CORC   S1MX   S1TR',
     5        92X,'MERR   RMSE   CORC   S1MX   S1TR')
 330   FORMAT(5X,I3,I5,2X, 5F7.2,2X,5F7.2,2X,5F7.2)
 340   FORMAT(/)
 350   format(5f7.2,2x,5f7.2,2x,5f7.2)
 360   format(i10)
C
C..... . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C
c       NPAG = 0
       if (iparm .eq. 1) then 
          parm="HGT"
       else
          parm="TMP"
       endif
       DO 43 NVFCN = 1,NICR
       PRINT 310,MDLNAM,PLVL(LVL),parm,NVGRD(NVFCN)
       if ((plvl(lvl) .eq. "MSL") .and. (iparm .eq. 1)) then
       write(71,360)nbegin
       elseif ((plvl(lvl) .eq. "850MB") .and. (iparm .eq. 1)) then
       write(72,360)nbegin
       elseif ((plvl(lvl) .eq. "850MB") .and. (iparm .eq. 2)) then
       write(73,360)nbegin
       elseif ((plvl(lvl) .eq. "500MB") .and. (iparm .eq. 1)) then
       write(74,360)nbegin
       elseif ((plvl(lvl) .eq. "500MB") .and. (iparm .eq. 2)) then
       write(75,360)nbegin
       elseif ((plvl(lvl) .eq. "250MB") .and. (iparm .eq. 1)) then
       write(76,360)nbegin
       elseif ((plvl(lvl) .eq. "250MB") .and. (iparm .eq. 2)) then
       write(77,360)nbegin
       endif
       DO 44 IFHR = 1,JFCTS
        IF(.NOT. IVFCT(IFHR,MODEL)) GO TO 44
c       NPAG = NPAG + 1
       IFTHR = IFTINC*IFHR
C
       PRINT 320,IFTHR,NBEGIN,NEND
       DO 46 ICYL = ICYB,ICYE,ICYI
       IDAY = IVT(3,ICYL)
       PRINT 330,IDAY,ICYL, (SX(I,ICYL,IFHR,NVFCN),I=1,15)
       if ((plvl(lvl) .eq. "MSL") .and. (iparm .eq. 1)) then
       WRITE(71,350)(SX(I,ICYL,IFHR,NVFCN),I=1,15)
       elseif ((plvl(lvl) .eq. "850MB") .and. (iparm .eq. 1)) then
       WRITE(72,350)(SX(I,ICYL,IFHR,NVFCN),I=1,15)
       elseif ((plvl(lvl) .eq. "850MB") .and. (iparm .eq. 2)) then
       WRITE(73,350)(SX(I,ICYL,IFHR,NVFCN),I=1,15)
       elseif ((plvl(lvl) .eq. "500MB") .and. (iparm .eq. 1)) then
       WRITE(74,350)(SX(I,ICYL,IFHR,NVFCN),I=1,15)
       elseif ((plvl(lvl) .eq. "500MB") .and. (iparm .eq. 2)) then
       WRITE(75,350)(SX(I,ICYL,IFHR,NVFCN),I=1,15)
       elseif ((plvl(lvl) .eq. "250MB") .and. (iparm .eq. 1)) then
       WRITE(76,350)(SX(I,ICYL,IFHR,NVFCN),I=1,15)
       elseif ((plvl(lvl) .eq. "250MB") .and. (iparm .eq. 2)) then
       WRITE(77,350)(SX(I,ICYL,IFHR,NVFCN),I=1,15)
       endif
  46   CONTINUE
C
        IF(IFHR  .NE.  JFCTS) PRINT 340
c       ENDIF
C
  44   CONTINUE
  43   CONTINUE
C
c..... end of parameter loop
  15   continue
C
        RETURN
        END
c
      subroutine getgrib(run,id,ianl,iver,iparm,ifhr,field,iret)
c  subprogram getgrib
c  programmer:  Andrew Krein
c
c  Abstract:  Retrieves the requested grib data for a particular
c  valid time.
c
c  Program History Log:
c    96-06-20  Andrew Krein
c    96-09-26  modified for use with wmoxmain.f for gfs/avn only
c    99-09-17  compile on IBM RS6000, add call to baopenr
c    13-12-05  Fixed variables imod and jmod for the 240 forecast hour.
c              These variables were set to zero.
c
c  Usage:  call getgrib(run,id,ianl,iver,iparm,ifhr,field,iret)
c    Input Argument List:
c      run:  integer model name (1=GFS, 2=AVN, 3=NGM, 4=ETA)
c      id:   pds numbers from label
c      ianl: non-zero for analysis
c      iver: non-zero for verifying at desired date/time
c      iparm: field type (1=msl or hgt, 2=temp)
c      ifhr: forecast hour
c
c    Output Argument List:
c      field:  grid data array
c      iret:   error message from getgb
c
c  Other variables used
c    mxcyl - maximum number of cycles in a monthly verification
c    mxlvl - maximum number of levels to in verification
c    mxfct - maximum number of forecast times to verify
c    ip    - interpolation method (=0 for bilinear)
c    ipopt - interpolation options (=0 for none)
c
c  Subprograms called:
c    getgb,ipolates
c
c  Variables passed to getgb:
c    idat - logical unit of the unblocked grib data file
c    iinv - logical unit of the unblocked grib index file
c    ipts - integer number of data points to unpack
c    0    - number of messages to skip, =0 to search from beginning
c    jpds - integer (25) pds parameters to search for (-1 for wildcard)
c    jgds - integer (22) gds parameters to search for (-1 for wildcard)
c           only searched for if jpds(3)=255
c
c  Output from getgb
c    ki    - integer number of points unpacked
c    kr    - integer message number unpacked
c    kpdsi - integer (25) unpacked pds parameters
c    kgdsi - integer (22) unpacked gds parameters
c    li    - logical (ki) unpacked bitmap if present
c    hi    - real (ki) unpacked data
c
c  Attributes:
c    Language:  FORTRAN-90
c    Machine:   IBM RS6000
c
c
       parameter (mxmdl=2,mxfct=10,mxlvl=1,
     1            mxcyl=1,mxin=360*181,mxout=145*37)
       character(10) itruth
       character(8) cdat(11), cindex(11), c2dat(10), c2indx(10)
       character(8) cdt, cdx 
       character(6) plvl
       logical(1) ivfct,ivlvl
       logical(1) li(mxin),lo(mxout)
       real hi(mxin),ho(mxout)
       real rlat(mxout),rlon(mxout)
       integer id(6),jpds(25),jgds(22)
       integer kpdsi(25),kgdsi(22),ibi(mxlvl),ibo(mxlvl)
       integer kpdso(25),kgdso(22)
       integer dat(10),inv(10)
       integer dat2(10),inv2(10)
       integer run, ipopt(20)
       real field(145,37)
       character gdso(42)
c
c
       DATA cdat /'GRIBAD0','GRIBFD1','GRIBFD2','GRIBFD3','GRIBFD4',
     &            'GRIBFD5','GRIBFD6','GRIBFD7','GRIBFD8','GRIBFD9',
     &            'GRIBFDL'/
       DATA cindex /'GRIBAI0','GRIBFI1','GRIBFI2','GRIBFI3','GRIBFI4',
     &              'GRIBFI5','GRIBFI6','GRIBFI7','GRIBFI8','GRIBFI9',
     &              'GRIBFIL'/
       DATA c2dat /'GRIBAD1','GRIBAD2','GRIBAD3','GRIBAD4',
     &             'GRIBAD5','GRIBAD6','GRIBAD7','GRIBAD8','GRIBAD9',
     &             'GRIBADL'/
       DATA c2indx /'GRIBAI1','GRIBAI2','GRIBAI3','GRIBAI4',
     &              'GRIBAI5','GRIBAI6','GRIBAI7','GRIBAI8','GRIBAI9',
     &              'GRIBAIL'/
c
c
c  define output locations kdgso(1)<0 ==> rlat and rlon are input args
c  into ipolates
c
c
       k=1
       jpds=-1
       if (iparm .eq. 1) then
         jpds(5)=id(1)
       else
         jpds(5)=id(2)
       endif
       jpds(6)=id(3)
       jpds(7)=id(4)
       ip=0
       ipopt=0
c
c
c  define data and inventory locations for forecasts valid at ivfdte
c
        if ((ifhr.ne.0) .and. (ianl.eq.0) .and. (iver.eq.1)) then
          ida=31
          iin=51
          do i=1,10
             dat(i)=ida
             inv(i)=iin
             ida=ida+1
             iin=iin+1
          enddo
c
c  define data and inventory locations for analyses valid at ivfdte-ifhr
c
        elseif((ifhr.ne.0).and.(ianl.eq.1).and.(iver.eq.0)) then
          ida=41
          iin=61
          do i=1,10
             dat2(i)=ida
             inv2(i)=iin
             ida=ida+1
             iin=iin+1
          enddo
        endif
c
c  set idat and iinv for the current analysis field
c
       if ((ianl .eq. 1) .and. (iver .eq. 0) .and. (ifhr .eq. 0)) then
         idat=20
         iinv=25
         cdt = cdat(1)
         cdx = cindex(1)
         call baopenr(idat,cdat(1),ier1)
         call baopenr(iinv,cindex(1),ier2)
         if (ier1.ne.0.or.ier2.ne.0)    then
           print *, ' *** CANNOT OPEN VANL FILES, QUITTING *** ',
     &              ' err dat = ', ier1, '  err index = ', ier2
           stop 99
         endif
       endif
c
c  set idat and iinv for a forecast or analysis field valid current or ifhr ago
c
       if     ((ifhr .ne. 0).and.(ianl .eq. 0).and.(iver .eq. 1)) then
c
c  get forecast from time ifhr prior to current verification time
c
         idat=dat(ifhr)
         iinv=inv(ifhr)
c
C for idat and cdat, when ifhr = 10, imod is set to 11
C reason: imods is reset to zero with each passages.
c
         if (ifhr .lt. 10) then
            imod = mod(iinv,10) + 1
            imods = imod
         elseif (ifhr .eq. 10) then
            imod = 11
         endif

         call baopenr(idat,cdat(imod),ier1)
         call baopenr(iinv,cindex(imod),ier2)
         if (ier1.ne.0.or.ier2.ne.0)    then
           print *, ' *** CANNOT OPEN FCST FILES  ', idat, ' ', iinv,
     &              ' ', cdat(j), ' ', cindex(j),
     &              ' ***  err dat = ', ier1, '  err index = ', ier2
           if (ier1.ne.0)    then
             iret = ier1
           else
             iret = ier2
           endif
           go to 20
         endif
c
       elseif ((ifhr .ne. 0).and.(ianl .eq. 1).and.(iver .eq. 0)) then
c
c  get analysis from ifhr prior to current verification time
c
         idat=dat2(ifhr)
         iinv=inv2(ifhr)
c
C for idat and c2dat, when ifhr = 10, jmod is set to 10
C reason: jmods is reset to zero with each passages.
c
         if (ifhr .lt. 10) then
            jmod = mod(iinv,10)
            jmods = jmod
         elseif (ifhr .eq. 10) then
            jmod = 10
         endif
c
         call baopenr(idat,c2dat(jmod),ier1)
         call baopenr(iinv,c2indx(jmod),ier2)
         if (ier1.ne.0.or.ier2.ne.0)    then
           print *, ' *** CANNOT OPEN ANL FILES  ', idat, ' ', iinv,
     &              ' ', c2dat(j), ' ', c2indx(j),
     &              ' ***  err dat = ', ier1, '  err index = ', ier2
           if (ier1.ne.0)    then
             iret = ier1
           else
             iret = ier2
           endif
           go to 20
         endif
c
       endif
c
c
         call getgb(idat,iinv,mxin,0,jpds,jgds,
     1              ki,kr,kpdsi,kgdsi,li(k),hi(k),iret)
c
         if (iret .ne. 0) then
           print*
           print*,'iret from getgb= ',iret
           print*,'idat, iinv= ',idat, iinv
           print*,'cdat, cindex= ',cdt, cdx
           print*
           goto 20
         endif
c
         ibi(k)=mod(kpdsi(4)/64,2)
c
c  call makgds for the NH or SH global model on a 145x37 grid (29 or 30)
c
         ig=id(6)
         call makgds(ig,kgdso,gdso,lengds,iret)
c
         if (iret .ne. 0) then
           print*
           print*,'iret from makgds= ',iret
           print*,'idat, iinv= ',idat, iinv
           print*
           goto 20
         endif
c
c  correction for error in kgds parameters returned from makgds for grid 30
c
         if (ig .eq. 30) then
           kgdso(4)=-90000
           kgdso(5)=0
         endif
c
c  interpolate from 1 degree lola to 2.5 degree lola grids
c
       call ipolates(ip,ipopt,kgdsi,kgdso,mxin,mxout,k,ibi,
     1                    li,hi,ko,rlat,rlon,ibo,lo,ho,iret)
c
c
         if (iret .ne. 0) then
           print*
           print*,'iret from ipolates= ',iret
           print*,'idat, iinv= ',idat, iinv
           print*
           goto 20
         endif
c
       iz=0
       do ia=1,37
         do io=1,145
           iz=iz+1
c
           if ((jpds(5) .eq. 2) .and. (jpds(6) .eq. 102)) then
c
c  for mslp
c
             field(io,ia)=ho(iz)/100.0
           elseif (jpds(5) .eq. 11) then
c
c  for temps at any level
c
             field(io,ia)=ho(iz)-273.15
           elseif ((jpds(5) .eq. 7) .and. (jpds(6) .eq. 100)) then
c
c  for hgts at any isobaric level
c
             field(io,ia)=ho(iz)
           endif
c
         enddo
       enddo
c
c
  20   continue
       return
       end
