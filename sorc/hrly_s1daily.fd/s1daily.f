C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C MAIN PROGRAM:  S1DAILY     DAILY S1 SCORE VERIFICATION
C   PRGMMR: C. L. VLCEK      ORG: NP12     DATE: 99-08-25
C
C ABSTRACT: S1 SCORE VERIFICATION OF NUMERICAL FORECAST MODELS
C   USING DATA FROM GRIB FILES.  TABLES OF DAILY SCORES FOR
C   SEVERAL VERIFICATION NETWORKS ARE PRINTED AS WELL AS A 
C   SUMMARY BY FORECAST HOUR; A STATISTICAL ANALYSIS OF SCORES
C   IS ALSO TABULATED.  GRADIENT VALUES, I.E., THE ERROR,
C   MAXIMUM, VERIFYING, AND THE FORECAST TO VERIFYING RATIO, ARE
C   ALSO SUMMARIZED AND ANALYZED.  UP TO SIX PRESSURE LEVELS AND
C   TEN FORECAST PERIODS CAN BE DONE.
C
C PROGRAM HISTORY LOG:
C   88-12-23  ROBERT Y. HIRANO
C   89-01-12  ROBERT Y. HIRANO (INCLUDED ADDITIONAL OPTIONS)
C   91-03-13  36-DAY ARCHIVE FILE NAMES AND GETX SUBRS CHANGED.
C   92-06-23  RGNAR1 ROWS CHGD FM 11-33 TO 10-32. SPEC VFCN RGNAR1
C             INSTD OF LLEURP. NGM CHG TO MAP 105 DATA ONLY SOON.
C   93-01-05  RGNAR1 ROWS CHGD BACK TO ORIGINAL GRID.
C   93-03-09  SPEC VFCN RGNAR1 CORRECTED. IGNORE PREVIOUS VFCN.
C   93-06-18  INCLUDE ETA MON AND 4DAY VFCN OPTIONS.
c   96-06-14  modified to account for read from grib files rather
c             than from obsolete 36-day archive.
c             modified to read daily rather than 4day verification.
c             modified to read in grib pds values rather than ON84
c             labels.
c   96-08-01  conversion completed
c   98-08-27  Made Y2K compliant
c   99-08-24  compiled on IBM RS6000
C
C USAGE:
C   INPUT FILES:
C     FT10F001 - BEGIN AND END DATE OF VERIFICATION PERIOD
C     FT11F001 - USR.DATE(T00Z)
C     FT12F001 - pds values OF VERIFICATION LEVELS and fields
C     FT13F001 - VERIFICATION DATA (MODEL,TRUTH,HOURS,ETC)
C     FT14F001 - DIMENSIONS OF IRREGULAR VERIFICATION NETWORKS
C     FT18F001 - VFCN OPTION (1=MON,4=daily SV,OTR=4DAY NOSV)
C     FT20F001 - COPY VERIFICATION DATA TO TAPE IF DESIRED
C     FT60F001 - ETA VERIFICATION ARCHIVE DISK FILE
C
C   OUTPUT FILES:
C     FT06F001 - VERIFICATION TABLES
C
C   SUBPROGRAMS CALLED:
C     UNIQUE:    - AFILL,IFILL,STATS,getgrib,dateconv
C                - S1COMP,S1EVAL,S1GETF,S1GLBL,S1GRAD,
C                - S1IREG,S1RECT,S1RGNL,S1SCRS,STATS
C     LIBRARY:
C       SPECIAL  - IAND,IOR,POLATE
C
C REMARKS: CAN GENERATE A LOT OF PRINT (FOR 12-72 AVN FCSTS, MSL
C          AND 500MB DAILY AND MONTHLY TABLES WILL YIELD ABOUT
C          5K LINES AND CPU TIME OF 5-6 MINUTES).
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  IBM RS6000
C
C$$$
C
       PARAMETER    (MXCYL=1,MXNET=14,MXLVL=6,MXMDL=4,MXFCT=10,MXHR=10)
       CHARACTER(10) JRUN(MXMDL),ITRUTH
       CHARACTER(6) PLVL(MXLVL)
       CHARACTER(3) JMDL(MXMDL),JANL(MXMDL),NAME,NTRU
       INTEGER(8)  JDATE,NDATE,IDATE(MXCYL),IVT(4,MXCYL) 
       INTEGER(8)  IDS(12,MXLVL)
       INTEGER(4)  NFCTS(MXMDL),NPTS(MXNET)
       INTEGER*4    ipad
       LOGICAL(1)  IVFCT(MXFCT,MXHR),IVLVL(MXLVL),IVMDL(MXMDL),
     &             IVPRT(3),IDLY(MXLVL)
c
c
       COMMON /XDATA/ IDATE,IVFCT,IVLVL
       COMMON /WDATA/ IVT,IDS,PLVL
       COMMON /YDATA/ NBEGIN,NEND,ICYB,ICYE,ICYI,IVFLD,IFTINC,
     1                JLVLS,JFCTS,MDANL,ITRUTH
       common /cdata/ ivfcn
       COMMON/LIMITS/ IBGN(146),IEND(146),JBGN(12),JEND(12),
     1        IBRGN,IERGN,JBRGN,JERGN,
     2        KBGBL,KEGBL,LBGBL,LEGBL,KBLL,KELL,LBLL,LELL,NPTS
C
       DATA  ICON1 / 1000000000 /
C
 100  FORMAT(I5)
 101  FORMAT(1X,I8,1X,I8)
C#######
 102  FORMAT(I10)
C#######
 105  FORMAT(1X,6L1,2I3,1X,4L1,4I3,1X,A9,1X,I1,1X,3L1,1X,6L1)
 110  FORMAT(i1,1x,i3,1x,i4,1x,a6)
 120  FORMAT(1X,A3,1X,A9,1X,A3,1X,10L1,I3)
 130  FORMAT(4I3,3X,I3,3X,3I3,I4)
 140  FORMAT(12I3,/,12I3)
 150  FORMAT( 14I3,/,14I3,/,17I3,/,2(13I3,/),13I3,/,21I3,/,
     1       15I3,/,8I3,/,18I3)
 160  FORMAT(12I3)
 200  FORMAT('  *** VERIFICATION PERIOD ',I12,' TO ' ,I12,' *** ',
     1       /,10X,'PRESSURE LEVELS = ',6L1,',  TOTAL = ',
     2       I2,', TO DO= ',I2,' .... MODELS = ',4L1,',  TOTAL= ',
     3       I2,', TO DO= ',I2,/, 15X,'.....TOTAL FCSTS = ',I2,
     4       5X,'..... VERIFYING FLD = ',I2,2X,A9,3X,'(MODEL =',I2,
     5       ')',3X,'IVPRT= ',3L1,3X,'IDLY= ',6L1)
 210  FORMAT(10X,'.....IDENT  = ',I2,' ... ',3i7,5X,
     1       ' ... ',A6,' .....')
 220  FORMAT(10X,'.....MODEL ',I2,2X,A3,' ... ',A9,
     1       ' RUN ... ',A3,' ANALYSIS ..... ',/,30X,'FCSTS = ',10L1,
     2       ' .. TOTAL OF ',I2,' TIMES .. TO DO = ',I2)
 230   FORMAT(5X,'JSNET AND JFNET (LATLON NET)=',2I4,' ...NET1 AND ',
     1        'NET2 (TOTAL NET)= ',2I4,/,10X,'KNET1 AND KNET2= ',2I5,
     2        ' ... VERIFY CYCLES (BEGIN,END,INCREMENT)= ',3I3,
     3        ' ... FCST INCREMENT= ',I3,/,10X,'ISAVE= ',I2,3X,
     4        'COPY TO TAPE (1=YES,0=NO)',/)
C
 240   FORMAT(10X,'CYCLE=',I3,4X,'YR/MN/DY/HR = ',4I6,8X,
     1       'IDATE = ',i8)
 245   FORMAT(10X,'CYCLE=',I3,4X,'YR/MN/DY/HR = ',4I6,8X,
     1       'IDATE = ',i10)
 250   FORMAT(10X,'...........LAST CYCLE DATE = ',I10,4X,
     1        'REQUESTED END DATE = ',I10,4X,'TOTAL CYCLES= ',I4,///)
 260   FORMAT(10X,'***** CALLING SUBROUTINE S1GLBL *****',/,
     1        20X,'MODEL  NET KNET JNET NAME NTRU NPTS(1-MXNET)',/,
     2        20X,4I5,2X,A3,2X,A3,14I4,/, 20X,
     3        'IVPRT    IDLY ISAVE',/,20X,2X,3L1,2X,6L1,I6,/,
     4        22X,'|||',/, 22X,'||',12('-'),' GRADIENT ANALYSIS',/,
     5        22X,'|',8('-'),' S1 (TRUE GRAD)',/, 22X,
     6        4('-'),' S1 (MAX GRAD)',///)
 270   FORMAT(10X,'***** CALLING SUBROUTINE S1RGNL *****',/,
     1        20X,'MODEL  NET KNET JNET NAME NTRU NPTS(1-MXNET)',/,
     2        20X,4I5,2X,A3,2X,A3,14I4,/, 20X,
     3        'IVPRT    IDLY ISAVE',/,20X,2X,3L1,2X,6L1,I6,/,
     4        22X,'|||',/, 22X,'||',12('-'),' GRADIENT ANALYSIS',/,
     5        22X,'|',8('-'),' S1 (TRUE GRAD)',/, 22X,
     6        4('-'),' S1 (MAX GRAD)',///)
 280   FORMAT(15X,'IVFCN= ',I3,5X,'1=MONTH VFCN,   4=',
     1        'daily VFCN AND SAVE TO DISK',/,30X,'OTRNUM=',
     2        'daily VFCN ONLY',///)
 300   FORMAT(5X,'**** DIMENSIONS OF VERIFICATION NETWORKS ****')
 310   FORMAT(50X,'ROW TO ROW ... COL TO COL',/,
     1        10X,'REGIONAL AREA 1',50X,I3,4X,I3,5X,I3,4X,I3,
     2        114X,'NPTS= ',I4,/,
     3        10X,'GLOBAL AREA 1',  50X,I3,4X,I3,5X,I3,4X,I3,
     4        114X,'NPTS= ',I4,/,
     5        10X,'GLOBAL EUROPE',  50X,I3,4X,I3,5X,I3,4X,I3,
     6        114X,'NPTS= ',I4)
 320   FORMAT(10X,'5 DEG LAT, 10 DEG LON (1 THRU 4) ... ',
     1        '2.5 DEG LATLON (5 THRU 12) NETWORKS ........')
 330   FORMAT(10X,'AREA= ',I3,5X,'START ROW= ',I3,5X,'END ROW= ',I3,
     1        95X,'.... ',I3,' ROWS')
 331   FORMAT(12X,'START COL= ',7I4,5X,'END COL= ',7I4,
     1        114X,'NPTS= ',I4)
 335   FORMAT(12X,'START COL= ',17I4,5X,/,14X,'END COL= ',17I4,
     1        114X,'NPTS= ',I4)
 336   FORMAT(12X,'START COL= ',13I4,5X,/,14X,'END COL= ',13I4,
     1        114X,'NPTS= ',I4)
 339   FORMAT(12X,'START COL= ',21I3,5X,/,14X,'END COL= ',21I3,
     1        114X,'NPTS= ',I4)
 340   FORMAT(12X,'START COL= ',15I4,5X,/,14X,'END COL= ',15I4,
     1        114X,'NPTS= ',I4)
 341   FORMAT(12X,'START COL= ',8I3,5X,'END COL= ',8I3,
     1        114X,'NPTS= ',I4)
 342   FORMAT(12X,'START COL= ',18I4,5X,/,14X,'END COL= ',18I4,
     1        114X,'NPTS= ',I4,/,10X,'***********************',//)
 510   FORMAT('  ... ERROR IN DATE ... STOP ... ',4I3)
C
C
C...................................................................
C.....READ IN DATA, PRINT, AND INITIALIZE ARRAYS.....
C        1.  VERIFICATION OPTION
C        2.  VERIFICATION PERIOD (BEGIN AND END) FOR MONTHLY
C        3.  CURRENT 00Z DATE FOR FOUR DAY ETA VFCN
C        4.  grib pds(5), pds(6), pds(7) for verification fields
C        5.  NUMBER OF LEVELS TO DO, NUMBER OF FORECASTS TO DO,
C            VERIFYING FIELD TO USE, AND TABLES TO BE PRINTED.
C        6.  MODELS, RUN, AND ANALYSIS
C        7.  VFCN NETWORKS, FCST HR INCREMENT, CYCLES
C...................................................................
C
       CALL IFILL(plvl,11,MXLVL,1,0)
       CALL IFILL(NPTS,MXNET,1,1,0)
C
       READ(18,100,END=900) IVFCN
C
C......MONTHLY VFCN (IVFCN=1) OR DAILY VFCN (IVFCN=4)
       IF(IVFCN  .EQ.  1) THEN
        READ(10,101,END=900) NBEGIN,NEND
       ELSE
        READ(11,102,END=900) NDATE
          nbegin=ndate
          nend=ndate
       endif
C
       LOOP10: DO J = 1,MXLVL
         read(12,*,end=900) (IDS(I,J),I=1,3),plvl(j)
         IDS(12,J)=99
       PRINT 210,J,(IDS(K,J),K=1,3),PLVL(J)
       END DO LOOP10
C
       READ(13,105,END=900) IVLVL,JLVLS,NLVLS,IVMDL,JMDLS,NMDLS,
     1                      JFCTS,IVFLD,ITRUTH,MDANL,IVPRT,IDLY
       PRINT 105, IVLVL,JLVLS,NLVLS,IVMDL,JMDLS,NMDLS,JFCTS,
     1            IVFLD,ITRUTH,MDANL,IVPRT,IDLY
       PRINT 200, NBEGIN,NEND,IVLVL,JLVLS,NLVLS,IVMDL,JMDLS,NMDLS,
     1            JFCTS,IVFLD,ITRUTH,MDANL,IVPRT,IDLY
       LOOP12: DO I = 1,JMDLS
       READ(13,120,END=900) JMDL(I),JRUN(I),JANL(I),
     1             (IVFCT(J,I),J=1,JFCTS),NFCTS(I)
       PRINT 220,I,JMDL(I),JRUN(I),JANL(I),(IVFCT(J,I),J=1,JFCTS),
     1             JFCTS,NFCTS(I)
       END DO LOOP12
C
       READ(13,130,END=900) JSNET,JFNET,NET1,NET2,IFTINC,
     1      ICYB,ICYE,ICYI,ISAVE
       KNET1 = MXCYL * NET1
       KNET2 = MXCYL * NET2
       PRINT 230, JSNET,JFNET,NET1,NET2,KNET1,KNET2,ICYB,ICYE,ICYI,
     1      IFTINC,ISAVE
C
C...................................................................
C        5.  ROW AND COLUMN DIMENSIONS OF VFCN NETWORKS
C...................................................................
       READ(14,140,END=900) JBGN,JEND
       READ(14,150,END=900) IBGN
       READ(14,150,END=900) IEND
       READ(14,160,END=900) IBRGN,IERGN,JBRGN,JERGN,KBGBL,KEGBL,
     1      LBGBL,LEGBL,KBLL,KELL,LBLL,LELL
       JP1 = JERGN - JBRGN + 1
       IP1 = IERGN - IBRGN + 1
       NP1 = JP1 * IP1
       JP2 = KEGBL - KBGBL + 1
       IP2 = LEGBL - LBGBL + 1
       NP2 = JP2 * IP2
       JP3 = KELL  - KBLL  + 1
       IP3 = LELL  - LBLL  + 1
       NP3 = JP3 * IP3
       NPTS(1) = NP2
       NPTS(14) = NP1
C
       PRINT 300
       PRINT 310, JBRGN,JERGN,IBRGN,IERGN,NP1,LBGBL,LEGBL,
     1      KBGBL,KEGBL,NP2,LBLL,LELL,KBLL,KELL,NP3
       PRINT 320
       I1 = 1
       I2 = 0
       IPTS = 0
       LOOP20: DO K = 1,12
       KP1 = K + 1
       JD = JEND(K) - JBGN(K) + 1
       I2 = I2 + JD
       PRINT 330,K,JBGN(K),JEND(K),JD
C
        LOOP18: DO IC = I1,I2
        IPTS = IPTS + IEND(IC) - IBGN(IC) + 1
        END DO LOOP18
C
CCC    GO TO (21,21,21,21,25,26,26,26,29,30,31,32) K
       IF(K .GE. 1 .AND. K .LE. 4) THEN
         PRINT 331, (IBGN(I),I=I1,I2),(IEND(J),J=I1,I2),IPTS
       ENDIF
       IF(K .EQ. 5) THEN
         PRINT 335, (IBGN(I),I=I1,I2),(IEND(J),J=I1,I2),IPTS
       ENDIF
       IF(K .GE. 6 .AND. K .LE. 8) THEN
         PRINT 336, (IBGN(I),I=I1,I2),(IEND(J),J=I1,I2),IPTS
       ENDIF
       IF(K .EQ. 9) THEN
         PRINT 339, (IBGN(I),I=I1,I2),(IEND(J),J=I1,I2),IPTS
       ENDIF
       IF(K .EQ. 10) THEN
       PRINT 340, (IBGN(I),I=I1,I2),(IEND(J),J=I1,I2),IPTS
       ENDIF
       IF(K .EQ. 11) THEN
       PRINT 341, (IBGN(I),I=I1,I2),(IEND(J),J=I1,I2),IPTS
       ENDIF
       IF(K .EQ. 12) THEN
       PRINT 342, (IBGN(I),I=I1,I2),(IEND(J),J=I1,I2),IPTS
       ENDIF
       I1 = I2 + 1
       NPTS(KP1) = IPTS
       IPTS = 0
       END DO LOOP20
C
C....................................................................
C.....SETUP VERIFICATION TIME TABLE.....
C....................................................................
C
       CALL IFILL(IDATE,MXCYL,1,1,0)
       CALL IFILL(IVT,4,MXCYL,1,0)
C
        KOUNT = 0
        KBEGIN = NBEGIN
        LOOP40: DO IJ = 1,MXCYL
        INCR = IJ - 1
        KOUNT = KOUNT + 1
c
c  for a daily verification the nbegin=nend=idate so the following
c  line is set.
c
         IDATE(IJ)=KBEGIN
C
        IF ( IDATE(IJ) .LT. ICON1 ) THEN
C
C         DATE FORMAT: YYMMDDHH
C
         CALL DATECNV8(IDATE(IJ),IVT(1,IJ),IVT(2,IJ),IVT(3,IJ),
     1                 IVT(4,IJ))
C
         PRINT 240,IJ,IVT(1,IJ),IVT(2,IJ),IVT(3,IJ),IVT(4,IJ),
     1             IDATE(IJ)
C
        ELSE
C
C         DATE FORMAT: YYYYMMDDHH
C  
          CALL DATECNV10( IDATE(IJ), IVT(1,IJ), IVT(2,IJ),
     1                   IVT(3,IJ),  IVT(4,IJ))
          PRINT 245,IJ,IVT(1,IJ),IVT(2,IJ),IVT(3,IJ),IVT(4,IJ),
     1              IDATE(IJ)
C
        END IF
C
C
        IF(IDATE(IJ).EQ.nEND) EXIT LOOP40
        END DO LOOP40
C
        PRINT 250,IDATE(IJ),nEND,KOUNT
C
C  .  ................................
C  .  CALCULATE LFM GRID 1A LAT-LON  .
C  .  ................................
C
       CALL LFMLL
C
C....................................................................
C......DO S1 SCORE TABULATION......
C....................................................................
C
       PRINT 280,IVFCN
       NTRU = JMDL(MDANL)
       LOOP50: DO MODEL = 1,JMDLS
       NAME = JMDL(MODEL)
       IF(.NOT. IVMDL(MODEL)) CYCLE LOOP50 
C
       IF(MODEL  .LE.  2) THEN
        NET = NET1
        KNET = KNET1
        JNET = JSNET
C
        PRINT 260,MODEL,NET,KNET,JNET,NAME,NTRU,NPTS,IVPRT,
     1            IDLY,ISAVE
        CALL S1GLBL(MODEL,NET,KNET,JNET,NAME,NTRU,NPTS,IVPRT,
     1              IDLY,ISAVE)
C
       ELSE
        NPTS(1) = NP1
        NET = NET2
        KNET = KNET2
        JNET = JFNET
        JFCTS = 7
        PRINT 270,MODEL,NET,KNET,JNET,NAME,NTRU,NPTS,IVPRT,
     1            IDLY,ISAVE
        CALL S1RGNL(MODEL,NET,KNET,JNET,NAME,NTRU,NPTS,IVPRT,
     1              IDLY,ISAVE)
C
       ENDIF
C  Change JFCTS above from a 4 to a 7 io include every 12hr forecast.
C   Before 12hr, 24hr, 36hr, and 48hr.  7 means 12hr, 24hr, 36hr, 48hr,
C   60hr, 72hr, and 84hr.
C
       END DO LOOP50
C
 900    CONTINUE
C
       STOP
       END
c
      SUBROUTINE GETGRIB(run,id,ianl,iver,ipts,ilon,ilat,ilon2,ilat2,
     1                   ifhr,hout,hout2,iret)
c  subprogram getgrib
c  programmer:  Andrew Krein
c
c  Abstract:  Retrieves the requested grib data for a particular
c  valid time.
c
c  Program History Log:
c    96-06-20  Andrew Krein
c    98-08-27  C. Vlcek      Make Y2K compliant (main).
c    99-09-09  C. Vlcek      Convert for use on IBM RS6000.
c
c  Usage:  call getgrib(run,id,ianl,iver,ipts,ilon,ilat,ifhr,hout,ierr)
c    Input Argument List:
c      run:  integer model name (1=GFS, 2=AVN, 3=NGM, 4=ETA)
c      id:   pds numbers from label
c      ianl: non-zero for analysis
c      iver: non-zero for verifying at desired date/time
c      ipts: total number of points of grib data
c      ilon: number of longitude points
c      ilat: number of latitude points
c      ifhr: forecast hour to call
c
c    Output Argument List:
c      hout:  grid data array
c      hout2: secondary grid data array
c      ierr:  error message from getgb
c
c  Other variables used
c    mxmdl - number of models in verification
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
       parameter (mxmdl=4,mxfct=11,mxlvl=6,
     1            mxcyl=1,mxin=360*181,mxout=145*37)
       character(10) itruth
       character(6) plvl
       logical(1) ivfct,ivlvl
C      logical(1) li(ipts,1),lo(5365,1),lo2(2385,1)
C      logical(1) lo3(4225,1),lo4(646,1)
C      real hi(ipts,1),ho(5365,1),ho2(2385,1),ho3(4225,1),ho4(646,1)
       logical(1) li(ipts),lo(5365),lo2(2385)
       logical(1) lo3(4225),lo4(646)
       real hi(ipts),ho(5365),ho2(2385),ho3(4225),ho4(646)
       integer id(12),jpds(25),jgds(22)
       integer kpdsi(25),kgdsi(22),ibi(mxlvl),ibo(mxlvl)
       integer kpdso(25),kgdso1(22),kgdso2(22),kgdso(22),kgdso3(22)
       integer dat(11),inv(11)
       integer run, ipopt(20)
       real hout(ilon,ilat)
       real hout2(ilon2,ilat2)
       real rlat(5365),rlon(5365)
       real rlat2(2385),rlon2(2385)
       real rlat3(4225),rlon3(4225)
       real rlat4(646),rlon4(646)
       character gdso1(42),gdso2(42)
       character(8) cdat(11), cindex(11)
c
c  define output locations kdgso(1)<0 ==> rlat and rlon are input args
c  into ipolates
c
       common /xdata/ idate(mxcyl),ivfct(mxfct,mxmdl),ivlvl(mxlvl)
       common /wdata/ ivt(4,mxcyl),ids(12,mxlvl),plvl(mxlvl)
c
       common /ydata/ nbegin,nend,icyb,icye,icyi,ivfld,iftinc,
     1                jlvls,jfcsts,mdanl,itruth
c
       common /cdata/ ivfcn
       common /zdata/ xltln(2,53,45)
       data kgdso3 /0,38,17,20000,-145000,128,60000,-52500,2500,2500,
     1              64,0,0,0,0,0,0,0,0,255,0,0/
       data cdat /'GRIBAD0','GRIBFD1','GRIBFD2','GRIBFD3','GRIBFD4',
     &            'GRIBFD5','GRIBFD6','GRIBFD7','GRIBFD8','GRIBFD9',
     &            'GRIBFDL'/
       data cindex /'GRIBAI0','GRIBFI1','GRIBFI2','GRIBFI3','GRIBFI4',
     &              'GRIBFI5','GRIBFI6','GRIBFI7','GRIBFI8','GRIBFI9',
     &              'GRIBFIL'/
C
       k=1
       jpds=-1
       jpds(5)=id(1)
       jpds(6)=id(2)
       jpds(7)=id(3)
       ip=0
       ipopt=0
c
       if ((run .eq. 3) .or. (run .eq. 4)) then
          ida=30
          iin=70
          DO i=1,7
             dat(i)=ida
             inv(i)=iin
             ida=ida+1
             iin=iin+1
          ENDDO
       ENDIF
c
       IF (run .eq. 2) THEN
          ida=30
          iin=70
          DO i=1,10
             dat(i)=ida
             inv(i)=iin
             ida=ida+1
             iin=iin+1
          ENDDO
       ENDIF
c
       IF (run .eq. 1) THEN
          ida=30
          iin=70
          DO i=1,10
             dat(i)=ida
             inv(i)=iin
             ida=ida+1
             iin=iin+1
          ENDDO
       ENDIF
c
c  set idat and iinv for the analysis field
c
      IF ((ianl .eq. 1) .and. (iver .eq. 0)) THEN
         idat=30
         iinv=70
         call baopenr(idat,cdat(1),ierr)
         ier0 = ierr
         if (ierr.ne.0)   then
         print *, '*** Cannot open analysis file, ierr = ', ierr
         ENDIF
         call baopenr(iinv,cindex(1),ierr)
         if (ierr.ne.0)   then
           print *, '*** Cannot open analisis file, ierr = ', ierr
         endif
         if (ier0.ne.0.or.ierr.ne.0)    stop 99
      endif
c
c  set idat and iinv for a forecast field
c
       if ((ianl .eq. 0) .and. (iver .eq. 1)) then
         idat=dat(ifhr)+1
         iinv=inv(ifhr)+1
         j = ifhr+1
         call baopenr(idat,cdat(j),ierr)
         iret = ierr
         if (ierr.ne.0)    then
           print *, '*** Cannot open file', idat, '  ', cdat(j),
     &              ' ierr = ', ierr
         endif
         call baopenr(iinv,cindex(j),ierr)
         if (ierr.ne.0)    then
          print *, '*** Cannot open file', iinv, '  ', cindex(j),
     &              ' ierr = ', ierr
           iret = ierr
         endif
         if (iret.ne.0)    return
       endif
c
         call getgb(idat,iinv,ipts,0,jpds,jgds,
     1              ki,kr,kpdsi,kgdsi,li,hi,iret)
c
c *****  DIAGNOSTICS  ******
         print 1111, idat, iinv, iret, (hi(kkk),kkk=3001,3010)
 1111    format ('  DIAGNOSTICS:', 3i5, 10f10.1)
         if (iret.ne.0)    return
c *****  END DIAGS   *******
c
         ibi(k)=mod(kpdsi(4)/64,2)
c
c
c call makgds for the regional models standard 53x45 grid
c
       if ((run .eq. 3) .or. (run .eq. 4)) then
         ig=6
         ji=2385
         jo=2385
         jo3=646
         call makgds(ig,kgdso,gdso,lengds,iret)
c
       elseif ((run .eq. 1) .or. (run .eq. 2)) then
c
c  call makgds for the global models and the 145x37 grid (29)
c  and the 65x65 grid (27)
c
         ig1=29
         ji1=65160
         jo1=5365
         ig2=27
         jo2=4225
         call makgds(ig1,kgdso1,gdso1,lengds1,iret)
         call makgds(ig2,kgdso2,gdso2,lengds2,iret2)
         kgdso2(4)=-20826
c
       endif
c
c
       if (run .le. 2) then
c
c  interpolate from 1 degree lola to 2.5 degree lola grids
c
ckum       print*,'In getgrib before ipolates for run  #  ',run
       call ipolates(ip,ipopt,kgdsi,kgdso1,ji1,jo1,k,ibi,
     1                    li,hi,ko,rlat,rlon,ibo,lo,ho,iret1)
c       print*,'iret from ipolates 145x37 ',iret1
c
       iz=0
       do iy=1,ilat
         do ix=1,ilon
           iz=iz+1
           if (jpds(6) .eq. 102) then
             hout(ix,iy)=ho(iz)/100.0
           else
             hout(ix,iy)=ho(iz)
           endif
c
         enddo
       enddo
c
         else
c
c  extract the 53x45 regional model grid
c
        call ipolates(ip,ipopt,kgdsi,kgdso,ji,jo,k,ibi,
     1                li,hi,ko,rlat2,rlon2,ibo,lo2,ho2,iret2)
c       print*,'iret from ipolates 53x45= ',iret2
c
       iz=0
       do iy=1,ilat
         do ix=1,ilon
           iz=iz+1
           if (jpds(6) .eq. 102) then
             hout(ix,iy)=ho2(iz)/100.0
           else
             hout(ix,iy)=ho2(iz)
           endif
c
         enddo
       enddo
c
       endif
c
c
c  interpolate from 1 degree lola grid to 65x65 grid
c
       if ((run .eq. 1) .or. (run .eq. 2)) then
C
         call ipolates(ip,ipopt,kgdsi,kgdso2,ji1,jo2,k,ibi,
     1                 li,hi,ko,rlat3,rlon3,ibo,lo3,ho3,iret3)
C
       iz2=0
       do iy=1,65
         do ix=1,65
           iz2=iz2+1
           if (jpds(6) .eq. 102) then
             hout2(ix,iy)=ho3(iz2)/100.0
           else
             hout2(ix,iy)=ho3(iz2)
           endif
c
         enddo
       enddo
       endif
c
c  interpolate from 53x45 regional model grid to 2.5 degree grid over
c  regional model area
c
       if ((run .eq. 3) .or. (run .eq. 4)) then
c         call lolacalc(rlon2,rlat2)
         call ipolates(ip,ipopt,kgdsi,kgdso3,ji,jo3,k,ibi,
     1                 li,hi,ko,rlat4,rlon4,ibo,lo4,ho4,iret4)
c       print*,'iret from ipolates 38x17= ',iret4
       izz=0
       do iy=1,17
         do ix=1,38
           izz=izz+1
           if (jpds(6) .eq. 102) then
             hout2(ix,iy)=ho4(izz)/100.0
           else
             hout2(ix,iy)=ho4(izz)
           endif
         enddo
       enddo
       endif
c
       RETURN
       END

