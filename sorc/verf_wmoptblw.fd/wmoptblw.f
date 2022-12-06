C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C
C MAIN PROGRAM: wmoptblw
C   PRGMMR: LILLY            ORG: NP12        DATE: 2009-03-09
C
C ABSTRACT: READ WMO MONTHLY VERIFICATION STATISTICS ON ARCHIVE DISK
C
C PROGRAM HISTORY LOG:
C   1994-03-09  Original Author: Charles Vlcek
C
C USAGE:
C   INPUT FILES:
C     FT40F001 - READS VERIFICATION STATISTICS ON ARCHIVE DISK
C
C   OUTPUT FILES:
C     FT60F001 - Prints WMO MONTHLY VERIFICATION TABLES ACCORDING TO
C                WMO STANDARDS for THE Northern Hemisphere,
C                THE Southern Hemisphere and the TROPICS
C
C   SUBPROGRAMS CALLED:
C
C   EXIT STATES:
C     COND =   0 - SUCCESSFUL RUN
C
C REMARKS:
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE : IBM RS6000
C
C$$$

C     PROGRAM WMOPTBLW

C      READ WMO MONTHLY VERIFICATION STATISTICS ON ARCHIVE DISK    .
C         ~VWMOMON.mm (DATA WRITTEN IN NON STANDARD WMO            .
C         TABLE FORMAT ... REARRANGE TO CONFORM)                   .
C      PRINT WMO MONTHLY VERIFICATION TABLES ACCORDING TO WMO      .
C         STANDARDS ..... NORTHERN HEMISPHERE (1), TROPICS (2),    .
C         SOUTHERN HEMISPHERE (3)                                  .

C   97-02-01  Andrew S. Krein   conversion to cray completed       .
C   98-10-06  Charles L. Vlcek  f90 and y2k-compliant              .
C   99-09-23  Charles L. Vlcek  compile on IBM RS6000              .
C   09-10-01  LILLY  compile on P6                                 .
 
       DIMENSION Z(10,2),X(4,10,2,7,3)
       DIMENSION NC(9),NR(3),NT(9,3,2)

       DATA NC /1,2,3,4,1,2,3,2,1/, NR /1,3,2/,
     1      NT /4*1,5*0,4*2,3*4,2*6,4*3,3*5,2*7,
     2          4*1,3*3,2*5,4*2,3*4,2*6,9*0/

 100   FORMAT(I4,I2)
 110   FORMAT(///,10X,'***** WMO MONTHLY VERIFICATION ARCHIVE DISK ',
     1        '...YEAR AND MON = ',2I6,2X,'*****',///, 1X,
     2        'RGN TBL COL  FTHR= 1 - 10 DAYS ......... ',
     3        92X,' STAT  TBL  RGN',//)
 120   FORMAT (3I2,20F8.2)
 130   FORMAT (3I4,20F7.2,92X,3I5)

C......FILL ARRAY........................

       X = 999.99

C.     READ YEAR, MONTH, STATISTICAL ARRAY                      .
C.     ARCHIVED DATA FORMAT....                                 .
C.       REGION L3=1,2,3  NRNHEM, SRNHEM,TROPICS                .
C.       TABLE  L2=1,2,3  NRNHEM,SRNHEM(MSL,500MB,250MB)        .
C.                        TROPICS(850MB,250MB,XXXXX)            .
C.       COLUMN L1=1-4    HGT...MERR,RMSE,CORREL,S1             .
C.                 5-7    TMP...MERR,RMSE,CORREL                .
C.                 8-9    WND...RMSEV,MERR                      .

        READ(40,100,END=50) IYEAR,IMON
  20   CONTINUE
       READ(40,120,END=50) L3,L2,L1,Z
C
C.     REARRANGE INTO WMO STANDARD TABLE FORMAT                 .
C.       FORMAT ... N1=STAT  1-4  MERR,RMSE OR RMSEV,           .
C.                                ANOCOR, S1 SCORE              .
C.                  N3=WMO TBL NRN AND SRN HEM                  .
C.                      1-7  MSL,Z500,Z250,T500,T250,V500,V250  .
C.                     WMO TBL TROPICS                          .
C.                      1-5  Z850,Z250,T850,T250,V850,V250      .
C.                  N4=REGION  1=NRNHEM, 2=TROPICS, 3=SRNHEM    .

        N4 = NR(L3)
        N1 = NC(L1)
       IF (N4.EQ.2) THEN
        N3 = NT(L1,L2,2)
       ELSE
        N3 = NT(L1,L2,1)
       ENDIF
       DO 60 I = 1,10
        DO J=1,2
          X(N1,I,J,N3,N4) = Z(I,J)
        END DO
  60   CONTINUE
       GO TO 20

  50   CONTINUE

C......PRINT VERIFICATION WMOTBLS.......................

       CALL WMOTBL(IYEAR,IMON,X)

       STOP
       END


       SUBROUTINE WMOTBL(MYR,MMN,X)

C......PRINT WMO STANDARD MONTHLY VERIFICATION TABLES.............

       CHARACTER(5)  NRMS(2)
       CHARACTER(8)  NLAT(3)
       CHARACTER(9)  NMON(13)
       CHARACTER(14) NGMT
       CHARACTER(16) NPRM(6)
       CHARACTER(19) NRGN(3)
       CHARACTER(27) NLVL(13)

       DIMENSION X(4,10,2,7,3),ZTBL(7,3)
       DIMENSION NTYP(7,3),NSTT(7,3)

       DATA  NRMS/'RMSE ','RMSEV'/,
     1       NLAT/'90N-20N','20N-20S','20S-90S'/,
     2       NMON/'JANUARY  ','FEBRUARY ','MARCH    ','APRIL    ',
     3            'MAY      ','JUNE     ','JULY     ','AUGUST   ',
     4            'SEPTEMBER','OCTOBER  ','NOVEMBER ','DECEMBER ',
     5            'ANNUAL   '/

       DATA  NGMT /'00 GMT  12 GMT'/,
     1       NPRM /'  MEAN ERROR    ', '     RMSE       ',
     2             '  CORRELATION   ', '   S1 SCORE     ',
     3             'MEAN SPEED ERROR', '     RMSEV      '/,
     4       NRGN /'NORTHERN HEMISPHERE', '     TROPICS       ',
     5             'SOUTHERN HEMISPHERE'/

       DATA  NLVL /'MEAN-SEA-LEVEL PRESSURE    ',
     1   '500 HPA GEOPOTENTIAL HEIGHT', '250 HPA GEOPOTENTIAL HEIGHT',
     2   '500 HPA TEMPERATURE        ', '250 HPA TEMPERATURE        ',
     3   '500 HPA WIND               ', '250 HPA WIND               ',
     4   '850 HPA GEOPOTENTIAL HEIGHT', '250 HPA GEOPOTENTIAL HEIGHT',
     5   '850 HPA TEMPERATURE        ', '250 HPA TEMPERATURE        ',
     6   '850 HPA WIND               ', '250 HPA WIND               '/

       DATA  ZTBL /1.1,1.2,1.3,1.4,1.5,1.6,1.7,
     1             2.1,2.2,2.3,2.4,2.5,2.6,0.0,
     2             3.1,3.2,3.3,3.4,3.5,3.6,3.7/

       DATA  NTYP /1,1,1,2,2,3,3, 1,1,2,2,3,3,0, 1,1,1,2,2,3,3/
     1       NSTT /1,2,3,4,5,6,7, 8,9,10,11,12,13, 0,
     2             1,2,3,4,5,6,7/

 100   FORMAT (///, 24X, 'VERIFICATION TO W.M.O. STANDARDS', /,
     1         20X, 40('-'), //, 18X, 'N.C.E.P./U.S.A.',
     2         15X,A9, 2X,I4,/, 16X, 48('-'),//, 23X,'GLOBAL ',
     3         'FORECAST SYSTEM MODEL (GFS)', /, 22X, 35('-'), ////)
 110   FORMAT (//,'  TABLE', 3X,F3.1, 3X,A19,' VERIFICATION AGAINST',
     1         ' ANALYSES (',A7,')', /, 16X, 59('-'), /,
     2         16X, A27, 5X, A9, 3X, I4, /, 16X, 47('-') )
 111   FORMAT (//, ' FORECAST', 3X, 4(1X,A16), /,'  PERIOD', 9X,
     1         '(HPA)',13X, '(HPA)',  7X, 14('-'), 3X, 14('-'),/,1X,
     2         '(HOURS)', 5X, A14, 3X,A14, 3X,A14, 3X,A14,/,' --------',
     3         4X, 6('-'), 2X, 6('-'), 3X, 6('-'), 2X, 6('-'),
     4         3X, 6('-'), 2X, 6('-'), 3X, 6('-'), 2X, 6('-'), /)
 112   FORMAT (//, ' FORECAST', 3X, 3(1X,A16), /, '  PERIOD', 10X,
     1         '(K)', 15X, '(K)', 8X, 14('-'), /, 1X,
     2         '(HOURS) ', 4X, A14, 3X, A14, 3X, A14, /, ' --------',
     3         4X, 6('-'), 2X, 6('-'), 3X, 6('-'), 2X, 6('-'),
     4         3X, 6('-'), 2X, 6('-'), /)
 113   FORMAT (//,' FORECAST', 3X, 2(1X,A16), /, '  PERIOD', 9X,
     1         '(M/S)', 13X, '(M/S)', /, 1X,
     2         '(HOURS)', 5X, A14, 3X, A14, /, ' --------',
     3         4X, 6('-'), 2X, 6('-'), 3X, 6('-'), 2X, 6('-'), /)
 114   FORMAT (//, ' FORECAST', 3X, 4(1X,A16), /,'  PERIOD', 9X,
     1         '( M )',13X, '( M )',  7X, 14('-'), 3X, 14('-'),/,1X,
     2         '(HOURS)', 5X, A14, 3X,A14, 3X,A14, 3X,A14,/,' --------',
     3         4X, 6('-'), 2X, 6('-'), 3X, 6('-'), 2X, 6('-'),
     4         3X, 6('-'), 2X, 6('-'), 3X, 6('-'), 2X, 6('-'), /)
  121   FORMAT (3X, I3, 6X, F7.1, 1X, F7.1, 2X, F7.1, 1X, F7.1,
     *          2x,F7.1,1x,F7.1,2x,F7.1, 1X, F7.1)
  122   FORMAT (3X, I3, 6X, F7.1, 1X, F7.1, 2X, F7.1, 1X, F7.1, 2X,
     *          F7.1, 1X, F7.1)
  123   FORMAT (3X, I3, 6X, F7.1, 1X, F7.1, 2X, F7.1, 1X, F7.1)
  131   FORMAT (3X, I3, 6X, F7.1, 1X, F7.1, 2X, F7.1, 1X, F7.1, 2X,
     *          F7.2, 1X, F7.2)
 150   FORMAT (///)
 200   FORMAT (///, '#', 28X, '**  NCEP/USA  GFS  **', //)

       IF (MMN.EQ.0)    MMN = 13
       PRINT 100, NMON(MMN),MYR

       NPAGE = 0
       DO 10 KRGN = 1,3

        DO 20 KTBL = 1,7
         KSTT = NSTT(KTBL,KRGN)
         KTYP = NTYP(KTBL,KRGN)
         IF (KTBL.EQ.7.AND.KRGN.EQ.2) GO TO 20
          PRINT 110, ZTBL(KTBL,KRGN),NRGN(KRGN),NLAT(KRGN),
     1               NLVL(KSTT),NMON(MMN),MYR
          IF (KTYP.EQ.1) THEN
          IF (KTBL.EQ.1.AND.KRGN.EQ.1) THEN
           PRINT 111,(NPRM(I),I=1,4),NGMT,NGMT,NGMT,NGMT
          ELSE
          IF (KTBL.EQ.1.AND.KRGN.EQ.3) THEN
           PRINT 111,(NPRM(I),I=1,4),NGMT,NGMT,NGMT,NGMT
          ELSE
           PRINT 114,(NPRM(I),I=1,4),NGMT,NGMT,NGMT,NGMT
          ENDIF
          ENDIF
          ENDIF

           IF (KTYP.EQ.2) THEN
            PRINT 112,(NPRM(I),I=1,3),NGMT,NGMT,NGMT
           ELSE
           IF (KTYP.EQ.3) THEN
            PRINT 113,NPRM(5),NPRM(6),NGMT,NGMT
           ENDIF
          ENDIF


         DO 30 KFHR = 1,10
          IHR = 24 * KFHR
          IF (KTYP.EQ.1) THEN
           IF (KSTT.EQ.1) THEN
            PRINT 121, IHR,X(1,KFHR,1,KTBL,KRGN),
     *          X(1,KFHR,2,KTBL,KRGN),X(2,KFHR,1,KTBL,KRGN),
     *          X(2,KFHR,2,KTBL,KRGN),X(3,KFHR,1,KTBL,KRGN),
     *          X(3,KFHR,2,KTBL,KRGN),X(4,KFHR,1,KTBL,KRGN),
     *          X(4,KFHR,2,KTBL,KRGN)
           ELSE
            PRINT 121, IHR,X(1,KFHR,1,KTBL,KRGN),
     *          X(1,KFHR,2,KTBL,KRGN),X(2,KFHR,1,KTBL,KRGN),
     *          X(2,KFHR,2,KTBL,KRGN),X(3,KFHR,1,KTBL,KRGN),
     *          X(3,KFHR,2,KTBL,KRGN),X(4,KFHR,1,KTBL,KRGN),
     *          X(4,KFHR,2,KTBL,KRGN)
           ENDIF
          ELSE
           IF (KTYP.EQ.2) THEN
            PRINT 122, IHR,X(1,KFHR,1,KTBL,KRGN),
     *          X(1,KFHR,2,KTBL,KRGN),X(2,KFHR,1,KTBL,KRGN),
     *          X(2,KFHR,2,KTBL,KRGN),X(3,KFHR,1,KTBL,KRGN),
     *          X(3,KFHR,2,KTBL,KRGN)
           ELSE
            PRINT 123, IHR,X(1,KFHR,1,KTBL,KRGN),
     *          X(1,KFHR,2,KTBL,KRGN),X(2,KFHR,1,KTBL,KRGN),
     *          X(2,KFHR,2,KTBL,KRGN)
           ENDIF
          ENDIF
   30    CONTINUE
         PRINT 150
         NPAGE = NPAGE + 1
         IF (NPAGE  .EQ.  3) THEN
          NPAGE = 0
          PRINT 200
         ENDIF
   20   CONTINUE
  10   CONTINUE


       RETURN
       END
