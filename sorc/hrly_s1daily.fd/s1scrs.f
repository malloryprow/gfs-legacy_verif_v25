       SUBROUTINE S1SCRS(MODEL,NAME,NTRU,PLEVEL,KNET,NET,IVT,IVFCT,
     1                  NPTS,IPDLY,S1SCR,ITYPE)
C SUBPROGRAM:    S1SCRS
C   PRGMMR: ROBERT HIRANO    ORG: W/NMC42    DATE: 88-12-14
C
C ABSTRACT: TABULATE S1 SCORES FOR MODEL VERIFICATION NETWORKS
C           (GFS,AVN HAVE 14 AREAS WHEREAS THE NGM AND LFM HAVE 9).
C
C PROGRAM HISTORY LOG:
C   88-12-14  ROBERT HIRANO
C   92-06-23  ROBERT HIRANO         LLEURP REPLACED BY RGNAR1
C
C USAGE:    CALL S1SCRS(MODEL,NAME,NTRU,PLEVEL,KNET,NET,IVT,IVFCT,
C                       NPTS,IPDLY,S1SCR,ITYPE)
C   INPUT ARGUMENT LIST:
C     ITRUTH   - VERIFYING FIELD NAME
C     IVT      - YEAR, MONTH, DAY, HOUR OF EACH CYCLE
C     IVFCT    - REQUESTED FORECAST HOURS
C     MODEL    - MODEL ID (1=GFS,2=AVN,3=NGM,4=LFM)
C     NAME     - MODEL NAME
C     NTRU     - VERIFYING FIELD NAME
C     KNET     - NET TIME NUMBER OF CYCLES (1)
C     NET      - NUMBER OF VERIFICATION NETWORKS
C     PLEVEL   - PRESSURE LEVEL NAME
C     S1SCR    - S1 SCORES FOR ALL CYCLES AND FORECAST HOURS
C     ITYPE    - S1 SCORES TYPE (1=MAX GRAD AND 2=TRUE GRAD ONLY)
C     NPTS     - NUMBER OF POINTS IN VERIFICATION NETWORK
C     IPDLY    - PRINT DAILY VALUES (1=YES,0=NO)
C
C$$$
C
       PARAMETER   (NETX=14, JFCTX=10, MXCYL=1, MXHR=10, JSTTX=10)
       CHARACTER(10) ITRUTH
       CHARACTER(6) PLEVEL,AREA(NETX),AREA1
       CHARACTER(5)  JSTAT(JSTTX)
       CHARACTER(3) NAME,NTRU,MONTH(12),GNAME(2),GRDN
       REAL    S1SCR(KNET,JFCTS),X(JSTTX,NETX,JFCTX),W(MXCYL)
       INTEGER(8) IVT(4,MXCYL),NPTS(NETX)
       LOGICAL(1) IVFCT(JFCTX,MXHR)
C
       COMMON /YDATA/ NBEGIN,NEND,ICYB,ICYE,ICYI,IVFLD,IFTINC,
     1                JLVLS,JFCTS,MDANL,ITRUTH
C
       common /cdata/ ivfcn
C
       DATA AREA/'GBLAR1','59GRID','WEST33','EAST33','49GRID',
     1           'LLNAMR','LL59PT','LLW33 ','LLE33 ','LLASIA',
     2           'LLNPAC','LLALSK','LLNATL','RGNAR1'/
       DATA AREA1/'RGNAR1'/
       DATA GNAME/'MAX','TRU'/
       DATA JSTAT/'  NUM',' MEAN','ABSDV','  VAR',' SDEV',' SKEW',
     1           ' KURT','  MAX','  MIN','RANGE'/
       DATA MONTH/'JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG',
     1            'SEP','OCT','NOV','DEC'/
C
 100   FORMAT(/)
 105   FORMAT(15X,'***** S1 SCORE VERIFICATION (S1=ERROR/',
     1        A3,') ***** ',/,6X,'.....MODEL= ',A3,' .....LEVEL= ',A6,
     2        ' .....TRUTH= ',A3,1X,A9,' .....FCST HR= ',I3,' .....',
     3        /,11X,'V.T.',/, '  CYL  YR/MN/DY/HR', 14(2X,A6),/)
 110   FORMAT(I5,1X,4I3)
 120   FORMAT('+',30X,'......  N O T   A V A I L A B L E  .....')
 130   format(i5,1x,4i3,14f8.2)
 200   FORMAT(15X,'***** S1 SCORE VERIFICATION (S1=ERROR/',
     1        A3,') ***** ',/,6X,'.....MODEL= ',A3,' .....LEVEL= ',A6,
     2        ' .....TRUTH= ',A3,1X,A9,' .....FCST HR= ',I3,' .....',
     3        /,11X,'V.T.',/, '  CYL  YR/MN/DY/HR',  9(2X,A6),/)
 230   format(i5,1x,4i3,9f8.2)
 300   FORMAT( //,10X,' NUM',19X,14F8.0,/,10X,'MEAN',19X,14F8.2,/,
     1        10X,'SDEV',19X,14F8.2,/)
 310   FORMAT( //,10X,' NUM',19X, 9F8.0,/,10X,'MEAN',19X, 9F8.2,/,
     1        10X,'SDEV',19X, 9F8.2,/)
 320   FORMAT(20X,'...',I4,' HR  ',A3,' FCST AT ',A6,' ...',
     1        'S1 (ERROR/',A3,'), TRUTH= ',A3,1X,A9,' ...')
 400   FORMAT(/,25X,'..... STATISTICAL ANALYSIS S1 SCORE (S1=',

     1        'ERROR/',A3,') DISTRIBUTION .....',/,
     2        9X,'.... ',A3,' ... VERIFIED AGAINST ',
     3        A3, 1X, A9,'  AT ',A6,'  FOR THE PERIOD  ',I2,'Z',
     4        I3,A3,I2,' - ',I2,'Z',I3,A3,I2,' ....',//,
     5        9X, 'FTHR     AREA',4X,10(3X,A5),5X,'NETPTS',/)
 410   FORMAT(9X,I3,' ....')
 420   FORMAT(10X,A6, 2X,F8.0,F8.2,5F8.3,3F8.2,5X,I5)
 430   FORMAT(/)
 500   FORMAT(10X,'NC,XM,AD,VAR,SD,A3,A4,ZX,ZN,R= ',I3,F8.2,
     1        3F8.3,2F8.4,3F8.2)
C
       CALL AFILL(X,JSTTX,NETX,JFCTX,.0)
       GRDN = GNAME(ITYPE)
       IF(MODEL  .GT.  2) AREA(1) = AREA1
       IF(IVFCN  .NE.  4) THEN
C
C..................................................................
C      DO STATISTICAL ANALYSIS OF S1 SCORE DISTRIBUTION
C..................................................................
C
       LOOP10: DO IFT = 1,JFCTS
       LOOP12: DO IAR = 1,NET
       NC = 0
       LOOP14: DO I = IAR,KNET,NET
       IF(S1SCR(I,IFT)  .EQ.  0.0) CYCLE LOOP14
       NC = NC + 1
       W(NC) = S1SCR(I,IFT)
       END DO LOOP14
C
       IF(NC .EQ. 0) CYCLE LOOP12
       CALL STATS(W,NC,XM,AD,VAR,SD,A3,A4,ZX,ZN,R)
       X(1,IAR,IFT) = NC
       X(2,IAR,IFT) = XM
       X(3,IAR,IFT) = AD
       X(4,IAR,IFT) = VAR
       X(5,IAR,IFT) = SD
       X(6,IAR,IFT) = A3
       X(7,IAR,IFT) = A4
       X(8,IAR,IFT) = ZX
       X(9,IAR,IFT) = ZN
       X(10,IAR,IFT) = R
       END DO LOOP12
       END DO LOOP10
       ENDIF
C
C..................................................................
C      PRINT S1 SCORE TABLES FOR EACH FORECAST HOUR
C..................................................................
C
       LOOP15: DO MM=1,1
       IF(IPDLY .NE. 1) CYCLE LOOP15
C
c       PRINT 100
       LOOP20: DO IFT = 1,JFCTS
       IF(.NOT. IVFCT(IFT,MODEL)) CYCLE LOOP20
       IFHR = IFTINC*IFT
C.....
       IF(MODEL .LE. 2) THEN
         PRINT 105,GRDN,NAME,PLEVEL,NTRU,ITRUTH,IFHR,
     1             (AREA(I),I=1,NET)
       ELSE
         PRINT 200,GRDN,NAME,PLEVEL,NTRU,ITRUTH,IFHR,
     1             (AREA(I),I=1,NET)
       ENDIF
C.....
C
       LOOP30: DO I = ICYB,ICYE,ICYI
       K1 = NET*(I-1) + 1
       K2 = K1 + NET - 1
C.....
        IF(S1SCR(K1,IFT)  .LE.  .0) THEN
         PRINT 120
         EXIT LOOP30
        ELSE
C...
         IF(MODEL .LE. 2) THEN
          PRINT 130, I,(IVT(J,I),J=1,4),(S1SCR(K,IFT),K=K1,K2)
         ELSE
          PRINT 230, I,(IVT(J,I),J=1,4),(S1SCR(K,IFT),K=K1,K2)
         ENDIF
C...
       ENDIF
C.....
       END DO LOOP30
C.....
       IF(IVFCN  .EQ.  4) THEN
        PRINT 430
       ELSE
        IF(MODEL .LE. 2) THEN
        PRINT 300,(X(1,J1,IFT),J1=1,NET),(X(2,J2,IFT),J2=1,NET),
     1            (X(5,J3,IFT),J3=1,NET)
        ELSE
        PRINT 310,(X(1,J1,IFT),J1=1,NET),(X(2,J2,IFT),J2=1,NET),
     1            (X(5,J3,IFT),J3=1,NET)
        ENDIF
         PRINT 320,IFHR,NAME,PLEVEL,GRDN,NTRU,ITRUTH
       ENDIF
C.....
       IF(IVFCN  .EQ.  1) PRINT 100
       END DO LOOP20
C
       END DO LOOP15
C
C..................................................................
C      PRINT STATISTICAL ANALYSIS TABLES FOR EACH FORECAST HOUR
C..................................................................
C
       IF(IVFCN  .NE.  4) THEN
       MONB = NBEGIN
       MONE = NEND
       KC = 0
       KC5 = 1
       LOOP40: DO N = 1,JFCTS
       IF(X(1,1,N) .EQ. 0.) CYCLE LOOP40
       KC = KC + 1
C.....
       IF(KC  .EQ.  KC5) THEN
        PRINT 400,GRDN,NAME,NTRU,ITRUTH,PLEVEL,NBEGIN,NBEGIN,
     1            MONTH(MONB),NBEGIN,NEND,NEND,MONTH(MONE),
     2            NEND,(JSTAT(I),I=1,JSTTX)
        KC5 = KC5 + 4
       ENDIF
C.....
       NFTHR = IFTINC * N
       PRINT 410,NFTHR
       LOOP42: DO K = 1,NET
       PRINT 420,AREA(K),(X(I,K,N),I=1,JSTTX),NPTS(K)
       END DO LOOP42
       PRINT 430
C
       END DO LOOP40
       ENDIF
C
       RETURN
       END

