       SUBROUTINE S1GRAD(MODEL,NAME,NTRU,PLEVEL,KNET,NET,IVT,IVFCT,
     1                   NPTS,IPDLY,SGRAD)
C SUBPROGRAM:    S1GRAD
C   PRGMMR: ROBERT HIRANO    ORG: W/NMC42    DATE: 88-12-14
C
C ABSTRACT: ANALYZE S1 GRADIENT DATA FOR VERIFICATION NETWORKS
C           (MRF,AVN HAVE 14 AREAS WHEREAS THE NGM AND LFM HAVE 9).
C
C PROGRAM HISTORY LOG:
C   88-12-14  ROBERT HIRANO
C   92-06-23  ROBERT HIRANO      LLEURP REPLACED BY RGNAR1
C
C USAGE:    CALL S1GRAD(MODEL,NAME,NTRU,PLEVEL,KNET,NET,IVT,IVFCT,
C                        NPTS,IPDLY,SGRAD)
C   INPUT ARGUMENT LIST:
C     ITRUTH   - VERIFYING FIELD NAME
C     IVT      - YEAR, MONTH, DAY, HOUR OF EACH CYCLE
C     IVFCT    - REQUESTED FORECAST HOURS
C     MODEL    - MODEL ID (1=MRF,2=AVN,3=NGM,4=eta)
C     NAME     - MODEL NAME
C     NTRU     - VERIFYING FIELD NAME
C     KNET     - NET TIMES NUMBER OF CYCLES (1)
C     NET      - NUMBER OF VERIFICATION NETWORKS
C     PLEVEL   - PRESSURE LEVEL NAME
C     NPTS     - NUMBER OF POINTS IN VERIFICATION NETWORKS
C     IPDLY    - PRINT DAILY VALUES (1=YES,0=NO)
C     SGRAD    - GRAD (ERR,MAX,MAX/ANL,FCST/ANL)
C
C   SUBPROGRAM CALLED:
C     UNIQUE:   - AFILL, STATS
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN-77
C   MACHINE:  NAS
C
C$$$
C
       PARAMETER (NETX=14, JFCTX=10, MXCYL=1, MXHR=10, JSTTX=10)
       CHARACTER(10) ITRUTH
       CHARACTER(7) GFLD(4)
       CHARACTER(6) PLEVEL,AREA(NETX),AREA1
       CHARACTER(5)  JSTAT(JSTTX)
       CHARACTER(3) NAME,NTRU,MONTH(12)
       REAL    SGRAD(KNET,4,JFCTS),X(JSTTX,NETX,JFCTX),W(MXCYL)
       INTEGER(8) IVT(4,MXCYL),NPTS(NETX)
       LOGICAL(1) IVFCT(JFCTX,MXHR)
       COMMON /YDATA/ NBEGIN,NEND,ICYB,ICYE,ICYI,IVFLD,IFTINC,
     1                JLVLS,JFCTS,MDANL,ITRUTH
       common /cdata/ ivfcn
C
       DATA AREA/'GBLAR1','59GRID','WEST33','EAST33','49GRID',
     1           'LLNAMR','LL59PT','LLW33 ','LLE33 ','LLASIA',
CCC  2           'LLNPAC','LLALSK','LLNATL','LLEURP'/
     2           'LLNPAC','LLALSK','LLNATL','RGNAR1'/
       DATA AREA1/'RGNAR1'/
       DATA  GFLD/'ERR/PT ','MAX/PT ','TRU/PT ','FCT/TRU'/
       DATA JSTAT/'  NUM',' MEAN','ABSDV','  VAR',' SDEV',' SKEW',
     1           ' KURT','  MAX','  MIN','RANGE'/
       DATA MONTH/'JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG',
     1            'SEP','OCT','NOV','DEC'/
C
C
 100   FORMAT(//,3X,'... ',A7,' (GRADIENT)  ',A3,'  AT  ',A6,
     1        '  VRFCN FLD= ',A3,1X,A9,'  FCST= ',I3,' HRS .....',/,
     2          11X,'V.T.',/, '  CYL  YR/MN/DY/HR', 14(2X,A6),/)
 110   FORMAT(I5,1X,4I3)
 120   FORMAT('+',30X,'......  N O T   A V A I L A B L E  .....')
 130   FORMAT('+',19X,14F8.1)
 135   FORMAT('+',19X,14F8.2)
 200   FORMAT(//,3X,'... ',A7,' (GRADIENT)  ',A3,'  AT  ',A6,
     1        '  VRFCN FLD= ',A3,1X,A9,'  FCST= ',I3,' HRS .....',/,
     2          11X,'V.T.',/, '  CYL  YR/MN/DY/HR',  9(2X,A6),/)
 230   FORMAT('+',19X, 9F8.1)
 235   FORMAT('+',19X, 9F8.2)
 300   FORMAT( //,10X,' NUM',19X,14F8.0,/,10X,'MEAN',19X,14F8.1,/,
     1        10X,'SDEV',19X,14F8.1,/)
 305   FORMAT( //,10X,' NUM',19X,14F8.0,/,10X,'MEAN',19X,14F8.2,/,
     1        10X,'SDEV',19X,14F8.2,/)
 310   FORMAT( //,10X,' NUM',19X, 9F8.0,/,10X,'MEAN',19X, 9F8.1,/,
     1        10X,'SDEV',19X, 9F8.1,/)
 315   FORMAT( //,10X,' NUM',19X, 9F8.0,/,10X,'MEAN',19X, 9F8.2,/,
     1        10X,'SDEV',19X, 9F8.2,/)
 320   FORMAT(20X,'...',A7,' (GRADIENT) AT ',I3,' HR ',A3,
     1        ' FCST AT ',A6,'  VERIFIED AGAINST ',A3,1X,A9,' ...')
 400   FORMAT(/,25X,'..... STATISTICAL ANALYSIS ',A7,' (GRADI',
     1        'ENT) DISTRIBUTION .....',/,9X,'... ',A3,
     2        ' ... VERIFIED AGAINST ',A3,1X,A9,'  AT ',A6,
     3        '  FOR PERIOD  ',I2,'Z',
     4        I3,A3,I2,' - ',I2,'Z',I3,A3,I2,' .....',//,
     5        9X, 'FTHR     AREA',4X,10(3X,A5),5X,'NETPTS',/)
 410   FORMAT(9X,I3,' ....')
 420   FORMAT(10X,A6, 2X,F8.0,F8.1,5F8.2,3F8.1,5X,I5)
 425   FORMAT(10X,A6, 2X,F8.0,F8.2,5F8.2,3F8.2,5X,I5)
 430   FORMAT(/)
 440   FORMAT(/)
 500   FORMAT(10X,'NC,XM,AD,VAR,SD,A3,A4,ZX,ZN,R= ',I3,F8.2,
     1        3F8.3,2F8.4,3F8.2)
C
       CALL AFILL(X,JSTTX,NETX,JFCTX,.0)
       IF(MODEL  .GT.  2) then
          AREA(1) = AREA1
       ENDIF
       NGT = 0
C
C..................................................................
C      DO STATISTICAL ANALYSIS OF GRADIENT DISTRIBUTION
C..................................................................
C
       LOOP50: DO NG = 1,4
       LOOP10: DO IFT = 1,JFCTS
       LOOP12: DO IAR = 1,NET
       NC = 0
       LOOP14: DO I = IAR,KNET,NET
       IF(SGRAD(I,NG,IFT) .EQ. 0.0) CYCLE LOOP14
       NC = NC + 1
       W(NC) = SGRAD(I,NG,IFT)
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
      PRINT  500,NC,XM,AD,VAR,SD,A3,A4,ZX,ZN,R
       END DO LOOP12
       END DO LOOP10
C
C..................................................................
C      PRINT GRADIENT TABLES FOR EACH FORECAST HOUR
C..................................................................
C
       LOOP24: DO MX=1,1
       IF(IPDLY .NE. 1) CYCLE LOOP24 
C
       LOOP20: DO IFT = 1,JFCTS
       IF(.NOT. IVFCT(IFT,MODEL)) CYCLE LOOP20
       IFHR = IFTINC*IFT
C.....
       IF(MODEL .LE. 2) THEN
         PRINT 100,GFLD(NG),NAME,PLEVEL,NTRU,ITRUTH,IFHR,
     1             (AREA(I),I=1,NET)
       ELSE
         PRINT 200,GFLD(NG),NAME,PLEVEL,NTRU,ITRUTH,IFHR,
     1             (AREA(I),I=1,NET)
       ENDIF
C.....
C
       LOOP30: DO I = ICYB,ICYE,ICYI
       IF(IVT(1,I) .EQ. 0) CYCLE LOOP30
       K1 = NET*(I-1) + 1
       K2 = K1 + NET - 1
       PRINT 110,I,(IVT(J,I),J=1,4)
C.....
        IF(SGRAD(K1,NG,IFT) .LE. .0) THEN
         PRINT 120
         CYCLE LOOP30
        ELSE
C...
         IF(MODEL .LE. 2) THEN
          IF(NG .NE. 4) THEN
            PRINT 130,(SGRAD(K,NG,IFT),K=K1,K2)
          ELSE
            PRINT 135,(SGRAD(K,NG,IFT),K=K1,K2)
          ENDIF
         ELSE
          IF(NG .NE. 4) THEN
            PRINT 230,(SGRAD(K,NG,IFT),K=K1,K2)
          ELSE
            PRINT 235,(SGRAD(K,NG,IFT),K=K1,K2)
          ENDIF
         ENDIF
C...
       ENDIF
C.....
       END DO LOOP30
C.....
       IF(MODEL .LE. 2) THEN
         IF(NG .NE. 4) THEN
          PRINT 300,(X(1,J1,IFT),J1=1,NET),(X(2,J2,IFT),J2=1,NET),
     1            (X(5,J3,IFT),J3=1,NET)
         ELSE
          PRINT 305,(X(1,J1,IFT),J1=1,NET),(X(2,J2,IFT),J2=1,NET),
     1            (X(5,J3,IFT),J3=1,NET)
         ENDIF
       ELSE
         IF(NG .NE. 4) THEN
          PRINT 310,(X(1,J1,IFT),J1=1,NET),(X(2,J2,IFT),J2=1,NET),
     1            (X(5,J3,IFT),J3=1,NET)
         ELSE
          PRINT 315,(X(1,J1,IFT),J1=1,NET),(X(2,J2,IFT),J2=1,NET),
     1            (X(5,J3,IFT),J3=1,NET)
         ENDIF
       ENDIF
C.....
       PRINT 320,GFLD(NG),IFHR,NAME,PLEVEL,NTRU,ITRUTH
C
       END DO LOOP20
C
       END DO LOOP24
C
C..................................................................
C      PRINT STATISTICAL ANALYSIS TABLES FOR EACH FORECAST HOUR
C..................................................................
C
       MONB = NBEGIN
       MONE = NEND
       KC = 0
       KC5 = 1
       LOOP40: DO N = 1,JFCTS
       IF(X(1,1,N) .EQ. 0.) CYCLE LOOP40
       KC = KC + 1
C.....
       IF(KC  .EQ.  KC5) THEN
        PRINT 400,GFLD(NG),NAME,NTRU,ITRUTH,PLEVEL,NBEGIN,
     1            NBEGIN,MONTH(MONB),NBEGIN,NEND,NEND,
     2            MONTH(MONE),NEND,(JSTAT(I),I=1,JSTTX)
        KC5 = KC5 + 4
       ENDIF
C.....
       NFTHR = IFTINC * N
       PRINT 410,NFTHR
       LOOP42: DO K = 1,NET
        IF(NG .NE. 4) THEN
         PRINT 420,AREA(K),(X(I,K,N),I=1,JSTTX),NPTS(K)
        ELSE
         PRINT 425,AREA(K),(X(I,K,N),I=1,JSTTX),NPTS(K)
        ENDIF
       END DO LOOP42
       PRINT 430
C
       END DO LOOP40
       END DO LOOP50
C
       RETURN
       END

