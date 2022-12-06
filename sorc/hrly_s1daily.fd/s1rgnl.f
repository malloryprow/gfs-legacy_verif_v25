       SUBROUTINE S1RGNL(MODEL,NET,KNET,JNET,NAME,NTRU,NPTS,
     1                   IVPRT,IDLY,ISAVE)
C SUBPROGRAM:    S1RGNL
C   PRGMMR: ROBERT HIRANO    ORG: W/NMC42    DATE: 88-12-14
C
C ABSTRACT: SET UP REGIONAL MODEL MONTHLY S1 SCORE EVALUATION
C           USING D.D. 36-DAY ARCHIVE DISK FILES.
C
C PROGRAM HISTORY LOG:
C   88-12-14  ROBERT HIRANO
C
C USAGE:    CALL S1RGNL(MODEL,NET,KNET,JNET,NAME,NTRU,NPTS,
C                       IVPRT,IDLY,ISAVE)
C   INPUT ARGUMENT LIST:
C     MODEL    - REGIONAL MODEL ID (3=NGM,4=ETA)
C     NET      - TOTAL NUMBER OF VERIFICATION NETWORKS TO DO
C     KNET     - MAX CYCLES TIMES NET (1*NET)
C     JNET     - NUMBER OF LATLON NETWORKS IN LOOP
C     NAME     - MODEL NAME
C     NTRU     - VERIFYING FIELD NAME
C     NPTS     - NUMBER OF POINTS IN VERIFICATION NETWORKS
C     IVPRT    - PRINT OPTION (1=S1(MXGRD),2=S1(TRUGRD),3=GRAD)
C     IDLY     - OPTION TO PRINT DAILY SCORES (BY LEVELS)
C     ISAVE    - OPTION TO COPY DATA TO TAPE (1=YES,0=NO)
C
C   SUBPROGRAM CALLED:
C     UNIQUE:   - S1EVAL, S1SCRS, S1GRAD
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN-90
C   MACHINE:  IBM RS6000
C
C$$$
C
C Change JFCTX from 4 to 7 to increase the forecast hour.
C
       PARAMETER   (MXCYL=1, NETX=09, KNETX=1*NETX, MXHR=10,
     2              MXFCT=10, MXLVL=6,
     1              JFCTX=7, JLVLX=6, MXNET=14, JMSL=1, J500=4)
C
       CHARACTER(10) ITRUTH
       CHARACTER(6) PLEVEL,PLVL
       CHARACTER(3) NAME,NTRU
       REAL     SONE(KNETX,JFCTX),SONEB(KNETX,JFCTX),
     1            S1FIN1(KNETX,JFCTX),S1FIN2(KNETX,JFCTX),
     2            S1A(NETX),S1B(NETX),
     3            GRAD(KNETX,4,JFCTX),SGRAD(4,NETX)
       INTEGER  NPTS(MXNET)
       INTEGER*4    ipad
       LOGICAL(1)  IVFCT,IVLVL,IVPRT(3),IDLY(JLVLX)
C
       COMMON /XDATA/ IDATE(MXCYL),IVFCT(MXFCT,MXHR),IVLVL(MXLVL)
       COMMON /WDATA/ IVT(4,MXCYL),IDS(12,MXLVL),PLVL(MXLVL)
       COMMON /YDATA/ NBEGIN,NEND,ICYB,ICYE,ICYI,IVFLD,IFTINC,
     1                JLVLS,JFCTS,MDANL,ITRUTH
       common /cdata/ ivfcn
        data igrda /6/
        data igrdf /6/
C
C
 100   FORMAT(5X,'..... ',A3,' (M=',I1,') VERIFIED AGNST ',A3,1X,
     1        A9,' (IVFLD=',I1,') ... ',I2,' LEVELS, ',I3,' FCSTS ',
     2        '... CYCLE BEGIN,END,INCREM= ',3I4,' .....',//,
     3        15X,'..... FCST HOUR INCREM= ',I3,' ***** ISAVE= ',
     4        I2,' ...COPY TO TAPE? (0=NO,1=YES) *****',//,
     5        15X,'..... IGRDA= ',i9,' ..... IGRDF= ',i9,' .....',//)
 150   FORMAT(//,10X,'***** ERROR ***** ERROR ***** ERROR *****',
     1        //,25X,'...WRITING S1 VERIFICATION TO TAPE...',//,
     2        30X,A3,' (MODEL= ',I1,') VERIFIED AGAINST ',A3,1X,
     3        A9,' (IVFLD= ',I1,')',//,35X,'PRESSURE LVL= ',
     4        A6,' (LVL= ',I1,')',//,20X,'**********************')
 200   FORMAT(////,'********** COPIED ',A3,1X,A6,' AND ',A6,2X,
     1        'VERIFICATION DATA TO ARCHIVE TAPE **********',///)
C
C...................................................................
C   CHECK...IS THIS A COMPARISON OF NGM AND LFM ANALYSES OR
C           INITIALIZED FIELDS?  IF SO, SET CORRECT GRID MAPS.
C...................................................................
C
       IF(IVFLD  .EQ.  1) THEN
        IF(IFTINC  .EQ.  0) THEN
         IF(MODEL  .NE.  MDANL) IGRDF = IGRDA
        ENDIF
       ELSE
         IGRDA = IGRDF
       ENDIF
C
C
       PRINT 100,NAME,MODEL,NTRU,ITRUTH,IVFLD,JLVLS,JFCTS,
     1           ICYB,ICYE,ICYI,IFTINC,ISAVE,IGRDA,IGRDF
C
C...................................................................
C   DO VERIFICATION, SUMMARY TABLES, AND/OR SAVE
C...................................................................
C....
       LOOP10: DO LVL = 1,JLVLS
       LOOP30: DO MM=1,1
C
C  .....DO VERIFICATION.....
C......
        IF(.NOT. IVLVL(LVL)) CYCLE LOOP10
        CALL S1EVAL(MODEL,LVL,NET,KNET,JNET,IGRDA,IGRDF,
     1              S1A,S1B,SONE,SONEB,GRAD,SGRAD)
C
C  .....S1 SCORE (ERROR/MAX).....
C......
       PLEVEL = PLVL(LVL)
       IF(.NOT. IVPRT(1)) CYCLE LOOP30
       IPDLY = 1
       IF(.NOT. IDLY(LVL)) IPDLY = 0
       CALL S1SCRS(MODEL,NAME,NTRU,PLEVEL,KNET,NET,IVT,IVFCT,NPTS,
     1            IPDLY,SONE,1)
C
C  .....SAVE VERIFICATION DATA.....
C......
       IF(ISAVE  .NE.  1) CYCLE LOOP30
       IF(LVL  .EQ.  JMSL) THEN
        LVLX = 1
       ELSE
        IF(LVL  .EQ. J500) THEN
         LVLX = 2
        ELSE
         EXIT LOOP30
        ENDIF
       ENDIF
         LOOP22: DO J = 1,JFCTX
          LOOP24: DO K = 1,KNET
          IF(LVLX  .EQ.  1) THEN
           S1FIN1(K,J) = SONE(K,J)
          ELSE
           S1FIN2(K,J) = SONE(K,J)
          ENDIF
          END DO LOOP24
       END DO LOOP22
C
C...................................................................
C   DO ALTERNATE S1 SCORE VERIFICATION (S1=ERROR/TRU)
C...................................................................
C......
       END DO LOOP30
       LOOP32: DO MZ=1,1
       IF(.NOT. IVPRT(2)) CYCLE LOOP32
       IPDLY = 1
       IF(.NOT. IDLY(LVL)) IPDLY = 0
       CALL S1SCRS(MODEL,NAME,NTRU,PLEVEL,KNET,NET,IVT,IVFCT,NPTS,
     1            IPDLY,SONEB,2)
C
C...................................................................
C   DO ANALYSES OF GRADIENTS
C...................................................................
C......
       END DO LOOP32
       IF(.NOT. IVPRT(3)) CYCLE LOOP10
       IPDLY = 1
       IF(.NOT. IDLY(LVL)) IPDLY = 0
       CALL  S1GRAD(MODEL,NAME,NTRU,PLEVEL,KNET,NET,IVT,IVFCT,
     1              NPTS,IPDLY,GRAD)
C
       END DO LOOP10
C
C...................................................................
C   COPY TO TAPE IF DESIRED
C...................................................................
C......
       LOOP12: DO MM=1,1
       IF(ISAVE  .NE.  1) EXIT LOOP12
       WRITE(20,ERR=90) NBEGIN,NEND,NAME,MODEL,NTRU,ITRUTH,
     1          MDANL,IVFLD,KNET,NET,NPTS,ICYB,ICYE,ICYI,
     2          IFTINC,JFCTX,IVT
        WRITE(20,ERR=90) PLVL(JMSL),S1FIN1,PLVL(J500),S1FIN2
       PRINT 200,NAME,PLVL(JMSL),PLVL(J500)
       EXIT LOOP12
C
  90   PRINT 150, NAME,MODEL,NTRU,ITRUTH,PLEVEL,LVL
       END DO LOOP12
C
       RETURN
       END
c

