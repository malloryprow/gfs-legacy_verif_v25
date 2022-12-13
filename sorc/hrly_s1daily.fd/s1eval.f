	SUBROUTINE S1EVAL(MODEL,LVL,NET,KNET,JNET,IGRDA,IGRDF,
     1                   S1A,S1B,SONE,SONEB,GRAD,SGRAD)
C SUBPROGRAM:    S1EVAL
C   PRGMMR: ROBERT HIRANO    ORG: W/NMC42    DATE: 88-12-14
C
C ABSTRACT: FOR EACH OF THE REQEUSTED MONTHLY CYCLES, RETRIEVES
C           THE VERIFYING FIELD (ANALYSIS OR INITIALIZED), THE
C           FORECASTS, AND CALLS SUBPROGRAM S1COMP TO CALCULATE
C           S1 SCORES FOR VARIOUS VERIFICATION AREAS.
C
C PROGRAM HISTORY LOG:
C   88-12-14  ROBERT HIRANO
C
C USAGE:    CALL S1EVAL(MODEL,LVL,NET,KNET,JNET,IGRDA,IGRDF,
C                       S1A,S1B,SONE,SONEB,GRAD,SGRAD)
C   INPUT ARGUMENT LIST:
C     MODEL    - MODEL ID (1=GFS,2=AVN,3=NGM,4=ETA)
C     LVL      - PRESSURE TO DO (=1,2,ETC, NON SPECIFIC)
C     NET      - NUMBER OF VERIFICATION NETWORKS TO DO
C     KNET     - MAX CYCLES TIMES NET (1*NET)
C     JNET     - TOTAL NUMBER OF NETWORKS TO DO
C     IGRDA    - ON84 GRID TYPE OF VERIFICATION FIELD
C     IGRDF    - ON84 GRID TYPE OF FORECAST FIELD
C
C   OUTPUT ARGUMENT LIST:
C     GRAD     - ERROR,MAX,RATIOS X/A AND F/A GRADIENTS (=KNET)
C     SGRAD    - ERROR,MAX,RATIOS X/A AND F/A GRADIENTS (=NET)
C     SONE     - S1 SCORES FOR ENTIRE PERIOD (=KNET)
C     SONEB    - ALTERNATIVE S1 SCORE FOR ENTIRE PERIOD (=KNET)
C     S1A      - S1 SCORES FOR A FORECAST HOUR (=NET)
C     S1B      - ALTERNATE S1 SCORE (W.R.T. TRUE GRAD)
C
c  Variables Used:
c
c    idate
c    ivfdte    - verifying date = idate(icyl)
c    ivfld     - in common YDATA, verifying field, read in from ft13
c    icyb      - in common YDATA, cycle begin, read in from ft13 (=1)
c    icye      - in common YDATA, cycle end, read in from ft13 (=8)
c    icyi      - in common YDATA, cycle interval, read in from ft13 (=1)
c    label(l)  - declared integer, =ids(l,lvl), ids read in from ft12
c                grib pds info
c    labelone  - ids(1,lvl), pds(5) (=2 for msl, =7 for hgt)
c    iftinc    - in common YDATA, forecast increment, read in from ft13 (=12)
c    jfcts     - in common YDATA, max number of forecast times, from ft13 (=10)
c    ivfct     - in common XDATA, logical*1(mxfct,mxmdl), read from ft13
c    labelf    - labels for forecast fields, declared integer
c                labelf(1)=?
c                labelf(5)= forecast grid id
c                labelf(7)= forecast verifying date
c    labelarc  - declared integer
c
C   SUBPROGRAM CALLED:
C     UNIQUE:   - AFILL, S1COMP, S1GETF
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN-77
C   MACHINE:  NAS
C
C$$$
C
C
       PARAMETER    (MXCYL=1,MXLVL=6,MXMDL=4,MXFCT=10,MXHR=10)
       CHARACTER(10)  ITRUTH
       CHARACTER(6)  PLVL
       REAL     SONE(KNET,JFCTS),SONEB(KNET,JFCTS),
     1            S1A(NET),S1B(NET)
       REAL     GRAD(KNET,4,JFCTS),SGRAD(4,NET)
       REAL     HEMANL(145,37),HEMFCT(145,37),
     1            FMANL(53,45),FMFCT(53,45),
     2            fmanl2(38,17),fmfct2(38,17),
     3            hemanl2(65,65),hemfct2(65,65)
       INTEGER  LABEL(12),LABELF(12),LBLARC(12)
       INTEGER*4   ipad
       LOGICAL(1) IVFCT,IVLVL
       DATA  NTMAX /3/
C
       COMMON /XDATA/IDATE(MXCYL),IVFCT(MXFCT,MXHR),IVLVL(MXLVL)
       COMMON /WDATA/IVT(4,MXCYL),IDS(12,MXLVL),PLVL(MXLVL)
       COMMON /YDATA/ NBEGIN,NEND,ICYB,ICYE,ICYI,IVFLD,IFTINC,
     1         JLVLS,JFCTS,MDANL,ITRUTH
       common /cdata/ ivfcn
C
 200    FORMAT(' ERROR NUMBER ',I3,' OCCURRED FOR MDANL= ',I2,
     1         ' ...IANL= ',I2,' ...IVER= ',I2,' *************',/,
     2         5X,'LABEL= ', 8Z9,' ... DO NEXT CYCLE ...')
 210    FORMAT(' ERROR NUMBER ',I3,' OCCURRED FOR MODEL= ',I2,
     1         ' ...IANL= ',I2,' ...IVER= ',I2,' *************',/,
     2         5X,'LABEL= ', 8i9,' ... DO NEXT FCST HR ...')
 300   FORMAT(/)
 400   FORMAT(6i8,1x, i10, i4)
 410   FORMAT(9f7.2)
 411   FORMAT(14f7.2)
C
       CALL AFILL(SONE,KNET,JFCTS,1,.0)
       CALL AFILL(SONEB,KNET,JFCTS,1,.0)
       CALL AFILL(GRAD,KNET,4,JFCTS,.0)
       CALL AFILL(SGRAD,4,NET,1,.0)
       CALL IFILL(LABEL,12,1,1,0)
       CALL IFILL(LABELF,12,1,1,0)
       CALL IFILL(LBLARC,12,1,1,0)
C
C..................................................................
C...   FOR EACH REQUESTED CYCLE PERIOD..........
C...     1. SPECIFY LABEL VARIABLES AND CALL SUBROUTINE S1GETF TO
C...        RETRIEVE VERIFYING FIELD.  RESET LABEL, MAY HAVE BEEN
C...        ALTERED BY 36DAY GETARC SUBROUTINE.
C..................................................................
C.....
       PRINT 300
       NTIMES = 0
       LOOP10: DO ICYL = ICYB,ICYE,ICYI
       IVFDTE = IDATE(ICYL)
       IF(IVFDTE  .EQ.  0) CYCLE LOOP10
       NTIMES = NTIMES + 1
C
       IF(IVFLD  .EQ.  1) THEN
        IANL = 1
        IVER = 0
       ELSE
        IANL = 0
        IVER = 1
       ENDIF
C
       LOOP12: DO L = 1,12
       LABEL(L) = IDS(L,LVL)
       END DO LOOP12
       LABEL(7) = IVFDTE
       LABEL(5) = IGRDA
       LBLONE = IDS(1,LVL)
C.....
C       PRINT 100, MDANL, IVFLD,IANL,IVER,(LABEL(L),L=1,8)
C.....
       if ((ianl .eq. 1) .and. (iver .eq. 0)) then
         ifhr=0
       endif
       CALL S1GETF(MDANL,LABEL,LBLONE,IANL,IVER,ifhr,FMANL,fmanl2,
     1             HEMANL,hemanl2,IERR)
C.....
       IF(IERR  .NE. 0) THEN
        PRINT 200,IERR,MDANL,IANL,IVER,(LABEL(L),L=1,8)
        CYCLE LOOP10
       ENDIF
C.....
       LOOP14: DO L = 1,12
       LABEL(L) = IDS(L,LVL)
       END DO LOOP14
       LABEL(5) = IGRDA
       LABEL(7) = IVFDTE
C
C..................................................................
C...     2. SPECIFY LABEL VARIABLES AND CALL SUBROUTINE S1GETF TO
C...        RETRIEVE VERIFYING FORECAST HOUR.  NOTE THAT THE
C...        NGM AND LFM FORECAST FIELDS ARE RETURNED IN THE
C...        SAME DIMENSIONS AS THEIR ANALYSIS FIELD.
C..................................................................
C
       IVER = 1
       IANL = 0
       IF(IFTINC  .EQ.  0) THEN
        IF(MODEL  .NE.  MDANL) THEN
         IF(IVFLD  .EQ.  1) THEN
          IANL = 1
          IVER = 0
         ENDIF
        ENDIF
       ENDIF
C.....
C
       LOOP20: DO IFHR = 1,JFCTS
C
C      IF ( IFHR .NE. 2 ) CYCLE LOOP20
C
ckum        print*,'In s1eval  ivfct values  ',ivfct(ifhr,model)
        IF(.NOT. IVFCT(IFHR,MODEL)) CYCLE LOOP20
        JFT = IFTINC * IFHR
Ckum        print*,'In s1eval forecast hour jft  =  ',JFT
C
       LOOP22: DO L = 1,12
        LABELF(L) = IDS(L,LVL)
       END DO LOOP22
        LABELF(5) = IGRDF
        LABELF(7) = IVFDTE
       LOOP23: DO L = 1,12
        LBLARC(L) = LABELF(L)
       END DO LOOP23
C.....
c        IF(NTIMES  .LE.  NTMAX) THEN
c         PRINT  110,LABEL(1),LABELF(1),LABELF(5),LABELF(7),LABELF(8)
c        ENDIF
C.....
       CALL S1GETF(MODEL,LABELF,LBLONE,IANL,IVER,ifhr,FMFCT,fmfct2,
     1             HEMFCT,hemfct2,IERR)
       DO J = 1,45
       DO I = 1,53
       END DO
       END DO
C.....
       IF(IERR  .NE. 0) THEN
        PRINT 210,IERR,MODEL,IANL,IVER,(LABELF(L),L=1,8)
        CYCLE LOOP20
       ENDIF
C.....
       LOOP24: DO L = 1,12
        LABELF(L) = IDS(L,LVL)
       END DO LOOP24
       LABELF(5) = IGRDF
       LABELF(7) = IVFDTE
C
C..................................................................
C...     3. CALCULATE S1 SCORES FOR VERIFICATION NETWORKS.
C           STACK NETWORK SCORES BY CYCLE PERIODS.
C..................................................................
C.....
       IF(MODEL  .LE.  2) THEN
        CALL S1COMP(MODEL,HEMANL,HEMFCT,hemanl2,hemfct2,145,37,65,65,
     1              NET,JNET,S1A,S1B,SGRAD)
       ELSE
        CALL S1COMP(MODEL,FMANL,FMFCT,fmanl2,fmfct2,53,45,38,17,
     1              NET,JNET,S1A,S1B,SGRAD)
       ENDIF
C.....
       LOOP26: DO I = 1,NET
        LNET = NET*(ICYL-1) + I
        SONE(LNET,IFHR) = S1A(I)
        SONEB(LNET,IFHR) = S1B(I)
        GRAD(LNET,1,IFHR) = SGRAD(1,I)
        GRAD(LNET,2,IFHR) = SGRAD(2,I)
        GRAD(LNET,3,IFHR) = SGRAD(3,I)
        GRAD(LNET,4,IFHR) = SGRAD(4,I)
       END DO LOOP26
C
C..... COPY FOUR DAY VERIFICATION TO DISK .....
       IF (IVFCN  .EQ.  4) THEN
         WRITE(60,400)  (LBLARC(L),L=1,7),JFT
         if (MODEL .GE. 3) THEN
           WRITE(60,410) (S1A(MA),MA=1,NET)
           write(60,410) (S1B(MB),MB=1,NET)
         else
           WRITE(60,411) (S1A(MA),MA=1,NET)
           write(60,411) (S1B(MB),MB=1,NET)
         endif
       ENDIF

C..... END OF FORECAST LOOP .....
       END DO LOOP20
C
C..... END OF CYCLE LOOP .....
       END DO LOOP10
C
        RETURN
        END

