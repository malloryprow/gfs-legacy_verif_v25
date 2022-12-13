C ABSTRACT: C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C
C MAIN PROGRAM: CQCSUM96
C   PRGMMR: LILLY            ORG: NP12        DATE: 2009-06-01
C
C ABSTRACT: SUMMING OF COUNTS WILL BE DONE AS THE DATA IS READ
C
C PROGRAM HISTORY LOG:
C
C 1991-12-24  W. Collins -- ORIGINAL AUTHOR
C 2009-06-01 Steve Lilly -- add tmplate for documentation block
C
C USAGE:
C   INPUT FILES:
C
C   OUTPUT FILES:
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
C   MACHINE : P6
C
C$$$

      PROGRAM CQCSUM96

      PARAMETER (ITYPS=116,NPLVL=21,NBLOCK=119,NSTNS=1000,
     &  ITPP=ITYPS+3,ITP7=ITYPS+7,NTIMES=1460)
C
C     SOME DEFINITIONS:
C       -----------------------------------------------------------
C           --- FOR ERRORS ---
C       NS        - NUMBER OF UNIQUE STATIONS WITH ERRORS
C       NE        - TOTAL COUNT OF ERRORS
C       NES       - TOTAL COUNT OF ERRORS FOR SHIPS
C       ND        - COUNT OF DATE/TIMES
C       N(IT,IP)  - COUNT OF ERRORS BY TYPE AND PRESSURE
C       NC(IT,IP) - COUNT OF CORRECTIONS BY TYPE AND PRESSURE
C       NDC(,,)   - COUNT OF ERRORS BY DECISION, BLOCK, LEVEL
C       NDCB(,)   - COUNT OF ERRORS BY DECISION, BLOCK
C       NDCP(,)   - COUNT OF ERRORS BY DECISION, LEVEL
C       NDEC()    - COUNT OF ERRORS BY DECISION
C       NRO(,)    - COUNT OF REPORTS WITH OBSERVATION ERRORS
C       NDT(,)    - COUNT OF ERRORS BY TYPE, DECISION
C       ID(IS,1)  - LIST OF STNS WITH ERRORS BY STATION ID-S
C       ID(IS,2)  - ERROR COUNT FOR EACH STATION, ALL TYPES
C       ID(IS,3-ITP)- COUNT FOR EACH STATION OF ERRORS 1-ITYPS
C       ID(IS,ITPP)- COUNT FOR EACH STATION OF OBSERVATION ERRORS
C       IDS(IS,1) - LIST OF SHIPS WITH ERRORS BY STATION ID-S
C       IDS(IS,2) - ERROR COUNT FOR SHIPS, ALL TYPES
C       IDS(IS,3-ITP)- COUNT FOR EACH SHIP OF ERRORS 1-ITYPS
C       NB()      - COUNT OF ERRORS FOR STATIONS BY BLOCK NO.
C       NBL(IT,)  - COUNT OF ERRORS FOR STNS BY TYPE AND BLOCK NO.
C       NEB()     - COUNT OF ERROR-CONTAINING STNS BY BLOCK
C       NN()      - COUNT OF ERRORS BY ERROR TYPE
C       NNC()     - COUNT OF CORRECTIONS BY ERROR TYPE
C       NP()      - COUNT OF ERRORS BY PRESSURE
C       NCP()     - COUNT OF CORRECTIONS BY PRESSURE
C       NCONF()   - NUMBER OF CONFIDENT (1&2) CORRECTIONS BY BLOCK
C       PCONF()   - PERCENT OF CONFIDENT (1&2) CORRECTIONS BY BLOCK
C       -----------------------------------------------------------
C           --- FOR TOTAL REPORTING ---
C       NTOTB(,1) - TOTAL COUNT REPORTING BY BLOCK
C       NTOTB(,2) -          AT 00Z
C       NTOTB(,3) -          AT 12Z
C       NTOTL(1)  - TOTAL COUNT OF REPORTING LAND STATIONS
C       NTOTL(2)  -          AT 00Z
C       NTOTL(3)  -          AT 06Z
C       NTOTL(4)  -          AT 12Z
C       NTOTL(5)  -          AT 18Z
C       NTOTS(1)  - TOTAL COUNT OF REPORTING SHIPS
C       NTOTS(2)  -          AT 00Z
C       NTOTS(3)  -          AT 12Z
C       -----------------------------------------------------------
C
C     SUMMING OF COUNTS WILL BE DONE AS THE DATA IS READ
C
C     W. COLLINS  --  CHANGES MADE FOR USE BY CQCHT96
C
      REAL PBL(NBLOCK), PBEL(ITYPS,NBLOCK), PEL(ITYPS),
     &  AL(5), AS(5), AB(NBLOCK,5), AEL(ITYPS),
     &  PSB(NBLOCK), PRB(NBLOCK), PCONF(NBLOCK),
     &  VMSG(2), SBAS(NSTNS), SPMSLI(NSTNS), SPMSLR(NSTNS),
     &  SBS(NSTNS), SPIS(NSTNS), SPHS(NSTNS), VINC(21,4)
      INTEGER N(ITYPS,NPLVL), NB(NBLOCK), ID(NSTNS,ITP7),
     &  IPLVL(NPLVL), NN(ITYPS), NP(NPLVL), NBL(ITYPS,NBLOCK),
     &  IDATES(NTIMES), IDS(100,ITPP), NTB(NBLOCK,NTIMES),
     &  NTOTB(NBLOCK,5), NTOTL(5), NTOTS(5),
     &  IDENT, NTD(5), NEB(NBLOCK), NCONF(NBLOCK),
     &  ITYP(20), IBL(100), NTYP(ITYPS), IPR(ITYPS),
     &  IDT, NDC(0:5,NBLOCK,NPLVL), NDCB(0:5,NBLOCK),
     &  NDCP(0:5,NPLVL), NDEC(0:5), NRO(NBLOCK), IDO(NSTNS,NTIMES),
     &  NTL(NTIMES), NTS(NTIMES), NC(ITYPS,NPLVL), NNC(ITYPS),
     &  NCP(NPLVL), IBIN(1500,3), NDT(0:ITYPS,0:5),
     &  ISR(NSTNS,8), ISRS(100,8), ICAT(5), IKY(20)
      CHARACTER*1 CIDX(5), CNUM(11), BUF(132), STAR, NINE,
     &  SID1(8), CTIME(100,62), CTIMES(100,62), BLANK1
      CHARACTER*2 CY, CDEC, CLEV, CVAR
      CHARACTER*4 CNTB(25), PART
      CHARACTER*5 CNTS, IDX
      CHARACTER*6 CNTL, CNVAL, BLANK6,
     &  CVALZ, CVALT
      CHARACTER*8 BLANK, NINES, CDS(100), BLOCK, IDSTN,
     &  CS1X, CS2X, CZCORX, CSC1X, CSC2X, IDX8,
     &  SID, CSRS(100), CDT
      CHARACTER*10 CII, CDTE, CDATE, CDATES(NTIMES), DTENEW
      COMMON /OBLK/ NBLK(18,20), NNBLK(18)
      EQUIVALENCE (CNTL,BUF(10)), (CNTS,BUF(16)), (CNTB(1),BUF(21)),
     &  (DTENEW,BUF(122)), (CY,BUF(130)), (IDX8,BUF(2))
      EQUIVALENCE (CIDX,IDX)
      EQUIVALENCE (SID,SID1), (ISRS,CSRS)
      DATA ISTRT /0/, IBNN /0/
      DATA IPLVL/1000,925,850,700,500,400,300,250,200,
     &  150,100,70,50,30,20,10,7,5,3,2,1/
      DATA NINES /'99999999'/, BLANK /'        '/, BLANK6 /'      '/
      DATA BLANK1 /' '/
      DATA STAR /'*'/, NINE /'9'/
      DATA BLOCK /'BLOCK TO'/
      DATA CNUM /' ','0','1','2','3','4','5','6','7','8','9'/
      DATA NOBLK /18/
      DATA IPR /1,2,3,5,6,7,8,9,10,20,21,30,40,49,50,100,102,105,
     &  106,97*0/
      DATA VMSG /99999.,9999.9/
 
C     INITIALIZE
 
      ISHIP = 0
      LAND  = 0
      ISR   = 0
      CDATE = BLANK1
      DO J=1,NTIMES
        CDATES(J) = BLANK1
      ENDDO
      DO 1 I=1,20
        ITYP(I) = 0
    1 CONTINUE
      DO 2 I=1,100
        IBL(I) = 0
    2 CONTINUE
 
C     READ TYPES TO PRINT IN DETAIL (< 0. FOR ALL).
 
      DO I=1,19
         READ(55,584,END=2220) IKY(I)
         IF(IKY(3).EQ.46)IKEY=2   ! TAIWAN
         IF(IKY(4).EQ.58)IKEY=2   ! TAIWAN, CHINA
         IF(IKY(5).EQ.59)IKEY=2   ! TAIWAN, CHINA
         IF(IKY(3).EQ.68)IKEY=2   ! SOUTH AFRICA
         IF(IKY(3).EQ.70)IKEY=2   ! ALASKA
         IF(IKY(3).EQ.71)IKEY=2   ! CANADA
         IF(IKY(4).EQ.72)IKEY=2   ! U.S.
         IF(IKY(5).EQ.74)IKEY=2   ! U.S.
         IF(IKY(3).EQ.76)IKEY=2   ! MEXICO
         IF(IKY(3).EQ.100)IKEY=1  ! ALL
      ENDDO
 584  FORMAT(I5)
 2220 CONTINUE

      REWIND(55)

      DO 3 I=1,20
        READ(55,587) ITYP(I)
        IF(IKEY.EQ.2)THEN
        WRITE(6,587) ITYP(I)
        ENDIF

        IF(ITYP(I).EQ.0) GO TO 4
    3 CONTINUE
  587 FORMAT(I5)
    4 CONTINUE

C     READ BLOCK NUMBERS TO PRINT IN DETAIL (= 100 FOR ALL).

      DO 6 I=1,100
        READ(55,588,END=7) IBL(I)

        IF(IKEY.EQ.2)THEN
        WRITE(6,588) IBL(I)
        ENDIF

    6 CONTINUE
  588 FORMAT(I5)
    7 CONTINUE

C     BEGIN INITIALIZE THE COUNTERS

      NS  = 0
      NSS = 0
      NE  = 0
      NES = 0
      ND  = 0
      DO 224 I=1,NTIMES
        NTL(I) = 0
        NTS(I) = 0
  224 CONTINUE
      DO 5 I=1,5
        NTD(I) = 0
        NTOTL(I) = 0
        NTOTS(I) = 0
    5 CONTINUE
      DO 59 I=0,5
        NDEC(I) = 0
        DO 57 J=1,NBLOCK
          NDCB(I,J) = 0
          DO 56 K=1,NPLVL
            NDC(I,J,K) = 0
   56     CONTINUE
   57   CONTINUE
        DO 58 K=1,NPLVL
          NDCP(I,K) = 0
   58   CONTINUE
   59 CONTINUE
      DO 20 IP=1,NPLVL
        NP(IP) = 0
        NCP(IP) = 0
        DO 10 I=1,ITYPS
          N(I,IP) = 0
          NC(I,IP) = 0
   10   CONTINUE
   20 CONTINUE
      DO 30 IB=1,NBLOCK
        NEB(IB) = 0
        NB(IB) = 0
        NCONF(IB) = 0
        PCONF(IB) = 0.
        DO 225 I=1,NTIMES
          NTB(IB,I) = 0
  225   CONTINUE
        DO 22 I=1,5
          NTOTB(IB,I) = 0
   22   CONTINUE
        DO 25 I=1,ITYPS
          NBL(I,IB) = 0
   25   CONTINUE
   30 CONTINUE
      DO 36 J=1,ITPP
        DO 35 IS=1,NSTNS
          ID(IS,J) = 0
   35   CONTINUE
   36 CONTINUE
      DO 40 IS=1,100
        CDS(IS) = BLANK
        DO 38 J=1,ITPP
          IDS(IS,J) = 0
   38   CONTINUE
   40 CONTINUE
      DO 45 I=1,ITYPS
        NN(I) = 0
        NNC(I) = 0
   45 CONTINUE
      DO 48 IS=1,NSTNS
        SBAS(IS) = 0.
        SPMSLI(IS) = 0.
        SPMSLR(IS) = 0.
        SBS(IS) = 0.
        SPIS(IS) = 0.
        SPHS(IS) = 0.
   48 CONTINUE
      DO 51 I=1,NBLOCK
        NRO(I) = 0
   51 CONTINUE
      DO 52 I=1,NSTNS
        DO 49 J=1,NTIMES
          IDO(I,J) = 0
   49   CONTINUE
   52 CONTINUE
      DO 195 J=1,3
        DO 190 I=1,500
          IBIN(I,J) = 0
  190   CONTINUE
  195 CONTINUE
      DO J=1,5
        DO I=0,ITYPS
          NDT(I,J) = 0
        ENDDO
      ENDDO

C     END INITIALIZE THE COUNTERS


C     READ CARDS DEFINING COMBINED REGIONS.

      CALL REGUNS

C     READ ALL BLOCK TOTALS INFORMATION.
C     STORE IN NTL, NTS AND NTB.
C     THIS SECTION FOR TOTAL NUMBER OF STATIONS CHECKED.
C     CONVERT 1ST OF 4 RECORDS USING INTERNAL READS.

      JD = 0
  608 CONTINUE
      JD = JD + 1
      IF(JD.GT.NTIMES) GO TO 200

C     END OF FILE OR BLANK LINE SIGNALS END OF TOTAL FILE.

      READ(15,602,END=1615) BUF
  602 FORMAT(132A1)
      IF(CNTL.EQ.BLANK6) GO TO 1615

      READ(UNIT=CNTL,FMT=610) NTL(JD)
  610 FORMAT(I6)
      READ(UNIT=CNTS,FMT=611) NTS(JD)
  611 FORMAT(I5)
      DO 301 I=1,25
        READ(UNIT=CNTB(I),FMT=6111) NTB(I,JD)
 6111 FORMAT(I4)
  301 CONTINUE
      READ(UNIT=DTENEW,FMT=612) IDATES(JD)
  612 FORMAT(I10)
      CDATES(JD) = DTENEW
      ND = ND + 1
 
C     NOW READ NEXT THREE RECORDS.
 
      READ(15,705,END=1615) IDX8, (NTB(I,JD),I=26,50),DTENEW
      READ(15,705,END=1615) IDX8, (NTB(I,JD),I=51,75),DTENEW
      READ(15,706,END=1615) IDX8, (NTB(I,JD),I=76,99),DTENEW
  705 FORMAT(1X,A8,11X,  25I4,1X,A10)
  706 FORMAT(1X,A8,11X,  24I4,5X,A10)
 
C     DETERMINE OBSERVATION TIME FOR BLOCK TOTALS.
 
      READ(UNIT=CY,FMT=613) ICY
  613 FORMAT(I2)
  
C     DONT COUNT MISSING DAYS
  
      IF(ICY.GT.24) GO TO 608 
  
      ICY = 2 + (ICY+1)/6
  557 FORMAT(' ICY:',I8,' DATE:',I10)

C     ADD TO TOTALS.
C       NTOTL: TOTAL LAND STATIONS REPORTING
C       NTOTS: TOTAL SHIPS REPORTING

      NTOTL(1) = NTOTL(1) + NTL(JD)
      NTOTS(1) = NTOTS(1) + NTS(JD)
      NTOTL(ICY) = NTOTL(ICY) + NTL(JD)
      NTOTS(ICY) = NTOTS(ICY) + NTS(JD)

C     COUNT DATES.

      NTD(1) = NTD(1) + 1
      NTD(ICY) = NTD(ICY) + 1

C     NTOTB:  TOTAL COUNT BY BLOCK

      DO 305 I=1,NBLOCK
        NTOTB(I,1) = NTOTB(I,1) + NTB(I,JD)
        NTOTB(I,ICY) = NTOTB(I,ICY) + NTB(I,JD)

C       INCREMENT COUNT FOR APPROPRIATE COMBINED REGION(S).

        DO 304 II=1,NOBLK
          NNN = NNBLK(II)
          DO 303 J=1,NNN
            IF(NBLK(II,J).EQ.I) THEN
              NTOTB(II+99,1) = NTOTB(II+99,1) + NTB(I,JD)
              NTOTB(II+99,ICY) = NTOTB(II+99,ICY) + NTB(I,JD)
            ENDIF
  303     CONTINUE
  304   CONTINUE
  305 CONTINUE
      GO TO 608
 1615 CONTINUE
      NDATES = JD - 1

C     READ THE DATA.

      ICNT = 0
  100 CONTINUE

      READ(12,601,END=200) IDSTN,CDTE,SEQN,ISCAN,LEVL,PRESS,
     &  LTYP,LSTYP,CVAR,OVAL,COR,CORVAL,PCOR,QM,IHSC,BAS,PMSLI,PMSLR
  601 FORMAT(1X,A8,1X,A10,F6.0,I5,I6,F10.1,2I7,2X,A2,4F10.1,F6.0,I6,
     &  3(1X,F7.1))   ! Change to this after next CQCHT implementation
      CDATE = CDTE
      READ(UNIT=CDATE,FMT=612) IDT
      IF(IDX.EQ.BLANK) GO TO 200
 
C     CHECK TO SEE IF STATION IS SHIP.
 
      IDX = IDSTN
      NCHAR = 0
      DO 62 I=1,5
        DO 61 J=1,11
          IF(CIDX(I).EQ.CNUM(J)) THEN
  585       FORMAT(' CNUM,NCHAR: ',A1,2X,I3)
            GO TO 62
          ENDIF
   61   CONTINUE
        NCHAR = NCHAR + 1
   62 CONTINUE
 
 
  586 FORMAT(' NCHAR,IDX: ',I3,2X,A8)
      IF(NCHAR.EQ.0) THEN
        READ(UNIT=IDSTN,FMT=555) IDENT
  555   FORMAT(I5)
  556   FORMAT(' IDENT: ',I5)
        IB = IDENT/1000
      ENDIF
      IVAR = IV(CVAR)
  713 FORMAT(' IVAR: ',I2)
      IT = ITYPE(IHSC,IVAR)
  723 FORMAT(' ERROR TYPE: ',I4)
      LEV = NMANLV(PRESS)
      IF(LEV.LE.0) GO TO 100
  711 FORMAT(' LEV: ',I2)
  701 FORMAT(I2)
      IDEC = IDECN(QM)
  712 FORMAT(' IDEC: ',I2)
 
C     DECISION 2 NOT CONSIDERED.
C     DECISION 5 MUST BE COUNTED.
 
      IDEC3 = IDEC
      IF(IDEC3.EQ.4) IDEC3 = 3
      IPRX = IPLVL(LEV)
      TX = VMSG(2)
      ZX = VMSG(1)
      IF(IVAR.EQ.1) ZX = OVAL
  702 FORMAT(F6.0)
      IF(IVAR.EQ.2) TX = OVAL
  703 FORMAT(F6.1)
      ZCORX = VMSG(1)
      TCORX = VMSG(2)
      ZCX = VMSG(1)
      TCX = VMSG(2)
      IF(IVAR.EQ.1) THEN
        ZCX = CORVAL
        ZCORX = COR
      ELSEIF(IVAR.EQ.2) THEN
        TCX = CORVAL
        TCORX = COR
      ENDIF
      IF(ABS(BAS).GT.30000) BAS = 0.
      IF(ABS(PMSLI).GT.30000) PMSLI = 0.
      IF(ABS(PMSLR).GT.30000) PMSLR = 0.
  721 FORMAT(1X,I10,A9,3F10.0)
      IF(IVAR.LE.0.OR.IVAR.GT.5) GO TO 100
      IWRITB = 0
      IWRITT = 1
      IWRITS = 0
      DO 900 I=1,100
        IF(NCHAR.EQ.0.AND.IBL(I).EQ.IB.OR.IBL(I).EQ.100) IWRITB = 1
        IF(NCHAR.NE.0.AND.IBL(I).EQ.100) IWRITS = 1
  900 CONTINUE
      IF(IWRITB.EQ.1) THEN
        DO 910 I=1,20
          IF(ITYP(I).LT.0) IWRITT = 1
          IF(ITYP(I).EQ.IT) IWRITT = 1
  910   CONTINUE
        IF(IWRITT.EQ.1) THEN

          IF(IKEY.EQ.2)THEN
          IF(MOD(ICNT,25).EQ.0) WRITE(6,600)
          WRITE(6,603) IDSTN,CDTE,SEQN,ISCAN,LEVL,PRESS,
     &      LTYP,LSTYP,CVAR,OVAL,COR,CORVAL,PCOR,QM,IHSC,BAS,PMSLI,PMSLR
  603     FORMAT(1X,A8,A10,F5.0,I5,I4,F8.1,I5,I6,2X,A2,4F9.1,F4.0,I5,
     &      3(1X,F7.1))
          ICNT = ICNT + 1
          ENDIF
        ENDIF
  600   FORMAT(/'   STN         DATE SEQN SCAN LEV   PRESS LTYP LSTYP',
     &    ' VAR     OVAL      COR   CORVAL     PCOR  QM IHSC     BAS',
     &    '   PMSLI   PMSLR')
  607   FORMAT(1X,132A1)
        ENDIF
 
C     PRINT SHIP DATA IF ALL BLOCKS ARE PRINTED.
 
      IF(IWRITS.EQ.1.AND.IWRITT.NE.1) THEN

        IF(IKEY.EQ.2)THEN
        IF(MOD(ICNT,25).EQ.0) WRITE(6,600)
        WRITE(6,603) IDSTN,CDTE,SEQN,ISCAN,LEVL,PRESS,
     &    LTYP,LSTYP,CVAR,OVAL,COR,CORVAL,PCOR,QM,IHSC,BAS,PMSLI,PMSLR
        ICNT = ICNT + 1
        ENDIF

      ENDIF
      IF(IT.GT.ITYPS.OR.IDEC.EQ.2) GO TO 100

C     FIND TOTAL INFORMATION FOR CORRESPONDING DATE.

      DO 1000 JD=1,NDATES
        IF(IDT.EQ.IDATES(JD)) THEN
          IDATE = JD
          GO TO 1010
        ENDIF
 1000 CONTINUE

C     NO CORRESPONDING DATE FOUND.  READ NEXT DATA.

      IF(IKEY.EQ.2)THEN
      WRITE(6,1009) IDT
      ENDIF

 1009 FORMAT(' NO CORRESPOINDING DATE FOUND. IDT = ',I10)
      GO TO 100
 1010 CONTINUE

C     CORRESPONDING DATE FOUND

C     COUNT OF ERROR BY TYPE, DECISION.
C     DON'T CONSIDER DECISION 2'S FURTHER.

      NDT(IT,IDEC) = NDT(IT,IDEC) + 1
      IF(IDEC.EQ.2) GO TO 100


C     ADD TO SUMS. NUMBER OF ERRORS.

      NE = NE + 1
      IF(NCHAR.NE.0) NES = NES + 1

C     N(IT,IP): NUMBER OR ERRORS BY TYPE AND PRESSURE.

      DO 50 I=1,NPLVL
        IP = I
        IF(IPRX.EQ.IPLVL(I)) GO TO 60
   50 CONTINUE
   60 CONTINUE
  609   FORMAT(' IT,IP: ',2I8)
        IF(IT.GT.0) THEN
        N(IT,IP) = N(IT,IP) + 1
        IF(IDEC.EQ.1) NC(IT,IP) = NC(IT,IP) + 1
      ENDIF
 
C     COUNT OF REPORTS WITH OBSERVATION ERRORS.
 
      IF(IDEC3.EQ.3) THEN
        DO 63 I=1,NSTNS
          IF(IDENT.EQ.IDO(I,IDATE)) GO TO 65
          IF(IDO(I,IDATE).EQ.0) GO TO 64
   63   CONTINUE
   64   IDO(I,IDATE) = IDENT
        IBLK = IDENT/1000
  614   FORMAT(' IBLK: ',I3)
        NRO(IBLK) = NRO(IBLK) + 1
   65   CONTINUE
      ENDIF
 
C     ID(IS, ): ADD STATION TO LIST IF NOT THERE ALREADY.
      IF(NCHAR.NE.0) GO TO 75
 
C     TEST TO SEE IF IDENT MATCHES ANY IN LIST: ID(IS,1)
C     IF SO, ADD 1 TO COUNTER.
C     IF NOT, ADD STATION TO LIST.
C     IF THE STATION IS A SHIP, KEEP TRACK OF IT SEPARATELY.
 
      DO 70 I=1,NS
        IF(IDENT.EQ.ID(I,1).AND.IDEC.NE.2) THEN
          ID(I,2) = ID(I,2) + 1
          IF(IT.GT.0) ID(I,2+IT) = ID(I,2+IT) + 1
          IF(IT.GE.30 .AND. IT.LE.50) ID(I,ITPP) = ID(I,ITPP) + 1
          IS = I
          GO TO 80
        ELSEIF(IDENT.EQ.99999) THEN
          GO TO 200
        ENDIF
   70 CONTINUE
      IF(NS.LT.1000) THEN
        NS = NS + 1
        ID(NS,1) = IDENT
  635   FORMAT(' NS,IDENT: ',2I8)
        ID(NS,2) = 1
        IF(IT.GT.0) ID(NS,2+IT) = 1
        IF(IDEC3.EQ.3) ID(I,ITPP) = ID(I,ITPP) + 1
        IS = NS
        GO TO 80
      ELSE
        GO TO 100
      ENDIF
   75 CONTINUE
      DO 76 I=1,NSS
        IF(IDSTN.EQ.CDS(I)) THEN
          IDS(I,2) = IDS(I,2) + 1
          IF(IT.GT.0) IDS(I,2+IT) = IDS(I,2+IT) + 1
          IF(IDEC3.EQ.3) IDS(I,ITPP) = IDS(I,ITPP) + 1
          GO TO 100
        ELSEIF(IDSTN.EQ.'99999'.OR.IDSTN.EQ.NINES) THEN
          GO TO 200
        ENDIF
   76 CONTINUE
      IF(NSS.LT.100) THEN
        NSS = NSS + 1
        CDS(NSS) = IDSTN
        IDS(NSS,2) = 1
        IF(IT.GT.0) IDS(NSS,2+IT) = 1
      ENDIF
      GO TO 100
   80 CONTINUE
 
C     COLLECT INFORMATION ON TYPE 100 AND 102 ERRORS.
 
      IF(IT.EQ.100.OR.IT.EQ.102) THEN
  722   FORMAT(1X,I9,3F10.0)
        SBAS(IS) = SBAS(IS) + BAS
        SPMSLI(IS) = SPMSLI(IS) + PMSLI
        SPMSLR(IS) = SPMSLR(IS) + PMSLR
        SBS(IS) = SBS(IS) + BAS**2
        SPIS(IS) = SPIS(IS) + PMSLI**2
        SPHS(IS) = SPHS(IS) + PMSLR**2
        IBN = BAS/5.
        DO 1080 I=1,IBNN
          II = I
          IF(IDENT.EQ.IBIN(I,1).AND.IDENT.NE.0
     &      .AND.IBN.EQ.IBIN(I,2).AND.ABS(IBN).GE.6) GO TO 1090
 1080   CONTINUE

C     NEW STATION AND/OR BIN

        IF(ABS(IBN).GE.6) THEN
          IBNN = IBNN + 1
          IBIN(IBNN,1) = IDENT
          IBIN(IBNN,2) = IBN
          IBIN(IBNN,3) = IBIN(IBNN,3) + 1
  251     FORMAT(1X,I9,F10.0,4I10)
        ENDIF
        GO TO 2000

C     MATCHES EXISTING STATION AND BIN.

 1090   CONTINUE
        IBIN(II,3) = IBIN(II,3) + 1
 2000   CONTINUE
      ENDIF
 
C     NB(IB): NUMBER OF ERRORS BY BLOCK.
 
      IF(IB.LT.100) THEN
        IF(IT.GT.0) NBL(IT,IB) = NBL(IT,IB) + 1
C       INCREMENT COUNTER FOR CONFIDENT CORRECTIONS.
        IF((IT.LT.30 .OR. IT.GT.50) .AND. IDEC.EQ.1)
     &    NCONF(IB) = NCONF(IB) + 1
      ENDIF
 
C     INCREMENT COUNTER FOR APPROPRIATE COMBINED REGION(S).
 
      DO 82 I=1,NOBLK
        NNN = NNBLK(I)
        DO 81 J=1,NNN
          IF(NBLK(I,J).EQ.IB.AND.IT.GT.0) THEN
            NBL(IT,I+99) = NBL(IT,I+99) + 1
            IF((IT.LT.30 .OR. IT.GT.50) .AND. IDEC.EQ.1)
     &         NCONF(I+99) = NCONF(I+99) + 1
          ENDIF
   81   CONTINUE
   82 CONTINUE
 
C     INCREMENT COUNTERS STRATIFIED BY DECISION.
 
      NDC(IDEC3,IB,IP) = NDC(IDEC3,IB,IP) + 1
      IF(IDEC3.NE.2) THEN
        NDCB(IDEC3,IB) = NDCB(IDEC3,IB) + 1
      ENDIF
      IF(IT.LT.30 .OR. IT.GT.50) NDCB(2,IB) = NDCB(2,IB) + 1 
      NDCP(IDEC3,IP) = NDCP(IDEC3,IP) + 1
      NDEC(IDEC3) = NDEC(IDEC3) + 1
 
C     RETURN TO 100 TO READ MORE DATA
 
      GO TO 100
 
C     PRINT DATES.
C     COMPUTE COUNT OF ERRORS BY ERROR TYPE.
 
  200 CONTINUE
      IF(IKEY.NE.3)THEN
      WRITE(6,512)
      ENDIF
  512 FORMAT(' DATES:')
      DO 85 I=1,NDATES
        IF(IKEY.NE.3)THEN
        WRITE(6,513) CDATES(I)
        ENDIF
   85 CONTINUE
 
C     PRINT TOTALS INFORMATION.
 
  513 FORMAT(10X,A10)

      IF(IKEY.NE.3)THEN
      WRITE(6,630) (NTOTL(I),I=1,5)
      ENDIF
  630 FORMAT(' TOTAL LAND REPORTS:',I7,'  00Z:',I7,'  06Z:',I7,
     &  '  12Z:',I7,'  18Z:',I7)

      IF(IKEY.NE.3)THEN
      WRITE(6,631) (NTOTS(I),I=1,5)
      ENDIF
  631 FORMAT(' TOTAL SHIP REPORTS:',I7,'  00Z:',I7,'  06Z:',I7,
     &  '  12Z:',I7,'  18Z:',I7)

      IF(IKEY.NE.3)THEN
      WRITE(6,632) (NTD(I),I=1,5)
      ENDIF
  632 FORMAT(' NO. REPORT TIMES:  ',I7,'  00Z:',I7,'  06Z:',I7,
     &  '  12Z:',I7,'  18Z:',I7)

      IF(IKEY.NE.3)THEN
      WRITE(6,633)
      ENDIF
      WRITE(17,633)
  633 FORMAT(' TOTAL NUMBER OF REPORTS FOR LAND STATIONS BY',
     &  ' WMO BLOCK (COMBINED, 00Z 06Z 12Z 18Z):',//,
     &  6X,'     0     1     2     3     4     5     6     7',
     &  '     8     9',/)
 
C     INCREMENT COUNT FOR APPROPRIATE COMBINED REGION(S).
 
      DO 1305 I=1,NBLOCK
        DO 1304 II=1,NOBLK
          NNN = NNBLK(II)
          DO 1303 J=1,NNN
            IF(NBLK(II,J).EQ.I) THEN
              NRO(II+99) = NRO(II+99) + NRO(I)
              DO 1195 IDC=1,5
                NDCB(IDC,II+99) = NDCB(IDC,II+99) + NDCB(IDC,I)
                DO 1194 IP=1,NPLVL
                  NDC(IDC,II+99,IP) = NDC(IDC,II+99,IP)
     &              + NDC(IDC,I,IP)
 1194           CONTINUE
 1195         CONTINUE
            ENDIF
 1303     CONTINUE
 1304   CONTINUE
 1305 CONTINUE
      NZERO = 0
      DO 542 K=1,5
        DO 541 I=1,12
          II = 10 * (I-1)
          IF(I.EQ.1) THEN

            IF(IKEY.NE.3)THEN
            WRITE(6,651) II, NZERO, (NTOTB(J,K),J=1,9)
            ENDIF
            IF(K.EQ.1)

     &      WRITE(17,1651) II, NZERO, (NTOTB(J,K),J=1,9)
          ELSE
            IF(IKEY.NE.3)THEN
            WRITE(6,651) II, (NTOTB(II+J-1,K),J=1,10)
            ENDIF
            IF(K.EQ.1)
     &      WRITE(17,1651) II, (NTOTB(II+J-1,K),J=1,10)
          ENDIF
  541   CONTINUE

        IF(IKEY.NE.3)THEN
        WRITE(6,626)
        ENDIF
  542 CONTINUE
      NNOBS = 0
      DO 105 IP=1,NPLVL
        NNOBS = NNOBS + NDCP(3,IP)
        DO 90 I=1,ITYPS
          NN(I) = NN(I) + N(I,IP)
          NNC(I) = NNC(I) + NC(I,IP)
   90   CONTINUE
  105 CONTINUE
      DO 108 IB=1,NBLOCK
        DO 106 I=1,ITYPS
          NB(IB) = NB(IB) + NBL(I,IB)
  106   CONTINUE
  108 CONTINUE

C     COMPUTE COUNT OF ERRORS BY PRESSURE.

      DO 120 IP=1,NPLVL
        DO 110 I=1,ITYPS
          NP(IP) = NP(IP) + N(I,IP)
          NCP(IP) = NCP(IP) + NC(I,IP)
  110   CONTINUE

C       ADD OBSERVATION ERRORS TO COUNT.

  120 CONTINUE
 
C     COUNT THE NUMBER OF ERROR-CONTAINING STATIONS BY BLOCK
 
      DO 310 I=1,NS
        IB = ID(I,1)/1000
        IF(IB.LT.1.OR.IB.GT.99) GO TO 310
        NEB(IB) = NEB(IB) + 1
 
C       INCREMENT COUNT FOR APPROPRIATE COMBINED REGION(S).
 
        DO 309 II=1,NOBLK
          NNN = NNBLK(II)
          DO 308 J=1,NNN
            IF(NBLK(II,J).EQ.IB) THEN
              NEB(II+99) = NEB(II+99) + 1
            ENDIF
  308     CONTINUE
  309   CONTINUE
  310 CONTINUE
C
C     COMPUTATIONS...
C
      DO 312 I=1,NS
        IF(ID(I,102)+ID(I,104).GE.1) THEN
          T1 = SBAS(I)/(ID(I,102)+ID(I,104))
          T2 = SPMSLI(I)/(ID(I,102)+ID(I,104))
          T3 = SPMSLR(I)/(ID(I,102)+ID(I,104))
          SBAS(I) = T1
          SPMSLI(I) = T2
          SPMSLR(I) = T3
          SBS(I) = SBS(I)/(ID(I,102)+ID(I,104)) - T1**2
          SPIS(I) = SPIS(I)/(ID(I,102)+ID(I,104)) - T2**2
          SPHS(I) = SPHS(I)/(ID(I,102)+ID(I,104)) - T3**2
          IF(SBS(I).GT.0.) THEN
            SBS(I) = SQRT(SBS(I))
          ELSE
            SBS(I) = 0.
          ENDIF
          IF(SPIS(I).GT.0.) THEN
            SPIS(I) = SQRT(SPIS(I))
          ELSE
            SPIS(I) = 0.
          ENDIF
          IF(SPHS(I).GT.0.) THEN
            SPHS(I) = SQRT(SPHS(I))
          ELSE
            SPHS(I) = 0.
          ENDIF
        ENDIF
  312 CONTINUE
      DO 320 I=1,5
        IF(NTD(I).EQ.0) THEN
          AL(I) = 0.
          AS(I) = 0.
        ELSE
          AL(I) = REAL(NTOTL(I)) / REAL(NTD(I))
          AS(I) = REAL(NTOTS(I)) / REAL(NTD(I))
        ENDIF
        DO 315 J=1,NBLOCK
          IF(NTD(I).EQ.0) THEN
            AB(J,I) = 0.
          ELSE
            AB(J,I) = REAL(NTOTB(J,I)) / REAL(NTD(I))
          ENDIF
  315   CONTINUE
  320 CONTINUE
      IF(AL(1).LT.1.0E-6.OR.NTD(1).EQ.0) THEN
        PL = 0.
      ELSE
        PL = 100. * NE / (AL(1)*REAL(NTD(1)))
      ENDIF
      DO 325 I=1,NBLOCK
        IF(NTOTB(I,1).EQ.0) THEN
          PBL(I) = 0.
        ELSE
          PBL(I) = 100. * REAL(NB(I)) / REAL(NTOTB(I,1))
        ENDIF
  325 CONTINUE
      DO 327 I=1,ITYPS
        NTYP(I) = 0
        DO 326 J=1,99
          NTYP(I) = NTYP(I) + NBL(I,J)
  326   CONTINUE
  327 CONTINUE
      DO 330 J=1,NBLOCK
        DO 328 I=1,ITYPS
          IF(NTYP(I).EQ.0) THEN
            PBEL(I,J) = 0.
          ELSE
            PBEL(I,J) = 100. * REAL(NBL(I,J)) / REAL(NTYP(I))
          ENDIF
  328   CONTINUE
  330 CONTINUE
      DO 340 I=1,ITYPS
        SUM = 0.
        DO 335 J=1,99
          SUM = SUM + NBL(I,J)
  335   CONTINUE
        IF(NTOTL(1).EQ.0 .OR. REAL(NE).EQ.0) THEN
          PEL(I) = 0.
        ELSE
          PEL(I) = 100. * SUM / REAL(NE-NES)
        ENDIF
        IF(ND.EQ.0) THEN
          AEL(I) = 0.
        ELSE
          AEL(I) = SUM / REAL(ND)
        ENDIF
  340 CONTINUE
      DO 350 I=1,NBLOCK
        IF(AB(I,1).EQ.0.) THEN
          PSB(I) = 0.
        ELSE
          PSB(I) = 100. * NEB(I) / AB(I,1)
        ENDIF
        SUM = 0.
        DO 345 J=1,ITYPS
          SUM = SUM + NBL(J,I)
  345   CONTINUE
        IF(NTOTB(I,1).EQ.0) THEN
          PRB(I) = 0.
        ELSE
          PRB(I) = 100. * SUM / REAL(NTOTB(I,1))
        ENDIF
  350 CONTINUE
      DO 360 I=1,NBLOCK
        IF(NB(I).EQ.0) THEN
          PCONF(I) = 0.
        ELSE
          IF(NDCB(2,I).GT.0) THEN
            PCONF(I) = 100. * REAL(NDCB(1,I))/REAL(NDCB(2,I))
          ENDIF
C         PCONF(I) = 100. * REAL(NCONF(I))/REAL(NB(I))
        ENDIF
  360 CONTINUE
 
C     OUTPUT THE INFORMATION.
 
      IF(IKEY.NE.3)THEN
      WRITE(6,615) (AL(I),I=1,5)
      ENDIF
  615 FORMAT(' AVERAGE NUMBER OF REPORTING LAND STATIONS:',F8.1,
     &  '   00Z:',F8.1,'   06Z:',F8.1,'   12Z:',F8.1,'   18Z:',F8.1)

      IF(IKEY.NE.3)THEN
      WRITE(6,616) (AS(I),I=1,5)
      ENDIF
  616 FORMAT(' AVERAGE NUMBER OF REPORTING SHIPS:        ',F8.1,
     &  '   00Z:',F8.1,'   06Z:',F8.1,'   12Z:',F8.1,'   18Z:',F8.1)

      IF(IKEY.NE.3)THEN
      WRITE(6,617)
      ENDIF
  617 FORMAT(' AVERAGE NUMBER OF REPORTING LAND STATIONS BY',
     &  ' WMO BLOCK (COMBINED, 00Z 06Z 12Z 18Z):')
        ZERO = 0.
        DO 539 K=1,5

        IF(IKEY.NE.3)THEN
        WRITE(6,719)
        ENDIF
        DO 539 I=1,12
          II = 10 * (I-1)
          IF(I.EQ.1) THEN

            IF(IKEY.NE.3)THEN
            WRITE(6,718) II, ZERO, (AB(J,K),J=1,9)
            ENDIF
          ELSE
            IF(IKEY.NE.3)THEN
            WRITE(6,718) II, (AB(II+J-1,K),J=1,10)
            ENDIF
          ENDIF
  539   CONTINUE

        IF(IKEY.NE.3)THEN
        WRITE(6,626)
        ENDIF
  540 CONTINUE
  626 FORMAT(1X)
  618 FORMAT(1X,I6,10F6.1)
  718 FORMAT(1X,I6,10F6.0)
 1718 FORMAT(1X,I5,',',9(F5.0,','),F5.0)
  719 FORMAT(//,6X,'     0     1     2     3     4     5     6     7',
     &  '     8     9',/)

      IF(IKEY.NE.3)THEN
      WRITE(6,503) NE, NES, ND
      ENDIF
  503 FORMAT(11X,'***SUMMARY OF HYDROSTATIC',
     &  ' ERROR SUSPICIONS AND CORRECTIONS***',//,
     &  ' TOTAL NUMBER OF ERRORS = ',I5,', TOTAL NUMBER OF SHIP ',
     &  'ERRORS = ',I5,', NUMBER OF TIMES = ',I5)

      IF(IKEY.NE.3)THEN
      WRITE(6,619) PL
      ENDIF
  619 FORMAT(/' PERCENT OF',
     &  ' LAND REPORTS WITH ERROR SUSPICIONS:',F8.2)

      IF(IKEY.NE.3)THEN
      WRITE(6,620) (PEL(I),I=1,3), (PEL(I),I=5,10), PEL(20),
     &  PEL(21), PEL(30), PEL(40), PEL(49), PEL(50), PEL(100),
     &  PEL(102), PEL(105), PEL(106)
      ENDIF
  620 FORMAT(/' PERCENTAGE DISTRIBUTION OF LAND REPORTS WITH ERROR',
     &  ' SUSPICIONS BY ERROR TYPE:',/,'      1     2     3     5',
     &  '     6     7     8     9    10    20    21    30    40',
     &  '    49    50   100',
     &  '   102   105   106',/,1X,19F6.2)

      IF(IKEY.NE.3)THEN
      WRITE(6,621) (AEL(I),I=1,3), (AEL(I),I=5,10), AEL(20),
     &  AEL(21), AEL(30), AEL(40), AEL(49), AEL(50), AEL(100),
     &  AEL(102), AEL(105), AEL(106)
      ENDIF
  621 FORMAT(/' AVERAGE NO. OF LAND REPORTS WITH ERROR ',
     &  'SUSPICIONS PER OBSERVATION TIME ',
     &  'BY ERROR TYPE:',/,'      1     2     3     5     6',
     &  '     7     8     9    10    20    21    30    40',
     &  '    49    50   100',
     &  '   102   105   106',/,1X,19F6.2)

      IF(IKEY.NE.3)THEN
      WRITE(6,650)
      ENDIF
  650 FORMAT(/' NUMBER OF STATIONS WITH HYDROSTATICALLY DETECTED',
     &  ' ERROR SUSPICIONS BY WMO BLOCK:',//,
     &  6X,'     0     1     2     3     4     5     6',
     &  '     7     8     9',/)
      NZERO = 0
      DO 565 I=1,12
        II = 10 * (I-1)
        IF(I.EQ.1) THEN

          IF(IKEY.NE.3)THEN
          WRITE(6,651) II, NZERO, (NEB(J),J=1,9)
          ENDIF
        ELSE
          IF(IKEY.NE.3)THEN
          WRITE(6,651) II, (NEB(II+J-1),J=1,10)
          ENDIF
        ENDIF
  565 CONTINUE
  651 FORMAT(1X,11I6)
 1651 FORMAT(1X,10(I5,','),I5)
 
C     NUMBER OF DECISIONS 1 BY WMO BLOCK.
 
      IF(IKEY.NE.3)THEN
      WRITE(6,752)
      ENDIF
  752 FORMAT(/' NUMBER OF DECISIONS 1',
     &  ' BY WMO BLOCK:',//,
     &  6X,'     0     1     2     3     4     5     6     7',
     &  '     8     9',/)
      DO 753 I=1,12
        II = 10*(I-1)
        IF(I.EQ.1) THEN

          IF(IKEY.NE.3)THEN
          WRITE(6,651) II, NZERO, (NDCB(1,J),J=1,9)
          ENDIF
        ELSE
          IF(IKEY.NE.3)THEN
          WRITE(6,651) II, (NDCB(1,II+J-1),J=1,10)
          ENDIF
        ENDIF
  753 CONTINUE

C     NUMBER OF HYDROSTATIC SUSPICIONS BY WMO BLOCK.

      IF(IKEY.NE.3)THEN
      WRITE(6,754)
      ENDIF
      WRITE(17,754)
  754 FORMAT(/' NUMBER OF HYDROSTATIC SUSPICIONS',
     &  ' BY WMO BLOCK:',//,
     &  6X,'     0     1     2     3     4     5     6     7',
     &  '     8     9',/)
      DO 755 I=1,12
        II = 10*(I-1)
        IF(I.EQ.1) THEN

          IF(IKEY.NE.3)THEN
          WRITE(6,651) II, NZERO, (NDCB(2,J),J=1,9)
          ENDIF
          WRITE(17,1651) II, NZERO, (NDCB(2,J),J=1,9)
        ELSE
          IF(IKEY.NE.3)THEN
          WRITE(6,651) II, (NDCB(2,II+J-1),J=1,10)
          ENDIF
          WRITE(17,1651) II, (NDCB(2,II+J-1),J=1,10)
        ENDIF
  755 CONTINUE

C     NUMBER OF OBSERVATION ERRORS WMO BLOCK.

      IF(IKEY.NE.3)THEN
      WRITE(6,756)
      ENDIF
      WRITE(17,756)
  756 FORMAT(/' NUMBER OF OBSERVATION ERRORS',
     &  ' BY WMO BLOCK:',//,
     &  6X,'     0     1     2     3     4     5     6     7',
     &  '     8     9',/)
      DO 757 I=1,12
        II = 10*(I-1)
        IF(I.EQ.1) THEN

          IF(IKEY.NE.3)THEN
          WRITE(6,651) II, NZERO, (NB(J)-NDCB(2,J),J=1,9)
          ENDIF
          WRITE(17,1651) II, NZERO, (NB(J)-NDCB(2,J),J=1,9)
        ELSE
          IF(IKEY.NE.3)THEN
          WRITE(6,651) II, (NB(II+J-1)-NDCB(2,II+J-1),J=1,10)
          ENDIF
          WRITE(17,1651) II, (NB(II+J-1)-NDCB(2,II+J-1),J=1,10)
        ENDIF
  757 CONTINUE

C     NUMBER OF DECISIONS 3 OR 4 BY WMO BLOCK.

      IF(IKEY.NE.3)THEN
      WRITE(6,652)
      ENDIF
      WRITE(17,652)
  652 FORMAT(/' NUMBER OF DECISIONS 3 OR 4',
     &  ' BY WMO BLOCK:',//,
     &  6X,'     0     1     2     3     4     5     6     7',
     &  '     8     9',/)
      DO 653 I=1,12
        II = 10*(I-1)
        IF(I.EQ.1) THEN

          IF(IKEY.NE.3)THEN
          WRITE(6,651) II, NZERO, (NDCB(3,J),J=1,9)
          ENDIF
          WRITE(17,1651) II, NZERO, (NDCB(3,J),J=1,9)
        ELSE
          IF(IKEY.NE.3)THEN
          WRITE(6,651) II, (NDCB(3,II+J-1),J=1,10)
          ENDIF
          WRITE(17,1651) II, (NDCB(3,II+J-1),J=1,10)
        ENDIF
  653 CONTINUE

C     NUMBER OF REPORTS WITH DECNS 3 OR 4 BY WMO BLOCK.

      IF(IKEY.NE.3)THEN
      WRITE(6,654)
      ENDIF
  654 FORMAT(/' NUMBER OF REPORTS WITH DECISIONS 3 OR 4 ',
     &  'BY WMO BLOCK:',//,
     &  6X,'     0     1     2     3     4     5     6     7',
     &  '     8     9',/)
      DO 655 I=1,12
        II = 10*(I-1)
        IF(I.EQ.1) THEN

          IF(IKEY.NE.3)THEN
          WRITE(6,651) II, NZERO, (NRO(J),J=1,9)
          ENDIF
        ELSE
          IF(IKEY.NE.3)THEN
          WRITE(6,651) II, (NRO(II+J-1),J=1,10)
          ENDIF
        ENDIF
  655 CONTINUE

      IF(IKEY.NE.3)THEN
      WRITE(6,504)
      ENDIF
  504 FORMAT(/' ERROR SUSPICION COUNTS BY ERROR TYPE AND PRESSURE:',//,
     &  ' PRESS    1    2    3    5    6    7    8    9',
     &  '   10   20   21   30   40   49   50  100',
     &  '  102  105  106 TOTAL',/)
      DO 130 IP=1,NPLVL

        IF(IKEY.NE.3)THEN
        WRITE(6,505) IPLVL(IP),(N(I,IP),I=1,3),(N(I,IP),I=5,10),
     &  N(20,IP), N(21,IP), N(30,IP), N(40,IP), N(49,IP), N(50,IP),
     &  N(100,IP),N(102,IP),N(105,IP),N(106,IP),NP(IP)
        ENDIF
  130 CONTINUE
      NETOT = 0
      DO 131 IP=1,NPLVL
        NETOT = NETOT + NP(IP)
  131 CONTINUE
  505 FORMAT(1X,20I5,I6)

      IF(IKEY.NE.3)THEN
      WRITE(6,506) (NN(I),I=1,3),
     &  (NN(I),I=5,10),NN(20),NN(21),NN(30),
     &  NN(40),NN(49),NN(50),NN(100),NN(102),NN(105),NN(106),
     &  NETOT
      ENDIF
  506 FORMAT(' TOTAL',19I5,I6///)

      IF(IKEY.NE.3)THEN
      WRITE(6,6504)
      ENDIF
 6504 FORMAT(/' ERROR CORRECTION COUNTS BY ERROR TYPE AND PRESSURE:',//,
     &  ' PRESS    1    2    3    5    6    7    8    9',
     &  '   10   20   21   30   40   49   50  100  102',
     &  '  105  106  TOTAL',/)
      NZERO = 0
      DO 132 IP=1,NPLVL

        IF(IKEY.NE.3)THEN
        WRITE(6,505) IPLVL(IP),(NC(I,IP),I=1,3),(NC(I,IP),I=5,10),
     &  NC(20,IP), NC(21,IP), NC(30,IP), NC(40,IP), NC(49,IP),
     &  NC(50,IP),
     &  NC(100,IP),NC(102,IP),NC(105,IP),NC(106,IP),NCP(IP)
        ENDIF
  132 CONTINUE
      NCTOT = 0
      DO 1131 IP=1,NPLVL
        NCTOT = NCTOT + NCP(IP)
 1131 CONTINUE

      IF(IKEY.NE.3)THEN
      WRITE(6,506) (NNC(I),I=1,3),
     &  (NNC(I),I=5,10),NNC(20),NNC(21),NNC(30),NNC(40),
     &  NNC(49),NNC(50),NNC(100),NNC(102),NNC(105),NNC(106),NCTOT
      ENDIF

      IF(IKEY.NE.3)THEN
      WRITE(6,627)
      ENDIF
      DO I=0,5

        IF(IKEY.NE.3)THEN
        WRITE(6,628) I, (NDT(IT,I),IT=1,3), (NDT(IT,I),IT=5,10),
     &    NDT(20,I), NDT(21,I), NDT(30,I), NDT(40,I), NDT(49,I),
     &    NDT(50,I),
     &    NDT(100,I), NDT(102,I), NDT(105,I), NDT(106,I), NDT(0,I)
        ENDIF
      ENDDO
  627 FORMAT(/' ERROR SUSPICION COUNTS BY ERROR TYPE AND DECISION:',//,
     &  ' DECSN    1    2    3    5    6    7    8    9',
     &  '   10   20   21   30   40   49   50  100  102',
     &  '  105  106    0',/)
  628 FORMAT(1X,21I5)

      IF(IKEY.NE.3)THEN
      WRITE(6,507)
      ENDIF
  507 FORMAT(/' ERROR SUSPICION COUNTS BY STATION BLOCK (ALL ',
     &  'ERROR TYPES):',//,
     &  6X,'    0    1    2    3    4    5    6    7',
     &  '    8    9',/)
      NZERO = 0
      DO 140 I=1,12
        II = 10 * (I-1)
        IF(I.EQ.1) THEN

          IF(IKEY.NE.3)THEN
          WRITE(6,508) II, NZERO, (NB(J),J=1,9)
          ENDIF
        ELSE
          IF(IKEY.NE.3)THEN
          WRITE(6,508) II, (NB(II+J-1),J=1,10)
          ENDIF
        ENDIF
  140 CONTINUE
  508 FORMAT(1X,11I5)
      DO 160 JJ=1,ITYPS
        IT = IPR(JJ)
        IF(IT.EQ.0) GO TO 160

        IF(IKEY.NE.3)THEN
        WRITE(6,514) IT
        ENDIF
  514 FORMAT(///,' ERROR SUSPICION COUNTS BY STATION BLOCK (',
     &  'ERROR TYPE = ',I3,'):',//,
     &  6X,'    0    1    2    3    4    5    6    7',
     &  '    8    9',/)
        DO 150 I=1,12
          II = 10 * (I-1)
        IF(I.EQ.1) THEN

          IF(IKEY.NE.3)THEN
          WRITE(6,508) II, NZERO, (NBL(IT,J),J=1,9)
          ENDIF
        ELSE
          IF(IKEY.NE.3)THEN
          WRITE(6,508) II, (NBL(IT,II+J-1),J=1,10)
          ENDIF
        ENDIF
  150   CONTINUE
  160 CONTINUE

      IF(IKEY.NE.3)THEN
      WRITE(6,624)
      ENDIF
  624 FORMAT(/' (HYDROSTATIC ERRORS/REPORTS)X100 BY',
     &  ' WMO BLOCK:',//,
     &  6X,'     0     1     2     3     4     5     6     7',
     &  '     8     9',/)
      ZERO = 0.
      DO 558 I=1,12
        II = 10 * (I-1)
        IF(I.EQ.1) THEN

          IF(IKEY.NE.3)THEN
          WRITE(6,618) II, ZERO, (PBL(J),J=1,9)
          ENDIF
        ELSE
          IF(IKEY.NE.3)THEN
          WRITE(6,618) II, (PBL(II+J-1),J=1,10)
          ENDIF
        ENDIF
  558 CONTINUE

      IF(IKEY.NE.3)THEN
      WRITE(6,591)
      ENDIF
  591 FORMAT(/' PERCENT OF CONFIDENT CORRECTIONS BY',
     &  ' WMO BLOCK (ALL HYDROSTATICALLY DETECTABLE TYPES):',//,
     &  6X,'     0     1     2     3     4     5     6',
     &  '     7     8     9',/)
      ZERO = 0.
      DO 590 I=1,12
        II = 10 * (I-1)
        IF(I.EQ.1) THEN

          IF(IKEY.NE.3)THEN
          WRITE(6,618) II, ZERO, (PCONF(J),J=1,9)
          ENDIF
        ELSE
          IF(IKEY.NE.3)THEN
          WRITE(6,618) II, (PCONF(II+J-1),J=1,10)
          ENDIF
        ENDIF
  590 CONTINUE
 
C     LIST OF OFFENDING STATIONS.
C     FIRST SORT THE LIST BY BLOCK AND STATION NO.
 
      CALL SORT(ID,NSTNS,ITPP,SBAS,SBS,SPMSLI,SPIS,SPMSLR,SPHS,NS)

      IF(IKEY.NE.3)THEN 
      WRITE(6,509)
  509 FORMAT(///,' LIST OF STATIONS WITH ERROR SUSPICIONS:')
      ENDIF

      IF(IKEY.EQ.3)THEN
      WRITE(6,5555)
 5555 FORMAT(' LIST OF STATIONS WITH ERROR SUSPICIONS:')
      ENDIF

 1020 READ(16,594,END=1021,ERR=1021) CDT,DHR,SID,NLV,PART,
     &    (ICAT(I),I=1,5)
      IF(PART.EQ.'MASS') THEN
        READ(16,659,END=1021,ERR=1021) ((VINC(J,K),J=1,21),K=1,3),BASRES
      ELSEIF(PART.EQ.'WIND') THEN
        READ(16,660,END=1021,ERR=1021) ((VINC(J,K),J=1,21),K=1,4)
      ELSEIF(PART.EQ.'    ') THEN
        GOTO 1021
      ELSE
        GOTO 1020
      ENDIF
  594 FORMAT(1X,A10,1X,F8.2,1X,A8,1X,I5,1X,A4,1X,5I5)
  659 FORMAT(10X,21F8.0,/,10X,21F8.1,/,10X,21F8.0,/,10X,F8.0)
  660 FORMAT(10X,21F8.1,/,10X,21F8.1,/,10X,21F8.1,/,10X,21F8.1)

      ICHAR = 0
      DO I=1,8
        DO J=1,11
          IF(SID1(I).EQ.CNUM(J)) GOTO 1022
        ENDDO
        ICHAR = ICHAR + 1
 1022   CONTINUE
      ENDDO
      IF(ICHAR.EQ.0) THEN

C  LAND STATION.  SEE IF STATION IS ALREADY ON LIST
C  ------------------------------------------------

        READ(UNIT=SID,FMT=595) ISID
  595   FORMAT(I8)
        IF(LAND.GT.0) THEN
          DO II=1,LAND
            I = II
            IF(ISID.EQ.ISR(I,1)) GOTO 1023
          ENDDO
        ENDIF

C  NEW LAND STATION FOR LIST
C  -------------------------

        LAND = LAND + 1
        I = LAND
        ISR(I,1) = ISID
 1023   CONTINUE
        ISR(I,2) = ISR(I,2) + 1       ! COUNT OF TIMES STATION REPORTED
        ISR(I,3) = ISR(I,3) + NLV     ! ADD NO. OF LEVELS REPORTING
        DO J=1,5
          ISR(I,J+3) = ISR(I,J+3) + ICAT(J) ! ADD COUNT OF EACH CATEGORY
        ENDDO
      ELSE

C  SHIP
C  ----

        IF(ISHIP.GT.0) THEN
          DO II=1,ISHIP
            I = II
            IF(SID.EQ.CSRS(I)) GOTO 1029
          ENDDO
        ENDIF
        GOTO 1031
 1029   CONTINUE
        ISRS(I,2) = ISRS(I,2) + 1
        ISRS(I,3) = ISRS(I,3) + NLV
        DO J=1,5
          ISRS(I,J+3) = ISRS(I,J+3) + ICAT(J)
        ENDDO
        GOTO 1030

C  NEW SHIP FOR LIST
C  -----------------

 1031   IF(ISHIP.LT.100) THEN
          ISHIP = ISHIP + 1
          I = ISHIP
          CSRS(I) = SID
 1024     CONTINUE
          ISRS(I,2) = ISRS(I,2) + 1
          ISRS(I,3) = ISRS(I,3) + NLV
          DO J=1,5
            ISRS(I,J+3) = ISRS(I,J+3) + ICAT(J)
          ENDDO
        ENDIF
 1030 CONTINUE
      ENDIF
      GOTO 1020
 1021 CONTINUE

      WRITE(6,403) LAND,ISHIP
  403 FORMAT(' LAND,ISHIP: ',2I5)

C  FINISHED MAKING LISTS
C  ---------------------

      ISS = 0
      DO 170 IS=1,NS
        NUM = 0
        LEVS = 0
        NCAT1 = 0
        NCAT2 = 0
        DO L=1,LAND
          IF(ISR(L,1).EQ.ID(IS,1)) GOTO 1025
        ENDDO
        GOTO 1026
 1025   NUM = ISR(L,2)/2    ! isr(l,2) counts mass/wind parts
        IF(ISR(L,2).GT.0) THEN
          LEVS  = ISR(L,3)/ISR(L,2)  ! Average no. levels
          NCAT1 = ISR(L,4)/ISR(L,2)  ! Average mand levels
          NCAT2 = ISR(L,5)/ISR(L,2)  ! Average sig levels
        ENDIF
        ID(IS,ITYPS+4) = NUM
        ID(IS,ITYPS+5) = LEVS
        ID(IS,ITYPS+6) = NCAT1
        ID(IS,ITYPS+7) = NCAT2
 1026   CONTINUE
        IDBL = ID(IS,1)/1000
        DO 169 I=1,100
          IF(IDBL.EQ.IBL(I) .OR. IBL(I).EQ.100) THEN
            ISS = ISS + 1
            IF(MOD(ISS-1,10).EQ.0) WRITE(6,511)

            WRITE(6,510) ID(IS,1),NUM,LEVS,NCAT1,NCAT2,ID(IS,ITPP),
     &        ID(IS,2)-ID(IS,ITPP),
     &        (ID(IS,J),J=3,5),(ID(IS,K),K=7,12),ID(IS,22),ID(IS,23),
     &        ID(IS,32),ID(IS,42),ID(IS,51),ID(IS,52),
     &        ID(IS,102),ID(IS,104),ID(IS,107),
     &        ID(IS,108)
          ENDIF
  169   CONTINUE
  170 CONTINUE
  510 FORMAT(1X,I7,4I5,21I4)
  511 FORMAT(/,' STATION  NUM LEVS MAND  SIG OBS HYD   1   2   3   5',
     &  '   6   7   8   9  10  20  21  30  40  49  50 100',
     &  ' 102 105 106',/)

C  PRINT OUT LIST OF OFFENDING SHIPS

      IF(IBL(1).EQ.100) THEN

        WRITE(6,515)
        DO 180 IS=1,NSS
          NUM  =  0
          LEVS  = 0
          NCAT1 = 0
          NCAT2 = 0
          DO L=1,ISHIP
            IF(CSRS(L).EQ.CDS(IS)) GOTO 1027
          ENDDO
          GOTO 1028
 1027     NUM = ISRS(L,2)
          IF(ISRS(L,2).GT.0) THEN
            LEVS  = ISRS(L,3)/ISRS(L,2)
            NCAT1 = ISRS(L,4)/ISRS(L,2)

            NCAT2 = ISRS(L,5)/ISRS(L,2)
          ENDIF
 1028     CONTINUE
          IF(MOD(IS-1,10).EQ.0) WRITE(6,516)

          WRITE(6,517) CDS(IS),NUM,LEVS,NCAT1,NCAT2,IDS(IS,ITPP),
     &        (IDS(IS,J),J=2,5),(IDS(IS,K),K=7,12),IDS(IS,22),
     &        IDS(IS,23),IDS(IS,32),IDS(IS,42),IDS(IS,51),IDS(IS,52),
     &        IDS(IS,102),IDS(IS,104),IDS(IS,107),IDS(IS,108)
  180   CONTINUE
      ENDIF
  515 FORMAT(/,' LIST OF SHIPS WITH ERRORS:')
  516 FORMAT(/,' SHIP     NUM LEVS MAND  SIG OBS HYD   1   2   3   5',
     &  '   6   7   8   9  10  20  21  30  40  49  50 100',
     &  ' 102 105 106',/)
  517 FORMAT(1X,A7,4I5,21I4)


C     BASELINE ERRORS.

      WRITE(6,639)
      DO 220 IS=1,NS
        IF(ID(IS,102)+ID(IS,104).GT.1) THEN
          IBSUM = ID(IS,102) + ID(IS,104)

          WRITE(6,640) ID(IS,1),IBSUM,SBAS(IS),SBS(IS),
     &      SPMSLI(IS),SPIS(IS),SPMSLR(IS),SPHS(IS)
        ENDIF
  220 CONTINUE
  639 FORMAT(/' STATION  COUNT  AV ZS-ERR  SD ZS-ERR',
     &  '  AV PM-INC  SD PM-INC  AV PM-RES  SD PM-RES')
  640 FORMAT(1X,2I7,6F11.1)

C     SORT BINNED RESULTS BY STATION, BIN.

      CALL SORTB(IBIN,IBNN)

C     PRINT RESULTS

      WRITE(6,2010)
      DO 2011 I=1,500
        IF(IBIN(I,1).NE.0) THEN
          IERR = IBIN(I,2) * 5

          WRITE(6,2020) IBIN(I,1), IERR, IBIN(I,3)
        ENDIF
 2011 CONTINUE
 2010 FORMAT(' BASELINE ERROR DISTRIBUTION'//
     &  '  STATION   ERR   CNT')
 2020 FORMAT(1X,I8,2I6)

C     APPROXIMATE CONSISTENT BASELINE ERROR.

      IDNT = IBIN(1,1)
      IOLD = 1
      SUM = 0.
      SUMS = 0.
      ISUM = 0
      DO 2200 I=1,IBNN
        IF(IBIN(I,1).EQ.IDNT) THEN
          IF(IBIN(I,3).GE.2) THEN
            SUM = SUM + IBIN(I,2) * IBIN(I,3) * 5.
            SUMS = SUMS + IBIN(I,3) * (IBIN(I,2)*5.)**2
            ISUM = ISUM + IBIN(I,3)
            AVG = SUM/ISUM
	    IF(ISUM.GT.0) THEN
              STD = SUMS/ISUM - AVG**2
            ELSE
              STD = 0.
            ENDIF
            IF(STD.GT.0.) THEN
              STD = SQRT(STD)
            ELSE
              STD = 0.
            ENDIF
          ENDIF
        ELSE
          IF(ISUM.GE.2)

     &      WRITE(6,2221) IBIN(IOLD,1), AVG, STD, ISUM
 2221     FORMAT(' PERSISTENT ZS-ERROR FOR: ',I6,' MEAN = ',
     &      F6.0,' M, STD = ',F6.1,' M**2, CNT = ',I3)
          IOLD = I
          IDNT = IBIN(I,1)
          SUM = 0.
          SUMS = 0.
          ISUM = 0
          IF(IBIN(I,3).GE.2) THEN
            SUM = SUM + IBIN(I,2) * IBIN(I,3) * 5.
            SUMS = SUMS + IBIN(I,3) * (IBIN(I,2)*5.)**2
            ISUM = ISUM + IBIN(I,3)
            AVG = SUM/ISUM
            STD = SUMS/ISUM - AVG**2
            IF(STD.GT.0.) THEN
              STD = SQRT(STD)
            ELSE
              STD = 0.
            ENDIF
          ENDIF
        ENDIF
 2200 CONTINUE
      STOP
  400 CONTINUE

      WRITE(6,700)
  700 FORMAT(/' PROBLEM WITH READING TOTAL REPORTS INFORMATION.',
     &  ' DATES DONT AGREE.')
      STOP
  401 CONTINUE

      WRITE(6,402)
  402 FORMAT(/' ERROR IN READING FROM UNIT 16.')
      STOP
      END
C***********************************************************

      FUNCTION IDECN(QM)

      IDECN = QM
      IF(IDECN.EQ.13) IDECN = 4

      RETURN
      END

C***********************************************************

      FUNCTION ITYPE(IHSC,IVAR)

      ITYPE = IHSC
      IF(IHSC.EQ.22) ITYPE = 20
      IF(IHSC.EQ.23 .OR. IHSC.EQ.24 .OR. IHSC.EQ.25) ITYPE = 21
      IF(IHSC.GE.30 .AND. IHSC.LE.37) ITYPE = 30
      IF(IHSC.GE.41 .AND. IHSC.LE.43) ITYPE = 40
      IF(IHSC.EQ.49 .AND. IVAR.EQ.2) ITYPE = 50

      RETURN
      END

C***********************************************************

      FUNCTION IV(CVAR)

      CHARACTER*2 CVAR,CZ,CT,CP
      DATA CZ /' Z'/, CT /' T'/, CP /' P'/
      IF(CVAR.EQ.CZ) THEN
        IV = 1
      ELSEIF(CVAR.EQ.CT) THEN
        IV = 2
      ELSEIF(CVAR.EQ.CP) THEN
        IV = 5
      ELSE
        IV = 0
      ENDIF
 
      RETURN
      END

C***********************************************************

      FUNCTION MANLEV(P)
      SAVE ISET, MANLIN

      INTEGER MANLIN(10001)
      DATA ISET /0/

      IF(ISET.EQ.0) THEN
         DO I=1,10001
         MANLIN(I) = 0
         IF(I.EQ.10000) MANLIN(I) = 1
         IF(I.EQ. 9250) MANLIN(I) = 2
         IF(I.EQ. 8500) MANLIN(I) = 3
         IF(I.EQ. 7000) MANLIN(I) = 4
         IF(I.EQ. 5000) MANLIN(I) = 5
         IF(I.EQ. 4000) MANLIN(I) = 6
         IF(I.EQ. 3000) MANLIN(I) = 7
         IF(I.EQ. 2500) MANLIN(I) = 8
         IF(I.EQ. 2000) MANLIN(I) = 9
         IF(I.EQ. 1500) MANLIN(I) = 10
         IF(I.EQ. 1000) MANLIN(I) = 11
         IF(I.EQ.  700) MANLIN(I) = 12
         IF(I.EQ.  500) MANLIN(I) = 13
         IF(I.EQ.  300) MANLIN(I) = 14
         IF(I.EQ.  200) MANLIN(I) = 15
         IF(I.EQ.  100) MANLIN(I) = 16
         IF(I.EQ.   70) MANLIN(I) = 17
         IF(I.EQ.   50) MANLIN(I) = 18
         IF(I.EQ.   30) MANLIN(I) = 19
         IF(I.EQ.   20) MANLIN(I) = 20
         IF(I.EQ.   10) MANLIN(I) = 21
         ENDDO
         ISET = 1
      ENDIF

      IP = NINT(P*10.)

      IF(IP.GT.10000 .OR. IP.LT.10 .OR. MOD(IP,10).NE.0) THEN
         MANLEV = 0
      ELSE
         MANLEV = MANLIN(NINT(P*10.))
      ENDIF

      RETURN
      END

C******************************************************************

      FUNCTION NMANLV(P)
      SAVE ISET, NANLIN

      INTEGER NANLIN(10501)
      DATA ISET /0/

C  DETERMINE THE INDEX OF THE NEAREST MANDATORY LEVEL
C  --------------------------------------------------

      IF(ISET.EQ.0) THEN
         DO I=1,10501
         NANLIN(I) = 0
         IF(                I.GE.9620) NANLIN(I) = 1
         IF(I.LE.9619 .AND. I.GE.8880) NANLIN(I) = 2
         IF(I.LE.8879 .AND. I.GE.7750) NANLIN(I) = 3
         IF(I.LE.7749 .AND. I.GE.6000) NANLIN(I) = 4
         IF(I.LE.5999 .AND. I.GE.4500) NANLIN(I) = 5
         IF(I.LE.4499 .AND. I.GE.3500) NANLIN(I) = 6
         IF(I.LE.3499 .AND. I.GE.2750) NANLIN(I) = 7
         IF(I.LE.2749 .AND. I.GE.2250) NANLIN(I) = 8
         IF(I.LE.2249 .AND. I.GE.1750) NANLIN(I) = 9
         IF(I.LE.1749 .AND. I.GE.1250) NANLIN(I) = 10
         IF(I.LE.1249 .AND. I.GE. 850) NANLIN(I) = 11
         IF(I.LE. 849 .AND. I.GE. 600) NANLIN(I) = 12
         IF(I.LE. 599 .AND. I.GE. 400) NANLIN(I) = 13
         IF(I.LE. 399 .AND. I.GE. 250) NANLIN(I) = 14
         IF(I.LE. 249 .AND. I.GE. 150) NANLIN(I) = 15
         IF(I.LE. 149 .AND. I.GE.  85) NANLIN(I) = 16
         IF(I.LE.  84 .AND. I.GE.  60) NANLIN(I) = 17
         IF(I.LE.  59 .AND. I.GE.  40) NANLIN(I) = 18
         IF(I.LE.  39 .AND. I.GE.  25) NANLIN(I) = 19
         IF(I.LE.  24 .AND. I.GE.  15) NANLIN(I) = 20
         IF(I.LE.  14                ) NANLIN(I) = 21
         ENDDO
         ISET = 1
      ENDIF

      IP = NINT(P*10.)

      IF(IP.GT.10500) THEN
         NMANLV = 1
      ELSEIF(IP.LT.5) THEN
         NMANLV = 21
      ELSE
         NMANLV = NANLIN(IP)
      ENDIF

      RETURN
      END


      SUBROUTINE SORT(IS,NSTNS,LL,S1,S2,S3,S4,S5,S6,NUM)
 
C$$$  SUBROUTINE DOCUMENTATION BLOCK
C
C SUBPROGRAM: SORT
C   PRGMMR: LILLY            ORG: NP12        DATE: 2009-09-23
C
C ABSTRACT: Sort the number of values in list 
C
C PROGRAM HISTORY LOG:
C
C 2009-09-23 Steven G. Lilly --  Updated sorc codes
C
C USAGE:
C   INPUT FILES:
C     FT05FT001   -  
C                    
C
C   OUTPUT FILES:
C     FT06FT001   -
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
C   MACHINE : P6
C
C$$$

C     SORT IS(I,J) FOR J=2,LL BY VALUE OF IS(I,1).
C     THE NUMBER OF VALUES IN LIST IS NUM.

      INTEGER IS(NSTNS,LL)
      REAL S1(NSTNS), S2(NSTNS), S3(NSTNS), S4(NSTNS),
     &  S5(NSTNS), S6(NSTNS)
      NUMM = NUM-1
      DO 30 I=1,NUMM
        JLIM = NUM-I
        DO 20 J=1,JLIM
          IF(IS(J,1).GT.IS(J+1,1)) THEN
            DO 10 L=1,LL
              IST = IS(J,L)
              IS(J,L) = IS(J+1,L)
              IS(J+1,L) = IST
   10       CONTINUE
            T = S1(J)
            S1(J) = S1(J+1)
            S1(J+1) = T
            T = S2(J)
            S2(J) = S2(J+1)
            S2(J+1) = T
            T = S3(J)
            S3(J) = S3(J+1)
            S3(J+1) = T
            T = S4(J)
            S4(J) = S4(J+1)
            S4(J+1) = T
            T = S5(J)
            S5(J) = S5(J+1)
            S5(J+1) = T
            T = S6(J)
            S6(J) = S6(J+1)
            S6(J+1) = T
          ENDIF
   20   CONTINUE
   30 CONTINUE
      RETURN
      END


      SUBROUTINE SORTB(IBIN,IB)
C$$$  SUBROUTINE DOCUMENTATION BLOCK
C
C SUBPROGRAM: SORTB
C   PRGMMR: LILLY            ORG: NP12        DATE: 2009-09-23
C
C ABSTRACT: Generates the GFS Headr file
C
C PROGRAM HISTORY LOG:
C
C 2009-09-23 Steven G. Lilly --  Updated sorc codes
C
C USAGE:
C   INPUT FILES:
C     FT05FT001   -
C
C
C   OUTPUT FILES:
C     FT06FT001   -
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
C   MACHINE : P6
C
C$$$

      INTEGER IBIN(1500,3)
      IBM = IB - 1
      L=1
        DO 30 I=1,IBM
          IP = I + 1
          DO 20 J=IP,IB
            IF(IBIN(I,L).GT.IBIN(J,L)) THEN
              DO 10 K=1,3
                ITMP = IBIN(I,K)
                IBIN(I,K) = IBIN(J,K)
                IBIN(J,K) = ITMP
   10         CONTINUE
            ENDIF
   20     CONTINUE
   30   CONTINUE
   40 CONTINUE
      RETURN
      END


      SUBROUTINE REGUNS
C$$$  SUBROUTINE DOCUMENTATION BLOCK
C
C SUBPROGRAM: REGUNS
C   PRGMMR: LILLY            ORG: NP12        DATE: 2009-09-23
C
C ABSTRACT: Generates the GFS Headr file
C
C PROGRAM HISTORY LOG:
C
C 2009-09-23 Steven G. Lilly --  Updated sorc codes
C
C USAGE:
C   INPUT FILES:
C     FT05FT001   -
C
C
C   OUTPUT FILES:
C     FT06FT001   -
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
C   MACHINE : P6
C
C$$$


C     DEFINE SPECIAL REGIONS FOR COMBINED STATISTICS.

      COMMON /OBLK/ NBLK(18,20), NNBLK(18)
      IB = 0
      DO 20 I=1,18
        NNBLK(I) = 0
        DO 10 J=1,20
          NBLK(I,J) = 0
   10   CONTINUE
   20 CONTINUE
   40 CONTINUE
      IB = IB + 1

      READ(56,500,END=100) NNBLK(IB)
      NUM = NNBLK(IB)
      READ(56,501) (NBLK(IB,I),I=1,NUM)
      GO TO 40
  100 CONTINUE
  500 FORMAT(I5)
  501 FORMAT(20I3)
      RETURN
      END

      SUBROUTINE UNPACK(IPACK,IHSC,IINC,IHOI,IVOI,IBAS,IIPL,IHPL)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    UNPACK      INPACK ERROR VALUE.
C   PRGMMR: W. COLLINS       ORG: W/NMC22    DATE: 91-07-23
C
C ABSTRACT: UNPACK ERROR VALUE INTO ITS PARTS.
C
C PROGRAM HISTORY LOG:
C   91-07-23  W. COLLINS
C   92-01-31  W. COLLINS  CHANGE TO ARGUMENT ORDER, FUNCTION.
C   95-07-19  W. COLLINS  CHANGE BASE FROM 10 TO 21.
C
C USAGE:    CALL UNPACK(IPACK, IHSC, IINC, IHOI, IVOI, IBAS, IIPL, IHPL)
C   INPUT ARGUMENT LIST:
C     IPACK    - PACKED ERROR
C
C   OUTPUT ARGUMENT LIST:
C     IHSC     - HYDROSTATIC ERROR TYPE
C     IINC     - INCREMENT CODE
C     IHOI     - HORIZONTAL RESIDUAL CODE
C     IVOI     - VERTICAL RESIDUAL CODE
C     IBAS     - BASELINE RESIDUAL CODE
C     IIPL     - PS INCREMENT CODE
C     IHPL     - MSL PRESSURE RESIDUAL CODE
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  P6
C
C$$$
 
C     UNPACK ERROR FLAGS FROM IPACK INTO IHSC,...

      IP = IPACK
      IF(IP.NE.0) THEN
        IHPL = MOD(IP,21)
        IP = NINT((IP-IHPL)/21.)
      ELSE
        GO TO 1
      ENDIF
      IF(IP.NE.0) THEN
        IIPL = MOD(IP,21)
        IP = NINT((IP-IIPL)/21.)
      ELSE
        GO TO 2
      ENDIF
      IF(IP.NE.0) THEN
        IBAS = MOD(IP,21)
        IP = NINT((IP-IBAS)/21.)
      ELSE
        GO TO 3
      ENDIF
      IF(IP.NE.0) THEN
        IVOI = MOD(IP,21)
        IP = NINT((IP-IVOI)/21.)
      ELSE
        GO TO 4
      ENDIF
      IF(IP.NE.0) THEN
        IHOI = MOD(IP,21)
        IP = NINT((IP-IHOI)/21.)
      ELSE
        GO TO 5
      ENDIF
      IF(IP.NE.0) THEN
        IINC = MOD(IP,21)
        IP = NINT((IP-IINC)/21.)
      ELSE
        GO TO 6
      ENDIF
      IF(IP.NE.0) THEN
        IHSC = IP
      ELSE
        GO TO 7
      ENDIF
      RETURN
    1 IHPL = 0
    2 IIPL = 0
    3 IBAS = 0
    4 IVOI = 0
    5 IHOI = 0
    6 IINC = 0
    7 IHSC = 0
      RETURN
      END
