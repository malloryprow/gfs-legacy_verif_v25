C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C
C MAIN PROGRAM: QCAIRSUM
C   PRGMMR: LILLY            ORG: NP12        DATE: 2015-04-24
C
C ABSTRACT:  SUMMARIZES AIRCRAFT OBS. VS FIRST GUESS AND PREPARES TABLES
C     FOR QC MONITORING REPORT.  THESE COMPRISE OF RMSV VALUES FOR
C     EACH CARRIER AND MEAN VECTOR DIFFERENCE FOR EACH 5-DEGREE
C     LAT-LON SQUARE (ALL AIRCRAFT OBS).
C
C PROGRAM HISTORY LOG:
C
C 1998-07-24 C. L. VLCEK     -- ORIGINAL AUTHOR
C 2008-06-01 Steven G. Lilly -- add tmplates for Documentation block
C 2015-04-24 Steven G. Lilly -- increase array sizes from 2500 to 5000
C
C USAGE:
C   INPUT FILES:
C     FT07FT001   -  FIRST CHECK TO SEE WHETHER OLD (2-DIGIT YEAR)
C                    OR NEW (4-DIGIT YEAR) FORMAT;
C
C   OUTPUT FILES:
C     FT21FT001   -  Air Craft data
C     FT22FT001   -  ASDAR data
C     FT23FT001   -  ACARS data
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

      PROGRAM QCAIRSUM

C     PROGRAM QCAIRSUM BY C. VLCEK
            
C     SUMMARIZES AIRCRAFT OBS. VS FIRST GUESS AND PREPARES TABLES
C     FOR QC MONITORING REPORT.  THESE COMPRISE OF RMSV VALUES FOR
C     EACH CARRIER AND MEAN VECTOR DIFFERENCE FOR EACH 5-DEGREE
C     LAT-LON SQUARE (ALL AIRCRAFT OBS).
      
C     MODIFIED 10/97 TO INCLUDE ASDAR AND ACARS (ON SEP FILES)
C     ALSO ADDED SPEED AND TEMP BIAS, RMST, AND REJ COUNTS.
C     MODIFIED 08/98 TO COMPILE IN F90 AND MAKE Y2K-COMPLIANT.
          
      REAL ATBIAS(5000), ASMDT2(5000), SUMDT2(36,72), TBIAS(36,72)
      REAL SUMDV2(5000), SUMDU(36,72), SUMDV(36,72), SBIAS(36,72)
      REAL RMSV(5000), ASBIAS(5000), VARVE, DVECT2, DV2, DU, DV, X
      REAL RMST(36,72), ARMST(5000), VARTE
      INTEGER  KOUNT(5000), NCOUNT(36,72), NREJ(5000), NGROSS(5000)
      INTEGER IAC(3)
      INTEGER  KTEMP(5000), NTEMPS(36,72), KREJ(5000), KGROSS(5000)
      INTEGER  INDATE, ENDATE, IDATE, LDATE, ILAT, JLON, IN, NCYC
      INTEGER  ICNT(3), JCNT(3), ITOUT, IPRT, ICEN19
      CHARACTER*8  ACID(5000), CSID, BLANK, CARRER, IACTYP(3)
      CHARACTER*1  CHARID(8), IBLANK, IA, IZ
      LOGICAL  OK, OKT, OLD, FIRST
      
      EQUIVALENCE (CARRER,CHARID(1))
      
      DATA  SUMDV2 /5000*0./, SUMDU /2592*0./, ICNT /3*0/, JCNT /3*0/
      DATA  SUMDV /2592*0./, RMSV /5000*0./, IAC /1,5,6/, IPRT /30/
      DATA  RMST  /2592*0./, ARMST /5000*0./
      DATA  SBIAS /2592*0./, ASBIAS /5000*0./, ATBIAS /5000*0./ 
      DATA  TBIAS /2592*0./, ASMDT2 /5000*0./, SUMDT2 /2592*0./
      DATA  KOUNT /5000*0/, NCOUNT /2592*0/, IN /10/, ITOUT /20/
      DATA  KTEMP /5000*0/, NTEMPS /2592*0/
      DATA  IACTYP /' AIRCFT ', '  ASDAR ', '  ACARS '/
      DATA  BLANK /'        '/ 
      DATA  ACID  /5000*'        '/
      DATA  IBLANK /' '/, IA /'A'/, IZ /'Z'/
C     
      ICEN19 = 19*10**8
      
      
C     SELECT PERIOD OF SUMMARIZATION
      
      READ (7,100)  INDATE, ENDATE
  100 FORMAT (2I12)
      
      
C     BIG LOOP OVER 3 TYPES: AC, ASDAR, ACARS
      
      DO 90 II = 1,3
      
C     (RE)-INITIALIZE IO PORTS, ARRAY VALUES
      
      FIRST  = .TRUE.
      OLD    = .FALSE.
      IN = II + 10
      ITOUT = II + 20
      IPRT  = II + 30
      LDATE = 0
      NCYC  = 0
      
      DO JJ = 1,36
        DO KK = 1,72
          SUMDT2(JJ,KK) = 0.
          SUMDU(JJ,KK) = 0.
          SUMDV(JJ,KK) = 0.
          SBIAS(JJ,KK) = 0.
          TBIAS(JJ,KK) = 0.
          RMST(JJ,KK) = 0.
          NCOUNT(JJ,KK) = 0
          NTEMPS(JJ,KK) = 0
        END DO
      END DO
      DO JJ = 1,5000
        RMSV(JJ) = 0.
        ARMST(JJ) = 0.
        SUMDV2(JJ) = 0.
        ASMDT2(JJ) = 0.
        ASBIAS(JJ) = 0.
        ATBIAS(JJ) = 0.
        KOUNT(JJ) = 0
        KTEMP(JJ) = 0
        ACID(JJ) = BLANK
        NGROSS(JJ) = 0
        NREJ(JJ) = 0
        KGROSS(JJ) = 0
        KREJ(JJ) = 0
      END DO
      
C     READ IN DATA:  FIRST CHECK TO SEE WHETHER OLD (2-DIGIT YEAR)
C                    OR NEW (4-DIGIT YEAR) FORMAT; CONVERT Y2 TO Y4
      
    1 CONTINUE
        IF (FIRST)    THEN
          READ (IN,101,ERR=1,END=50)  IY
  101     FORMAT (I1)
          IF (IY.EQ.9)    OLD = .TRUE.
          FIRST = .FALSE.
          REWIND IN
          GO TO 1
        ENDIF
        IF (OLD)    THEN
          READ (IN,102,ERR=1,END=50)  IDATE, CSID, ILAT, JLON, DU,DV,DS,
     &                                WQM, DT, TQM
  102     FORMAT (I8, 2X, A8, 2I4, 6F6.1)
          IDATE = IDATE + ICEN19
        ELSE
          READ (IN,103,ERR=1,END=50)  IDATE, CSID, ILAT, JLON, DU,DV,DS,
     &                                WQM, DT, TQM
  103     FORMAT (I10, 2X, A8, 2I4, 6F6.1)
        ENDIF
        IF (IDATE.LT.INDATE)    GO TO 1
        IF (IDATE.GT.ENDATE)    GO TO 50
        DV2 = DU*DU + DV*DV
        DVECT2 = SQRT(DV2)
        CARRER = CSID
        ICNT(II) = ICNT(II) + 1
      
C       DETERMINE GENERAL CARRIER LABEL (AIRCRAFT ONLY)
      
        IF (II.EQ.1)    THEN
          IF (CHARID(1).LT.IA.OR.CHARID(1).GT.IZ)      GO TO 1
          DO I=2,8
            IF (CHARID(I).LT.IA.OR.CHARID(I).GT.IZ)  CHARID(I) = IBLANK
            IF (CHARID(I-1).EQ.IBLANK)               CHARID(I) = IBLANK
          END DO  
        ENDIF
         
C       MATCH UP CARRIERS ID TO ARRAY ...
C       IF NO MATCH, ASSIGN NEW (NEXT) ARRAY VALUE
        
        DO I=1,5000
          IF (ACID(I).EQ.CARRER.OR.ACID(I).EQ.BLANK)    THEN
            IF (ACID(I).EQ.BLANK)   ACID(I) = CARRER
            OK = .TRUE.
            IF (WQM.GT.3.0)    THEN
              NREJ(I) = NREJ(I) + 1
C             OK = .FALSE.
            ENDIF
            IF (DVECT2.GE.40.)    THEN 
              IF (DVECT2.LT.5000.)    THEN
                NGROSS(I) = NGROSS(I) + 1
              ENDIF
              OK = .FALSE.
            ENDIF
            IF (OK)    THEN 
              KOUNT(I) = KOUNT(I) + 1
              SUMDV2(I) = SUMDV2(I) + DV2
              ASBIAS(I) = ASBIAS(I) + DS
            ENDIF
            OKT = .TRUE.
            IF (TQM.GT.3.0)    THEN
              KREJ(I) = KREJ(I) + 1
C             OKT = .FALSE.
            ENDIF
            IF (DT.GE.10.)    THEN 
              IF (DT.LT.5000.)    THEN
                KGROSS(I) = KGROSS(I) + 1
              ENDIF
              OKT = .FALSE.
            ENDIF
            IF (OKT)    THEN 
              KTEMP(I) = KTEMP(I) + 1
              ASMDT2(I) = ASMDT2(I) + DT*DT
              ATBIAS(I) = ATBIAS(I) + DT
            ENDIF
            GO TO 4 
          ENDIF
        END DO  
        WRITE (IPRT,104)
  104   FORMAT
     &  (' ACHTUNG!! UNIQUE CARRIERS EXCEEDS ARRAY SIZE!')
        DO  I=1,5000
          WRITE (IPRT,105) I, ACID(I)
  105     FORMAT (1X, I4, 2X, A8)
        END DO
        STOP 104
    4   CONTINUE
        
C       COUNT, SUMDU, SUMDV FOR LAT-LON BOXES (5-DEG SQUARES)
        
        IF (OK)    THEN
          NCOUNT(ILAT,JLON) = NCOUNT(ILAT,JLON) + 1
          SUMDU(ILAT,JLON) = SUMDU(ILAT,JLON) + DU
          SUMDV(ILAT,JLON) = SUMDV(ILAT,JLON) + DV
          SBIAS(ILAT,JLON) = SBIAS(ILAT,JLON) + DS
        ENDIF
        
        IF (OKT)    THEN
          NTEMPS(ILAT,JLON) = NTEMPS(ILAT,JLON) + 1
          SUMDT2(ILAT,JLON) = SUMDT2(ILAT,JLON) + DT*DT
          TBIAS(ILAT,JLON) = TBIAS(ILAT,JLON) + DT
        ENDIF
        
C       CYCLE COUNT ....
        
        IF (IDATE.NE.LDATE)    THEN
          NCYC = NCYC + 1
          LDATE = IDATE
        ENDIF
        
        JCNT(II) = JCNT(II) + 1
     
      GO TO 1
      
C     COME HERE WHEN LAST DATE DONE OR EOF
      
   50 CONTINUE
      
C     BOX SCORES .... NCOUNT BECOMES AVG DAILY COUNT
C     (1/2 ADDED TO ALLOW FOR TRUNCATION)
      
      DO 55 I=1,36
        DO 54 J=1,72
          X = NCOUNT(I,J)
          IF (X.GT.0.)    THEN
            NCOUNT(I,J) = (4*NCOUNT(I,J)+NCYC/2)/NCYC
          ELSE
            NCOUNT(I,J) = 5000.
          ENDIF
          IF (X.GT.9.)    THEN
            SUMDU(I,J) = SUMDU(I,J)/X
            SUMDV(I,J) = SUMDV(I,J)/X
            SBIAS(I,J) = SBIAS(I,J)/X
          ELSE
            SUMDU(I,J) = 5000.
            SUMDV(I,J) = 5000.
            SBIAS(I,J) = 5000.
          ENDIF
          Y = NTEMPS(I,J)
          IF (Y.GT.0.)    THEN
            NTEMPS(I,J) = (4*NTEMPS(I,J)+NCYC/2)/NCYC
          ELSE
            NTEMPS(I,J) = 5000.
          ENDIF
          IF (Y.GT.9.)    THEN
            RMST(I,J) = SQRT(SUMDT2(I,J)/Y)
            TBIAS(I,J)  = TBIAS(I,J)/Y
          ELSE
            RMST(I,J) = 5000.
            TBIAS(I,J)  = 5000.
          ENDIF
   54   CONTINUE
   55 CONTINUE
      
C     CALC RMSV STATS FOR INDIVIDUAL CARRIERS (N.GE.10)
      
      DO 60 I=1,5000
        IF (ACID(I).EQ.BLANK)    GO TO 70
        IF (KOUNT(I).GE.10)      THEN    
          XCOUNT = KOUNT(I)
          ASBIAS(I) = ASBIAS(I)/XCOUNT
          VARVE  = SUMDV2(I)/XCOUNT
          RMSV(I) = SQRT(VARVE)
        ENDIF
        IF (KTEMP(I).GE.10)      THEN    
          XCOUNT = KTEMP(I)
          ATBIAS(I) = ATBIAS(I)/XCOUNT
          VARTE  = ASMDT2(I)/XCOUNT
          ARMST(I) = SQRT(VARTE)
        ELSE
          ATBIAS(I) = 0.
        ENDIF
   60 CONTINUE
   70 CONTINUE
      
C     PRINT AND WRITE STATS HERE
      
      N = I-1  
      WRITE (IPRT,210) INDATE, ENDATE, N, IACTYP(II)
  210 FORMAT 
     & (4X,' STATS FROM', I12, '  TO', I12, '  FOR', I4,A8, 'CARRIERS')
C     WRITE (IPRT,211)
C 211 FORMAT (//, 4X, 'CARRIER', 5X, 'BIAS', 5X, 'RMSV (MPS)',
C    &  5X, '# OBS', 4X, '# GROSS', 4X, '# REJ')
      WRITE (IPRT,212)
      WRITE (IPRT,212)
  212 FORMAT ('   ')
      DO I=1,N
        IF (KOUNT(I).GE.10)    THEN    
          WRITE (IPRT,213) ACID(I), ASBIAS(I), RMSV(I), KOUNT(I), 
     &                     NGROSS(I), NREJ(I), ATBIAS(I), ARMST(I),
     &                     KTEMP(I), KGROSS(I), KREJ(I)
  213     FORMAT (1X, A8, 2X, F6.1, 2X, F6.1, 2X, I5, 1X, I4, 1X, I4,
     &            5X, F6.1, 2X, F6.1, 2X, I5, 1X, I4, 1X, I4)
        ENDIF
      END DO   
      
C     NOW WRITE BOX SCORES TO FILE: ONE RECORD OF IDENTIFIERS 
C     FOLLOWED BY ONE RECORD OF DATA.  THIS SCHEME PERMITS SAT
C     WIND AND TEMP BOX SCORES TO BE STORED ON SAME FILE, WHICH
C     WILL BE USED AS INPUT TO ANY FUTURE GRAPHICS PROGRAM.
      
      INST = IAC(II)
      IDIM = 36
      JDIM = 72
      ILEV =  0
      IRET =  0
      ISTAT = 0 
      
C     IDIM AND JDIM ARE DIMENSIONS OF ARRAY (5-DEGREE SQUARES HERE)
C     ILEV REFERS TO P-LEVEL BAND (0 INDICATES NOT APPLICABLE)
C     INST REFERS TO INSTRUMENT (1=AIRCRAFT 2=SATWND 3=SATEMS 4=SATRAD)
C                               (5=ASDAR    6=ACARS  7=UNASSN 8=UNASSN)
C     IRET REFERS TO SAT RETRIEVAL (0=NA 1=CLEAR 2=PC 3=MICROWAVE)
C     ISTAT REFERS TO TYPE OF STATISTIC (0=COUNT 1=UBIAS 2=VBIAS)
C                          (3=SPEED BIAS 4=TEMP BIAS, 5=RMS TEMP)       
      
      WRITE (ITOUT) INDATE, ENDATE, IDIM, JDIM, ILEV, INST, IRET, ISTAT
      WRITE (ITOUT) NCOUNT
      ISTAT = 1
      WRITE (ITOUT) INDATE, ENDATE, IDIM, JDIM, ILEV, INST, IRET, ISTAT
      WRITE (ITOUT) SUMDU
      ISTAT = 2
      WRITE (ITOUT) INDATE, ENDATE, IDIM, JDIM, ILEV, INST, IRET, ISTAT
      WRITE (ITOUT) SUMDV
      ISTAT = 3
      WRITE (ITOUT) INDATE, ENDATE, IDIM, JDIM, ILEV, INST, IRET, ISTAT
      WRITE (ITOUT) SBIAS
      ISTAT = 4
      WRITE (ITOUT) INDATE, ENDATE, IDIM, JDIM, ILEV, INST, IRET, ISTAT
      WRITE (ITOUT) TBIAS
      ISTAT = 5
      WRITE (ITOUT) INDATE, ENDATE, IDIM, JDIM, ILEV, INST, IRET, ISTAT
      WRITE (ITOUT) RMST 
      
   90 CONTINUE
      
      PRINT 190, ICNT, JCNT
  190 FORMAT (' ICNT = ', 3I8, 5X, 'JCNT = ', 3I8)
      STOP
      END
