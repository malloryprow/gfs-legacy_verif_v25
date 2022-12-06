C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C
C MAIN PROGRAM: vstats
C   PRGMMR: LILLY            ORG: NP12        DATE: 2009-06-01
C
C ABSTRACT: EXTRACTS RAOB VECTOR STATS AND PRINTS WORST OFFENDERS
C     IN QC MONITORING REPORT FORMAT.
C
C PROGRAM HISTORY LOG:
C
C 1998-07-24 C. L. VLCEK     -- ORIGINAL AUTHOR
C 2008-06-01 Steven G. Lilly -- add tmplates for Documentation block
C
C USAGE:
C   INPUT FILES:
C     FT07FT001   -  READS THE RAW MONTHLY STATISTICS OF RADIOSONDE WINDS
C
C   OUTPUT FILES:
C     FT06FT001   -  GENERATES MONTHLY STATISTICS OF RADIOSONDE OBSERVATION OF WIND
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

      PROGRAM VSTATS

C     EXTRACTS RAOB VECTOR STATS AND PRINTS WORST OFFENDERS
C     IN QC MONITORING REPORT FORMAT.
  
C     08-26-1998: COMPILE IN F90, READ NDATE AS YYYYMM
C     08-09-1999: COMPILE ON RS6000.
              
      REAL(4)  UBIAS, VBIAS, RMSV, UBIAS1, VBIAS1, RMSV1
      INTEGER(4)  ISTN, NSTN, IUTC, IP, NDV, NG, IN, IX, IZ(2)
      INTEGER(4)  IP1, NDV1, NG1
      CHARACTER*5  IDSTN, STATI, BEGIN, ID
      
      DATA  IZ /00, 12/, IX /0/, IN /7/ 
      DATA  IDSTN /'IDSTN'/, STATI /'STATI'/, BEGIN /' BEGI'/
      
      
C     READ IN DATA; READ NDATE AS YYYYMM (SKIP DDHH PORTION)
      
    1 CONTINUE
        READ (IN,101,END=50)  ID
  101   FORMAT (1X, A5,/)
        IF (ID.EQ.BEGIN)    THEN
          READ (IN,102) NYEAR,NMONTH 
  102     FORMAT (7X,I4,I2)
          IF (NMONTH.EQ.1)   THEN
          PRINT 300, NYEAR        
  300     FORMAT (12X, 'RAWINSONDE QUALITY MONITORING -- NCEP, ',
     &    'WASHINGTON', //, 26X, 'WIND', 7X,'JANUARY  ',I4, ///,
     &    ' STATION  UTC    LEVEL  # OBS  # GROSS  UBIAS',
     &    '    VBIAS      RMS  # LEV >', //)
        ENDIF
          IF (NMONTH.EQ.2)   THEN
          PRINT 301, NYEAR
  301     FORMAT (12X, 'RAWINSONDE QUALITY MONITORING -- NCEP, ',
     &    'WASHINGTON', //, 26X, 'WIND', 7X,'FEBRUARY  ',I4, ///,
     &    ' STATION  UTC    LEVEL  # OBS  # GROSS  UBIAS',
     &    '    VBIAS      RMS  # LEV >', //)
        ENDIF
          IF (NMONTH.EQ.3)   THEN
          PRINT 302, NYEAR
  302     FORMAT (12X, 'RAWINSONDE QUALITY MONITORING -- NCEP, ',
     &    'WASHINGTON', //, 26X, 'WIND', 7X,'MARCH  ',I4, ///,
     &    ' STATION  UTC    LEVEL  # OBS  # GROSS  UBIAS',
     &    '    VBIAS      RMS  # LEV >', //)
        ENDIF
          IF (NMONTH.EQ.4)   THEN
          PRINT 303, NYEAR
  303     FORMAT (12X, 'RAWINSONDE QUALITY MONITORING -- NCEP, ',
     &    'WASHINGTON', //, 26X, 'WIND', 7X,'APRIL  ',I4, ///,
     &    ' STATION  UTC    LEVEL  # OBS  # GROSS  UBIAS',
     &    '    VBIAS      RMS  # LEV >', //)
        ENDIF
          IF (NMONTH.EQ.5)   THEN
          PRINT 304, NYEAR
  304     FORMAT (12X, 'RAWINSONDE QUALITY MONITORING -- NCEP, ',
     &    'WASHINGTON', //, 26X, 'WIND', 7X,'MAY  ',I4, ///,
     &    ' STATION  UTC    LEVEL  # OBS  # GROSS  UBIAS',
     &    '    VBIAS      RMS  # LEV >', //)
        ENDIF
          IF (NMONTH.EQ.6)   THEN
          PRINT 305, NYEAR
  305     FORMAT (12X, 'RAWINSONDE QUALITY MONITORING -- NCEP, ',
     &    'WASHINGTON', //, 26X, 'WIND', 7X,'JUNE  ',I4, ///,
     &    ' STATION  UTC    LEVEL  # OBS  # GROSS  UBIAS',
     &    '    VBIAS      RMS  # LEV >', //)
        ENDIF
          IF (NMONTH.EQ.7)   THEN
          PRINT 306, NYEAR
  306     FORMAT (12X, 'RAWINSONDE QUALITY MONITORING -- NCEP, ',
     &    'WASHINGTON', //, 26X, 'WIND', 7X,'JULY  ',I4, ///,
     &    ' STATION  UTC    LEVEL  # OBS  # GROSS  UBIAS',
     &    '    VBIAS      RMS  # LEV >', //)
        ENDIF
          IF (NMONTH.EQ.8)   THEN
          PRINT 307, NYEAR
  307     FORMAT (12X, 'RAWINSONDE QUALITY MONITORING -- NCEP, ',
     &    'WASHINGTON', //, 26X, 'WIND', 7X,'AUGUST  ',I4, ///,
     &    ' STATION  UTC    LEVEL  # OBS  # GROSS  UBIAS',
     &    '    VBIAS      RMS  # LEV >', //)
        ENDIF
          IF (NMONTH.EQ.9)   THEN
          PRINT 308, NYEAR
  308     FORMAT (12X, 'RAWINSONDE QUALITY MONITORING -- NCEP, ',
     &    'WASHINGTON', //, 26X, 'WIND', 7X,'SEPTEMBER  ',I4, ///,
     &    ' STATION  UTC    LEVEL  # OBS  # GROSS  UBIAS',
     &    '    VBIAS      RMS  # LEV >', //)
        ENDIF
          IF (NMONTH.EQ.10)   THEN
          PRINT 309, NYEAR
  309     FORMAT (12X, 'RAWINSONDE QUALITY MONITORING -- NCEP, ',
     &    'WASHINGTON', //, 26X, 'WIND', 7X,'OCTOBER  ',I4, ///,
     &    ' STATION  UTC    LEVEL  # OBS  # GROSS  UBIAS',
     &    '    VBIAS      RMS  # LEV >', //)
        ENDIF
          IF (NMONTH.EQ.11)   THEN
          PRINT 310, NYEAR
  310     FORMAT (12X, 'RAWINSONDE QUALITY MONITORING -- NCEP, ',
     &    'WASHINGTON', //, 26X, 'WIND', 7X,'NOVEMBER  ',I4, ///,
     &    ' STATION  UTC    LEVEL  # OBS  # GROSS  UBIAS',
     &    '    VBIAS      RMS  # LEV >', //)
        ENDIF
          IF (NMONTH.EQ.12)   THEN
          PRINT 311, NYEAR
  311     FORMAT (12X, 'RAWINSONDE QUALITY MONITORING -- NCEP, ',
     &    'WASHINGTON', //, 26X, 'WIND', 7X,'DECEMBER  ',I4, ///,
     &    ' STATION  UTC    LEVEL  # OBS  # GROSS  UBIAS',
     &    '    VBIAS      RMS  # LEV >', //)
        ENDIF
        ENDIF
        IF (ID.EQ.STATI)    THEN
          IX = IX + 1
          IUTC = IZ(IX)
        ENDIF
      IF (ID.NE.IDSTN)    GO TO 1
     
      N = 0
      NLEV = 0
      RMSV1 = 0.
    2 CONTINUE
        READ (IN,103)  ISTN, IP, NDV, NG, UBIAS, VBIAS, RMSV
  103   FORMAT (1X, I5, I7, 28X, 2I7, 3F7.2)
        IF (ISTN.NE.0)    NSTN = ISTN
        IF (IP.EQ.0)      GO TO 2
        N = N + 1
        IF (RMSV.GT.14.5.AND.NDV.GT.9)    THEN
          NLEV = NLEV + 1
          IF (RMSV.GT.RMSV1)    THEN
            IP1 = IP
            NG1 = NG
            NDV1 = NDV
            UBIAS1 = UBIAS
            VBIAS1 = VBIAS
            RMSV1 = RMSV
          ENDIF
        ENDIF
      IF (IP.EQ.50)    THEN
        IF (NLEV.GT.0)    THEN
          PRINT 201, NSTN,IUTC,IP1,NDV1,NG1,UBIAS1,VBIAS1,RMSV1,NLEV
  201     FORMAT (1X, I5, I7, 2I8, I7, 3F9.1, I9)
        ENDIF
        GO TO 1
      ELSE
        GO TO 2
      ENDIF
     
   50 CONTINUE
      
      STOP
      END
