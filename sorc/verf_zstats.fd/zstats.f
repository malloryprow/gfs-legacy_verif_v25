C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C
C MAIN PROGRAM: wsum
C   PRGMMR: LILLY            ORG: NP12        DATE: 2009-03-09
C
C ABSTRACT:  EXTRACTS RAOB HEIGHT STATS AND PRINTS WORST OFFENDERS
C            IN QC MONITORING REPORT FORMAT.
C
C PROGRAM HISTORY LOG:
C   1998-08-28  Original Author: Charles Vlcek
C
C USAGE:
C   INPUT FILES:
C    FT07F001 - READS DATA
C
C   OUTPUT FILES:
C    FT60F001 - PRINTS RAWDINSONDE QUALITY STATISTICS
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

C     PROGRAM ZSTATS

            
      REAL(8)  R8, Z8
      REAL(4)  ZBIAS, RMSZ, XRMS, XRMS1, W(11)
      INTEGER(4)  ISTN, NSTN, IUTC, IP, NDZ, NG, IN, IX, IZ(2)
      INTEGER(4)  IBIAS, IRMSZ, IP1, NDZ1, NG1
      CHARACTER*5  IDSTN, STATI, BEGIN, ID
      
      DATA  W /3.70, 3.40, 2.90, 2.20, 1.90, 1.60, 1.50, 1.37,
     &         1.19, 1.00, 0.80/            
      DATA  IZ /00, 12/, IX /0/, IN /7/ 
      DATA  IDSTN /'IDSTN'/, STATI /'STATI'/, BEGIN /' BEGI'/
      
      
C     READ IN DATA
      
    1 CONTINUE
        READ (IN,101,END=50)  ID
  101   FORMAT (1X, A5,/)
        IF (ID.EQ.BEGIN)    THEN
          READ (IN,102)  NYEAR,NMONTH
  102     FORMAT (7X,I4,I2)
          IF (NMONTH.EQ.1)   THEN
          PRINT 300, NYEAR
  300     FORMAT (8X, 'RAWINSONDE QUALITY MONITORING -- NCEP, ',
     &    'WASHINGTON', //, 18X, 'GEOPOTENTIAL',5X,'JANUARY  ',I4, ///,
     &    ' STATION  UTC   LEVEL  # OBS   # GROSS   BIAS',
     &    '      RMS  # LEV >', //)
        ENDIF
          IF (NMONTH.EQ.2)   THEN
          PRINT 301, NYEAR
  301     FORMAT (8X, 'RAWINSONDE QUALITY MONITORING -- NCEP, ',
     &    'WASHINGTON', //, 18X, 'GEOPOTENTIAL',5X,'FEBRUARY  ',I4, ///,
     &    ' STATION  UTC   LEVEL  # OBS   # GROSS   BIAS',
     &    '      RMS  # LEV >', //)
        ENDIF                                                   
          IF (NMONTH.EQ.3)   THEN
          PRINT 302, NYEAR
  302     FORMAT (8X, 'RAWINSONDE QUALITY MONITORING -- NCEP, ',
     &    'WASHINGTON', //, 18X, 'GEOPOTENTIAL',5X,'MARCH  ',I4, ///,
     &    ' STATION  UTC   LEVEL  # OBS   # GROSS   BIAS',
     &    '      RMS  # LEV >', //)
        ENDIF                                                   
          IF (NMONTH.EQ.4)   THEN
          PRINT 303, NYEAR
  303     FORMAT (8X, 'RAWINSONDE QUALITY MONITORING -- NCEP, ',
     &    'WASHINGTON', //, 18X, 'GEOPOTENTIAL',5X,'APRIL  ',I4, ///,
     &    ' STATION  UTC   LEVEL  # OBS   # GROSS   BIAS',
     &    '      RMS  # LEV >', //)
        ENDIF                                                   
          IF (NMONTH.EQ.5)   THEN
          PRINT 304, NYEAR
  304     FORMAT (8X, 'RAWINSONDE QUALITY MONITORING -- NCEP, ',
     &    'WASHINGTON', //, 18X, 'GEOPOTENTIAL',5X,'MAY  ',I4, ///,
     &    ' STATION  UTC   LEVEL  # OBS   # GROSS   BIAS',
     &    '      RMS  # LEV >', //)
        ENDIF                                                   
          IF (NMONTH.EQ.6)   THEN
          PRINT 305, NYEAR
  305     FORMAT (8X, 'RAWINSONDE QUALITY MONITORING -- NCEP, ',
     &    'WASHINGTON', //, 18X, 'GEOPOTENTIAL',5X,'JUNE  ',I4, ///,
     &    ' STATION  UTC   LEVEL  # OBS   # GROSS   BIAS',
     &    '      RMS  # LEV >', //)
        ENDIF                                                   
          IF (NMONTH.EQ.7)   THEN
          PRINT 306, NYEAR
  306     FORMAT (8X, 'RAWINSONDE QUALITY MONITORING -- NCEP, ',
     &    'WASHINGTON', //, 18X, 'GEOPOTENTIAL',5X,'JULY  ',I4, ///,
     &    ' STATION  UTC   LEVEL  # OBS   # GROSS   BIAS',
     &    '      RMS  # LEV >', //)
        ENDIF                                                   
          IF (NMONTH.EQ.8)   THEN
          PRINT 307, NYEAR
  307     FORMAT (8X, 'RAWINSONDE QUALITY MONITORING -- NCEP, ',
     &    'WASHINGTON', //, 18X, 'GEOPOTENTIAL',5X,'AUGUST  ',I4, ///,
     &    ' STATION  UTC   LEVEL  # OBS   # GROSS   BIAS',
     &    '      RMS  # LEV >', //)
        ENDIF                                                   
          IF (NMONTH.EQ.9)   THEN
          PRINT 308, NYEAR
  308     FORMAT (8X, 'RAWINSONDE QUALITY MONITORING -- NCEP, ',
     &    'WASHINGTON', //, 18X, 'GEOPOTENTIAL',5X,'SEPTEMBER  ',I4, ///,
     &    ' STATION  UTC   LEVEL  # OBS   # GROSS   BIAS',
     &    '      RMS  # LEV >', //)
        ENDIF                                                   
          IF (NMONTH.EQ.10)   THEN
          PRINT 309, NYEAR
  309     FORMAT (8X, 'RAWINSONDE QUALITY MONITORING -- NCEP, ',
     &    'WASHINGTON', //, 18X, 'GEOPOTENTIAL',5X,'OCTOBER  ',I4, ///,
     &    ' STATION  UTC   LEVEL  # OBS   # GROSS   BIAS',
     &    '      RMS  # LEV >', //)
        ENDIF                                                   
          IF (NMONTH.EQ.11)   THEN
          PRINT 310, NYEAR
  310     FORMAT (8X, 'RAWINSONDE QUALITY MONITORING -- NCEP, ',
     &    'WASHINGTON', //, 18X, 'GEOPOTENTIAL',5X,'NOVEMBER  ',I4, ///,
     &    ' STATION  UTC   LEVEL  # OBS   # GROSS   BIAS',
     &    '      RMS  # LEV >', //)
        ENDIF                                                   
          IF (NMONTH.EQ.12)   THEN
          PRINT 311, NYEAR
  311     FORMAT (8X, 'RAWINSONDE QUALITY MONITORING -- NCEP, ',
     &    'WASHINGTON', //, 18X, 'GEOPOTENTIAL',5X,'DECEMBER  ',I4, ///,
     &    ' STATION  UTC   LEVEL  # OBS   # GROSS   BIAS',
     &    '      RMS  # LEV >', //)
        ENDIF                                                   
        ENDIF
        IF (ID.EQ.STATI)    THEN
          IX = IX + 1
          IUTC = IZ(IX)
        ENDIF
      IF (ID.NE.IDSTN)    GO TO 1
     
      N = 0
      NLEV = 0
      XRMS1 = 0.
    2 CONTINUE
        READ (IN,103)  ISTN, IP, NDZ, NG, ZBIAS, RMSZ
  103   FORMAT (1X, I5, 3I7, 2F7.2)
        IF (ISTN.NE.0)    NSTN = ISTN
        IF (IP.EQ.0)      GO TO 2
        N = N + 1
        XRMS = W(N)*RMSZ 
        IF (XRMS.GT.100..AND.NDZ.GT.9)    THEN
          NLEV = NLEV + 1
          IF (XRMS.GT.XRMS1)    THEN
            XRMS1 = XRMS
            IP1 = IP
            NG1 = NG
            NDZ1 = NDZ
            R8 = RMSZ
            Z8 = ZBIAS
            IBIAS = IFIX(Z8)
            IRMSZ = IFIX(R8)
          ENDIF
        ENDIF
      IF (IP.EQ.50)    THEN
        IF (NLEV.GT.0)    THEN
          PRINT 201, NSTN, IUTC, IP1, NDZ1, NG1, IBIAS, IRMSZ, NLEV
  201     FORMAT (1X, I5, I7, I8, I7, I8, 3I9)
        ENDIF
        GO TO 1
      ELSE
        GO TO 2
      ENDIF
     
   50 CONTINUE
      
      STOP
      END
