C$$$  MAIN PROGRAM DOCUMENTATION BLOCK                                          
C                .      .    .                                       .          
C MAIN PROGRAM:  SELECSUM    SELECT SUMAC DATA AND COPY/PRINT/INVEN             
C   PRGMMR: CHARLES L. VLCEK ORG: W/NP12      DATE: 99-05-21                    
C                                                                               
C ABSTRACT: SELECTS DATA FROM SEQUENTIAL SUMAC VERIFICATION STATISTICS          
C   FILE AND COPIES TO ANOTHER SEQUENTIAL FILE, PRINTS OR INVENTORIES           
C   THE DATA, COMPUTES AVERAGES (WITH SUBSEQUENT WRITE/PRINT/INVEN),            
C   OR GENERATES TIME-SERIES RECORDS OF SELECTED DATA.  SELECTIONS              
C   MAY BE MADE BY VERIFICATION DATE, FORECAST MODEL, FORECAST HOUR,            
C   VERIFICATION AREA, INDIVIDUAL STATISTICS (FOR TIME-SERIES), OR              
C   ANY COMBINATION THEREOF.  SELECSUM DEALS WITH 4-LEVEL VERIFI-               
C   CATIONS AT 850, 500, 250, AND 100 MB; SEE PROGRAM SELEC1 FOR                
C   WORKING WITH SINGLE-LEVEL VERIFICATIONS (AT 500 MB).                        
C                                                                               
C PROGRAM HISTORY LOG:                                                          
C   81-??-??  VLCEK                                                             
C   87-10-22  VLCEK       SEVERAL CHANGES: 1) INCLUDE OPTION FOR                
C                         READING ORIGINAL OUTPUT FILE (FT15) AND               
C                         WRITING TO A NEW OUTPUT FILE (FT25 --                 
C                         THIS OPTION CAN BE REPEATED); 2) DISABLE              
C                         THE COPY OPTION WHILE AVERAGING IS IN                 
C                         PROGRESS; AND 3) PERMIT SIMULTANEOUS                  
C                         AVERAGING IN EACH SEPARATE VERIFICATION               
C                         AREA IF 'BLANK' AREA IS SELECTED FOR                  
C                         AVERAGING.                                            
C   87-11-??  VLCEK       MODIFY TIME-SERIES DATE IDENTIFIER TO                 
C                         PERMIT IDENTIFICATION OF VERIFICATION                 
C                         TIME UNITS THAT ARE NEITHER DAILY NOR                 
C                         MONTHLY.                                              
C   88-03-??  VLCEK       DISABLE DATE SELECTION IF SELECTED                    
C                         BEGINNING AND ENDING DATES ARE BOTH ZERO.             
C   88-10-05  VLCEK       REQUIRE 'ACTIVE' SELECTION OF NON-                    
C                         STANDARD TIME UNITS FOR BUILDING TIME-                
C                         SERIES RECORDS (I.E., NEITHER DAILY NOR               
C                         MONTHLY VERIFICATION DATA).                           
C   89-01-12  VLCEK       CALL NEW SUBROUTINE TO TAKE BAD-DATA                  
C                         'SPIKES' OUT OF TIME-SERIES ARRAY BEFORE              
C                         WRITING RECORD TO FT20 (OPTIONAL).                    
C   89-03-20  VLCEK       AVOID ENDFILING FT15, FT25, FT30, ETC.                
C                         FILES IF NO NEW RECORDS WERE WRITTEN.                 
C   90-03-26  VLCEK       MODIFY FOR RECOMPILATION IN VS FORTRAN 77.            
C   90-07-19  VLCEK       INCLUDE OPTION TO SKIP RECORDS.                       
C   90-11-29  VLCEK       CORRECT AND REFINE QUALITY CONTROL OF                 
C                         DATA USED IN AVERAGING PROCESS.                       
C   92-02-05  VLCEK       EXPAND 'SUPER-INVEN' OPTION TO SELECT LINE            
C                         OF DATA TO BE PRINTED (500 MB HEIGHT STATS            
C                         CONTINUES TO BE DEFAULT).                             
C   94-08-10  VLCEK       INCREASE DIMENSION FOR 'AVG' AND 'AVAREA'             
C                         FROM 7 TO 10. (TO 15 96-12-20)                                         
C   96-03-20  Y.Z. ZHANG  CONVERT CODE FROM HDS TO CRAY MACHINE.
C                         MODIFY INCOMPATIBLE LANGUAGE PARTS, 
C                         SUCH AS SEPARATING THE FUNCTIONS OF 
C                         SPECIFICATION FROM ASSIGNING INITIAL VALUE.
C                         BRING SUBROUTINE 'DSPIKE' INTO THIS CODE.
C                         CHANGE INPUT VARIABLE LIST, CHANGE THE
C                         'CALL W3FS11' TO 'CALL SW3FS1', RECODE
C                         'SW3FS1' AS REQUIRED, AND REFINE THE CODE.
C   96-12-20  VLCEK       SELECT MODELS BY FIRST THREE CHARS
C   97-10-09  VLCEK       MODIFY 'SUPER-INVEN' PRINT FORMAT, OPTIONS
C   98-07-16  VLCEK       MODIFY TIME-SERIES PRINT FORMAT, MAKE IT
C                         Y2K COMPATIBLE.
C   99-05-21  VLCEK       CONVERT FOR USE ON IBM RS6000.
C   09-10-01  LILLY       CONVERT FOR USE ON P6.
C                                                                               
C                                                                               
C USAGE:                                                                        
C   INPUT FILES:                                                                
C     FT05F001 - CARD FILE TO SPECIFY OPTIONS TO BE USED AND RECORDS            
C              - TO BE SELECTED FROM FT10, FT15, ETC.                           
C     FT10F001 - READ-ONLY FILE OF STANDARD SEQUENTIAL RECORDS OF               
C              - 4-LEVEL SUMAC VERIFICATION DATA; EACH RECORD CON-              
C              - TAINS S1, MEAN ERROR, S.D. ERROR, RMS ERROR, MEAN              
C              - AND S.D. OF OBSERVED Z, T, RH, SPEED, AND VECTOR               
C              - AT EACH LEVEL FOR ONE FORECAST MODEL, HOUR, AREA,              
C              - AND VERIFICATION TIME UNIT (ONE OBS., ONE MONTH, ETC).         
C              - ALSO CONTAINS IDENTIFIERS FOR RECORD.                          
C     FT15F001 - SAME AS ABOVE (ALSO FT25, FT30, FT35, ETC.),                   
C              - BUT MAY ALSO BE USED AS OUTPUT (SEE BELOW).                    
C     FT20F001 - NORMALLY AN OUTPUT FILE (SEE BELOW) BUT MAY                    
C              - READ BACK TO PRINT CONTENTS (INVEN).                           
C                                                                               
C   OUTPUT FILES:                                                               
C     FT06F001 - PRINTOUT OF VERIFICATION STATISTICS, INVEN,                    
C              - OR TIME-SERIES RECORDS; ALSO PRINTS BACK OPTIONS               
C              - AND DATA SELECTED FOR DIAGNOSTIC PURPOSES, AND                 
C              - CERTAIN ERROR/WARNING MESSAGES.                                
C     FT15F001 - (ALSO FT25, FT30, FT35, ETC.) SEE INPUT; CONTAINS              
C              - DATA COPIED OR AVERAGED FROM NEXT HIGHER FT #                  
C              - (EXCEPT FT20).                                                 
C     FT20F001 - SET OF RECORD IDENTIFIERS AND TIME-SERIES RECORDS              
C              - (SINGLE ARRAY OF LENGTH 500, 'MISSING' FILL) OF                
C              - OF ANY ONE VERIFICATION STATISTIC PER RECORD IN                
C              - SEQUENTIAL FILE.                                               
C                                                                               
C                                                                               
C   SUBPROGRAMS CALLED:                                                         
C     UNIQUE:    - DSPIKE                                                       
C     LIBRARY:     (NONE)                                                       
C                                                                               
C   EXIT STATES:                                                                
C     COND =   0 - SUCCESSFUL RUN                                               
C          =  88 - ERROR READING BACK FT20                                      
C                                                                               
C REMARKS: SEE C. VLCEK FOR DOCUMENTATION AND INSTRUCTIONS.
C                                                                               
C ATTRIBUTES:                                                                   
C   LANGUAGE: FORTRAN 90 , IBM FORTRAN
C   MACHINE:  IBM RS6000
C                                                                               
C$$$                                                                            
                                                                               
      COMMON  /INFO/  XSTAT(500), MSG, FACTOR                                   
                                                                               
      REAL(4) STAT(10,500), PAGE(6,20), RMSA(20), RMSC(20)          
      REAL(4) XSTAT, LIST(128), GCODE, KGRID                                    
      REAL(4) MSG, FACTOR, GROSS, AVG(15,6,20)                      
                                                                               
      INTEGER(4) FDATE, LDATE, DATES(2), IDATE(2), NDATE, MDATE, NCEN
      INTEGER(4) KWRITE, CONT, IX(10), JX(10), OUT, ODD, NREC, IXX, JXX 
      INTEGER(4) JMONTH(12), KOUNT(15,6,20), NPREV, IN, INTVAL
      INTEGER(4) LDATE1(4), LFDATE(4), LDATE2(4), LLDATE(4), LFDTE              
      INTEGER(4) LNDATE(4), INVEN, LEVEL(4), ICEN, ICEN19
                                                                               
      CHARACTER(13) INVLAB(21)
      CHARACTER(8) DDNAME, INMOD, INAREA, IDSTAT(6), QLEVEL(5)                  
      CHARACTER(8) BLANK8, AVAREA(15), AREA, LABELS(5)             
      CHARACTER(8) MLABEL, ALABEL, ALL, INTLBL(3)                  
      CHARACTER(4) MONTHS(23), COLH(23), FH1(2), FH2(2), STORE                  
      CHARACTER(4) FNAL, ANAL, BLANK, PERN, PERS, PQ(20)          
      CHARACTER(4) F000, F006, F012, F024, F036, F048, F060                 
      CHARACTER(4) F072, F084, F096, F108, F120, F132, F144                     
      CHARACTER(4) F156, F168, F180, F192, F204, F216                
      CHARACTER(4) F228, F240             
      CHARACTER(3) MDL, MOD3
      CHARACTER(1) VEE, EF, LETTER                                  
                                                                               
      EQUIVALENCE (FH1(1),INMOD), (FH2(1),DDNAME), (LETTER,DDNAME)              
      EQUIVALENCE (FH1(1),MDL), (FH2(1),MOD3)
                                                                               
      DATA  RMSA /50.0, 5.0, 60.0, 10.0, 10.0,                                  
     *            50.0, 4.0, 60.0, 10.0, 10.0,                                  
     *            50.0, 5.0, 60.0, 10.0, 10.0,                                  
     *            50.0, 5.0, 60.0, 10.0, 10.0/                                  
                                                                               
      DATA  RMSC /2.0, 0.07, 0.0, 0.10, 0.10,                                   
     *            3.0, 0.08, 0.0, 0.25, 0.25,                                   
     *            4.0, 0.07, 0.0, 0.30, 0.30,                                   
     *            3.0, 0.07, 0.0, 0.10, 0.10/                                   
                                                                               
      DATA  IDSTAT /'      S1', '    BIAS', '    S.D.',                         
     *              '     RMS', '   AVOBS', '   SDOBS'/                         
                                                                               
      DATA  QLEVEL /'MB HGT  ', 'MB TEMP.', 'MB R.H. ',                         
     *              'MB SPEED', 'MB VECT.'/                                     
                                                                               
      DATA  LABELS /'  HEIGHT', '   TEMP.', ' REL HUM',                         
     *              '   SPEED', '  VECTOR'/                                     
                                                                               
      DATA  INTLBL /'   DAILY', ' MONTHLY', 'INTERVL?'/                         
                                                                               
      DATA  MONTHS /' JAN', ' FEB', ' MAR', ' APR', ' MAY', ' JUN',             
     *              ' JUL', ' AUG', ' SEP', ' OCT', ' NOV', ' DEC',             
     *              ' JAN', ' FEB', ' MAR', ' APR', ' MAY', ' JUN',             
     *              ' JUL', ' AUG', ' SEP', ' OCT', ' NOV'/                     
                                                                               
      DATA  PQ  /'850Z', '850T', '850R', '850S', '850V',
     &           '500Z', '500T', '500R', '500S', '500V',
     &           '250Z', '250T', '250R', '250S', '250V',
     &           '100Z', '100T', '100R', '100S', '100V'/

      DATA  STAT /5000*999./, GROSS /1000000.0/
      DATA  LEVEL /850,500,250,100/, IX /10*0/, JX /10*0/, NREC /0/             
      DATA  IN /10/, OUT /15/, NPREV /1/, ODD /0/, INVEN /6/, NCEN /19/         
      DATA  JMONTH /0,31,59,90,120,151,181,212,243,273,304,334/        
                                                                               
      DATA INVLAB /'850 MB HEIGHT',  '850 MB TEMPS.',              
     &             '850 MB R.H.  ',  '850 MB SPEED ',              
     &             '850 MB VECTOR',                                
     &             '500 MB HEIGHT',  '500 MB TEMPS.',              
     &             '500 MB R.H.  ',  '500 MB SPEED ',              
     &             '500 MB VECTOR',                                
     &             '250 MB HEIGHT',  '250 MB TEMPS.',              
     &             '250 MB R.H.  ',  '250 MB SPEED ',              
     &             '250 MB VECTOR',                                
     &             '100 MB HEIGHT',  '100 MB TEMPS.',              
     &             '100 MB R.H.  ',  '100 MB SPEED ',              
     &             '100 MB VECTOR',  'ALL AVAILABLE'/                           
                                                                               
      DATA  BLANK8 /'        '/,  ALL /' (ALL)  '/                             
      DATA  FNAL /'FNAL'/, ANAL /'ANAL'/, BLANK /'    '/                 
      DATA  F000 /'F000'/, F006 /'F006'/, F012  /'F012'/                 
      DATA  F024 /'F024'/, F036 /'F036'/, F048  /'F048'/                 
      DATA  F060 /'F060'/, F072 /'F072'/, F084  /'F084'/                 
      DATA  F096 /'F096'/, F108 /'F108'/, F120  /'F120'/                 
      DATA  F132 /'F132'/, F144 /'F144'/, F156 /'F156'/                                 
      DATA  F168 /'F168'/, F180 /'F180'/, F192 /'F192'/                                 
      DATA  F204 /'F204'/, F216 /'F216'/, F228 /'F228'/                                 
      DATA  F240 /'F240'/                                 
      DATA  PERN /'PERN'/, PERS /'PERS'/                                 
      DATA  VEE /'V'/, EF /'F'/
                                                                               
      ICEN = 10**8
      ICEN19 = 19*ICEN
      MSG = 999.                                                              
      KWRITE = 0                                                              
      NWRITE = 0                                                              
                                                                               
   10 CONTINUE                                                                
                                                                               
C     INITIALIZATION OF VALUES....                                            
                                                                               
      AVAREA = BLANK8                                                   
      AVG = 0.                                                  
      KOUNT = 0                                                 
                                                                               
      IID = 0                                                                 
      ODD = 0                                                                 
      NTIME = 0                                                               
                                                                               
   15 CONTINUE                                                                
      READ 100, INMOD, INAREA, LFDATE, LLDATE, NPRINT, CONT                   
  100 FORMAT (8X,A8,8X,A8, 6X,I4,3I2, 6X,I4,3I2, 3X, I1, 3X, I1)               
     
C     CONVERT 2-DIGIT YEAR TO 4-DIGIT (20TH CEN ASSUMED)
     
      IF (LFDATE(1).LT.100)    LFDATE(1) = LFDATE(1) + 1900
      IF (LLDATE(1).LT.100)    LLDATE(1) = LLDATE(1) + 1900
                                                                               
C     RECORD SKIP OPTION: IF 'START HOUR' IS NEGATIVE, 'END DATE'             
C     CONTAINS NUMBER OF RECORDS TO BE SKIPPED.                               
                                                                               
      IF (LFDATE(4).GE.0)    GO TO 17                                         
      IF (CONT.GE.6)       REWIND IN                                        
      IF (CONT.GE.6)       NREC = 0                                         
      NSKIP = 10000*LLDATE(2) + 100*LLDATE(3) + LLDATE(4)                   
      DO 16 NS=1,NSKIP                                                      
      READ (IN)                                                           
      NREC = NREC + 1                                                     
   16 CONTINUE                                                              
      GO TO 15                                                              
   17 CONTINUE                                                                
                                                                               
C     INVEN OPTION: IF 'START HOUR' IS POSITIVE AND P=4, LINE                 
C     OF STATS (1-20) TO BE PRINTED WILL ASSUME VALUE GIVEN BY                
C     'START HOUR' (WHICH WILL THEN BE RESET TO ZERO).  OUT-OF-               
C     RANGE VALUE WILL FORCE PRINTING OF ALL 20 LINES.                        
                                                                               
      IF(NPRINT.EQ.4) THEN                                                
         INVEN = LFDATE(4)                                                     
         LFDATE(4) = 0                                                         
      ENDIF                                                                   
      IF(INVEN.EQ.0.OR.INVEN.GT.20) INVEN = 21                            
                                                                               
      CALL SW3FS1(FDATE,LFDATE(1),LFDATE(2),LFDATE(3),LFDATE(4),0)            
      CALL SW3FS1(LDATE,LLDATE(1),LLDATE(2),LLDATE(3),LLDATE(4),0)            
                                                                               
C     SAMPLE I/P CONTROL CARD FOLLOWS (IGNORE 'C' IN COLUMN 1)....              
C     SECOND CARD SHOWS NEW FORMAT (7/16/1998 COMPILATION)
                                                                               
C MODEL=YOURFCST   AREA=   NH102  FDATE=77090000  LDATE=82080000 P=1 C=0        
C MODEL=YOURFCST   AREA=   NH102  FD= 1977090000  LD= 1982080000 P=1 C=0        
                                                                               
      IF (CONT.GE.9)    GO TO 50                                              
      IF (CONT.NE.5)    GO TO 24                                              
      IF (NWRITE.NE.0)    END FILE OUT                                      
      IN = IN + 5                                                           
      IF (IN.EQ.20)   IN = 25                                               
      OUT = OUT + 5                                                         
      IF (OUT.EQ.20)  OUT = 25                                              
      REWIND IN                                                             
      NREC = 0                                                              
      NWRITE = 0                                                            
   24 CONTINUE                                                                
      FACTOR = NPRINT - 1                                                     
      FACTOR = FACTOR/2.0                                                     
      IF (NPRINT.GT.4)    NPRINT = 0                                          
      MLABEL = INMOD                                                          
      ALABEL = INAREA                                                         
      IF (MLABEL.EQ.BLANK8)    MLABEL = ALL                                   
      IF (ALABEL.EQ.BLANK8)    ALABEL = ALL                                   
      NP = NPRINT + NPREV                                                     
      NPREV = NPRINT                                                          
                                                                               
      IF (CONT.EQ.5)    CONT = 0                                              
      IF (CONT.LT.6)    GO TO 25                                              
      NREC = 0                                                              
      REWIND IN                                                             
      CONT = CONT-6                                                         
   25 CONTINUE                                                                
      NSTAT = 0                                                               
      NDATE = 0                                                               
      LFZ = LFDATE(4)                                                         
      LLZ = LLDATE(4)                                                         
      IF (CONT.NE.1)      GO TO 29                                            
      IX(1)=0                                                                
      IX(6)=0                                                                
      READ 101, (IX(KX),JX(KX),KX=1,5),CONT                                  
      IF (CONT.EQ.1) READ 101, (IX(KY),JX(KY),KY=6,10), CONT               
  101 FORMAT ( 5(2I4,4X), 11X, I1)                                           
                                                                               
C     SAMPLE I/P FOLLOWS (IGNORE 'C' IN COLUMN 1)....                           
C                                                                               
C  1  11       2  12       3  13       4  14       5  15             C=0        
                                                                               
      IF (IX(1).EQ.0)   GO TO 29                                             
      PRINT 201                                                              
  201 FORMAT (//, ' YOU HAVE REQUESTED THE FOLLOWING STATISTICS:')          
      DO 26 NSTAT=1,10                                                       
      IF (IX(NSTAT).LE.0.OR.IX(NSTAT).GT.6)    GO TO 27                     
      IF (JX(NSTAT).LE.0.OR.JX(NSTAT).GT.20)   GO TO 27                     
      L = (JX(NSTAT) + 4)/5                                                 
      M = (JX(NSTAT)) - 5*(L-1)                                             
      N =  IX(NSTAT)                                                        
      PRINT 202, IDSTAT(N), LEVEL(L), QLEVEL(M)                             
  202 FORMAT (10X, A8, I4, 1X, A8)                                     
      IF (IX(NSTAT).EQ.1.AND.M.GE.4)    IX(NSTAT)=9                         
      IF (IX(NSTAT).EQ.9)    PRINT 203                                      
  203 FORMAT (' THE ABOVE STATISTIC IS GARBAGE.  REQUEST IGNORED.')        
   26 CONTINUE                                                               
   27 CONTINUE                                                               
      NSTAT = NSTAT-1                                                        
   29 CONTINUE                                                                
                                                                              
      IF (CONT.NE.2.AND.CONT.NE.8)    GO TO 1130                              
      FHR = 246.0                                                           
      IF (FH1(2).EQ.ANAL.OR.FH1(2).EQ.F000)    FHR = 0.0                    
      IF (FH1(2).EQ.F006)    FHR =   6.0                                    
      IF (FH1(2).EQ.F012)    FHR =  12.0                                    
      IF (FH1(2).EQ.F024)    FHR =  24.0                                    
      IF (FH1(2).EQ.F036)    FHR =  36.0                                    
      IF (FH1(2).EQ.F048)    FHR =  48.0                                    
      IF (FH1(2).EQ.F060)    FHR =  60.0                                    
      IF (FH1(2).EQ.F072)    FHR =  72.0                                    
      IF (FH1(2).EQ.F084)    FHR =  84.0                                    
      IF (FH1(2).EQ.F096)    FHR =  96.0                                    
      IF (FH1(2).EQ.F108)    FHR = 108.0                                    
      IF (FH1(2).EQ.F120)    FHR = 120.0                                    
      IF (FH1(2).EQ.F132)    FHR = 132.0                                    
      IF (FH1(2).EQ.F144)    FHR = 144.0                                    
      IF (FH1(2).EQ.F156)    FHR = 156.0                                    
      IF (FH1(2).EQ.F168)    FHR = 168.0                                    
      IF (FH1(2).EQ.F180)    FHR = 180.0                                    
      IF (FH1(2).EQ.F192)    FHR = 192.0                                    
      IF (FH1(2).EQ.F204)    FHR = 204.0                                    
      IF (FH1(2).EQ.F216)    FHR = 216.0                                    
      IF (FH1(2).EQ.F228)    FHR = 228.0                                    
      IF (FH1(2).EQ.F240)    FHR = 240.0                                    
 1130 CONTINUE                                                                
                                                                             
C     ALL DONE WITH PROCESSING SELECTOR CARDS                                 
C                                                                               
C     * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
C     * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
C                                                                               
C     READ AND SELECT DATA...                                                 
C     IF DATES DO NOT INCLUDE CENTURY, ASSUME 20TH AND ADD IT
                                                                               
   30 CONTINUE                                                                
      READ (IN,ERR=70,END=40)  IDATE,AREA,DDNAME,KGRID,GCODE,PAGE
      NREC = NREC + 1                                                         
      IF (IDATE(1).LT.ICEN)    IDATE(1) = IDATE(1) + ICEN19
      IF (IDATE(2).LT.ICEN)    IDATE(2) = IDATE(2) + ICEN19
      DATES(1) = IDATE(1)                                                     
      DATES(2) = IDATE(2)                                                     
      CALL SW3FS1(DATES(1),LDATE1(1),LDATE1(2),LDATE1(3),LDATE1(4),1)         
      CALL SW3FS1(DATES(2),LDATE2(1),LDATE2(2),LDATE2(3),LDATE2(4),1)         

      IF (IX(1).NE.0.AND.NREC.EQ.1.AND.FDATE.LT.DATES(1)) THEN 
         FDATE = DATES(1)                                                    
         CALL SW3FS1(FDATE,LFDATE(1),LFDATE(2),LFDATE(3),LFDATE(4),1)  
      END IF
                                                                               
C     ...CHANGED INITIAL DATE IF LT DATE OF FIRST RECORD & TIME-SERIES        
C     OPTION IS BEING EXERCISED (TO AVOID EXCESSIVE NULL DATA).......         
C     IF STATS ARE DAILY, CHECK OPTION TO SELECT 00Z ONLY OR 12Z ONLY:        
                                                                               
      IF (DATES(1).EQ.DATES(2).AND.LFZ.EQ.LLZ.AND.LFZ.NE.LDATE1(4))           
     *   GO TO 30                                                            
                                                                               
C     IF MODEL & HR ID IS IN 'V&HRMODL' FORMAT, CONVERT TO 'MODLF&HR'         
                                                                               
      IF (LETTER.NE.VEE)    GO TO 28                                          
      LETTER = EF                                                           
      STORE  = FH2(1)                                                       
      FH2(1) = FH2(2)                                                       
      FH2(2) = STORE                                                        
      IF (FH2(2).EQ.FNAL)    FH2(2) = ANAL                                  
   28 CONTINUE                                                                
                                                                               
C     MATCH UP MODEL, AREA, AND DATES WITH THOSE DESIRED                        
                                                                               
      IF (DATES(2).GT.LDATE.AND.LDATE.GT.0)        GO TO 40                   
      IF (MDL.NE.MOD3.AND.FH1(1).NE.BLANK)         GO TO 30                   
      IF (FH1(2).NE.FH2(2).AND.FH1(2).NE.BLANK)    GO TO 30                   
      IF (INAREA.NE.AREA.AND.INAREA.NE.BLANK8)     GO TO 30                   
      IF (DATES(1).LT.FDATE.AND.FDATE.GT.0)        GO TO 30                   
      IF (NDATE.LT.DATES(2))    NDATE = DATES(2)                              
                                                                               
C     PROCESS SELECTED DATA, AS PER INSTRUCTIONS; PRINT AND COPY...             
C     COPY TO 'OUT' (FT15) IF NPRINT IS 0 OR 1 AND CONT.NE.2 (OR 8);            
C     USE DD DUMMY IF NEITHER COPY NOR AVERAGES ARE DESIRED.                    
                                                                               
      IF (NPRINT.LE.1.AND.CONT.NE.2)                                          
     *   WRITE (OUT)  DATES, AREA, DDNAME, KGRID, GCODE, PAGE                
      IF (NPRINT.LE.1.AND.CONT.NE.2)     NWRITE = NWRITE + 1                  
      IF (NPRINT.EQ.0.OR.NPRINT.EQ.4)    GO TO 33                             
      PRINT 301, DDNAME, AREA,                                              
     *           LDATE1(2), LDATE1(3), LDATE1(1), LDATE1(4),                
     *           LDATE2(2), LDATE2(3), LDATE2(1), LDATE2(4),                
     *           KGRID, GCODE, NREC                                         
  301 FORMAT (/1X,A8, ', AREA ', A8,                                   
     *  ', FROM ', 2(I2,'/'),2I4,'Z', ' TO ', 2(I2,'/'),2I4,'Z',        
     *        1X, 'K=', F4.0, 1X, 'G=', F3.0, 1X, 'NREC=', I3)
      IF (NPRINT.EQ.1)    GO TO 33                                          
      PRINT 302                                                             
  302 FORMAT (//23X, 'S1', 2X, 'MEAN ERR', 2X, 'STD. DEV.',        
     *       5X, 'RMS', 2X, 'MEAN OBS', 2X, 'S.D. OBS.')                  
      DO 32 LEV=1,4                                                         
      PRINT 303      
  303 FORMAT (/)                                                      
      DO 31 LQ=1,5                                                        
      J = 5*(LEV-1) + LQ                                                
      IF (LQ.GE.4)    PAGE(1,J) = 999999.                               
      IF(LQ.EQ.1) THEN
         PRINT 305,LEVEL(LEV),LABELS(LQ),(PAGE(I,J),I=1,6)
      ELSE
         PRINT 304, LABELS(LQ), (PAGE(I,J),I=1,6)
      END IF      
  304 FORMAT(8X,A8,2(1X,F8.2),4(2X,F8.2))  
  305 FORMAT(1X,I3,' MB',1X,A8,2(1X,F8.2),4(2X,F8.2))
   31 CONTINUE                                                            
   32 CONTINUE                                                              
      IF (NPRINT.EQ.3)   GO TO 10                                           
   33 CONTINUE                                                                
     
C     SUPER-INVEN: WRITE DATE AS MMDDHH IF DAILY STATS (YEAR SHOULD
C     BE ON OUTPUT FILENAME), ELSE AS YYYYMM TO CONSERVE LINE SPACE
     
      IF (NPRINT.EQ.4)   THEN                                         
        IF (DATES(1).EQ.DATES(2))    THEN
          MDATE = 10000*LDATE1(2) + 100*LDATE1(3) + LDATE1(4)
        ELSE
          MDATE = 100*LDATE1(1) + LDATE1(2)
        ENDIF
        IF (INVEN.EQ.21)    THEN
          J1 = 1
          J2 =20
        ELSE
          J1 = INVEN
          J2 = INVEN
        ENDIF
        DO JJ=J1,J2
          PRINT 306, DDNAME, AREA, MDATE, PQ(JJ),            
     *               (PAGE(I,JJ),I=1,6)                      
  306     FORMAT (' ', A8,1X,A8,I7,2X,A4, 4(1X,F7.2), 1X,F8.2,1X,F7.2 )         
        END DO
      END IF
                                                                               
C       AVERAGE STATS OVER PERIOD IF DESIRED....                                
                                                                               
      IF (CONT.NE.2)    GO TO 136                                             
      DO 131 IA=1,15                                                        
      IF (AVAREA(IA).EQ.BLANK8)    AVAREA(IA) = AREA                      
      IF (AVAREA(IA).EQ.AREA)      GO TO 132                              
  131 CONTINUE                                                              
      PRINT 231, AREA                                                       
  231 FORMAT (' *** NO MATCH FOR ', A8, ' IN AVG; PROBABLE ERROR',         
     *        ' ***')                                                       
      GO TO 136                                                             
                                                                               
C     CONSTRUCT AVERAGES HERE.  "CUT" IS A CRUDE EMPIRICALLY                
C     DERIVED TOSSOUT THRESHHOLD FOR QUESTIONABLE RMS VALUES,               
C     BASED ON THE HIGHEST PROBABLE LEGITIMATE VALUE FOR ANY                
C     MODEL (PERSISTENCE GETS A SPECIAL RATE) IN ANY AREA AT                
C     ANY TIME OF THE YEAR FOR A GIVEN QUANTITY, LEVEL, AND                 
C     FORECAST HOUR.                                                        
                                                                               
C     IT IS FIRST COMPUTED FOR SINGLE-OBSERVATION (DAILY) STATS;            
C     IF STATS ARE OTHER THAN DAILY, "CUT" IS TAKEN TO BE TWO-              
C     THIRDS OF ITS "DAILY" VALUE (MOST APPLICABLE TO MONTHLY               
C     STATS), THOUGH ONE-HALF MIGHT BE A BETTER VALUE FOR HEIGHTS.          
                                                                             
C     THESE ARE RATHER LOOSE LIMITS; GOOD DATA WILL RARELY BE               
C     EXCLUDED BUT SOME BAD DATA MAY STAY IN.  WHEN FEASIBLE, IT            
C     WOULD BE MORE ACCURATE TO COMPUTE AVERAGES FROM TIME-                 
C     SERIES RECORDS USING THE "DSPIKE" OPTION (SEE BELOW).                 
                                                                               
  132 CONTINUE                                                              
      DO 135 J=1,20                                                         
      CUT = RMSA(J) + FHR*RMSC(J)                                         
      IF (FH2(1).EQ.PERN.OR.FH2(1).EQ.PERS)    CUT = 2.5*CUT              
      IF (DATES(1).NE.DATES(2))                CUT = CUT/1.5              
      IF (PAGE(4,J).GT.CUT.OR.PAGE(3,J).LT.0.1)     GO TO 135             
      DO 134 I=1,6                                                        
      IF (I.EQ.1.AND.PAGE(I,J).GT.100.)    GO TO 133                    
      AVG(IA,I,J) = AVG(IA,I,J) + PAGE(I,J)                           
      KOUNT(IA,I,J) = KOUNT(IA,I,J) + 1                               
  133 CONTINUE                                                          
  134 CONTINUE                                                            
  135 CONTINUE                                                              
  136 CONTINUE                                                                
                                                                               
      IF (IX(1).EQ.0)    GO TO 30                                             
      IF (NSTAT.EQ.0)    GO TO 30                                             
                                                                              
C     CHECK TO SEE WHETHER STATS ARE MONTHLY , DAILY, OR NEITHER;               
C     CONSTRUCT TIME-SERIES RECORDS.  NOTE: MONTHLY STATS ARE                   
C     ASSUMED, UNLESS A) BEGINNING AND ENDING DATE/TIME ON ACTUAL               
C     DATA RECORD ARE EQUAL, INDICATING DAILY (SINGLE-OBSERVATION)              
C     STATISTICS, OR B) THE DAY-OF-MONTH FOR THE 'S E L E C T E D'              
C     ENDING DATE EXCEEDS 31, IN WHICH CASE ALL DATA IS  A S S U M E D          
C     TO BE FOR SOME OTHER TIME INTERVAL.  PROPER IDENTIFICATION IS             
C     ESSENTIAL WHEN THE TIME-SERIES RECORDS ARE SUBSEQUENTLY READ              
C     BY MAN OR MACHINE FOR PLOTTING GRAPHS.  FOR THIS REASON              
C     IT IS  V E R Y  UNWISE TO HAVE 'MIXED' TIME INTERVALS IN THE              
C     SUMAC INPUT FILE (I.E., DAILY AND MONTHLY).                               
                                                                               
      NTIME = NTIME + 1                                                       
      N = NTIME                                                               
      IF (LLDATE(3).GT.31)    GO TO 35                                        
      IY = LFDATE(1)                                                        
      JY = LDATE1(1)                                                        
      IM = LFDATE(2)                                                        
      JM = LDATE1(2)                                                        
      ND = LDATE1(3)                                                        
      IF (IID.EQ.0.AND.ND.GT.25)    JM = JM+1                               
      N = 12*(JY-IY) + JM - IM + 1                                          
      IF (DATES(1).NE.DATES(2))    GO TO 35                                   
      IF (IID.NE.0)              GO TO 34                                   
      IID = LFDATE(3)                                                     
      L = (IY-1)/4                                                        
      LDX = 1461*L                                                        
      LDX = LDX + 365*(IY-4*L-1)                                          
      LDX = LDX + JMONTH(IM) + IID                                        
      IF (MOD(IY,4).EQ.0.AND.IM.GT.2)    LDX = LDX+1                      
   34 CONTINUE                                                              
      LZ = LDATE2(4)                                                        
      IF (LZ.NE.LFZ.AND.LZ.NE.LLZ)    GO TO 30                              
      JJD = LDATE1(3)                                                       
      L = (JY-1)/4                                                          
      LD = 1461*L                                                           
      LD = LD + 365*(JY-4*L-1)                                              
      LD = LD + JMONTH(JM) + JJD                                            
      IF (MOD(JY,4).EQ.0.AND.JM.GT.2)    LD = LD+1                          
      N = LD - LDX + 1                                                      
      IF (LFZ.NE.LLZ)    N = 2*N + LZ/12 - 1                                
   35 CONTINUE                                                                
      IF (N.LE.0.OR.N.GT.500)    GO TO 30                                     
      DO 36 ISTAT=1,NSTAT                                                     
        IF (IX(ISTAT).EQ.9)    GO TO 36                                       
        I = IX(ISTAT)                                                         
        J = JX(ISTAT)                                                         
        IF (PAGE(I,J).GT.99999.)     GO TO 36     
        STAT(ISTAT,N) = PAGE(I,J)                                             
   36 CONTINUE                                                                
      GO TO 30                                                                
                                                                              
C     * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
C     * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
C                                                                               
C     END-OF-FILE REACHED.  WRAP UP AND GO BACK FOR MORE.                       
C                                                                               
C     WRITE TIME-SERIES RECORDS...                                              
C     
C     INTVAL IS TIME-INTERVAL OF DATA (1=DAILY 2=MONTHLY 3=OTHER)
                                                                               
   40 CONTINUE                                                                
      INTVAL = 2
      CALL SW3FS1(NDATE,LNDATE(1),LNDATE(2),LNDATE(3),LNDATE(4),1)            
      LNDATE(4) = LLDATE(4)                                                   
      CALL SW3FS1(NDATE,LNDATE(1),LNDATE(2),LNDATE(3),LNDATE(4),0)            
      LFDTE = LFDATE(1)                                                       
      IF (IX(1).EQ.0)    GO TO 45                                             
      IF (NSTAT.EQ.0)    GO TO 45                                             
      IF (IID.NE.0)           INTVAL = 1
      IF (LLDATE(3).GT.31)    INTVAL = 3
      DO ISTAT=1,NSTAT                                                    
        IF (IX(ISTAT).NE.9)    THEN
          DO I=1,500                                                        
            XSTAT(I) = STAT(ISTAT,I)                                            
            STAT(ISTAT,I) = 999.                                                
          END DO                                                              
                                                                               
          IF (FACTOR.GE.1.9)    CALL DSPIKE                                     
                                                                               
          CALL SW3FS1(FDATE,LFDATE(1),LFDATE(2),LFDATE(3),LFDATE(4),0)          
          WRITE (20) INMOD, INAREA, INTVAL, FDATE, NDATE,          
     *               IX(ISTAT), JX(ISTAT), XSTAT                                  
          KWRITE = KWRITE + 1                                                   
        ENDIF
      END DO                                                                
                                                                               
C     WRITE AVERAGE FOR PERIOD...                                             
                                                                               
   45 CONTINUE                                                                
      DO 48 IA=1,15                                                           
      IF (AVAREA(IA).EQ.BLANK8)    GO TO 49                                 
      IKOUNT = 0                                                            
      DO 47 I=1,6                                                           
      DO 46 J=1,20                                                        
      NK = KOUNT(IA,I,J)                                                
      XK = KOUNT(IA,I,J)                                                
      IKOUNT = IKOUNT + NK                                              
      IF (NK.GT.0)    AVG(IA,I,J) = AVG(IA,I,J)/XK                      
      IF (NK.LE.0)    AVG(IA,I,J) = GROSS                               
   46 CONTINUE                                                            
   47 CONTINUE                                                              
      IF (IKOUNT.EQ.0)    GO TO 48                                          
      LFDATE(1) = LFDTE                                                     
      CALL SW3FS1(FDATE,LFDATE(1),LFDATE(2),LFDATE(3),LFDATE(4),0)          
      WRITE(OUT) FDATE, NDATE, AVAREA(IA), INMOD, KGRID, GCODE,             
     *           ((AVG(IA,IB,IC),IB=1,6),IC=1,20)                           
      NWRITE = NWRITE + 1                                                   
   48 CONTINUE                                                                
   49 CONTINUE                                                                
      GO TO 10                                                                
                                                                               
C       * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
C       * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
C                                                                               
C     END-OF-JOB FLAG; READ BACK FT20 AND PRINT IF NPRINT.NE.0                
                                                                               
   50 CONTINUE                                                                
      IF (KWRITE.NE.0)    ENDFILE 20                                          
      IF (KWRITE.NE.0)    REWIND  20                                          
      IF (NPRINT.EQ.0)      GO TO 55                                          
C     PRINT 154                                                               
      PRINT 250, KWRITE                                                       
  250 FORMAT (' HERE IS INVENTORY OF UNIT 20 CONTAINING SELECTED ',          
     *  'STATISTICS ...',//,' ', I3, ' RECORDS WRITTEN THIS SESSION.')         
   51 CONTINUE                                                                
      READ (20,ERR=80,END=55)  DDNAME,AREA,INTVAL,DATES,IXX,JXX,XSTAT         
      CALL SW3FS1                                                           
     *     (DATES(1),LDATE1(1),LDATE1(2),LDATE1(3),LDATE1(4),1)             
      CALL SW3FS1                                                           
     *     (DATES(2),LDATE2(1),LDATE2(2),LDATE2(3),LDATE2(4),1)             
      NFYEAR = LDATE1(1)                                                    
      DO 52 II=1,23                                                         
        COLH(II) = MONTHS(II)                                               
        IF (INTVAL.NE.2)    COLH(II) = BLANK                              
   52 CONTINUE                                                              
      L = (JXX+4)/5                                                           
      M = JXX - 5*(L-1)                                                       
      M1 = LDATE1(2)                                                        
      M2 = M1 + 11                                                          
      NMAX = 0                                                              
      DO 53 MAX=1,500                                                       
        IF (XSTAT(MAX).NE.999.0)    NMAX = MAX                              
   53 CONTINUE                                                              
      PRINT 251, DDNAME, AREA, LDATE1, LDATE2, INTLBL(INTVAL),
     *   IDSTAT(IXX), LEVEL(L), QLEVEL(M), (COLH(MX),MX=M1,M2)
  251 FORMAT (//' ',A8, ', AREA ', A8, ', FROM',I5,3I3,' TO',I5,         
     *     3I3, 2X, A8,/,' FOR ', A8,I4,1X,A8,//8X, 12(A4,4X), /)             
      IF (NMAX.NE.0)    PRINT 252, (XSTAT(MM),MM=1,NMAX)                    
  252 FORMAT (4X,12F8.2)                                            
      IF (NMAX.EQ.0)    PRINT 253                                           
  253 FORMAT ( '*** NO DATA AVAILABLE.  CHECK YOUR INPUT FILES'         
     *       , ' AND SELECTIONS IN EACH STEP OF JOB THAT WROTE'         
     *       , ' THIS RECORD. ***', / )                                 
      GO TO 51                                                                
                                                                               
C     * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
C     * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
C                                                                              
C     ERROR/END MESSAGES HERE ....                                            
                                                                               
   55 CONTINUE                                                                
      STOP                                                                    
                                                                               
   70 CONTINUE                                                                
      PRINT 270, IN                                                           
  270 FORMAT ( ' *** ERROR WHILE READING INPUT ON UNIT', I3, ' ***')         
      GO TO 30                                                                
                                                                               
   80 CONTINUE                                                                
      PRINT 280                                                               
  280 FORMAT (/ ' *** ERROR IN READBACK OF O/P ON UNIT 20 ***')             
      STOP 88                                                                 
      END                                                                     
C     DATA SET DSPIKE     AT LEVEL 003 AS OF 01/23/89                      
                                                                               
      SUBROUTINE DSPIKE                                                         
                                                                               
C     DELETE BAD DATA THAT WOULD CAUSE "SPIKES" ON LINE GRAPH                   
                                                                               
      COMMON /INFO/  XSTAT(500), MSG, FACTOR                                    
                                                                               
      REAL(4) DELTA(500), XSTAT, MSG, SUM, SUMSQ, FACTOR, SD                    
      REAL(4) CUTOFF, DMEAN                                                     
                                                                               
      EPS=1.0E-8
      N = 0                                                                     
      SUM = 0.                                                                  
      SUMSQ = 0.                                                                
      DMEAN = 0.                                                                
                                                                               
      DO 2 I=2,499                                                              
      DELTA(I) = MSG                                                          
      IF (ABS(XSTAT(I-1)-MSG).LT.EPS.OR. 
     &    ABS(XSTAT(I  )-MSG).LT.EPS.OR.
     &    ABS(XSTAT(I+1)-MSG).LT.EPS   )    GO TO 1                                    
      DELTA(I) = ABS(2.0*XSTAT(I) - XSTAT(I-1) - XSTAT(I+1) )               
      SUM = SUM + DELTA(I)                                                  
      N = N+1                                                               
    1 CONTINUE                                                                
    2 CONTINUE                                                                  
                                                                               
      IF (N.NE.0) DMEAN = SUM/FLOAT(N) 
      DO 4 I=2,499                                                              
      IF (ABS(DELTA(I)-MSG).LT.EPS)    GO TO 3
      DELTA(I) = ABS(DELTA(I) - DMEAN)                                      
      SUMSQ = SUMSQ + DELTA(I)*DELTA(I)                                     
    3 CONTINUE                                                                
    4 CONTINUE                                                                  
                                                                               
      IF (N.LE.1) GO TO 6
      SD = SQRT(SUMSQ/FLOAT(N-1)) 
      CUTOFF = SD*FACTOR                                                        
      DO 5 I=2,499                                                              
      IF (DELTA(I).GT.CUTOFF.AND.DELTA(I).NE.MSG)    XSTAT(I) = MSG           
    5 CONTINUE                                                                  
    6 CONTINUE                                                                  
      RETURN                                                                    
      END                                                                       
      SUBROUTINE SW3FS1(IDATE,IYEAR,MONTH,IDAY,IHOUR,NN)
      INTEGER(4) IDATE,IYEAR,MONTH,IDAY,IHOUR
      IF(NN.NE.0) THEN
         IYEAR=IDATE/1000000
         LLL=IDATE-IYEAR*1000000 
         MONTH=LLL/10000
         LLLL=LLL-MONTH*10000
         IDAY=LLLL/100
         IHOUR=IDATE-IYEAR*1000000-MONTH*10000-IDAY*100
       ELSE
         IDATE=IYEAR*1000000+MONTH*10000+IDAY*100+IHOUR
       END IF
       RETURN
       END 
