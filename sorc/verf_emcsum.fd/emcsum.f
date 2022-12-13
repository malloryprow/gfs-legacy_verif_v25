C$$$  MAIN PROGRAM DOCUMENTATION BLOCK                                          
C                .      .    .                                       .          
C MAIN PROGRAM:  EMCSUM      SELECT SUMAC DATA AND COPY/PRINT/INVEN             
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

      REAL (8)   RINC(5)
      REAL (4)   XST(6) 
      INTEGER(4) BDATE,BYEAR,BMONTH,BDAY,BHOUR
      INTEGER(8) CDATE(8), DDATE(8)
      CHARACTER (4)  PPQ
                                                                               
      CHARACTER(13) INVLAB(21)
      CHARACTER(8) FCSTMH, INMOD, INAREA, IDSTAT(6), QLEVEL(5)                  
      CHARACTER(8) BLANK8, AVAREA(15), AREA, LABELS(5)             
      CHARACTER(8) MLABEL, ALABEL, ALL, INTLBL(3)                  
      CHARACTER(4) MONTHS(23), COLH(23), FH1(2), FH2(2), STORE                  
      CHARACTER(4) FNAL, ANAL, BLANK, PERN, PERS, PQ(20)          
      CHARACTER(4) F000, F006, F012, F024, F036, F048, F060                 
      CHARACTER(4) F072, F084, F096, F108, F120, F132, F144                     
      CHARACTER(3) MDL, MOD3
      CHARACTER(1) VEE, EF, LETTER                                  
                                                                               
      EQUIVALENCE (FH1(1),INMOD), (FH2(1),FCSTMH), (LETTER,FCSTMH)              
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
      DATA  F132 /'F132'/, F144 /'F144'/                                 
      DATA  PERN /'PERN'/, PERS /'PERS'/                                 
      DATA  VEE /'V'/, EF /'F'/
                                                                               
      IREC = 0
      ICNT = 0
                                                                               
      ICEN = 10**8
      ICEN19 = 19*ICEN
      MSG = 999.                                                              
      KWRITE = 0                                                              
      NWRITE = 0                                                              
      PRINT 150                                                               
                                                                               
  150 FORMAT (18X, 'SUMMARY OF VERIFICATION STATISTICS',                 
     *       /18X, '  (WITH RESPECT TO OBSERVATIONS)  ',                 
     *       /18X, '              *****               ',                 
     *       /18X, '             NCEP/USA             ',                 
     *       /18X, '          IBM FORTRAN 90          ',                 
     *       /18X, '    CODE COMPILED  21 MAY 1999    ',                 
     *       /18X, '  PROGRAMMER:  CHARLES L. VLCEK   ', ////)           
                                                                               
  154 FORMAT (//18X, 'SUMMARY OF VERIFICATION STATISTICS',                 
     *       /18X, '  (WITH RESPECT TO OBSERVATIONS)  ',                 
     *       /18X, '               *****              ',                 
     *       /18X, '              NCEP/USA            ')               
                                                                               
C     READ DATA-SELECT CARD(S) AND PRINT REQUESTS AT TOP OF PAGE,               
C     FOLLOWED BY COLUMN LABELS, IF ANY.                                        
                                                                               
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

      print*,'First print 100'
      write(*,100)  INMOD, INAREA, LFDATE, LLDATE, NPRINT, CONT
     
C     CONVERT 2-DIGIT YEAR TO 4-DIGIT (20TH CEN ASSUMED)
     
      IF (LFDATE(1).LT.100)    LFDATE(1) = LFDATE(1) + 1900
      IF (LLDATE(1).LT.100)    LLDATE(1) = LLDATE(1) + 1900
                                                                               
C     RECORD SKIP OPTION: IF 'START HOUR' IS NEGATIVE, 'END DATE'             
C     CONTAINS NUMBER OF RECORDS TO BE SKIPPED.                               
                                                                               
      IF (LFDATE(4).GE.0)    GO TO 17                                         
      IF (CONT.GE.6)       REWIND IN                                        
      IF (CONT.GE.6)       NREC = 0                                         
      NSKIP = 10000*LLDATE(2) + 100*LLDATE(3) + LLDATE(4)                   

C     PRINT *, 'NSKIP = ', NSKIP

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
      IF (NPRINT.NE.3.AND.NP.NE.0)  PRINT 154                                 
      NPREV = NPRINT                                                          
      IF (NPRINT.NE.3)                                                        
     *  PRINT 200, IN, MLABEL, ALABEL, LFDATE, LLDATE, NPRINT, CONT             
  200   FORMAT (// ' YOU HAVE REQUESTED FROM FT', I2, ':  '/                  
     *        ' MODEL & HOUR = ', A8, '  AREA=', A8/ ' DATES ARE',              
     *       I5,3I3, 2X, I5,3I3, '  PRINT=', I1, '  CONT=', I1/)
                                                                               
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
      IF (NPRINT.EQ.4)    PRINT 204, INVLAB(INVEN), IDSTAT                    
  204 FORMAT (' YOU HAVE REQUESTED A ''SUPER-INVEN'' OF LABELS ',            
     *        'AND ', A13, ' STATISTICS.'//               
     *        ' MDL&HOUR   AREA   PERIOD   P-Q', 4A8, 1X, 2A8 )         
                                                                               
C     GET NUMERIC VALUE OF FORECAST HOUR IF AVERAGING OPTION CHOSEN           
C     (USED IN ASSIGNING UPPER LIMIT TO 'ACCEPTABLE' INDIVIDUAL               
C     STAT VALUES).                                                           
                                                                              
      IF (CONT.NE.2.AND.CONT.NE.8)    GO TO 1130                              
      FHR = 150.0                                                           
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
 1130 CONTINUE                                                                
                                                                             
C     ALL DONE WITH PROCESSING SELECTOR CARDS                                 
                                                                               
C     READ AND SELECT DATA...                                                 
C     IF DATES DO NOT INCLUDE CENTURY, ASSUME 20TH AND ADD IT
                                                                               
   30 CONTINUE                                                                

      READ (IN,ERR=70,END=40)  IDATE,AREA,FCSTMH,KGRID,GCODE,PAGE

        if(fcstmh(1:3).eq.'ECM')then
        print*,'Second Print'
        print*,'idate,area,fcstmh,kgrid,gcode  ',
     *  idate,area,fcstmh,kgrid,gcode
        write(*,'(5f15.2)')((page(i,j),i=1,6),j=1,20)
        endif


C      PRINT *, 'MAIN:  AREA = ', AREA
C      IF ( ICNT .LT. 6 ) THEN
C      END IF

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
      IF (MDL .NE. MOD3 .AND. FH1(1) .NE. BLANK)   GO TO 30                   
      IF (FH1(2).NE.FH2(2).AND.FH1(2).NE.BLANK)    GO TO 30                   
      IF (INAREA.NE.AREA.AND.INAREA.NE.BLANK8)     GO TO 30                   
      IF (DATES(1).LT.FDATE.AND.FDATE.GT.0)        GO TO 30                   
      IF (NDATE.LT.DATES(2))    NDATE = DATES(2)                              
                                                                               
C     PROCESS SELECTED DATA, AS PER INSTRUCTIONS; PRINT AND COPY...             
C     COPY TO 'OUT' (FT15) IF NPRINT IS 0 OR 1 AND CONT.NE.2 (OR 8);            
C     USE DD DUMMY IF NEITHER COPY NOR AVERAGES ARE DESIRED.                    
                                                                               
C       PRINT *, 'CHK1:  NPRINT = ', NPRINT
C       PRINT *, 'CHK1:  CONT = ', CONT

      IF (NPRINT.LE.1.AND.CONT.NE.2)                                          
     *   WRITE (OUT)  DATES, AREA, FCSTMH, KGRID, GCODE, PAGE                
      IF (NPRINT.LE.1.AND.CONT.NE.2)     NWRITE = NWRITE + 1                  
      IF (NPRINT.EQ.0.OR.NPRINT.EQ.4)    GO TO 33                             
      IF (NPRINT.GE.2)    PRINT 154                                         
      PRINT 301, FCSTMH, AREA,                                              
     *           LDATE1(2), LDATE1(3), LDATE1(1), LDATE1(4),                
     *           LDATE2(2), LDATE2(3), LDATE2(1), LDATE2(4),                
     *           KGRID, GCODE, NREC                                         

  301 FORMAT (1X,A8, ', AREA ', A8,                                   
     *  ', FROM ', 2(I2,'/'),2I4,'Z', ' TO ', 2(I2,'/'),2I4,'Z',        
     *        1X, 'K=', F4.0, 1X, 'G=', F5.0, 1X, 'NREC=', I7)
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


          PPQ = PQ(JJ)

C         GET THE STATISTICAL PARAMETERS

          DO  MM = 1,6
             XST(MM) = PAGE(MM,JJ)
          END DO

C         CONVERT THE ARKSUMAC RECORD DATA TO VSDB FORMAT

          print*,'Before VSDB  '
          print*,'J1,J2,PPQ,XST,FCSTMH,IDATE,AREA   ',
     *    J1,J2,PPQ,XST,FCSTMH,IDATE,AREA

          CALL  VSDB ( FCSTMH, IDATE, AREA, PPQ, XST ) 

          PRINT 306, FCSTMH, AREA, MDATE, PQ(JJ),            
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
                                                                              
C     END-OF-FILE REACHED.  WRAP UP AND GO BACK FOR MORE.                       
C     WRITE TIME-SERIES RECORDS...                                              
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
                                                                               
C     END-OF-JOB FLAG; READ BACK FT20 AND PRINT IF NPRINT.NE.0                
                                                                               
   50 CONTINUE                                                                
      IF (KWRITE.NE.0)    ENDFILE 20                                          
      IF (KWRITE.NE.0)    REWIND  20                                          
      IF (NPRINT.EQ.0)      GO TO 55                                          
      PRINT 154                                                               
      PRINT 250, KWRITE                                                       
  250 FORMAT (' HERE IS INVENTORY OF UNIT 20 CONTAINING SELECTED ',          
     *  'STATISTICS ...',//,' ', I3, ' RECORDS WRITTEN THIS SESSION.')         
   51 CONTINUE                                                                
      READ (20,ERR=80,END=55)  FCSTMH,AREA,INTVAL,DATES,IXX,JXX,XSTAT         
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
      PRINT 251, FCSTMH, AREA, LDATE1, LDATE2, INTLBL(INTVAL),
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
                                                                               
C     ERROR/END MESSAGES HERE ....                                            
                                                                               
   55 CONTINUE                                                                
      PRINT 255                                                               
  255 FORMAT (/ ' ALL DONE AND ALL''S WELL')                                
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
      DATA ICNT / 0 /

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
       SUBROUTINE  VSDB ( FCSTMH, IDATE, AREA, PPQ, XST )
C$$$  SUBPROGRAM DOCUMENTATION BLOCK                       
C                                                          
C SUBPROGRAM: VSDB           CONVERTS SUMAC RECORD DATA TO VSDB FORMAT
C   PRGMMR: BOB HOLLERN      ORG: W/NP12     DATE: 00-02-10
C                                                          
C ABSTRACT: CONVERTS THE ARKSUMAC DATA TO THE EMC VERIFICATION   
C   DATABASE (VSDB) FORMAT.
C
C PROGRAM HISTORY LOG:                                     
C   00-02-06  BOB HOLLERN, AUTHOR
C                                                          
C USAGE:  CALL VSDB ( FCSTMH, IDATE, AREA, PPQ, XST )
C                                                          
C   INPUT ARGUMENT LIST:                                   
C                                                          
C     FCSTMH   -   VARIABLE CONTAINING IN THE FIRST 4 BYTES THE MODEL
C                  NAME AND IN THE LAST 4 BYTES THE FORECAST HOUR
C
C     IDATE    -   ARRAY CONTAINING THE STARTING AND ENDING
C                  VERIFICATION DATE/TIME DATA
C                  
C     AREA     -   VARIABLE CONTAINING THE VERIFICATION AREA               
C
C     PPQ      -   VARIABLE CONTAINING THE LEVEL AND PARAMETER TYPE
C
C     XST      -   ARRAY CONTAINING THE 6 STATISTICAL PARAMETERS      
C
C   OUTPUT ARGUMENT LIST:                                   
C
C     IRET     -   RETURN CODE VARIABLE SET TO 0, IF NO PROBLEMS; SET   
C                  TO 1, IF PROBLEMS
C
C   INPUT FILES:
C
C
C   OUTPUT FILES:
C
C    FORT.06       FORTRAN PRINT FILE
C
C
C    FORT.50    
C
C
C
C ATTRIBUTES:                                              
C   LANGUAGE: FORTRAN 90                                   
C$$$                                                       

C      DATA BASE VERSION

       REAL (4)  XST(6)

       INTEGER (4)   IDATE(2)

       CHARACTER*1   LINFD, BLANK,  KEQU

       CHARACTER*4   MHRS,   MODEL
       CHARACTER*4   PPQ

       CHARACTER*3   VSVER

       CHARACTER*8   FCSTMH,  AREA,  HHH,  MDLNM1,  MDLNM2

       CHARACTER*10   JDATE

       CHARACTER*20   NFILENM,  OFILENM

       CHARACTER*255   VSDBRC

       DATA BLANK / ' ' /
       DATA OFILENM  / 'AAAAAAAAAAAAAAAAAAAA' /
       DATA KEQU  / '=' /
       DATA VSVER / 'V01' /

100    FORMAT ( A )

C      COMPUTE THE NUMBER OF HOURS FROM THE START OF THE PERIOD
C      TO THE END OF THE PERIOD.

          print*,'Inside VSDB  '
          print*,'PPQ,XST,FCSTMH,IDATE,AREA   ',
     *    PPQ,XST,FCSTMH,IDATE,AREA

       CALL  TIMDIF ( IDATE, NHRS, IRET )

       WRITE( UNIT=MHRS(1:4), FMT='(I4)' ) NHRS
        print*,'NHRS  =  ',NHRS

C      GET THE STARTING DATE OF THE PERIOD -- YYYYMMDDHH
C      AND CONVERT IT TO ASCII

       WRITE( UNIT=JDATE(1:10), FMT='(I10.9)' ) IDATE(1)

       DO I = 1,255
          VSDBRC(I:I) = ' '
       END DO

       LINFD = CHAR(10)

       N = 1

C      HEADER FIELD 1:  VERIFICATION DATABASE VERSION

       VSDBRC(N:N+2) = VSVER

C      HEADER FIELD 2:  FORECAST MODEL VERIFIED

       IF(FCSTMH(1:3).EQ.'ECM')THEN
          MODEL = FCSTMH(1:3)
       ELSE
          MODEL = FCSTMH(1:4)
       ENDIF

       print*,'Before FMODEL'
       print*,'MODEL  =   ',model
       CALL  FMODEL( MODEL, IDATE(1), MX, MDLNM1, MDLNM2, IRET )
       print*,'After FMODEL'

C      PRINT *, 'VSDB:  MDLNM1 = $$', MDLNM1, '$$'
C      CONVERT ALL THE FOLLOWING MODEL RECORDS TO THE VSDB FORMAT.
C      PER IS A SPECIAL CASE

       IF ( MDLNM1(1:3) .EQ. 'PER' ) THEN
           MX = 3
       END IF

       print*,'N,MX   ',N,MX

       N = N + 4
       L = N + MX - 1
       VSDBRC(N:L) = MDLNM1(1:MX)

       print*,'n,l,vsdbrc(n:l)  ',n,l,vsdbrc(n:l)

       N = N + MX + 1

C      HEADER FIELD 3:  FORECAST HOUR VERIFIED

       IF ( FCSTMH(5:8) .EQ. 'ANAL' ) THEN
           N = N - 1
           VSDBRC(N:N+5) = '/ANL 0'
           N = N + 7
       ELSE IF ( FCSTMH(6:6) .NE. '0' ) THEN
           VSDBRC(N:N+2) = FCSTMH(6:8)
           N = N + 4 
       ELSE IF ( FCSTMH(7:7) .NE. '0' ) THEN
           VSDBRC(N:N+1) = FCSTMH(7:8)
           N = N + 3 
       ELSE
           VSDBRC(N:N) = FCSTMH(8:8)
           N = N + 2 
       END IF

C      HEADER FIELD 4:  VERIFYING DATE

C      WILL BE STORED IN THE FORM YYYYMMDDHH/+W.  THE + SIGN
C      BEFORE THE W INDICATES THAT YYYYMMDDHH IS THE BEGINNING
C      OF THE TIME INTERVAL.  THE W IS THE WIDTH IN HOURS
C      OF THE TIME INTERVAL.

       VSDBRC(N:N+9) = JDATE(1:10)
C
       N = N + 10
       VSDBRC(N:N+1) = '/+'

C      NUMBER OF HOURS FROM THE START TO THE END OF THE PERIOD

       N = N + 2

       IF ( MHRS(3:3) .EQ. ' ' ) THEN
          VSDBRC(N:N) = MHRS(4:4)
          N = N + 2
       ELSE IF ( MHRS(2:2) .EQ. ' ' ) THEN
          VSDBRC(N:N+1) = MHRS(3:4)
          N = N + 3
       ELSE IF ( MHRS(1:1) .EQ. ' ' ) THEN
          VSDBRC(N:N+2) = MHRS(2:4)
          N = N + 4
       ELSE
          VSDBRC(N:N+3) = MHRS(1:4)
          N = N + 5
       END IF

C      HEADER FIELD 5:  VERIFYING DATA SOURCE OR ANALYSIS

       VSDBRC(N:N+5) = 'ADPUPA'
       N = N + 7

C      HEADER FIELD 6:  VERIFYING GRID OR REGION

       M = 0

       DO I = 1, 8
         IF ( AREA(I:I) .NE. ' ' ) THEN
           M = M + 1
         END IF
       END DO

       L = N + M - 1
       VSDBRC(N:L) = AREA
       N = N + M

C      HEADER FIELD 7:  STATISTIC TYPE

       N = N + 1
       VSDBRC(N:N+4) = 'VLCEK'
       N = N + 6

C      HEADER FIELD 8:  PARAMETER IDENTIFIER

       IF ( PPQ(4:4) .EQ. 'Z' .OR. PPQ(4:4) .EQ. 'T' ) THEN

C          HEIGHT OR TEMPERATURE FIELD

           VSDBRC(N:N) = PPQ(4:4)
           N = N + 2
       ELSE IF ( PPQ(4:4) .EQ. 'S' ) THEN

C          WIND SPEED FIELD

           VSDBRC(N:N+3) = 'WSPD'
           N = N + 5
       ELSE IF ( PPQ(4:4) .EQ. 'V' ) THEN

C          VECTOR WIND FIELD

           VSDBRC(N:N+3) = 'VWND'
           N = N + 5
       ELSE IF ( PPQ(4:4) .EQ. 'R' ) THEN

C          RELATIVE HUMIDITY FIELD

           VSDBRC(N:N+1) = 'RH'
           N = N + 3
       ELSE

           PRINT *, 'CANNOT IDENTIFY PARAMETER'
           RETURN
       END IF

C      HEADER FIELD 9:  LEVEL IDENTIFIER 

       VSDBRC(N:N) = 'P'
       N = N + 1
       VSDBRC(N:N+2) = PPQ(1:3)
       N = N + 4

C      SEPARATOR FIELD

       VSDBRC(N:N) = KEQU
       N = N + 2

C      DATA FIELDS

       CALL  DATFLD ( XST, N, VSDBRC, IRET )

       DO I = 1,20
          NFILENM(I:I) = ' '
       END DO

C      PER IS A SPECIAL CASE.  SET MX = 3.

       NFILENM(1:MX) = MDLNM2(1:MX)
       MX = MX + 1
       NFILENM(MX:MX) = '_'
       MX = MX + 1
       NFILENM(MX:MX+5) = JDATE(1:6)
       MX = MX + 6
       NFILENM(MX:MX+5) = '.vsdb'

C      PRINT *, 'VSDB:  OFILENM = $$', OFILENM,'$$'
C      PRINT *, 'VSDB:  NFILENM = $$', NFILENM, '$$'

       IF ( NFILENM .NE. OFILENM ) THEN

          CLOSE ( UNIT = 60 )

          OPEN ( UNIT=60, IOSTAT = IOS,
     +           FILE = NFILENM,
     +           STATUS = 'UNKNOWN',
     +           ACCESS = 'SEQUENTIAL',
     +           FORM = 'FORMATTED',
     +           POSITION = 'APPEND'  )

          OFILENM = NFILENM

       END IF

       WRITE ( 60, 100 ) VSDBRC(1:N)

       RETURN
       END
       SUBROUTINE  DATFLD ( XST, N, VSDBRC, IRET )
C$$$  SUBPROGRAM DOCUMENTATION BLOCK                       
C                                                          
C SUBPROGRAM: DATFLD         CONVERTS STATISTICAL DATA FIELDS TO ASCII             
C   PRGMMR: BOB HOLLERN      ORG: W/NP12     DATE: 00-02-10
C                                                          
C ABSTRACT: CONVERTS THE STATISTICAL DATA S1, BIAS STATISTIC, STANDARD
C   DEVIATION OF THE ERROR, RMS ERROR, MEAN OBS, AND THE STANDARD
C   DEVIATION OF THE OBS TO CHARACTER FORMAT AND SAVES THESE VALUES
C   IN THE VSDBRC ARRAY.
C
C
C PROGRAM HISTORY LOG:                                     
C   00-02-10  BOB HOLLERN, AUTHOR
C                                                          
C USAGE:  CALL DATFLD ( XST, N, VSDBRC )
C                                                          
C   INPUT ARGUMENT LIST:                                   
C                                                          
C     XST      -   REAL ARRAY CONTAINING THE SUMAC STATISTICAL DATA
C                  FOR CURRENT PARAMETER
C
C     N        -   POINTER TO WHERE TO BEGIN STORING CHARACTER DATA
C                  IN VSDBRC ARRAY
C                  
C   OUTPUT ARGUMENT LIST:                                   
C
C     N        -   POINTER SET TO WHERE NEXT TO STORE DATA IN VSDBRC
C
C     VSDBRC   -   CHARACTER ARRAY TO HOLD ONE RECORD OF SUMAC DATA
C                  CONVERTED TO THE VSDB FORMAT
C
C     IRET     -   RETURN CODE VARIABLE SET TO 0, IF NO PROBLEMS; SET   
C                  TO 1, IF PROBLEMS
C
C ATTRIBUTES:                                              
C   LANGUAGE: FORTRAN 90                                   
C$$$                                                       
       REAL (4)  XST(6)

       CHARACTER*255   VSDBRC

       CHARACTER*8     HHH

       CHARACTER*7     MSNG

       DATA MSNG  / '-1.1E31' /

       IRET = 0

C      DATA COUNT IS SET TO 1 SO THAT THE NUMBERS CAN BE COMBINED
C      FOR AVERAGING IN VARIOUS WAYS AT THE COMMAND OF THE FVS USER.

       VSDBRC(N:N) = '1'
       N = N + 2

       DO I = 1,6
         XX = ABS( XST(I) )

         IF ( XX .GE. 1000000. ) THEN
            VSDBRC(N:N+6) = MSNG
            N = N + 8
         ELSE
            WRITE( UNIT=HHH(1:8), FMT='(F8.2)' ) XST(I)
            M = 0
            DO J = 1,8

C             ELIMINATE LEADING BLANKS

              IF ( HHH(J:J) .EQ. ' ' ) THEN
                 M = M + 1
              END IF
            END DO

            K = N + (8-M)
            VSDBRC(N:K-1) = HHH(M+1:8)
            N = K + 1
         END IF
       END DO

       RETURN
       END 
       SUBROUTINE  TIMDIF ( IDATE, NHRS, IRET )
C$$$  SUBPROGRAM DOCUMENTATION BLOCK                       
C                                                          
C SUBPROGRAM: TIMDIF         COMPUTES NUMBER OF HOURS IN TIME INTERVAL
C   PRGMMR: BOB HOLLERN      ORG: W/NP12     DATE: 00-02-10
C                                                          
C ABSTRACT: COMPUTES THE NUMBER OF HOURS FOR THE TIME PERIOD OF THE
C   CURRENT SUMAC PARAMETER DATA.
C
C PROGRAM HISTORY LOG:                                     
C   00-02-10  BOB HOLLERN, AUTHOR
C                                                          
C USAGE:  CALL TIMDIF ( IDATE, NHRS, IRET )
C                                                          
C   INPUT ARGUMENT LIST:                                   
C                                                          
C     IDATE    -   ARRAY CONTAINING THE STARTING AND ENDING
C                  VERIFICATION DATE/TIME DATA
C                  
C   OUTPUT ARGUMENT LIST:                                   
C
C     NHRS     -   NUMBER OF HOURS IN TIME PERIOD OF THE CURRENT
C                  SUMAC PARAMETER DATA
C
C     IRET     -   RETURN CODE VARIABLE SET TO 0, IF NO PROBLEMS; SET   
C                  TO 1, IF PROBLEMS
C
C
C ATTRIBUTES:                                              
C   LANGUAGE: FORTRAN 90                                   
C$$$                                                       
       INTEGER (4)   IDATE(2)

       REAL (8)   RINC(5)

       INTEGER(4) BDATE,BYEAR,BMONTH,BDAY,BHOUR

       INTEGER(8) CDATE(8), DDATE(8)

       IRET = 0

       CALL SW3FS1(IDATE(1),BYEAR, BMONTH, BDAY, BHOUR, 1 )

       CDATE(1) = BYEAR
       CDATE(2) = BMONTH
       CDATE(3) = BDAY
       CDATE(4) = -500
       CDATE(5) = BHOUR
       CDATE(6) = 0
       CDATE(7) = 0
       CDATE(8) = 0

       CALL SW3FS1(IDATE(2),BYEAR, BMONTH, BDAY, BHOUR, 1 )

       DDATE(1) = BYEAR
       DDATE(2) = BMONTH
       DDATE(3) = BDAY
       DDATE(4) = -500
       DDATE(5) = BHOUR
       DDATE(6) = 0
       DDATE(7) = 0
       DDATE(8) = 0

       IT = 2

       DO I = 1,5
         RINC(I) = 0.0
       END DO

C      COMPUTE THE ELASPSED TIME IN HOURS FOR THE PERIOD FROM
C      CDATE TO DDATE

       CALL  W3DIFDAT ( DDATE, CDATE, IT, RINC )

       NHRS = RINC(2) + .5

       RETURN
       END
       SUBROUTINE  FMODEL( INMDL, MDATE, MX, MDLNM1, MDLNM2, IRET )
C$$$  SUBPROGRAM DOCUMENTATION BLOCK                       
C                                                          
C SUBPROGRAM: FMODEL         CONVERTS SUMAC MODEL NAME TO VSDB MDL NAME
C   PRGMMR: BOB HOLLERN      ORG: W/NP12     DATE: 00-02-10
C                                                          
C ABSTRACT: CONVERTS THE NAMES OF THE FORECAST MODELS USED IN THE
C   SUMAC PROGRAM TO THE CORRESPONDING VSDB MODEL NAMES.
C
C PROGRAM HISTORY LOG:                                     
C   00-02-10  BOB HOLLERN, AUTHOR
C                                                          
C USAGE:  CALL VSDB ( INMDL, MDATE, MX, MDLNM1, MDLNM2, IRET )
C                                                          
C   INPUT ARGUMENT LIST:                                   
C                                                          
C     INMDL    -   THE SUMAC MODEL NAME
C
C     MDATE    -   STARTING DATE/TIME OF THE SUMAC VERIFICATION DATA
C                  
C   OUTPUT ARGUMENT LIST:                                   
C
C     MX       -   NUMBER OF NON-BLANK CHARACTERS IN MODEL NAME
C
C     MDLNM1   -   VSDB MODEL NAME
C
C     MDLNM2   -   VSDB MODEL NAME IN LOWER CASE USED AS PART OF THE
C                  FILE NAME OF THE FILE WHERE THE CURRENT VSDB MODEL
C                  DATA WILL BE WRITTEN
C
C     IRET     -   RETURN CODE VARIABLE SET TO 0, IF NO PROBLEMS; SET   
C                  TO 1, IF PROBLEMS
C
C ATTRIBUTES:                                              
C   LANGUAGE: FORTRAN 90                                   
C$$$                                                       
       INTEGER (4)  MDATE,  NDATE

       CHARACTER*8  MDLNM1,  MDLNM2,  LTABLE(11),  NTABLE(11)
       CHARACTER*8  BLANKS

       CHARACTER*4  INMDL,  MTABLE(11)

       DATA  BLANKS  / '        ' /

       DATA  MTABLE  / 
     +                 'GFS ',  'GFSX',
     +                 'LRG ',  'LRGN',  'LRGS',
     +                 'REG ',  'REGL', 
     +                 'UKM ',  'UKMO',  'UKMS',  'UKMX'  /

       DATA  NTABLE  / 
     +             'GFS     ',  'GFS     ',
     +             'GFS     ',  'GFS     ',   'GFS     ',
     +             'LFM     ',  'LFM     ', 
     +             'UKM     ',  'UKM     ',   'UKM     ',  'UKM     '/

       DATA  LTABLE  / 
     +             'gfs     ',  'gfs     ',
     +             'gfs     ',  'gfs     ',   'gfs     ',
     +             'lfm     ',  'lfm     ', 
     +             'ukm     ',  'ukm     ',   'ukm     ',  'ukm     '/

       IRET = 0

       NDATE = MDATE / 10000

       MDLNM1 = BLANKS
       MDLNM2 = BLANKS

C      PRINT *, 'FMODEL:  NDATE = ', NDATE
C      PRINT *, 'FMODEL:  INMDL = $$', INMDL,'$$'

       DO  I = 1,11
          IF ( INMDL .EQ. MTABLE(I) ) THEN
            MDLNM1 = NTABLE(I)
            MDLNM2 = LTABLE(I)

C           IF THE SUMAC MODEL IS THE "REG" OR "REGL", THEN MDLNM1
C           CAN BE EITHER THE LFM OR THE ETA MODEL. THE ETA MODEL
C           REPLACED THE LFM MODEL ON JUNE 6, 1993 AT 12Z.

            IF ( NDATE .GE. 199306 .AND. MDLNM1 .EQ. 'LFM     ' ) THEN
              MDLNM1 = 'NAM     '
              MDLNM2 = 'nam     '
            END IF

            M = 0

            DO J = 1, 8
                IF ( MDLNM1(J:J) .NE. ' ' ) THEN
                   M = M + 1
                END IF
            END DO

            MX = M

C          PRINT *, 'FMODEL:  MX = ', MX

            RETURN
          END IF
       END DO

       IRET = 1
       MDLNM1 = INMDL

       RETURN
       END
