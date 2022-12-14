      

      SUBROUTINE ACCUMU(NF,MKR)

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    ACCUMU      ACCUMULATE PARTIAL ERROR TERMS
C   PRGMMR: VLCEK            ORG: W/NP12    DATE: 99-05-26 
C
C ABSTRACT: ACCUMULATES THE VARIOUS PARTIAL TERMS OVER ALL THE 
C    STATIONS EACH GIVEN AREA FOR THE NF-TH FORECAST FILE (HOURS) 
C    BEING VERIFIED.  THE ACCUMULATIONS ARE THEN SENT TO SUB- 
C    ROUTINE PRTSTA TO COMPUTE AND PRINT FINAL STATISTICS, AND/OR 
C    TO SUBROUTINE WRTSTA TO COMPUTE AND WRITE THESE STATISTICS  
C    ONTO A SEQUENTIAL FILE. 
C 
C
C PROGRAM HISTORY LOG:
C   76-??-??  STACKPOLE 
C   89-01-25  VLCEK       CHANGE FROM FORTRAN 66 TO VS FORTRAN 77
C   91-09-??  VLCEK       ADD OPTION TO DISABLE DATE CHECK
C   94-06-24  VLCEK       DELETE REFERENCES TO ACCUMULATION FILE. 
C                         REMOVE CONDITION THAT FORECAST FILE NAME
C                         BEGIN WITH LETTER /X/ TO WRITE STATS.
C   96-06-25  Y. ZHANG    MOVE IT TO CRAY AND REFINE IT.
C   98-07-14  VLCEK       COMPILE IN FORTRAN 90 -- NO CODE CHANGE.
C   99-05-26  VLCEK       MOVE TO IBM RS6000.
C   
C USAGE:    CALL ACCUMU(NF)
C   INPUT ARGUMENT LIST:
C     NF       - THE NF-TH FORECAST FILE BEING VERIFIED IN THIS RUN 
C
C   OUTPUT ARGUMENT LIST:  NONE
C 
C   INPUT FILES: NONE
C 
C   OUTPUT FILES:
C       FT06F001 - VARIOUS STATUS AND ERROR MESSAGES.  STATISTICS 
C                  PERTAINING TO VERIFICATIONS ARE OBTAINED BY 
C                  CALLING ANOTHER SUBROUTINE, /PRSTAT/.
C
C REMARKS: DATA PERTAINING TO VERIFICATION OF CURRENT FORECASTS IS 
C          BROUGHT IN VIA COMMON BLOCKS. 
C
C ATTRIBUTES: 
C   LANGUAGE: FORTRAN 90    IBM FORTRAN
C   MACHINE:  IBM RS6000
C
C$$$ 
CC
CC       THE ARRAY BLKID, WHICH WAS READ IN FROM AN ACCUMULATION FILE
CC       IN ACUMVER4, IS SIMPLY INITIALIZED HERE AS FOLLOWS:
CC    
CC            1.   AREA NUMBER 
CC            2.   NO. OF STATIONS IN AREA + 1  (FOR SHIP REPORTS)
CC            3,4  LONG MIN AND MAX FOR AREA 
CC            5,6  LATITUDE MIN AND MAX DITTO 
CC            7,8  CURRENT DATE (TWICE, PACKED-O.N. 29)
CC            9,10 MAP TYPE K, GENERATING CODE G 
CC           11,12 AREA NAME (CHARACTERS) 
CC   
CC       THE ID WORD IS ALL IN FLOATING POINT FOLLOWED BY
CC       A 2-D ARRAY CONTAINING SUM(S1) AND SUM(N) 
CC       FOR THE S1 VARIABLES AND LEVELS, FOLLOWED BY 
CC       A 1-D ARRAY  WITH THE BLOCK-STN NUMBER OF THE
CC       STATIONS IN THE AREA  (IN FIXED POINT INTEGER FORM),
CC       FOLLOWED BY A 3-D ARRAY THUS...  (REAL*8) 
CC         I - INDEX FOR STATIONS PLUS ONE FOR SHIPS 
CC         J - VARIABLES PARTIAL SUMS AND NO OF ACCUMULATED QUANTITIES
CC         K - LEVELS (PRESSURE)
CC 

C     /G/ IS RESERVED FOR CONSTANTS NAMED IN A PARAMETER STATEMENT 
C     CONSTANTS NAMED IN PARAMETER STATEMENTS ARE TAKEN FROM THE
C     ORIGINAL PL1 DECLARATIONS LISTED BELOW.  THE CHARACTER /#/ 
C     IS CONVERTED TO THE CHARACTER /G/ (SEE IMPLICIT STATEMENT
C     ABOVE).  SOME NAMES HAVE TO BE ABBREVIATED TO STAY WITHIN 
C     THE SIX CHARACTER LIMIT; THESE ARE SHOWN LEFT OF THE OLD NAME.

C  GNTRY  #NTREE    CHAR,     /* NO OF ENTREES IN RA FILE */
C  GNTRYP #NTREEP   CHAR, 
C  GNTRID #NTREEID  CHAR,
C  GORFIL #FORFIL   CHAR,    /* MAX NO OF FILES TO BE VERIFIED */
C         #NVAR     CHAR,    /* NO OF VARIABLES TO  VERIFY */
C         (#Q1,#Q2,#Q3,#Q4,#Q5,       /* FOR ON84 Q VALUES FIXED DEC */
C         #Q6,#Q7,#Q8,#Q9,#Q10) CHAR, /* DESIRED FOR VERIFICATION */
C  GVECTV #VECTVAR  CHAR,    /* VARIABLE WHICH IS A VECTOR */
C         #NVARP    CHAR,    /* FOR DIMENSIONS */
C         #NLEV     CHAR,    /* NO OF LEVELS TO INCLUDE */ 
C         (#P1,#P2,#P3,#P4,#P5,     /*  FOR DESIRED PRESSURE LEVELS*/
C         #P6,#P7,#P8,#P9,#P10,     /*  FIXED DEC MB  */
C         #P11,#P12,#P13,#P14,#P15)  CHAR, /* PUT VALUE AT MAN LEVL */
C         #IMAX     CHAR,   /* MAX DIMENSIONS OF FIELD */
C         #JMAX     CHAR,   /* TO BE VERIFIED */
C  GMXDIM #MAXDIM   CHAR,   /* PRODUCT OF IMAX*JMAX */
C  GMXSTN #MAXSTN   CHAR,   /* MAX NO OF ADP STNS POSSIBLE */
C         #INADP    CHAR,   /* INPUT FILE NO FOR ADP FILE */
C         #NMAPS    CHAR,   /* NO OF VERIFIABLE GRIDS */
C  GNERQU #NERQUA   CHAR    /* NO OF ERROR QUANTITIES */ 
C  GNGRQU #NGRQUA CHAR      /* NO OF GRADIENT QUANTITIES  
C                              AND NO OF S1 SCORES AND COUNTS */
C  GACCUM #NACCUM CHAR      /* NO OF ACCUMULATING QUANTITIES 
C                                 AND COUNTS OF SAME */ 
C  GMXTBL #MXSTBL CHAR      /* MAX NO OF STATIONS IN BLOCK/AREA */ 
C  GVRBLK #VERBLK CHAR      /* DIMENSION OF ONE AREA BLOCK */
C  GNGRQ1 #NGRQUAP11 CHAR   /* FOR EQUIVALENCING INTO AREA BLOCK */
C  GSTBLK #STRTBLK CHAR     /* DITTO */
C         #NPKD   CHAR      /* SIZE OF PACKED ADP BLOCK */
C         #NUPD   CHAR      /* UNPACKED ADP REPORT DIMENSION */
C  GMINBN #MINBNSN CHAR     /* MIN BLOCK STATION NUMBER TO SAVE */ 
C  GMAXBN #MAXBNSN CHAR     /* MAX BLOCK STATION NUMBER TO SAVE */
C  GSVBL  #SVBLAB CHAR      /* SIZE OF VERIF BLOCK LABEL */
C  GSVBL1 #SVBLABP1 CHAR    /* DITTO + 1 FOR EQUIVALENCE */
C         #TWO CHAR         /*FIRST DIMENSION OF ARRAY COMPUTING MEAN*/
C         #PROD CHAR        /* PRODUCT OF DIMENSIONS OF STNDEV ARRAY */
C         #TIMES CHAR       /* PRODUCT OF DIMENSIONS OF SUM ARRAY    */
C         #MVAR CHAR        /* NUMBER OF ELEMENTS IN EACH UPA REPORT */
C  GSXTN  #SIXTEN CHAR      /* SIZE OF LOCBLK ARRAY */                 
C         #THREE CHAR       /* NUMBER OF WORDS IN EOF */ 
C         #EIGHT CHAR       /* NUMBER OF WORDS IN LABEL */
C  GPROW  #PAGEROW CHAR     /* SECOND DIMENSION OF PAGE ARRAY */
C  GARK   #ARKSUMAC CHAR    /* FT NUMBER FOR SUMAC ARCHIVE OUTPUT */
C
C     GQ1 IS HEIGHT, GQ2 IS TEMPERATURE, GQ3 IS RELATIVE HUMIDITY.    
C     GQ4 IS U-COMPONENT OF WIND; V-COMPONENT IS ASSUMED TO BE GQ4+1. 
C     REMAINING GQ VALUES ARE RESERVED FOR FUTURE USE.                

      IMPLICIT  INTEGER(G)

      PARAMETER  (GORFIL=22, GNVAR=4, GVECTV=4)
      PARAMETER  (GQ1=7, GQ2=11, GQ3=52, GQ4=33, GQ5=34)
      PARAMETER  (GQ6=0, GQ7=0, GQ8=0, GQ9=0, GQ10=0)
      PARAMETER  (GNVARP=(GNVAR+1), GNLEV=4)
      PARAMETER  (GP1=0, GP2=850, GP3=0, GP4=500, GP5=0, GP6=0)
      PARAMETER  (GP7=250, GP8=0, GP9=0, GP10=100, GP11=0, GP12=0)
      PARAMETER  (GP13=0, GP14=0, GP15=0)
      PARAMETER  (GIMAX=360, GJMAX=181, GMXDIM=(GIMAX*GJMAX) )
      PARAMETER  (GMXSTN=1100, GMINBN=0, GMAXBN=99999)
      PARAMETER  (GNERQU=10, GNGRQU=6, GACCUM=(GNERQU*2) )
      PARAMETER  (GMXTBL=600, GSVBL=12,  GNMAPS=17 )
      PARAMETER  (GMXARA=15, GMXRUN=15)

C     COMMON FOR AREAS AND STATION LISTS (DATA READ IN MAIN)         

      COMMON /AREAS / ALOLA(GMXARA,4)  , AREA(GMXARA)  ,
     &                KSTN(GMXARA)     , STNLST(GMXARA , GMXTBL)
      COMMON /RAFILE/ FORFIL(GORFIL)   , NFILE         , GCODE ,
     &                DATA(GMXDIM)     , DATA1(GMXDIM) , MINWND,
     &                MAXWND, IGRID    , VDATE1, VDATE2,
     &                VDATE , DIAG     , PRSTAT, WRSTAT,
     &                INTPM , KRUN     , FHR(GORFIL)
      COMMON /RADTAB/ RUNTAB(GMXRUN)   , ARATAB(GMXARA,GMXRUN) ,
     &                                   FHRTAB(GORFIL,GMXRUN) ,
     &                                   INDARA(GMXARA,GMXRUN) ,
     &                DOSTAT(GMXARA    , GORFIL,GMXRUN)
      COMMON /ADPSTN/ IDSTN(GMXSTN)    , ALON(GMXSTN)  , ALAT(GMXSTN) ,
     &                NRPT(GMXSTN)     , DISTNN(GMXSTN),
     &                ZTRUV(GMXSTN,GNVARP,GNLEV),
     &                STI(GMXSTN), STJ(GMXSTN)  , JSTA
      COMMON /MAPINF/ NSTA, KMAP , IMXMAP       , JMXMAP,
     &                NCYCMP     , F9, DATENL

C     COMMON FOR ERRORS AND GRADIENT FOR EACH STATION AND LEVEL      

      COMMON /STNPRT/ STNERR(GMXSTN,GNERQU,GNLEV),
     &                STNGRD(GMXSTN,GNGRQU,GNLEV)

      REAL         STATS , MINWND, MAXWND, ALOLA
      INTEGER      RPT   , KMAP  , GCODE , STNLST, NFILE, VDATE1
      INTEGER      VDATE2, VDATE , IGRID , KSTN
      CHARACTER(8) FORFIL, VFILE , C8AREA, AREA  , BLANK, ARATAB
      CHARACTER(4) FHR   , KRUN  , RUNTAB, FHRTAB, BLANK4
      CHARACTER(1) INDARA, BLANK1, DOSTAT
      LOGICAL      PRSTAT, WRSTAT, MATCH , OUT  , DIAG

      DIMENSION    BLKID(GSVBL), COUNT(GACCUM,GNLEV), S1(GNGRQU,GNLEV),
     &             STATS(GACCUM,GNLEV), SMATCH(GNGRQU,GNLEV) ,
     &             DEV(GACCUM,GNLEV)

      EQUIVALENCE  (C8AREA,BLKID(11))

      DATA         BLANK /'        '/, BLANK4/'    '/,BLANK1/' '/

      IF(DIAG) PRINT 9999
 9999 FORMAT (T15,'BEGIN SUBROUTINE ACCUMU ....'//)

C     CHECK IF INPUT FORECAST VALID TIME CAN BE PROCESSED

      DO MF=1,GORFIL
      MKH = MF
      IF(FHR(NF).EQ.FHRTAB(MF,MKR)) GO TO 5
      END DO
      PRINT *,'FOR FORECAST HOUR ',FHR(NF),
     &        ' HAVE TO BE ADDED IN FHRTAB.'
      CALL EREXIT(2)
   5  CONTINUE

C     FRIST DO IS OVER ALL AVAILABLE AREAS (MAX OF 15)               

      DO 250 NA = 1,GMXARA
      IF(AREA(NA).NE.BLANK) THEN

         DO IA=1,GMXARA
         MKA = IA
         IF(AREA(NA).EQ.ARATAB(IA,MKR)) GO TO 15
         END DO
         IF(DIAG)
     &   PRINT *,'VERIFICATION OF ',FHR(NF),' HOURS FORECASTS ',
     &           'FOR AREA ',AREA(NA),' HAVE NOT BEEN DONE,'
         IF(DIAG) PRINT *,'BECAUSE SCHEDULE DOES NOT REQUEST IT.'
         GO TO 250
  15     CONTINUE

         CALL GETDDN(KRUN,INDARA(MKA,MKR),1,FHR(NF),VFILE)

         IF(KRUN.EQ.'LRG ') THEN
            IF(AREA(NA).EQ.'TROPICS ') THEN
               CALL GETDDN('GFS ',INDARA(MKA,MKR),1,FHR(NF),VFILE)
            END IF
         END IF

         IF(DOSTAT(MKA,MKH,MKR).NE.'T') GO TO 250

C        RESET (TO ZERO) COMPONENTS OF STATS FOR SUMMATION          
C        OVER STATIONS IN AREA.                                     

C        DO 30 I=1,GNLEV
C        DO 20 J=1,GACCUM
         STATS = 0.
         COUNT = 0.
C  20    CONTINUE
C        DO 25 J=1,GNGRQU
         SMATCH = 0.
         S1 = 0.
C  25    CONTINUE
C  30    CONTINUE

C        ASSIGN HEADER INFO TO BLKID ARRAY FOR PASSAGE              
C        TO SUBROUTINES PRTSTA AND WRTSTA                           

         C8AREA   = AREA(NA)
         BLKID(1) = FLOAT(NA)
         BLKID(2) = FLOAT(KSTN(NA))
         BLKID(3) = ALOLA(NA,1)
         BLKID(4) = ALOLA(NA,2)
         BLKID(5) = ALOLA(NA,3)
         BLKID(6) = ALOLA(NA,4)
         BLKID(7) = DATENL
         BLKID(8) = DATENL
         BLKID(9) = FLOAT(KMAP)
         BLKID(10)= FLOAT(GCODE)
C         BLKID(11)= AREA NAME (FIRST 4 CHARS)  (EQUIVALENCED        
C         BLKID(12)= AREA NAME (LAST  4 CHARS)      TO C8AREA)       

C        HERE WE GO AGAIN ...                                       
C        GO THRU  ADP DATA AND CORRESPONDING ERROR TERMS.           
C        SEARCH LIST OF ALL ADP STATIONS (IN IDSTN) FOR             
C        STATIONS IN THIS PARTICULAR AREA (IN STNLST).              
C        WATCH OUT FOR STNLST STATIONS MISSING FROM IDSTN.          
C        AND TAKE ADVANTAGE THAT BOTH STATION LISTS ARE ORDERED.    

         NSTB = IFIX(BLKID(2) - 1.)
         IDTOP = 1
         DO 110 IB=1, NSTB
         DO 90 ID=IDTOP, JSTA
         IDX = ID
         IF(IDSTN(ID).EQ.STNLST(NA,IB)) THEN

C           MATCH FOUND - ACCUMULATE STATS                       

            MATCH = .TRUE.
            DO 60 NL = 1,GNLEV
            DO 50 NV=1,GNERQU,2
            LNV = NV*2 - 1
            IF(ABS(STNERR(ID,NV,NL)).LT.F9-1.) THEN

               STATS(LNV,NL)   = STATS(LNV,NL)  + STNERR(ID,NV,NL)
               COUNT(LNV,NL)   = COUNT(LNV,NL)  + 1.
               STATS(LNV+1,NL) = STATS(LNV+1,NL)+ STNERR(ID,NV+1,NL)
               COUNT(LNV+1,NL) = COUNT(LNV+1,NL)+ 1.
               STATS(LNV+2,NL) = STATS(LNV+2,NL)+ (STNERR(ID,NV,NL))**2
               COUNT(LNV+2,NL) = COUNT(LNV+2,NL)+ 1.
               STATS(LNV+3,NL) = STATS(LNV+3,NL)
     &                         + (STNERR(ID,NV+1,NL))**2
               COUNT(LNV+3,NL) = COUNT(LNV+3,NL)+ 1.
            END IF
   50       CONTINUE
   60       CONTINUE

C           NOW DO S1 SCORES, PARTLY                             

            DO 66 NL = 1,GNLEV
            DO 64 NV = 1,GNGRQU,2
            IF((ABS(STNGRD(ID,NV,NL)).LT.F9-1.).AND.
     &         (ABS(STNGRD(ID,NV+1,NL)).LT.F9-1.)) THEN
               FG            = STNGRD(ID,NV,NL)
               OG            = STNGRD(ID,NV+1,NL)
               S1(NV,NL)     = S1(NV,NL)  + ABS(FG-OG)
               S1(NV+1,NL)   = S1(NV+1,NL)+ AMAX1(ABS(FG),ABS(OG))
               SMATCH(NV,NL) = SMATCH(NV,NL) + 1.
            END IF
   64       CONTINUE
   66       CONTINUE

C           DO NOT COMPLETE ID LOOP , UNNECESSARY                
C           SINCE WE HAVE FOUND MATCH - NON STRUCTURED, SORRY    

            GO TO 100
         ENDIF

C        NO MATCH - CHECK TO SEE IF WE HAVE EXCEEDED THE AREA STATION
C        NUMBER IN THE MASTER LIST - IF SO LEAVE ID DO LOOP 
C        AND ADJUST IDTOP FOR NEXT PASS NON-STRUCTURED, SORRY

         MATCH = .FALSE.
         IF(IDSTN(ID).GT.STNLST(NA,IB)) GO TO 100
   90    CONTINUE

C        FALL THRU DO ID CONTINUE ONLY IF AREA                    
C        STATION NUMBER BEING SOUGHT (IB)                         
C        IS LARGER THAN LARGEST IDSTN STATION.                    
C        IF THAT HAPPENS THIS MEANS WE ARE ALL DONE --            
C        WE CAN SAY THAT BECAUSE WE (IN PROGRAM QCADP)            
C        GIVE SHIP REPORTS A STATION BLKSTN NUMBER LARGER         
C        THAN ANY NORMAL STATION REPORTS SO MAKE A NON-           
C        STRUCTURED DEPARTURE FROM DO IB LOOP AND BYPASS          
C        SECTION TO ACCUMULATE SHIP REPORT STATISTICS.            

         GO TO 180
  100    CONTINUE
         IDTOP = IDX + 1
         IF (.NOT.MATCH) IDTOP = IDTOP - 1
         IF (IDTOP.GT.JSTA) GO TO 180
  110    CONTINUE

C        NOW ON FOLLOW THRU HAVE COMPLETED ALL LAND STATIONS        
C        AND IDTOP INDEXES THE NEXT REPORT IN IDSTN IF THERE;       
C        NEXT ACCUMULATE SHIP REPORTS (IF ANY).                     

C        FIRST TEST IF THERE ARE ANY MORE REPORTS.                  
C        IF SO THEN SEARCH ON THRU IDSTN FOR                        
C        SHIP REPORTS (LARGE BLOCK/STN NUMBERS)                     
C        AND IF FOUND ACCUMULATE THOSE THAT ARE IN OUR AREA         
C        AS DEFINED BY LOLA LIMITS.                                 

         IF(IDTOP.LT.JSTA) THEN
            NSTB = NSTB + 1
            DO 170 ID=IDTOP,JSTA
            IF(IDSTN(ID).GT.99999) THEN
               OUT = .TRUE.

C              TEST LATITUDE LIMITS OF SHIP REPORT                  

               IF((ALAT(ID).GE.BLKID(5)).AND.
     &            (ALAT(ID).LE.BLKID(6))) THEN

C                 TEST LONGITUDE LIMITS TWO WAYS                     
C                 DEPENDING UPON WHETHER LIMITS CROSS THE PRIME      
C                 (ZERO DEG) MERIDIAN OR NOT (IN THAT ORDER)         

                  IF(BLKID(3).GT.BLKID(4)) THEN
                     IF((ALON(ID).GE.0.).AND.
     &               (ALON(ID).LE.BLKID(4))) OUT = .FALSE.
                     IF((ALON(ID).GE.BLKID(3)).AND.
     &               (ALON(ID).LE.359.9)) OUT = .FALSE.
                  ELSE
                     IF((ALON(ID).GE.BLKID(3)).AND.
     &               (ALON(ID).LE.BLKID(4))) OUT = .FALSE.
                  END IF
               END IF
               IF(.NOT.OUT) THEN
                  DO 140 NL=1,GNLEV
                  DO 130 NV=1,GNERQU,2
                  IF(STNERR(ID,NV,NL).EQ.F9) GO TO 120
                  LNV             =    NV*2 - 1
                  STATS(LNV,NL)   = STATS(LNV,NL)  + STNERR(ID,NV,NL)
                  COUNT(LNV,NL)   = COUNT(LNV,NL)  + 1.
                  STATS(LNV+1,NL) = STATS(LNV+1,NL)+ STNERR(ID,NV+1,NL)
                  COUNT(LNV+1,NL) = COUNT(LNV+1,NL)+ 1.
                  STATS(LNV+2,NL) = STATS(LNV+2,NL)+
     &                              (STNERR(ID,NV,NL))**2.
                  COUNT(LNV+2,NL) = COUNT(LNV+2,NL)+ 1.
                  STATS(LNV+3,NL) = STATS(LNV+3,NL)
     &                            + (STNERR(ID,NV+1,NL))**2.
                  COUNT(LNV+3,NL) = COUNT(LNV+3,NL) + 1.
  120             CONTINUE
  130             CONTINUE
  140             CONTINUE

C                 NOW THE SHIP CONTRIBUTION TO S1                    

                  DO 146 NL = 1,GNLEV
                  DO 144 NV = 1,GNGRQU,2
                  IF((ABS(STNGRD(ID,NV,NL)).LT.F9-1.).AND.
     &               (ABS(STNGRD(ID,NV+1,NL)).LT.F9-1.)) THEN
                     FG            = STNGRD(ID,NV,NL)
                     OG            = STNGRD(ID,NV+1,NL)
                     S1(NV,NL)     = S1(NV,NL)  + ABS(FG-OG)
                     S1(NV+1,NL)   = S1(NV+1,NL)+ AMAX1(ABS(FG),ABS(OG))
                     SMATCH(NV,NL) = SMATCH(NV,NL) + 1.
                  END IF
  144             CONTINUE
  146             CONTINUE
               END IF
            END IF
  170       CONTINUE
         END IF

C        NON-STRUCTURED EXIT COMES HERE (TO 180).                 

  180    CONTINUE

C        ALL DONE WITH ACCUMULATING FOR THIS PARTICULAR AREA --     
C        PRINT AND/OR WRITE STATISTICS FOR THIS AREA AND HOUR.      

         IF(MINWND.GT.30) CALL VERJET (STATS, S1, SMATCH, BLKID,
     &                           VFILE, NF, COUNT, DEV)
         IF(PRSTAT) CALL PRTSTA (STATS, S1, SMATCH, BLKID,
     &                           VFILE, NF, COUNT, DEV)
         IF(WRSTAT) CALL WRTSTA (STATS, S1, SMATCH, BLKID,
     &                           VFILE, NF, COUNT, DEV)
      ELSE

C        ALL DONE WITH ALL AREAS SELECTED FOR THIS HOUR --            
C        MAKE NON STRUCTURED DEPARTURE FROM NA LOOP                   

         GO TO 260
      ENDIF

  250 CONTINUE

C     ALL DONE WITH ALL AREAS                                        

  260 CONTINUE
      RETURN
      END
