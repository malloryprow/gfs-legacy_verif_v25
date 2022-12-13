      SUBROUTINE ADPTOS
C$$$  SUBPROGRAM  DOCUMENTATION  BLOCK                                  
C                .      .    .                                       .  
C SUBPROGRAM:    ADPTOS      TOSS OUT BAD RADIOSONDE DATA               
C   PRGMMR: STACKPOLE        ORG: W/NP12    DATE: 96-05-06
C                                                                       
C ABSTRACT: LOOKS THROUGH DATA AND FLAGS BAD DATA WHEN ENCOUNTERED,     
C           ARCHIVES CHECKED DATA FIELDS AT USERS OPTION.               
C                                                                       
C PROGRAM HISTORY LOG:                                                  
C   76-??-??  STACKPOLE                                                 
C   85-10-10  VLCEK       CHECK U AND V SEPARATELY, RATHER THAN SPEED.  
C   86-10-16  VLCEK       UTILIZE QUALITY MARKERS (READ IN WITH ADP     
C                         DATA IN O.N 85 FORMAT; SEE SUB /GETADP/).     
C   87-09-23  VLCEK       INCLUDE OPTION TO SAVE SELECTED ADP DATA      
C                         FOLLOWING TOSSOUT PROCEDURES (ON FT60).       
C   89-01-25  VLCEK       CHANGE FROM FORTRAN 66 TO VS FORTRAN 77.      
C   89-08-19  VLCEK       ADD SEQ FILE (FT70F001) TO HOLD TOSSOUT       
C                         RECORDS.                                      
C   90-06-14  VLCEK       FT60 AND FT70 OUTPUT WILL BE WRITTEN;         
C                         USE DD DUMMY IF NOT DESIRED.                  
C                                                                       
C   96-05-06  Y. ZHANG    CHANGE READING DIRECT ACCESS ANALYSIS FIELDS
C                         FILE INTO READING GRIB ANALYSIS FIELDS FILE,
C                         CHANGE THE METHOD OF INTERPOLTED ANALYZED FIELD 
C                         TO LOCATIONS OF REPORTS AND REFINE IT. COMMENTS
C                         SUBROUTINE ROTWND BECAUSE SUBROUTINE IPOLATEV
C                         (DO INTERPOLATING) HAS IN ADVANCE DONE ROTATING 
C                         WIND DIRECTIONS.
C
C   98-07-10  VLCEK       MAKE Y2K COMPATIBLE AND COMPILE IN F90
C   99-05-26  VLCEK       MAKE COMPATIBLE WITH IBM RS6000 AND COMPILE.
C                                                                       
C USAGE:    CALL ADPTOS                                                 
C   INPUT ARGUMENT LIST: NONE                                           
C                                                                       
C   OUTPUT ARGUMENT LIST: NONE                                          
C                                                                       
C   INPUT FILES:                                                        
C     ANL      - GRIB ANALYSIS FILE (OR F00) USED AS CONTROL         
C                IN TOSSOUTS.                                           
C                                                                       
C   OUTPUT FILES:                                                       
C     FT60F001 - SELECTED AND QUALITY-CHECKED RADIOSONDE DATA,          
C                ALONG WITH TIME, DATE, AND SUNDRY IDENTIFIERS.         
C                SEE COMMENTS BELOW DOCBLOCK FOR FURTHER DETAILS.       
C     FT70F001 - SEQUENTIAL FILE HOLDING LIST OF STATIONS WHICH         
C                HAVE HAD BAD DATA TOSSED AND TYPE OF DATA TOSSED       
C                (E.G. 500 MB HEIGHTS, 250 MB WINDS, ETC.).  DATE       
C                RECORD HEADS EACH LIST.  THIS FILE PROVIDES BASIS      
C                FOR MONTHLY TOSSOUT SUMMARY AND ARCHIVES.              
C     FT06F001 - DATE/TIME OF ADP FILE AND CORRESPONDING ANL FILE.      
C                FOR EACH LEVEL AND QUANTITY: NUMBER OF STATIONS        
C                ACCEPTED, NUMBER OF STATIONS TOSSED, MEAN DIFF-        
C                ERENCE (ANL-OBS), TOSS THRESHHOLD, IDENT OF TOSSED     
C                STATIONS AND CORRESPONDING TOSSED VALUES.              
C                                                                       
C REMARKS: MOST DATA IS PASSED IN COMMON, SEE COMMENTS BELOW DOCBLOCK   
C          FOR DETAILS.
C                                                                       
C ATTRIBUTES:                                                           
C   LANGUAGE: VS FORTRAN 90    IBM FORTRAN                                         
C   MACHINE:  IBM RS6000
C                                                                       
C$$$                                                                    
C                                                                       
C                                                                       
C - - - - - - - - - I N P U T   V A R I A B L E S  - - - - - - - - -    
C                                                                       
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE   
C     -----       ----------------------------------        ---------   
C                                                                       
C     DATA        REAL * 8 ARRAY WHICH HOLDS NON-WIND       /RAFILE/    
C                 DATA AT THE PRESCRIBED ANALYSIS                       
C                 GRIDPOINTS. THIS DATA WILL BE                         
C                 INTERPOLATED TO THE OBSERVATIONS                      
C                 LOCATION.                                             
C                                                                       
C     DATA1       REAL * 8 ARRAY WHICH HOLDS WIND DATA      /RAFILE/    
C                 AT THE PRESCRIBED ANALYSIS GRID POINTS.               
C                 THIS DATA WILL BE INTERPOLATED TO THE                 
C                 OBSERVATIONS LOCATION.                                
C                                                                       
C     F9          REAL * 8 CONSTANT DENOTING MISSING DATA   /MAPINF/    
C                 F9=99999.                                             
C                                                                       
C     IAPPCK      INTEGER * 8 VARIABLE CONTAINING THE       /CFDATE/    
C                 PACKED DATE ON THE ADP OBSERVATION                    
C                 FILE.                                                 
C                                                                       
C     IDSTN       INTEGER * 8 ARRAY HOLDING STATION IDS.    /ADPSTN/    
C                                                                       
C     IMXMAP      INTEGER * 8 VARIABLE DENOTING THE         /MAPINF/    
C                 NUMBER OF COLUMNS IN THE DATA ARRAY.                  
C                                                                       
C     JMXMAP      INTEGER * 8 VARIABLE DENOTING THE         /MAPINF/    
C                 NUMBER OF ROWS IN THE DATA ARRAY.                     
C                                                                       
C     JSTA        INTEGER * 8 COUNTER OF THE NUMBER OF      /ADPSTN/    
C                 STATIONS WHICH HAVE THE PROPER ID.                    
C                                                                       
C     NCYCMP      INTEGER * 8 VARIABLE WHICH DEFINES        /MAPINF/    
C                 THE GRID AS BEING CYCLIC OR NOT                       
C                                                                       
C     QWANT       AN INTEGER * 8 ARRAY USED TO DETERMINE    /LEVELS/    
C                 THE QUANITY PORTION OF A FILES O.N.                   
C                 85 LABEL.                                             
C                                                                       
C     ANLD        CHAR * 9 VARIABLE DENOTING THE ANALYSIS   /RAFILE/    
C                 FILE TO WHICH ALL DATA ARE COMPARED                   
C                 FOR TOSS OUT CRITERIA.                                
C                                                                       
C     ANLI        CHAR * 9 VARIABLE DENOTING THE ANALYSIS   /RAFILE/    
C                 INDEX FILE 
C              
C     ZTRUV       REAL * 8 ARRAY WHICH HOLDS THE UNSCALED   /ADPSTN/    
C                 DATA                                                  
C                                                                       
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
C                                                                       
C - - - - - - - - - O U T P U T   V A R I A B L E S - - - - - - - - - - 
C                                                                       
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE   
C     -----       ----------------------------------        ---------   
C                                                                       
C     DATA        SEE DESCRIPTION IN THE INPUT LIST         /RAFILE/    
C                                                                       
C     DATA1       SEE DESCRIPTION IN THE INPUT LIST         /RAFILE/    
C                                                                       
C     DATENL      REAL * 8 VARIABLE DENOTING MOST RECENT    /MAPINF/    
C                 VERIFICATION DATE ALREADY COMPLETED.                  
C                 THIS IS NOT THE CURRENT VERIFICATION                  
C                 DATE BUT THE PREVIOUS DATE                            
C                                                                       
C     F9          SEE DESCRIPTION IN THE INPUT LIST         /MAPINF/    
C                                                                       
C     IAPPCK      SEE DESCRIPTION IN THE INPUT LIST         /CFDATE/    
C                                                                       
C     IMXMAP      SEE DESCRIPTION IN THE INPUT LIST         /MAPINF/    
C                                                                       
C     JMXMAP      SEE DESCRIPTION IN THE INPUT LIST         /MAPINF/    
C                                                                       
C     KMAP        INTEGER * 8 VARIABLE DENOTING THE         /MAPINF/    
C                 GRID TYPE OF THE ANALYSIS FILE                        
C                                                                       
C     NCYCMP      SEE DESCRIPTION IN THE INPUT LIST         /MAPINF/    
C                                                                       
C     NOTOSS      SEE DESCRIPTION IN THE INPUT LIST         /ISWTCH/    
C                                                                       
C     QWANT       SEE DESCRIPTION IN THE INPUT LIST         /LEVELS/    
C                                                                       
C     STVL        REAL * 8 ARRAY HOLDING A NON-WIND         /LOCAL/     
C                 GRIDPOINT VALUE OF THE ANALYSIS                       
C                 FIELD WHICH HAS BEEN INTERPOLATED                     
C                 TO THE OBSERVATION POINT                              
C                                                                       
C     STV1        REAL * 8 ARRAY HOLDING A WIND GRIDPOINT   /LOCAL/     
C                 VALUE OF THE ANALYSIS WHICH HAS BEEN                  
C                 INTERPOLATED TO THE OBSERVATION POINT                 
C                                                                       
C     ANLD        CHAR * 9 VARIABLE DEFINING THE ANALYSIS   /RAFILE/    
C                 FILE                                                  
C                                                                       
C     ANLI        CHAR * 9 VARIABLE DENOTING THE ANALYSIS   /RAFILE/    
C                 INDEX FILE 
C              
C     DIAGNOSTICS SUNDRY VARIABLES PRINTED WHEN ERRORS      UNIT 6      
C                 ARE ENCOUNTERED                                       
C                                                                       
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
C                                                                       
C - - - - - - - - - S U B P R O G R A M S   C A L L E D - - - - - - - - 
C                                                                       
C     NAME(S)                                               LIBRARY     
C     -------                                               -------     
C                                                                       
C     STIJ, STDEV, EREXIT                                   LOCAL       
C                                                                       
C     SQRT                                                  CRAY
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
C                                                                       
C     WHAT YOU SEE --- MAY NOT BE WHAT YOU GET.                
C     THERFORE SUB ADPTOS PURPOSE IS TO LOOK THROUGH THE DATA  
C     AND FLAG BAD DATA WHEN ENCOUNTERED.                      
C                                                                       
C     UTILIZING STATISTICAL COMPARISIONS BETWEEN DATA AND      
C     ANALYSES A GOODNESS OF FIT TEST IS APPLIED OF DATA TO    
C     ANALYSES (ANALYSES ASSUMED TO BE TRUE STATE OF           
C     ATMOSPHERE). STATIONS EXCEEDING THE PRESCRIBED TEST      
C     CRITERIA ARE THEN FLAGGED (-99999.), AS INDICATING BAD   
C     DATA.                                                    
C                                                                       
C     THE CHECKED ADP DATA FIELDS WILL BE WRITTEN ON A SE-     
C     QUENTIAL FILE (FT60). THE FIRST RECORD WILL CONTAIN THE  
C     KMAP VALUE (NEGATIVIZED FOR IDENTIFICATION), THE NUMBER  
C     OF STATIONS/RECORDS TO FOLLOW, THE PACKED AND UNPACKED   
C     DATES, SEVERAL VARIABLES NEEDED FOR MAPPING AND INTER-   
C     POLATION, AND 14 DUMMY WORDS.  EACH SUCCEEDING RECORD    
C     CONTAINS THE STATION ID, THE LATITUDE, LONGITUDE,        
C     I AND J GRIDPOINT VALUES, AND THE ID AND DISTANCE FROM   
C     AN ASSIGNED NEIGHBOR STATION (FROM SUB NEARPT) FOR S1    
C     CALCULATIONS.  THIS IS FOLLOWED BY THE STATION VALUES    
C     FOR HEIGHT, TEMPS, RH, AND U AND V WIND COMPONENTS AT    
C     850, 500, 250, AND 100 MB.                               

      IMPLICIT  INTEGER(G)

C     /G/ IS RESERVED FOR CONSTANTS NAMED IN A PARAMETER STATEMENT   
C     LIST AND EXPLANATION OF NAMES IS IN SUBROUTINE ACCUMU.         

C     GQ1 IS HEIGHT, GQ2 IS TEMPERATURE, GQ3 IS RELATIVE HUMIDITY.    
C     GQ4 IS U-COMPONENT OF WIND; V-COMPONENT IS ASSUMED TO BE GQ4+1. 
C     REMAINING GQ VALUES ARE RESERVED FOR FUTURE USE.                


      PARAMETER  (GNVAR=4, GVECTV=4)
      PARAMETER  (GQ1=7, GQ2=11, GQ3=52, GQ4=33, GQ5=34)
      PARAMETER  (GQ6=0, GQ7=0, GQ8=0, GQ9=0, GQ10=0)
      PARAMETER  (GNVARP=(GNVAR+1), GNLEV=4)
      PARAMETER  (GP1=0, GP2=850, GP3=0, GP4=500, GP5=0, GP6=0)
      PARAMETER  (GP7=250, GP8=0, GP9=0, GP10=100, GP11=0, GP12=0)
      PARAMETER  (GP13=0, GP14=0, GP15=0)
      PARAMETER  (GIMAX=360, GJMAX=181, GMXDIM=(GIMAX*GJMAX) )
      PARAMETER  (GMXSTN=1100, GMINBN=0, GMAXBN=99999)

      COMMON /CFDATE/ IAPPCK, IAPUNP(4)
      COMMON /LOCAL / STVL(GMXSTN) , STV1(GMXSTN)
      COMMON /WIND  / FU(GMXSTN)   , FV(GMXSTN)
      COMMON /LEVELS/ PLEV(GNLEV)  , IPL(GNLEV)    , QWANT(GNVARP)
      COMMON /RAFILE/ DATA(GMXDIM) , DATA1(GMXDIM) , IGRID,
     &                XS(4), RPT, ANLD, ANLI
      COMMON /ADPSTN/ IDSTN(GMXSTN), ALON(GMXSTN)  , ALAT(GMXSTN),
     &                NRPT(GMXSTN) , DISTNN(GMXSTN),
     &                ZTRUV(GMXSTN , GNVARP,GNLEV) ,
     &                STI(GMXSTN)  , STJ(GMXSTN)   , JSTA
      COMMON /MAPINF/ NSTA, KMAP   , IMXMAP        , JMXMAP,
     &                NCYCMP       , F9            , DATENL
C     COMMON /LLUNIT/ LULST, LUBFI, LUGBD, LUGBI, LUTOS, LUADP, LUOUT
      COMMON /PARMIN/ LPRDAT, INTPM, INDXA

      REAL         SZTRUV(GNVARP,GNLEV,3,2)
      REAL         DUMMY(14)
      INTEGER      IGRID  , RPT    , QWANT  , IDT(4)  , IDT3, PLEV
CCCCC INTEGER      JPDS(25)  , JGDS(22)  , KPDSI(25) , KGDSI(22)
      INTEGER      JPDS(27)  , JGDS(27)  , KPDSI(27) , KGDSI(27)
      INTEGER      KGDSO(22) , IBI(GNLEV), IBO(GNLEV), IPOPT(20)
      INTEGER      IPP(GNLEV), NDICTN(GMXSTN)
      INTEGER      LULST, LUBFI, LUGBD, LUGBI, LUTOS, LUADP, LUOUT
      CHARACTER(10) CHQ(GNVARP)
      CHARACTER(9)  ANLD,ANLI
      CHARACTER(1)  INDXA
      LOGICAL      VECTOR, LI(GMXDIM), LO, FIRST, LPRDAT

C######################################
       CHARACTER(8)   CCC,  clock_

       REAL(4)  ELASPSED, ELAS1, etime_
       TYPE TB_TYPE
          SEQUENCE
          REAL(4) USRTIME
          REAL(4) SYSTIME
       END TYPE
       TYPE (TB_TYPE) ETIME_STRUCT
      
C######################################



      DATA CHQ   / 'HEIGHT','TEMP.','REL. HUM.','U-COMP.','V-COMP.' /
      DATA DUMMY / 14*0./
      DATA LULST /9/, LUBFI /10/, LUGBD /11/, LUGBI /12/
      DATA LUADP /60/, LUTOS /70/, LUOUT /6/
   
C    INITIALIZE



      ANLD = ANLD(1:8)//CHAR(0)
      ANLI = ANLI(1:8)//CHAR(0)
      NSINN  = 0
      NSINS  = 0
      NPT    = 0
      F9N    = -F9
      VSIGMA = 0.
      VSGMAX = 0.
      NSIN   = 0.
      NSOUT  = 0.
      FIRST  = .TRUE.
      DO I = 1,GNLEV
        IPP(I) = PLEV(I)
      END DO
      SZTRUV = 999.

C     READ STATION ID OVER ONE SUBAREA
C      
C      DO I=1,GMXSTN
C      READ(7,'(I5)',END=10) NDICTN(I)
C      END DO
C 10   CONTINUE
C      NSTN = I-1
C 
C     OPEN ANALYSIS FILES; DIAGNOSTIC PRINT STATEMENTS COMMENTED OUT
C 
      CALL BAOPENR(LUGBD,ANLD,IRET)
C     PRINT *, 'BAOPENR: IRET = ', IRET
C     PRINT *, 'BAOPENR: LUGBD = ', LUGBD
      CALL BAOPENR(LUGBI,ANLI,IRET)
C     PRINT *, 'BAOPENR: IRET = ', IRET
C     PRINT *, 'BAOPENR: LUGBI = ', LUGBI

C     PRINT *, 'ANLD = ', ANLD
C     PRINT *, 'ANLI = ', ANLI

C     READ ANALYSIS                                                  
C     DO TOSS OUT COMPARISION BY VARIABLE AT EACH LEVEL.             

C             PRINT *, 'GNLEV = ', GNLEV

      DO 160 K=1,GNLEV

C             PRINT *, 'k = ', K
C             PRINT *, 'IPP = ', IPP(K)

      DO 150 NV = 1,GNVAR
      IF(NV.EQ.3.AND.IPP(K).LT.300) GO TO 150
      NVK = 10*K + NV
      JPDS    = -1
      JGDS    = -1
      IF (IGRID.NE.999)    THEN
        JPDS(3) = IGRID
      ENDIF
      JPDS(5) = QWANT(NV)
C               PRINT *, 'JPDS(5) = ', JPDS(5)
      JPDS(6) = 100
      JPDS(7) = IPP(K)
      JK = 0
      KR = 0
     
C     GRIB DATES: KPDSI(21) IS CURRENTLY 20(TH CENTURY) -- KPDSI(8)  
C     IS YEAR OF CENTURY RANGING FROM 1 TO 100.  PACK IT INTO IDT(1).
C     IAPPCK PASSED IN COMMON CFDATE IS NOW COMPLIANT
     
C     PRINT *, 'LUGBD = ', LUGBD
C     PRINT *, 'LUGBI = ', LUGBI
C     PRINT *, 'GMXDIM = ', GMXDIM
C     PRINT *, 'JK = ', JK
C     WRITE ( 6,122) (JPDS(III),III=1,27)
C 122     FORMAT(1X, 'JPDS = ', 6(/1X,5(I10,2X) ) )
C     WRITE ( 6,123) (JGDS(III),III=1,27)
C 123     FORMAT(1X, 'JGDS = ', 6(/1X,5(I10,2X) ) )

C############################################################
C        PRINT *, 'ADPTOS:  BEFORE CALL TO GETGB'

C        CCC = clock_()
C        PRINT *, 'CCC = ', CCC

C        PREVIOUS TIME VALUE
C        ELAS1 = ELAPSED

C        ELAPSED = etime_(ETIME_STRUCT)

C        PRINT *, 'ELAPSED = ', ELAPSED

C        ELAS1 = ELAPSED - ELAS1


C        PRINT *, 'TIME DIFFERENCE:  ELAS1 = ', ELAS1
C############################################################

      CALL GETGB(LUGBD,LUGBI,GMXDIM,JK,JPDS,JGDS,KI,KR,KPDSI,KGDSI,
     &           LI,DATA,IRET)

C############################################################
C        PRINT *, 'ADPTOS:  AFTER CALL TO GETGB'

C        CCC = clock_()
C        PRINT *, 'CCC = ', CCC

C        PREVIOUS TIME VALUE
C        ELAS1 = ELAPSED

C        ELAPSED = etime_(ETIME_STRUCT)

C        PRINT *, 'ELAPSED = ', ELAPSED

C        ELAS1 = ELAPSED - ELAS1


C        PRINT *, 'TIME DIFFERENCE:  ELAS1 = ', ELAS1
C############################################################
C
C       PRINT *, 'GETGB: IRET  = ', IRET
C
      IF(IRET.NE.0) CALL EREXIT(4)
      IF(FIRST) THEN
         KMAP = IGRID
         IF(IGRID.EQ.999) KMAP = KPDSI(3)
         NCEN = KPDSI(21) - 1
         IDT(1) = KPDSI(8) + 100*NCEN
         IDT(2) = KPDSI(9)
         IDT(3) = KPDSI(10)
         IDT(4) = KPDSI(11)
         IDT3   = IDT(1)*1000000+IDT(2)*10000
     &           +IDT(3)*100    +IDT(4)
         DATENL = FLOAT(IDT3)
         IDTX = 100*IDT(1) + IDT(2)
         IDTY = 100*IDT(3) + IDT(4)
         PRINT 4, ANLD, ANLI, IDT, KMAP
    4    FORMAT (//'FOR TOSSOUTS, DATA ARE TESTED AGAINST ',
     &             'ANALYSES ON FILE' /'WITH DDNAME = ',A8,1X,
     &             'WHICH HAS AN INDEX FILE NAME OF ', A8/,
     &             'DATED (Y,M,D,H)...', I5,3I3,2X,
     &             'AND CONTAINS MAPS OF GRID TYPE ', I4//)

C        TEST FOR AGREEMENT OF ADP AND TOSSOUT ANALYSIS DATES           

         PRINT 7, IDT3, IAPPCK
    7    FORMAT ('PACKED DATES (HEX) (IN ADPTOS): ANAL, ADP ',
     &            2X,Z10,2X,Z10)
         IF(IDT3.EQ.IAPPCK) GO TO 6
         PRINT 5
    5    FORMAT (T10, 'BUT THIS DATE DOES NOT',
     &                ' AGREE WITH THE ADP DATE - DISASTER, I QUIT...')
         CALL EREXIT(3)
    6    CONTINUE
         WRITE (LUTOS)  IDTX, IDTY
         PRINT 8, XS
    8    FORMAT (/'COEFFICIENTS FOR TOSS LIMITS ARE:', 4F8.2, //)

         CALL STIJ

         FIRST  = .FALSE.
      END IF
      IF(NV.NE.GVECTV) THEN
         IBI(K)  = MOD(KPDSI(4)/64,2)
         VECTOR = .FALSE.
         GO TO 40
      ELSE
         VECTOR  = .TRUE.
         JPDS    = -1
         JGDS    = -1
         JPDS(5) = QWANT(NV+1)
         JPDS(6) = 100
         JPDS(7) = IPP(K)
         JK = 0
         KR = 0
         CALL GETGB(LUGBD,LUGBI,GMXDIM,JK,JPDS,JGDS,KI,KR,KPDSI,KGDSI,
     &              LI,DATA1,IRET)
         IF(IRET.NE.0) CALL EREXIT(4)
         IBI(K)  = MOD(KPDSI(4)/64,2)
      END IF
   40 CONTINUE

C     INITIALIZE SUMMATION VARIABLES                             
C     FOR STDEV (AND GROSS) ERROR CHECKING.                      

      SN     = 0.
      SUM    = 0.
      SUMSQ  = 0.
      VSUM   = 0.
      VSUMSQ = 0.

C     NEXT DO IS OVER ALL ADP STATIONS FOR                       
C     PURPOSE OF INTERPOLATING FIELD VALUES TO                   
C     STATIONS                                                   

      IP    = 1
      IF(INTPM.NE.999) IP = INTPM
      KGDSO = -1
      IPOPT = 0

C#############################################################
C        PRINT *, 'ADPTOS:  BEFORE CALL TO IPOLATES'

C        CCC = clock_()
C        PRINT *, 'CCC = ', CCC

C        PREVIOUS TIME VALUE
C        ELAS1 = ELAPSED

C        ELAPSED = etime_(ETIME_STRUCT)

C        PRINT *, 'ELAPSED = ', ELAPSED

C        ELAS1 = ELAPSED - ELAS1


C        PRINT *, 'TIME DIFFERENCE:  ELAS1 = ', ELAS1
C############################################################

      DO 60 NS = 1,JSTA
      STVL(NS) = F9
      STV1(NS) = F9
      IF(STI(NS).GE.F9-1.) GO TO 60
      AALAT = ALAT(NS)
      AALON = ALON(NS)
      IF(AALON.GT.180.) AALON = AALON-360.
      IF(.NOT.VECTOR) THEN
         CALL IPOLATES
     &   (IP,IPOPT,KGDSI,KGDSO,GMXDIM,1,1,IBI(K),LI,DATA,
     &    1,AALAT,AALON,IBO(K),LO,STVL(NS),IRET)

      ELSE
         SROT = 0.
         CROT = 1.
         CALL IPOLATEV
     &   (IP,IPOPT,KGDSI,KGDSO,GMXDIM,1,1,IBI(K),LI,DATA,DATA1,
     &   1,AALAT,AALON,CROT,SROT,IBO(K),LO,STVL(NS),
     &   STV1(NS),IRET)

C        SPECIAL PROCEDURE FOR SOUTH POLE STATION ....          

         IF(IDSTN(NS).EQ.89009.AND.KMAP.EQ.30) THEN
            STVL(NS) = (DATA(1) + DATA1(37) -
     &                 DATA(73) - DATA1(109) ) / 4.0
            STV1(NS) = (DATA1(1) - DATA(37) -
     &                 DATA1(73) + DATA(109) ) / 4.0
         END IF
         FU(NS) = STVL(NS)
         FV(NS) = STV1(NS)
      ENDIF
   60 CONTINUE

C############################################################
C        PRINT *, 'ADPTOS: AFTER  CALLS TO IPOLATES'

C        CCC = clock_()
C        PRINT *, 'IN ADPTOS: CCC = ', CCC

C        PREVIOUS TIME VALUE
C        ELAS1 = ELAPSED

C        ELAPSED = etime_(ETIME_STRUCT)

C        PRINT *, 'ELAPSED = ', ELAPSED

C        ELAS1 = ELAPSED - ELAS1


C        PRINT *, 'IN ADPTOS: TIME DIFFERENCE:  ELAS1 = ', ELAS1
C############################################################


C     COMPUTE RMS DIFFERENCE (OBS - ANAL) AT STATION PTS.        

      CALL STDEV (VECTOR, SN, SUM, SUMSQ, NV, K, VSUM, VSUMSQ)
      IF(SN.LT.10.) GO TO 3090
      ANMEAN = SUM/SN
      IF(VECTOR) VMEAN = VSUM/SN
      VAR    = (SUMSQ/SN - ANMEAN**2) * SN/(SN-1.)
      SIGMA  = SQRT(VAR)
      IF(VECTOR) VVAR   = (VSUMSQ/SN - VMEAN**2) * SN/(SN-1.)
      IF(VECTOR) VSIGMA = SQRT(VVAR)

C     READ THRU DATA AGAIN TO RECOMPUTE SIGMA, ETC. TAKING OUT                           
C     EFFECTS OF SUPER-GROSS ERRORS, DEFINED AS THOSE WHICH                               
C     EXCEED 1/2  (SEE 0.5 BELOW) OF THEORETICAL MAXIMUM                      
C     SIGMA FOR SN POINTS.                               

      SIGMAX = 0.5*SQRT(SN)
      VSGMAX = 0.5*SQRT(SN)
      IF(VECTOR) GO TO 73
      DO 72 JS=1,JSTA
      IF(STI(JS).GE.F9-1.) GO TO 71
      STNOBS  = ZTRUV(JS,NV,K)
      IF(STNOBS.GE.F9-1.) GO TO 71
      STNDIF  = STNOBS - STVL(JS)
      SIGSTN  = ABS(STNDIF)/SIGMA
      IF(SIGSTN.LT.SIGMAX) GO TO 70
      SUM   = SUM - STNDIF
      SUMSQ = SUMSQ - STNDIF*STNDIF
      SN    = SN - 1.
   70 CONTINUE
   71 CONTINUE
   72 CONTINUE
   73 CONTINUE

      IF(.NOT.VECTOR) GO TO 77
      DO 76 JS=1,JSTA
      IF(STI(JS).GE.F9-1.) GO TO 75
      IF(STVL(JS).GE.F9-1.) GO TO 75
      IF(STV1(JS).GE.F9-1.) GO TO 75
      UO = ZTRUV(JS,NV,K)
      VO = ZTRUV(JS,NV+1,K)
      IF(UO.GE.F9-1..OR.VO.GE.F9-1.) GO TO 75


C     SPDOBS = SQRT(UO*UO + VO*VO)                         
C     SPDANL = SQRT(STVL(JS)**2 + STV1(JS)**2)             
C     SPDIF  = SPDOBS - SPDANL                             
C     SIGSTN = ABS(SPDIF)/SIGMA                            
      UDIF   = UO - FU(JS)
      VDIF   = VO - FV(JS)

C     TEST:  PRINT SOUTH POLE WINDS                        

      IF(IDSTN(JS).EQ.89009) THEN
         PRINT 1111, UO, VO, FU(JS), FV(JS)
 1111    FORMAT ('SOUTH POLE WINDS:  U-OBS =', F6.1, 2X,
     &           'V-OBS =', F6.1, 2X, 'U-ANL =', F6.1, 2X,
     &           'V-ANL =', F6.1/)
      ENDIF
      SIGSTN = ABS(UDIF)/SIGMA
      VSGSTN = ABS(VDIF)/VSIGMA
      IF (SIGSTN.LT.SIGMAX.AND.VSGSTN.LT.VSGMAX)  GO TO 74
      SUM    = SUM    - UDIF
      SUMSQ  = SUMSQ  - UDIF*UDIF
      VSUM   = VSUM   - VDIF
      VSUMSQ = VSUMSQ - VDIF*VDIF
      SN     = SN - 1.
   74 CONTINUE
   75 CONTINUE
   76 CONTINUE
   77 CONTINUE

      ANMEAN = SUM/SN
      VAR    = (SUMSQ/SN - ANMEAN**2)*SN/(SN-1.)
      SIGMA  = SQRT(VAR)
      IF(VECTOR)   VMEAN  = VSUM/SN
      IF(VECTOR)   VVAR   = (VSUMSQ/SN - VMEAN**2) * SN/(SN-1.)
      IF(VECTOR)   VSIGMA = SQRT(VVAR)
      TOSS = XS(NV) * SIGMA
      IF(VECTOR)    VTOSS = XS(NV)*VSIGMA
      IF(.NOT.VECTOR) PRINT 5000, ANMEAN, TOSS, IPP(K), CHQ(NV)
      IF(VECTOR) PRINT 5001, ANMEAN, TOSS, IPP(K), CHQ(NV),
     &           VMEAN, VTOSS, IPP(K), CHQ(NV+1)

C     RUN THRU DATA YET AGAIN; COMPARE ANALYSIS VS STATIONS                               
C     AND TOSS OUT BAD REPORTS                                   

      FNN    = 0.
      SUMFN  = 0.
      SUMFQN = 0.
      SUMVN  = 0.
      SUMVQN = 0.
      FNS    = 0.
      SUMFS  = 0.
      SUMFQS = 0.
      SUMVS  = 0.
      SUMVQS = 0.

      DO 130 JS = 1,JSTA
      STN = ZTRUV(JS,NV,K)
      IF(STN.GE.F9-1..OR.STI(JS).GE.F9-1.) GO TO 140
      IF(STVL(JS).GE.F9-1.) GO TO 140
      IF(VECTOR) GO TO 190
      DIF = STN-STVL(JS)
      IF(ABS(DIF).LT.TOSS) GO TO 170
      PRINT 5002, IDSTN(JS), DIF
      WRITE (LUTOS)  IDSTN(JS), NVK
      GO TO 200
  190 CONTINUE
      STN1 = ZTRUV(JS,NV+1,K)
      IF(STN1.GE.F9-1..OR.STV1(JS).GE.F9-1.) GO TO 180
      SPDOBS = SQRT (STN*STN + STN1*STN1)
      SPDANL = SQRT(FU(JS)**2+FV(JS)**2)
      DIF    = SPDOBS-SPDANL
      DIFU   = STN-FU(JS)
      DIFV   = STN1-FV(JS)
      DIFVE  = DIFU*DIFU+DIFV*DIFV
      DIFVE  = SQRT(DIFVE)
      IF(ABS(DIFU).LT.TOSS.AND.ABS(DIFV).LT.VTOSS)
     &   GO TO 170
      PRINT 5004,  IDSTN(JS), DIFU, DIFV
      WRITE (LUTOS)  IDSTN(JS), NVK
  200 CONTINUE
      NSOUT = NSOUT + 1
      ZTRUV(JS,NV,K) = F9N
      IF(VECTOR) ZTRUV(JS,NV+1,K) = F9N
      GO TO 180
  170 CONTINUE
      NSIN = NSIN + 1

C     ACCUMULATION FOR COMPUTING S. DEV.

C      DO JJS = 1,NSTN
C      IF(IDSTN(JS).EQ.NDICTN(JJS)) GO TO 120
C      END DO 
C      GO TO 140
C  120 CONTINUE                                               
C
      IF(VECTOR) THEN
         IF(ALAT(JS).GE.0.) THEN
            SUMFN  = SUMFN+DIF*(-1.0)
            SUMFQN = SUMFQN+DIF*DIF
            SUMVN  = SUMVN+DIFVE
            SUMVQN = SUMVQN+DIFVE*DIFVE
            NSINN  = NSINN+1
         ELSE
            SUMFS  = SUMFS+DIF*(-1.0)
            SUMFQS = SUMFQS+DIF*DIF
            SUMVS  = SUMVS+DIFVE
            SUMVQS = SUMVQS+DIFVE*DIFVE
            NSINS  = NSINS+1
         END IF
      ELSE
         IF(ALAT(JS).GE.0.) THEN
            SUMFN  = SUMFN+DIF*(-1.0)
            SUMFQN = SUMFQN+DIF*DIF
            NSINN  = NSINN+1
         ELSE
            SUMFS  = SUMFS+DIF*(-1.0)
            SUMFQS = SUMFQS+DIF*DIF
            NSINS  = NSINS+1
         END IF
      END IF
  180 CONTINUE
  140 CONTINUE
  130 CONTINUE
      PRINT 5003, NSIN, NSOUT

C     COMPUTE S. DEV. AND SO ON.

      IF(NSINS.EQ.0) GO TO 131
      FNS    = FLOAT(NSINS)
      SUMFS  = SUMFS/FNS
      SUMFQS = (SUMFQS/FNS-SUMFS**2)
CZ      SUMFQS = (SUMFQS/FNS-SUMFS**2)*FNS/(FNS-1.)
      SZTRUV(NV,K,1,2) = SUMFS
      SZTRUV(NV,K,2,2) = SQRT(SUMFQS)
      SZTRUV(NV,K,3,2) = NSINS
      IF(VECTOR) THEN
         SUMVS  = SUMVS/FNS
         SUMVQS = (SUMVQS/FNS-SUMVS**2)
CZ         SUMVQS = (SUMVQS/FNS-SUMVS**2)*FNS/(FNS-1.)
         SZTRUV(NV+1,K,1,2) = SUMVS
         SZTRUV(NV+1,K,2,2) = SQRT(SUMVQS)
         SZTRUV(NV+1,K,3,2) = NSINS
      END IF
  131 CONTINUE

      IF(NSINN.EQ.0) GO TO 132
      FNN    = FLOAT(NSINN)
      SUMFN  = SUMFN/FNN
      SUMFQN = (SUMFQN/FNN-SUMFN**2)
CZ      SUMFQN = (SUMFQN/FNN-SUMFN**2)*FNN/(FNN-1.)
      SZTRUV(NV,K,1,1) = SUMFN
      SZTRUV(NV,K,2,1) = SQRT(SUMFQN)
      SZTRUV(NV,K,3,1) = NSINN
      IF(VECTOR) THEN
         SUMVN  = SUMVN/FNN
         SUMVQN = (SUMVQN/FNN-SUMVN**2)
CZ         SUMVQN = (SUMVQN/FNN-SUMVN**2)*FNN/(FNN-1.)
         SZTRUV(NV+1,K,1,1) = SUMVN
         SZTRUV(NV+1,K,2,1) = SQRT(SUMVQN)
         SZTRUV(NV+1,K,3,1) = NSINN
      END IF
  132 CONTINUE
      NSINN = 0
      NSINS = 0
      NSIN   = 0.
      NSOUT  = 0.
  150 CONTINUE
  160 CONTINUE

C     ARCHIVE THE ADP DATA FIELDS IF DESIRED                         
C  ** NONCOMPLIANT DATES IN OLD VERSION OF IAPPCK, IAPUNP, AND DATENL **
C     UPSTREAM CHANGES TO CODE SHOULD HAVE CORRECTED THIS, SO NO CHANGE HERE

      JMAP = -KMAP
      WRITE (LUADP) JMAP, JSTA, IAPPCK, IAPUNP, DATENL,
     &              NPT,IMXMAP, JMXMAP, NCYCMP, F9, DUMMY
      DO 162 I=1,JSTA
      WRITE (LUADP) IDSTN(I), ALON(I), ALAT(I), STI(I),
     &              STJ(I), NRPT(I), DISTNN(I),
     &              ((ZTRUV(I,J,K),K=1,4),J=1,5)
  162 CONTINUE
      PRINT 5005, JSTA, IAPUNP

C     PRINT OUT S. DEV. AND SO ON.

      WRITE(LUOUT,666) (IAPUNP(I),I=4,1,-1)
      DO J=GNLEV,1,-1
      WRITE(LUOUT,888) IPP(J),(SZTRUV(I,J,1,1),SZTRUV(I,J,2,1),
     &              IFIX(SZTRUV(I,J,3,1)),I=1,GNVARP)
      END DO
      WRITE(LUOUT,777)
      WRITE(LUOUT,555)
      DO J=GNLEV,1,-1
      WRITE(LUOUT,888) IPP(J),(SZTRUV(I,J,1,2),SZTRUV(I,J,2,2),
     &              IFIX(SZTRUV(I,J,3,2)),I=1,GNVARP)
      END DO
      WRITE(LUOUT,777)
      WRITE(LUOUT,999)
 666  FORMAT(22X,' STATISTICS ON THE DEVIATION OF OBS FROM ANALYSIS '/
     &      ,22X,' ================================================ '//
     &      ,35X,'DATE&TIME:',I2,'Z',1X,I2,'/',I2,'/',I4             //
     &      ,5X,'----------------------------------------------------'
     &         ,'--------------------------------'/
     &      ,18X,'                 NORTH HEMISPHERE                  '/
     &      ,5X,'----------------------------------------------------'
     &         ,'--------------------------------'/
     &      ,5X,' LEV        Z               T              RH       '
     &         ,'       SPD             VEC '/
     &      ,5X,'    ------------------------------------------------'
     &         ,'--------------------------------'/
     &      ,5X,' hPa MEAN  S.D.  NO. MEAN  S.D.  NO. MEAN  S.D.  NO.'
     &         ,' MEAN  S.D.  NO. MEAN  S.D.  NO.'/
     &      ,5X,'----------------------------------------------------'
     &         ,'--------------------------------')
 555  FORMAT(
     &       5X,'----------------------------------------------------'
     &         ,'--------------------------------'/
     &      ,18X,'                 SOUTH   HEMISPHERE                '/
     &      ,5X,'----------------------------------------------------'
     &         ,'--------------------------------'/
     &      ,5X,' LEV        Z               T              RH       '
     &         ,'       SPD             VEC '/
     &      ,5X,'    ------------------------------------------------'
     &         ,'--------------------------------'/
     &      ,5X,' hPa MEAN  S.D.  NO. MEAN  S.D.  NO. MEAN  S.D.  NO.'
     &         ,' MEAN  S.D.  NO. MEAN  S.D.  NO.'/
     &      ,5X,'----------------------------------------------------'
     &         ,'--------------------------------')
 888  FORMAT(5X,1X,I3,5(F5.1,F6.1,I5))
 777  FORMAT(5X,'----------------------------------------------------'
     &         ,'--------------------------------'///)
C
 999  FORMAT(5X,'Note: MEAN-----Denotes mean deviation of observed',
     &       1X, 'value from analysis', /20X,'over one hemisphere.',
     &       /5X, '      S.D.-----Denotes standard deviation of',
     &      1X, 'observed value from analysis',
     &     /20X, 'over one hemisphere.',
     &      /5X, '      NO. -----Denotes number of observed values',
     &      1X, 'over one hemisphere.',
     &     /11X, '999.  >---Denotes missing data.')
C
 5000 FORMAT('MEAN= ', F5.1, 3X,' TOSS= ',F5.1,4X,'FOR ',I4,'MB',2X,A10)
 5001 FORMAT('UMEAN=', F5.1, 3X,' UTOSS=',F5.1,4X,'FOR ',I4,'MB',2X,A10,
     &     /,'VMEAN=', F5.1, 3X,' VTOSS=',F5.1,4X,'FOR ',I4,'MB',2X,A10)
 5002 FORMAT('STATNO = ',I6,3X,'DIF=',F10.1)
 5003 FORMAT('NO OF STNS ACCEPTED= ', I5, 3X,
     &       'NO OF STNS TOSSED=', I5 /)
 5004 FORMAT('STATNO = ', I6, '   DIFU = ',F7.2,'   DIFV = ',F7.2)
 5005 FORMAT('ADP DATA FOR', I5, ' STATIONS ON', I5, 3I3,
     &       ' HAVE BEEN ARCHIVED.'///)
  165 CONTINUE

C############################################################
C        PRINT *, 'AT THE END OF ADPTOS'

C        CCC = clock_()
C        PRINT *, 'ADPTOS: CCC = ', CCC

C        PREVIOUS TIME VALUE
C        ELAS1 = ELAPSED

C        ELAPSED = etime_(ETIME_STRUCT)

C        PRINT *, 'ADPTOS: ELAPSED = ', ELAPSED

C        ELAS1 = ELAPSED - ELAS1


C        PRINT *, 'ADPTOS: TIME DIFFERENCE:  ELAS1 = ', ELAS1
C############################################################

      RETURN

C     DISASTER AREA                                                  

 3090 CONTINUE
      PRINT 3055
 3055 FORMAT (' DISASTER IN ADPTOS...'/)
      PRINT 3095, IFIX(SN)
 3095 FORMAT (' ONLY', I3, ' ADP STATION REPORTS AVAILABLE, ',
     &        'NOT ENOUGH FOR PROPER VERIFICATION.')
      JMAP = -KMAP
      JSTA = 0
      WRITE (LUADP) JMAP, JSTA, IAPPCK, IAPUNP, DATENL,
     &              NPT, IMXMAP, JMXMAP, NCYCMP, F9, DUMMY


      CALL EREXIT(5)
      RETURN
      END
