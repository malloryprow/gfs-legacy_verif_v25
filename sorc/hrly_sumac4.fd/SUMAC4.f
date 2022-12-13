C$$$  MAIN PROGRAM DOCUMENTATION BLOCK  *** 
C
C MAIN PROGRAM: SUMAC4
C   PRGMMR: LILLY            ORG: NP12        DATE: 1999-09-22
C
C ABSTRACT:  ACCUMULATES ANALYSIS OR FORECAST ERRORS WITH RESPECT
C   TO RADIONSONDE OBSERVATIONS.  INPUT NEEDS ARE RAOB OBSERVATIONS 
C   FOR A SPECIFIC DATE, A COLLECTION OF FILES OF FIELDS (NMC 
C   STANDARD) TO BE VERIFIED, AND A LIST OF STATIONS FOR EACH
C   GEOGRAPHIC AREA . OUTPUT FILE CONTAINS THE ERROR STATISTICS
C   (BY AREA ONLY) FOR THE DAY (OR CASE).  FULL DETAILS AVAILABLE
C   FROM VLCEK, W/NP12.
C
C
C PROGRAM HISTORY LOG: 
C
C   76-??-??    STACKPOLE    (AS ACUMVER4)
C  
C   83-04-??    VLCEK        INTRODUCE NEW SUBROUTINE TO WRITE         
C                            DAILY STATS ON SEQUENTIAL FILE.   
C 
C   85-10-10    VLCEK        REINSTATE SUBROUTINE TO PRINT DAILY 
C                            STATS (WAS WITHHELD TO CONSERVE CORE).
C
C   86-10-16    VLCEK        STORE READ-IN ADP DATA IN CORE RATHER
C                            THAN WRITE ON WORK DATASETS; INTRODUCE 
C                            OPTION TO SELECT RANGE OF WIND SPEEDS 
C                            TO BE USED IN WIND VERIFICATIONS.    
C
C   87-09-23    VLCEK        INTRODUCE SUBROUTINE TO READ FROM ADP 
C                            DATA FILE; INTRODUCE NEW SUBROUTINES 
C                            (PARALLEL TO PARTSM) TO READ FROM WARD'S
C                            36-DAY ARCHIVE (LFM AND RAFS ONLY). 
C 
C   89-01-25    VLCEK        MODIFIED FOR COMPATIBILITY WITH VS   
C                            FORTRAN 77 COMPILATION (SUBROUTINE
C                            EBC2I8 ELIMINATED, PARAMETER STATEMENTS
C                            SUBSTITUTED FOR PL1 PREPROCESSING).
C
C   91-09-XX    VLCEK        UPDATE ACCESS TO 36-DAY ARCHIVES 
C                            (SUB PSUM36): PRIVATE SUBROUTINES 
C                            ARE DROPPED IN FAVOR OF LINK-EDITING       
C                            ARC.WD20.ARCH.LOAD.  ADD &OPTION           
C                            NAMELIST VARIABLE /SETMAP/ TO INDI-        
C                            CATE DESIRED MAP TYPE (DEFAULT VALUE       
C                            OF 999 PERMITS 'AUTOMATIC' SELECTION).     
C                            COPIED SUBROUTINE MSIZE FROM SELMAP        
C                            TO GET PROPER RECORD LENGTH FOR GRID.      
C                                                                       
C   94-06-24    VLCEK        COPY AND MODIFY ACUMVER4 TO 'SUMAC LITE':  
C                            SELF-STARTING, NON-ACCUMULATING (EXCEPT    
C                            OVER STATIONS IN AREA DURING CURRENT OBS). 
C                            USES RAOB DATA PRE-TREATED BY PROGRAMS     
C                            RADNCORR AND QCADP.  NO INDIVIDUAL RAOB    
C                            STATION STATS IN VERIFICATION OUTPUT.      
C                            USE FIRST LETTER OF FORECAST FILE NAME     
C                            (/V/) TO IDENTIFY VSAM FILE; IT DOES NOT   
C                            NEED TO BE /X/ TO WRITE STATS TO FT10.     
C
C   96-06-25    Y. ZHANG     CHANGE READING VSAM FORECAST FILES INTO 
C                            READING READING GRIB FORECAST FILES,REMOVE
C                            A UNNECESSARY SUBROUTINE MSIZE;
C                            REMOVE SUBROUTINE PSUM36 WHICH ACCESSED 
C                            36-DAY ARCHIVES ON HDS DUE TO ITS UNAVAILAB
C                            ON CRAY AND OTHERS RELATED TO ACCESS VSAM F
C                            A PARAMETER /INTPM/ ON METHOD OF INTERPOLAT
C                            FORECAST TO OBSERVATIONAL LOCATIONS FROM
C                            GRID POINTS HAS BEEN ADDED IN NAMELIST
C                            /OPTION/;
C                            REMOVE VARIABLE /SETMAP/ IN NAMELIST OPTION
C                            MAKE SUBROUTINE STIJ, WHICH COMPUTES I AND 
C                            COORIDNATES OF OBSERVATION POINTS, ARE CALL
C                            ONLY TO DEPEND ON IF INPUT GRID TYPE OF FOR
C                            TO BE VERIFIED IN THIS CODE AGREES WITH THA
C                            OF BACKGROUNDS USED IN QCADP RUN; 
C                            MAXIMUM NUMBER OF AREAS TO VERIFY HAS BEEN
C                            INCREASED UP TO 15 BY A PARAMETER GMXARA 
C                            ADDED IN PARAMETER STATEMENTS;
C                            THE VALUES INDICATING THE VARIABLES TO BE
C                            VERIFIED HAS BEEN CHANGED AS THE DOCUMENT
C                            ON GRIB FILES. ADD GRID TYPE(2,3,6,45,104) 
C                            OF FORECAST FIELD TO BE VERIFIED. REMOVED
C                            ORIGINAL GRID TYPE 45;
C                            COMMENTS SUBROUTINE ROTWND BECAUSE SUBROUTI
C                            IPOLATEV (DO INTERPOLATING) HAS IN ADVANCE 
C                            DONE ROTATINGWIND DIRECTIONS;
C                            ADD SUBROUTINE VERJET WHICH DO VERIFICATION
C                            OF FORECAST FOR JET AT 250MB;
C                            DESIGNING A TABLE ON SPECIFYING WHICH MODEL
C                            HOURS, AREAS TO BE REQUIRED TO VERIFY,
C                            BY READING IN THIS TABLE, IT CAN CARRY OUT 
C                            TO AUTOMATICLLY CONTROL WHAT WILL BE DONE O
C                            ALSO, FACILITATES ADDING OR REMOVING SOME M
C                            HOURS AND AREAS WITH DOING OR WITHOUT DOING
C                            VERIFICATIONS;
C                            REWRITE A SUBCODE GETDDN PRODUCING DDNAME F
C                            VERIFYING FORECASTS AND THUS ARCHIVE STATIS
C                            CAN BE IDENTIFIED BY CONTAINING A UNIQUE NA
C                            IMPLYING THE MESSAGES ON FORECAST MODEL, HO
C                            AND WHERE THE VERIFYING AREA LOCATES (N. HE
C                            S. HEM., TROPICS);
C                            RWWRITE A SUBCODE WRITSM TO WRITE STATISTIC
C                            A SEQUENTIAL FILE WITH A FORMAT OF HALF-WOR
C                            (AS OPERATIONAL WRITE ON HDS) FOR THE CONVE
C                            OF SELECTING STATS USING A UNIQUE SELECSUM 
C                            ON CRAY;
C                            RECODE A SUBROUTINE GVALDT TO UPDATES OR BA
C                            A FULL WORD DATE/TIME WORD BY A SPECIFIED N
C                            OF HOURS;
C                            ADD A FUNCTION INTFHR TO GET A INTEGER HOUR
C                            ITS ASCII FORMAT 'FXXX'.     
C   96-09-10    Y. ZHANG     ADD A SUBROUTINE PARTEC FOR SPECIALLY READI
C                            ECMWF's GRIB FORECAST FILE.
C   96-11-27    Y. ZHANG     DEACTIVATE MANY PRINTED OUTPUTS AND MODIFY
C                            SUBROUTINE PARTEC.
C   98-07-14    C. VLCEK     COMPILE IN F90, MAKE Y2K-COMPLIANT.
C   99-06-09    C. VLCEK     COMPILE ON IBM RS6000.
C   15-01-23    S. LILLY     INCREASE TABLE SIZE (GMXTBL) FROM 511 TO
C                            600 ALLOWING MORE STATION IDS.
C 
C USAGE:                                                                
C                                                                       
C   INPUT FILES :                                                       
C     FT05F001   -  NAMELIST STYLE INPUT SPECIFYING THE DDNAMES OF      
C                   THE FILES CONTAINING THE FORECAST OR ANALYSIS       
C                   FIELDS TO BE VERIFIED AND VARIOUS OPTIONS.          
C                                                                       
C                   NAMELIST /FILES/ HAS THE AFOREMENTIONED DDNAMES     
C                   IN /FORFIL/, THE NUMBER OF WHICH IS SPECIFIED IN    
C                   /NFILE/.  THE NUMBER OF ADDITIONAL CARDS NEEDED     
C                   TO READ IN /FORFIL/ IS GIVEN BY /RPT/.              
C                                                                       
C     FT07F001   -  CONTAINS LOLA LIMITS, CHAR*8 NAME, AND STATION LIST 
C                   FOR VERIFICATION AREA (CF PROGRAM STRVER4V).        
C                                                                       
C     FT08F001   -  CONTAINS PREPROCESSED UPPER AIR RAOB DATA (QCADP,   
C                   RADNCORR).  CONTENTS OF THIS FILE CAN BE LISTED     
C                   OR COPIED BY (CHECKOUT) PROGRAM ADPCOPY (VLCEK).    
C                                                                       
C     FT09F001   -  NAMELIST /OPTION/ PROVIDES OPTIONS CORRESPONDING    
C                   TO THOSE FOUND IN ACUMVER4 NAMELISTS /ANLFIL/ AND   
C                   /FILES/ (INCLUDING /RPT/ GIVEN ABOVE):              
C                                                                       
C                   THIS NAMELIST PROVIDES OPTIONS TO PRINT AREA STATS  
C                   (PRSTAT=T), OR WRITE THE AREA STATISTICS ONTO A     
C                   SEQUENTIAL FILE (WRSTAT=T).  DEFAULT = .FALSE.      
C                   SPECIFY DIAG=T IF YOU WANT ADDITIONAL PRINT STATE-  
C                   MENTS FOR DIAGNOSTIC PURPOSES (DEFAULT = .FALSE.).  
C                                                                       
C                   MINWND, MAXWND ADDED TO /OPTION/ NAMELIST ON 10/86. 
C                   WIND STATISTICS AT EACH STATION WILL BE COMPUTED    
C                   AND ACCUMULATED ONLY IF EITHER THE FORECAST OR THE  
C                   OBSERVED WIND SPEED AT THE STATION IS GT.MINWND AND 
C                   LT.MAXWND (DEFAULT MIN=0, MAX=150 M/SEC).  THEY CAN 
C                   BE USED TO CONFINE WIND VERIFICATIONS TO JET STREAM 
C                   REGIONS.  CAUTION -- SAMPLE SIZE MAY BE LOW.  IT IS 
C                   BETTER TO ACCUMULATE THIS FOR AWHILE WITH SUBROUTINE
C                   VERJET (AUTOMATIC IF MIN.GT.30). 
C                                                                       
C                   VDATE1 AND VDATE2 ARE THE ENDPOINTS OF A VERIFI-    
C                   CATION PERIOD (DEFAULT IS FOREVER).  STATS WILL     
C                   BE PRINTED AND/OR WRITTEN FOR EACH OBS PERIOD       
C                   DURING THE INTERVAL FOR WHICH THE APPROPRIATE       
C                   FORECAST FIELDS AND VERIFYING RAOBS ARE AVAILABLE.  
C
C     FT10F001   -  INPUT A TABLE ON SPECIFYING WHICH MODELS AND HOURS
C                   OPERATIONALLY VERIFIED OVER WHICH AREAS.
C                                                                       
C    /FT11F001   -  FIRST GRIB INDEX FORECAST(or ANALYSIS) FILE (IF ANY)
C 1.<               TO BE VERIFIED.              
C    \FT12F001   -  FIRST GRIB FORECAST(or ANALYSIS) FILE (IF ANY)
C                   TO BE VERIFIED.              
C    /FT13F001   -  SECOND GRIB INDEX FORECAST(or ANALYSIS) FILE (IF ANY
C 2.<               TO BE VERIFIED.              
C    \FT14F001   -  SECOND GRIB FORECAST(or ANALYSIS) FILE (IF ANY)
C                   TO BE VERIFIED.              
C    /FT15F001   -  THIRD GRIB INDEX FORECAST(or ANALYSIS) FILE (IF ANY)
C 3.<               TO BE VERIFIED.              
C    \FT16F001   -  THIRD GRIB FORECAST(or ANALYSIS) FILE (IF ANY)
C                   TO BE VERIFIED.              
C .
C .
C . (Where: n1=2*NFILE-1+10, n2=2*NFILE+10)
C
C F  /FTn1F001   -  NFILE'th GRIB FORECAST(or ANALYSIS) FILE (IF ANY) TO
C I <               BE VERIFIED.              
C L  \FTn2F001   -  NFILE'th GRIB INDEX FORECAST(or ANALYSIS) FILE (IF A
C E                 TO BE VERIFIED.              
C                                                                       
C                -  TYPICALLY, NFILE SPECIFIED IN FT05F001 INPUT EXACTLY
C                   DENOTES NUMBER OF FILES OF FORECAST FIELDS FROM ONE
C                   MODEL TO BE VERIFIED. SHOULD HAVE VALID TIME      
C                   CORRESPONDING TO ADP OBS TIME.           
C                   (CODE WILL CHECK AND REJECT IF APPROPRIATE).        
C                   IF WRSTAT=T (SEE FT05F001) INDIVIDUAL DAILY         
C                   ERROR STATISTICS WILL BE WRITTEN TO FT71F001        
C                   AS A SEQUENTIAL FILE.
C                                                                       
C                   DDNAMES FOR CONVENIENCE OF IDENTIFYING STATISTICS
C                   FROM DIFFERENT MODELS WILL BE SPECIALLY CODED BY NAM
C                   OF THE MODEL AND HOURS AS FOLLOWS: MODLFXXX, WHERE  
C                                     XXX IS THE FORECAST HOUR          
C                       (EXCEPT WHEN FXXX = ANAL FOR THE ANALYSIS),     
C                                AND MODL IS THE MODEL:                 
C                                    REGL IS THE NAM
C                                    RAFS IS THE NGM                    
C                               MRFN,MRFS IS MEDIUM RANGE (N.,S. HEM)   
C                               LRGN,LRGS IS AVIATION     (N.,S. HEM)   
C                               DASN,DASS IS GLOBAL       (N.,S. HEM)   
C                               PERN,PERS IS PERSISTENCE  (N.,S. HEM)   
C                               UKMX,UKMS IS U.K. MET. OFFICE           
C                               ECMN,ECMS IS EUROPEON CENTER MODEL      
C                   FXXX WILL BE GOT FROM FHR SPECIFIED IN FT05F001 INPU
C                   THIS MEANS THAT FHR IN FT05F001 INPUT MUST BE ACCURA
C                   SPECIFIED, THE ORDERS MUST BE CORRESPONDING WITH THE
C                   SEQUENCE OF INPUT FORECAST FILES.
C                                                                       
C     MODEL NAMES CORRESPONDING TO THE STANDARD RUN NAMES ARE ALSO      
C     ACCEPTABLE. 
C                                                                       
C                                                                       
C   OUTPUT FILES:                                                       
C     FT06F001   -  ERROR MESSAGES AND OTHER INTERESTING REMARKS,       
C                   DAILY STATS PRINTED IF DESIRED.                     
C                                                                       
C     FT71F001   -  FILE CONTAINING DAILY STATISTICS (NOT ACCUMULATED). 
C                   OPTIONAL--MUST SPECIFY WRSTAT=T ON FT05 NAMELIST;   
C                                                                       
C                                                                       
C   SUBPROGRAMS CALLED:                                                 
C     UNIQUE :  ACCUMU, ADPFIL, EREXIT, MSIZE , NAMESTN, PARTSM,        
C               PRTSTA, SETPNQ, STIJ  , WRTSTA, WRITSM , GVALDT,
C               GETDDN, INTFHR           
C                                                                       
C     LIBRARY:                                                          
C         W3LIB:    W3FB02, W3FB04
C                   
C                                                                       
C   EXIT STATES:                                                        
C                                                                       
C     COND = 0000   O.K. RUN - NO ERROR CONDITIONS SET - SEE FT06F001   
C          = 1060   SUB ROTWND: FORECAST MAP TYPE NOT RECOGNIZED.       
C          = 1070   SUB STIJ: FORECAST MAP TYPE NOT RECOGNIZED.         
C                                                                       
C   REMARKS:  CAUTION -- THIS CODE WILL NOT DEFEND AGAINST DUPLICATION
C     CANCEL/RESTARTS.  ALL ERROR STOP CODES ARE CONTAINED IN SUB-
C     ROUTINE EREXIT.  FULL WRITEUP OF THIS AND ALL OTHER SUMAC
C     RELATED CODES AVAILABLE FROM C. VLCEK, QAP, W/NP12.
C                                                                       
C ATTRIBUTES:                                                           
C   LANGUAGE: FORTRAN 90    IBM FORTRAN
C   MACHINE:  IBM RS6000
C                                                                       
C$$$                                                                    
                                                                       
CC    MAIN PROGRAM FOR VERIFICATION AGAINST ADP DATA                 
                                                                       
      IMPLICIT INTEGER(G)

C     /G/ IS RESERVED FOR PARAMETER CONSTANTS.  SEE SUBROUTINE       
C     ACCUMU FOR COMPLETE LIST OF PARAMETERS AND DESCRIPTION.        

      PARAMETER (GORFIL=22,GMXTBL=600,GMXARA=15)
      PARAMETER (GIMAX=360, GJMAX=181  , GMXDIM=(GIMAX*GJMAX))
      PARAMETER (GMXRUN=15)

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
      COMMON /UNITS / IUAREA, IUADP    , IUGBD(GORFIL) , IUGBI(GORFIL) ,
     &                IUJET , IUSTA    , IUOUT  , IUOPN, IUTAB

      REAL          MINWND, MAXWND
      INTEGER       VDATE1, VDATE2, VDATE , RPT   , STNLST, NFILE
      INTEGER       GCODE
      CHARACTER(8)  FORFIL, AREA  , BLANK , ARATAB
      CHARACTER(4)  FHR   , KRUN  , RUNTAB, FHRTAB, BLANK4
      CHARACTER(1)  INDARA, BLANK1, DOSTAT
      CHARACTER(80) GARBAGE
      LOGICAL       PRSTAT, WRSTAT, DIAG

C     DATA  IUAREA/ 7/, IUADP/ 8/, IUOUT/ 6/, IUOPN/9/
C     DATA  IUTAB /10/, IUJET/70/, IUSTA/71/
      DATA  BLANK  /'        '/  , BLANK4/'    '/     , BLANK1/' '/

      NAMELIST /FILES/  RPT   , NFILE , FHR   , KRUN
      NAMELIST /OPTION/ RPT   , VDATE1, VDATE2, PRSTAT, WRSTAT,
     &                  MINWND, MAXWND, DIAG  , INTPM

      CALL W3TAGB('SUMAC4',1999,0265,0055,'NP12')                          

      CALL SETPNQ
    
      IUOUT  =  6
      IUAREA =  7
      IUADP  =  8
      IUOPN  =  9
      IUTAB  = 10
      IUJET  = 70
      IUSTA  = 71

      RPT    =          0
      VDATE1 =          0
      VDATE2 = 9999123112
      NAREA  =          0
      MINWND =          0
      MAXWND =        150.
      IGRID  =        999
      INTPM  =        999
      PRSTAT =     .FALSE.
      WRSTAT =     .FALSE.
      DIAG   =     .FALSE.
      KRUN   =     BLANK4
  
C     INITIALIZE ARRAY FIELDS

      DO I=1,GORFIL
        IUGBI(I)  = 2*I - 1 + 10
        IUGBD(I)  = 2*I + 10
      END DO

      RUNTAB = BLANK4
      FHR    = BLANK4
      FHRTAB = BLANK4
      AREA   = BLANK
      ARATAB = BLANK
      INDARA = BLANK1
      DOSTAT = BLANK1

C     READ IN RUNTAB, FHRTAB, ARATAB, INDARA

      MRUN = 0
      READ (IUTAB,'(A80)') GARBAGE
C      WRITE(6,'(A80)') GARBAGE
      READ (IUTAB,'(A80)') GARBAGE
C      WRITE(6,'(A80)') GARBAGE
      READ (IUTAB,'(A80)') GARBAGE
C      WRITE(6,'(A80)') GARBAGE
      READ (IUTAB,'(A80)') GARBAGE
C      WRITE(6,'(A80)') GARBAGE
      READ (IUTAB,'(A80)') GARBAGE
C      WRITE(6,'(A80)') GARBAGE
   14 CONTINUE
        MRUN = MRUN + 1
        READ (IUTAB,111,END=15) RUNTAB(MRUN),MFILE,MAREA
C        WRITE(6,111) RUNTAB(MRUN),MFILE,MAREA
        IF(MFILE.GT.GORFIL) THEN
           PRINT *,'GORFIL= ', GORFIL, ' MFILE= ',MFILE
           CALL EREXIT(3)
        END IF
        READ (IUTAB,112) (FHRTAB(J,MRUN),J=1,MFILE)
C        WRITE(6,112) (FHRTAB(J,MRUN),J=1,MFILE)
        DO I=1,MAREA
          READ (IUTAB,113) ARATAB(I,MRUN),INDARA(I,MRUN),
     &                     (DOSTAT(I,J,MRUN),J=1,MFILE)
C          WRITE(6,113) ARATAB(I,MRUN),INDARA(I,MRUN),
C     &                     (DOSTAT(I,J,MRUN),J=1,MFILE)
        END DO
      IF(MRUN.LT.GMXRUN) GO TO 14
   15 CONTINUE
      MRUN = MRUN - 1
  111 FORMAT (16X,A4,21X,I2,14X,I2)
  112 FORMAT (16X,A4,21(1X,A4))
  113 FORMAT (5X,A8,1X,A1,1X,A1,21(4X,A1))

C     READ IN VERIFICATION AREAS (UP TO 15), STATION LISTS

   16 CONTINUE
        NAREA = NAREA + 1
        READ (IUAREA,116,END=20) (ALOLA(NAREA,I),I=1,4), AREA(NAREA)
  116   FORMAT (4F5.1, 7X, A8)
        NSTN = 0
   17   CONTINUE
          NSTN = NSTN + 1
          IF(NSTN.LT.GMXTBL) THEN
             READ (IUAREA,117) STNLST(NAREA,NSTN)
             LSTSTN = STNLST(NAREA,NSTN)
          ELSE
             READ (IUAREA,117) LSTSTN
          ENDIF
  117     FORMAT (I5)
        IF(LSTSTN.NE.99999) GO TO 17
        IF(NSTN.GT.GMXTBL) PRINT 118, AREA(NAREA), NSTN, GMXTBL
  118   FORMAT (' *** WARNING! ***  AREA ', A8, ' HAS TOO MANY ',
     &          ' STATIONS (', I3, ') ... ONLY FIRST', I4,
     &          ' STATIONS ARE ENTERED.')
        STNLST(NAREA,GMXTBL) = 99999
        KSTN(NAREA)          = NSTN
        IF(NSTN.GT.GMXTBL) KSTN(NAREA) = GMXTBL
      IF(NAREA.LT.GMXARA) GO TO 16
   20 CONTINUE

C     READ OPTION NAMELIST 

      READ (IUOPN,OPTION)
   24 CONTINUE
      IF(RPT.GT.0) THEN
         READ (IUOPN,OPTION)
         RPT = RPT - 1
         GO TO 24
      ENDIF

      IF(DIAG) PRINT 10
   10 FORMAT (//T47,' *** CALCULATE VERIFICATION STATISTICS *** '//)
      IF(DIAG) PRINT 11, GMXTBL
   11 FORMAT (/T45,' *** UP TO',I4,' STATIONS IN AREA PERMITTED *** '/)
      IF(DIAG) PRINT 12
   12 FORMAT (/T43,
     &     ' *** THIS VERSION COMPILED 6-09-1999 BY C. VLCEK *** '/)
      IF(DIAG) PRINT 124, VDATE1, VDATE2
  124 FORMAT (T15,'FORECASTS TO BE VERIFIED FROM',I11,' TO',I11,
     &        ' INCLUSIVE, USING ADP FROM FT09 ARCHIVE.')

C     READ FILE NAMELIST

      READ (5,FILES)
      KRPT = RPT
   25 CONTINUE
      IF(RPT.GT.0) THEN
         READ (5,FILES)
         RPT = RPT - 1
         GO TO 25
      ENDIF
      IF(NFILE.EQ.0) GO TO 50

        DO NF=1,NFILE
          CALL GETDDN(KRUN,'-',0,FHR(NF),FORFIL(NF))
        END DO

        IF(DIAG) PRINT 30
   30   FORMAT (//T15, 'START ACCUMULATION OF PARTIAL SUMS' )
        IF(DIAG) PRINT 32, NFILE, KRPT, PRSTAT, WRSTAT, FORFIL
   32   FORMAT (//T15, 'INPUT FROM NAMELIST /FILES/:  NFILE = ',
     &          I2, 4X, 'RPT = ', I2, 4X, 'PRSTAT = ', L1, 4X,
     &          'WRSTAT = ', L1, 4X, 'FORFILS:'/(T13,12(2X,A8)))
        IF(DIAG) PRINT 34, MINWND, MAXWND
   34   FORMAT (//T15,'WIND SPEED RANGE SELECTED FOR VERIFICATION:'
     &            ,4X, 'MINWND = ', F4.0, 4X, 'MAXWND = ', F4.0, 4X,
     &            '(DEFAULT IS 0 - 150 M/SEC)', // )

        IER = 1
   36   CONTINUE
          CALL ADPFIL(IER)
          IF(IER.EQ.0) THEN
             IF(KRUN.EQ.'ECM ') THEN
                CALL PARTEC
             ELSE
                CALL PARTSM
             END IF
          END IF
        IF(IER.EQ.0) GO TO 36

        IF(DIAG) PRINT 40
   40   FORMAT (///T15, 'END ACCUMULATION, PLANNED EXIT...')

   50 CONTINUE

      CALL W3TAGE('SUMAC4') 
      STOP
      END
