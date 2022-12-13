C$$$  MAIN PROGRAM DOCUMENTATION BLOCK  ***                             
C                                                                       
C MAIN PROGRAM: QCADP
C   PRGMMR: LILLY            ORG: EMC        DATE: 2016-04-22
C                                                                       
C ABSTRACT:  COMPARES RADIOSONDE OBSERVATIONS OF Z, T, RH, U, AND       
C   V AT 850, 500, 250, AND 100 MB TO THE ANALYZED VALUES AT THOSE      
C   LEVELS.  MEAN AND STANDARD DEVIATION OF (ANL - OBS) IS COMPUTED     
C   (AND RECOMPUTED) IN TWO-PASS OPERATION.  THE FIRST PASS ELIMIN-     
C   ATES THE REALLY GROSS ERRORS (ARISING FROM BAD TRANSMISSIONS        
C   AND THE LIKE) FROM THE SAMPLE BEFORE THE MEAN AND S.D. OF           
C   (ANL - OBS) IS RECOMPUTED.  A TOSS THRESHHOLD IS ESTABLISHED        
C   BY MULTIPLYING THE S.D. BY A QUASI-ARBITRARY CONSTANT; EACH         
C   OBSERVATION THAT DIFFERS FROM THE ANALYZED VALUE BY MORE THAN       
C   THE THRESHHOLD AMOUNT IS 'TOSSED' BY ASSIGNING A 'MISSING'          
C   VALUE OF 99999.  THE SET OF OBSERVATIONS MAY THEN BE WRITTEN        
C   TO A NEW FILE (FT60F001) AND A RECORD OF THE TOSSOUTS MAY BE        
C   PRESERVED ON ANOTHER FILE (FT70F001).  THIS PROGRAM WAS ORIGIN-     
C   ALLY A PART OF PROGRAMS ACUMVER4, ET AL, BUT IS BEING PRESENTED     
C   HERE AS A STAND-ALONE PROGRAM AS A MATTER OF CONVENIENCE, SUCH      
C   AS FOR TESTING NEW CONSTANTS FOR THE TOSSOUT THRESHHOLD, BUILD-     
C   ING A QUALITY-CONTROLLED ADP LIBRARY, OR IDENTIFYING REPEAT         
C   OFFENDERS.  FULL DETAILS AVAILABLE FROM VLCEK, W/NP12.
C                                                                       
C PROGRAM HISTORY LOG:                                                  
C                                                                       
C   76-??-??    STACKPOLE    (AS ACCUMVER, LATER ACUMVER4)              
C                                                                       
C   85-10-10    VLCEK        QUALITY CHECK FOR OBSERVED U AND V         
C                            AGAINST ANALYSIS (FORMERLY CHECKED         
C                            SCALAR WIND SPEED ONLY).                   
C                                                                       
C   86-10-16    VLCEK        STORE READ-IN ADP DATA IN CORE RATHER      
C                            THAN WRITE ON WORK DATASETS; UTILIZE       
C                            QUALITY MARKERS INCLUDED WITH ADP DATA     
C                            (O.N. 85 FORMAT) IN QUALITY CONTROL        
C                            ROUTINE.                                   
C                                                                       
C   87-09-23    VLCEK        SAVE SELECTED ADP DATA (FOLLOWING          
C                            QUALITY CONTROL) ON SEQUENTIAL FILE.       
C                                                                       
C   89-01-25    VLCEK        MODIFIED FOR COMPATIBILITY WITH VS         
C                            FORTRAN 77 COMPILATION (SUBROUTINE         
C                            EBC2I8 ELIMINATED, PARAMETER STATEMENTS    
C                            SUBSTITUTED FOR PL1 PREPROCESSING).        
C                                                                       
C   89-08-19    VLCEK        ADD SEQ FILE (FT70F001) TO HOLD ADP        
C                            TOSSOUT RECORDS (SUB ADPTOS).              
C                                                                       
C   90-06-14    VLCEK        MAKE TOSSOUT PROCEDURE A STAND-ALONE       
C                            PROGRAM (CHECKOUT USE ONLY).  ALLOW        
C                            TOSSOUT COEFFICIENTS TO BE READ IN AS      
C                            NAMELIST VARIABLE TO OVERRIDE DEFAULT.     
C                                                                       
C   92-06-18    VLCEK        'KEEP' RAW DATA WITH NEW QUALITY MARKERS   
C                            'C' AND 'Q'; BYPASS TESTING OF RH DATA.    
C                                                                       
C   93-06-23    VLCEK        SPECIAL PROCEDURE FOR COMPUTING ANL WINDS  
C                            AT SOUTH POLE TO TEST WINDS AT STN 89009.  
C                                                                       
C   93-07-12    VLCEK        MAKE PRODUCTION MODULE FOR STAND-ALONE.    
C                            CHANGE SEQUENTIAL I/O NUMBERS TO CONFORM   
C                            TO CONVENTION.                             
C    
C   96-05-06    Y. ZHANG     MOVE IT TO CRAY FROM HDS , CHANGE READING
C                            SEQUENTIAL O.N. 85 ADPUPA FILE AND VSAM ANA
C                            FILE (on HDS) INTO READING BUFR (ADP data) 
C                            AND GRIB FILE (ANAL fields)(on CRAY), REMOV
C                            SUBROUTINE SCALE DUE TO ALL OF ITS FUNCTION
C                            COVERED BY GETADP, INTERPOLATION SCHEME WAS
C                            CHANGED TO BICUBIC FROM BIQUADRATIC, MODIFY
C                            THE FORMAT OF THE WRITEOUT, REFINE CONTENTS
C                            ALL PARAMETERS AND MOST OF SUBROUTINES. COM
C                            SUBROUTINE ROTWND BECAUSE SUBROUTINE IPOLAT
C                            (DO INTERPOLATING) HAS IN ADVANCE DONE ROTA
C                            WIND DIRECTIONS. THE SUBROUTINE RELATED TO 
C                            VSAM FILE ARE REMOVED. TWO VARIABLES, LPRDA
C                            INDXA, ARE ADDED INTO NAMELIST /INPARM/, SO
C                            BY SETTING LPRDAT=.TRUE. OR .FALSE., TO PRI
C                            OR NOT ADP DATA PASSED BY QCADP CAN BE CONT
C                            AND BY SETTING INDXA='N','S','G', IT CAN CA
C                            RUNNING QCADP WITH ADP REPORTS OVER N.HEM.,
C                            OR GLOBAL. SEVERAL GRID TYPE (2,3,6,45,104)
C                            ADDED, THE ORIGINAL GRID TYPE 45 IS REMOVED
C                            ITS UNAVAILABLE ON CRAY. THE ELEMENT CODES 
C                            VARIABLES TO BE VERIFIED ARE CHANGED AS SPE
C                            IN GRIB FILES(CF. O.N. 388). 
C                                                                       
C   98-06-24    VLCEK        MAKE Y2K COMPATIBLE, COMPILE IN FORTRAN 90
C   16-04-22    LILLY        CONVERT FROM GRIB 1 TO GRIB 2
C                                                                       
C                                                                       
C USAGE:                                                                
C                                                                       
C   INPUT FILES :                                                       
C     FT09F001   -  NAMELIST STYLE INPUT SPECIFYING THE DDNAME OF       
C                   THE FILE CONTAINING THE ANALYSIS FIELDS TO BE       
C                   VERIFIED.  THE MAP TYPE IS ASSUMED TO BE THE        
C                   ONE FOUND ON THE FIRST RECORD ON THE FILE UNLESS    
C                   'IGRID=NN' IS SPECIFIED IN THE NAMELIST
C                                                                       
C     FT10F001   -  CONTAINS UPPER AIR ADP (RAOB, OR OTHER TYPE) DATA   
C                   FROM BUFR FILE
C                                                                       
C     FT11F001   -  ANALYSIS FIELDS GRIB FILE
C
C     FT12F001   -  ANALYSIS FIELDS GRIB INDEX FILE
C                                                                       
C   OUTPUT FILES:                                                       
C     FT06F001   -  ERROR MESSAGES AND OTHER INTERESTING REMARKS,       
C                   INCLUDING LISTS OF BAD DATA TOSSED.                 
C                                                                       
C     FT60F001   -  ARCHIVE OF UPPER AIR RAOB DATA SAVED AFTER BEING    
C                   PROCESSED BY THIS PROGRAM.  PROCESSING INVOLVES     
C                   VARIOUS QUALITY CHECKS, SCALING OF QUANTITIES,      
C                   AND SELECTION OF OBSERVED Z, T, RH, U, AND V        
C                   AT 850, 500, 250, AND 100 MB. (FORMERLY FT09F001)   
C                                                                       
C     FT70F001   -  FILE CONTAINING DATE/TIME, BLOCK AND CALL NUMBERS   
C                   OF RAOB STATIONS WHICH HAVE HAD OBSERVATIONS TOSSED 
C                   BY THIS PROGRAM, AND TYPE OF DATA THAT WAS TOSSED.  
C                   THE ACTUAL VALUE OF THE OBSERVATION THAT WAS TOSSED 
C                   IS  N O T  SAVED HERE.  PROGRAM READTOSS (VLCEK)    
C                   READS THIS FILE TO SUMMARIZE THE TOSSOUT RECORD     
C                   OF SELECTED STATIONS BY PROVIDING THE CUMULATIVE    
C                   TOTAL OVER A SELECTED PERIOD OF TIME FOR EACH       
C                   QUANTITY AND LEVEL.  (FORMERLY FT33F001)            
C                                                                       
C                                                                       
C   SUBPROGRAMS CALLED:                                                 
C     UNIQUE :  ADPTOS, EREXIT, GETADP, NAMSTN, NEARPT,
C               SETPNQ, STDEV, STIJ                              
C                                                                       
C                                                                       
C     LIBRARY:                                                          
C     W3LIB  :      W3FB02, W3FB04
C                                                                       
C                                                                       
C   EXIT STATES:                                                        
C                                                                       
C     COND = 0000   O.K. RUN - NO ERROR CONDITIONS SET - SEE FT06F001   
C          = 1030   SUB ADPTOS: DATE MISMATCH FILES /ANL/ AND FT20F001  
C          = 1040   SUB ADPTOS: FATAL W3FK03 ERROR READING ANL FILE.    
C          = 1050   SUB ADPTOS: TOO FEW ADP REPORTS IN FT20 TO USE.     
C                                                                       
C   REMARKS:  ALL ERROR MESSAGES (SEE COPY ABOVE) AND ERROR       
C     STOP CODES ARE CONTAINED IN SUBROUTINE EREXIT.  FULL WRITEUP      
C     OF THIS AND ALL THE OTHER SUMAC RELATED CODES AVAILABLE FROM      
C     C. VLCEK, W/NP12.  YOU HAVE BUT TO ASK.                         
C                                                                       
C ATTRIBUTES:                                                           
C   LANGUAGE: VS FORTRAN 90, CRAY FORTRAN                               
C   MACHINE:  CRAY                                                      
C                                                                       
C$$$                                                                    
                                                                      
C     MAIN PROGRAM FOR QUALITY COTROL OF ADP DATA USED FOR VERIFICATION 
    
                                                                       
      PROGRAM QCADP

      IMPLICIT INTEGER(G)

C     /G/ IS RESERVED FOR PARAMETER CONSTANTS.  SEE SUBROUTINE       
C     ACCUMU FOR COMPLETE LIST OF PARAMETERS AND DESCRIPTION.        

      PARAMETER  (GIMAX=360, GJMAX=181, GMXDIM=(GIMAX*GJMAX))
      PARAMETER  (GNVAR=4, GVECTV=4)
      PARAMETER  (GNVARP=(GNVAR+1), GNLEV=4)
      PARAMETER  (GMXSTN=1100, GMINBN=0, GMAXBN=99999)

      COMMON /RAFILE/ DATA(GMXDIM) , DATA1(GMXDIM) , IGRID,
     &                XS(4)        , RPT,      ANLD, ANLI
      COMMON /ADPSTN/ IDSTN(GMXSTN), ALON(GMXSTN)  , ALAT(GMXSTN),
     &                NRPT(GMXSTN) , DISTNN(GMXSTN),
     &                ZTRUV(GMXSTN , GNVARP,GNLEV) ,
     &                STI(GMXSTN)  , STJ(GMXSTN)   , JSTA
C     COMMON /LLUNIT/ LULST, LUBFI, LUGBD, LUGBI, LUTOS, LUADP, LUOUT
      COMMON /PARMIN/ LPRDAT, INTPM, INDXA


C######################################
       CHARACTER(8)   CCC, clock_

       REAL(4)  ELASPSED, ELAS1, etime_
       TYPE TB_TYPE
          SEQUENCE
          REAL(4) USRTIME
          REAL(4) SYSTIME
       END TYPE
       TYPE (TB_TYPE) ETIME_STRUCT
      
C######################################


      INTEGER       IGRID, RPT, LULST, LUBFI, LUGBD, LUGBI, LUTOS
      INTEGER       LUADP, LUOUT
      CHARACTER(9)  ANLD, ANLI
      CHARACTER(1)  INDXA
      LOGICAL       LPRDAT
      DATA          LULST/9 /, LUBFI/10/, LUGBD/11/, LUGBI/12/,
     &              LUADP/60/, LUTOS/70/, LUOUT/6 /

      NAMELIST /INPARM/ ANLD, ANLI, IGRID, RPT, XS,
     &                  LPRDAT, INTPM, INDXA

      CALL W3TAGB('QCADP',1999,0265,0063,'NP12') 
      PRINT 10
   10 FORMAT(//T10,
     &      ' *** CHECK OBSERVATIONS AGAINST ANALYSIS *** ' // )

C     SET DEFAULT VALUES FOR NAMELIST VARIABLES; XS IS A SET         
C     OF COEFFS FOR TOSSOUT THRESHHOLDS (# OF S.D. (ANL-OBS)).       

C############################################################
C BSM        CCC = clock_()
C BSM        PRINT *, 'MAIN:  CCC = ', CCC
         PRINT *, 'MAIN:  '

C        SAVE PREVIOUS ELASPED TIME
C BSM         ELAS1 = ELAPSED

C BSM         ELAPSED = etime_(ETIME_STRUCT)

C BSM         PRINT *, 'MAIN:  ELAPSED = ', ELAPSED

C BSM         ELAS1 = ELAPSED - ELAS1
C BSM         PRINT *, 'MAIN: TIME DIFFERENCE: ELASl = ', ELAS1
C############################################################
      CALL SETPNQ
      XS(1)   = 3.9
      XS(2)   = 3.75
      XS(3)   = 3.0
      XS(4)   = 3.5
      LPRDAT  = .FALSE.
      RPT     = 0
      IGRID   = 999
      INTPM   = 999
      INDXA   = 'G'
   15 CONTINUE
C     READ(LULST,INPARM,END=16)
      READ(LULST,INPARM)
      RPT = RPT-1
      IF(RPT.GE.0) GO TO 15
C  16 CONTINUE
      PRINT 20
   20 FORMAT(//, T15, 'START GETADP, NEARPT, AND ADPTOS', // )

C############################################################
C BSM         CCC = clock_()
C BSM         PRINT *, 'MAIN:  B GETADP: CCC = ', CCC

C        SAVE PREVIOUS ELASPED TIME
C BSM         ELAS1 = ELAPSED

C BSM         ELAPSED = etime_(ETIME_STRUCT)

C BSM         PRINT *, 'MAIN: B GETADP  ELAPSED = ', ELAPSED

C BSM         ELAS1 = ELAPSED - ELAS1
C BSM         PRINT *, 'MAIN: B GETADP TIME DIFFERENCE: ELASl = ', ELAS1
C############################################################
      CALL GETADP

C############################################################
C BSM         CCC = clock_()
C BSM         PRINT *, 'MAIN:  A GETADP: CCC = ', CCC

C        SAVE PREVIOUS ELASPED TIME
C BSM         ELAS1 = ELAPSED

C BSM         ELAPSED = etime_(ETIME_STRUCT)

C BSM         PRINT *, 'MAIN: A GETADP  ELAPSED = ', ELAPSED

C BSM         ELAS1 = ELAPSED - ELAS1
C BSM         PRINT *, 'MAIN: A GETADP TIME DIFFERENCE: ELASl = ', ELAS1
C############################################################

      CALL NEARPT

      CALL ADPTOS

C############################################################
C BSM         CCC = clock_()
C BSM         PRINT *, 'MAIN:  A ADPTOS: CCC = ', CCC

C        SAVE PREVIOUS ELASPED TIME
C BSM         ELAS1 = ELAPSED

C BSM         ELAPSED = etime_(ETIME_STRUCT)

C BSM         PRINT *, 'MAIN: A ADPTOS  ELAPSED = ', ELAPSED

C BSM         ELAS1 = ELAPSED - ELAS1
C BSM         PRINT *, 'MAIN: A ADPTOS TIME DIFFERENCE: ELASl = ', ELAS1
C############################################################
      IF(.NOT.LPRDAT) GO TO 30
      WRITE(LUOUT,1111)
      DO I=1,JSTA
      WRITE(LUOUT,2222) IDSTN(I),ALON(I),ALAT(I),STI(I),STJ(I),
     &              NRPT(I),DISTNN(I)
      WRITE(LUOUT,3333) ((ZTRUV(I,J,L),J=1,GNVARP),L=1,GNLEV)
      WRITE(LUOUT,4444)
      END DO
1111  FORMAT(///'ADPUPA REPORTS SUBJECTED TO QCADP STEP ARE ',
     &          'PRINTED HERE:'//)
2222  FORMAT('IDSTN=',I6,'  LON.=',F5.1,'  LAT.=',F5.1,
     &       '  STI=',F5.1,'  STJ=',F5.1,'  NRPT=',I5,
     &       '  DISTNN=',F6.1,' KM')
3333  FORMAT(10F10.2)
4444  FORMAT(/)
   30 CONTINUE

      PRINT 40
   40 FORMAT(/// 'END TOSSOUT PROCEDURE, PLANNED EXIT...' )

      CALL W3TAGE('QCADP') 
      STOP
      END
