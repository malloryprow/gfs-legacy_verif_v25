
      SUBROUTINE WRITSM(NDATEI,BLK1,BLK2,VFILE,RMAPI,
     &                  GCODEI,PAGEI)

C$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
C                .      .    .                                       .  
C SUBPROGRAM:    WRITSM      WRITE VERIFICATIONS FROM CURRENT OBS       
C   PRGMMR: VLCEK             ORG: W/NP12        DATE: 1999-06-09 
C                                                                       
C ABSTRACT: THIS SUBROUTINE WRITES THE S1 SCORES, MEAN, STANDARD,       
C   AND RMS ERRORS, & MEAN AND STANDARD DEVIATIONS OF THE OBSERVED      
C   VALUES OVER A PREDEFINED AREA FOR Z, T, RH, SPEED, AND VECTOR       
C   AT 850, 500, 250, AND 100 MB ON A SEQUENTIAL FILE WITH A FORMAT
C   OF HALF-WORDS TO CONSERVE ARCHIVE DISK SPACE AND TO PERMIT
C   SELECTION OF STATS USING PROGRAM /SELECSUM/ ON CRAY.  EACH 
C   RECORD CONTAINS A SET OF IDENTIFIERS WHICH INCLUDES FORECAST 
C   MODEL, FORECAST HOUR, AREA NAME, FORECAST GRID MAP TYPE AND 
C   GENERATING CODE.  THE VERIFICATION STATS ARE COMPUTED FROM
C   SUMMATIONS OF ERROR TERMS AND OBSERVATION VALUES AND COUNTS
C   OVER EACH AREA PROVIDED BY THE ARGUMENT LIST.  EACH OUTPUT
C   RECORD CORRESPONDS TO A PAGE OF AREA STATISTICS PRINTED BY 
C   SUBROUTINE /PRTSTA/, MINUS THE COUNTS AND STATION LISTS.
C                                                                       
C PROGRAM HISTORY LOG:                                                  
C   96-06-25  Y. ZHANG  
C   98-07-14  C. VLCEK COMPILE IN F90.  NO ACTUAL CODE CHANGE BUT SEE 
C                      NOTE ON EFFECT OF SWITCH FROM 8 TO 10-DIGIT
C                      Y2K-COMPLIANT DATE IN FIRST ARG INPUT /NDATE/.
C   99-06-09  C. VLCEK COMPILE ON IBM RS6000.
C                                                                       
C USAGE:  CALL WRITSM (NDATE,BLKID(11),BLKID(12),VFILE,RMAP,
C        &             GCODE,PAGE)
C
C   INPUT ARGUMENT LIST: 
C
C                 ALL INPUT ARGUMENTS ARE SAME AS SHOWN IN WRTSTA.  
C                                                                       
C   OUTPUT ARGUMENT LIST:                                               
C                                                                       
C                 NONE
C
C   OUTPUT FILES:                                                       
C     FT10F001 - SEQUENTIAL FILE CONTAINING IDENTIFIERS AND             
C                VERIFICATION DATA DESCRIBED IN ABSTRACT.               
C                                                                       
C                                                                       
C REMARKS: CAUTION -- OBSERVATION STATISTICS WILL NOT BE SAVED IF       
C   PRINT OPTION (PRSTAT=T, CALLING SUBROUTINE PRTSTA) IS ALSO          
C   EXERCISED, DUE TO DUAL USE OF ARGUMENT LIST.  ERROR STATS ARE O.K.  
C                                                                       
C     NOTE: DATES ARE Y2K-COMPLIANT, BUT 10-DIGIT DATE WILL REACH
C           MAX LIMIT FOR INTEGER*4 (NDATEO) IN YEAR 2148.
C    
C
C ATTRIBUTES:                                                           
C   LANGUAGE: IBM FORTRAN 90
C   MACHINE:  IBM RS6000
C                                                                       
C$$$                                                                    

      IMPLICIT  INTEGER(G)

      PARAMETER    (GORFIL=22, GNVAR=4, GVECTV=4)
      PARAMETER    (GNVARP=(GNVAR+1), GNLEV=4)
      PARAMETER    (GPROW = (GNLEV*GNVARP))
      PARAMETER     (GMXRUN=15)

      COMMON /UNITS / IUAREA, IUADP    , IUGBD(GORFIL) , IUGBI(GORFIL) ,
     &                IUJET , IUSTA    , IUOUT  , IUOPN, IUTAB

      REAL         RMAPI   , GCODEI , PAGEI(6,GPROW)
      REAL         BLK1  , BLK2    , BKID(2), PPDAT
      REAL(4)      RMAPO , GCODEO  , PAGEO(6,GPROW)
      INTEGER      NDATEI
      INTEGER(4)   NDATEO
      CHARACTER(8) VFILE , AREA

      EQUIVALENCE  (AREA,BKID(1))

      BKID(1) = BLK1
      BKID(2) = BLK2

      PAGEO = PAGEI
      NDATEO = NDATEI
      RMAPO  = RMAPI
      GCODEO = GCODEI

      WRITE(IUSTA) NDATEO,NDATEO,AREA,VFILE,RMAPO,GCODEO,PAGEO
      RETURN
      END
