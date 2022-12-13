
      SUBROUTINE WRTSTA(STATS,S1,S1K,BLKID,VFILE,NF,COUNT,DEV)

C$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
C                .      .    .                                       .  
C SUBPROGRAM:    WRTSTA      WRITE VERIFICATIONS FROM CURRENT OBS       
C   PRGMMR: VLCEK            ORG: W/NP12   DATE: 1999-06-09 
C                                                                       
C ABSTRACT: THIS SUBROUTINE WRITES THE S1 SCORES, MEAN, STANDARD,       
C   AND RMS ERRORS, & MEAN AND STANDARD DEVIATIONS OF THE OBSERVED      
C   VALUES OVER A PREDEFINED AREA FOR Z, T, RH, SPEED, AND VECTOR       
C   AT 850, 500, 250, AND 100 MB ON A SEQUENTIAL FILE.  EACH RECORD     
C   CONTAINS A SET OF IDENTIFIERS WHICH INCLUDES FORECAST MODEL,        
C   FORECAST HOUR, AREA NAME, FORECAST GRID MAP TYPE AND GENERATING     
C   CODE.  THE VERIFICATION STATS ARE COMPUTED FROM SUMMATIONS OF       
C   ERROR TERMS AND OBSERVATION VALUES AND COUNTS OVER EACH AREA        
C   PROVIDED BY THE ARGUMENT LIST.  EACH OUTPUT RECORD CORRESPONDS      
C   TO A PAGE OF AREA STATISTICS PRINTED BY SUBROUTINE /PRTSTA/,        
C   MINUS THE COUNTS AND STATION LISTS.                                 
C                                                                       
C PROGRAM HISTORY LOG:                                                  
C   83-04-??  VLCEK                                                     
C   89-01-25  VLCEK       CONVERT FROM FORTRAN 66 TO VS FORTRAN 77      
C   94-06-24  VLCEK       MINOR REVISIONS AS PART OF CONVERSION OF      
C                         MAIN FROM ACUMVER4 TO SUMAC4                  
C   96-06-25  Y. ZHANG    MOVE IT TO CRAY, ADD A SUBROUTINE, NAMED 
C                         AS WRITSM WHICH IS NEWLY CODED, IN ORDER
C                         TO WRITE DOWN THE REQUIRED STATISTICS BY
C                         A SAME FORMAT AS CURRENT OPERATION FOR
C                         BEING UNIQUELY READ WITH A SELECSUM CODE.
C   98-07-14  VLCEK       MINOR REVISIONS AS PART OF CONVERSION TO      
C                         Y2K-COMPLIANCE.  COMPILE IN F90.
C   98-11-17  VLCEK       CHECK TWO PREVIOUSLY UNTESTED DENOMINATORS
C                         TO AVOID DIVIDING BY ZERO.
C   99-06-09  VLCEK       COMPILE ON IBM RS6000.
C                                                                       
C USAGE:    CALL WRTSTA(STATS,S1,S1K,BLKID,VFILE,NF,COUNT)              
C   INPUT ARGUMENT LIST:                                                
C     STATS    - R*8 ARRAY CONTAINING SUMMATIONS OF ERROR TERMS         
C                AND OBSERVED VALUES OVER AN AREA.  IT CONTAINS         
C                COMPUTED STATS IF SUBROUTINE /PRTSTA/ WAS CALLED       
C                FIRST (SEE OUTPUT ARG LIST).                           
C     S1       - R*4 ARRAY CONTAINING SUMMATIONS (OF NUMERATOR          
C                AND DENOMINATOR) FOR S1 CALCULATIONS.  IF SUB-         
C                ROUTINE /PRTSTA/ WAS CALLED FIRST, THE ARRAY           
C                CONTAINS S1 SCORES AND COUNTS (SEE OUT-ARG LIST).      
C     S1K      - R*4 COUNTS ASSOCIATED WITH S1 SUMMATIONS ABOVE         
C     BLKID    - R*4 ARRAY CONTAINING DATE AND AREA INFORMATION         
C     VFILE    - C*8 WORD IDENTIFYING FORECAST MODEL AND HOUR,          
C                CORRESPONDS TO DDNAME OF ACCUMULATION FILE;            
C                SEE SUBROUTINE /ACCUMU/.                               
C     NF       - I*4 VARIABLE USED AS SUBSCRIPT TO C*8 NAMELIST         
C                ARRAY /FORFIL/, IDENTIFYING FORECAST MODEL AND         
C                HOUR SIMILAR TO VFILE ABOVE, EXCEPT THAT NAME IS       
C                THAT OF CORRESPONDING FORECAST FILE DDNAME.  IT        
C                IS NOT REFERENCED IN THIS SUBROUTINE, ALTHOUGH IT      
C                IS USED IN SUBROUTINE /PRTSTA/ (SAME ARG LIST).        
C                                                                       
C   OUTPUT ARGUMENT LIST:                                               
C     STATS    - R*8 COMPUTED STATS (MEAN, S.D., RMS, ETC.),            
C                OVERWRITING ARGUMENT INPUT VALUES.                     
C     S1       - R*4 S1 SCORES (OVERWRITING INPUT NUMERATOR VALUE)      
C                AND COUNTS (FROM S1K, OVERWRITING DENOMINATORS)        
C                                                                       
C   OUTPUT FILES:                                                       
C     FT06F001 - PRINTED IDENTIFICATION OF RECORDS WRITTEN.             
C     FT70F001 - SEQUENTIAL FILE CONTAINING IDENTIFIERS AND             
C                VERIFICATION DATA DESCRIBED IN ABSTRACT.               
C                                                                       
C                                                                       
C REMARKS: CAUTION -- OBSERVATION STATISTICS WILL NOT BE SAVED IF       
C   PRINT OPTION (PRSTAT=T, CALLING SUBROUTINE PRTSTA) IS ALSO          
C   EXERCISED, DUE TO DUAL USE OF ARGUMENT LIST.  ERROR STATS ARE O.K.  
C   131 STATEMENTS.
C                                                                       
C ATTRIBUTES:                                                           
C   LANGUAGE: FORTRAN 90   IBM FORTRAN
C   MACHINE:  IBM RS6000
C                                                                       
C$$$                                                                    

      IMPLICIT  INTEGER(G)

C     /G/ IS RESERVED FOR CONSTANTS NAMED IN A PARAMETER STATEMENT.   
C     LIST AND EXPLANATIONS OF PARAMETER NAMES CAN BE FOUND IN        
C     SUBROUTINE ACCUMU.                                              
C     GQ1 IS HEIGHT, GQ2 IS TEMPERATURE, GQ3 IS RELATIVE HUMIDITY.    
C     GQ4 IS U-COMPONENT OF WIND; V-COMPONENT IS ASSUMED TO BE GQ4+1. 
C     REMAINING GQ VALUES ARE RESERVED FOR FUTURE USE.                

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
      PARAMETER  (GMXTBL=600, GSVBL=12,  GNMAPS=17)
      PARAMETER  (GPROW=(GNLEV*GNVARP), GTWO=2, GMXARA=15)
      PARAMETER  (GMXRUN=15)

      COMMON /ADPSTN/ IDSTN(GMXSTN)    , ALON(GMXSTN)  , ALAT(GMXSTN) ,
     &                NRPT(GMXSTN)     , DISTNN(GMXSTN),
     &                ZTRUV(GMXSTN,GNVARP,GNLEV),
     &                STI(GMXSTN), STJ(GMXSTN)  , JSTA
      COMMON /MAPINF/ NSTA, KMAP , IMXMAP       , JMXMAP,
     &                NCYCMP     , F9, DATENL
      COMMON /AREAS / ALOLA(GMXARA,4)  , AREA(GMXARA)  ,
     &                KSTN(GMXARA)     , STNLST(GMXARA , GMXTBL)
      COMMON /RAFILE/ FORFIL(GORFIL)   , NFILE         , GCODE ,
     &                DATA(GMXDIM)     , DATA1(GMXDIM) , MINWND,
     &                MAXWND, IGRID    , VDATE1, VDATE2,
     &                VDATE , DIAG     , PRSTAT, WRSTAT,
     &                INTPM , KRUN     , FHR(GORFIL)
      COMMON /LEVELS/ PLEV(GNLEV)      , IPL(GNLEV)    , QWANT(GNVARP)
      COMMON /STNPRT/ STNERR(GMXSTN,GNERQU,GNLEV),
     &                STNGRD(GMXSTN,GNGRQU,GNLEV)
      COMMON /UNITS / IUAREA, IUADP    , IUGBD(GORFIL) , IUGBI(GORFIL) ,
     &                IUJET , IUSTA    , IUOUT  , IUOPN, IUTAB

      REAL         MEAN(GNLEV), AMEAN , SIGMA, STATS, STNDEV,DEVIAT
      REAL         MINWND     , MAXWND, ALOLA, PAGE(6,GPROW)
      REAL         RMAP       , RCODE
      INTEGER      VDATE1     , VDATE2, VDATE , RPT  , QWANT , PLEV
      INTEGER      IDATE(4)   , NDATE , STNLST, NFILE, IGRID , GCODE
      LOGICAL      PRSTAT     , WRSTAT, DIAG  , FIRST
      CHARACTER(8) FORFIL     , VFILE , AREA
      CHARACTER(4) FHR, KRUN

      DIMENSION    STATS(GACCUM,GNLEV), STNDEV(GTWO,GNLEV),
     &             BLKID(GSVBL)       , S1(GNGRQU,GNLEV)  ,
     &             S1K(GNGRQU,GNLEV)  , DEVIAT(GNLEV)     ,
     &             RMEAN(GNLEV)       , RDVIAT(GNLEV)     ,
     &             IBN(10), ISN(10)   , PLINE(12)         ,
     &             DEV(GACCUM,GNLEV)  , COUNT(GACCUM,GNLEV)


      DATA  BIG /9999999.9/
      DATA  ESP /  1.E-8  /

      NDATE = IFIX(BLKID(8))

C     COMPUTE AND WRITE AVERAGES FOR ENTIRE AREA                   

      DO 70  NL = 1, GNLEV

C     SKIP THIS PART IF PRSTAT=T BECAUSE SUBROUTINE PRTSTA         
C     HAS ALREADY COMPUTED STATS AND PLACED THEM BACK INTO         
C     ARGUMENT ARRAYS S1 AND STATS USED BY BOTH SUBROUTINES.       

      IF (PRSTAT) GO TO 40
      DO NV = 1,GNGRQU,2
        IF (ABS(S1K(NV,NL)).LT.ESP.OR.ABS(S1(NV+1,NL)).LT.ESP)  THEN
          S1(NV,NL)   = BIG
          S1(NV+1,NL) = 0
        ELSE
          S1(NV,NL)   = 100.*S1(NV,NL) / S1(NV+1,NL)
          S1(NV+1,NL) = S1K(NV,NL)
        ENDIF
      END DO
      L   = 1
      LP1 = L + 1
      DO 30 J=2,GACCUM,4
        JM1 = J - 1
        JP2 = J + 2
        JP1 = J + 1
        IF ((ABS(COUNT(J,NL)).LT.ESP).OR.(ABS(COUNT(JP2,NL)).LT.ESP).OR.
     &     (ABS(COUNT(JM1,NL)).LT.ESP).OR.(ABS(COUNT(JP1,NL)).LT.ESP))  
     &     GO TO 25
          STNDEV(L,NL)   = STATS(J,NL)
          STNDEV(L+1,NL) = STATS(J+2,NL)
          MEAN(NL)       = STNDEV(L,NL) / COUNT(J,NL)
          IF (ABS(MEAN(NL)).LT.ESP)    GO TO 25
          AMEAN          = STATS(JM1,NL) / COUNT(JM1,NL)
          DEVIAT(NL)     = STNDEV(L+1,NL) / COUNT(JP2,NL)
     &                   - ((MEAN(NL))**2)
          SIGMA          = STATS(JP1,NL) / COUNT(JP1,NL) - AMEAN **2
          IF (DEVIAT(NL).LT.0.)  DEVIAT(NL) = 0.0
          IF (SIGMA.LT.0.)  SIGMA = 0.0

C***      DEVIAT(NL) AND SIGMA ARE FORCED TO BE ZERO BECAUSE OF THE ROUND-OFF 
C***      ERROR.                                                             
C***      THIS ROUND-OFF ERROR IS DUE TO THE VERY SMALL DIFFERENCE IN THE    
C***      TWO TERMS INVOLVED IN COMPUTING THE STANDARD DEVIATION WHEN VERY   
C***      FEW ACCUMULATIONS ARE DONE                                         

          DEVIAT(NL) = SQRT(DEVIAT(NL))
          SIGMA      = SQRT(SIGMA)
          RMEAN(NL)  = (STATS(JM1,NL)/COUNT(JM1,NL))
     &               / MEAN(NL)
          RDVIAT(NL) = BIG
          IF (DEVIAT(NL).NE.0.)  RDVIAT(NL) = SIGMA/DEVIAT(NL)

C         STORE THE S.D. ERROR IN STATS, OVERWRITING           
C         THE SUMMATION OF THE OBSERVED VALUES.                

          STATS(J,NL)   = SIGMA
          DEV(JM1,NL)   = MEAN(NL)
          DEV(J,NL)     = DEVIAT(NL)
          DEV(JP1,NL)   = RMEAN(NL)
          DEV(JP2,NL)   = RDVIAT(NL)
          STATS(JM1,NL) = STATS(JM1,NL) / COUNT(JM1,NL)
          IF (MOD(JM1,4).EQ.3) STATS(JM1,NL) = SQRT(STATS(JM1,NL))
          STATS(JP1,NL) = STATS(JP1,NL) / COUNT(JP1,NL)
          IF (MOD(JP1,4).EQ.3) STATS(JP1,NL) = SQRT(STATS(JP1,NL))
          GO TO 30
   25   CONTINUE
        DEV(JM1,NL) = BIG
        DEV(J,NL)   = BIG
        DEV(JP1,NL) = BIG
        DEV(JP2,NL) = BIG
        STATS(JM1,NL) = BIG
        STATS(J  ,NL) = BIG
        STATS(JP1,NL) = BIG
   30 CONTINUE
   40 CONTINUE

C     PLACE STATS AND COUNTS IN ARRAY /PAGE/ TO GET              
C     PROPER LINEUP FOR WRITING SEQUENTIAL RECORD.               

      JNL = 5*(NL-1)
      DO 52 J=1,5
      JROW = JNL + J
      DO 51 I=1,6
      KS1 = 1 + 2*(J-1)
      KST = I + 4*(J-1) - 1
      KDV = I + 4*(J-1) - 4
      PAGE(I,JROW) = BIG
      IF(I.EQ.1.AND.J.LT.4) PAGE(I,JROW) = S1(KS1,NL)
      IF(I.GE.2.AND.I.LE.4) PAGE(I,JROW) = STATS(KST,NL)
      IF(I.GE.5)            PAGE(I,JROW) = DEV(KDV,NL)
   51 CONTINUE
   52 CONTINUE
   70 CONTINUE

C     ALL DONE WITH SETUP...NOW WRITE ONE RECORD CORRESPONDING     
C     TO ONE PAGE OF AREA STATISTICS (MINUS COUNTS & STATIONS)     

      NA       = IFIX(BLKID(1))
      RMAP     = BLKID(9)
      RCODE    = BLKID(10)
      IDATE(1) = NDATE/1000000
      IRMYR    = MOD(NDATE,1000000)
      IDATE(2) = IRMYR/10000
      IRMMN    = MOD(IRMYR,10000)
      IDATE(3) = IRMMN/100
      IDATE(4) = MOD(IRMMN,100)
      CALL WRITSM (NDATE,BLKID(11),BLKID(12),VFILE,RMAP,
     &             RCODE,PAGE)
      IF (DIAG) PRINT 9600, VFILE, AREA(NA),
     &          IDATE(4), IDATE(2), IDATE(3), IDATE(1)
 9600 FORMAT (' WROTE RECORD ON UNIT 71 FOR VFILE=', A8, 4X,
     &        'AREA=', A8, 4X, 'VERIFICATION TIME IS  ',
     &        I2, 'Z', 1X, 2(I2,1H/), I4//)

      RETURN
      END
