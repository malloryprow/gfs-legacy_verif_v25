
      SUBROUTINE PRTSTA (STATS,S1,S1K,BLKID,VFILE,NF,COUNT,DEV)

C$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
C                .      .    .                                       .  
C SUBPROGRAM:    PRTSTA      PRINT VERIFICATIONS FROM CURRNT OBS        
C   PRGMMR: VLCEK            ORG: W/NP12    DATE: 1999-06-07 
C                                                                       
C ABSTRACT: THIS SUBROUTINE PRINTS THE S1 SCORES, MEAN, STANDARD,       
C   AND RMS ERRORS, & MEAN AND STANDARD DEVIATIONS OF THE OBSERVED      
C   VALUES OVER A PREDEFINED AREA FOR Z, T, RH, SPEED, AND VECTOR       
C   AT 850, 500, 250, AND 100 MB, ALONG WITH ASSOCIATED COUNTS AND      
C   ADP STATION LISTS.  TITLE INFORMATION INCLUDES FORECAST MODEL,      
C   FORECAST HOUR, AREA AND ASSOCIATED LAT.-LON. BOUNDARIES, AND        
C   DATE OF VERIFICATION.  STATS ARE COMPUTED FROM SUMMATIONS OF        
C   ERROR TERMS AND OBSERVATION VALUES AND COUNTS OVER EACH AREA        
C   PROVIDED BY THE ARGUMENT LIST.  OPTIONAL PRINT OUTPUT PROVIDES      
C   THE INDIVIDUAL STATION OBSERVATIONS, CORRESPONDING FORECASTS        
C   INTERPOLATED TO THE STATION, AND THE (F-O) ERRORS.                  
C                                                                       
C PROGRAM HISTORY LOG:                                                  
C   76-??-??  STACKPOLE                                                 
C   ??-??-??  STACKPOLE   REMOVED FROM OPERATIONS TO CONSERVE CORE      
C   85-10-10  VLCEK       REINSTATED FOLLOWING CONVERSION TO NAS        
C   89-01-25  VLCEK       CONVERT FROM FORTRAN 66 TO VS FORTRAN 77      
C   94-06-24  VLCEK       MINOR CHANGES AS PART OF CONVERSION OF        
C                         MAIN FROM ACUMVER4 TO SUMAC4                  
C   96-06-25  Y. ZHNAG    MOVE IT TO CRAY AND REFINE IT.
C   98-07-14  VLCEK       COMPILE IN FORTRAN 90, PRINT 4-DIGIT YEAR
C   99-06-07  VLCEK       COMPILE oN IBM RS6000.
C                                                                       
C USAGE:    CALL PRTSTA(STATS,S1,S1K,BLKID,VFILE,NF,COUNT)              
C   INPUT ARGUMENT LIST:                                                
C     STATS    - R*8 ARRAY CONTAINING SUMMATIONS OF ERROR TERMS         
C                AND OBSERVED VALUES OVER AN AREA.                      
C     S1       - R*8 ARRAY CONTAINING SUMMATIONS (OF NUMERATOR          
C                AND DENOMINATOR) FOR S1 CALCULATIONS.                  
C     S1K      - R*8 COUNTS ASSOCIATED WITH S1 SUMMATIONS ABOVE         
C     BLKID    - R*8 ARRAY CONTAINING DATE AND AREA INFORMATION         
C     VFILE    - C*8 WORD IDENTIFYING FORECAST MODEL AND HOUR,          
C                CORRESPONDS TO DDNAME OF ACCUMULATION FILE;            
C                SEE SUBROUTINE /ACCUMU/.                               
C     NF       - I*4 VARIABLE USED AS SUBSCRIPT TO C*8 NAMELIST         
C                ARRAY /FORFIL/, IDENTIFYING FORECAST MODEL AND         
C                HOUR SIMILAR TO VFILE ABOVE, EXCEPT THAT NAME IS       
C                THAT OF CORRESPONDING FORECAST FILE DDNAME.  IT        
C                IS USED ONLY ONCE, IN CONJUNCTION WITH INDIVIDUAL      
C                STATION STATISTICS.                                    
C                                                                       
C   OUTPUT ARGUMENT LIST:                                               
C     STATS    - R*8 COMPUTED STATS (MEAN, S.D., RMS, ETC.),            
C                OVERWRITING ARGUMENT INPUT VALUES.                     
C     S1       - R*8 S1 SCORES (OVERWRITING INPUT NUMERATOR VALUE)      
C                AND COUNTS (FROM S1K, OVERWRITING DENOMINATORS)        
C                                                                       
C   OUTPUT FILES:                                                       
C     FT06F001 - PRINTED STATISTICS AS DEFINED IN ABSTRACT.             
C                                                                       
C REMARKS: 230 STATEMENTS.
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

      PARAMETER  (GORFIL=22, GNVAR=4, GVECTV=4)
      PARAMETER  (GQ1=7, GQ2=11, GQ3=52, GQ4=33, GQ5=34)
      PARAMETER  (GQ6=0, GQ7=0, GQ8=0, GQ9=0, GQ10=0)

C     GQ1 IS HEIGHT, GQ2 IS TEMPERATURE, GQ3 IS RELATIVE HUMIDITY.    
C     GQ4 IS U-COMPONENT OF WIND; V-COMPONENT IS ASSUMED TO BE GQ4+1. 
C     REMAINING GQ VALUES ARE RESERVED FOR FUTURE USE.                

      PARAMETER  (GNVARP=(GNVAR+1), GNLEV=4)
      PARAMETER  (GP1=0, GP2=850, GP3=0, GP4=500, GP5=0, GP6=0)
      PARAMETER  (GP7=250, GP8=0, GP9=0, GP10=100, GP11=0, GP12=0)
      PARAMETER  (GP13=0, GP14=0, GP15=0)
      PARAMETER  (GIMAX=360, GJMAX=181, GMXDIM=(GIMAX*GJMAX) )
      PARAMETER  (GMXSTN=1100, GMINBN=0, GMAXBN=99999)
      PARAMETER  (GNERQU=10, GNGRQU=6, GACCUM=(GNERQU*2) )
      PARAMETER  (GMXTBL=600, GSVBL=12,  GNMAPS=17)
      PARAMETER  (GTWO=2, GMXARA=15, GMXRUN=15)

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

      REAL         MEAN(GNLEV), AMEAN , SIGMA , STATS  , STNDEV
      REAL         DEVIAT     , MINWND, MAXWND, ALOLA
      INTEGER      VDATE1     , VDATE2, VDATE , RPT    , KMAP  ,
     &             STNLST     , Q, P  , IGRID  , NFILE , PLEV  ,
     &             NDATE(4)   , IDATE , GCODE , QWANT  ,
     &             IBNI(10)   , ISNI(10)
      CHARACTER(8) VFILE, AREA, FORFIL
      CHARACTER(4) FHR  , KRUN, ISNC(10)      , IBNC(10)
      LOGICAL      PRSTAT     , WRSTAT, DIAG  , FIRST
      DIMENSION    STATS(GACCUM,GNLEV), S1(GNGRQU,GNLEV) ,
     &             STNDEV(GTWO,GNLEV) , S1K(GNGRQU,GNLEV),
     &             DEVIAT(GNLEV)      , RMEAN(GNLEV)     ,
     &             RDVIAT(GNLEV)      , IBN(10), ISN(10) ,
     &             BLKID(GSVBL)       , PLINE(12)        ,
     &             DEV(GACCUM,GNLEV)  , COUNT(GACCUM,GNLEV)

      EQUIVALENCE  (IBNI(1), IBNC(1)), (ISNI(1),ISNC(1))

      DATA BIG     /9999999.9/
      DATA ESP     /  1.E-8  /

 9970 FORMAT (47X, '*** NATIONAL METEOROLOGICAL CENTER ***', /
     &        45X, 'SUMMARY VERIFICATIONS AGAINST UPPER AIR DATA'/
     &        49X, 'FOR AREA ', I3, ' (', A8, ') OUTLINED BY'//
     &        35X, 'EAST LONGITUDE', F6.1, ' TO', F6.1, ', ',
     &             'LATITUDE',F6.1,' TO',F6.1,' INCLUSIVE')

 9960 FORMAT ( 43X, 'FIELDS VERIFIED ARE',
     &             ' FROM FILE WITH DDNAME   ', A8)
 9930 FORMAT (/37X, 'ON NMC GRID K = ', I3,
     &              ' FROM MODEL WITH GENERATING CODE G = ', I3)
 9920 FORMAT ( 49X, 'FOR VERIFICATION TIME',
     &         I3, 'Z', I3, '/', I2, '/', I4)
 9900 FORMAT ( 62X, '**********', /,
     &         ' ', 42X, 'VERIFICATIONS FOR ENTIRE AREA NO. ', I3,
     &         ' (', A8, ')' /
     &         47X, '(NO. OF REPORTS AVERAGED IN PARENTHESES)'   )
 9602 FORMAT (/ 6X, 'PRESSURE LEVEL... ', I5, 'MB',
     &         15X, 'ERROR STATISTICS', 13X, '*', 1X,
     & 'OBSERVATION STATISTICS', 1X, '*', 1X, 'RATIO OF ERRORS TO OBS')
 9603 FORMAT (' ', 37X, 'MEAN', 10X, 'S.D.', 10X, 'RMS', 6X, '*',
     & 5X, 'MEAN', 8X, 'S.D.', 3X, '*', 2X, 'RATIO OF', 4X, 'RATIO OF')
 9604 FORMAT (' ', 24X, 'S1', 10X, 'ERROR', 9X, 'ERROR', 9X, 'ERROR',
     & 5X, '*', 5X, 'OBS', 9X, 'OBS', 4X, '*', 4X, 'MEANS', 7X, 'S.D.')
 9680 FORMAT (' ', 2X, 'HEIGHT      (M)', 1X, F6.1, '(', F5.0, ')', 1X,
     & F6.1, '(', F5.0, ')', 2 (1X, F6.1, '(', F5.0, ')' ), 1X, '*', 1X,
     & 1PE9.2, 3X, 1PE9.2, 2X, '*', 1X, 1PE9.2, 1X, 0PF9.2, 5X, 'HGT.')
 9670 FORMAT (' ', 2X, 'TEMPERATURE (C)', 2X, F5.1, '(', F5.0, ')', 2X,
     & F5.1, '(', F5.0, ')', 2 (1X, F6.1, '(', F5.0, ')' ), 1X, '*',
     & 4X, F6.2, 6X, F6.2, 2X, '*', 1X, 1PE9.2, 4X, 0PF6.2, 5X, 'TEMP.')
 9660 FORMAT (' ', 2X, 'REL. HUMID. (%)', 2X, F5.1, '(', F5.0, ')',
     & 2X, F5.1, '(', F5.0, ')', 2 (2X, F5.1, '(', F5.0, ')' ), 1X, '*',
     & 4X, F6.2, 6X, F6.2, 2X, '*', 4X, F6.2, 4X, F6.2, 5X, 'R.H.')
 9650 FORMAT (' ', 2X, 'WIND SPEED(M/S)', 15X, F6.1, '(', F5.0, ')',
     &          2 (1X, F6.1, '(', F5.0, ')' ), 1X, '*', 4X, F6.2, 6X,
     &          F6.2, 2X, '*', 4X, F6.2, 4X, F6.2, 5X, 'SPEED' )
 9640 FORMAT (' ', 2X, 'VELOCITY  (M/S)', 15X, F6.1, '(', F5.0, ')',
     &          2 (1X, F6.1, '(', F5.0, ')' ), 1X, '*', 4X, F6.2, 6X,
     &          F6.2, 2X, '*', 4X, F6.2, 4X, F6.2, 5X, 'VELOC.' )
 9605 FORMAT (' ', 131(1H*) )

      NSTB  = IFIX(BLKID(2)) - 1
      NA    = IFIX(BLKID(1))
      KMAP  = IFIX(BLKID(9))
      GCODE = IFIX(BLKID(10))
      IDATE = IFIX(BLKID(8))
      PRINT 9970, NA, AREA(NA), (BLKID(N), N=3,6)
      PRINT 9960, VFILE
      PRINT 9930, KMAP, GCODE
      NDATE(1) = IDATE/1000000
      NRMYR    = MOD(IDATE,1000000)
      NDATE(2) = NRMYR/10000
      NRMMN    = MOD(NRMYR,10000)
      NDATE(3) = NRMMN/100
      NDATE(4) = MOD(NRMMN,100)
      PRINT 9920, NDATE(4), NDATE(2), NDATE(3), NDATE(1)
      FIRST = .TRUE.

C     COMPUTE AND PRINT AVERAGES FOR ENTIRE AREA                       

      DO 70  NL = 1, GNLEV
      DO 20 NV = 1,GNGRQU,2
      IF(ABS(S1K(NV,NL)).LT.ESP) GO TO 16
      S1(NV,NL) = 100. * S1(NV,NL) / S1(NV+1,NL)
      S1(NV+1,NL) = S1K(NV,NL)
      GO TO 18
   16 CONTINUE
      S1(NV,NL) = BIG
      S1(NV+1,NL) = 0
   18 CONTINUE
   20 CONTINUE
      L   = 1
      LP1 = L + 1
      DO 30 J=2,GACCUM,4
      JM1 = J - 1
      JP2 = J + 2
      JP1 = J + 1
      IF((ABS(COUNT(J,NL)).LT.ESP).OR.(ABS(COUNT(JP2,NL)).LT.ESP).OR.
     &   (ABS(COUNT(JM1,NL)).LT.ESP).OR.
     &   (ABS(COUNT(JP1,NL)).LT.ESP)) GO TO 25
      STNDEV(L,NL)   = STATS(J,NL)
      STNDEV(L+1,NL) = STATS(J+2,NL)
      MEAN(NL)       = STNDEV(L,NL)   / COUNT(J,NL)
      AMEAN          = STATS(JM1,NL)  / COUNT(JM1,NL)
      DEVIAT(NL)     = STNDEV(L+1,NL) / COUNT(JP2,NL)-((MEAN(NL))**2)
      SIGMA          = STATS(JP1,NL)  / COUNT(JP1,NL) - AMEAN **2
      IF(DEVIAT(NL).LT.0.) DEVIAT(NL) = 0.0
      IF(SIGMA.LT.0.)  SIGMA          = 0.0

C***  DEVIAT(NL) AND SIGMA ARE FORCED TO BE ZERO BECAUSE OF THE ROUND-OFF 
C***  ERROR.                                                             
C***  THIS ROUND-OFF ERROR IS DUE TO THE VERY SMALL DIFFERENCE IN THE    
C***  TWO TERMS INVOLVED IN COMPUTING THE STANDARD DEVIATION WHEN VERY   
C***  FEW ACCUMULATIONS ARE DONE                                         

      DEVIAT(NL) = SQRT(DEVIAT(NL))
      SIGMA      = SQRT(SIGMA)
      RMEAN(NL)  = STATS(JM1,NL) / COUNT(JM1,NL) / MEAN(NL)
      RDVIAT(NL) = BIG
      IF(DEVIAT(NL).NE.0.) RDVIAT(NL) = SIGMA / DEVIAT(NL)

C     STORE THE S.D. ERROR IN STATS, OVERWRITING                 
C     THE SUMMATION OF THE OBSERVED VALUES.                      

      STATS(J,NL)   = SIGMA
      DEV(JM1,NL)   = MEAN(NL)
      DEV(J,NL)     = DEVIAT(NL)
      DEV(JP1,NL)   = RMEAN(NL)
      DEV(JP2,NL)   = RDVIAT(NL)
      STATS(JM1,NL) = STATS(JM1,NL) / COUNT(JM1,NL)
      IF(MOD(JM1,4).EQ.3) STATS(JM1,NL) = SQRT(STATS(JM1,NL))
      STATS(JP1,NL) = STATS(JP1,NL) / COUNT(JP1,NL)
      IF(MOD(JP1,4).EQ.3) STATS(JP1,NL) = SQRT(STATS(JP1,NL))
      GO TO 30
   25 CONTINUE
      DEV(JM1,NL) = BIG
      DEV(J,NL)   = BIG
      DEV(JP1,NL) = BIG
      DEV(JP2,NL) = BIG
      STATS(JM1,NL) = BIG
      STATS(J  ,NL) = BIG
      STATS(JP1,NL) = BIG
   30 CONTINUE
      IF(FIRST) PRINT 9900, NA, AREA(NA)
      FIRST = .FALSE.
      PRINT 9602, PLEV(NL)
      PRINT 9603
      PRINT 9604
      PRINT 9605
      PRINT 9680, (S1(N,NL),N=1,2), STATS(1,NL), COUNT(1,NL),
     &            STATS(2,NL)     , COUNT(1,NL), STATS(3,NL),
     &            COUNT(3,NL)     , (DEV(M,NL),M=1,4)
      PRINT 9670, (S1(N,NL),N=3,4), STATS(5,NL), COUNT(5,NL),
     &            STATS(6,NL)     , COUNT(5,NL),
     &            STATS(7,NL)     , COUNT(7,NL), (DEV(M,NL),M=5,8)
      PRINT 9660, (S1(N,NL),N=5,6), STATS(9,NL), COUNT(9,NL),
     &            STATS(10,NL)    , COUNT(9,NL), STATS(11,NL),
     &            COUNT(11,NL)    , (DEV(M,NL),M=9,12)
      PRINT 9650, STATS(13,NL)    , COUNT(13,NL),
     &            STATS(14,NL)    , COUNT(13,NL),
     &            STATS(15,NL)    , COUNT(15,NL), (DEV(M,NL),M=13,16)
      PRINT 9640, STATS(17,NL)    , COUNT(17,NL),
     &            STATS(18,NL)    , COUNT(17,NL),
     &            STATS(19,NL)    , COUNT(19,NL), (DEV(M,NL),M=17,20)
      PRINT 9605
   70 CONTINUE
 9780 FORMAT (/16X, 'FOLLOWING STATIONS ARE INCLUDED IN AREA'
     &              , I4, ' (', A8, ')' )
 9790 FORMAT ( 10(4X,I5) )
      PRINT 9780, NA, AREA(NA)
      PRINT 9790, (STNLST(NA,NS),NS=1,NSTB)
      RETURN
      END
