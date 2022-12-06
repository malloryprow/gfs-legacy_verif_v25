

      SUBROUTINE VERJET(STATS,S1,S1K,BLKID,VFILE,NF,COUNT,DEV)
C                                                                               
C$$$  SUBPROGRAM DOCUMENTATION BLOCK                                            
C                .      .    .                                       .          
C SUBPROGRAM:    VERJET      APPEND PARTIAL SUMS FOR 250 MB WINDS               
C   PRGMMR: VLCEK            ORG: W/NP12     DATE: 1999-06-09
C                                                                               
C ABSTRACT: THIS SUBROUTINE WRITES THE ERROR ACCUMULATION TERMS FOR             
C   250 MB WINDS EXCEEDING 80 KNOTS.  NOT ENOUGH OBSERVATIONS EXIST             
C   FOR RELIABLE STATISTICS OF SINGLE OBSERVATIONS, SO THE RAW TERMS            
C   ARE BEING APPENDED TO A SPECIAL FILE.  THIS FILE WILL BE READ BY            
C   A SMALL PROGRAM (JETVER) WHICH WILL CALCULATE MEAN AND RMSVE SCORES         
C   AT THE END OF EACH MONTH.                                                   
C                                                                               
C PROGRAM HISTORY LOG:                                                          
C   96-06-17  VLCEK                                                             
C   96-07-02  Y. ZHANG      CHANGE ALL I*4 OR R*4 ARGUMENTS INTO
C                           I*8 OR R*8 ARGUMENTS AND MODIFY CONSTANTS
C                           IN PARAMETERS, ADD THIS SUBROUTINE INTO
C                           CODE SUMAC4 RUN ON CRAY.
C   98-07-14  VLCEK         COMPILE IN F90, PRINT 4-DIGIT YEAR
C   99-06-09  VLCEK         COMPILE ON IBM RS6000.
C                                                                               
C USAGE:    CALL VERJET(STATS,S1,S1K,BLKID,VFILE,NF,COUNT)                      
C   INPUT ARGUMENT LIST:                                                        
C     STATS    - R*8 ARRAY CONTAINING SUMMATIONS OF ERROR TERMS                 
C                AND OBSERVED VALUES OVER AN AREA.                              
C     S1       - R*8 ARRAY CONTAINING SUMMATIONS FOR S1 CALCULATIONS            
C                (NOT USED HERE).                                               
C     S1K      - R*8 COUNTS ASSOCIATED WITH S1 SUMMATIONS (NOT USED)            
C     BLKID    - R*8 ARRAY CONTAINING DATE AND AREA INFORMATION                 
C     VFILE    - C*8 WORD IDENTIFYING FORECAST MODEL AND HOUR;                  
C                SEE SUBROUTINE /ACCUMU/.                                       
C     NF       - I*8 VARIABLE USED AS SUBSCRIPT TO C*8 NAMELIST                 
C                ARRAY /FORFIL/, IDENTIFYING FORECAST MODEL AND                 
C                HOUR SIMILAR TO VFILE ABOVE, EXCEPT THAT NAME IS               
C                THAT OF CORRESPONDING FORECAST FILE DDNAME.  IT                
C                IS NOT REFERENCED IN THIS SUBROUTINE, ALTHOUGH IT              
C                IS USED IN SUBROUTINE /PRTSTA/ (SAME ARG LIST).                
C                                                                               
C   OUTPUT ARGUMENT LIST:  (NONE)                                               
C                                                                               
C   OUTPUT FILES:                                                               
C     FT06F001 - PRINTED IDENTIFICATION OF RECORDS WRITTEN.                     
C     FT70F001 - SEQUENTIAL FILE CONTAINING IDENTIFIERS AND                     
C                ERROR TERMS FOR 250 MB WIND FORECASTS.                         
C                                                                               
C                                                                               
C REMARKS: NONE                                                                 
C                                                                               
C ATTRIBUTES:                                                                   
C   LANGUAGE: VS FORTRAN 90                                                     
C   MACHINE:  IBM RS6000
C                                                                               
C$$$                                                                            

      IMPLICIT  INTEGER(G)

C     /G/ IS RESERVED FOR CONSTANTS NAMED IN A PARAMETER STATEMENT.           
C     LIST AND EXPLANATIONS OF PARAMETER NAMES CAN BE FOUND IN                
C     SUBROUTINE ACCUMU.                                                      
C     GQ1 IS HEIGHT, GQ2 IS TEMPERATURE, GQ3 IS RELATIVE HUMIDITY.            
C     GQ4 IS U-COMPONENT OF WIND; V-COMPONENT IS ASSUMED TO BE GQ4+1.         
C     REMAINING GQ VALUES ARE RESERVED FOR FUTURE USE.  BELOW ...             
C     SELECT LEVELS FOR VERIFICATION.                                         

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

      COMMON /AREAS / ALOLA(GMXARA,4)  , AREA(GMXARA)  ,
     &                KSTN(GMXARA)     , STNLST(GMXARA , GMXTBL)
      COMMON /RAFILE/ FORFIL(GORFIL)   , NFILE         , GCODE ,
     &                DATA(GMXDIM)     , DATA1(GMXDIM) , MINWND,
     &                MAXWND, IGRID    , VDATE1, VDATE2,
     &                VDATE , DIAG     , PRSTAT, WRSTAT,
     &                INTPM , KRUN     , FHR(GORFIL)
      COMMON /ADPSTN/ IDSTN(GMXSTN)    , ALON(GMXSTN)  , ALAT(GMXSTN) ,
     &                NRPT(GMXSTN)     , DISTNN(GMXSTN),
     &                ZTRUV(GMXSTN,GNVARP,GNLEV),
     &                STI(GMXSTN) , STJ(GMXSTN) , JSTA
      COMMON /MAPINF/ NSTA, KMAP  , IMXMAP      , JMXMAP,
     &                NCYCMP      , F9, DATENL
      COMMON /LEVELS/ PLEV(GNLEV) , IPL(GNLEV)  , QWANT(GNVARP)

C     COMMON FOR ERRORS AND GRADIENT FOR EACH STATION AND LEVEL      

      COMMON /STNPRT/ STNERR(GMXSTN,GNERQU,GNLEV),
     &                STNGRD(GMXSTN,GNGRQU,GNLEV)
      COMMON /UNITS / IUAREA, IUADP   , IUGBD(GORFIL) , IUGBI(GORFIL) ,
     &                IUJET , IUSTA   , IUOUT, IUOPN  , IUTAB

      REAL         MEAN(GNLEV), AMEAN , SIGMA, STATS, STNDEV,DEVIAT
      REAL         MINWND     , MAXWND, ALOLA, PAGE(6,GPROW)
      REAL         RMAP       , RCODE , TMP(2)
      INTEGER      VDATE1     , VDATE2, VDATE , RPT  , QWANT , PLEV
      INTEGER      IDATE(4)   , NDATE , STNLST, NFILE, IGRID , GCODE
      LOGICAL      PRSTAT     , WRSTAT, DIAG  , FIRST
      CHARACTER(8) FORFIL     , VFILE , AREA  , C8AREA
      CHARACTER(4) FHR, KRUN

      DIMENSION    STATS(GACCUM,GNLEV), STNDEV(GTWO,GNLEV),
     &             BLKID(GSVBL)       , S1(GNGRQU,GNLEV)  ,
     &             S1K(GNGRQU,GNLEV)  , DEVIAT(GNLEV)     ,
     &             RMEAN(GNLEV)       , RDVIAT(GNLEV)     ,
     &             IBN(10), ISN(10)   , PLINE(12)         ,
     &             DEV(GACCUM,GNLEV)  , COUNT(GACCUM,GNLEV)

      EQUIVALENCE  (C8AREA,TMP(1))

      TMP(1)= BLKID(11)
      TMP(2)= BLKID(12)
      NDATE = IFIX(BLKID(8))

C     SELECT AND WRITE ERROR TERMS AND COUNTS FOR 250 MB WINDS             
C     NL IS LEVEL SUBSCRIPT, = 3 FOR 250 MB.                               

      NL = 3

C     INITIALIZE AS ZERO IN CASE COUNTS ARE ZERO FOR THIS RUN              

      SUMSE  = 0.
      SUMSO  = 0.
      SUMVE  = 0.
      SUM2SE = 0.
      SUM2SO = 0.
      SUM2VE = 0.
      CTSE   = 0.
      CTSO   = 0.
      CTVE   = 0.
      CT2SE  = 0.
      CT2SO  = 0.
      CT2VE  = 0.

      DO 30 J=13,20
        IF(COUNT(J,NL).EQ.0.) GO TO 40
   30 CONTINUE

C     COUNTS ARE O.K. ... FETCH CURRENT SUMMATION                          
C     OBS SPEED IS DUPLICATED IN 18 AND 20                                 

      SUMSE  = STATS(13,NL)
      CTSE   = COUNT(13,NL)
      SUMSO  = STATS(14,NL)
      CTSO   = COUNT(14,NL)
      SUM2SE = STATS(15,NL)
      CT2SE  = COUNT(15,NL)
      SUM2SO = STATS(16,NL)
      CT2SO  = COUNT(16,NL)
      SUMVE  = STATS(17,NL)
      CTVE   = COUNT(17,NL)
      SUM2VE = STATS(19,NL)
      CT2VE  = COUNT(19,NL)

   40 CONTINUE

C     ALL DONE WITH SETUP...NOW APPEND A RECORD TO JETVER FILE;            
C     IDENTIFIERS FIRST, THEN DATA .... 

      NA       = IFIX(BLKID(1))
      RMAP     = BLKID(9)
      GCODE    = IFIX(BLKID(10))
      IDATE(1) = NDATE/1000000
      IRMYR    = MOD(NDATE,1000000)
      IDATE(2) = IRMYR/10000
      IRMMN    = MOD(IRMYR,10000)
      IDATE(3) = IRMMN/100
      IDATE(4) = MOD(IRMMN,100)
      WRITE (IUJET)  NDATE , C8AREA, VFILE,
     &            SUMSE , CTSE , SUMSO, CTSO, SUM2SE, CT2SE,
     &            SUM2SO, CT2SO, SUMVE, CTVE, SUM2VE, CT2VE
      IF(DIAG) PRINT 9600, VFILE   , AREA(NA),
     &            IDATE(4), IDATE(2), IDATE(3), IDATE(1)
 9600 FORMAT ('WROTE VERJET RECORD ON UNIT 70 FOR VFILE=', A8,
     &        4X, 'AREA=', A8, 4X, 'VERIFICATION TIME IS  ',
     &        I2, 'Z', 1X, 2(I2,1H/), I4)

      RETURN
      END
