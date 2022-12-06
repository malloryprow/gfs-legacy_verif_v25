
      SUBROUTINE STDEV(VECTOR,SN,SUM,SUMSQ,NV,K,VSUM,VSUMSQ)

C$$$  SUBPROGRAM  DOCUMENTATION  BLOCK                                  
C                .      .    .                                       .  
C SUBPROGRAM:    STDEV       COMPUTES RMS DIFFERENCE FOR TOSSOUTS       
C   PRGMMR: STACKPOLE        ORG: W/NP12    DATE: 96-05-06
C                                                                       
C ABSTRACT: CALCULATES RMS DIFFERENCES OF ANALYSIS AND OBSERVATIONS     
C   FOR USE AS TOSS OUT CRITERIA IN ADP QUALITY CHECK.                  
C                                                                       
C PROGRAM HISTORY LOG:                                                  
C   76-??-??  STACKPOLE                                                 
C   85-10-10  VLCEK       DO RMS OF U AND V RATHER THAN SPEED           
C   89-01-25  VLCEK       CONVERT FROM FORTRAN 66 TO VS FORTRAN 77      
C   96-05-06  Y. ZHANG    MOVE IT TO CRAY AND REFINE ITS PARAMETERS.
C   98-07-29  VLCEK       COMPILE IN F90 (NO CODE CHANGES)
C                                                                       
C USAGE:  CALL STDEV(VECTOR,SN,SUM,SUMSQ,NV,K,VSUM,VSUMSQ)              
C   INPUT ARGUMENT LIST:                                                
C     VECTOR   - L*1 VARIABLE; IF TRUE, REST OF ARG LIST IS             
C                IDENTIFIED AS DEALING WITH U AND V COMPONENTS.         
C     SN       - R*8 COUNTER USED IN RMS DIFF. CALCULATIONS.            
C     SUM      - R*8 SUM OF (ANL-OBS) DIFFERENCES.                      
C     SUMSQ    - R*8 SUM OF SQUARED (ANL-OBS) DIFFERENCES.              
C     NV       - I*8 VARIABLE INDICATING METEOROLOGICAL QUANTITY        
C                REPRESENTED BY SUM AND SUMSQ: 1=HEIGHT, 2=TEMP.,       
C                3=R.H., 4=WIND (U AND V COMPS)                         
C     K        - I*8 VARIABLE INDICATING THE PRESSURE LEVEL             
C     VSUM     - R*8 SUM OF (ANL-OBS) V-COMP DIFFERENCES, IF            
C                VECTOR=T (/SUM/ HOLDS U-COMP DIFFERENCES).             
C     VSUMSQ   - R*8 SUM OF SQUARES OF (ANL-OBS) V-COMP DIFFERENCES,    
C                IF VECTOR=T (U-COMP REPRESENTED BY /SUMSQ/).           
C                                                                       
C   OUTPUT ARGUMENT LIST:                                               
C     SN       - SEE INPUT ARGUMENT LIST                                
C     SUM      - SEE INPUT ARGUMENT LIST                                
C     SUMSQ    - SEE INPUT ARGUMENT LIST                                
C     VSUM     - SEE INPUT ARGUMENT LIST                                
C     VSUMSQ   - SEE INPUT ARGUMENT LIST                                
C                                                                       
C REMARKS: 69 STATEMENTS, 1294 BYTES.                                   
C                                                                       
C ATTRIBUTES:                                                           
C   LANGUAGE: VS FORTRAN 90    CRAY FORTRAN                                         
C   MACHINE:  CRAY
C                                                                       
C$$$                                                                    
C                                                                       
C - - - - - - O T H E R   I N P U T   V A R I A B L E S  - - - - - -    
C                                                                       
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE   
C     -----       ----------------------------------        ---------   
C                                                                       
C     F9          REAL * 8 CONSTANT DENOTING MISSING        /MAPINF/    
C                 DATA. F9=99999.                                       
C                                                                       
C     STVL        REAL * 8 ARRAY HOLDING A NON-WIND         /LOCAL/     
C                 GRID POINT VALUE OF THE ANALYSIS                      
C                 OR FORECAST FIELD WHICH HAS BEEN                      
C                 INTERPOLATED TO THE OBSERVATION POINT                 
C                                                                       
C     STV1        REAL * 8 ARRAY HOLDING A WIND GRIDPOINT   /LOCAL/     
C                 VALUE OF THE ANALYSIS OR FORECAST                     
C                 FIELD WHICH HAS BEEN INTERPOLATED TO                  
C                 THE OBSERVBATION POINT                                
C                                                                       
C     ZTRUV       REAL * 8 ARRAY WHICH HOLDS THE SCALED     /ADPSTN/    
C                 DATA                                                  
C                                                                       
C                                                                       
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

      IMPLICIT  INTEGER(G)

C     /G/ IS RESERVED FOR CONSTANTS NAMED IN A PARAMETER STATEMENT.   
C     LIST AND EXPLANATIONS OF PARAMETER NAMES CAN BE FOUND IN        
C     SUBROUTINE ACCUMU.                                              

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

      COMMON /ADPSTN/ IDSTN(GMXSTN), ALON(GMXSTN)  , ALAT(GMXSTN),
     &                NRPT(GMXSTN) , DISTNN(GMXSTN),
     &                ZTRUV(GMXSTN , GNVARP,GNLEV) ,
     &                STI(GMXSTN)  , STJ(GMXSTN)   , JSTA
      COMMON /MAPINF/ NSTA, KMAP   , IMXMAP        , JMXMAP,
     &                NCYCMP       , F9            , DATENL
      COMMON /RAFILE/ DATA(GMXDIM) , DATA1(GMXDIM) , IGRID,
     &                XS(4), RPT, ANLD, ANLI
      COMMON /LOCAL / STVL(GMXSTN) , STV1(GMXSTN)
      COMMON /WIND  / FU(GMXSTN)   , FV(GMXSTN)
      COMMON /PARMIN/ LPRDAT, INTPM, INDXA

      INTEGER       IGRID, RPT
      CHARACTER(9)  ANLD, ANLI
      CHARACTER(1)  INDXA
      LOGICAL       VECTOR, LPRDAT


      IF(VECTOR) GO TO 30
      DO 20 JS=1,JSTA
      IF(STI(JS).GE.F9-1.) GO TO 10
      STNOBS = ZTRUV(JS,NV,K)
      IF(STNOBS.GE.F9-1.) GO TO 10
      STNDIF = STNOBS - STVL(JS)
      SUM    = SUM + STNDIF
      SUMSQ  = SUMSQ + STNDIF*STNDIF
      SN     = SN + 1.
   10 CONTINUE
   20 CONTINUE
      GO TO 60
   30 CONTINUE

C     DO WINDS                                                       

      DO 50 JS=1,JSTA
      IF(STI(JS).GE.F9-1.) GO TO 40
      IF(STVL(JS).GE.F9-1.) GO TO 40
      IF(STV1(JS).GE.F9-1.) GO TO 40
      UO = ZTRUV(JS,NV,K)
      VO = ZTRUV(JS,NV+1,K)
      IF(UO.GE.F9-1..OR.VO.GE.F9-1.) GO TO 40
C     SUM    = SUM + SPDIF                                       
C     SPDOBS = SQRT(UO*UO + VO*VO)                               
C     SPDANL = SQRT(STVL(JS)**2+STV1(JS)**2)                     
C     SPDIF  = SPDOBS - SPDANL                                   
      UDIF   = UO  - FU(JS)
      VDIF   = VO  - FV(JS)
      SUM    = SUM + UDIF
      SUMSQ  = SUMSQ  + UDIF*UDIF
      VSUM   = VSUM   + VDIF
      VSUMSQ = VSUMSQ + VDIF*VDIF
      SN     = SN + 1.
   40 CONTINUE
   50 CONTINUE

   60 CONTINUE
      RETURN
      END
