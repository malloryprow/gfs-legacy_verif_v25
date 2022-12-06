                                                                       
      SUBROUTINE NEARPT

C$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
C                .      .    .                                       .  
C SUBPROGRAM:    NEARPT      FIND NEAREST NEIGHBORING STATION           
C   PRGMMR: STACKPOLE        ORG: W/NP12    DATE: 96-05-06
C                                                                       
C ABSTRACT:  THE PRIMARY PURPOSE OF THIS SUBROUTINE IS TO PROVIDE       
C   POINT PAIRS FOR AN S1-TYPE CALCULATION.  ANY OTHER APPLICATION      
C   OF THIS PRODUCT DOES NOT CARRY THE FULL GUARANTEE OF COMPATA-       
C   BILITY OR SUCCESS FROM THE PROGRAMMER.                              
C                                                                       
C PROGRAM HISTORY LOG:                                                  
C   76-??-??  STACKPOLE                                                 
C   89-01-25  VLCEK       CONVERT FROM FORTRAN 66 TO VS FORTRAN 77      
C   96-05-06  Y. ZHANG    MOVE IT FROM HDS TO CRAY AND MODIFY THE
C                         COMMENTS ON SOME VARIABLES.
C   98-07-29  VLCEK       COMPILE IN F90 (NO CODE CHANGES)
C                                                                       
C USAGE:    CALL NAMSTN                                                 
C   INPUT ARGUMENT LIST:  NONE                                          
C                                                                       
C   OUTPUT ARGUMENT LIST: NONE                                          
C                                                                       
C REMARKS: ALL DATA IS TRANSFERRED IN COMMON.  SEE COMMENTS BELOW       
C   FOR 'INPUT' AND 'OUTPUT' VARIABLES.  66 STATEMENTS, 1702 BYTES.     
C                                                                       
C ATTRIBUTES:                                                           
C   LANGUAGE: VS FORTRAN 90    CRAY FORTRAN                                         
C   MACHINE:  CRAY
C                                                                       
C$$$                                                                    
CC                                                                      
      IMPLICIT INTEGER(G)

      PARAMETER (GNVAR=4, GNVARP=(GNVAR+1), GNLEV=4, GMXSTN=1100)

C     IMPORTANT 'INPUT' AND 'OUTPUT' VARIABLES ARE LOCATED IN        
C     THIS COMMON BLOCK.  THEY ARE:                                  

C     IDSTN - BLOCK AND STATION I.D. NUMBER FOR ORIGINAL STN.   
C     ALAT  - LATITUDE OF ORIGINAL STATION (DEGREES).           
C     ALON  - EAST LONGITUDE OF ORIGINAL STATION (DEGREES).     

C     NRPT  - BLOCK AND STATION I.D. OF 'NEIGHBOR' STATION.     
C     DISTNN- DISTANCE (KM) BETWEEN ORIGINAL AND NEIGHBOR STNS. 

      COMMON /ADPSTN/ IDSTN(GMXSTN), ALON(GMXSTN)  , ALAT(GMXSTN),
     &                NRPT(GMXSTN) , DISTNN(GMXSTN),
     &                ZTRUV(GMXSTN , GNVARP,GNLEV) ,
     &                STI(GMXSTN)  , STJ(GMXSTN)   , JSTA
      COMMON /MAPINF/ NSTA, KMAP   , IMXMAP        , JMXMAP,
     &                NCYCMP       , F9            , DATENL

      DIMENSION  DLAMDA(90)

C     LAT - LON EXTENTS IN DEGREES, DISTANCES BETWEEN POINTS IN KM.    

      DPHI  = 15.
      DLAM  = 15.
      DSMIN = 100.
      DSMAX = SQRT(DPHI*DPHI + DLAM*DLAM) *111.12
      PI    = 3.1415926535
      RAD   = PI/180.

C     CALCULATE LONGITUDE LENGTH OF LATITUDE BAND AS FUNCTON           
C     OF COS(LAT)                                                      

      DO 10 JLAT=1,90
      FJLAT        = FLOAT(JLAT-1)
      ALATR        = FJLAT*RAD
      COSPHI       = COS(ALATR)
      DLAMC        = DLAM/COSPHI
      DLAMDA(JLAT) = AMIN1(DLAMC, 180.)
   10 CONTINUE

C     POINT J1 IS PRIMARY POINT, J2 IS SECONDARY POINT OR              
C     THE NEAREST POINT TO J1.                                         

      DO 30 J1=1,JSTA
      DT1T2      = DSMAX
      ALATP1     = ALAT(J1)
      ALONP1     = ALON(J1)
      JLTMIN     = 0
      DISTNN(J1) = 0.
      NRPT(J1)   = 0 

C     START SEARCHING.                                               

C     TO PLAY SEARCH, PLEASE OBSERVE THE FOLLOWING GUIDELINES:       
C     1) AVOID CONGESTION, STAY COOL AND UNBIASED---              
C        KEEP A RESPECTABLE DISTANCE BETWEEN POINT PAIRS.         
C     2) PROHIBIT SELF-INDULGENCE--- NO SELF-PAIRING OF POINTS    
C     3) BE UNIQUE, PREVENT CONFLICTS OF INTEREST--- NO DUAL      
C        PAIRING OF THE SAME SET OF POINTS.                       
C     4) ACCEPT REALITY, SOME POINTS ARE BEYOND HOPE OF PAIRING   
C        ---PUT RESTRICTION ON RADIUS OF SEARCH.                  
C     5) BE PROGRESSIVE, OFFER AN EEO UMBRELLA--- ALL SECONDARY   
C        POINTS ARE TREATED EQUAL W/R TO NATURAL ORIGIN.          

      DO 50 JSERCH=1,2
      IF (JLTMIN.EQ.0 .AND. JSERCH.EQ.2)    GO TO 60
      DO 70 J2=1,JSTA
      ALATP2 = ALAT(J2)

C     CHECK LAT BOUNDARY                                       

      DPHIP2 = ABS(ALATP2 - ALATP1)
      IF (DPHIP2.GT.DPHI)    GO TO 80
      ALONP2 = ALON(J2)
      LATP2  = ABS( ALATP2) + 1
      DLON   = ABS(ALONP2 - ALONP1 )
      IF (DLON.GT.180.)   DLON = 360. - DLON

C     CHECK LON BOUNDARY                                     

      IF(DLON.GT.DLAMDA(LATP2).AND.LATP2.LT.85) GO TO  90
      IF(J2.EQ.J1.OR.J2.EQ.JLTMIN) GO TO 100
      CALL DIST (ALATP1, ALONP1, ALATP2, ALONP2, DP1P2)
      IF(DP1P2.GT.DT1T2) GO TO 110
      IF(DP1P2.GT.DSMIN) GO TO 120

C     POINTS TOO CLOSE                               

      JLTMIN   = J2
      NRPT(J1) = 0
      DT1T2    = DSMAX

C     RECYCLE J2  LOOP                               
C     EXIT TO END OF DO 50 LOOP                      

      GO TO 50

  120 CONTINUE
      IF(JLTMIN.EQ.0) GO TO 200
      IF(NRPT(J2).EQ.JLTMIN.OR.NRPT(JLTMIN).EQ.J2) GO TO 160
  200 CONTINUE

C     CHECK FOR CONDITION 3.                           

      IF (NRPT(J2).EQ.J1) GO TO 160
      DT1T2    = DP1P2
      NRPT(J1) = J2
  160 CONTINUE
  110 CONTINUE
  100 CONTINUE
   90 CONTINUE
   80 CONTINUE
   70 CONTINUE
   60 CONTINUE

C     END SEARCH.                                                  

      IF (NRPT(J1).EQ.0)   GO TO 170

C     NEARBY POINT FOUND, SAVE DISTANCE BETWEEN POINT PAIR FOR   
C     S1 CALCULATION.                                            

      DISTNN(J1) = DT1T2
  170 CONTINUE
   50 CONTINUE
   30 CONTINUE

      RETURN
      END
