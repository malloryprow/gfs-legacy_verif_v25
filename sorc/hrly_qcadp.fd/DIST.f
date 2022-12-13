
      SUBROUTINE DIST (XLAT1, XLON1, XLAT2, XLON2, DKM)

C$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
C                .      .    .                                       .  
C SUBPROGRAM:    DIST        COMPUTE GREAT CIRCLE DISTANCE              
C   PRGMMR: STACKPOLE        ORG: W/NP12    DATE: 89-01-25             
C                                                                       
C ABSTRACT: CALCULATES SHORTEST GREAT CIRCLE DISTANCE (IN KM)           
C   BETWEEN TWO POINTS ON EARTH GIVEN THE LATITUDE AND LONGITUDE        
C   OF EACH.                                                            
C                                                                       
C PROGRAM HISTORY LOG:                                                  
C   76-??-??  STACKPOLE                                                 
C   89-01-25  VLCEK      COMPILE IN F77  (NO CODE CHANGE)
C   96-05-06  ZHANG      COMPILE ON CRAY (NO CODE CHANGE)
C   98-07-29  VLCEK      COMPILE IN F90  (NO CODE CHANGE)
C                                                                       
C USAGE:    CALL DIST(XLAT1, XLON1, XLAT2, XLON2, DKM)                  
C   INPUT ARGUMENT LIST:                                                
C     XLAT1    - LATITUDE OF FIRST POINT (DEGREES).                     
C     XLON1    - WEST LONGITUDE OF FIRST POINT (DEGREES).               
C     XLAT2    - LATITUDE OF SECOND POINT (DEGREES).                    
C     XLON2    - WEST LONGITUDE OF SECOND POINT (DEGREES).              
C                                                                       
C   OUTPUT ARGUMENT LIST:                                               
C     DKM      - DISTANCE BETWEEN TWO POINTS, IN KM.                    
C                                                                       
C REMARKS: CONVERSIONS FROM FORTRAN 66 TO F77 TO CRAY TO F90
C          DID NOT REQUIRE ANY CODING CHANGES.  THIS SUBROUTINE         
C          USES 12 STATEMENTS, 618 BYTES.                               
C                                                                       
C ATTRIBUTES:                                                           
C   LANGUAGE: VS FORTRAN 90    CRAY FORTRAN                                           
C   MACHINE:  CRAY
C                                                                       
C$$$                                                                    

C     CALCULATE DISTANCE IN KM, BETWEEN TWO POINTS ON A SPHERE.

      DATA XKMDEG / 111.12 /

      PI   = 3.1415926535
      RAD  = PI/180.
      PHI1 = XLAT1*RAD
      PHI2 = XLAT2*RAD
      DLAMDA = ABS (XLON2 - XLON1)

C     ALWAYS USE SHORTEST GREAT CIRCLE DISTANCE BETWEEN THE    
C     TWO POINTS.                                              

      IF (DLAMDA.GT.180.)    DLAMDA = 360. - DLAMDA
      DLAMDA = DLAMDA*RAD
      CD  = SIN(PHI1)*SIN(PHI2) + COS(PHI1)*COS(PHI2)*COS(DLAMDA)
      DKM = ACOS(CD)*XKMDEG/RAD
      RETURN
      END
