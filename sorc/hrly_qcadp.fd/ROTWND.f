                                                                       
      SUBROUTINE ROTWND(UG,VG)

C$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
C                .      .    .                                       .  
C SUBPROGRAM:    ROTWND      ROTATE WINDS FROM GRID TO EARTH COMPS      
C   PRGMMR: STACKPOLE        ORG: W/NP12    DATE: 96-05-06
C                                                                       
C ABSTRACT:  THIS SUBROUTINE ROTATES THE U,V GRID COMPONENTS            
C   TO U,V EARTH-ORIENTED COMPONENTS FOR GRIDS OTHER THAN THE           
C   LAT-LON GRIDS.  ROTATION ANGLE MEASURED AS THE ANGLE                
C   FROM THE 90W LONG TO THE VERTICAL LONG LINE OF THE GRID,            
C   I.E. VERTICAL LONG - 90W LONG.                                      
C                                                                       
C PROGRAM HISTORY LOG:                                                  
C   76-??-??  STACKPOLE                                                 
C   89-01-25  VLCEK       CONVERT FROM FORTRAN 66 TO VS FORTRAN 77      
C                                                                       
C   96-05-06  Y. ZHANG    MODIFY SOME PARAMETERS IN ORDER TO ADD A
C                         GRID TYPE TO BE PROCESSED
C   98-07-29  VLCEK       COMPILE IN F90 (NO CODE CHANGES)
C
C USAGE:    CALL ROTWND(UG,VG)                                          
C   INPUT ARGUMENT LIST:                                                
C     UG       - R*8 ARRAY OF GRID-ORIENTED U-COMPONENT OF WIND         
C     VG       - R*8 ARRAY OF GRID-ORIENTED V-COMPONENT OF WIND         
C                                                                       
C   OUTPUT ARGUMENT LIST:                                               
C     UG       - R*8 ARRAY OF EARTH-ORIENTED U-COMPONENT OF WIND        
C                (NO CHANGE IF GRID WAS ORIGINALLY LAT.-LON.).          
C     VG       - R*8 ARRAY OF EARTH-ORIENTED V-COMPONENT OF WIND        
C                (NO CHANGE IF GRID WAS ORIGINALLY LAT.-LON.).          
C                                                                       
C   OUTPUT FILES:                                                       
C     FT06F001 - ERROR MESSAGE IF GRID TYPE NOT RECOGNIZED              
C                                                                       
C REMARKS: SEE DATA STATEMENT FOR /MAPLST/ ARRAY FOR LIST OF            
C   ACCEPTED MAP TYPES.  ACTUAL GRID ASSOCIATED WITH INPUT              
C   ARGUMENT ARRAYS UG AND VG IS GIVEN BY VARIABLE /KMAP/,              
C   PASSED BY COMMON BLOCK /MAPINF/.   51 STATEMENTS, 1166 BYTES.       
C                                                                       
C ATTRIBUTES:                                                           
C   LANGUAGE: VS FORTRAN 90     CRAY FORTRAN                                        
C   MACHINE:  CRAY
C                                                                       
C$$$                                                                    

      IMPLICIT INTEGER(G)

C     /G/ IS RESERVED FOR CONSTANTS NAMED IN A PARAMETER STATEMENT.  
C     THE COMPLETE LIST AND EXPLANATIONS OF PARAMETER NAMES CAN BE   
C     FOUND IN SUBROUTINE ACCUMU.                                    

      PARAMETER (GMXSTN=1100, GNLEV=4, GNVAR=4, GNVARP=(GNVAR+1) )
      PARAMETER (GNMAPS=16)

      COMMON /ADPSTN/ IDSTN(GMXSTN), ALON(GMXSTN)  , ALAT(GMXSTN),
     &                NRPT(GMXSTN) , DISTNN(GMXSTN),
     &                ZTRUV(GMXSTN , GNVARP,GNLEV) ,
     &                STI(GMXSTN)  , STJ(GMXSTN)   , JSTA
      COMMON /MAPINF/ NSTA, KMAP   , IMXMAP        , JMXMAP,
     &                NCYCMP       , F9            , DATENL

      LOGICAL(1) MAP

      DIMENSION  MAPLST(GNMAPS)
      DIMENSION  ROTANG(GNMAPS), UG(GMXSTN), VG(GMXSTN)

      DATA MAPLST /2,3,5,6,21,22,26,27,28,29,30,33,34,45,46,104/
      DATA ROTANG /0.,0.,15.,15.,0.,0.,15.,-10.,-10.,6*0.,15. /

      DATA PI /3.1415926535/

C     CHECK KMAP AGAINST EXISTING MAP TYPES.                         

      MAP = .FALSE.
      DO 20 KM = 1,GNMAPS
      IF(KMAP.NE.MAPLST(KM)) GO TO 10
      KMAPT = KM
      MAP = .TRUE.
      GO TO 30
   10 CONTINUE
   20 CONTINUE
   30 CONTINUE
      IF(.NOT.MAP) GO TO 250
      IF(KMAPT.EQ.1.OR.KMAPT.EQ.2.OR.KMAPT.EQ.5.OR.
     &   KMAPT.EQ.6.OR.(KMAPT.GT.9.AND.KMAPT.LE.15)) GO TO 230

C     ROTATE WIND FOR NON- LAT-LON GRIDS.                        

      RADPD = PI/180.
      DO 200 JS=1,JSTA
      IF(STI(JS).GE.F9-1.) GO TO 260
      ANG  = (ALON(JS) + ROTANG(KMAPT))* RADPD
      SINA = SIN(ANG)
      COSA = COS(ANG)
      UGS  = UG(JS)
      VGS  = VG(JS)
      IF(KMAP.EQ.28) GO TO 210

C     NORTHERN HEMISPHERE ROTATION.                        

      UE = -UGS*SINA + VGS*COSA
      VE = -UGS*COSA - VGS*SINA
      GO TO 220
  210 CONTINUE

C     SOUTHERN HEMISPHERE ROTATION.                        

      UE = -UGS*SINA - VGS*COSA
      VE =  UGS*COSA - VGS*SINA
  220 CONTINUE
      UG(JS) = UE
      VG(JS) = VE
  260 CONTINUE
  200 CONTINUE
  230 CONTINUE

C     LAT-LON GRID EXIT.                                           

      RETURN

C     MAP TYPE NOT MATCHED.                                          

  250 CONTINUE
      PRINT 100, KMAP
  100 FORMAT  ('MAP TYPE NOT MATCHED IN SUB ROTWND FOR KMAP=',I5)
      CALL EREXIT(6)
      RETURN
      END
