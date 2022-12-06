                                                                       
      SUBROUTINE STIJ

C$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
C                .      .    .                                       .  
C SUBPROGRAM:    STIJ        COMPUTE I AND J OF STATION FOR GRID        
C   PRGMMR: STACKPOLE        ORG: W/NP12    DATE: 96-05-06
C                                                                       
C ABSTRACT:   COMPUTES I & J OF STATION FOR APPROPRIATE GRID.           
C                                                                       
C PROGRAM HISTORY LOG:                                                  
C   76-??-??  STACKPOLE                                                 
C   89-01-25  VLCEK       CONVERT FROM FORTRAN 66 TO VS FORTRAN 77      
C   96-05-06  Y. ZHANG    ADD GRID TYPE 2,3,6,45,104 AND SOME STATEMENTS
C                         TO PROCESS BOTH HEMISPHERE FIELDS AND REFINE IT.
C   98-07-29  VLCEK       COMPILE IN F90 (NO CODE CHANGES).
C                         
C                                                                       
C USAGE:    CALL STIJ                                                   
C   INPUT ARGUMENT LIST:  NONE                                          
C                                                                       
C   OUTPUT ARGUMENT LIST: NONE                                          
C                                                                       
C   OUTPUT FILES:                                                       
C     FT06F001 - ERROR MESSAGE IF MAP TYPE NOT RECOGNIZED               
C                                                                       
C REMARKS: SEE DATA STATEMENT FOR /MAPLST/ FOR ACCEPTED MAP TYPES.      
C          104 STATEMENTS, 2254 BYTES.                                  
C                                                                       
C ATTRIBUTES:                                                           
C   LANGUAGE: VS FORTRAN 90    CRAY FORTRAN                                         
C   MACHINE:  CRAY
C                                                                       
C$$$                                                                    

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
      PARAMETER  (GNMAPS=16)

      COMMON /RAFILE/ DATA(GMXDIM) , DATA1(GMXDIM) , IGRID,
     &                XS(4), RPT, ANLD, ANLI
      COMMON /ADPSTN/ IDSTN(GMXSTN), ALON(GMXSTN)  , ALAT(GMXSTN),
     &                NRPT(GMXSTN) , DISTNN(GMXSTN),
     &                ZTRUV(GMXSTN , GNVARP,GNLEV) ,
     &                STI(GMXSTN)  , STJ(GMXSTN)   , JSTA
      COMMON /MAPINF/ NSTA, KMAP   , IMXMAP        , JMXMAP,
     &                NCYCMP       , F9            , DATENL
      COMMON /PARMIN/ LPRDAT, INTPM, INDXA

      CHARACTER(9)  ANLD, ANLI
      CHARACTER(1)  INDXA
      INTEGER       IGRID, RPT
      LOGICAL(1)    MAP
      LOGICAL       LPRDAT

      DIMENSION    MAPLST(GNMAPS), POLEI(GNMAPS), POLEJ(GNMAPS),
     &             DIJ(GNMAPS)   , MAXI(GNMAPS) , MAXJ(GNMAPS) ,
     &             NCYCLE(GNMAPS), ORIENT(GNMAPS)

      DATA MAPLST /2,3,5,6,21,22,26,27,28,29,30,33,34,45,46,104/
      DATA POLEI /0.,0.,27.,27.,0.,0.,27.,33.,33.,4*0.,0.,0.,75.5 /
      DATA POLEJ /0.,0.,49.,49.,0.,0.,49.,33.,33.,4*0.,0.,0.,109.5 /
      DATA DIJ    /2.5,1.,190.5,190.5,5.,5.,190.5,381.,381.,2.5,2.5,
     &             2.,2.,1.25,3.75,90.755 /
      DATA MAXI   /144,360,53,53,73,73,53,65,65,145,145,181,181,
     &             288, 97,147/
      DATA MAXJ   /73,181,57,45,19,19,45,65,65,37,37,46,46,
     &             145,25,110/
      DATA NCYCLE /1,1,0,0,1,1,0,0,0,6*1,0/
      DATA ORIENT /0.,0.,105.,105.,2*0.,105.,2*80.,6*0.,105./

C     SUBROUTINE STIJ DETERMINES THE I, J MAP COORDINATES W/R          
C     TO THE ANALYSES OR FORECASTS AT THE STATION LOCATION.            

C     RECIPE FOR ADDING NEW MAPS TO BE VERIFIED                  
C                                                                       
C     1.  CHANGE VALUE OF PARAMETER CONSTANT /GNMAPS/                
C     2.  PLACE APPROPRIATE PARAMETERS IN FOLLOWING 8 VARIABLES      
C     3.  PLACE DITTO IN MEMBER ROTWND (2 VARIABLES)                 
C     4.  CHANGE DATE IN MEMBER ACCUMVER                             
C     5.  CHANGE GOTO(...) AT FTN STMT 160 + 1                       
C     6.  DITTO IN ROTWND, STMT 30 + 2                               

      MAP = .FALSE.

C     CHECK KMAP AGAINST EXISTING MAP TYPES.                         

      DO 20 KM=1,GNMAPS
      IF(KMAP.NE.MAPLST(KM)) GO TO 10
      KMAPT = KM
      MAP = .TRUE.
      GO TO 30
   10 CONTINUE
   20 CONTINUE
   30 CONTINUE
      IF(.NOT.MAP) GO TO 70
      NCYCMP = NCYCLE(KMAPT)
      IMXMAP = MAXI(KMAPT)
      JMXMAP = MAXJ(KMAPT)

C     INITIALIZE STI, STJ MISSING(F9).                             

      DO 160 JS=1,JSTA
      STI(JS) = F9
      STJ(JS) = F9
  160 CONTINUE
      GO TO (200,200,40,40,50,120,40,40,40,50,120,50,120,
     &       200,120,40), KMAPT

C     POLAR STEREOGRAPHIC GRIDS DONE HERE.                         

   40 CONTINUE
      DO 60 JS = 1,JSTA
      STLAT  = ALAT(JS)
      STLONW = 360. - ALON(JS)
      IF(KMAP.EQ.28) GO TO 100

C     NORTHERN HEMISPHERE                                        

      IF(STLAT.LT.0.) GO TO 170
      CALL W3FB04 (STLAT, STLONW, DIJ(KMAPT),
     &             ORIENT(KMAPT), XI, XJ)
      GO TO 110
  100 CONTINUE

C     SOUTHERN HEMISPHERE                                        

      IF(STLAT.GT.0.) GO TO 170
      CALL W3FB02 ( STLAT, STLONW, DIJ(KMAPT), XI, XJ )
  110 CONTINUE
      STI(JS) = XI + POLEI(KMAPT)
      STJ(JS) = XJ + POLEJ(KMAPT)
      IF(STI(JS).LT.1..OR.STI(JS).GT.FLOAT(IMXMAP)) STI(JS) = F9
      IF(STJ(JS).LT.1..OR.STJ(JS).GT.FLOAT(JMXMAP)) STJ(JS) = F9
      IF(STI(JS).GE.F9-1.) STJ(JS) = F9
      IF(STJ(JS).GE.F9-1.) STI(JS) = F9
  170 CONTINUE
   60 CONTINUE
      GO TO 90

C     LATITUDE-LONGITUDE GRIDS DONE HERE.                          

   50 CONTINUE

C     NORTHERN HEMISPHERE                                          

      DO 80 JS =1,JSTA
      IF( ALAT(JS).LT.0. ) GO TO 130
      STI(JS) = ALON(JS)/DIJ(KMAPT) + 1.
      STJ(JS) = ALAT(JS)/DIJ(KMAPT) + 1.
  130 CONTINUE
   80 CONTINUE
      GO TO 90

C     SOUTHERN HEMISPHERE                                          

  120 CONTINUE
      DO 140 JS=1,JSTA
      IF(ALAT(JS).GT.0.) GO TO 150
      STI(JS) = ALON(JS)/DIJ(KMAPT) + 1.
      STJ(JS) = (ALAT(JS) + 90.)/DIJ(KMAPT) + 1.
  150 CONTINUE
  140 CONTINUE
      GO TO 90

C     BOTH  HEMISPHERES                                          

  200 CONTINUE
      DO 210 JS=1,JSTA
      IF(INDXA.EQ.'N'.AND.ALAT(JS).LT.0.) GO TO 209
      IF(INDXA.EQ.'S'.AND.ALAT(JS).GE.0.) GO TO 209
      STI(JS) = ALON(JS)/DIJ(KMAPT) + 1.
      STJ(JS) = (90.-ALAT(JS))/DIJ(KMAPT) + 1.
  209 CONTINUE
  210 CONTINUE
   90 CONTINUE
      RETURN

C     MAP TYPE MATCH NOT FOUND                                       

   70 CONTINUE
      PRINT 1000, KMAP
 1000 FORMAT ('MAP TYPE NOT MATCHED IN SUB STIJ FOR KMAP=',I5 )
      CALL EREXIT(7)
      RETURN
      END
