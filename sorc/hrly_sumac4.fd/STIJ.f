
      SUBROUTINE STIJ
C$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
C                .      .    .                                       .  
C SUBPROGRAM:    STIJ        COMPUTE I AND J OF STATION FOR GRID        
C   PRGMMR: VLCEK            ORG: W/NP12     DATE: 1999-06-07 
C                                                                       
C ABSTRACT:   COMPUTES I & J OF STATION FOR APPROPRIATE GRID.           
C                                                                       
C PROGRAM HISTORY LOG:                                                  
C   76-??-??  STACKPOLE                                                 
C   89-01-25  VLCEK       CONVERT FROM FORTRAN 66 TO VS FORTRAN 77      
C   96-06-25  Y. ZHANG    MOVE IT TO CRAY AND ADD GRID TYPE(2,3,6,
C                         45,104) OF FORECAST FIELD TO BE VERIFIED.
C                         REMOVED ORIGIONAL GRID TYPE 45.
C   96-09-10  Y. ZHANG    ADD GRID TYPE 254 FOR ECMWF's WIND FORECASTS.
C   98-07-28  VLCEK       COMPILE IN FORTRAN 90.                        
C   99-06-07  VLCEK       COMPILE ON IBM RS6000.
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
C          104 STATEMENTS.
C                                                                       
C ATTRIBUTES:                                                           
C   LANGUAGE: VS FORTRAN 90   IBM FORTRAN 
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
      PARAMETER  (GMXARA=15,GMXRUN=15)

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

      REAL         MINWND, MAXWND, ALOLA
      INTEGER      VDATE1, VDATE2, VDATE , RPT , STNLST, NFILE
      INTEGER      GCODE, INDEX(GNMAPS)
      CHARACTER(8) FORFIL, AREA
      CHARACTER(4) FHR   , KRUN
      LOGICAL      PRSTAT, WRSTAT, DIAG, MAP

      DIMENSION    MAPLST(GNMAPS), POLEI(GNMAPS) , POLEJ(GNMAPS) ,
     &             DIJ(GNMAPS)   , MAXI(GNMAPS)  , MAXJ(GNMAPS)  ,
     &             NCYCLE(GNMAPS), ORIENT(GNMAPS)


      DATA INDEX  /1,1,2,2,3,4,2,2,2,3,4,3,4,1,4,2,5/ 
      DATA MAPLST /2,3,5,6,21,22,26,27,28,29,30,33,34,45,46,104,254/
      DATA POLEI /0.,0.,27.,27.,0.,0.,27.,33.,33.,4*0.,0.,0.,75.5,0./
      DATA POLEJ /0.,0.,49.,49.,0.,0.,49.,33.,33.,4*0.,0.,0.,109.5,0./
      DATA DIJ    /2.5,1.,190.5,190.5,5.,5.,190.5,381.,381.,2.5,2.5,
     &             2.,2.,1.25,3.75,90.755,2.5/
      DATA MAXI   /144,360,53,53,73,73,53,65,65,145,145,181,181,
     &             288, 97,147,144/
      DATA MAXJ   /73,181,57,45,19,19,45,65,65,37,37,46,46,
     &             145,25,110,29/
      DATA NCYCLE /1,1,0,0,1,1,0,0,0,6*1,0,1/
      DATA ORIENT /0.,0.,105.,105.,2*0.,105.,2*80.,6*0.,105.,0./


C     SUBROUTINE STIJ DETERMINES THE I, J MAP COORDINATES W/R          
C     TO THE ANALYSES OR FORECASTS AT THE STATION LOCATION.            
C                                                                       
C     RECIPE FOR ADDING NEW MAPS TO BE VERIFIED                  
C                                                                       
C     1.  CHANGE VALUE OF PARAMETER CONSTANT /GNMAPS/                
C     2.  PLACE APPROPRIATE PARAMETERS IN FOLLOWING 9 VARIABLES      
C         (ASSIGN CASE GROUP IN INDEX ARRAY)
C     3.  PLACE DITTO IN MEMBER ROTWND (2 VARIABLES)                 
C     4.  CHANGE DATE IN MEMBER ACCUMVER                             
C     5.  CHANGE GOTO(...) AT FTN STMT 160 + 1                       
C     6.  DITTO IN ROTWND, STMT 30 + 2                               

      MAP = .FALSE.

C     CHECK KMAP AGAINST EXISTING MAP TYPES.                         

      DO 20 KM=1,GNMAPS
        IF(KMAP.EQ.MAPLST(KM)) THEN
          KMAPT = KM
          MAP = .TRUE.
          GO TO 30
        ENDIF
   20 CONTINUE
   30 CONTINUE

      IF(MAP) THEN
        NCYCMP = NCYCLE(KMAPT)
        IMXMAP = MAXI(KMAPT)
        JMXMAP = MAXJ(KMAPT)

C       INITIALIZE STI, STJ MISSING(F9).                             

        STI = F9
        STJ = F9
C       GO TO (200,200,40,40,50,120,40,40,40,50,120,50,120,
C    &         200,120,40,220), KMAPT
        IMAPT = INDEX(KMAPT)
C 
        IF (DIAG)    PRINT 140, KMAP, IMAPT, NCYCMP, IMXMAP, JMXMAP
  140   FORMAT ('  KMAP = ', I3, '  CASE = ', I1, '  NCYCMP = ', I2,
     &          '  IMXMAP = ', I3, '  JMXMAP = ', I3)

        SELECTCASE(IMAPT)

C         POLAR STEREOGRAPHIC GRIDS DONE HERE.                         

C  40     CONTINUE
          CASE(2)
            DO 60 JS = 1,JSTA
              STLAT  = ALAT(JS)
              STLONW = 360. - ALON(JS)

C             NORTHERN HEMISPHERE                                        

              IF(KMAP.NE.28) THEN
                IF(STLAT.GE.0.) THEN
                  CALL W3FB04 (STLAT, STLONW, DIJ(KMAPT),
     &                         ORIENT(KMAPT), XI, XJ)
                ELSE

C             SOUTHERN HEMISPHERE                                        

                  CALL W3FB02 ( STLAT, STLONW, DIJ(KMAPT), XI, XJ )
                ENDIF

                STI(JS) = XI + POLEI(KMAPT)
                STJ(JS) = XJ + POLEJ(KMAPT)
                IF(STI(JS).LT.1..OR.STI(JS).GT.FLOAT(IMXMAP)) 
     &            STI(JS) = F9
                IF(STJ(JS).LT.1..OR.STJ(JS).GT.FLOAT(JMXMAP)) 
     &            STJ(JS) = F9
                IF(STI(JS).GT.F9-1.) STJ(JS) = F9
                IF(STJ(JS).GT.F9-1.) STI(JS) = F9
              ENDIF
   60       CONTINUE
C           GO TO 90

C         LATITUDE-LONGITUDE GRIDS DONE HERE.                          

C         NORTHERN HEMISPHERE                                          

C  50     CONTINUE
          CASE(3)
            DO JS =1,JSTA
              IF( ALAT(JS).GE.0. ) THEN
                STI(JS) = ALON(JS)/DIJ(KMAPT) + 1.
                STJ(JS) = ALAT(JS)/DIJ(KMAPT) + 1.
              ENDIF
            ENDDO
C           GO TO 90

C         SOUTHERN HEMISPHERE                                          

C 120     CONTINUE
          CASE(4)
            DO JS=1,JSTA
              IF(ALAT(JS).LT.0.) THEN
                STI(JS) = ALON(JS)/DIJ(KMAPT) + 1.
                STJ(JS) = (ALAT(JS) + 90.)/DIJ(KMAPT) + 1.
              ENDIF
            ENDDO
C           GO TO 90

C         BOTH  HEMISPHERES

C 200     CONTINUE
          CASE(1)
            DO JS=1,JSTA
              STI(JS) = ALON(JS)/DIJ(KMAPT) + 1.
              STJ(JS) = (90.-ALAT(JS))/DIJ(KMAPT) + 1.
            ENDDO
C           GO TO 90

C         BETWEEN 35N---35S

C 220     CONTINUE
          CASE(5)
            DO JS=1,JSTA
              IF(ABS(ALAT(JS)).LE.35.) THEN     
                STI(JS) = ALON(JS)/DIJ(KMAPT) + 1.
                STJ(JS) = (35.-ALAT(JS))/DIJ(KMAPT) + 1.
              ENDIF
            ENDDO

        END SELECT
C  90   CONTINUE
        RETURN
      ENDIF

C     MAP TYPE MATCH NOT FOUND                                       

      PRINT 1000, KMAP
 1000 FORMAT ('MAP TYPE NOT MATCHED IN SUB STIJ FOR KMAP=',I5 )
      CALL EREXIT(7)
      RETURN
      END
