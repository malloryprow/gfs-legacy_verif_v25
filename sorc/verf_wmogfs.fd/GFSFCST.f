C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C
C MAIN PROGRAM: GFSFCST
C   PRGMMR: LILLY            ORG: NP12        DATE: 2009-03-09
C
C ABSTRACT: This program generates the means of SUMAC4
C           TIME-SERIES data selected for International
C           distribution.  This code is set to do 24hr
C           interval forecasts ie; 24hr, 48hr, 72hr,
C           96hr, 120hr, 144hr, 168hr, 192hr, 216hr and
C           240hr.  This version prints stats in WMO-
C           approved format.
C
C PROGRAM HISTORY LOG:
C   2009-03-09  Original Author: Steve Lilly
C   2009-10-01  Lilly - Converts on to P6
C
C USAGE:
C   INPUT FILES:
C     FT10F001 - Selected Time-Series data
C
C   OUTPUT FILES:
C     FT60F001 - Printed stats from the GFS model forecasts
C                in WMO-approved format.
C
C   SUBPROGRAMS CALLED:
C
C   EXIT STATES:
C     COND =   0 - SUCCESSFUL RUN
C
C REMARKS:
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE : IBM RS6000
C
C$$$

C     PROGRAM  GFSFCST                                            
                                                                                
C     7-number of models  10-FHR (24,48,72,96,120,144,168,192,216,240)

      REAL(4)  MEAN00(7,3,3,20), RMS00Z(7,3,3,20)                                 
      REAL(4)  MEAN12(7,3,3,20), RMS12Z(7,3,3,20), DATA(500)                      

      INTEGER(4)  P(3), DATES(2), FDATE(4), LDATE(4), IVL, I, J 

      CHARACTER(9) MONTH(12)                      
      CHARACTER(8) AREA, KAREA(7), A1(7), A2(7)                      
      CHARACTER(4) MODEL, HOUR, FHR(20)                                      
      CHARACTER(3) SEL3, SELX, MDL3, REG, RAF, LRG, GFS, LRGN                            

      LOGICAL(1) REGL                                                            
      LOGICAL(1) LRGL                                                            
                                                                                
      EQUIVALENCE (MODEL,MDL3)                                                  
                                                                                
      DATA  P /850,500,250/
      DATA  REG /'REG'/, RAF /'RAF'/, LRG /'LRG'/, GFS /'GFS'/                               
      DATA  LRGN /'LRG'/
      DATA  MEAN00 /1260*9999./, RMS00Z /1260*9999./                              
      DATA  MEAN12 /1260*9999./, RMS12Z /1260*9999./                              
      DATA  DATA /500*999.000/                  

      DATA  A1 /'NORTH AM', 'EUROPE  ', 'ASIA    ', 'AUSTRALI',                 
     &          'TROPICS ', 'NORTH HE', 'SOUTH HE'/                             

      DATA  A2 /'ERICA   ', '        ', '        ', 'A/ NEW Z',                 
     &          '        ', 'MISPHERE', 'MISPHERE'/                             

      DATA  KAREA  /'NA110   ', 'EUR96   ', 'ASIA153 ', 'AUS24   ',             
     &              'TROPICS ', 'NHEM4WMO', 'SH31    '/                         

      DATA  MONTH  /'JANUARY  ', 'FEBRUARY ', 'MARCH    ', 'APRIL    ',          
     &              'MAY      ', 'JUNE     ', 'JULY     ', 'AUGUST   ',          
     &              'SEPTEMBER', 'OCTOBER  ', 'NOVEMBER ', 'DECEMBER '/          

      DATA  FHR /'F012', 'F024', 'F036', 'F048', 'F060', 'F072',
     &         'F084', 'F096', 'F108', 'F120', 'F132', 'F144',
     &         'F156', 'F168', 'F180', 'F192', 'F204', 'F216',
     &         'F228', 'F240'/
                                                                                
  100   FORMAT (1X,A3,1X,A3)                                                    
  105   FORMAT (//'0#', 25X, '** NCEP/USA  GFS MODEL **' )
  106   FORMAT (//'0#', 25X, '** NCEP/USA  ', A3, ' MODEL **' )
  107   FORMAT (//'0#', 25X, '** NCEP/USA           MODEL **' )

  110   FORMAT (//'0', '  TABLE', F7.1, 2X, 2A8, '  VERIFICATION',
     &          ' AGAINST RADIOSONDES', /' ', 16X, 50('-') )
 
  121   FORMAT (' ', 16X, I3, ' HPA    GEOPOTENTIAL HEIGHT', 5X, A9,
     &          I5, /' ', 16X, 48('-') )

  122   FORMAT (' ', 16X, I3, ' HPA    TEMPERATURE        ', 5X, A9,
     &          I5, /' ', 16X, 48('-') )

  123   FORMAT (' ', 16X, I3, ' HPA    WIND               ', 5X, A9,
     &          I5, /' ', 16X, 48('-') )
 

  131   FORMAT (//'0', ' FORECAST', 6X, 'MEAN ERROR', 11X, 'RMSE',
     &          /' ',  '  PERIOD', 11X, '(M)',
     &          15X, '(M)', /' ', 1X, '(HOURS)', 3X,
     &          2('  00 GMT  12 GMT '), /' ',  ' --------', 2X,
     &          2('  ------  ------ '), /)

  132   FORMAT (//'0',  ' FORECAST', 2X, 'MEAN SPEED ERROR', 9X,
     &          'RMSEV', /' ',  '  PERIOD', 10X, '(M/S)',
     &          13X, '(M/S)',  /' ', 1X, '(HOURS)', 3X,
     &          2('  00 GMT  12 GMT '), /' ', ' --------', 2X,
     &          2('  ------  ------ '), /)

  138   FORMAT (//'0', ' FORECAST', 6X, 'MEAN ERROR', 11X, 'RMSE',
     &          /' ',  '  PERIOD', 11X, '(K)',
     &          15X, '(K)', /' ', 1X, '(HOURS)', 3X,
     &          2('  00 GMT  12 GMT '), /' ',  ' --------', 2X,
     &          2('  ------  ------ '), /)

  140   FORMAT (' ', 1X, I5, 4X, 2(2X, F7.2, 1X, F7.2) )


C       READ MODEL SELECTOR CARD (2 SELECTIONS POSSIBLE; OPL MODEL              
C       IS 'LRG' FOR NON-TROPICAL AREAS AND 'LRGX' FOR TROPICS)                  

        READ 100, SEL3, SELX                                                    

        REGL = .FALSE.                                                          

        LRGL = .FALSE.

          IF (SEL3.EQ.REG.OR.SEL3.EQ.RAF)    REGL = .TRUE.                        

          IF (SEL3.EQ.LRGN.OR.SEL3.EQ.GFS)    LRGL = .TRUE.                        

       IF  (LRGL)  THEN
         IFCST=20
         ITX=12
         INC=2
         IB=2
       ENDIF

       IF  (REGL)  THEN
         IFCST=7
         ITX=12
         INC=1
         IB=1
       ENDIF

                                                                                
C       READ AND IDENTIFY TIME-SERIES RECORDS, SELECT DESIRED MODEL.            
C       WILL ONLY SELECT FROM DAILY STATS (MARKED BY IVL=1)
                                                                                
    1   CONTINUE                                                                

        READ (10,ERR=99,END=10)  MODEL,HOUR,AREA,IVL,DATES,I,J,DATA           

         IF (MDL3.NE.SEL3.AND.MDL3.NE.SELX)    GO TO 1                         

          IF (REGL.AND.AREA.NE.KAREA(1))        GO TO 1                         

           CALL DATFIX(DATES(1),FDATE(1),FDATE(2),FDATE(3),FDATE(4),1)           
           CALL DATFIX(DATES(2),LDATE(1),LDATE(2),LDATE(3),LDATE(4),1)           

           IF (IVL.NE.1)                            GO TO 1                      

            DO 2 K=1,7                                                            

             IF (AREA.EQ.KAREA(K))                  GO TO 3                      

    2       CONTINUE                                                              

           GO TO 1                                                               

    3    CONTINUE                                                              

            DO 4 IHR=IB,IFCST,INC                                                         

             IF (HOUR.EQ.FHR(IHR))                  GO TO 5                      

    4       CONTINUE                                                              

         GO TO 1                                                               

    5    CONTINUE                                                              

         IF (I.NE.2.AND.I.NE.4)                   GO TO 1                      
          LEV = (J-1)/5 + 1                                                     

         IF (LEV.GT.3)                            GO TO 1                      
          IQ = MOD((J-1),5) + 1                                                 

         IF (IQ.EQ.3)                             GO TO 1                      

          IF (IQ.GT.3)                IQ = 3                                    
                                                                                
C         IDENTIFICATION COMPLETE; COMPUTE TIME-SERIES AVERAGE                  
                                                                                
         N = 0                                                                 
         X = 9999.                                                             
         TOTAL = 0.                                                            

         DO 7 M=1,500                                                          
          IF (DATA(M).GT.299.)                   GO TO 6                      
           TOTAL = TOTAL + DATA(M)                                           
           N = N + 1                                                         
    6      CONTINUE                                                            
    7   CONTINUE                                                              
                                                                                
        IF (N.GT.0)                      X = TOTAL/N                          

         IF (I.EQ.2.AND.LDATE(4).EQ.0)    MEAN00(K,IQ,LEV,IHR) = X             

          IF (I.EQ.2.AND.LDATE(4).EQ.12)   MEAN12(K,IQ,LEV,IHR) = X             

           IF (I.EQ.4.AND.LDATE(4).EQ.0)    RMS00Z(K,IQ,LEV,IHR) = X             

            IF (I.EQ.4.AND.LDATE(4).EQ.12)   RMS12Z(K,IQ,LEV,IHR) = X             
                                                                                
C         DONE:  READ ANOTHER TIME-SERIES RECORD                                
                                                                                
       GO TO 1                                                                 
                                                                                
   10  CONTINUE                                                                
                                                                                
C      END OF FILE REACHED:  PRINT THE STATS IN WMO FORMAT.                    
                                                                                
       IYEAR = FDATE(1)                                                 
       MM = FDATE(2)                                                           
       KX = 7                                                                  

       IF (REGL)    KX = 1                                                     

        DO 30 K=1,KX                                                            

         DO 25 IQ=1,3                                                          

          IF (SEL3.EQ.LRG.OR.SEL3.EQ.GFS)    THEN                           

          PRINT 105                                                         
          ELSE IF (REGL)    THEN                                              

          PRINT 106, SEL3                                                   
          ELSE                                                                

          PRINT 107                                                         
          ENDIF                                                               

          DO 20 LEV=1,3                                                       

           TAB = 1.0*K + 3.0 + 0.1*(3.0*(IQ-1) + LEV)                        
           PRINT 110, TAB, A1(K), A2(K)                                      

           IF (IQ.EQ.1)    PRINT 121, P(LEV), MONTH(MM), IYEAR               

            IF (IQ.EQ.2)    PRINT 122, P(LEV), MONTH(MM), IYEAR               

             IF (IQ.EQ.3)    PRINT 123, P(LEV), MONTH(MM), IYEAR               

              IF (IQ.EQ.1)    PRINT 131                                         
              IF (IQ.EQ.2)    PRINT 138                                         

               IF (IQ.EQ.3)    PRINT 132                                         

           DO 15 IHR=IB,IFCST,INC                                                    
            JHR = ITX*IHR                                                    
            PRINT 140, JHR,                                                 
     &                 MEAN00(K,IQ,LEV,IHR), MEAN12(K,IQ,LEV,IHR),          
     &                 RMS00Z(K,IQ,LEV,IHR), RMS12Z(K,IQ,LEV,IHR)           

   15     CONTINUE                                                          

   20    CONTINUE                                                            
   25    CONTINUE                                                              
   30    CONTINUE                                                                

         STOP                                                                    
                                                                                
C     PRINT DISASTER MESSAGE AND QUIT IF PROBLEM READING TIME-SERIES            
                                                                               
   99 CONTINUE                                                                  
      PRINT 199                                                                 

  199 FORMAT ('0 *** READ ERROR IN WMOSTATS JOBSTEP ***')                       
      STOP 99                                                                   
      END                                                                       
                                                                                
      SUBROUTINE W3FS11 (IDATE,IYEAR,MONTH,IDAY,IHOUR,NN)   
 
C     LOCAL DATE (UN)PACKER: REPLACES OLD HDS SUB W3FS11
 
      INTEGER(4)  IYEAR,MONTH,IDAY,IHOUR                     

      CHARACTER(1)  IDATE(4)                                
 
C     PACK IF NN=0, ELSE UNPACK
                                                         
      IF(NN.NE.0) THEN                                  
       IYEAR = ICHAR ( IDATE(1) )                    
       MONTH = ICHAR ( IDATE(2) )                   
       IDAY  = ICHAR ( IDATE(3) )                  
       IHOUR = ICHAR ( IDATE(4) )                 
      ELSE                                         
       IDATE(1) = CHAR ( IYEAR )                
       IDATE(2) = CHAR ( MONTH )               
       IDATE(3) = CHAR ( IDAY  )              
       IDATE(4) = CHAR ( IHOUR )             
      END IF                                  
                                             
      RETURN                                
      END                                  
 
      SUBROUTINE DATFIX(IDATE,IYEAR,MONTH,IDAY,IHOUR,NN)
 
C     DATE (UN)PACKER: SECOND VERSION
 
      INTEGER(4) IDATE,IYEAR,MONTH,IDAY,IHOUR
 
C     PACK IF NN=0, ELSE UNPACK
 
      IF(NN.NE.0) THEN
       IYEAR=IDATE/1000000
       LLL=IDATE-IYEAR*1000000 
       MONTH=LLL/10000
       LLLL=LLL-MONTH*10000
       IDAY=LLLL/100
       IHOUR=IDATE-IYEAR*1000000-MONTH*10000-IDAY*100
      ELSE
       IDATE=IYEAR*1000000+MONTH*10000+IDAY*100+IHOUR
      END IF

      RETURN
       END 
