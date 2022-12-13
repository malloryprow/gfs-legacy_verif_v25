C  PROGRAM SHIPSUM BY C.L.VLCEK                                                 
C                                                                               
C  READ DAILY FILE OF OBS-GES RAOB HEIGHTS AND WINDS AT 11 MANDATORY            
C  LEVELS, COMPUTE MEAN AND RMS FOR MONTH AND TABULATE NUMBER OF OBS AND        
C  NUMBER OF GROSS ERRORS.  WRITE RESULTS TO ANOTHER FILE WHICH WILL            
C  SERVE AS INPUT FOR SELECTING DATA TO BE PRINTED FOR THE QUALITY              
C  MONITORING REPORT.  COMPUTATIONS ARE TO BE MADE SEPARATELY FOR 00 UTC        
C  AND 12 UTC DATA.                                                             
C                                                                               
C  08-27-1998:  COMPILED IN F90 AND MADE Y2K-COMPLIANT
C  02-16-1999:  ADD COPY OPTION (FOR BINARY FILE ONLY!!)
C  08-11-1999:  COMPILE ON IBM RS6000, OPTION TO READ FROM ASCII.
C  10-01-2009:  COMPILE ON P6.
C                                                                               
C      INPUT FILES:                                                             
C        FT07 -- CARD INPUT SPECIFYING PERIOD (USUALLY A CALENDAR MONTH)        
C                OVER WHICH TO COMPUTE STATISTICS; OPTION TO PRINT STATS        
C                (0=NOPRINT, 1=LIST, 2=STATS, 3=DATE-INVEN, 4=COPY).            
C        FT10 -- FILE CONTAINING INDIVIDUAL OBS-MINUS-GES VALUES                
C                                                                               
C      OUTPUT FILES:                                                            
C        FT06 -- PRINTOUT OF JOB STATUS, OPTIONAL PRINTOUT OF STATS             
C        FT15 -- COPY OF FILE CONTAINING INDIVIDUAL OBS-MINUS-GES VALUES        
C        FT20 -- FILE CONTAINING COUNTS (>9) AND MEAN AND RMS VALUES OF         
C                OBS-MINUS-GES FOR HEIGHT AND WIND AT EACH LEVEL FOR            
C                EACH STATION                                                   
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
C   MACHINE : P6
C
C$$$

      PROGRAM SHIPSUM

       REAL(8) SUM2DZ(99,11), S2DVCT(99,11), DUV2, VARVCT, XLAT, XLON           
                                                                                
       REAL(4) ZBIAS(11), UBIAS(11), VBIAS(11), RMSZ(11), RMSVE(11)             
       REAL(4) DZ(11), DU(11), DV(11), DVECT(11), ZGROS(11), VGROS(11)          
       REAL(4) SUMDZ(99,11),SUMDU(99,11),SUMDV(99,11),SUMDVE(99,11)          
                                                                               
       INTEGER(8) DATE, YY, MM, DD, HH, FDATE, LDATE, XDATE
       INTEGER(8) NCEN, NCEN19
                                                                               
       INTEGER(4) P(11)
       INTEGER(4) NDZ(99,11), NGROSZ(99,11), NDU(99,11), NDV(99,11)            
       INTEGER(4) NGROSV(99,11), NDVECT(99,11), NPRINT, NASCII, ICPY            
                                                                               
       CHARACTER(8) SHIPID, IDSHIP(99), LSTSHP, BLANK8, JDSHIP
                                                                               
       DATA  F9 /999./, P /1000,850,700,500,400,300,250,200,150,100,50/         
       DATA  ZGROS /3*100.,150.,175.,200.,225.,250.,275.,300.,325./             
       DATA  VGROS /35.,35.,40.,45.,50.,60.,60.,50.,50.,45.,40./                
       DATA  I9 /99999/, IN /10/, ICPY /15/, ITOUT /20/, MIN /4/
       DATA  XDATE /0/     
       DATA  BLANK8 /'        '/, JDSHIP /'IDSHIP  '/
      
       NCEN   = 10**8
       NCEN19 = 19*NCEN
                                                                               
C      FIRST READ IN DATE RANGE FOR SUMMARY (USUALLY 1 CALENDAR MONTH)          
                                                                               
       READ(7,100) FDATE, LDATE, NPRINT, NASCII                                 
  100  FORMAT(4I12)                                                             
       IF (NPRINT.EQ.3)    PRINT *, 'INVEN REQUESTED (DATE ONLY) ....'
                                                                               
C      TWO-PASS OPERATION -- ONE FOR 00 UTC AND ONE FOR 12 UTC                  
                                                                               
       LSTSHP = BLANK8                                                          
                                                                               
       DO 2000 ITIME=1,2                                                        
                                                                               
C      INITIALIZE SUM VALUES TO ZERO AND STATION IDENTS TO (BLANK)              
                                                                               
         IF (ITIME.EQ.2)  THEN                                                
           IF (NPRINT.GE.3)    GO TO 2000
           REWIND IN
         ENDIF
         NSTN = 0                                                               
         DO I=1,99                                                             
           IDSHIP(I) = BLANK8                                                   
           DO J=1,11                                                            
             SUM2DZ(I,J) = 0.                                                   
             S2DVCT(I,J) = 0.                                                   
             SUMDZ(I,J)  = 0.                                                   
             SUMDU(I,J)  = 0.                                                   
             SUMDV(I,J)  = 0.                                                   
             SUMDVE(I,J) = 0.                                                   
             NDZ(I,J)    = 0                                                    
             NDU(I,J)    = 0                                                    
             NDV(I,J)    = 0                                                    
             NDVECT(I,J) = 0                                                    
             NGROSZ(I,J) = 0                                                    
             NGROSV(I,J) = 0                                                    
           END DO                                                               
         END DO                                                                 
                                                                               
C        NOW READ IN STATION REPORTS (OBS-GES) TO END-OF-FILE                   
C        ADD CENTURY INFO TO DATE FOR OLDER FILES (BEFORE 8/1998).
                                                                               
   10    CONTINUE                                                               
           IF (NASCII.EQ.0)    THEN
             READ(IN,ERR=999,END=1000)  DATE, SHIPID, XLAT, XLON,              
     &            (DZ(K),DU(K),DV(K),K=1,11)                                    
           ELSE
   11        CONTINUE
               READ (IN,101,ERR=999,END=1000)  SHIPID
  101          FORMAT (1X, A8)
             IF (SHIPID.NE.JDSHIP)    GO TO 11
             READ (IN,101,ERR=999,END=1000)  SHIPID
             READ(IN,102,ERR=999,END=1000)  SHIPID, DATE, P(1),              
     &            DZ(1),DU(1),DV(1)                                    
             DO K=2,11
               READ(IN,103,ERR=999,END=1000)  P(K),              
     &              DZ(K),DU(K),DV(K)                                    
             END DO
  102        FORMAT (1X, A8, I10, I8, 3F10.2)
  103        FORMAT (18X, I8, 3F10.2)
           ENDIF
           IF (FDATE.GT.NCEN.AND.DATE.LT.NCEN)   DATE = DATE + NCEN19
           IF (NPRINT.EQ.3.AND.DATE.NE.XDATE)    THEN
             PRINT 110, DATE
  110        FORMAT (1X, I12)
             XDATE = DATE
           ENDIF
                                                                               
C          CHECK DATES FIRST; IF PAST LDATE, DON'T WAIT FOR EOF ....            
                                                                               
           YY = DATE/1000000
           MY = DATE - 1000000*YY
           MM = MY/10000
           ND = MY - 10000*MM
           DD = ND/100
           HH = ND - 100*DD
           IH = HH/12 + 1                                                       
           IF (DATE.LT.FDATE)    GO TO 10                        
           IF (DATE.GT.LDATE)    GO TO 1000                      
           IF (NPRINT.EQ.4)    THEN
             WRITE (ICPY)  DATE, SHIPID, XLAT, XLON,              
     &             (DZ(K),DU(K),DV(K),K=1,11)                                   
             GO TO 10
           ENDIF
           IF (IH.NE.ITIME)    GO TO 10                        
                                                                               
C          CHECK FOR DUPLICATE REPORT ....                                      
                                                                               
           IF (SHIPID.EQ.LSTSHP)    GO TO 10                                    
           LSTSHP = SHIPID                                                      
                                                                               
C          IF NPRINT = 1, WE WANT TO LIST THE INPUT DATA                        
                                                                               
           IF (NPRINT.EQ.1)    THEN                                             
             PRINT 200                                                          
  200        FORMAT (//,' IDSHIP      DATE      PRES       ',                   
     &                  ' DZ        DU        DV',/)                            
             PRINT 201, SHIPID, DATE, P(1), DZ(1), DU(1), DV(1)                 
  201        FORMAT (1X, A6, I12, I8, 3F10.2)                                   
             DO I=2,11                                                          
               PRINT 202, P(I), DZ(I), DU(I), DV(I)                            
  202          FORMAT (18X, I8, 3F10.2)                                        
             END DO                                                             
           ENDIF                                                                
                                                                               
C          CHECK TO SEE WHERE SHIPID BELONGS IN IDSHIP ARRAY ...                
                                                                               
           DO I=1,99                                                           
             IF (IDSHIP(I).EQ.BLANK8)    THEN                                   
               IDSHIP(I) = SHIPID                                               
               NSTN = NSTN + 1                                                  
               GO TO 12                                                         
             ELSE IF (SHIPID.EQ.IDSHIP(I))    THEN
               GO TO 12
             ENDIF                                                              
           END DO                                                               
           PRINT *, '  *** MAX DIMENSIONS REACHED FOR STATIONS !!!',           
     &               '  INCREASE DIMENSIONS AND TRY AGAIN.  ***'               
           STOP 998                                                             
   12      CONTINUE                                                             
                                                                               
C          WE NOW HAVE 11 LEVELS OF HEIGHT AND WIND DATA                        
C          CHECK FOR MISSING OR GROSS VALUES; IF OK, THEN                       
C          COMPUTE VECTOR DIFFERENCE, AND ADD TO SUMS AND                       
C          COUNTERS.                                                            
                                                                               
           DO J=1,11                                                            
             IF (DZ(J).NE.F9)    THEN                                           
               IF (ABS(DZ(J)).GE.ZGROS(J))    THEN                              
                 NGROSZ(I,J) = NGROSZ(I,J) + 1                                  
               ELSE                                                             
                 NDZ(I,J) = NDZ(I,J) + 1                                        
                 SUMDZ(I,J) = SUMDZ(I,J) + DZ(J)                                
                 SUM2DZ(I,J) = SUM2DZ(I,J) + DZ(J)**2                           
               ENDIF                                                            
             ENDIF                                                              
             IF (DU(J).NE.F9.AND.DV(J).NE.F9)    THEN                           
               DUV2 = DU(J)**2 + DV(J)**2                                       
               DVECT(J) = SQRT(DUV2)                                          
               IF (DVECT(J).GE.VGROS(J))    THEN                                
                 NGROSV(I,J) = NGROSV(I,J) + 1                                  
               ELSE                                                             
                 NDU(I,J) = NDU(I,J) + 1                                        
                 NDV(I,J) = NDV(I,J) + 1                                        
                 SUMDU(I,J) = SUMDU(I,J) + DU(J)                                
                 SUMDV(I,J) = SUMDV(I,J) + DV(J)                                
                 SUMDVE(I,J) = SUMDVE(I,J) + DVECT(J)                           
                 S2DVCT(I,J) = S2DVCT(I,J) + DUV2                               
                 NDVECT(I,J) = NDVECT(I,J) + 1                                  
               ENDIF                                                            
             ENDIF                                                              
           END DO                                                               
         GO TO 10                                                               
                                                                               
C        ABOVE ... READ ANOTHER STATION                                         
C        BELOW ... REACHED END OF FILE OR LAST DESIRED DATE                     
                                                                               
 1000    CONTINUE                                                               
 
C        SKIP THIS PART IF COPY OR INVEN
C 
         IF (NPRINT.GE.3)    GO TO 2000
                                                                               
C        COMPUTE AND WRITE OUT STATISTICS FOR 00 OR 12 UTC DATA.                
C        FIRST WRITE HEADER RECORD (AND PRINT IF NPRINT =2) ....                
                                                                               
         IH = 12*(ITIME-1)                                                      
         WRITE (20)  NSTN, IH, FDATE, LDATE                                     
         IF (IH.EQ.0)  THEN
         WRITE(6,111) YY, IH
  111    FORMAT(10x,'NCEP',1x,'QUALITY',1x,'MONITORING',5x,
     &       'XXXXXXXXX',1x,I4,5x,'(0',I1,1x,'UTC',1x,
     &       'OBSERVATIONS)'/)
         ELSE
         WRITE(6,112) YY, IH
  112    FORMAT(//,10x,'NCEP',1x,'QUALITY',1x,'MONITORING',5x,
     &       'XXXXXXXXX',1x,I4,5x,'(',I2,1x,'UTC',1x,
     &       'OBSERVATIONS)'/)
         ENDIF
                                                                               
C        ... THEN STATS FOR EACH STATION AND LEVEL (N>MIN)                      
                                                                               
         DO I=1,99                                                             
           IF (IDSHIP(I).EQ.BLANK8)    GO TO 2000
           DO J=1,11                                                            
             ZBIAS(J) = F9                                                      
             UBIAS(J) = F9                                                      
             VBIAS(J) = F9                                                      
             RMSZ(J)  = F9                                                      
             RMSVE(J) = F9                                                      
             IF (NDZ(I,J).GT.MIN)    THEN                                      
               ZBIAS(J) = SUMDZ(I,J)/FLOAT(NDZ(I,J))                            
               VARZ     = SUM2DZ(I,J)/FLOAT(NDZ(I,J))                           
               RMSZ(J)  = SQRT(VARZ)                                           
             ENDIF                                                              
             IF (NDU(I,J).GT.MIN.AND.NDV(I,J).GT.MIN)    THEN                   
               UBIAS(J) = SUMDU(I,J)/FLOAT(NDU(I,J))                          
               VBIAS(J) = SUMDV(I,J)/FLOAT(NDV(I,J))                            
               VARVCT   = S2DVCT(I,J)/FLOAT(NDVECT(I,J))                        
               RMSVE(J) = SQRT(VARVCT)                                         
             ENDIF                                                              
           END DO                                                               
           IF (NPRINT.EQ.2)    THEN                                             
             PRINT 251                                                          
  251        FORMAT (//, ' SHIP     PRES    NDZ  NGROS  ZBIAS  ',               
     &               ' RMSZ    NDV  NGROS  UBIAS  VBIAS  RMSVE', /)             
             PRINT 252, IDSHIP(I), P(1), NDZ(I,1), NGROSZ(I,1),                 
     &                  ZBIAS(1), RMSZ(1), NDU(I,1), NGROSV(I,1),               
     &                  UBIAS(1), VBIAS(1), RMSVE(1)                            
  252        FORMAT (1X, A6, 3I7, 2F7.1, 2I7, 3F7.1)
             DO J=2,11                                                          
               PRINT 253,  P(J), NDZ(I,J), NGROSZ(I,J),                         
     &                     ZBIAS(J), RMSZ(J), NDU(I,J), NGROSV(I,J),            
     &                     UBIAS(J), VBIAS(J), RMSVE(J)                         
  253          FORMAT (7X, 3I7, 2F7.1, 2I7, 3F7.1)                              
             END DO                                                             
           ENDIF                                                                
         END DO                                                                 
 2000  CONTINUE                                                                 
       PRINT 990
  990  FORMAT (//)                 
                                                                               
C      JOB DONE ... PRINT OK MESSAGE AND QUIT                                   
                                                                               
       STOP                                                                     
                                                                               
C      COME HERE IF READ ERROR OCCURRED ... PRINT MESSAGE AND QUIT              
                                                                               
  999  CONTINUE                                                                 
       PRINT *, ' *** ERROR READING OBS-GES INPUT, QUITTING ***'              
       STOP 999                                                                 
       END                                                                      
