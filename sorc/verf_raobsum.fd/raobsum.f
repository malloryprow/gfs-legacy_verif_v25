C ABSTRACT: C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C
C MAIN PROGRAM: raobsum
C   PRGMMR: LILLY            ORG: NP12        DATE: 2009-06-01
C
C ABSTRACT: READ DAILY FILE OF OBS-GES RAOB HEIGHTS AND WINDS AT 11 MANDATORY
C           LEVELS, COMPUTE MEAN AND RMS FOR MONTH AND TABULATE NUMBER OF OBS AND
C           NUMBER OF GROSS ERRORS.  
C
C PROGRAM HISTORY LOG:
C
C 1994-12-24  C. L. VLCEK -- ORIGINAL AUTHOR
C
C USAGE:
C   INPUT FILES:
C        FT05 -- CARD INPUT SPECIFYING PERIOD (USUALLY A CALENDAR MONTH)
C                OVER WHICH TO COMPUTE STATISTICS; OPTION TO PRINT STATS
C                (0=NOPRINT, 1=LIST, 2=STATS, 3=DATE-INVEN, 4=COPY).
C        FT10 -- FILE CONTAINING INDIVIDUAL OBS-MINUS-GES VALUES
C
C   OUTPUT FILES:
C        FT05 -- CARD INPUT SPECIFYING PERIOD (USUALLY A CALENDAR MONTH)        
C                OVER WHICH TO COMPUTE STATISTICS; OPTION TO PRINT STATS        
C                (0=NOPRINT, 1=LIST, 2=STATS, 3=DATE-INVEN, 4=COPY).           
C        FT10 -- FILE CONTAINING INDIVIDUAL OBS-MINUS-GES VALUES                
C                                                                               
C      OUTPUT FILES:                                                            
C        FT06 -- PRINTOUT OF JOB STATUS, OPTIONAL PRINTOUT OF STATS
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

      PROGRAM RAOBSUM

C  PROGRAM RAOBSUM BY C.L.VLCEK                                                 
C                                                                               
C  READ DAILY FILE OF OBS-GES RAOB HEIGHTS AND WINDS AT 11 MANDATORY            
C  LEVELS, COMPUTE MEAN AND RMS FOR MONTH AND TABULATE NUMBER OF OBS AND        
C  NUMBER OF GROSS ERRORS.  WRITE RESULTS TO ANOTHER FILE WHICH WILL            
C  SERVE AS INPUT FOR SELECTING DATA TO BE PRINTED FOR THE QUALITY              
C  MONITORING REPORT.  COMPUTATIONS ARE TO BE MADE SEPARATELY FOR 00 UTC        
C  AND 12 UTC DATA.                                                             
C                                                                               
C                                                                               
C      INPUT FILES:                                                             
C        FT05 -- CARD INPUT SPECIFYING PERIOD (USUALLY A CALENDAR MONTH)        
C                OVER WHICH TO COMPUTE STATISTICS; OPTION TO PRINT STATS        
C                (0=NOPRINT, 1=LIST, 2=STATS, 3=DATE-INVEN, 4=COPY).           
C        FT10 -- FILE CONTAINING INDIVIDUAL OBS-MINUS-GES VALUES                
C                                                                               
C      OUTPUT FILES:                                                            
C        FT06 -- PRINTOUT OF JOB STATUS, OPTIONAL PRINTOUT OF STATS
C        FT15 -- FILE CONTAINING INDIVIDUAL OBS-MINUS-GES VALUES (COPY)   
C                                                                               
C        FT20 -- FILE CONTAINING COUNTS (>9) AND MEAN AND RMS VALUES OF         
C                OBS-MINUS-GES FOR HEIGHT AND WIND AT EACH LEVEL FOR            
C                EACH STATION                                                   
C                                                                               
C                                                                               
       REAL(8)  SUM2DZ(1500,11), S2DVCT(1500,11), DUV2, VARVCT             
       REAL(4)  ZBIAS(11), UBIAS(11), VBIAS(11), RMSZ(11), RMSVE(11)            
       REAL(4)  DZ(11), DU(11), DV(11), DVECT(11), ZGROS(11), VGROS(11)         
       REAL(4)  SUMDZ(1500,11),SUMDU(1500,11),SUMDV(1500,11)   
       REAL(4) SUMDVE(1500,11)
       INTEGER(8)  DATE, YY, MM, DD, HH, FDATE, LDATE, XDATE
       INTEGER(4)  DATE4, P(11), NPRINT, NCRAY
       INTEGER(4)  NDZ(1500,11), NGROSZ(1500,11), NDU(1500,11)     
       INTEGER(4)  NDV(1500,11)
       INTEGER(4)  IDSTN(1500), NGROSV(1500,11), NDVECT(1500,11)
       CHARACTER(8)  STNID
C                                                                               
       DATA  F9 /1500./, P /1000,850,700,500,400,300,250,200,150,100,50/         
       DATA  ZGROS /3*100.,150.,175.,200.,225.,250.,275.,300.,325./             
       DATA  VGROS /35.,35.,40.,45.,50.,60.,60.,50.,50.,45.,40./                
       DATA  I9 /150099/, IN /10/, ITOUT /20/, MIN /4/, XDATE /0/              
       DATA  IOUT /15/
C                                                                               
C      FIRST READ IN DATE RANGE FOR SUMMARY (USUALLY 1 CALENDAR MONTH)          
C                                                                               
       READ(7,100) FDATE, LDATE, NPRINT, NCRAY                                  
  100  FORMAT(2x,I10,2x,I10,11x,I1,11x,I1)                                                             
       PRINT *, ' BEGIN PROGRAM RAOBSUM .... '
       PRINT *, ' FDATE=',FDATE, '  LDATE=',LDATE, '  NPRINT='
     &        , NPRINT
       IF (NPRINT.EQ.3)    PRINT *, 'INVEN REQUESTED (DATE ONLY) ....'
C                                                                               
C      TWO-PASS OPERATION -- ONE FOR 00 UTC AND ONE FOR 12 UTC                  
C                                                                               
       LSTN = I9                                                                
C                                                                               
       DO 2000 ITIME=1,2                                                        
C                                                                               
C      INITIALIZE SUM VALUES TO ZERO AND STATION IDENTS TO 150099                
C                                                                               
         IF (ITIME.EQ.2)  THEN                                                
           IF (NPRINT.GE.3)    GO TO 2000
           REWIND IN
         ENDIF
         NSTN = 0                                                               
         DO I=1,1500                                                             
           IDSTN(I) = I9                                                        
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
C                                                                               
C        NOW READ IN STATION REPORTS (OBS-GES) TO END-OF-FILE                   
C                                                                               
   10    CONTINUE                                                               
           IF (NCRAY.EQ.0)    THEN
             READ(IN,ERR=10,END=1000)  STNID, DATE,               
     &            (DZ(K),DU(K),DV(K),K=1,11)                                    
           ELSE
             READ(IN,ERR=10,END=1000)  STNID, DATE4,               
     &            (DZ(K),DU(K),DV(K),K=1,11)                                    
             DATE = DATE4
           ENDIF
           IF (NPRINT.EQ.3.AND.DATE.NE.XDATE)    THEN
             PRINT 110, DATE
  110        FORMAT (1X, I10)
             XDATE = DATE
           ENDIF
C                                                                               
C          CHECK DATES FIRST; IF PAST LDATE, DON'T WAIT FOR EOF ....            
C                                                                               
           YY = DATE/1000000
           MY = DATE - 1000000*YY
           MM = MY/10000
           ND = MY - 10000*MM
           DD = ND/100
           HH = ND - 100*DD
           IH = HH/12 + 1                                                       
           IF (DATE.LT.FDATE)    GO TO 10                        
           IF (DATE.GT.LDATE)    GO TO 1000                      
C        
C          IF COPY ONLY (NPRINT=4), DO THAT HERE ....
C     
           IF (NPRINT.EQ.4)    THEN
             WRITE(IOUT)  STNID, DATE, (DZ(K),DU(K),DV(K),K=1,11)
             GO TO 10
           ELSE IF (IH.NE.ITIME)    THEN    
             GO TO 10
           ENDIF
C                                                                               
C          CHECK FOR DUPLICATE REPORT ....                                      
C                                                                               
           READ (UNIT=STNID,FMT=101,ERR=10,END=1000)  ISTN
  101      FORMAT (I5)
           IF (ISTN.EQ.LSTN)    GO TO 10                                        
           LSTN = ISTN                                                          
C                                                                               
C          IF NPRINT = 1, WE WANT TO LIST THE INPUT DATA                        
C                                                                               
           IF (NPRINT.EQ.1)    THEN                                             
             PRINT 200                                                          
  200        FORMAT (//,' IDSTN      DATE      PRES       ',                    
     &                  ' DZ        DU        DV',/)                            
             PRINT 201, ISTN, DATE, P(1), DZ(1), DU(1), DV(1)                  
  201        FORMAT (1X, I5, I12, I8, 3F10.2)                                  
             DO I=2,11                                                          
               PRINT 202, P(I), DZ(I), DU(I), DV(I)                            
  202          FORMAT (16X, I10, 3F10.2)                                        
             END DO                                                             
           ENDIF                                                                
C                                                                               
C          CHECK TO SEE WHERE ISTN BELONGS IN IDSTN ARRAY ...                   
C                                                                               
           DO I=1,1500                                                           
             IF (IDSTN(I).EQ.I9)    THEN                                        
               IDSTN(I) = ISTN                                                  
               NSTN = NSTN + 1                                                  
               GO TO 11                                                         
             ELSE IF (ISTN.EQ.IDSTN(I))    THEN
               GO TO 11
             ENDIF                                                              
           END DO                                                               
           PRINT *, '  *** MAX DIMENSIONS REACHED FOR STATIONS !!!',           
     &               '  INCREASE DIMENSIONS AND TRY AGAIN.  ***'               
           STOP 998                                                             
   11      CONTINUE                                                             
C                                                                               
C          WE NOW HAVE 11 LEVELS OF HEIGHT AND WIND DATA                        
C          CHECK FOR MISSING OR GROSS VALUES; IF OK, THEN                       
C          COMPUTE VECTOR DIFFERENCE, AND ADD TO SUMS AND                       
C          COUNTERS.                                                            
C                                                                               
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
C                                                                               
C        ABOVE ... READ ANOTHER STATION                                         
C        BELOW ... REACHED END OF FILE OR LAST DESIRED DATE                     
C                                                                               
 1000    CONTINUE                                                               
C                                                                               
C        COMPUTE AND WRITE OUT STATISTICS FOR 00 OR 12 UTC DATA.                
C        FIRST WRITE HEADER RECORD (AND PRINT IF NPRINT =2) ....                
C                                                                               
         IH = 12*(ITIME-1)                                                      
         WRITE (20)  NSTN, IH, FDATE, LDATE                                     
         IF (NPRINT.EQ.2)    PRINT 250, NSTN, IH, FDATE, LDATE                  
  250    FORMAT (//, ' STATISTICAL SUMMARY FOR ', I5, ' STATIONS (',            
     &           I2, 'Z OBSERVATIONS) BETWEEN', I12, ' AND', I12, /)            
C                                                                               
C        ... THEN STATS FOR EACH STATION AND LEVEL (N>MIN)                        
C                                                                               
         DO I=1,1500                                                             
           IF (IDSTN(I).EQ.I9)    GO TO 2000
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
           WRITE (20)  IDSTN(I), (P(J), NDZ(I,J), NGROSZ(I,J),                  
     &                 ZBIAS(J), RMSZ(J), NDU(I,J), NGROSV(I,J),                
     &                 UBIAS(J), VBIAS(J), RMSVE(J), J=1,11)                    
           IF (NPRINT.EQ.2)    THEN                                             
             PRINT 251                                                          
  251        FORMAT (//, ' IDSTN   PRES    NDZ  NGROS  ZBIAS  ',                 
     &               ' RMSZ    NDV  NGROS  UBIAS  VBIAS  RMSVE', /)             
             PRINT 252, IDSTN(I), P(1), NDZ(I,1), NGROSZ(I,1),                  
     &                  ZBIAS(1), RMSZ(1), NDU(I,1), NGROSV(I,1),               
     &                  UBIAS(1), VBIAS(1), RMSVE(1)                            
  252        FORMAT (1X, I5, 3I7, 2F7.1, 2I7, 3F7.2)                            
             DO J=2,11                                                          
               PRINT 253,  P(J), NDZ(I,J), NGROSZ(I,J),                         
     &                     ZBIAS(J), RMSZ(J), NDU(I,J), NGROSV(I,J),            
     &                     UBIAS(J), VBIAS(J), RMSVE(J)                         
  253          FORMAT (6X, 3I7, 2F7.1, 2I7, 3F7.2)                              
             END DO                                                             
           ENDIF                                                                
         END DO                                                                 
 2000  CONTINUE                                                                 
C                                                                               
C      JOB DONE ... PRINT OK MESSAGE AND QUIT                                   
C                                                                               
       PRINT 990
  990  FORMAT (///, '  RAOBSUM JOB SUCCESSFULLY COMPLETED. ')                 
       STOP                                                                     
C                                                                               
C      COME HERE IF READ ERROR OCCURRED ... PRINT MESSAGE AND QUIT              
C                                                                               
C 1500  CONTINUE                                                                 
C      PRINT *, ' *** ERROR READING OBS-GES INPUT, QUITTING ***'              
C      STOP 1500                                                                 
       END                                                                      
