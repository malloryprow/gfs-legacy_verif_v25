      SUBROUTINE ADSHIP

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:   ADSHIP      
C   PRGMMR: LILLY             ORG: W/NP12     DATE: 2009-03-09
C
C ABSTRACT: This subroutine generates ship-based radiosonde reports
C           of geopotential height(Z) and wind(U,V) observation minus
C           the first guess(FG) value at the point of observation.
C           Pressure levels are 1000MB, 850MB, 700MB, 500MB, 400MB,
C           300MB, 250MB, 200MB, 150MB, 100MB and 50MB.  Also, the
C           station ID, lat and long are given.
C
C PROGRAM HISTORY LOG:
C   2009-03-09  Original Author: Steve Lilly
C
C USAGE:    CALL ADSHIP
C   INPUT ARGUMENT LIST:
C     datatype  - identifies the type of data being used
C                 "ship" to indicate ship observations
C
C     FT12F001  - gdas1.txx"Z".prepbufr
C                 where xx=(00 and 12)
C
C   OUTPUT ARGUMENT LIST:
C      qcship$mm$dd - contains the daily obs - guess differences
C                     for all desired pressure levels (BINARY)
C     shipout$mm$dd - contains the daily obs - guess differences
C                     for all desired pressure levels (ASCII)
C
C   SUBPROGRAMS CALLED:
C     LIBRARY:
C       W3LIB  - NONE
C
C
C   NOTES:     - NONE
C
C   EXIT STATES:
C      COND =   0 - Successful Run
C
C   LANGUAGE: IBM FORTRAN 90
C   MACHINE:  IBM RS6000
C
C$$$

       COMMON  /DATELN/  LENDAT
   
       REAL(4) DDZ(11), DDU(11), DDV(11), DDVECT(11)                                

       INTEGER PP(11), IP, STNID, DATE, YY, MM, DD, HH, INFILE, LENDAT           
       INTEGER ICNT

       CHARACTER(8)  SUBSET, SID
       CHARACTER(10) CDATE
       CHARACTER(40) HSTR, CAT1                                                 

       LOGICAL       SKIP                                                       

       DIMENSION     HHDR(10), UUPA(10,255)                             
                                                                               
       DATA  F9 /999./, PP /1000,850,700,500,400,300,250,200,150,100,50/         
       DATA  HSTR /'SID XOB YOB DHR ELV T29          '/                         
       DATA  CAT1 /'CAT=1 POB ZOB UOB VOB ZFC UFC VFC'/                         
       DATA  XMSG /10E10/, INFILE/12/, ISHIPRAOB/24/                           
 
 299   FORMAT (1X, I10, 2X, A6, F6.2, F8.2, /)
 300   FORMAT (1X, 'PRES     DDZ     DDU     DDV',//,                      
     &   11(1X,I4,3F7.2,/), //)                                      
 9999  FORMAT (' RAOB INCREMENT JOB COMPLETED FOR', I5, 3I3)                   

       XMSG1 = XMSG - 1.
       LENDAT = 10
       HDRX = 0.
       ICNT = 0

       DO I=1,10
         HHDR(I)=0.0
       END DO

       DO J=1,10
         DO K=1,255
           UUPA(J,K)=0.0
         END DO
       END DO


C*     Open the BUFR file                                                   
                                                                                
       CALL OPENBF(INFILE,'IN',INFILE)                                          
                                                                                
C*     Finds subset 'ADPUPA' ...                                                 
                                                                                
       CALL READMG(INFILE,SUBSET,IDATE,IRET)                                  
       DO WHILE (SUBSET.NE.'ADPUPA')                                          
         IF (IRET.NE.0)  THEN                                         
           ICNT = 1
         ENDIF
         IF (ICNT.EQ.1)  EXIT
         IF (ICNT.EQ.0) CALL READMG(INFILE,SUBSET,IDATE,IRET)
       END DO
                                                                                
C*     Split date up into components                                            
                                                                                
       WRITE (CDATE, '(I10)')  IDATE                                       
       READ  (CDATE, '(I4,3I2)') YY, MM, DD, HH                                
       PRINT*, 'DATA VALID AT ', CDATE                                          
                                                                                
C*     Read subset (ADPUPA)                                             
                                                                                
       DO WHILE (ICNT.EQ.0)
         CALL READSB(INFILE,IRET)                                               
           DO WHILE (IRET.NE.0)                                                  
             CALL READMG(INFILE,SUBSET,IDATE,IRET)                             
               DO WHILE (SUBSET.NE.'ADPUPA')                                   
                 IF(IRET.NE.0) THEN                                      
                   ICNT = 1
                 ENDIF
             IF (ICNT.EQ.1)  EXIT
             IF (ICNT.EQ.0)  CALL READMG(INFILE,SUBSET,IDATE,IRET)
           END DO
         IF (ICNT.EQ.1)  EXIT
         IF (ICNT.EQ.0)  CALL READSB(INFILE,IRET)
       END DO
       IF (ICNT.EQ.1)  EXIT
                                                                                
C*     Now get the data                                                  
                                                                                
       CALL UFBINT(INFILE,HHDR,10,  1,IRET,HSTR)                               
       CALL UFBINT(INFILE,UUPA,10,255,IRET,CAT1)                               
       IF (IRET.EQ.0)   CYCLE 
       RTP = HHDR(6)
       SKIP = RTP.LT.22 .OR. RTP.GT.23                                        
       IF (SKIP)    CYCLE                                                  
                                                                                
C*     We have several levels of height and wind data.                    
C*     Compute obs-ges differences at 11 desired levels; but first           
C*     initialize as all missing if this is the first pass for current
C*     station.  No check for true duplicate reports.  Duplicate reports will 
C*     be handled by programs that read the output of this one.    
                                                                                
       IF (HHDR(1).NE.HDRX)    THEN
         DO I=1,11                                                            
           DDZ(I) = F9                                                        
           DDU(I) = F9                                                         
           DDV(I) = F9                                                         
           DDVECT(I) = F9                                                      
         END DO                                                               
       ENDIF
                                                                           
       DO J=1,IRET                                                           
         IP = IFIX(UUPA(1,J))
           DO I=1,11
             IF (IP.EQ.PP(I))    THEN                                            
               IF (UUPA(2,J).LT.XMSG1.AND.UUPA(5,J).LT.XMSG1)  THEN               
                 DDZ(I) = UUPA(2,J) - UUPA(5,J) 
               ENDIF                                                            
               IF (UUPA(3,J).LT.XMSG1.AND.UUPA(6,J).LT.XMSG1)  THEN              
                 DDU(I) = UUPA(3,J) - UUPA(6,J)
               ENDIF                                                           
               IF (UUPA(4,J).LT.XMSG1.AND.UUPA(7,J).LT.XMSG1)   THEN             
                 DDV(I) = UUPA(4,J) - UUPA(7,J)
               ENDIF                                                            
             ENDIF                                                             
           END DO
       END DO                                                                
                                                                               
C*     Write the stnid and differences and then read another report             
                                                                              
       IF (HHDR(1).EQ.HDRX)    THEN
         WRITE (ISHIPRAOB)  IDATE, HHDR(1), HHDR(3), HHDR(2),
     &                     (DDZ(I),DDU(I),DDV(I),I=1,11)
         PRINT 299, IDATE, HHDR(1), HHDR(3), HHDR(2)
         PRINT 300,  (PP(I),DDZ(I),DDU(I),DDV(I),I=1,11)                        
       ENDIF
       HDRX = HHDR(1)
       END DO 
                                                                                
C*     Come here when all reports have been read, then the program calls CLOSE file
C*     and quit

       CALL CLOSBF(INFILE)

       CALL CLOSBF(INFILE)                                                      
       PRINT 9999, YY, MM, DD, HH                                               
       STOP                                                                   
       END
