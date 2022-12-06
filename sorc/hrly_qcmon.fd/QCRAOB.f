      SUBROUTINE QCRAOB

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM: QCMON
C   PRGMMR: LILLY            ORG: NP12        DATE: 2009-03-09
C
C ABSTRACT: This subroutine generates radiosonde reports of
C           geopotential height(Z) and wind(U,V) observation minus
C           the first guess(FG) value at the point of observation.
C           Pressure levels are 1000MB, 850MB, 700MB, 500MB, 400MB,
C           300MB, 250MB, 200MB, 150MB, 100MB and 50MB.  Also, the
C           station ID, lat and long are given.
C
C PROGRAM HISTORY LOG:
C   2009-03-09  Original Author: Steve Lilly
C
C USAGE:
C   INPUT FILES:
C     datatype - identifies the type of data being used
C                "uair" to indicate upperair observations
C
C     FT10F001 - gdas1.txx"Z".prepbufr
C                where xx=(00 and 12)
C
C   OUTPUT FILES:
C     FT34F001 - radiosonde report
C                    qcraob$mm$dd  (BINARY)
C                    asraob$mm$dd  (ASCII)
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

       COMMON  /DATELN/  LENDAT

       REAL(4) DZ(11), DU(11), DV(11), DVECT(11)                                

       INTEGER P(11), IP, STNID, DATE, YY, MM, DD, HH, INFILE, LENDAT           
       INTEGER ICNT

       CHARACTER(8)  SUBSET, SID
       CHARACTER(10) CDATE
       CHARACTER(40) HSTR, CAT1                                                 

       LOGICAL       SKIP                                                       

       DIMENSION     HDR(10), UPA(10,255)                             
                                                                                
       DATA  F9 /999./, P /1000,850,700,500,400,300,250,200,150,100,50/         
       DATA  HSTR /'SID XOB YOB DHR ELV T29          '/                         
       DATA  CAT1 /'CAT=1 POB ZOB UOB VOB ZFC UFC VFC'/                         
       DATA  XMSG /10E10/, INFILE /10/, ITOUT2/34/                              
 
  300  FORMAT (A8, I10)
  301  FORMAT (11(I4,3F6.1,/))

       XMSG1 = XMSG - 1.
       LENDAT = 10
       HDRX = 0.
       ICNT = 0

       DO I=1,10
         HDR(I)=0.0
       END DO

       DO J=1,10
         DO K=1,255
           UPA(J,K)=0.0
         END DO
       END DO

C*     Open the BUFR file                                                  
                                                                                
       CALL OPENBF(INFILE,'IN',INFILE)                                          
                                                                                
C*     Finds subset 'ADPUPA'                                                
                                                                                
       CALL READMG(INFILE,SUBSET,IDATE,IRET)                                  
       DO WHILE (SUBSET.NE.'ADPUPA')                                         
         IF (IRET.NE.0)  THEN                                         
           ICNT = 1
         ENDIF
         IF (ICNT.EQ.0) CALL READMG(INFILE,SUBSET,IDATE,IRET)
         IF (ICNT.EQ.1) EXIT
       END DO                                                                  

C*     Split date up into components                                            
                                                                                
       WRITE (CDATE, '(I10)')  IDATE                                           
       READ  (CDATE, '(I4,3I2)') YY, MM, DD, HH                                 
                                                                                
C*     Read subset (ADPUPA)                                             
                                                                                
       DO WHILE (ICNT.EQ.0)                                                                
         CALL READSB(INFILE,IRET)                                               
           DO WHILE (IRET.NE.0)                                                 
             CALL READMG(INFILE,SUBSET,IDATE,IRET)                              
               DO WHILE (SUBSET.NE.'ADPUPA')                                    
                 IF(IRET.NE.0)  THEN                                       
                 ICNT = 1
               ENDIF
             IF (ICNT.EQ.1) EXIT 
             IF (ICNT.EQ.0) CALL READMG(INFILE,SUBSET,IDATE,IRET)
           END DO
         IF (ICNT.EQ.1) EXIT
         IF (ICNT.EQ.0) CALL READSB(INFILE,IRET)
       END DO
       IF (ICNT.EQ.1)  EXIT
      
C*     Now get the data                                                   
       CALL UFBINT(INFILE,HDR,10,  1,IRET,HSTR)                               
       CALL UFBINT(INFILE,UPA,10,255,IRET,CAT1)                               
       IF (IRET.EQ.0)  CYCLE 
       RTP = HDR(6)
       IRTP = IFIX(RTP)
       SKIP = IRTP.LT.11 .OR. IRTP.GT.13                                      
       IF (SKIP)   CYCLE                                                 
                                                                           
C*     We have several levels of height and wind data.                    
C*     Compute obs-ges differences at 11 desired levels; but first           
C*     initialize as all missing if this is the first pass for current
C*     station.  No check for true duplicate reports. Duplicate reports will 
C*     be handled by programs that read the output of this one.              
                                                                                
       IF (HDR(1).NE.HDRX)    THEN
         DO I=1,11                                                           
           DZ(I) = F9                                                         
           DU(I) = F9                                                         
           DV(I) = F9                                                         
           DVECT(I) = F9                                                      
         END DO                                                               
       ENDIF
                                                                               
       DO J=1,IRET                                                           
         IP = IFIX(UPA(1,J)) 
           DO I=1,11
             IF (IP.EQ.P(I))    THEN                                            
               IF (UPA(2,J).LT.XMSG1.AND.UPA(5,J).LT.XMSG1)  THEN             
                 DZ(I) = UPA(2,J) - UPA(5,J) 
               ENDIF                                                            
               IF (UPA(3,J).LT.XMSG1.AND.UPA(6,J).LT.XMSG1)  THEN             
                 DU(I) = UPA(3,J) - UPA(6,J)
               ENDIF                                                    
               IF (UPA(4,J).LT.XMSG1.AND.UPA(7,J).LT.XMSG1)   THEN           
                 DV(I) = UPA(4,J) - UPA(7,J)
               ENDIF                                                           
             ENDIF                                                              
           END DO
       END DO                                                                 
                                                                                
C*     Write the stnid and differences and then read another report              
                                                                                
       IF (HDR(1).EQ.HDRX)    THEN
         WRITE (ITOUT2)  HDR(1), IDATE, 
     &                  (DZ(I),DU(I),DV(I),I=1,11)
         PRINT 300, HDR(1), IDATE
         PRINT 301, (P(I),DZ(I),DU(I),DV(I),I=1,11)                   
       ENDIF
       HDRX = HDR(1)
       END DO
                                                                                
C*     Come here when all reports have been read, then the program calls CLOSE file
C*     and quit
                                                                                
       CALL CLOSBF(INFILE)                                                      
       RETURN                                                                     
       END
