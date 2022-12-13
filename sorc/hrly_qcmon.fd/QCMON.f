C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C
C MAIN PROGRAM: QCMON
C   PRGMMR: LILLY            ORG: NP12        DATE: 2009-03-09
C
C ABSTRACT: This program generates the following daily statistics 
C           for the Monthly Quality Monitoring Report.
C             1) Reads PREPBUFR file to generate AIRCAR
C                reports of wind(U,V) and temperature(T)
C                observation minus the first guess (FG)
C                value at the point of observation
C             2) Reads PREPBUFR file to generate carrier
C                aircraft reports of wind(U,V) and
C                temperature(T) observation minus the
C                first guess (FG) value at the point of
C                observation
C             3) Reads PREPBUFR file to generate satellite
C                reports of wind(U,V) and temperature(T)
C                observation minus the first guess (FG)
C                value at the point of observation
C             4) Reads PREPBUFR file to generate radiosonde
C                reports of wind(U,V) and temperature(T)
C                observation minus the first guess (FG)
C                value at the point of observation
C             5) Reads PREPBUFR file to generate AMDAR/
C                ASDAR reports of wind(U,V) and temperature(T)
C                observation minus the first guess (FG)
C                value at the point of observation
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
C                where xx=(00, 06, 12 and 18)
C
C   OUTPUT FILES:
C     FT15F001 - ACARS reports qcacr$mm$dd  (ASCII)
C     FT20F001 - carrier aircraft reports qcair$mm$dd  (ASCII)
C     FT25F001 - AMDAR/ASDAR reports qcasd$mm$dd  (ASCII)
C     FT30F001 - satellite reports qcsatw$mm$dd  (ASCII)
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

      PROGRAM QCMON

       COMMON  /DATELN/  LENDAT
                                                                                
       REAL          DU, DV, DS, DT

       INTEGER       STNID , DATE, ILAT, JLON, IP, LENDAT                        
       INTEGER       ICNT, INUM
                                                
       CHARACTER(4)  DATATYPE 
       CHARACTER(8)  SUBSET, SID , CSID, REPORT(4)                                  
       CHARACTER(10) CDATE 
       CHARACTER(40) HSTR  , VSTR , VWND , VWNDD                                         

       DIMENSION     HDR(7), SFC(8), IIDATE(4), WFC(5), WFCC(8)                           
 
       EQUIVALENCE   (HDR(1),CSID)                                               
                                                                                
       DATA  HSTR  /'SID XOB YOB DHR ELV T29 TYP'/                                
       DATA  VSTR  /'CAT=6 ZFC TQM POB PFC PQM TOB ZOB TFC '/                     
       DATA  VWNDD /'CAT=6 UOB VOB WQM UFC VFC POB PFC PQM '/
       DATA  VWND  /'CAT=6 UOB VOB WQM UFC VFC '/                                 
       DATA  XMSG /10E10/, INFILE/10/, IQCACR/15/                                  
       DATA  IQCAIR/20/, IQCASD/25/, IQCSATW/30/
       DATA  REPORT/'AIRCAR', 'ADPUPA', 'AIRCFT', 'SATWND'/
  
   50  FORMAT (A4)
 1111  FORMAT (I10, 2X, A8, 2I4, 6F6.1)
 1112  FORMAT (I10, 2X, A8, 3I4, 3F6.1)

        READ(5,50) DATATYPE
       
        IF ( DATATYPE .EQ. "ship" ) THEN
         CALL ADSHIP
        ENDIF
       
       DO I=1,4
         LENDAT = 10
         ICNT   = 0
                                                                                
         IF (I.EQ.2) THEN
         CALL QCRAOB
         ENDIF

         IF (I.EQ.2)  CYCLE

C*     Open the BUFR file                                                 
                                                                                
         CALL OPENBF(INFILE,'IN',INFILE)                                           
                                                                                
C*     Finding subsets                                                
C*       1-AIRCAR
C*       2-ADPUPA
C*       3-AIRCFT
C*       4-SATWND
                                                                                
         K1 = 0

         CALL READMG(INFILE,SUBSET,IDATE,IRET)                                     
         DO WHILE (SUBSET.NE.REPORT(I))                                        
           IF (IRET.NE.0) THEN                                                
             ICNT = 1
           ENDIF
           IF (ICNT.EQ.0) CALL READMG(INFILE,SUBSET,IDATE,IRET)                    
           IF (ICNT.EQ.1) EXIT                    
         END DO
                                                                                
C*     Split date up into components                                             
                                                                                
         WRITE (CDATE, '(I10)') IDATE                                              
         READ  (CDATE, '(I4,3I2)') IIDATE                                       

C*     Now read the subsets                                              
                                                                                
         DO WHILE (ICNT.EQ.0)
         CALL READSB(INFILE,IRET)                                                
           DO WHILE(IRET.NE.0)
           CALL READMG(INFILE,SUBSET,IDATE,IRET)                           
             DO WHILE(SUBSET.NE.REPORT(I))
               IF (IRET.NE.0) THEN                                       
                 ICNT = 1
               ENDIF
               IF (ICNT.EQ.1) EXIT
               IF (ICNT.EQ.0) CALL READMG(INFILE,SUBSET,IDATE,IRET)
             END DO
           IF (ICNT.EQ.1) EXIT
           IF (ICNT.EQ.0) CALL READSB(INFILE,IRET)                                
           END DO
         IF (ICNT.EQ.1) EXIT
                                                                                
C*     Now get the data                                                     
                                                                                
         IF (SUBSET.EQ.'AIRCAR'.OR.SUBSET.EQ.'AIRCFT')  THEN
         CALL UFBINT(INFILE,HDR,7,1,IRET,HSTR)                                     
         IRTP = IFIX(HDR(6))                                                       
         ITYP = IFIX(HDR(7))                                                  
         IF (IRTP .NE. 41) CYCLE

         IF (REPORT(I).EQ.'AIRCAR')  THEN
           IF (ITYP .EQ. 130 .OR. ITYP .EQ. 133) CALL                              
     &       UFBINT(INFILE,SFC,8,1,IRET,VSTR)                                         
           IF (ITYP .EQ. 230 .OR. ITYP .EQ. 233) CALL                              
     &       UFBINT(INFILE,WFC,5,1,IRET,VWND) 
         ENDIF

         IF (REPORT(I).EQ.'AIRCFT')  THEN
           IF (ITYP .EQ. 230 .OR. ITYP .EQ. 231) CALL
     &       UFBINT(INFILE,WFC,5,1,IRET,VWND)
           IF (ITYP .EQ. 130 .OR. ITYP .EQ. 131) CALL
     &       UFBINT(INFILE,SFC,8,1,IRET,VSTR)
         ENDIF

         DT = SFC(6) - SFC(8)
         TQM= SFC(2)
         IP = IFIX(SFC(3))
         IF (IP.LT.70.OR.IP.GT.500) CYCLE 
                                                                                
C*     Get real observation date and time                                          
                                                                                
         IHR = IFIX(HDR(4))                                                        
                                                                                
C*     Get integer station id                                                    
                                                                                
C*     Write the stnid and differences; then read another report                 
C*     separate outfiles for AIRCRAFT (230) and ASDAR (231)
                                                                                
         FLON = HDR(2)                                                             
         FLAT = HDR(3)                                                             
         ILAT = IFIX((FLAT + 90.0)/5.0 + 1.0 )  
         FLON1= FLON                                                              
         IF (FLON.LT.180.)    THEN 
           FLON1 = FLON + 180.
         ELSE
           FLON1 = FLON - 180.
         ENDIF

         JLON = IFIX((FLON1)/5.0 + 1.0 )
         IDTIM= IFIX(HDR(4)+0.5)                                                   
         IF (HDR(4).LT.0.) IDTIM = IFIX(HDR(4)-0.5)                                 

         IF (REPORT(I).EQ.'AIRCAR')  THEN
           IF (ITYP .EQ. 233)    THEN                                            
             WQM = WFC(3)
             DU = WFC(1) - WFC(4)
             DV = WFC(2) - WFC(5)
             S2OBS = WFC(1)**2 + WFC(2)**2
             S2GES = WFC(4)**2 + WFC(5)**2
             DS = SQRT(S2OBS) - SQRT(S2GES)
             WRITE (IQCACR,1111)  IDATE,CSID,ILAT,JLON,DU,DV,DS,WQM,DT,TQM
           ENDIF                                                           
         ENDIF

         IF (REPORT(I).EQ.'AIRCFT')  THEN
           IF (ITYP .EQ. 230)    THEN
             WQM = WFC(3)
             DU = WFC(1) - WFC(4)
             DV = WFC(2) - WFC(5)
             S2OBS = WFC(1)**2 + WFC(2)**2
             S2GES = WFC(4)**2 + WFC(5)**2
             DS = SQRT(S2OBS) - SQRT(S2GES)
             WRITE (IQCAIR,1111)  IDATE,CSID,ILAT,JLON,DU,DV,DS,WQM,DT,TQM
           ELSE IF (ITYP .EQ. 231)    THEN
             WQM = WFC(3)
             DU = WFC(1) - WFC(4)
             DV = WFC(2) - WFC(5)
             S2OBS = WFC(1)**2 + WFC(2)**2
             S2GES = WFC(4)**2 + WFC(5)**2
             DS = SQRT(S2OBS) - SQRT(S2GES)
             WRITE (IQCASD,1111)  IDATE,CSID,ILAT,JLON,DU,DV,DS,WQM,DT,TQM
           ENDIF
         ENDIF
         ENDIF

         IF (SUBSET.EQ.'SATWND')  THEN

           CALL UFBINT(INFILE,HDR,7,1,IRET,HSTR)
           IRTP = IFIX(HDR(6))
           ITYP = IFIX(HDR(7))
           IF (IRTP.NE.63)   CYCLE
           IF (ABS(HDR(4)).GT.3.00)  CYCLE

           IF (ITYP.EQ.245)    THEN
             CALL UFBINT(INFILE,WFCC,8,1,IRET,VWNDD)
           ENDIF

           IP = IFIX(WFCC(6))
           IPL = 2
           IF (IP.LT.100.OR.IP.GT.1000)  CYCLE
           IF (IP.LE.400)   IPL = 3
           IF (IP.GT.700)   IPL = 1

C*     Get read observation date and time

           IHR = IFIX(HDR(4))

C*     Get integer station id

           FLON = HDR(2)
           FLAT = HDR(3)
           ILAT = IFIX((FLAT + 90.0)/10.0 + 1.0 )
           FLON1= FLON

             IF (FLON.LT.180.)    THEN
               FLON1 = FLON + 180.
             ELSE
               FLON1 = FLON - 180.
             ENDIF
           JLON = IFIX((FLON1)/10.0 + 1.0 )
           IDTIM= IFIX(HDR(4)+0.5)
           IF (HDR(4).LT.0.) IDTIM = IFIX(HDR(4)-0.5)
           IF (ITYP.EQ.245)    THEN
             K1 = K1 + 1
             DU = WFCC(1) - WFCC(4)
             DV = WFCC(2) - WFCC(5)
             UO2 = WFCC(1)**2
             VO2 = WFCC(2)**2
             UG2 = WFCC(4)**2
             VG2 = WFCC(5)**2
             SO = SQRT(UO2+VO2)
             SG = SQRT(UG2+VG2)
             DS = SO - SG
             WRITE (IQCSATW,1112)  IDATE, CSID, IPL, ILAT, JLON, DU,
     &                             DV, DS
           ENDIF
         ENDIF
         ENDDO

C*     Come here when all reports have been read, then the program calls CLOSE file
C*     and quit

         CALL CLOSBF(INFILE)

       ENDDO   
       STOP
       END
