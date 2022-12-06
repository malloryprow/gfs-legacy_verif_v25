
      SUBROUTINE NAMSTN (IDTYPR,STATN,NSTATN,ALAT,ALONW)

C$$$  SUBPROGRAM DOCUMENTATION BLOCK                                 
C                .      .    .                            .          
C SUBPROGRAM:    NAMSTN      CREATE BLOCK AND STATION IDENT        
C   PRGMMR: STACKPOLE        ORG: W/NP12    DATE: 96-05-06
C                                                                
C ABSTRACT:  THIS SUBROUTINE RENAMES STATIONS FOR SHIP REPORTS  
C   AND LAND STATIONS NOT GIVEN BY A BLOCK AND STATION NUMBER.        
C   ID IS CONVERTED FROM CHARACTER  TO INTEGER FORM.                 
C                                                                   
C PROGRAM HISTORY LOG:                                             
C   76-??-??  STACKPOLE                                           
C   89-01-25  VLCEK       CONVERT FROM FORTRAN 66 TO VS FORTRAN 77  
C   96-05-06  Y. ZHANG    MOVE FROM HDS TO CRAY AND REFINE
C                         CONSIDER THE REAL VALUE OF ALONW AS 
C                         EAST LONGITUDE AS THAT OF ORIGIONAL
C                         BUFR REPORTS.
C   98-07-29  VLCEK       COMPILE IN F90 (NO CODE CHANGES)
C                                                                  
C USAGE:    CALL NAMSTN(IDTYPR, STATN, NSTATN, ALAT, ALONW)       
C   INPUT ARGUMENT LIST:                                         
C     IDTYPR   - TWO-DIGIT CODE FOR REPORT TYPE.                
C     STATN    - ORIGINAL STATION NAME (CHARACTER), WHERE APPLICABLE.     
C     ALAT     - LATITUDE OF REPORTING STATION (DEGREES).             
C     ALONW    - EAST LONGITUDE OF REPORTING STATION (DEGREES).      
C                                                                   
C   OUTPUT ARGUMENT LIST:                                          
C     NSTATN   - NEW STATION I.D. NUMBER (INTEGER),               
C                OR OLD NUMBER CONVERTED TO INTEGER.             
C                                                               
C REMARKS: SEE COMMENTS BELOW FOR RENAMING PROCEDURES.         
C          32 STATEMENTS, 952 BYTES.                          
C                                                            
C ATTRIBUTES:                                               
C   LANGUAGE: VS FORTRAN 90, CRAY FORTRAN                                
C   MACHINE:  CRAY
C                                                        
C$$$                                                    

      IMPLICIT INTEGER(G)

      PARAMETER (GMINBN=0, GMAXBN=99999)

      CHARACTER(8)  STATN, STATNX
      CHARACTER(1)  LET(8)
      EQUIVALENCE   (LET(1),STATNX)

      IF (IDTYPR.NE.11)    GO TO 10

C     CONVERT CHARACTER TO INTEGER FOR BLOCK AND STATION NO. REP.   

      READ (UNIT=STATN,FMT=1,ERR=30,END=30)  NSTATN
    1 FORMAT (I5)

C     TEST IF STATION IS BEYOND LIMITS OF DESIRED BLOCK AND STATION NUMBER                             

      IF(NSTATN.LT.GMINBN.OR.NSTATN.GT.GMAXBN) NSTATN = 0
      RETURN
   10 CONTINUE

      IF (IDTYPR.NE.21)   GO TO 20

C     GIVE OSV ASPECIAL NUMBER :                  
C     BLOCK NUMBER = 99  (UNUSED IN ADPUPA)      
C     STATION = DECIMAL EQUIVALENT OF EBCDIC CHARACTER            
C     OF SHIPS CALL LETTER (THIRD CHARACTER IN NAME)             
C     E.G. SHIP C7H COMES OUT 99200                             

      STATNX = STATN
      NSTATN = 99000 + MOVA2I(LET(3))
      RETURN
   20 CONTINUE

C     RENAME STATION ID FOR FIXED OR MOVING STATION.       

      IREPTP = 0

C     FOR FIXED LAND OF SHIP NOT ORIGINALLY DENOTED BY BLOCK AND
C     STATION NUMBER.

      IF (IDTYPR.EQ.12.OR.IDTYPR.EQ.13)   IREPTP = 100

C     FOR MOVING SHIPS.                                       

      IF (IDTYPR.EQ.22.OR. IDTYPR.EQ.23)  IREPTP = 300

      IF (IREPTP.EQ.0)   GO TO 30
      IF (ALAT.LT.0)     IREPTP = IREPTP + 100

C     FOR NORTHERN HEMISPHERE 0 TO 90N LAT, BLOCK=100 FOR FIXED  
C     SHIPS.  300 FOR MOVING SHIPS. FOR SOUTHERN HEMISPHERE, -1 TO -90S LAT.                
C     BLOCK = 200 FOR FIXED SHIPS, 400 FOR MOVING SHIPS.     

      ILAT   = ABS(ALAT) + 0.51
      ILON   = ALONW + 0.51
      IBLOCK = IREPTP
      ISTN   = ILAT*1000 + ILON
      NSTATN = IBLOCK*1000 + ISTN
      RETURN

C     SKIP THIS REPORT, NO NAME CAN BE DETERMINED.                 

   30 CONTINUE
      NSTATN = 0
      RETURN
      END
