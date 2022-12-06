
      SUBROUTINE PARTEC

C$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
C                .      .    .                                       .  
C SUBPROGRAM:    PARTEC      SET UP AND COMPUTE EC's FORECAST-MINUS-OBS.     
C   PRGMMR: VLCEK            ORG: W/NP12    DATE: 99-07-22 
C                                                                       
C ABSTRACT:  INTERPOLATES ALL VALID EC's FORECAST FIELDS TO LOCATION OF      
C   RADIOSONDES HAVING VALID OBSERVATION VALUES FOR A GIVEN METEORO-    
C   LOGICAL QUANTITY AT A GIVEN LEVEL; COMPUTES (FORECAST - OBSERVED)   
C   ERROR VALUES.  SUMMATION OF ERROR AND ERROR-SQUARED OVER STATIONS   
C   IN A PREDEFINED AREA AND/OR OVER TIME IS ACCOMPLISHED BY CALLING    
C   SUBROUTINE ACCUMU FROM THIS SUBROUTINE, IN PREPARATION FOR COM-     
C   PUTING S1, MEAN ERROR, MEAN STANDARD ERROR, AND ROOT MEAN SQUARE    
C   ERROR, AS WELL AS MEAN AND S.D. OF OBSERVATIONS.                    
C                                                                       
C PROGRAM HISTORY LOG:                                                  
C   76-??-??  STACKPOLE                                                 
C   86-10-16  VLCEK       SELECT RANGE OF WIND SPEEDS TO BE VERIFIED    
C   89-01-25  VLCEK       CONVERT FROM FORTRAN 66 TO VS FORTRAN 77      
C   91-09-??  VLCEK       OVERRIDE AUTO FORECAST MAP TYPE SELECTION     
C                         IF &FILES NAMELIST VARIABLE /SETMAP/ NE.999   
C   94-06-24  VLCEK       IDENTIFY AND READ VSAM FORECAST FILES         
C   96-06-25  Y. ZHANG    CHANGE READING VSAM FORECAST FILES INTO 
C                         READING GRIB FORECAST FILES AND CHANGE THE
C                         SUBROUTINE TO INTERPOLATE FORECASTS FROM 
C                         GRIDPOINTS TO OBSERVATIONAL LOCATIONS BY USING
C                         A BICUBIC OR LINEAR METHOD. A SUBROUTINE ON 
C                         GETTING A PACKED FORECAST VALID DATE&TIME,
C                         NAMED AS GVALDT, HAS BEEN NEWLY CODED AND
C                         ADDED INTO IT. COMMENTS SUBROUTINE ROTWND 
C                         BECAUSE SUBROUTINE IPOLATEV (DO INTERPOLATING)
C                         HAS IN ADVANCE DONE ROTATING WIND DIRECTIONS.
C   96-09-10  Y. ZHANG    MODIFY THE METHOD FOR SPECIALLY READING ECMWF's 
C                         GRIB FORECASTS FILE. BECAUSE ORIGINAL EC's GRIB
C                         INDEX FILE DO NOT PROVIDE THE GRID TYPE MESSAGES,
C                         HERE RESET THE GRID TYPES 2 FOR TEMPERATURE,
C                         HEIGHT AND 254 FOR WIND.
C   98-07-14  VLCEK       COMPILE IN FORTRAN 90 AND MAKE Y2K COMPATIBLE.
C   99-07-22  VLCEK       CONVERT FOR USE ON IBM RS6000.
C                                                                       
C USAGE:    CALL PARTSM                                                 
C   INPUT ARGUMENT LIST:  NONE                                          
C                                                                       
C   OUTPUT ARGUMENT LIST: NONE                                          
C                                                                       
C   INPUT FILES:                                                        
C     FHHHMODL - GRIB FORECAST FILES (SEE DOCUMENTS ON GRIB FILE).
C                                                                       
C   OUTPUT FILES: NONE                                                  
C                                                                       
C                                                                       
C REMARKS: ADP DATA IS BROUGHT IN BY COMMON BLOCK /ADPSTN/; SEE         
C   PROGRAM QCADP OR SUBROUTINE ADPFIL.  
C                                                                       
C ATTRIBUTES:                                                           
C   LANGUAGE: VS FORTRAN 90    IBM FORTRAN                                         
C   MACHINE:  IBM RS6000
C                                                                       
C$$$                                                                    
CC                                                                      
      IMPLICIT  INTEGER(G)

C     /G/ IS RESERVED FOR CONSTANTS NAMED IN A PARAMETER STATEMENT.   
C     LIST AND EXPLANATIONS OF PARAMETER NAMES CAN BE FOUND IN        
C     SUBROUTINE ACCUMU.                                              

      PARAMETER  (GNLEVE=2)
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
      PARAMETER  (GMXARA=15, GMXRUN=15)

      COMMON /ADPSTN/ IDSTN(GMXSTN)    , ALON(GMXSTN)  , ALAT(GMXSTN) ,
     &                NRPT(GMXSTN)     , DISTNN(GMXSTN),
     &                ZTRUV(GMXSTN,GNVARP,GNLEV),
     &                STI(GMXSTN), STJ(GMXSTN)  , JSTA
      COMMON /MAPINF/ NSTA, KMAP , IMXMAP       , JMXMAP,
     &                NCYCMP     , F9, DATENL
      COMMON /CFDATE/ IAPPCK     , IAPUNP(4)
      COMMON /AREAS / ALOLA(GMXARA,4)  , AREA(GMXARA)  ,
     &                KSTN(GMXARA)     , STNLST(GMXARA , GMXTBL)
      COMMON /RAFILE/ FORFIL(GORFIL)   , NFILE         , GCODE ,
     &                DATA(GMXDIM)     , DATA1(GMXDIM) , MINWND,
     &                MAXWND, IGRID    , VDATE1, VDATE2,
     &                VDATE , DIAG     , PRSTAT, WRSTAT,
     &                INTPM , KRUN     , FHR(GORFIL)
      COMMON /LEVELS/ PLEV(GNLEV)      , IPL(GNLEV)    , QWANT(GNVARP)
      COMMON /STNPRT/ STNERR(GMXSTN,GNERQU,GNLEV),
     &                STNGRD(GMXSTN,GNGRQU,GNLEV)
      COMMON /LOCAL / STVL(GMXSTN) , STV1(GMXSTN),
     &                ZTRUVL(GMXSTN),ZTRUV1(GMXSTN)
      COMMON /RADTAB/ RUNTAB(GMXRUN)   , ARATAB(GMXARA,GMXRUN) ,
     &                                   FHRTAB(GORFIL,GMXRUN) ,
     &                                   INDARA(GMXARA,GMXRUN) ,
     &                DOSTAT(GMXARA    , GORFIL,GMXRUN)
      COMMON /UNITS / IUAREA, IUADP    , IUGBD(GORFIL) , IUGBI(GORFIL) ,
     &                IUJET , IUSTA    , IUOUT  , IUOPN, IUTAB

      REAL         MINWND, MAXWND, ALOLA
      INTEGER      VDATE1, VDATE2, VDATE , RPT, GCODE, IGRID , STNLST
      INTEGER      QWANT , NFILE , IFCDAT(4), IPP(GNLEV), NHR, PLEV
      INTEGER      JPDS(25)  , JGDS(22)  , KPDSI(25) , KGDSI(22)
      INTEGER      KGDSO(22) , IBI(GNLEV), IBO(GNLEV), IPOPT(20)

      CHARACTER(10) CHQ(GNVARP)
      CHARACTER(8)  FORFIL, AREA  , BLANK , ARATAB, C8GRBD, C8GRBI
      CHARACTER(4)  FHR   , KRUN  , RUNTAB, FHRTAB, BLANK4, FHRNF
      CHARACTER(4)  GRBD(2), GRBI(2) 
      CHARACTER(3)  CHR
      CHARACTER(1)  INDARA, BLANK1, DOSTAT, CFHR(4)
      LOGICAL       PRSTAT, WRSTAT, DIAG  , VECTOR, FIRST
      LOGICAL(1)    LI(GMXDIM)    , LO(GMXSTN)    , LHLE

      DATA  GRBD /'    ','DAT'/, GRBI /'    ','NDX'/
      DATA  BLANK/'        '/    , BLANK4 /'    '/     , BLANK1/' '/
      DATA  CHQ /'HEIGHT','TEMP.','REL. HUM.','U-COMP.','V-COMP.'/
      DATA  ESP /1.E-8/

      EQUIVALENCE (CFHR(1),FHRNF), (CFHR(2),CHR)
      EQUIVALENCE (GRBD(1),C8GRBD), (GRBI(1),C8GRBI)


      IPP = PLEV
      F9X = F9 - 1.

      IF(DIAG) PRINT 9900

C     CHECK IF THE RUN TYPE CAN BE PROCESSED

      DO IR=1,GMXRUN
        IF (RUNTAB(IR).NE.BLANK4) THEN
           MKR = IR
           IF (KRUN.EQ.RUNTAB(IR)) GO TO 4
        END IF
      END DO
      PRINT *,'RUN TYPE = ',KRUN
      CALL EREXIT(1)
   4  CONTINUE

C     OUTERMOST DO LOOP IS OVER THE 
C     VARIOUS FORECAST FILES TO BE VERIFIED                          

      DO 170 NF = 1, NFILE
   
      GRBD(1) = FHR(NF)
      GRBI(1) = FHR(NF)
      LUGDNF = IUGBD(NF)
      LUGINF = IUGBI(NF)
      CALL BAOPENR(LUGDNF,C8GRBD,IRET)
      CALL BAOPENR(LUGINF,C8GRBI,IRET)

      FHRNF= FHR(NF)
      READ(CHR,'(I3)') NHR

      FIRST  = .TRUE.

C     FIRST SET ERROR (AND GRADIENT) AREAS TO ALL MISSING          
C     IN CASE WE CANT FIND A FIELD OR A STATION IS MISSING         
C     OR SOMETHING                                                 

      STNERR = F9
      STNGRD = F9

C     INITIALIZE THE ARRAY USED IN GETGB AND IPOLATES&IPOLATEV

      KPDSI = 0
      KGDSI = 0
      IBI   = 0
      IBO   = 0
      DO I  = 1,GMXDIM
      LI(I) = .FALSE.
      END DO

C     NEXT DO IS OVER DESIRED LEVELS (850 AND 500 MB ONLY)         
C     CONSTRUCT LEVEL DEFINING PORTION OF LABEL                    

      DO 160 K=1,GNLEVE

C     NEXT DO IS OVER DESIRED VARIABLES                          
C     CONSTRUCT VARIABLE IDENTIFIERS, REPACK LABEL AND READ      

      DO 150 NV = 1,GNVAR

      REWIND LUGDNF
      REWIND LUGINF

      DATA  = 0.
      DATA1 = 0.

C     READ BACKGROUND FIELDS -- SKIP UNAVAILABLE FIELDS
C     ONLY 500 MB HEIGHTS AND 850 MB TEMPS AND WINDS ARE AVAILABLE.
C     WINDS ONLY BETWEEN 35N AND 35S FOR F000 THRU F072,
C     NO WINDS BEYOND F072 BUT NOT SKIPPED.

      IF(NV.EQ.3) GO TO 150
      IF(NV.EQ.1.AND.K.EQ.1) GO TO 150
      IF(NV.EQ.2.AND.K.EQ.2) GO TO 150
      IF(NV.EQ.4.AND.K.EQ.2) GO TO 150

      NRK     = 1
   51 CONTINUE

      JPDS    = -1
      JGDS    = -1
      JPDS(5) = QWANT(NV)
      JPDS(6) = 100
      JPDS(7) = IPP(K)
      JK      = NRK - 1
      KR      = 0
      CALL GETGB(LUGDNF,LUGINF,GMXDIM,JK,JPDS,JGDS,KI,KR,
     &           KPDSI,KGDSI,LI,DATA,IRET)
      LHLE = KPDSI(14).EQ.NHR .AND.
     &       KPDSI( 7).EQ.IPP(K) .AND. KPDSI(5).EQ.QWANT(NV)
      IF (IRET.EQ.0) THEN
        IF(LHLE) THEN
          GO TO 52
        ELSE
          NRK = NRK + 1
          GO TO 51
        END IF
      ELSE
        PRINT 9990
        PRINT 9980, IRET, FORFIL(NF),CHQ(NV),IPP(K)
        GO TO 140
      ENDIF

   52 CONTINUE

      GCODE     = KPDSI(2)
      IGRID     = 2
      IF (NV.EQ.GVECTV) IGRID = 254
      IF (FIRST) THEN
        IFCDAT(1) = KPDSI(8)  + 100*(KPDSI(21)-1)
        IFCDAT(2) = KPDSI(9)
        IFCDAT(3) = KPDSI(10)
        IFCDAT(4) = KPDSI(11)
        IFCHR     = KPDSI(14)
        NUNIT     = KPDSI(13)
        IF (NUNIT.EQ.0)  IFCHR = IFIX(IFCHR/60 + 0.5)
        IF (NUNIT.EQ.1)  IFCHR = IFCHR
        IF (NUNIT.EQ.2)  IFCHR = IFCHR*24
        IF (NUNIT.EQ.10) IFCHR = IFCHR*3
        IF (NUNIT.EQ.11) IFCHR = IFCHR*6
        IF (NUNIT.EQ.12) IFCHR = IFCHR*12
        IF (NUNIT.NE.0.AND.NUNIT.NE.1.AND.NUNIT.NE.2.AND.
     &      NUNIT.NE.10.AND.NUNIT.NE.11.AND.NUNIT.NE.12) GO TO 170
        IF (KRUN.EQ.'PERS') IFCHR = INTFHR(FHR(NF))
        CALL GVALDT(IFCDAT, IFCHR, IFCPCK)
        IF (IFCPCK.EQ.0) THEN
          PRINT *,'DATE OF FORECAST FIELD FOR VAR. ',NV,
     &            '  AT LEVEL ',K,' IS INCORRECT.'
          GO TO 170
        END IF

C       TEST FOR AGREEMENT OF ADP AND BACKGROUND DATES

        IF (DIAG) PRINT 7, IFCPCK, IAPPCK
    7   FORMAT (T15,'PACKED DATES (HEX) (IN PARTSM): FCST, ADP ',
     &           2X,Z10,2X,Z10//)
        IF (IFCPCK.EQ.IAPPCK) GO TO 6
        PRINT 5
    5   FORMAT (T10, 'BUT THIS DATE DOES NOT',
     &               ' AGREE WITH THE ADP DATE - DISASTER, I QUIT...')
        GO TO 170
    6   CONTINUE
        FIRST  = .FALSE.
      END IF

C     SEE IF FIXED MAP TYPE IS DESIRED AND NEWMAP REQUIRED           

      IF (IGRID.NE.KMAP) THEN
        KMAP = IGRID
        CALL STIJ
      ENDIF
      IF (NV.NE.GVECTV) THEN
        IBI(K)  = MOD(KPDSI(4)/64,2)
        VECTOR = .FALSE.
        GO TO 55
      ELSE
        VECTOR  = .TRUE.

   53   CONTINUE
        JPDS    = -1
        JGDS    = -1
        JPDS(5) = QWANT(NV+1)
        JPDS(6) = 100
        JPDS(7) = IPP(K)
        NRK = NRK + 1
        JK      = NRK - 1
        KR      = 0
        CALL GETGB(LUGDNF,LUGINF,GMXDIM,JK,JPDS,JGDS,KI,KR,
     &             KPDSI,KGDSI,LI,DATA1,IRET)
        LHLE = KPDSI(14).EQ.NHR .AND.
     &         KPDSI( 7).EQ.IPP(K) .AND. KPDSI(5).EQ.QWANT(NV+1)
        IF (IRET.EQ.0) THEN
          IF (LHLE) THEN
            GO TO 54
          ELSE
            GO TO 53
          END IF
        ELSE
          PRINT 9990
          PRINT 9980, IRET, FORFIL(NF),CHQ(NV+1),IPP(K)
          GO TO 140
        ENDIF
   54   CONTINUE
        IBI(K)  = MOD(KPDSI(4)/64,2)
      END IF
   55 CONTINUE

C     NEXT DO IS OVER ALL ADP STATIONS FOR PURPOSE             
C     OF INTERPOLATING FIELD VALUES TO STATIONS.               
C     ALSO SAVE STATION OBSERVATIONS;                          
C     SKIP STATIONS OUTSIDE DATA GRID                          
C     AND SET THEIR DATA VALUES MISSING.                       

      IP = 1
      IF (INTPM.NE.999) IP = INTPM
      KGDSO = -1
      IPOPT = 0

      DO 60 NS = 1,JSTA
        STVL(NS) = F9
        ZTRUVL(NS) = F9
        IF (VECTOR) THEN
          STV1(NS)   = F9
          ZTRUV1(NS) = F9
        ENDIF
        IF(STI(NS).LT.F9X) THEN
          AALAT = ALAT(NS)
          AALON = ALON(NS)
          IF (AALON.GT.180.) AALON = AALON-360.
          IF (.NOT.VECTOR) THEN
            CALL IPOLATES
     &      (IP,IPOPT,KGDSI,KGDSO,GMXDIM,1,1,IBI(K),LI,DATA,
     &       1,AALAT,AALON,IBO(K),LO,STVL(NS),IRET)
            ZTRUVL(NS) = ZTRUV(NS,NV,K)
          ELSE
            SROT = 0.
            CROT = 1.
            CALL IPOLATEV
     &      (IP,IPOPT,KGDSI,KGDSO,GMXDIM,1,1,IBI(K),LI,DATA,DATA1,
     &      1,AALAT,AALON,CROT,SROT,IBO(K),LO,STVL(NS),
     &      STV1(NS),IRET)
            ZTRUVL(NS) = ZTRUV(NS,NV,K)
            ZTRUV1(NS) = ZTRUV(NS,NV+1,K)
          ENDIF
        ENDIF
   60 CONTINUE

C     ROTATE WINDS, IF APPROPRIATE                             
C     AND SAVE STATION VALUES                                  

CZ      IF(VECTOR) CALL ROTWND(STVL,STV1)                    

C     DO AGAIN OVER STATIONS, THIS TIME TO                     
C     COMPUTE THE ERROR TERMS;                                 
C     VECTOR/SCALAR DONE SEPARATELY                            

      LNV = NV*2 - 1
      IF (.NOT.VECTOR) THEN
        DO 100 NS=1,JSTA
          X = ZTRUVL(NS)
          IF (STI(NS).LT.F9X.AND.STJ(NS).LT.F9X
     &       .AND.ABS(X).LT.F9X) THEN
            IF (DISTNN(NS).LT.ESP) GO TO 100
            Y = STVL(NS) - X
            STNERR(NS,LNV,K)   = Y
            STNERR(NS,LNV+1,K) = X
            INP = NRPT(NS)
            IF (INP.GT.0) THEN
              XNP   = ZTRUVL(INP)
              STINP = STVL(INP)
              IF (ABS(XNP).LT.F9X.AND.ABS(STINP).LT.F9X) THEN
                DI                 = 1./DISTNN(NS)
                STNGRD(NS,LNV,K)   = (STINP - STVL(NS)) * DI
                STNGRD(NS,LNV+1,K) = (XNP-X) * DI
              ENDIF
            ENDIF
          ENDIF
  100   CONTINUE
      ELSE
        DO 120 NS=1,JSTA
          IF (STI(NS).LT.F9X.AND.STJ(NS).LT.F9X) THEN
            UO = ZTRUVL(NS)
            VO = ZTRUV1(NS)
            IF (ABS(UO).LT.F9X.AND.ABS(VO).LT.F9X) THEN
              UF = STVL(NS)
              VF = STV1(NS)
              SO = SQRT(UO**2 + VO**2)
              SF = SQRT(UF**2 + VF**2)
              IF ((SO.GE.MINWND.AND.SO.LE.MAXWND).OR.
     &          (SF.GE.MINWND.AND.SF.LE.MAXWND)) THEN
                SE                 = SF - SO
                VECTE              = (UF-UO)**2 + (VF-VO)**2
                STNERR(NS,LNV,K)   = SE
                STNERR(NS,LNV+1,K) = SO
                STNERR(NS,LNV+2,K) = SQRT(VECTE)
                STNERR(NS,LNV+3,K) = SO
              ENDIF
            ENDIF
          ENDIF
  120   CONTINUE
      ENDIF
  140 CONTINUE
  150 CONTINUE
  160 CONTINUE

C     CALL TO SUB TO READ AREA ACCUMULATION                        
C     BLOCKS TO FIND STATIONS, ACCUMULATE PARTIALS ETC             

      CALL ACCUMU(NF,MKR)

      CALL BACLOSE (LUGDNF,IRET)
      CALL BACLOSE (LUGINF,IRET)

  170 CONTINUE
 9900 FORMAT(T15,'BEGIN SUBROUTINE PARTSM ....')
 9990 FORMAT(' TROUBLE IN PARTSM...')
 9980 FORMAT(' K03 ERROR NO. ', I3,
     &       ' SEARCHING FILE WITH DDNAME ',A8,
     &       ' FOR ',A10,' ON ',I4,'MB' /)
 9960    FORMAT(' FILE WITH DDNAME ',A8,
     &          ' HAS NO FORECASTS THAT VERIFY',
     &          ' AT THE ADP DATE/TIME.  CHECK YOUR JCL.')
 9970    FORMAT(/T10,'IDTABLE(1-8)...', 8Z10/
     &           T10,'LAST FORECAST TAU READ...',I5,
     &           '  UPDATED DATE AND ADP DATE...', 2Z10)

      RETURN
      END
