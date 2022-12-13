
      SUBROUTINE GETADP

C$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
C                .      .    .                                       .  
C SUBPROGRAM:    GETADP      READ AND SELECT DATA FROM ADPUPA           
C   PRGMMR: VLCEK            ORG: WX12      DATE: 99-06-18             
C                                                                       
C ABSTRACT:  THIS SUBROUTINE READS AN ADP FILE AND COMBINES THE         
C   DESIRED QUANTITIES AT THE SPECIFIED PRESSURE LEVEL(S) OR            
C   SURFACE INTO DISORDERLY 'FIELDS', THE ONLY COMMON RELATION-         
C   SHIP SHARED AMONG ALL DATA 'POINTS' BEING THEIR LEVEL.              
C                                                                       
C PROGRAM HISTORY LOG:                                                  
C   76-??-??  STACKPOLE                                                 
C   86-10-16  VLCEK       STORE INPUT IN CORE RATHER THAN WORK FILES    
C   89-01-25  VLCEK       CONVERT FROM FORTRAN 66 TO VS FORTRAN 77      
C   96-05-06  Y. ZHANG    RECODE THE PROGRAM AND MAKE IT READ DATA
C                         FROM PREPBUFR FILE, KEEP ALL OF FUNCTIONS
C                         OLD CODE HAD AND COMPRESS THE SIZE OF CODES,
C                         REMOVE THE SUBROUTINE SCALE IN OLD CODE.
C                         MAKE IT RUN ON CRAY. 
C   98-07-10  VLCEK       MAKE Y2K COMPATIBLE, COMPILE IN FORTRAN 90
C   99-04-29  VLCEK       USE 4-DIGIT YEAR FOR NCEP DATE ENTRY
C   99-06-18  VLCEK       COMPILE ON IBM RS6000.
C                                                                       
C USAGE:    CALL GETADP                                                 
C   INPUT ARGUMENT LIST:  NONE                                          
C                                                                       
C   OUTPUT ARGUMENT LIST:  NONE                                         
C                                                                       
C   INPUT FILES:                                                        
C     FT20F001 - PREPBUFR file
C                                                                       
C   OUTPUT FILES:                                                       
C     FT06F001 - ADPUPA LABEL AND DATE; WARNING AND ERROR MESSAGES.     
C                                                                       
C REMARKS: DATA IS PASSED IN COMMON.                                    
C                                                                       
C ATTRIBUTES:                                                           
C   LANGUAGE: FORTRAN 90, IBM FORTRAN 
C   MACHINE:  IBM RS6000
C                                                                       
C$$$                                                                    

      IMPLICIT  INTEGER(G)

C     /G/ IS RESERVED FOR CONSTANTS NAMED IN A PARAMETER STATEMENT   
C     LIST AND EXPLANATIONS OF NAMES ARE IN SUBROUTINE ACCUMU        

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

      PARAMETER (NBHR=6,NVARL1=6,NSTP1=6,NVARL2=4,NSTP2=1)
      PARAMETER (L1RPT1=NBHR+GNLEV*NVARL1,LBUF1=L1RPT1*GMXSTN)
      PARAMETER (L1RPT2=NBHR+GNLEV*NVARL2,LBUF2=L1RPT2*GMXSTN)
      PARAMETER (MBHR=7,MLEV=255,MVARL1=4,MVARL2=3,MELM=3,MPC=10)
      PARAMETER (FLON1=0.,FLAT1=-90.,FLON2=360.,FLAT2=90.)

      COMMON /CFDATE/ IAPPCK        , IAPUNP(4)
      COMMON /ADPSTN/ IDSTN(GMXSTN) , ALON(GMXSTN)   , ALAT(GMXSTN),
     &                NRPT(GMXSTN)  , DISTNN(GMXSTN) ,
     &                ZTRUV(GMXSTN,GNVARP,GNLEV)     ,
     &                STI(GMXSTN) , STJ(GMXSTN), JSTA
      COMMON /MAPINF/ NSTA  , KMAP , IMXMAP , JMXMAP ,
     &                NCYCMP, F9   , DATENL
      COMMON /LEVELS/ PLEV(GNLEV)  , IPL(GNLEV)    , QWANT(GNVARP)
C     COMMON /LLUNIT/ LULST, LUBFI, LUGBD, LUGBI, LUTOS, LUADP, LUOUT

      REAL      BUF1(LBUF1),BUF2(LBUF2),FQM(3,2),HDR(MBHR),CAT(MLEV)
      REAL      EVN1(MVARL1,MLEV,MPC,MELM),EVN2(MVARL2,MLEV,MPC,MELM)
      REAL      OBS1(MVARL1,MLEV,MPC,MELM-1)
      REAL      OBS2(MVARL2,MLEV,MPC,MELM-1)
      REAL      ZZZ(GNVARP,GNLEV)

      INTEGER   IP(GNLEV)
      INTEGER   QWANT, GETARG, PLEV
      INTEGER   LULST, LUBFI, LUGBD, LUGBI, LUTOS, LUADP, LUOUT

      CHARACTER(80) SUCCESS(3), WARNING(3) , HDSTR , OBSTR(2,MELM)
      CHARACTER(8)  SUBSET    , EVNTYP(MPC), SID

      EQUIVALENCE  (HDR(1),SID)

      DATA LULST /9/, LUBFI /10/, LUGBD /11/, LUGBI /12/
      DATA LUADP /60/, LUTOS /70/, LUOUT /6/
      DATA SUCCESS/'************************************************',
     .             '*************** GETADP SUCCESSFUL **************',
     .             '************************************************'/
      DATA WARNING/'************************************************',
     .             '! WARNING: NOT ALL OBS CAN BE SELECTED :WARNING !',
     .             '************************************************'/
      DATA HDSTR /'SID XOB YOB DHR ELV T29 TYP    '/
      DATA OBSTR /'POB TOB ZOB QOB' , 'POB UOB VOB',
     .            'PQM TQM ZQM QQM' , 'PQM WQM WQM',
     .            'PPC TPC ZPC QPC' , 'PPC WPC WPC'/
      DATA FQM   / 7.,7.,7.,7.,7.,0./
      DATA T0    / 273.15 /
      DATA VMAX  / 10E10  /
      DATA EMIS  / -1.    /

C######################################
CCC    CHARACTER(8)   CCC, clock_

CCC    REAL(4)  ELASPSED, ELAS1,  etime_
CCC    TYPE TB_TYPE
CCC       SEQUENCE
CCC       REAL(4) USRTIME
CCC       REAL(4) SYSTIME
CCC    END TYPE
CCC    TYPE (TB_TYPE) ETIME_STRUCT
CCC   

CCC      CCC = clock_()
CCC      PRINT *, 'GETADP:  CCC = ', CCC

CCC      ELAPSED = etime_(ETIME_STRUCT)

CCC      PRINT *, 'GETADP:  ELAPSED = ', ELAPSED
C############################################################
 
C     INITIALIZE

      JSTA     = 0
      NREPT    = 0
      NREPS1   = 0
      NREPS2   = 0
      LL1      = 0
      LL2      = 0
      MDATE    = 10**8 
      NDATE    = 10**9 
      F9       = 99999.

C     99999. INDICATES MISSING DATA                                    

      IP = PLEV
      BUF1  = F9
      BUF2  = F9
      ALON  = F9
      ALAT  = F9
      IDSTN = F9
      ZTRUV = F9

C     GET THE DATE OF THE REPORTS TO BE PROCESSED
C     CDATIM IS YMDH PORTION OF NCEP DATE OBTAINED
C     FROM SCRIPT.  FOR NOW, AT LEAST, KEEP IT AS IS
C     AND ASSUME SAME CENTURY AS ADP DATA (DEFAULT 19).

      READ (8,88) JDATE
   88 FORMAT (I10)
      IAPUNP(1) = JDATE/1000000
      IAPUNP(2) = MOD(JDATE,1000000)/10000
      IAPUNP(3) = MOD(JDATE,10000)/100
      IAPUNP(4) = MOD(JDATE,100)

C     OPEN THE DATA FILE AND CHECK THE DATE
C     LOOP THROUGH THE INPUT MESSAGES - READ THE NEXT SUBSET
    
      CALL DATELEN(10)

      PRINT*
      PRINT*, '**********  READING DATA IN GETADP  **********'
      CALL OPENBF(LUBFI,'IN',LUBFI)
 
C      READ AND LOCATE FIRST ADPUPA SUBSET

    1 CONTINUE
        CALL READMG(LUBFI,SUBSET,IDATE,IRET)
        IF (SUBSET.NE.'ADPUPA')    THEN
          IF (IRET.NE.0)    THEN
            GO TO 300
          ELSE
            GO TO 1
          ENDIF
        ENDIF
        PRINT*, '**********  READING DATA IN GETADP  **********'
        PRINT*
C       NCEN  = IDATE/MDATE
C       JDATE = JDATE + NCEN*MDATE
C       IAPUNP(1) = IAPUNP(1) + 100*NCEN
        IAPPCK    = JDATE
        PRINT *
        PRINT 6666, IAPPCK,IAPUNP
 6666   FORMAT (10X, 'PACKED (HEX) AND UNPACKED DATE',
     &        ' (IN GETADP)  (Y,M,D,IH) ',Z10, 4I5)
        IF (IDATE.NE.JDATE) GO TO 901
 
C      READ AND TEST FOR SUBSEQUENT ADPUPA SUBSETS

    2   CONTINUE
          CALL READSB(LUBFI,IRET)
          IF (IRET.NE.0) THEN
    3       CONTINUE
              CALL READMG(LUBFI,SUBSET,IDATE,IRET)
              IF (SUBSET.NE.'ADPUPA')    THEN
                IF(IRET.EQ.0) THEN
                  GO TO 3
                ELSE
                  GO TO 300
                END IF
              END IF
            GO TO 2
          END IF

C         INITIALIZE WORKING ARRAY AT FIRST OF PROCESSING EACH REPORT

          OBS1 = VMAX
          OBS2 = VMAX

C         READ THE REPORT HEADER

          CALL UFBINT(LUBFI,HDR,MBHR,  1,IRET,HDSTR)
          CALL UFBCNT(LUBFI,IREC,IFLD)

C         SELECT THE TYPES OF REPORTS REQUIRED

          IKX = HDR(6)
          JKX = HDR(7)
          XOB = HDR(2)
          YOB = HDR(3)
          NREPT = NREPT+1
        IF(IKX.NE.11.AND.IKX.NE.12.AND.IKX.NE.13.AND.
     *    IKX.NE.21.AND.IKX.NE.22.AND.IKX.NE.23) GO TO 2
        IF(JKX.LT.120.OR.(JKX.GT.123.AND.JKX.LT.220).OR.
     *    JKX.GT.223) GO TO 2
        IF(XOB.LT.FLON1.OR.XOB.GT.FLON2.OR.
     *    YOB.LT.FLAT1.OR.YOB.GT.FLAT2) GO TO 2

C     CHECK REPORT TYPE, CREATE A NEW STATION ID FOR MOVING AND FIXED SHIPS

          IISID = 0
          CALL NAMSTN (IKX, SID, IISID,YOB,XOB)
        IF (IISID.EQ.0)    GO TO 2

C         EXTRACT THE CATEGORY ON REPORTING LEVELS

          CALL UFBINT(LUBFI,CAT,1,MLEV,KLEV,'CAT')

C         EXTRACT THE REPORTING MESSAGES REQUIRED

          JKX=IFIX(HDR(7))/100
          DO I=1,MELM
            IF(JKX.EQ.1) THEN
              CALL UFBEVN
     *        (LUBFI,EVN1(1,1,1,I),MVARL1,MLEV,MPC,KLEV,OBSTR(JKX,I))
            ELSE IF(JKX.EQ.2) THEN
              CALL UFBEVN
     *        (LUBFI,EVN2(1,1,1,I),MVARL2,MLEV,MPC,KLEV,OBSTR(JKX,I))
            END IF
          END DO

          JKX=IFIX(HDR(7))/100
          IF(JKX.EQ.1) THEN

C           HERE PROCESS THE REPORTS ON TEMPERATURE AND HEIGHT

            DO K=1,MPC
              DO J=1,KLEV
                DO I=1,MVARL1
                  OBV = EVN1(I,J,K,1)
                  IQM = EVN1(I,J,K,2)
                  IPC = EVN1(I,J,K,3)
                  IF(OBV.LT.VMAX) THEN
                    IF(IPC.GE.IFIX(VMAX)-1) IPC = 1
                    OBS1(I,J,IPC,1) = OBV
                    OBS1(I,J,IPC,2) = IQM
                  END IF
                END DO
              END DO
            END DO

C           STORE THE HEADER AND SELECT THE LEVELS OF REPORTS REQUIRED

            MARK = 0
            BUF1(LL1+1) = IISID
            DO I = 2,NBHR
              BUF1(LL1+I) = HDR(I)
            END DO
            DO 20  LLEV=1,GNLEV
            IL1 = LL1+NBHR+(LLEV-1)*NVARL1
            IPLEV = IP(LLEV)
            DO 15 LEV=1,KLEV
              IIP=OBS1(1,LEV,1,1)+0.1
              ICAT=CAT(LEV)+0.1
              IF(ICAT.NE.1) GO TO 15
              IF(OBS1(1,LEV,1,1).GT.VMAX-1.) GO TO 15
              IF(IIP.LT.IPLEV) GO TO 16
              IF(IIP.EQ.IPLEV) THEN
                DO KT=1,MELM-1
                  MARKTT=0
                  MARKZZ=0
                  MARKRH=0
                  DO IT=NSTP1,1,-1
                    IF(MARKTT.EQ.0) THEN
                      IF(OBS1(2,LEV,IT,KT).LT.VMAX) THEN
                        BUF1(IL1+1)  = OBS1(2,LEV,IT,KT)
                        MARKTT=1
                      END IF
                    END IF
                    IF(MARKZZ.EQ.0) THEN
                      IF(OBS1(3,LEV,IT,KT).LT.VMAX) THEN
                        BUF1(IL1+2)  = OBS1(3,LEV,IT,KT)
                        MARKZZ=1
                      END IF
                    END IF
                    IF(MARKRH.EQ.0) THEN
                      IF(OBS1(4,LEV,IT,KT).LT.VMAX) THEN
                        BUF1(IL1+3)  = OBS1(4,LEV,IT,KT)
                        MARKRH=1
                      END IF
                    END IF
                    IF(MARKTT.EQ.1.AND.MARKZZ.EQ.1.AND.MARKRH.EQ.1)              
     *                                                   GO TO 14
                  END DO
14                CONTINUE
                  IL1 = IL1+3
                END DO
                GO TO 16
              END IF
15          CONTINUE
16          CONTINUE
            IBIG = LL1+NBHR+(LLEV-1)*NVARL1+1
            IEND = LL1+NBHR+(LLEV-1)*NVARL1+3
            DO IL = IBIG,IEND
              IF(BUF1(IL).LT.F9) THEN
                MARK = 1
                GO TO 19
              END IF
            END DO
19          CONTINUE
20          CONTINUE
            IF(MARK.EQ.0) THEN
              LL1 = LL1
            ELSE IF(MARK.EQ.1) THEN
              LL1 = LL1+L1RPT1
              NREPS1 = NREPS1+1
            END IF
            ELSE IF(JKX.EQ.2) THEN

C           HERE PROCESS THE REPORTS ON WIND COMPONENTS

            DO K=1,MPC
              DO J=1,KLEV
                DO I=1,MVARL2
                  OBV = EVN2(I,J,K,1)
                  IQM = EVN2(I,J,K,2)
                  IPC = EVN2(I,J,K,3)
                  IF(OBV.LT.VMAX) THEN
                    IF(IPC.GE.IFIX(VMAX)-1) IPC = 1
                    OBS2(I,J,IPC,1) = OBV
                    OBS2(I,J,IPC,2) = IQM
                  END IF
                END DO
              END DO
            END DO

C           STORE THE HEADER AND SELECT THE LEVELS OF REPORTS REQUIRED

            MARK = 0
            BUF2(LL2+1) = IISID
            DO I = 2,NBHR
              BUF2(LL2+I) = HDR(I)
            END DO
            DO 30  LLEV=1,GNLEV
              IL2 = LL2+NBHR+(LLEV-1)*NVARL2
              IPLEV = IP(LLEV)
              DO 25 LEV=1,KLEV
                IIP=OBS2(1,LEV,1,1)+0.1
                ICAT=CAT(LEV)+0.1
                IF(ICAT.NE.1) GO TO 25
                IF(OBS2(1,LEV,1,1).GT.VMAX-1.) GO TO 25
                IF(IIP.LT.IPLEV) GO TO 26
                IF(IIP.EQ.IPLEV) THEN
                  DO KT=1,MELM-1
                    DO IT=NSTP2,1,-1
                      IF(OBS2(2,LEV,IT,KT).LT.VMAX.AND.
     *                  OBS2(3,LEV,IT,KT).LT.VMAX) THEN
                        BUF2(IL2+1)  = OBS2(2,LEV,IT,KT)
                        BUF2(IL2+2)  = OBS2(3,LEV,IT,KT)
                        GO TO 24
                      END IF
                    END DO
24                  CONTINUE
                    IL2 = IL2+2
                  END DO
                  GO TO 26
                END IF
25            CONTINUE
26            CONTINUE
              IBIG = LL2+NBHR+(LLEV-1)*NVARL2+1
              IEND = LL2+NBHR+(LLEV-1)*NVARL2+2
              DO IL = IBIG,IEND
                IF(BUF2(IL).LT.F9) THEN
                  MARK = 1
                  GO TO 29
                END IF
              END DO
29            CONTINUE
30          CONTINUE
            IF(MARK.EQ.0) THEN
              LL2 = LL2
            ELSE IF(MARK.EQ.1) THEN
              LL2 = LL2+L1RPT2
              NREPS2 = NREPS2+1
            END IF
          END IF
          IF(NREPS1.GE.GMXSTN.OR.NREPS2.GE.GMXSTN) GO TO 300

C         GO READ ANOTHER REPORT

        GO TO 2

300   CONTINUE

C     CHANGE THE MISSING VALUE FOR THE PURPOSE OF VERIFICATION

      DO I1 = 1,LL1
        IF(BUF1(I1).GT.VMAX-1.) BUF1(I1) = F9
      END DO
      DO I2 = 1,LL2
        IF(BUF2(I2).GT.VMAX-1.) BUF2(I2) = F9
      END DO

      LS = 0
      DO 110 N = 1,NREPS1

C       TOSS THE DUPLICATING T&Z REPORTS

        IF(BUF1(LS+1).LT.0.) GO TO 109
          IDD = BUF1(LS+1)
          DO M = N+1,NREPS1
            LS1  = (M-1)*L1RPT1
            IDDD = BUF1(LS1+1)
            IF(IDDD.EQ.IDD) THEN
              WRITE(*,1111) (BUF1(LS+I),I=1,3),BUF1(LS+6)
              DO LLL = 1,4
                KL = LS+6+(LLL-1)*NVARL1
                WRITE(*,2222) LLL,(BUF1(KL+I),I=1,3)
              END DO
              WRITE(*,1111) (BUF1(LS1+I),I=1,3),BUF1(LS1+6)
              DO LLL = 1,4
                KL = LS1+6+(LLL-1)*NVARL1
                WRITE(*,2222) LLL,(BUF1(KL+I),I=1,3)
              END DO
              BUF1(LS1+1) = EMIS
            END IF
          END DO

C         TOSS THE DATA (T,Z,Q) WHICH QUALITY MARK GIVE A VALUE 
C         MORE THAN THAT SPECIFIED

          MARK = 0
          DO 108 L = 1,GNLEV
            LM = LS+NBHR+(L-1)*NVARL1
            LN = LS+NBHR+(L-1)*NVARL1+3
            DO J = 1,3
              IF(BUF1(LN+J).GT.FQM(J,1)) BUF1(LM+J) = F9
              IF(BUF1(LM+J).LT.F9) MARK = 1
            END DO
108       CONTINUE
          IF(MARK.EQ.0) THEN
             BUF1(LS+1) = EMIS
             GO TO 109
          END IF
109     CONTINUE
        LS = LS+L1RPT1
110   CONTINUE

      LS = 0
      DO 120 N = 1,NREPS2

C       TOSS THE DUPLICATING U&V REPORTS

        IF(BUF2(LS+1).LT.0.) GO TO 119
          IDD = BUF2(LS+1)
          DO M = N+1,NREPS2
            LS1  = (M-1)*L1RPT2
            IDDD = BUF2(LS1+1)
            IF(IDDD.EQ.IDD) THEN
              WRITE(*,1112) (BUF2(LS+I),I=1,3),BUF2(LS+6)
              DO LLL = 1,4
                KL = LS+6+(LLL-1)*NVARL2
                WRITE(*,3333) LLL,(BUF2(KL+I),I=1,2)
              END DO
              WRITE(*,1112) (BUF2(LS1+I),I=1,3),BUF2(LS1+6)
              DO LLL = 1,4
                KL = LS1+6+(LLL-1)*NVARL2
                WRITE(*,3333) LLL,(BUF2(KL+I),I=1,2)
              END DO
              BUF2(LS1+1) = EMIS
            END IF
          END DO

C         TOSS THE DATA (U,V) IF THE QUALITY MARK GIVES A VALUE 
C         GREATER THAN SPECIFIED

          MARK = 0
          DO 118 L = 1,GNLEV
            LM = LS+NBHR+(L-1)*NVARL2
            LN = LS+NBHR+(L-1)*NVARL2+2
            DO J = 1,2
              IF(BUF2(LN+J).GT.FQM(J,2)) BUF2(LM+J) = F9
              IF(BUF2(LM+J).LT.F9) MARK = 1
            END DO
118       CONTINUE
          IF(MARK.EQ.0) THEN
            BUF2(LS+1) = EMIS
            GO TO 119
          END IF
119     CONTINUE
        LS = LS+L1RPT2
120   CONTINUE

C     PUT TWO 1-D ARRAY OF MESSAGES, RESPECTIVE FOR T&Z AND U&V REPORTS, INTO 
C     THREE 1-D ARRAY AND ONE 3-D ARRAY, RESPECTIVE FOR STATION ID, LONGITUDE,
C     LATITUDE AND HEIGHT,TEMPERATURE,DEWPOINT TEMPERATURE,U&V COMPONENT.

      NOB = 0
      LS  = 0
      DO 130 N =1,NREPS1
        IF(BUF1(LS+1).LT.0.) GO TO 129
          NOB = NOB+1
          IDSTN(NOB) = BUF1(LS+1)
          ALON(NOB)  = BUF1(LS+2)
          ALAT(NOB)  = BUF1(LS+3)
          DO L =1,GNLEV
            LK = LS+NBHR+(L-1)*NVARL1
            ZTRUV(NOB,1,L) = BUF1(LK+2)
            ZTRUV(NOB,2,L) = BUF1(LK+1)+T0
            IF(BUF1(LK+1).GT.F9-1.) ZTRUV(NOB,2,L) = F9
            ZTRUV(NOB,3,L) = TQP2RH(BUF1(LK+1),BUF1(LK+3),IP(L))
          END DO
          LSS = 0
          DO 128 NN =1,NREPS2
            IF(BUF2(LSS+1).LT.0.) GO TO 127
              IF(ABS(BUF2(LSS+1)-BUF1(LS+1)).LT.0.1) THEN
                DO L =1,GNLEV
                  LKK = LSS+NBHR+(L-1)*NVARL2
                  ZTRUV(NOB,4,L) = BUF2(LKK+1)
                  ZTRUV(NOB,5,L) = BUF2(LKK+2)
                END DO
                BUF2(LSS+1) = EMIS
              END IF
127         CONTINUE
            LSS = LSS+L1RPT2
128       CONTINUE
129     CONTINUE
        LS = LS+L1RPT1
130   CONTINUE

      LSS = 0
      DO 140 NN =1,NREPS2
        IF(BUF2(LSS+1).LT.0.) GO TO 139
          NOB = NOB+1
          IF(NOB.GT.GMXSTN) GO TO 550
          IDSTN(NOB) = BUF2(LSS+1)
          ALON(NOB)  = BUF2(LSS+2)
          ALAT(NOB)  = BUF2(LSS+3)
          DO L =1,GNLEV
            LKK = LSS+NBHR+(L-1)*NVARL2
            ZTRUV(NOB,4,L) = BUF2(LKK+1)
            ZTRUV(NOB,5,L) = BUF2(LKK+2)
          END DO
139     CONTINUE
        LSS = LSS+L1RPT2
140   CONTINUE
      JSTA = NOB
      NSTA = JSTA

C     REORDER ADPUPA STATION REPORTS AS STATION ID WITH A ASCENDING

      DO I=1,JSTA-1
 145    CONTINUE
        MINSD = IDSTN(I)
        DO J=I+1,JSTA
          JSD   = IDSTN(J)
          IF(JSD.LT.MINSD) THEN
            MINSD    = IDSTN(J)
            ALOMIN   = ALON(J)
            ALAMIN   = ALAT(J)
            DO L=1,GNLEV
              DO M=1,GNVARP
                ZZZ(M,L) = ZTRUV(J,M,L)
              END DO
            END DO
 
            DO K=J,I+1,-1
              IDSTN(K) = IDSTN(K-1)
              ALAT(K)  = ALAT(K-1)
              ALON(K)  = ALON(K-1)
              DO L=1,GNLEV
                DO M=1,GNVARP
                  ZTRUV(K,M,L) = ZTRUV(K-1,M,L)
                END DO
              END DO
            END DO
  
            IDSTN(I) = MINSD
            ALAT(I)  = ALAMIN
            ALON(I)  = ALOMIN
            DO L=1,GNLEV
              DO M=1,GNVARP
                ZTRUV(I,M,L) = ZZZ(M,L)
              END DO
            END DO
            GO TO 145
          END IF
        END DO
      END DO

C     ALL DONE - RECORD THE OUTCOME

500   CONTINUE
      WRITE(LUOUT,'(A80)')
      WRITE(LUOUT,'(A80)') SUCCESS
      WRITE(LUOUT,111    ) NREPT,NOB
      WRITE(LUOUT,'(A80)') SUCCESS
      WRITE(LUOUT,'(A80)')
      GO TO 600

550   CONTINUE
      WRITE(LUOUT,'(A80)')
      WRITE(LUOUT,'(A80)') WARNING
      WRITE(LUOUT,111    ) NREPT,NOB
      WRITE(LUOUT,'(A80)') WARNING
      WRITE(LUOUT,'(A80)')

600   CONTINUE

C     CLOSE THE BUFR FILE

      CALL CLOSBF(LUBFI)


900   CONTINUE

111   FORMAT('TOTAL REPORTS: ',I6,'  ---  SELECTED: ',I6)
1111  FORMAT('Station ID issued duplicating T&Z  report:',F8.0
     *   ,'  Lon.=',F8.2,'  Lat.=',F8.2,'  T29TYP=',F8.0)
1112  FORMAT('Station ID issued duplicating U&V  report:',F8.0
     *   ,'  Lon.=',F8.2,'  Lat.=',F8.2,'  T29TYP=',F8.0)
2222  FORMAT('T&Z&Q on Level',I1,':',3F10.2)
3333  FORMAT('U&V   on Level',I1,':',3F10.2)

      RETURN

901   CONTINUE
      PRINT 902, IDATE 
902   FORMAT (' PREPDA AND NMC DATES DONT MATCH: IDATE = ', I10)
      STOP 98
C 902   PRINT *, 'PRCBFR - BAD OR MISSING NMCDATE FILE    '
C       STOP 99
      END
