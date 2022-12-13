       SUBROUTINE WNDEVL(MODEL,MDLNAM,LVL,WGT,IRGNB,IRGNE,
     1                   IDIM,JDIM,SX,IPDLY)
C SUBPROGRAM:    WNDEVL
C   PRGMMR: LILLY            ORG: NP12        DATE: 2007-07-22
C
C ABSTRACT: MEAN ERROR, RMS VEC ERR, AVG AND ABS MAX FCST AND ANL WIND
C FOR NORTH AND SOUTH HEMISPHERE AND TROPICS PRESSURE LEVELS 850, 500,
C AND 250MB.
C
C PROGRAM HISTORY LOG:
C 2007-07-22 STEVEN G. LILLY -- UPDATING SOURCE CODES
C
C USAGE:    CALL WNDEVL(MODEL,MDLNAM,LVL,WGT,IRGNB,IRGNE,
C                       IDIM,JDIM,SX,IPDLY)
C
C   INPUT ARGUMENT LIST:
C
C   OUTPUT ARGUMENT LIST:
C
C   SUBPROGRAM CALLED:
C     LIBRARY:    (NONE)
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN-90
C   MACHINE:  IBM RS6000
C
C$$$

C
C***   WMO WIND VERIFICATION ...................................
C
C
       PARAMETER (MXCYL=1,MXFCT=10,MXLVL=3,MAREA=5,MXRGN=3,
     1            NSAVE=6*MAREA,NTOTL=NSAVE*MXRGN)
C
       CHARACTER(15) NAMRGN
       CHARACTER(13) NVGRD(2)
       CHARACTER(11) NAREA
       CHARACTER(10)  ITRUTH
       CHARACTER(8)  PLVL
       CHARACTER(3)  MDLNAM
C
       REAL       SX(NTOTL,MXCYL,MXFCT,2),X(NSAVE,2),
     1            WGT(37,MXRGN)
C
       real anlnhmu(145,37),anlnhmv(145,37)
       real anlshmu(145,37),anlshmv(145,37)
       real trpanlu(145,37),trpanlv(145,37)
       real trpfctu(145,37),trpfctv(145,37)
       real hemfctu(145,37),hemfctv(145,37)
       real wgt1(37),wgt2(37),wgt3(37)
c
       INTEGER    LABEL(6),LABELF(6),LABELA(6),
     1            IDIM(2,MAREA,MXRGN),JDIM(2,MAREA,MXRGN)
       integer idim1(2,marea),idim2(2,marea),idim3(2,marea)
       integer jdim1(2,marea),jdim2(2,marea),jdim3(2,marea)
c
       LOGICAL(1)  IVFCT,IVLVL
C
       DATA  NVGRD /'2.5DEG LATLON','5.0DEG LATLON'/
       DATA  NTMAX / 1/
       DATA  IGRDNH,IGRDSH /29,30/
C
       COMMON /XDATA/IDATE(MXCYL),IDS(4,MXLVL),IVFCT(MXFCT,2),
     1         IVT(4,MXCYL),IVLVL(MXLVL),PLVL(MXLVL)
       COMMON /YDATA/ NBEGIN,NEND,ICYB,ICYE,ICYI,IVFLD,IFTINC,
     1         NICR,JLVLS,JFCTS,MDANL,ITRUTH,NAREA(4),NAMRGN(MXRGN)
C
C..................................................................
C
 100    FORMAT(4X,'MDANL,IVFLD,IANL,IVER= ',4I2,2X,'LABEL= ',6i9)
 200    FORMAT('0 ERROR NUMBER ',I3,' OCCURRED FOR MDANL= ',I2,
     1         ' ...IANL= ',I2,' ...IVER= ',I2,' *************',/,
     2         5X,'LABEL= ', 4i9,i11, i9, ' ... DO NEXT CYCLE ...')
C
 210    FORMAT('0 ERROR NUMBER ',I3,' OCCURRED FOR MODEL= ',I2,
     1         ' ...IANL= ',I2,' ...IVER= ',I2,' *************',/,
     2         5X,'LABEL= ', 5i9,' ... DO NEXT FCST HR ...')
C
C
 310   FORMAT(//,5X,'..... FOR MODEL = ',A3,' ... FOR  ',A7,
     1        'U AND V COMPONENTS  ..... ',A13,' GRID .....',/)
C
 320   FORMAT(5X,'FCST HOUR = ',I5,4X,'FROM  ',I10,' TO ',I10,' ...',
     1        '.........................',/, T18,
     2        '***** ',4(2X,A11), 8X, '*****', 5X,A15,//,
     3        5X,'DAY  CYL     MERR   RMSE   FBAR FMAX ABAR AMAX',
     4        5X, 'MERR   RMSE   FBAR FMAX ABAR AMAX',
     5        5X, 'MERR   RMSE   FBAR FMAX ABAR AMAX')
 330   FORMAT(5X,I3,2X,I3, 2(2X,2F7.2,2X,4F5.1),/,
     1        12X,3(2X,2F7.2,2X,4F5.1) )
 340   FORMAT(/)
 350   format(5(2f7.2,1x,4f5.1))
 360   format(i10,3x,i1)
C
C
C
       CALL IFILL(LABEL,6,1,1,0)
       CALL IFILL(LABELF,6,1,1,0)
       CALL IFILL(LABELA,6,1,1,0)
       CALL AFILL(TRPANLu,145,37,1,0.0)
       CALL AFILL(TRPANLv,145,37,1,0.0)
       CALL AFILL(TRPFCTu,145,37,1,0.0)
       CALL AFILL(TRPFCTv,145,37,1,0.0)
C
C..................................................................
C        1. SPECIFY LABEL VARIABLES AND CALL SUBROUTINE getgrib to
C           RETRIEVE VERIFYING FIELD.
C..................................................................
C
C..... . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C
       NTIMES = 0
C.....
       LOOP10: DO ICYL = ICYB,ICYE,ICYI
          IVFDTE = IDATE(ICYL)
          IF(IVFDTE  .EQ.  0) CYCLE LOOP10
          NTIMES = NTIMES + 1
C
          IF(IVFLD  .EQ.  1) THEN
            IANL = 1
            IVER = 0
          ELSE
            IANL = 0
            IVER = 1
          ENDIF
C
C.....
c  get u and v components of wind
C
C
       DO L = 1,4
         LABEL(L) = IDS(L,lvl)
       END DO
c
       LABEL(5) = IVFDTE
       LABEL(6) = IGRDNH
       PRINT 100, MDANL, IVFLD,IANL,IVER,(LABEL(L),L=1,6)
C.....
       ifhr=0
c
c  get current analysis for northern hemisphere
c
       CALL getgrib(MDANL,LABEL,IANL,IVER,ifhr,anlnhmu,anlnhmv,IERR)
c
       IF(IERR  .NE. 0) THEN
          PRINT 200,IERR,MDANL,IANL,IVER,(LABEL(L),L=1,6)
          CYCLE LOOP10
       ENDIF
C.....
       DO L = 1,4
          LABEL(L) = IDS(L,lvl)
       END DO
c
       LABEL(5) = IVFDTE
       LABEL(6) = IGRDSH
C.....
c
c  get current analysis for southern hemisphere
c
       CALL getgrib(MDANL,LABEL,IANL,IVER,ifhr,anlshmu,anlshmv,IERR)
c
       IF(IERR  .NE. 0) THEN
         PRINT 200,IERR,MDANL,IANL,IVER,(LABEL(L),L=1,6)
         CYCLE LOOP10
       ENDIF
C.....
       DO L = 1,4
          LABEL(L) = IDS(L,lvl)
       END DO
c
       LABEL(5) = IVFDTE
       LABEL(6) = IGRDNH
C
C
C...   SAVE ANL FOR TROPICS (20S-20N) .. STORE IN 1-17 ....
C
       JSH =28
       JNH =1
C.....
          DO J = 1,9
             JSH = JSH + 1
               DO I = 1,145
                  TRPANLu(I,J) = ANLSHMu(I,JSH)
                  TRPANLv(I,J) = ANLSHMv(I,JSH)
               END DO
          END DO
c
          DO J = 10,17
             JNH = JNH + 1
               DO I = 1,145
                  TRPANLu(I,J) = ANLNHMu(I,JNH)
                  TRPANLv(I,J) = ANLNHMv(I,JNH)
               END DO
          END DO
c
C
C.....END OF RETRIEVE VERIFYING ANALYSIS .....
C
C
C..................................................................
C...     2. SPECIFY LABEL VARIABLES AND CALL SUBROUTINE getgrib to
C...        RETRIEVE VERIFYING FORECAST HOUR.  
C..................................................................
C
C..... . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C
       IVER = 1
       IANL = 0
       IF(IFTINC  .EQ.  0) THEN
        IF(MODEL  .NE.  MDANL) THEN
         IF(IVFLD  .EQ.  1) THEN
          IANL = 1
          IVER = 0
         ENDIF
        ENDIF
       ENDIF
C.....
       LOOP20: DO IFHR = 1,JFCTS
          IF(.NOT. IVFCT(IFHR,MODEL)) CYCLE LOOP20
C.....
       LOOP30: DO IRGN = 1,MXRGN
       LOOP80: DO MX=1,1
c
          IF(IRGN  .EQ.  1) THEN
          IGRDF = IGRDNH
c
          ELSE
c
          IF(IRGN  .EQ.  2) THEN
            IGRDF = IGRDSH
c
            ELSE
c
         do i=1,37
            wgt3(i)=wgt(i,3)
         enddo
c
         do i1=1,marea
            idim3(1,i1)=idim(1,i1,3)
            idim3(2,i1)=idim(2,i1,3)
         enddo
c
         do i1=1,marea
            jdim3(1,i1)=jdim(1,i1,3)
            jdim3(2,i1)=jdim(2,i1,3)
         enddo
c
        CALL WMOWND(IDIM3,JDIM3,
     1              WGT3,TRPANLu,TRPANLv,
     2              TRPFCTu,TRPFCTv,IRGN,NICR,X)
        EXIT LOOP80
C
          ENDIF
c
          ENDIF
C
        JFT = IFTINC * IFHR
C
C.....
C.....
       DO L = 1,4
        LABELF(L) = IDS(L,lvl)
        LABELA(L) = IDS(L,lvl)
       END DO
        LABELF(5) = IVFDTE
        LABELF(6) = IGRDF
        LABELA(6) = IGRDF
C
c  get forecast for u and v components of wind.  getgrib converts 
c  m/s to kts to maintain consistency with old HDS version for
c  WMO reports.
c
       CALL getgrib(MODEL,LABELF,IANL,IVER,ifhr,hemfctu,hemfctv,IERR)
C
       IF(IERR  .NE. 0) THEN
         PRINT 210,IERR,MODEL,IANL,IVER,(LABELF(L),L=1,5)
         CYCLE LOOP20
       ENDIF
C
C
       IF(IERR  .NE. 0) THEN
         IAX = 1
         IVX = 0
         PRINT 210,IERR,MDANL,IAX,IVX,(LABELA(L),L=1,6)
         CYCLE LOOP20
       ENDIF
C.....
C
       DO L = 1,4
          LABELF(L) = IDS(L,lvl)
          LABELA(L) = IDS(L,lvl)
       END DO
c
       LABELF(5) = IVFDTE
       LABELF(6) = IGRDF
       LABELA(6) = IGRDF
C
C...   SAVE TROPICS SECTION FROM NHM AND SHM FIELDS ......
C
C
       IF(IRGN  .EQ.  1) THEN
        J1 = 10
        J2 = 17
        J3 = 1
        DO J = J1,J2
          J3 = J3 + 1
           DO I = 1,145
            TRPFCTu(I,J) = HEMFCTu(I,J3)
            TRPFCTv(I,J) = HEMFCTv(I,J3)
        END DO
        END DO
C
       ELSE
c
         IF(IRGN  .EQ.  2) THEN
           J1 = 1
           J2 = 9
           J3 = 28
c
         DO J = J1,J2
           J3 = J3 + 1
            DO I = 1,145
              TRPFCTu(I,J) = HEMFCTu(I,J3)
              TRPFCTv(I,J) = HEMFCTv(I,J3)
            END DO  
         END DO  
c
         ENDIF
       ENDIF
C
C
C..................................................................
C...     3. CALCULATE WMO VERIFICATION SCORES
C...     4. SAVE STATISTICS
C..................................................................
C.....
       IF(IRGN .EQ. 1) THEN
c
         DO i=1,37
            wgt1(i)=wgt(i,1)
         ENDDO
c
         DO i1=1,marea
            idim1(1,i1)=idim(1,i1,1)
            idim1(2,i1)=idim(2,i1,1)
         ENDDO
c
         DO i1=1,marea
            jdim1(1,i1)=jdim(1,i1,1)
            jdim1(2,i1)=jdim(2,i1,1)
         ENDDO
c
        CALL WMOWND(IDIM1,JDIM1,
     1              WGT1,ANLNHMu,ANLNHMv,
     2              HEMFCTu,HEMFCTv,IRGN,NICR,X)
       ELSE
c
         DO i=1,37
            wgt2(i)=wgt(i,2)
         ENDDO
c
         DO i1=1,marea
            idim2(1,i1)=idim(1,i1,2)
            idim2(2,i1)=idim(2,i1,2)
         ENDDO
c
         DO i1=1,marea
            jdim2(1,i1)=jdim(1,i1,2)
            jdim2(2,i1)=jdim(2,i1,2)
         ENDDO
c
        CALL WMOWND(IDIM2,JDIM2,
     1              WGT2,ANLSHMu,ANLSHMv,
     2              HEMFCTu,HEMFCTv,IRGN,NICR,X)
       ENDIF
C
       END DO LOOP80
C
C.....
       DO NVFCN = 1,NICR
         DO I = 1,NSAVE
          II = NSAVE*(IRGN - 1) + I
          SX(II,ICYL,IFHR,NVFCN) = X(I,NVFCN)
         END DO
       END DO
C
C..... END OF REGION LOOP  ......
c
       END DO LOOP30
C
C..... END OF FORECAST LOOP .....
c
       END DO LOOP20
C
C
C..... END OF CYCLE LOOP .....
c
       END DO LOOP10
C
C
C..................................................................
C...     5. PRINT DAILY VERIFICATION SCORES
C...         IPDLY=1 YES  ...  IPDLY=0 NO
C..................................................................
C..... . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C
       IF(IPDLY  .EQ.  1) THEN
       LOOP43: DO NVFCN = 1,NICR
C
       LOOP44: DO IR = IRGNB,IRGNE
       PRINT 310,MDLNAM,PLVL(LVL),NVGRD(NVFCN)
C
       if (plvl(lvl) .eq. "850MB") then
          write(70,360)  nbegin,ir
       elseif (plvl(lvl) .eq. "500MB") then
          write(71,360)  nbegin,ir
       elseif (plvl(lvl) .eq. "250MB") then
          write(72,360)  nbegin,ir
       endif
       LOOP46: DO IFHR = 1,JFCTS
       IF(.NOT. IVFCT(IFHR,MODEL) ) CYCLE LOOP46
       IFTHR = IFTINC*IFHR
       PRINT 555, IFTHR, IFHR
 555   FORMAT('IFTHR=',1x,I4,2x,'IFHR=',1x,I4)
       PRINT 320,IFTHR,NBEGIN,NEND,
     1           (NAREA(I),I=1,4),NAMRGN(IR)
        I1 = NSAVE*(IR-1) + 1
        I2 = I1 + NSAVE - 1
       LOOP48: DO ICYL = ICYB,ICYE,ICYI
       IDAY = IVT(3,ICYL)
       PRINT 330,IDAY,ICYL, (SX(I,ICYL,IFHR,NVFCN),I=I1,I2)
       if (plvl(lvl) .eq. "850MB") then
          write(70,350)  (sx(i,icyl,ifhr,nvfcn),i=i1,i2)
       elseif (plvl(lvl) .eq. "500MB") then
          write(71,350)  (sx(i,icyl,ifhr,nvfcn),i=i1,i2)
       elseif (plvl(lvl) .eq. "250MB") then
          write(72,350)  (sx(i,icyl,ifhr,nvfcn),i=i1,i2)
       endif
       END DO LOOP48
       PRINT 340
       END DO LOOP46
C
       END DO LOOP44
       END DO LOOP43
       ENDIF
C
C
        RETURN
        END
