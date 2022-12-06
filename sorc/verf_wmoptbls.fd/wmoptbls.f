C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C
C MAIN PROGRAM: wmoptbls
C   PRGMMR: LILLY            ORG: NP12        DATE: 2009-03-09
C
C ABSTRACT: READ WMO MONTHLY VERIFICATION STATISTICS ON ARCHIVE DISK  
C
C PROGRAM HISTORY LOG:
C   1994-03-09  Original Author: Charles Vlcek
C   2009-10-01  LILLY - Convert on the P6
C
C USAGE:
C   INPUT FILES:
C     FT40F001 - READ STATISTICAL ARRAY
C
C   OUTPUT FILES:
C     FT47F001 - Prints WMO MONTHLY VERIFICATION in NON-STANDARD tables
C                for Northern Hemisphere 
C     FT48F001 - Prints WMO MONTHLY VERIFICATION in NON-STANDARD tables
C                for Southern Hemisphere 
C     FT49F001 - Prints WMO MONTHLY VERIFICATION in NON-STANDARD tables
C                for Tropics
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

C     PROGRAM WMOPTBLS

C      READ WMO MONTHLY VERIFICATION STATISTICS ON ARCHIVE DISK    .
C     /gpfsuser/g02/wx12vc/ANLVER/output.d/vwmomon.MM  (fort.40)   .
C      PRINT WMO MONTHLY VERIFICATION IN NON-STANDARD TABLES       .
C          NORTHERN AND SOUTHERN HEMISPHERE AND TROPICS            .
C                                                                  .
C  97-02-01   Andrew S. Krein   conversion to cray completed       .
C  98-10-06   Charles L. Vlcek  f90 and y2k-compliant              .
C  99-09-23   Charles L. Vlcek  compile on IBM RS6000              .

       DIMENSION X(10,2,10,3,3),Z(10,2)
       DIMENSION X2(10,2,10,3,3),z2(10,2)
       integer   iyear, imon

C  get verifying year and month from first line (record) in fort.40

       rewind 40
       read (40,100) iyear, imon

 100   FORMAT(i4,I2)
 120   FORMAT(3I2,20F8.2)
 130   FORMAT(3I2,20F8.2)
 151   format(20f6.2,4x,2i5,4x,'1-2-9    500MB NH WIND MEAN ERROR')
 152   format(20f6.2,4x,2i5,4x,'1-3-9    250MB NH WIND MEAN ERROR')
 153   format(20f6.1,4x,2i5,4x,'2-2-9    500MB SH WIND MEAN ERROR')
 154   format(20f6.1,4x,2i5,4x,'2-3-9    250MB SH WIND MEAN ERROR')
 155   format(20f6.1,4x,2i5,4x,'3-1-9    850MB TR WIND MEAN ERROR')
 156   format(20f6.1,4x,2i5,4x,'3-2-9    250MB TR WIND MEAN ERROR')

C......FILL ARRAY........................

       X  = 999.99
       X2 = 999.99

C......READ STATISTICAL ARRAY......................................
C......NOTE      WIND MEAN ERROR WRITTEN IN FICTITIOUS COLUMN 9   .
C......          STORE IN ARRAY X2.................................

  20   CONTINUE
       READ(40,120,END=50) L3,L2,L1,Z
       IF (L1.NE.9)   THEN  
         DO I = 1,10
          DO J = 1,2
            X(I,J,L1,L2,L3) = Z(I,J)
          END DO
         END DO
       END IF
       GO TO 20
  50   CONTINUE

C      read input second time around to pick up mean wind errors

       rewind 40
       read (40,100) iyear, imon

  21   CONTINUE
       READ(40,120,END=51) L3,L2,L1,Z2
         DO I = 1,10
          DO J = 1,2
            X2(I,J,L1,L2,L3) = Z(I,J)
          END DO
         END DO
       GO TO 21
  51   CONTINUE

       write(44,151)((x2(ihr,J,10,2,1),ihr=1,10),J=1,2),iyear,imon
       write(41,152)((x2(ihr,J,10,3,1),ihr=1,10),J=1,2),iyear,imon 
       write(45,153)((x2(ihr,J,10,2,2),ihr=1,10),J=1,2),iyear,imon
       write(42,154)((x2(ihr,J,10,3,2),ihr=1,10),J=1,2),iyear,imon
       write(46,155)((x2(ihr,J,10,1,3),ihr=1,10),J=1,2),iyear,imon
       write(43,156)((x2(ihr,J,10,2,3),ihr=1,10),J=1,2),iyear,imon

C......PRINT VERIFICATION TABLES.......................
       CALL TABLE(IYEAR,IMON,X)


       STOP
       END
       SUBROUTINE TABLE(MYR,MMN,X)

C......PRINT WMO MONTHLY VERIFICATION TABLES......................

       INTEGER       KLVL(3,3)
       CHARACTER(3)  CMON(13)
       CHARACTER(5)  JHEM(2)
       CHARACTER(8)  JLVL(3,3)
       CHARACTER(17) JRGN(3)

       DIMENSION X(10,2,10,3,3)

       DATA  JRGN /'NORTH HEMISPHERE ','SOUTH HEMISPHERE ',
     1             'TROPICS (20 SOUTH'/
       DATA  JHEM /'NORTH','SOUTH'/
       DATA  CMON /'JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN',
     &             'JUL', 'AUG', 'SEP', 'OCT', 'NOV', 'DEC', 'ANN'/
       DATA  JLVL /'.M S L. ','.500HPA.','.250HPA.',
     1             '.M S L. ','.500HPA.','.250HPA.',
     2             '.850HPA.','.250HPA.','.XXXHPA.'/
       DATA  KLVL /-1, 500, 250, -1, 500, 250, 850, 250, -999/

 100   FORMAT (////,18X,'NCEP  -  W.M.O. NWP STANDARD ',
     1         'VERIFICATION SCORES',//)
 103   FORMAT (11X,'GLOBAL FORECAST SYSTEM MODEL:  ',A3,I5,', ',A17)
 110   FORMAT (11X,'(20-90 ',A5,'), VERIFICATION AGAINST ANALYSIS, ',
     1         '00 UTC FORECASTS.',/)
 115   FORMAT (11X,'TO 20 NORTH), VERIFICATION AGAINST ANALYSIS, ',
     1         '00 UTC FORECASTS.',/)
 120   FORMAT (11X,'MEAN ERROR (MERR), ROOT-MEAN-SQUARE ERROR (RMSE), ',
     1         'CORRELATION ',/,11X,'COEFFICIENT BETWEEN OBSERVED AND ',
     1         'FORECAST CHANGE (CCOR),',' S1 SCORE, ',/,t11,
     2         'AND ROOT-MEAN-SQUARE VECTOR WIND ERROR (RMSVE).',//)
 130   FORMAT (21X,'PRESSURE/GEOPOT HEIGHT', 40X,'TEMPERATURE',
     1         9X,'WIND',/,
     2         22X,'MERR         RMSE          CCOR            S1',
     3         13X,'MERR   RMSE  CCOR     RMSVE')
 135   FORMAT (22X,'GEOPOTENTIAL HEIGHT',42X,'TEMPERATURE',
     1         9X,'WIND',/,
     2         22X,'MERR         RMSE          CCOR             S1',
     3         13X,'MERR   RMSE  CCOR     RMSVE')
 140   FORMAT (/,T10,A8)
 145   FORMAT (10X,I3,'HR',F6.2,1X,F6.2,1X,F6.2,1X,F6.2,1X,F6.2,1X,F6.2,
     *         1X,F7.2,1X,F7.2)
 150   FORMAT (10X,I3,'HR',1X,F6.2,1X,F6.2,1X,F6.2,1X,F6.2,1X,F6.2,1X,
     *        F6.2,1X,F6.2,1X,F6.2,1X,F6.2,1X,F6.2,1X,F6.2,1X,F6.2,1X,
     *        F6.2,1X,F6.2,1X,F7.2,1X,F7.2)
 155   FORMAT (I4, I2, 1X, I3, 'HR', F7.2,F7.2,F7.2,F7.2,F7.2,F7.2,
     *         F7.2,F7.2, 14X, I4, I2)
 158   FORMAT (I4, I2, 1X ,I3, 'HR', 4F7.2, 2F6.2, 2F7.2, 4F6.2,
     &         2F6.2, 2F6.2, I4, I2)
 160   FORMAT (/)
 170   FORMAT (/////)

       MMM = MMN
       IF (MMM.EQ.0)    MMM = 13
       DO 20 IRGN = 1,3

C......PRINT APPROPRIATE HEADING...........................

        PRINT 100
        PRINT 103, CMON(MMM), MYR, JRGN(IRGN)

        IF (IRGN.NE.3) THEN
         PRINT 110,JHEM(IRGN)
        ELSE
         PRINT 115
        ENDIF

        PRINT 120
        IF (IRGN.NE.3) THEN
         PRINT 130
        ELSE
         PRINT 135
        ENDIF

C......PRINT STATISTICS....................................

        IF (IRGN.NE.3) THEN
         DO 30 ILVL = 1,3
          IF (ILVL.EQ.1) THEN
           PRINT 140,JLVL(ILVL,IRGN)
           DO 32 IHR = 1,10
            IFHR = 24 * IHR
            PRINT 145, IFHR,
     *                X(IHR,1,1,ILVL,IRGN),X(IHR,2,1,ILVL,IRGN),
     *                X(IHR,1,2,ILVL,IRGN),X(IHR,2,2,ILVL,IRGN),
     *                X(IHR,1,3,ILVL,IRGN),X(IHR,2,3,ILVL,IRGN),
     *                X(IHR,1,4,ILVL,IRGN),X(IHR,2,4,ILVL,IRGN)
            if (irgn.eq.1) then
              write (47,155) MYR, MMN, IFHR,
     *                X(IHR,1,1,ILVL,IRGN),X(IHR,2,1,ILVL,IRGN),
     *                X(IHR,1,2,ILVL,IRGN),X(IHR,2,2,ILVL,IRGN),
     *                X(IHR,1,3,ILVL,IRGN),X(IHR,2,3,ILVL,IRGN),
     *                X(IHR,1,4,ILVL,IRGN),X(IHR,2,4,ILVL,IRGN),
     *                KLVL(ILVL,IRGN), IRGN
            else
              write (48,155) MYR, MMN, IFHR,
     *                X(IHR,1,1,ILVL,IRGN),X(IHR,2,1,ILVL,IRGN),
     *                X(IHR,1,2,ILVL,IRGN),X(IHR,2,2,ILVL,IRGN),
     *                X(IHR,1,3,ILVL,IRGN),X(IHR,2,3,ILVL,IRGN),
     *                X(IHR,1,4,ILVL,IRGN),X(IHR,2,4,ILVL,IRGN),
     *                KLVL(ILVL,IRGN), IRGN

            endif
  32       CONTINUE

          ELSE
           PRINT 140,JLVL(ILVL,IRGN)
           DO 34 IHR = 1,10
            IFHR = 24 * IHR

            PRINT 150, IFHR,
     *                X(IHR,1,1,ILVL,IRGN),X(IHR,2,1,ILVL,IRGN),
     *                X(IHR,1,2,ILVL,IRGN),X(IHR,2,2,ILVL,IRGN),
     *                X(IHR,1,3,ILVL,IRGN),X(IHR,2,3,ILVL,IRGN),
     *                X(IHR,1,4,ILVL,IRGN),X(IHR,2,4,ILVL,IRGN),
     *                X(IHR,1,5,ILVL,IRGN),X(IHR,2,5,ILVL,IRGN),
     *                X(IHR,1,6,ILVL,IRGN),X(IHR,2,6,ILVL,IRGN),
     *                X(IHR,1,7,ILVL,IRGN),X(IHR,2,7,ILVL,IRGN),
     *                X(IHR,1,8,ILVL,IRGN),X(IHR,2,8,ILVL,IRGN),
     *                X(IHR,1,9,ILVL,IRGN),X(IHR,2,9,ILVL,IRGN),
     *                X(IHR,1,10,ILVL,IRGN),X(IHR,2,10,ILVL,IRGN)
            if (irgn.eq.1) then
              write(47,158) MYR, MMN, IFHR,
     *                X(IHR,1,1,ILVL,IRGN),X(IHR,2,1,ILVL,IRGN),
     *                X(IHR,1,2,ILVL,IRGN),X(IHR,2,2,ILVL,IRGN),
     *                X(IHR,1,3,ILVL,IRGN),X(IHR,2,3,ILVL,IRGN),
     *                X(IHR,1,4,ILVL,IRGN),X(IHR,2,4,ILVL,IRGN),
     *                X(IHR,1,5,ILVL,IRGN),X(IHR,2,5,ILVL,IRGN),
     *                X(IHR,1,6,ILVL,IRGN),X(IHR,2,6,ILVL,IRGN),
     *                X(IHR,1,7,ILVL,IRGN),X(IHR,2,7,ILVL,IRGN),
     *                X(IHR,1,8,ILVL,IRGN),X(IHR,2,8,ILVL,IRGN),
     *                X(IHR,1,9,ILVL,IRGN),X(IHR,2,9,ILVL,IRGN),
     *                X(IHR,1,10,ILVL,IRGN),X(IHR,2,10,ILVL,IRGN),
     *                KLVL(ILVL,IRGN), IRGN
            elseif (irgn.eq.2) then
              write(48,158) MYR, MMN, IFHR,
     *                X(IHR,1,1,ILVL,IRGN),X(IHR,2,1,ILVL,IRGN),
     *                X(IHR,1,2,ILVL,IRGN),X(IHR,2,2,ILVL,IRGN),
     *                X(IHR,1,3,ILVL,IRGN),X(IHR,2,3,ILVL,IRGN),
     *                X(IHR,1,4,ILVL,IRGN),X(IHR,2,4,ILVL,IRGN),
     *                X(IHR,1,5,ILVL,IRGN),X(IHR,2,5,ILVL,IRGN),
     *                X(IHR,1,6,ILVL,IRGN),X(IHR,2,6,ILVL,IRGN),
     *                X(IHR,1,7,ILVL,IRGN),X(IHR,2,7,ILVL,IRGN),
     *                X(IHR,1,8,ILVL,IRGN),X(IHR,2,8,ILVL,IRGN),
     *                X(IHR,1,9,ILVL,IRGN),X(IHR,2,9,ILVL,IRGN),
     *                X(IHR,1,10,ILVL,IRGN),X(IHR,2,10,ILVL,IRGN),
     *                KLVL(ILVL,IRGN), IRGN
            endif

  34       CONTINUE
          ENDIF
  30     CONTINUE
         PRINT 170

        ELSE
         DO 40 ILVL = 1,2
          PRINT 140,JLVL(ILVL,IRGN)
           DO 42 IHR = 1,10
            IFHR = 24 * IHR
            PRINT 150, IFHR,
     *                X(IHR,1,1,ILVL,IRGN),X(IHR,2,1,ILVL,IRGN),
     *                X(IHR,1,2,ILVL,IRGN),X(IHR,2,2,ILVL,IRGN),
     *                X(IHR,1,3,ILVL,IRGN),X(IHR,2,3,ILVL,IRGN),
     *                X(IHR,1,4,ILVL,IRGN),X(IHR,2,4,ILVL,IRGN),
     *                X(IHR,1,5,ILVL,IRGN),X(IHR,2,5,ILVL,IRGN),
     *                X(IHR,1,6,ILVL,IRGN),X(IHR,2,6,ILVL,IRGN),
     *                X(IHR,1,7,ILVL,IRGN),X(IHR,2,7,ILVL,IRGN),
     *                X(IHR,1,8,ILVL,IRGN),X(IHR,2,8,ILVL,IRGN),
     *                X(IHR,1,9,ILVL,IRGN),X(IHR,2,9,ILVL,IRGN),
     *                X(IHR,1,10,ILVL,IRGN),X(IHR,2,10,ILVL,IRGN)

            write(49,158) MYR, MMN, IFHR,
     *                X(IHR,1,1,ILVL,IRGN),X(IHR,2,1,ILVL,IRGN),
     *                X(IHR,1,2,ILVL,IRGN),X(IHR,2,2,ILVL,IRGN),
     *                X(IHR,1,3,ILVL,IRGN),X(IHR,2,3,ILVL,IRGN),
     *                X(IHR,1,4,ILVL,IRGN),X(IHR,2,4,ILVL,IRGN),
     *                X(IHR,1,5,ILVL,IRGN),X(IHR,2,5,ILVL,IRGN),
     *                X(IHR,1,6,ILVL,IRGN),X(IHR,2,6,ILVL,IRGN),
     *                X(IHR,1,7,ILVL,IRGN),X(IHR,2,7,ILVL,IRGN),
     *                X(IHR,1,8,ILVL,IRGN),X(IHR,2,8,ILVL,IRGN),
     *                X(IHR,1,9,ILVL,IRGN),X(IHR,2,9,ILVL,IRGN),
     *                X(IHR,1,10,ILVL,IRGN),X(IHR,2,10,ILVL,IRGN),
     *                KLVL(ILVL,IRGN), IRGN

  42       CONTINUE
  40     CONTINUE

        ENDIF

  20   CONTINUE


       RETURN
       END
