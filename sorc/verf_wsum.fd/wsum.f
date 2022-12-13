C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C
C MAIN PROGRAM: wsum
C   PRGMMR: LILLY            ORG: NP12        DATE: 2009-03-09
C
C ABSTRACT: GENERATES MONTHLY SUMMARY OF WMO VERIFICATION FOR GFS
C           verified against the GFS SSI ANALYSIS
C
C PROGRAM HISTORY LOG:
C   1994-03-09  Original Author: Charles Vlcek
C
C USAGE:
C   INPUT FILES:
C    FT10F001 - begin and end date of verification period
C    FT28F001 - 850mb wind scores
C    FT29F001 - 500mb wind scores
C    FT30F001 - 250mb wind scores
C
C   OUTPUT FILES:
C    FT80F001 - selected scores for WMO monthly tables in WRTDSK
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

C     PROGRAM WSUMM

       PARAMETER   ( JFCTX=10,MXCYL=31,JSTTX=08,MAREA=5,MXRGN=3,
     1               NSAVE=6*MAREA,NTOTL=NSAVE*MXRGN,mxlvl=3)
       CHARACTER(15) NAMRGN(7), RGNNAM(7)
       CHARACTER(13) NVGRD(2)
       CHARACTER(11) NAREA,NAMAR
       CHARACTER(10) ITRUTH, JTRUTH
       CHARACTER(7) NSTAT(6)
       CHARACTER(8) PLEVEL(3)
       CHARACTER(5)  JSTAT(JSTTX)
       CHARACTER(3) NAME,NTRU,MONTH(12)
       REAL SX(mxrgn,NTOTL,MXCYL,NSAVE),X(JSTTX,NSAVE,JFCTX,mxrgn), 
     1       W(31)
       integer nbegin(4),nend(4),idate(mxcyl)
 
       COMMON /YDATA/ NBEGIN,NEND,ICYB,ICYE,ICYI,IVFLD,IFTINC,
     1                NICR,JLVLS,JFCTS,MDANL,ITRUTH,NAREA(4),
     2                NAMRGN
 
       DATA NVGRD/'2.5DEG LATLON','5.0DEG LATLON'/
       DATA NSTAT/'WND ERR',' RMSVEC','FCT AVG','FCT MAX',
     1            'ANL AVG','ANL MAX'/
       DATA JSTAT/'  NUM',' MEAN','ABSDV',' SDEV',' SKEW',
     1           ' KURT','  MAX','  MIN'/
       DATA MONTH/'JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG',
     1            'SEP','OCT','NOV','DEC'/
       data ntru/'SSI'/
       data jtruth/' ANALYSIS'/
       data name/'MRF'/
       data plevel/' 850MB  ',' 500MB  ',' 250MB  '/
       data rgnnam/'  Q1  00E-88E  ','  Q2  90E-178  ',
     1  '  Q3  180-92W  ',
     1  '  Q4  90W-02W  ','NO HEM 20N-90N ','SO HEM 20S-90S ',
     2  'TROPICS 20S-20N'/
 
 
C  model 1 is GFS
C  marb and mare are the begin and end limits for the areas to 
C  generate monthly summaries for.  5 implies to only do monthly
C  summaries for the entire domain.  1 thru 4 are selected quadrants
C  that are only used when generating monthly summaries for
C  the AVN model.
 
       namrgn = rgnnam
       itruth = jtruth
       XX = 999.9
       iftinc=24
       jfcts=10
       nvfcn=1
       model=1
       marb=5
       mare=5
       sx=0.
       x=0.

C      DO STATISTICAL ANALYSIS
 
 100   format(i10,1x,i10)
 101   format(i10,3x,i1)
 110   format(5(2f7.2,1x,4f5.1))
 
C read in beginning and ending date for monthly summary (ending time is reset to 00z, since this is mrf)
 
       read(10,100) mbegin,mend
 
       call datecnv4(mbegin,nbegin(1),nbegin(2),nbegin(3),nbegin(4))

       call datecnv4(mend,nend(1),nend(2),nend(3),nend(4))

       nend(4) = 0
 
       infile=27
       do 19 inf=1,mxlvl
       infile=infile+1
       rewind (infile)
       ic=1
 
C  ic is the number of cycles for which daily data are available
 
  50   read(infile,101,end=900) idate(ic),irx
       do 15 ihr=1,jfctx
       read(infile,110) (sx(irx,ihr,ic,istat), istat=1,30)
 15    continue
       if (irx .eq. 3) then
       ic=ic+1
       endif
       goto 50
 900   continue
 
       ic=ic-1
 
C  mxrgn:  number of regions for which scores are produced (NH, SH, TROPICS)
C  jfcts:  number of forecast times (10 for GFS 24-240 hrs)
C  iquad:  quadrant limits
C  ista:  number of statistic types for which 24-hourly scores are produced 
 
       do 13 irgn=1,mxrgn
       do 12 ihr=1,jfcts
       do 17 iquad=marb,mare
       do 16 ista=1,6
       NC = 0
       DO 14 iday=1,ic
       is=6*(iquad-1)+ista
       IF(SX(irgn,ihr,iday,is) .EQ. XX) GO TO 14
       NC = NC + 1
       W(NC) = SX(irgn,ihr,iday,is)

       IF(NC .EQ. 0) GO TO 12
  14   continue
       CALL STATS(W,NC,XM,AD,VAR,SD,A3,A4,ZX,ZN,R)
       X(1,ista,ihr,irgn) = FLOAT(NC)
       X(2,ista,ihr,irgn) = XM
       X(3,ista,ihr,irgn) = AD
       X(4,ista,ihr,irgn) = SD
       X(5,ista,ihr,irgn) = A3
       X(6,ista,ihr,irgn) = A4
       X(7,ista,ihr,irgn) = ZX
       X(8,ista,ihr,irgn) = ZN
  16   continue
  17   continue
  12   CONTINUE

C      PRINT STATISTICAL ANALYSIS TABLES FOR EACH FORECAST HOUR

 400   FORMAT(//,9X,'.. ',A3,2X,A8,'  WINDS  FROM  ',I2,'Z',
     1        I3,A3,I4,' - ',I2,'Z',I3,A3,I4,'  VERIFIED VS  ',
     2        A3, 1X, A9,' FOR  ',A15,' .....',/,15X,A13,2X,
     3        'VERIFICATION GRID ',//)
 405   FORMAT(50X,'***** FOR AREA = ',A11,' *****',///,
     1        19X,A7,'..........',  73X,A7,'..........',//,
     2        5X, 'FTHR  NUM ',5(3X,A5),2A5,4X, 5(3X,A5),2(1X,A5),
     3        5X,'FTHR')
 410   FORMAT(5X,I4,F5.0,1X, F8.1,4F8.3,2F5.1, 4X,F8.1,4F8.3,2F6.1,
     1        5X,I4)
 420   FORMAT(20X,A7,'..........', 73X,A7,'..........',/,
     1        5X, 'FTHR  NUM ',5(3X,A5),2A5,4X, 5(3X,A5),2(1X,A5),
     2        5X,'FTHR')
 430   FORMAT(/)
 440   FORMAT(/)
 450   FORMAT(///)

       MONB = NBEGIN(2)
       MONE = NEND(2)

       DO 30 K = MARB,MARE
       PRINT 440

       if ((irgn .eq. 1) .and. (k .eq. 5)) then
          iarea=5
       elseif ((irgn .eq. 2) .and. (k .eq. 5)) then
          iarea=6
       elseif ((irgn .eq. 3) .and. (k .eq. 5)) then
          iarea=7
       endif
        PRINT 400,NAME,PLEVEL(inf),NBEGIN(4),NBEGIN(3),
     1            MONTH(MONB),NBEGIN(1),NEND(4),NEND(3),MONTH(MONE),
     2            NEND(1),NTRU,ITRUTH,NAMRGN(iarea),NVGRD(NVFCN)

       LTME = 0
       DO 32 L = 1,6,2
       LTME = LTME + 1
 
       IF(LTME  .EQ.  1) THEN
        IF(K  .NE.  5) THEN
         NAMAR = NAREA(K)
         PRINT 405, NAMAR, NSTAT(L),NSTAT(L+1),
     1            (JSTAT(I),I=2,JSTTX),(JSTAT(J),J=2,JSTTX)
        ELSE
         PRINT 430
         PRINT 420,NSTAT(L),NSTAT(L+1),
     1            (JSTAT(I),I=2,JSTTX),(JSTAT(J),J=2,JSTTX)
        ENDIF

       ELSE
        PRINT 420, NSTAT(L),NSTAT(L+1),
     1            (JSTAT(I),I=2,JSTTX),(JSTAT(J),J=2,JSTTX)
       ENDIF
 
       DO 40 N = 1,JFCTS
       IF(X(1,1,N,irgn) .EQ. 0.) GO TO 40
       NFTHR = IFTINC * N
       PRINT 410, NFTHR, (X(I,L,N,irgn),I=1,JSTTX),(X(J,L+1,N,irgn),
     1            J=2,JSTTX), NFTHR
  40   CONTINUE

       PRINT 450
 
  32   CONTINUE
 
 422   format(5x,'fthr  num ',5(3x,a5),2a5)
 421   format(5x,i4,f5.0,1x,f8.1,4f8.3,2f5.1)
 
C......COPY SELECTIVE DATA TO WMO MONTHLY TABLE ARCHIVE DISK...

       IF(NVFCN .EQ. 1  .AND.  k .EQ. mare) THEN
         IF(MODEL  .EQ.  1) THEN
 
          CALL WRTDSK(IRGN,inf,JSTTX,NSAVE,JFCTX,X)
         ENDIF
       ENDIF
 
  30   CONTINUE
 
 
  13   continue
  19   continue
       stop
       END
 
       SUBROUTINE WRTDSK(IRGN,LVL,JSTTX,NSAVE,JFCTX,X)

C$$$  SUBROUTINE DOCUMENTATION BLOCK
C
C SUBPROGRAM: WRTDSK
C   PRGMMR: LILLY            ORG: NP12        DATE: 2009-09-23
C
C ABSTRACT:
C
C PROGRAM HISTORY LOG:
C
C 2009-09-23 Steven G. Lilly --  Updated sorc codes
C
C USAGE:
C   INPUT FILES:
C     FT05FT001   -
C
C
C   OUTPUT FILES:
C     FT06FT001   -
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

C  WRITE mean error and RMSVE TO ARCHIVE WMO MONTHLY TABLE
C  LVL(1,2,3=850MB,500MB,250MB)
C  NHEM(rgn 1),SHEM(rgn 2) (500MB,250MB),TROPICS(rgn 3) (850MB,250MB)
 
C  IP8=9:  designator for mean error
C  IP8=8:  designator for RMSVE
 
       dimension x(jsttx,nsave,jfctx,3)
 
 100   FORMAT(3I2,10F8.2)
 
       IP8 = 8
       IF(IRGN  .NE.  3) THEN
C......DO NORTH OR SOUTH HEMISPHERE.........................
        IF(LVL  .EQ.  1) GO TO 10
        IF(LVL  .EQ.  2) THEN
         ILVL = 2
        ELSE
         ILVL = 3
        ENDIF
 
       ELSE
C......DO TROPICS...........................................
        IF(LVL  .EQ.  2) GO TO 10
        IF(LVL  .EQ.  1) THEN
         ILVL = 1
        ELSE
         ILVL = 2
        ENDIF
C
       ENDIF
 
C......WRITE RMSVE (JUNE94 ADDED MEAN ERR IN FICTITIOUS COLUMN 9)..
       WRITE(80,100) IRGN,ILVL,IP8,(X(2,2,ihr,irgn),Ihr=1,10)
       IP8 = IP8 + 1
       WRITE(80,100) IRGN,ILVL,IP8,(X(2,1,ihr,irgn),Ihr=1,10)
 
 
  10   CONTINUE
 
 
       RETURN
       END

       SUBROUTINE STATS(X,N,XM,AD,VAR,SD,A3,A4,ZX,ZN,R)

C$$$  SUBROUTINE DOCUMENTATION BLOCK
C
C SUBPROGRAM: STATS
C   PRGMMR: LILLY            ORG: NP12        DATE: 2009-09-23
C
C ABSTRACT:
C
C PROGRAM HISTORY LOG:
C
C 2009-09-23 Steven G. Lilly --  Updated sorc codes
C
C USAGE:
C   INPUT FILES:
C     FT05FT001   -
C
C
C   OUTPUT FILES:
C     FT06FT001   -
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

C
C  *****  CALCULATE:  XM=MEAN; AD=MEAN DEVIATION;
C  *****              VAR=VARIANCE; SD=STANDARD DEVIATION
C  *****              A3=COEF OF SKEWNESS; A4=COEF OF KURTOSIS
C  *****               ZX=MAXIMUM VALUE; ZN=MINIMUM VALUE; R=RANGE
 
 
       DIMENSION X(N)
 
       SX = 0.
       AD = 0.
       SX2 = 0.
       SX3 = 0.
       SX4 = 0.
 
       DO  1  I = 1,N
         SX = SX + X(I)
   1   CONTINUE


       XM = SX / N


       DO  2  I = 1,N
         XMM = X(I) - XM
         X2 = XMM * XMM
         X3 = XMM * X2
         X4 = X2 * X2

         AD = AD + ABS(XMM)
         SX2 = SX2 + X2
         SX3 = SX3 + X3
         SX4 = SX4 + X4

   2   CONTINUE


       AD = AD / FLOAT(N)
        VAR = SX2 / FLOAT(N - 1)
       SD = SQRT(VAR)



C  *****  MOMENT COEF OF SKEWNESS:  SKEWED RIGHT (6ONGER TAIL TO
C  *****      RIGHT) IF POSITIVE;  SKEWED LEFT (LONGER TAIL TO
C  *****     LEFT) IF NEGATIVE

       A3 = (SX3/FLOAT(N)) / (VAR*SD)


C  *****  MOMENT COEF OF KURTOSIS WITH RESPECT TO NORMAL:
C  *****     LEPTO (PEAKED) IF POSITIVE; PLATY (FLAT) IF NEGATIVE


       A4 = (SX4/FLOAT(N)) / (VAR*VAR)  -  3.


C  *****  DETERMINE MAXIMUM AND MINIMUM VALUES AND THE RANGE



       ZX = X(1)
       ZN = ZX

       DO  3  I = 2,N
         IF(X(I) .GT. ZX) ZX = X(I)
         IF(X(I) .LT. ZN) ZN = X(I)
   3   CONTINUE

       R = ZX - ZN
         IF(ZX .LT. 0.) R = ABS(R)


       RETURN
       END


       subroutine datecnv4(date1,iy,im,id,ih)

C$$$  SUBROUTINE DOCUMENTATION BLOCK
C
C SUBPROGRAM: datecnv4
C   PRGMMR: LILLY            ORG: NP12        DATE: 2009-09-23
C
C ABSTRACT:
C
C PROGRAM HISTORY LOG:
C
C 2009-09-23 Steven G. Lilly --  Updated sorc codes
C
C USAGE:
C   INPUT FILES:
C     FT05FT001   -
C
C
C   OUTPUT FILES:
C     FT06FT001   -
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

        
C  this subroutine takes an 10 digit integer date word yyyymmddhh format
C  and breaks it up into 4 integer parts.
 
       character*10 cdate
       character*4 cyear
       character*2 cmonth,cday,chour
       integer date1
       write (cdate,900) date1
       cyear=cdate(1:4)
       cmonth=cdate(5:6)
       cday=cdate(7:8)
       chour=cdate(9:10)
       read (cyear,800) iy
       read (cmonth,810) im
       read (cday,810) id
       read (chour,810) ih
 800    format (i4)
 810    format (i2)
 900    format (i10)
       return
       end
