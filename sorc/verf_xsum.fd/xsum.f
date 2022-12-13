C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C
C MAIN PROGRAM: xsum
C   PRGMMR: LILLY            ORG: NP12        DATE: 2009-03-09
C
C ABSTRACT: GENERATES MONTHLY SUMMARY OF WMO VERIFICATION SCORES
C           FOR GFS verified against the GFS SSI ANALYSIS
C
C PROGRAM HISTORY LOG:
C   1994-03-09  Original Author: Charles Vlcek
C
C USAGE:
C   INPUT FILES:
C    FT10F001  -  begin and end date of verification period
C    FT21F001  -  mslp scores
C    FT22F001  -  850mb hgt scores
C    FT23F001  -  850mb tmp scores
C    FT24F001  -  500mb hgt scores
C    FT25F001  -  500mb tmp scores
C    FT26F001  -  250mb hgt scores
C    FT27F001  -  250mb tmp scores
C
C   OUTPUT FILES:
C    FT31-37F001   -  monthly verification tables
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

C     PROGRAM XSUM

       PARAMETER (JFCTX=10,MXCYL=31,JMDLX=4,JSTTX=10,irgn=3)
       CHARACTER(17) NAMRGN(3)
       CHARACTER(13) NVGRD
       CHARACTER(10) ITRUTH
       CHARACTER(9) PLEVEL(7)
       CHARACTER(7) NSTAT(5)
       CHARACTER(5)  JSTAT(JSTTX)
       CHARACTER(3) NAME,NTRU,MONTH(12),GNAME(2),GRDN
       REAL    SX(jfctx,MXCYL,15),X(JSTTX, 5,JFCTX), W(31)
       integer nbegin(4),nend(4),idate(mxcyl)
C
       DATA NVGRD/'2.5DEG LATLON'/
       DATA NAMRGN/'NO HEM (20-90DEG)', 'SO HEM (20-90DEG)',
     1             'TROPICS (20S-20N)'/
       DATA NSTAT/'FCT ERR','RMS ERR','COR CHG','S1(MAX)',
     1            'S1(TRU)'/
       DATA JSTAT/'  NUM',' MEAN','ABSDV','  VAR',' SDEV',' SKEW',
     1           ' KURT','  MAX','  MIN','RANGE'/
       DATA MONTH/'JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG',
     1            'SEP','OCT','NOV','DEC'/
       data jfcts/10/
       data ntru/'SSI'/
       data itruth/' ANALYSIS'/
       data name/'GFS'/
       data plevel/'  PMSL   ',' 850 HGT ',' 850 TMP ',' 500 HGT ',
     1             ' 500 TMP ',' 250 HGT ',' 250 TMP '/ 
 
 
C      DO STATISTICAL ANALYSIS

 100   format(i10,1x,i10)
 101   format(i10)
 110   format(5f7.1,2x,5f7.1,2x,5f7.1)
 
C read in beginning and ending date for monthly summary (ending time reset to 00z, since this is mrf);
C write date label (year and month) on archive file vwmomon (ft70)

       read(10,100) mbegin,mend
 
       call datecnv4(mbegin,nbegin(1),nbegin(2),nbegin(3),nbegin(4))

       call datecnv4(mend,nend(1),nend(2),nend(3),nend(4))

       sx = 0.0
       X  = 0.0
       nend(4) = 0
 
       write (70,270)  nbegin(1), nbegin(2)
 270   format (i4,i2,74x)
 
       XX = 999.9
 
       infile=20
       do iff=1,7
       ic=1
         infile=infile+1
         rewind (infile)
 
  50     read(infile,101,end=900) idate(ic)
         do ihr=1,jfctx
           read(infile,110) (sx(ihr,ic,istat),istat=1,15)
         enddo
         ic=ic+1
         goto 50
 900     continue
 
         do ir=1,irgn
           DO  I = 1,5
             is=5*(ir-1)+I
             do 12 ihr=1,jfcts
               NC = 0
               DO 14 iday=1,ic-1
                 IF(SX(ihr,iday,is) .EQ. XX) GO TO 14
                 NC = NC + 1
                 if((is .eq. 3).or.(is .eq. 8).or.(is .eq. 13)) then
                   W(NC) = SX(ihr,iday,is)/100.0
                 else
                   W(NC) = SX(ihr,iday,is)
                 endif
  14           continue
 
             IF(NC .EQ. 0) GO TO 12
               CALL STATS(W,NC,XM,AD,VAR,SD,A3,A4,ZX,ZN,R)
               X(1,I,ihr) = FLOAT(NC)
               X(2,I,ihr) = XM
               X(3,I,ihr) = AD
               X(4,I,ihr) = VAR
               X(5,I,ihr) = SD
               X(6,I,ihr) = A3
               X(7,I,ihr) = A4
               X(8,I,ihr) = ZX
               X(9,I,ihr) = ZN
               X(10,I,ihr) = R
  12         continue
           enddo
            
C      PRINT STATISTICAL ANALYSIS TABLES FOR EACH FORECAST HOUR
 
 400       FORMAT(/,15X,'..... STATISTICAL ANALYSIS OF ... ',A7,
     1            ' ON ', A13,' GRID ... ', A17,' .....',/,
     2            9X,'.. ',A3,' ... VERIFIED AGAINST ',
     3            A3, 1X, A9,'  FOR ',A8,'  FOR THE PERIOD  ',I2,
     4            'Z', I3,A3,I4,' - ',I2,'Z',I3,A3,I4,' ..',//,
     5            9X, 'FTHR         ',4X,10(3X,A5),//)
 410       FORMAT(9X,I4,27X,F7.0, 10F8.3,/)
 430       FORMAT('0')
 440       FORMAT('1')
 450       FORMAT(/////)
 
           DO 32 L = 1,5
             iftinc=24
 
             MONB = NBEGIN(2)
             MONE = NEND(2)

             PRINT 400,NSTAT(L),NVGRD,NAMRGN(IR),NAME,NTRU,
     1            ITRUTH,PLEVEL(iff),NBEGIN(4),NBEGIN(3),
     2            MONTH(MONB),NBEGIN(1),NEND(4),NEND(3),MONTH(MONE),
     3            NEND(1),(JSTAT(I),I=1,JSTTX)

             DO 40 N = 1,jfcts
               IF(X(1,1,N) .EQ. 0.) GO TO 40
               NFTHR = IFTINC * N
               PRINT 410, NFTHR, (X(I,L,N),I=1,JSTTX)
  40         CONTINUE

  32       CONTINUE

           if ((infile.eq.21).and.(ir.le.2)) then
             lvl=1
           elseif(((infile.eq.22).or.(infile.eq.23)).and.(ir.eq.3)) then
             lvl=1
           elseif(((infile.eq.24).or.(infile.eq.25)).and.(ir.le.2)) then
             lvl=2
           elseif(((infile.eq.26).or.(infile.eq.27)).and.(ir.eq.3)) then
             lvl=2
           elseif(((infile.eq.26).or.(infile.eq.27)).and.(ir.le.2)) then
             lvl=3
           endif

C......WRITE SELECTIVE DATA TO DISK FOR VFCN TABLES......
        CALL WRTDSK(IR,LVL,infile,JSTTX,JFCTX,X)
 
       enddo
       enddo
       stop  
       END

       SUBROUTINE WRTDSK(IRGN,LVL,infile,JSTTX,JFCTX,X)

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

C......WRITE SELECTIVE PARAMETERS TO ARCHIVE WMO MONTHLY TABLE....
C......HEIGHT(MERR,RMSE,correl,S1),TEMPERATURE(MERR,RMSE,correl)..
C......NHEM,SHEM(MSL,500MB,250MB),TROPICS(850MB,250MB)............
C
       DIMENSION X(JSTTX,5,JFCTX)
 
 100   FORMAT(3I2,10F8.2)
 
C  ip1-ip4 (column 1-4)  hgt...merr, rmse, correl, s1
C  ip5-ip7 (column 5-7)  tmp...merr, rmse, correl
 
       ip1=1
       ip2=2
       ip3=3
       ip4=4
       ip5=5
       ip6=6
       ip7=7


       IF(LVL .EQ. 5) GO TO 10

       IF(IRGN .ne. 3) then 
          goto 15
       elseif (IRGN .eq. 3) then
          goto 16
       endif

C......DO NORTH OR SOUTH HEMISPHERE.........................
  15   if ((lvl .eq. 1) .and. (infile .eq. 21)) then
          WRITE(70,100) IRGN,LVL,IP1,(X(2,1,I),I=1,10)
          WRITE(70,100) IRGN,LVL,IP2,(X(2,2,I),I=1,10)
          WRITE(70,100) IRGN,LVL,IP3,(X(2,3,I),I=1,10)
          WRITE(70,100) IRGN,LVL,IP4,(X(2,4,I),I=1,10)
       elseif ((lvl .eq. 2) .and. (infile .eq. 24)) then
          WRITE(70,100) IRGN,LVL,IP1,(X(2,1,I),I=1,10)
          WRITE(70,100) IRGN,LVL,IP2,(X(2,2,I),I=1,10)
          WRITE(70,100) IRGN,LVL,IP3,(X(2,3,I),I=1,10)
          WRITE(70,100) IRGN,LVL,IP4,(X(2,4,I),I=1,10)
       elseif ((lvl .eq. 2) .and. (infile .eq. 25)) then
          WRITE(70,100) IRGN,LVL,IP5,(X(2,1,I),I=1,10)
          WRITE(70,100) IRGN,LVL,IP6,(X(2,2,I),I=1,10)
          WRITE(70,100) IRGN,LVL,IP7,(X(2,3,I),I=1,10)
       elseif ((lvl .eq. 3) .and. (infile .eq. 26)) then
          WRITE(70,100) IRGN,LVL,IP1,(X(2,1,I),I=1,10)
          WRITE(70,100) IRGN,LVL,IP2,(X(2,2,I),I=1,10)
          WRITE(70,100) IRGN,LVL,IP3,(X(2,3,I),I=1,10)
          WRITE(70,100) IRGN,LVL,IP4,(X(2,4,I),I=1,10)
       elseif ((lvl .eq. 3) .and. (infile .eq. 27)) then
          WRITE(70,100) IRGN,LVL,IP5,(X(2,1,I),I=1,10)
          WRITE(70,100) IRGN,LVL,IP6,(X(2,2,I),I=1,10)
          WRITE(70,100) IRGN,LVL,IP7,(X(2,3,I),I=1,10)
       endif

       goto 10

C......DO TROPICS...........................................

  16    IF ((LVL .EQ. 1)  .and. (infile .eq. 22)) then
          WRITE(70,100) IRGN,LVL,IP1,(X(2,1,I),I=1,10)
          WRITE(70,100) IRGN,LVL,IP2,(X(2,2,I),I=1,10)
          WRITE(70,100) IRGN,LVL,IP3,(X(2,3,I),I=1,10)
          WRITE(70,100) IRGN,LVL,IP4,(X(2,4,I),I=1,10)
        elseif ((lvl .eq. 1) .and. (infile .eq. 23)) then
          WRITE(70,100) IRGN,LVL,IP5,(X(2,1,I),I=1,10)
          WRITE(70,100) IRGN,LVL,IP6,(X(2,2,I),I=1,10)
          WRITE(70,100) IRGN,LVL,IP7,(X(2,3,I),I=1,10)
        elseif ((lvl .eq. 2) .and. (infile .eq. 26)) then
          WRITE(70,100) IRGN,LVL,IP1,(X(2,1,I),I=1,10)
          WRITE(70,100) IRGN,LVL,IP2,(X(2,2,I),I=1,10)
          WRITE(70,100) IRGN,LVL,IP3,(X(2,3,I),I=1,10)
          WRITE(70,100) IRGN,LVL,IP4,(X(2,4,I),I=1,10)
        elseif ((lvl .eq. 2) .and. (infile .eq. 27)) then
          WRITE(70,100) IRGN,LVL,IP5,(X(2,1,I),I=1,10)
          WRITE(70,100) IRGN,LVL,IP6,(X(2,2,I),I=1,10)
          WRITE(70,100) IRGN,LVL,IP7,(X(2,3,I),I=1,10)
       ENDIF


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


       XM = SX / FLOAT(N)


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



C  *****  MOMENT COEF OF SKEWNESS:  SKEWED RIGHT (LONGER TAIL TO
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
C SUBPROGRAM: DATECNV4
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

C  this subroutine takes an 10 digit integer date word yyyymmddhh       
C  format and breaks it up into 4 integer parts.

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
