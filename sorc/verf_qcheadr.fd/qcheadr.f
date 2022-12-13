C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C
C MAIN PROGRAM: QCHEADR
C   PRGMMR: LILLY            ORG: NP12        DATE: 2009-06-01
C
C ABSTRACT: Generates the GFS Headr file
C
C PROGRAM HISTORY LOG:
C
C 2008-06-01 Steven G. Lilly --  ORIGINAL AUTHOR
C
C USAGE:
C   INPUT FILES:
C     FT05FT001   -  FIRST CHECK TO SEE WHETHER OLD (2-DIGIT YEAR)
C                    OR NEW (4-DIGIT YEAR) FORMAT;
C
C   OUTPUT FILES:
C     FT06FT001   -  generates a header for the GFS WMO MONTHLY REPORT
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

      PROGRAM QCHEADR

      CHARACTER*4 Iyear,x2
      CHARACTER*8 Itmp,x3
      CHARACTER*9 Ioutp,x4
      CHARACTER*9 Month,x1
 
      READ(5,*) x1,x2,x3,x4
      Iyear= x2
      Itmp= x3
      Ioutp= x4
      Month= x1
 
      WRITE(6,10) Month,Iyear,Itmp,Ioutp
   10 FORMAT(" sed -e 's/XXXXXXXXXXXXXX/",A9,1x,A4,"/g'",
     & " ",A8,1x,">",1x,A9)
      STOP
      END
