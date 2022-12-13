C ABSTRACT: C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C
C MAIN PROGRAM: namheadr
C   PRGMMR: LILLY            ORG: NP12        DATE: 2009-06-01
C
C ABSTRACT: Generates the NAM Headr File
C
C PROGRAM HISTORY LOG:
C
C 2006-12-24  Steve Lilly -- ORIGINAL AUTHOR
C
C USAGE:
C   INPUT FILES:
C     FT05F001  - reads the year, month and day of the NAM stat file
C
C   OUTPUT FILES:
C     FT06F001  - generates a header for the NAM WMO MONTHLY REPORT
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

      PROGRAM NAMHEADR

C     PROGRAM NAMHEADR.F  by  Steven Lilly
      CHARACTER*4 Iyear,x2
      CHARACTER*7 Itmp,x3
      CHARACTER*8 Ioutp,x4
      CHARACTER*9 Month,x1

      READ(5,*) x1,x2,x3,x4
      Iyear= x2
      Itmp= x3
      Ioutp= x4
      Month= x1

      WRITE(6,10) Month,Iyear,Itmp,Ioutp
   10 FORMAT(" sed -e 's/XXXXXXXXXXXXXX/",A9,1x,A4,"/g'",
     & " ",A7,1x,">",1x,A9)
      STOP
      END
