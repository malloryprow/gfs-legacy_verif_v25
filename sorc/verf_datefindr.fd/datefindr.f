C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C
C MAIN PROGRAM: datefindr
C   PRGMMR: LILLY            ORG: NP12        DATE: 2009-06-01
C
C ABSTRACT: Find the month, day and year of the data file
C
C PROGRAM HISTORY LOG:
C
C 2008-06-01 Steven G. Lilly -- ORIGINAL AUTHOR
C
C USAGE:
C   INPUT FILES:
C     FT20F001  - 00Z Monthly Statistics table
C     FT30F001  - 12Z Monthly Statistics table
C
C   OUTPUT FILES:
C      FT40F001 - Single table consisting of both
C                 the 00Z table and the 12Z table
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

      PROGRAM DATEFINDR

      CHARACTER*4 Iyear,x1
      CHARACTER*2 Imonth,x2,Jtmp1,Jtmp2
      CHARACTER*2 Iday,x3
      CHARACTER*7 Itmp,x4
      CHARACTER*8 Ifile,x5
 
      READ(5,*) x1,x2,x3,x4,x5
      Iyear= x1
      Imonth= x2
      Iday= x3
      Itmp= x4
      Ifile= x5
      Jtmp1= "s1"
      Jtmp2= "s2"

      WRITE(6,10) Iyear,Itmp,Jtmp1
   10 FORMAT(" sed -e 's/YYYY/",A4,"/g'",
     & " ",A7,1x,">",1x,A2)
      WRITE(6,12) Imonth,Jtmp1,Jtmp2
   12 FORMAT(" sed -e 's/MM/",A2,"/g'",
     & " ",A2,1x,">",1x,A2)
      WRITE(6,14) Iday,Jtmp2,Ifile
   14 FORMAT(" sed -e 's/DD/",A2,"/g'",
     & " ",A2,1x,">",1x,A8)
      STOP
      END
