       SUBROUTINE DATECNV8(date1,iy,im,id,ih)
C SUBPROGRAM:    DATECNV8
C   PRGMMR: LILLY            ORG: NP12        DATE: 2007-07-22
C
C ABSTRACT: THIS SUBROUTINE TAKES A 8 DIGIT INTEGER DATA WORD
C YYYYMMDDHH FORMAT AND BREAKS IT UP INTO A 4 INTEGER PARTS.
C
C PROGRAM HISTORY LOG:
C 2007-07-22 STEVEN G. LILLY -- UPDATING SOURCE CODES
C
C USAGE:    CALL DATECNV8(DATE1,IY,IM,ID,IH)
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
C  this subroutine takes an 8 digit integer date word yymmddhh format
C  and breaks it up into 4 integer parts.
C

       CHARACTER*8 cdate
       CHARACTER*2 cyear,cmonth,cday,chour
       INTEGER date1
 800   FORMAT (i2)
 900   FORMAT (i8)
       WRITE (cdate,900) date1
       cyear=cdate(1:2)
       cmonth=cdate(3:4)
       cday=cdate(5:6)
       chour=cdate(7:8)
       READ (cyear,800) iy
       READ (cmonth,800) im
       READ (cday,800) id
       READ (chour,800) ih
       RETURN
       END
