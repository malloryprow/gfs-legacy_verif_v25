       SUBROUTINE INTRFM(FLD,II,JJ,FMANL,ITYP)
C$$$  SUBPROGRAM DOCUMENTATION  BLOCK
C
C SUBPROGRAM:   INTRFM
C   PRGMMR: ROBERT HIRANO    ORG: W/NMC42    DATE: 88-07-27
C
C ABSTRACT: Interpolate field to NAM grid 26 subset, NAM area
C           1  ITYP-1 2.5 Latlon Grid 29 to NAM GRID 26 (53,45)
C           ITYP-2  NGM C Grid 105 to NAM GRID 26 (53,46)
C
C PROGRAM HISTORY LOG:
C   88-07-27  ROBERT HIRANO
C
C USAGE:    CALL INTRFM(FLD,II,JJ,FMANL,ITYP)
C
C   INPUT ARGUMENT LIST:
C
C   OUTPUT ARGUMENT LIST:
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN-90
C   MACHINE:  IBM RS6000
C
C$$$
C
C      .........................................................
C      .  INTERPOLATE FIELD TO LFM GRID 26 SUBSET, LFM AREA 1  .
C      .  ITYP=1  2.5 LATLON GRID 29 TO LFM GRID 26 (53,45)    .
C      .  ITYP=2  NGM C GRID 105 TO LFM GRID 26 (53,46)        .
C      .........................................................
C      .  PASS LATLON OF LFM GRID 26 IN COMMON ZDATA           .
C      .    XLTLN(1, , )=LAT      XLTLN(2, , )=LONG            .
C      .........................................................
C
       DIMENSION FLD(II,JJ),FMANL(53,57)
       DATA IB,IE,JB,JE/ 17,45,  11,33/
       COMMON /ZDATA/ XLTLN(2,53,45)
C
       IF(ITYP  .EQ.  1) THEN
        LOOP10: DO J = JB,JE
        LOOP12: DO I = IB,IE
         ALAT = XLTLN(1,I,J)
         ALON = XLTLN(2,I,J)
         ZI = 145. - ALON/2.5
         ZJ = ALAT/2.5 + 1.
         CALL W3FT01(ZI,ZJ,FLD,X,II,JJ,1,2)
         FMANL(I,J) = X
        END DO LOOP12
        END DO LOOP10
C
       ELSE
        XPOLE = 40.5
        YPOLE = 88.5
        XDX = 90.75464
        ORIENT = 105.
        LOOP20: DO J = JB,JE
        LOOP22: DO I = IB,IE
         ALAT = XLTLN(1,I,J)
         ALON = XLTLN(2,I,J)
         CALL W3FB04(ALAT,ALON,XDX,ORIENT,XI,XJ)
         ZI = XI + XPOLE
         ZJ = XJ + YPOLE
         CALL W3FT03(FLD,X,ZI,ZJ,II,JJ,2)
         FMANL(I,J) = X
        END DO LOOP22
        END DO LOOP20
C
       ENDIF
C
       RETURN
       END

