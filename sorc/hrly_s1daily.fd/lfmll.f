       SUBROUTINE LFMLL
C$$$  SUBPROGRAM DOCUMENTATION  BLOCK
C
C SUBPROGRAM:   LFMLL
C   PRGMMR: ROBERT HIRANO    ORG: W/NMC42    DATE: 88-07-27
C
C ABSTRACT: Determine LATLON of NAM GRID 26 locations (53,45)
C           store in common ZDATA, XLTLN(1, , )=LAT
C           XLTLN(2, ,)=LOW
C
C PROGRAM HISTORY LOG:
C   88-07-27  ROBERT HIRANO
C
C USAGE:    CALL LFMLL
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
C    ...............................................................
C    .  DETERMINE LATLON OF LFM GRID 26 LOCATIONS (53,45)           .
C    .  STORE IN COMMON ZDATA, XLTLN(1, , )=LAT  XLTLN(2, , )=LON   .
C    ...............................................................
C
       COMMON /ZDATA/ XLTLN(2,53,45)
C
       ORIENT = 105.
       XDX = 190.5
       XPOLE = 27.
       YPOLE = 49.
       LOOP10: DO J  = 1,45
        LOOP12: DO I = 1,53
         XI = I - XPOLE
         XJ = J - YPOLE
         CALL W3FB05(XI,XJ,XDX,ORIENT,ALAT,ALON)
         XLTLN(1,I,J) = ALAT
         XLTLN(2,I,J) = 0.0 - ALON
         END DO LOOP12
       END DO LOOP10
C
       RETURN
       END

