      SUBROUTINE S1RECT(O,F,I,J,IB,IE,JB,JE,IDOI,IDOJ,
     1           S1,S1B,SERR,SGRD,SGRDA,SGF2A)
C$$$  SUBPROGRAM DOCUMENTATION  BLOCK
C
C SUBPROGRAM:    S1RECT
C   PRGMMR: ROBERT HIRANO    ORG: W/NMC42    DATE: 88-07-20
C
C ABSTRACT: COMPUTE THE S1 SCORE FOR ANY RECTANGULAR AREA, SUBAREA,
C           OR IN JUST I- OR J-DIRECTIONS.
C
C PROGRAM HISTORY LOG:
C   88-07-20  ROBERT HIRANO
C
C USAGE:    CALL S1RECT(O,F,I,J,IB,IE,JB,JE,IDOI,IDOJ,
C                S1,S1B,SERR,SGRD,SGRDA,SGF2A)
C   INPUT ARGUMENT LIST:
C     O        - THE VERIFYING FIELD
C     F        - THE FORECAST FIELD
C     I        - I DIMENSION OF THE FIELD
C     J        - J DIMENSION OF THE FIELD
C     IB       - SUBAREA MIN I POINT
C     IE       - SUBAREA MAX I POINT
C     JB       - SUBAREA MIN J POINT
C     JE       - SUBAREA MAX J POINT
C     IDOI     - DO THE I-DIRECTION (1=YES,0=NO)
C     IDOJ     - DO THE J-DIRECTION (1=YES,0=NO)
C
C   OUTPUT ARGUMENT LIST:
C     S1       - S1 SCORE FOR THE SUBAREA
C     S1B      - ALTERNATE S1 SCORE (W.R.T. ANL GRAD)
C     SERR     - SUM OF THE FCST ERROR OVER THE SUBAREA
C     SGRD     - SUM OF THE MAX GRADIENT OVER THE SUBAREA
C     SGRDA    - SUM OF THE TRUE GRADIENT OVER THE SUBAREA
C     SGF2A    - RATIO OF FORECAST GRADIENT TO ANALYSIS GRADIENT
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN-90
C   MACHINE:  IBM RS6000
C
C$$$
C
C
        logical(1)  kprint
        data kprint /.FALSE./
        DIMENSION O(I,J),F(I,J)
C
 100   FORMAT('   FOR SUBAREA (IB,IE,JB,JE) ',4I5,4X,'........')
 110   FORMAT(' SERR = ',E12.5,2X,'SGRD = ',E12.5,' .....I DIR')
 120   FORMAT(' SERR = ',E12.5,2X,'SGRD = ',E12.5,' .....J DIR')
C
       IF(KPRINT) PRINT 100,IB,IE,JB,JE
       SERR   = 0.
       SGRD = 0.
       SGRDA = 0.
       SGRDF = 0.
       IM1 = IE - 1
       JM1 = JE - 1
C
C...........................................................
C...  SUM THE FCST GRAD ERROR ALONG I DIRECTION ...
C...........................................................
C
       LOOP50: DO MM=1,1
       IF(IDOI .NE. 1) CYCLE LOOP50
       LOOP20: DO JJ = JB,JE
         LOOP10: DO II = IB,IM1
           FG = F(II+1,JJ) - F(II,JJ)
           OG = O(II+1,JJ) - O(II,JJ)
           if(f(ii,jj).eq.0.0)then
           endif
           SERR = SERR + ABS(FG-OG)
           SGRD = SGRD + AMAX1(ABS(FG),ABS(OG))
           SGRDA = SGRDA + ABS(OG)
           SGRDF = SGRDF + ABS(FG)
         END DO LOOP10
       END DO LOOP20
       SERR1 = SERR
       SGRD1 = SGRD
       IF(KPRINT) PRINT 110,SERR,SGRD
C
       END DO LOOP50

C...........................................................
C...  SUM THE FCST GRAD ERROR ALONG J DIRECTION ...
C...........................................................
C
       LOOP60: DO MX=1,1
       IF(IDOJ .NE. 1)  CYCLE LOOP60
       LOOP40: DO JJ = JB,JM1
         LOOP30: DO II = IB,IE
           FG = F(II,JJ+1) - F(II,JJ)
           OG = O(II,JJ+1) - O(II,JJ)
           if(f(ii,jj).eq.0.0)then
           endif
           SERR = SERR + ABS(FG-OG)
           SGRD = SGRD + AMAX1(ABS(FG),ABS(OG))
           SGRDA = SGRDA + ABS(OG)
           SGRDF = SGRDF + ABS(FG)
         END DO LOOP30
       END DO LOOP40
       SERR1 = SERR - SERR1
       SGRD1 = SGRD - SGRD1
       IF(KPRINT) PRINT 120,SERR1,SGRD1
C
       END DO LOOP60
C...........................................................
C...   COMPUTE S1 SCORE ...
C...........................................................
C
       S1  = 100. * SERR / SGRD
       S1B = 100. * SERR / SGRDA
C
       SGF2A = SGRDF / SGRDA
C
       RETURN
       END

