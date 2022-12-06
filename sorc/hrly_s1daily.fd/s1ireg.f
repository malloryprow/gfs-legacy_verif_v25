      SUBROUTINE S1IREG(O,F,IBGN,IEND,IDIM,JDIM,JBGN,JEND,JDMN,
     1                  S1,S1B,SERR,SGRD,SGRDA,SGF2A)
C$$$  SUBPROGRAM DOCUMENTATION  BLOCK
C
C SUBPROGRAM:    S1IREG
C   PRGMMR: ROBERT HIRANO    ORG: W/NMC42    DATE: 88-07-27
C
C ABSTRACT: COMPUTE THE S1 SCORE FOR AN IRREGULARLY SHAPED SUBAREA
C
C PROGRAM HISTORY LOG:
C   88-07-27  ROBERT HIRANO
C
C USAGE:    CALL S1IREG(O,F,IBGN,IEND,IDIM,JBGN,JEND,JDMN,S1,S1B,
C                       SERR,SGRD,SGRDA,SGF2A)
C   INPUT ARGUMENT LIST:
C     O        - THE VERIFYING FIELD
C     F        - THE FORECAST FIELD
C     IBGN     - MIN I(COLUMN) PT FOR EACH J(ROW) PT
C     IEND     - MAX I(COLUMN) PT FOR EACH J(ROW) PT
C     IDIM     - I DIMENSION OF THE FIELD
C     JDIM     - J DIMENSION OF THE FIELD
C     JBGN     - MIN J(ROW) POINT
C     JEND     - MAX J(ROW) POINT
C     JDMN     - J DIMENSION OF VERIFICATION AREA
C
C   OUTPUT ARGUMENT LIST:
C     S1       - S1 SCORE FOR THE SUBAREA
C     S1B      - ALTERNATE S1 SCORE (W.R.T. ANL GRAD)
C     SERR     - SUM OF THE FCST ERROR OVER THE SUBAREA
C     SGRD     - SUM OF THE MAX GRADIENT OVER THE SUBAREA
C     SGRDA    - SUM OF THE VERIFYING GRADIENT OVER THE SUBAREA
C     SGF2A    - RATIO OF FORECAST GRADIENT TO ANALYSIS GRADIENT
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN-90
C   MACHINE:  IBM RS6000
C
C$$$
C
C
        DIMENSION O(IDIM,JDIM),F(IDIM,JDIM)
        DIMENSION IBGN(JDMN),IEND(JDMN)
C
       SERR   = 0.
       SGRD = 0.
       SGRDA = 0.
       SGRDF = 0.
       JM1 = JEND - 1
C
C...........................................................
C... SUM FCST GRAD ERR AND MAX GRAD ALONG I DIRECTION ...
C...........................................................
C
       N = 0
       LOOP20: DO J = JBGN,JEND
       N = N + 1
       I1 = IBGN(N)
       IM1 = IEND(N) - 1
       IF(IBGN(N) .EQ. IEND(N)) CYCLE LOOP20
         LOOP10: DO I = I1,IM1
           FG = F(I+1,J) - F(I,J)
           OG = O(I+1,J) - O(I,J)
           SERR = SERR + ABS(FG-OG)
           SGRD = SGRD + AMAX1(ABS(FG),ABS(OG))
           SGRDA = SGRDA + ABS(OG)
           SGRDF = SGRDF + ABS(FG)
         END DO LOOP10
       END DO LOOP20
C
C...........................................................
C... SUM FCST GRAD ERR AND MAX GRAD ALONG J DIRECTION ...
C...........................................................
C
       N = 0
       LOOP40: DO J = JBGN,JM1
       N = N + 1
       NP1 = N + 1
       I1 = MAX0(IBGN(N),IBGN(NP1))
       I2 = MIN0(IEND(N),IEND(NP1))
         LOOP30: DO I = I1,I2
           FG = F(I,J+1) - F(I,J)
           OG = O(I,J+1) - O(I,J)
           SERR = SERR + ABS(FG-OG)
           SGRD = SGRD + AMAX1(ABS(FG),ABS(OG))
           SGRDA = SGRDA + ABS(OG)
           SGRDF = SGRDF + ABS(FG)
         END DO LOOP30
       END DO LOOP40
C
C...........................................................
C...  COMPUTE S1 SCORE ...
C...........................................................
C
       S1  = 100. * SERR / SGRD
       S1B = 100. * SERR / SGRDA
C
       SGF2A = SGRDF / SGRDA
C
       RETURN
       END

