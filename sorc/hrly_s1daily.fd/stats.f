       SUBROUTINE STATS(X,N,XM,AD,VAR,SD,A3,A4,ZX,ZN,R)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    STATS       CALCULATES A STATISTICAL SET
C   PRGMMR: R. Y. HIRANO     ORG: W/NMC42    DATE: 88-09-19
C
C ABSTRACT: ANALYZE A SET OF SCORES STATISTICALLY (MEAN,STDEV,ECT)
C
C PROGRAM HISTORY LOG:
C   88-09-19  ROBERT Y. HIRANO
C
C USAGE:    CALL STATS(X,N,XM,AD,VAR,SD,A3,A4,ZX,ZN,R)
C   INPUT ARGUMENT LIST:
C     X        - A SET OF VALUES.
C     N        - NUMBER OF VALUES IN ARRAY X.
C
C   OUTPUT ARGUMENT LIST:
C     XM       - FOR ARRAY X...THE MEAN
C     AD       -               AVERAGE DEVIATION FROM THE MEAN
C     VAR      -               VARIANCE
C     SD       -               STANDARD DEVIATION
C     A3       -               SKEWNESS OF THE DISTRIBUTION
C     A4       -               KURTOSIS (W.R. TO NORMAL)
C     ZX       -               MAXIMUM VALUE
C     ZN       -               MINIMUM VALUE
C     R        -               RANGE
C
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C   MACHINE:  NAS
C
C$$$
C
C
C
       DIMENSION X(N)
C
       SX = 0.
       AD = 0.
       SX2 = 0.
       SX3 = 0.
       SX4 = 0.
C
C...... 1. SUM VALUES, CALCULATE MEAN
C       2. SUM DEVIATION FROM MEAN
C            FIRST THRU FOURTH MOMENTS
C            THEN, CALCULATE ABSOLUTE DEVIATION, VARIANCE,
C                  AND STANDARD DEVIATION ....................
C
       DO  I = 1,N
         SX = SX + X(I)
       END DO
C
       XM = SX / N
C
       DO  I = 1,N
         XMM = X(I) - XM
         X2 = XMM * XMM
         X3 = XMM * X2
         X4 = X2 * X2
C
         AD = AD + ABS(XMM)
         SX2 = SX2 + X2
         SX3 = SX3 + X3
         SX4 = SX4 + X4
C
       END DO
C
C
       AD = AD / N
       VAR =  SX2 / (N - 1)
       SD = SQRT(VAR)
C
C
C...... 3. MOMENT COEF OF SKEWNESS: SKEWED RIGHT (LONGER TAIL TO
C            RIGHT) IF POSITIVE;   SKEWED LEFT (LONGER TAIL TO
C            LEFT) IF NEGATIVE ...................................
C...... 4. MOMENT COEF OF KURTOSIS WITH RESPECT TO NORMAL:
C            LEPTO (PEAKED) IF POSITIVE; PLATY (FLAT) IF NEGATIVE
C
C
       A3 = (SX3/N) / (VAR*SD)
       A4 = (SX4/N) / (VAR*VAR)  -  3.
C
C
C...... 5. DETERMINE MAXIMUM AND MINIMUM AND THE RANGE ............
C
       ZX = X(1)
       ZN = ZX
C
       DO  I = 2,N
         IF(X(I) .GT. ZX) ZX = X(I)
         IF(X(I) .LT. ZN) ZN = X(I)
       END DO
C
       R = ZX - ZN
         IF(ZX .LT. 0.) R = ABS(R)
C
       RETURN
       END

