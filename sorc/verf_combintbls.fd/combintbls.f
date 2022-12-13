C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C
C MAIN PROGRAM: combintbls
C   PRGMMR: LILLY            ORG: NP12        DATE: 2009-06-01
C
C ABSTRACT: Combines both the 00Z daily statistics file 
C           and the 12Z daily statistics file into one
C           single file for the GFS model output.
C
C PROGRAM HISTORY LOG:
C
C 2008-06-01 Steven G. Lilly -- ORIGINAL AUTHOR
C 2009-10-01 Steven G. Lilly -- Convert onto the P5
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
 
      PROGRAM COMBINTBLS

      INTEGER IYEAR, IMON, L(3), M(3)
         REAL X(10), Y(10)

  100 FORMAT(I4,I2)
  200 FORMAT(I2,I2,I2,1X,F7.2,1X,F7.2,1X,F7.2,1X,F7.2,1X,F7.2,1X,F7.2,
     *       1X,F7.2,1X,F7.2,1X,F7.2,1x,F7.2)
  300 FORMAT(I2,I2,I2,1X,F7.2,1X,F7.2,1X,F7.2,1X,F7.2,1X,F7.2,1X,F7.2,
     *       1X,F7.2,1X,F7.2,1X,F7.2,1X,F7.2,1X,F7.2,1X,F7.2,1X,F7.2,
     *       1X,F7.2,1X,F7.2,1X,F7.2,1X,F7.2,1X,F7.2,1x,F7.2,1x,F7.2)

      REWIND 20
      REWIND 30

      READ(20,100) IYEAR, IMON
      READ(30,100) IYEAR, IMON

      WRITE(40,100) IYEAR, IMON

      IOS=0
  
      DO WHILE (IOS .EQ. 0)

        READ(20,200,IOSTAT=IOS) L(1), L(2), L(3), X(1), X(2),
     *        X(3), X(4), X(5), X(6), X(7), X(8), X(9), X(10)

        READ(30,200,IOSTAT=IOS) M(1), M(2), M(3), Y(1), Y(2),
     *        Y(3), Y(4), Y(5), Y(6), Y(7), Y(8), Y(9), Y(10)

        WRITE(40,300) L(1), L(2), L(3), X(1), X(2), X(3), X(4),
     *              X(5), X(6), X(7), X(8), X(9), X(10), Y(1),
     *              Y(2), Y(3), Y(4), Y(5), Y(6), Y(7), Y(8),
     *              Y(9), Y(10)
 

      ENDDO

      CLOSE(40)
      END
