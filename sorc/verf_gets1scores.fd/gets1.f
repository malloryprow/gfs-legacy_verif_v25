C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C
C MAIN PROGRAM: gets1
C   PRGMMR: LILLY            ORG: NP12        DATE: 2009-06-01
C
C ABSTRACT: Get the monthly 36hr and 72 hr S1 score for the GFS and NAM
C           models at the MSL and 500MB levels 
C
C PROGRAM HISTORY LOG:
C
C 2008-06-01 Steven G. Lilly -- ORIGINAL AUTHOR
C
C USAGE:
C   INPUT FILES:
C     FT50F001  - The GFS or NAM 36hr and 72hr S1 Score for the MSL 
C                 and 500MB levels
C
C   OUTPUT FILES:
C     FT40F001  - The files which stores the GFS and NAM
C                 36hr and 72hr S1 Score for the MSL and 500MB levels 
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

      PROGRAM GETS1

      CHARACTER(3) x(18), MODEL

      DATA IFILE /50/

  50  FORMAT(A3)
 100  FORMAT(4x,18(A3,1x))

      READ(50,50) MODEL

      DO I=1,4
 
      READ(IFILE,100,end=200) ( x(J), J=1,18 ) 

      IF ( MODEL .EQ. "GFS" ) THEN
       IF ( I .EQ. 1 ) THEN
        WRITE(30,*) x(5)
        ENDIF
        IF ( I .EQ. 2 ) THEN
         WRITE(40,*) x(14)
        ENDIF
      ENDIF

      IF ( MODEL .EQ. "NAM" ) THEN
       IF ( I .EQ. 2 ) THEN
        WRITE(30,*) x(5)
       ENDIF
        IF ( I .EQ. 3 ) THEN
         WRITE(40,*) x(14)
        ENDIF
      ENDIF

      ENDDO

200   CONTINUE
      END

