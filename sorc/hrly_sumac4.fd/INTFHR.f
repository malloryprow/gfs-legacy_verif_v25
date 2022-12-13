      
      FUNCTION INTFHR(CFHR)

C     THIS FUNCTION GETS INTEGER HOURS FROM ITS ASCII FORMAT 'FXXX'.     

      INTEGER      LHR(3)
      CHARACTER(4) CFHR    , CTMP
      CHARACTER(1) CFHR1(4), CDIGIT(10)
      EQUIVALENCE  (CTMP,CFHR1(1))
      DATA CDIGIT  /'0','1','2','3','4','5','6','7','8','9'/

      CTMP = CFHR
      DO I = 2,4
      DO J = 1,10
      IF(CFHR1(I).EQ.CDIGIT(J)) THEN
         LHR(I-1) = J - 1
      END IF
      END DO
      END DO
      INTFHR = LHR(1)*100 + LHR(2)*10 + LHR(3)
      RETURN
      END
