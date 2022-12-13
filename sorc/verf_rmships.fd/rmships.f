C ABSTRACT: C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C
C MAIN PROGRAM: rmships
C   PRGMMR: LILLY            ORG: NP12        DATE: 2009-06-01
C
C ABSTRACT: To rmove ship reports having fewer than 10 observations
C           for 6 out of the 11 pressure levels 
C
C PROGRAM HISTORY LOG:
C
C 2008-10-24  Steve Lilly -- ORIGINAL AUTHOR
C
C USAGE:
C   INPUT FILES:
C        FT05 -- The monthly summary table for ship-based radiosonde observation
C
C   OUTPUT FILES:
C        FT06 -- Monthly summary table of ship reports having more than 10 
C                observations for 9 out of the 11 pressure levels
C
C      OUTPUT FILES:
C        FT06 -- PRINTOUT OF JOB STATUS, OPTIONAL PRINTOUT OF STATS
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

      PROGRAM RMSHIPS

       CHARACTER*1 x(78)
       CHARACTER*1 y(14,78)
 100  format(78A1)
      DO I=1,999999
         jcnt = 0
        READ(5,100,end=200) ( x(J), J=1,78 ) 
           icnt = 0
          IF(x(2).EQ."S")   THEN 
           DO J=1,14
             READ(5,100) ( y(J,K), K=1,78 )
               IF(y(J,20).GE."1")   THEN
                 icnt =  icnt + 1
               ELSE
                 jcnt = 1
               ENDIF
           ENDDO
           ENDIF
             IF(icnt.GE.6)   THEN
               print*,x
               DO M=1,14
                 print*,(y(M,L), L=1,78 )
               ENDDO
             ENDIF
         
           IF(icnt.EQ.0.AND.jcnt.EQ.0)   THEN
            print*,x
           ENDIF
           IF(icnt.EQ.0.AND.jcnt.EQ.1)   THEN
            print*,'1',x
           ENDIF
      ENDDO
200   CONTINUE
      END

