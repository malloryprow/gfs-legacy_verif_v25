       SUBROUTINE LOLACALC(rlon2,rlat2)
c
c  subprogram lolacalc
c  programmer:  Andrew Krein
c  Abstract:  Calculates the longitude and latitude points for a
c             2.5 deg grid over the regional model verification
c             area.
c
c  Program History Log:
c    96-07-31  Andrew Krein
c
c  Usage:  call lolacalc(rlon2,rlat2)
c
c  Input argument list:  None
c
c  Output argument list:
c     rlon2:  longitude points
c     rlat2:  latitude points
c
       real rlon2(646), rlat2(646)
       integer x
c
       del=0.
       DO x=1,38
          rlon2(x)=-145.+del
          rlat2(x)=20.
          del=del+2.5
       END DO
       del=0.
       DO x=39,76
          rlon2(x)=-145.+del
          rlat2(x)=22.5
          del=del+2.5
       END DO
       del=0.
       DO x=77,114
          rlon2(x)=-145.+del
          rlat2(x)=25.
          del=del+2.5
       END DO
       del=0.
       DO x=115,152
          rlon2(x)=-145.+del
          rlat2(x)=27.5
          del=del+2.5
       END DO
       del=0.
       DO x=153,190
          rlon2(x)=-145.+del
          rlat2(x)=30.
          del=del+2.5
       END DO
       del=0.
       DO x=191,228
          rlon2(x)=-145.+del
          rlat2(x)=32.5
          del=del+2.5
       END DO
       del=0.
       DO x=229,266
          rlon2(x)=-145.+del
          rlat2(x)=35.
          del=del+2.5
       END DO
       del=0.
       DO x=267,304
          rlon2(x)=-145.+del
          rlat2(x)=37.5
          del=del+2.5
       END DO
       del=0.
       DO x=305,342
          rlon2(x)=-145.+del
          rlat2(x)=40.
          del=del+2.5
       END DO
       del=0.
       DO x=343,380
          rlon2(x)=-145.+del
          rlat2(x)=42.5
          del=del+2.5
       END DO
       del=0.
       DO x=381,418
          rlon2(x)=-145.+del
          rlat2(x)=45.
          del=del+2.5
       END DO
       del=0.
       DO x=419,456
          rlon2(x)=-145.+del
          rlat2(x)=47.5
          del=del+2.5
       END DO
       del=0.
       DO x=457,494
          rlon2(x)=-145.+del
          rlat2(x)=50.
          del=del+2.5
       END DO
       del=0.
       DO x=495,532
          rlon2(x)=-145.+del
          rlat2(x)=52.5
          del=del+2.5
       END DO
       del=0.
       DO x=533,570
          rlon2(x)=-145.+del
          rlat2(x)=55.
          del=del+2.5
       END DO
       del=0.
       DO x=571,608
          rlon2(x)=-145.+del
          rlat2(x)=57.5
          del=del+2.5
       END DO
       del=0.
       DO j=609,646
          rlon2(j)=-145.+del
          rlat2(j)=60.
          del=del+2.5
       END DO
c
       DO k=1,646
       END DO
C
       RETURN
       END

