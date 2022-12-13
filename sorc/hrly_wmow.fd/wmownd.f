       SUBROUTINE WMOWND(IDIM,JDIM,WEIGHT,ANALU,ANALV,FCSTU,FCSTV,
     1                   IRGN,NICR,X)
C SUBPROGRAM:    WMOWND
C   PRGMMR: LILLY            ORG: NP12        DATE: 2007-07-22
C
C ABSTRACT: MEAN ERROR, RMS VEC ERR, AVG AND ABS MAX FCST AND ANL WIND
C FOR NORTH AND SOUTH HEMISPHERE AND TROPICS PRESSURE LEVELS 850, 500,
C AND 250MB.
C
C PROGRAM HISTORY LOG:
C 2007-07-22 STEVEN G. LILLY -- UPDATING SOURCE CODES
C
C USAGE:    CALL WMOWND(IDIM,JDIM,WEIGHT,ANALU,ANALV,FCSTU,FCSTV,
C                       IRGN,NICR,X)
C
C   INPUT ARGUMENT LIST:
C
C   OUTPUT ARGUMENT LIST:
C
C   SUBPROGRAM CALLED:
C     LIBRARY:    (NONE)
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN-90
C   MACHINE:  IBM RS6000
C
C$$$

C***   WMO WIND VFCN ..............................................
C***   MEAN ERROR, RMS VEC ERR, AVG AND ABS MAX FCST AND ANL WIND..
C***    FOR NORTH AND SOUTH HEMISPHERE AND TROPICS.................
C***    PRESSURE LEVELS 850, 500, AND 250MB........................
C
C
       PARAMETER (NGDPTS=5365,MPIPTS=145,MPJPTS=37,MAREA=5,
     1            NSAVE=6*MAREA)
C
       DIMENSION IDIM(2,MAREA),JDIM(2,MAREA)
       DIMENSION FW(NGDPTS),AW(NGDPTS),X(NSAVE,2)
       DIMENSION WEIGHT(37)
       DIMENSION ANALU(MPIPTS,MPJPTS),ANALV(MPIPTS,MPJPTS)
       DIMENSION FCSTU(MPIPTS,MPJPTS),FCSTV(MPIPTS,MPJPTS)
C
C
C.................................................................
C      FOR SPECIFIED AREAS (QUADRANTS AND ENTIRE HEMISPHERE)
C.................................................................
C
       LOOP8: DO NVFCN = 1,NICR
C
       LOOP10: DO NA = 1,MAREA
C
          I1 = IDIM(1,NA)
          I2 = IDIM(2,NA)
          J1 = JDIM(1,NA)
          J2 = JDIM(2,NA)
          NSV = 6*(NA-1)
C
          KCOUNT = 0
          BIAS = .0
          VECRMS = .0
          SUMWGT = .0
          SUMVE  = .0
          FWBAR  = .0
          AWBAR  = .0
          ILON = (I2-I1)/NVFCN  +  1
          FLON = ILON
C
C.....
         LOOP50: DO J = J1,J2,NVFCN
            SUME = .0
            SBIAS = .0
            SFW = .0
            SAW = .0
C.....
         LOOP52: DO I = I1,I2,NVFCN
            KCOUNT = KCOUNT + 1
            WINDF = SQRT(FCSTU(I,J)**2 + FCSTV(I,J)**2)
            WINDA = SQRT(ANALU(I,J)**2 + ANALV(I,J)**2)
            FW(KCOUNT) = WINDF
            AW(KCOUNT) = WINDA
            SPDIF = WINDF - WINDA
            ERROR1 = (FCSTU(I,J) - ANALU(I,J))**2
            ERROR2 = (FCSTV(I,J) - ANALV(I,J))**2
            VECTOR = SQRT(ERROR1 + ERROR2)
            SUME   = SUME + ERROR1 + ERROR2
            SBIAS = SBIAS + SPDIF
            VECRMS = VECRMS + VECTOR**2
            SFW  =  SFW  +  FW(KCOUNT)
            SAW  =  SAW  +  AW(KCOUNT)
         END DO LOOP52
C
            BIAS = BIAS + SBIAS*WEIGHT(J)
            SUMVE = SUMVE + SUME*WEIGHT(J)
            FWBAR = FWBAR  + SFW*WEIGHT(J)
            AWBAR = AWBAR  + SAW*WEIGHT(J)
            SUMWGT = SUMWGT + WEIGHT(J)
c
         END DO LOOP50
C
         FWBAR = (FWBAR/SUMWGT)/FLON
         AWBAR = (AWBAR/SUMWGT)/FLON
         BIAS = (BIAS/SUMWGT)/FLON
         VRMS  = SQRT((SUMVE/SUMWGT)/FLON)
C
         X(NSV+1,NVFCN) = BIAS
         X(NSV+2,NVFCN) = VRMS
C
C.................................................................
C      ANALYZE FCST AND ANALYSIS WIND DISTRIBUTION
C.................................................................
C
       CALL STATS(FW,KCOUNT,S1,S2,S3,S4,S5,S6,S7,S8,S9)
c
       X(NSV+3,NVFCN) = FWBAR
       X(NSV+4,NVFCN) = S7
       X3 = S1
C
       CALL STATS(AW,KCOUNT,S1,S2,S3,S4,S5,S6,S7,S8,S9)
c
       X(NSV+5,NVFCN) = AWBAR
       X(NSV+6,NVFCN) = S7
C
C
C.....END AREA LOOP.....
c
       END DO LOOP10
C
C.....END VERIFICATION NET LOOP.....
c
       END DO LOOP8
C
C
       RETURN
       END
