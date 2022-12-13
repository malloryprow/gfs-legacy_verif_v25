C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C MAIN PROGRAM:  S1SUM       MONTHLY S1 SCORE SUMMARY
C   PRGMMR: ROBERT HIRANO    ORG: W/NMC40     DATE: 93-06-23
C
C ABSTRACT:  MODEL S1 SCORE DAILY VERIFICATION SETS STORED ON
C   ARCHIVE DISK ARE READ AND SUMMARIZED FOR THE MONTH.  TABLES OF
C   DAILY SCORES FOR SEVERAL VERIFICATION NETWORKS ARE PRINTED AS
C   WELL AS A SUMMARY BY FORECAST HOUR; A STATISTICAL ANALYSIS OF
C   SCORES IS ALSO TABULATED.  12-48 HOUR FORECASTS FOR MSL, 500MB,
C   AND 250MB.  S1 (ERR/MAX) AND S1 (ERR/TRU).
C
C PROGRAM HISTORY LOG:
C   93-06-23  ROBERT Y. HIRANO
c
c   96-10-01  Andrew S. Krein  conversion to cray completed
c   98-09-22  C. Vlcek  Make Y2K compatible.
c   99-09-20  C. Vlcek  Compile on IBM RS6000
C
C USAGE:
C   INPUT FILES:
C     FT10F001 - BEGIN AND END DATE OF VERIFICATION PERIOD
C     FT12F001 - ON84 LABELS OF VERIFICATION LEVELS
C                VERIFICATION DATA (MODEL,TRUTH,HOURS,ETC)
C                NUMBER OF POINTS IN VERIFICATION NETWORKS
C     FT50F001 - DAILY VERIFICATION FILE.
C
C   OUTPUT FILES:
C     FT06F001 - VERIFICATION TABLES
C     FT30F001 - S1MAX ARCHIVE FILE
C     FT40F001 - S1TRU ARCHIVE FILE
C
C   SUBPROGRAMS CALLED:
C     UNIQUE:    - AFILL,IFILL,READS1,S1SCRS,STATS
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  IBM RS6000
C
C$$$
 
       PARAMETER    (MXCYL=62,MXNET=09,MXLVL=3)
       PARAMETER    (ieFCT=7,irFCT=4,iaFCT=10,imFCT=10)
       CHARACTER(10) ITRUTH
       CHARACTER(9) JRUN
       CHARACTER(6) PLVL(MXLVL)
       CHARACTER(3) JMDL,JANL,NTRU
 
       DIMENSION S1SCRe(MXNET,MXCYL,ieFCT,2)
       DIMENSION S1SCRr(MXNET,MXCYL,irFCT,2)
       DIMENSION S1SCRa(MXNET,MXCYL,iaFCT,2)
       DIMENSION S1SCRm(MXNET,MXCYL,imFCT,2)
       INTEGER  NBEGIN(4),NEND(4),IDATE(MXCYL),IVT(4,MXCYL),
     1          NFCTS,NPTS(MXNET),IDS(12,MXLVL), ICONV
       INTEGER*4  ipad
 
       LOGICAL(1) IVFCT(imFCT),IVLVL(MXLVL)
 
       DATA IFILE /50/, ICONV /100000000/
 
       COMMON /XDATA/ IDATE,IDS,IVT,ipad,IVFCT,IVLVL,PLVL
       COMMON /YDATA/ NBEGIN,NEND,ICYB,ICYE,ICYI,IVFLD,JLVLS,
     1                ITRUTH
 
 
 100  FORMAT(I10,1X,I10)
 110  FORMAT(i3,1X,i3,1x,i3,1x,A6)
 120  FORMAT(3L1,4I3,1X,A9,1X,A3)
 130  FORMAT(A3,1X,A9,1X,A3,1X,4L1,3I3)
 131  FORMAT(A3,1X,A9,1X,A3,1X,6L1,3I3)
 132  FORMAT(A3,1X,A9,1X,A3,1X,10L1,3I3)
C  NAM to 84 hrs
 133  FORMAT(A3,1X,A9,1X,A3,1X,7L1,3I3)
C  GFS to 120 hrs
 134  FORMAT(A3,1X,A9,1X,A3,1X,10L1,3I3)
 140  FORMAT(9I4)
 210  FORMAT(10X,'.....IDENT  = ',I2,' ... ',3i6,' ... ',A6,' .....')
 220  FORMAT(///,'  *** VERIFICATION PERIOD ',4I4,' TO ' ,4I4,' *** ',
     1       /,10X,'PRESSURE LEVELS = ',3L1,',  TOTAL = ',
     2       I2,', TO DO= ',I2, /, 15X,'.....TOTAL FCSTS = ',I2,5X,
     3       '..... VERIFYING FLD = ',I2,2X,A9,3X,'(MODEL =',A3,')',//)
 230  FORMAT(10X,'.....MODEL = ',A3,' ... ',A9,
     1       ' RUN ... ',A3,' ANALYSIS ..... ',/,30X,'FCSTS = ',4L1,
     2       ' .. TOTAL OF ',I3,' HOURS TO DO ',/,30X,'VERIFICATION= ',
     3       I3,5X,'1=S1X(ERR/MAX), 2=S1T(ERR/TRU), 3=BOTH SCORES',/,
     4       30X,'IDISK= ',I2,4X,'(0=NO,1=YES)',//)
 231  FORMAT(10X,'.....MODEL = ',A3,' ... ',A9,
     1       ' RUN ... ',A3,' ANALYSIS ..... ',/,30X,'FCSTS = ',6L1,
     2       ' .. TOTAL OF ',I3,' HOURS TO DO ',/,30X,'VERIFICATION= ',
     3       I3,5X,'1=S1X(ERR/MAX), 2=S1T(ERR/TRU), 3=BOTH SCORES',/,
     4       30X,'IDISK= ',I2,4X,'(0=NO,1=YES)',//)
 232  FORMAT(10X,'.....MODEL = ',A3,' ... ',A9,
     1       ' RUN ... ',A3,' ANALYSIS ..... ',/,30X,'FCSTS = ',9L1,
     2       ' .. TOTAL OF ',I3,' HOURS TO DO ',/,30X,'VERIFICATION= ',
     3       I3,5X,'1=S1X(ERR/MAX), 2=S1T(ERR/TRU), 3=BOTH SCORES',/,
     4       30X,'IDISK= ',I2,4X,'(0=NO,1=YES)',//)
C  NAM 84 hrs
 233  FORMAT(10X,'.....MODEL = ',A3,' ... ',A9,
     1       ' RUN ... ',A3,' ANALYSIS ..... ',/,30X,'FCSTS = ',7L1,
     2       ' .. TOTAL OF ',I3,' HOURS TO DO ',/,30X,'VERIFICATION= ',
     3       I3,5X,'1=S1X(ERR/MAX), 2=S1T(ERR/TRU), 3=BOTH SCORES',/,
     4       30X,'IDISK= ',I2,4X,'(0=NO,1=YES)',//)
C  GFS 120 hrs
 234  FORMAT(10X,'.....MODEL = ',A3,' ... ',A9,
     1       ' RUN ... ',A3,' ANALYSIS ..... ',/,30X,'FCSTS = ',10L1,
     2       ' .. TOTAL OF ',I3,' HOURS TO DO ',/,30X,'VERIFICATION= ',
     3       I3,5X,'1=S1X(ERR/MAX), 2=S1T(ERR/TRU), 3=BOTH SCORES',/,
     4       30X,'IDISK= ',I2,4X,'(0=NO,1=YES)',//)
 240   FORMAT(5X,'NUMBER OF VERIFICATION POINTS ON NETWORK GRIDS',/,
     1        10X,'   1    2    3    4    5    6    7    8    9',/,
     2        9X,9I5,//)
 250   FORMAT(10X,'CYCLE=',I3,4X,'YR/MN/DY/HR = ',4I6,8X,
     1       'IDATE = ',i10)
 260   FORMAT(10X,'...........LAST CYCLE DATE = ',i10,4X,
     1        'REQUESTED END DATE = ',i10,4X,'TOTAL CYCLES= ',I4,///)
 
 
       CALL IFILL(IDS,12,MXLVL,1,0)
       CALL IFILL(IDATE,MXCYL,1,1,0)
       CALL IFILL(IVT,4,MXCYL,1,0)
 
C.....READ IN OPTIONS, PRINT, AND INITIALIZE ARRAYS.....
 
       READ(10,100,END=900) mBEGIN,mEND
 
       if (mbegin.lt.iconv)    then
         call dateconv(mbegin,nbegin(1),nbegin(2),nbegin(3),nbegin(4))
         call dateconv(mend,nend(1),nend(2),nend(3),nend(4))
       else
         call datecnv4(mbegin,nbegin(1),nbegin(2),nbegin(3),nbegin(4))
         call datecnv4(mend,nend(1),nend(2),nend(3),nend(4))
       endif
       DO 10 J = 1,MXLVL
        READ(12,110,END=900) (IDS(I,J),I=1,3),PLVL(J)
        PRINT 210,J, (IDS(K,J),K=1,3),PLVL(J)
  10   CONTINUE
 
       READ(12,120,END=900) IVLVL,JLVLS,NLVLS,
     1                      JFCTS,IVFLD,ITRUTH,NTRU
       PRINT 120, IVLVL,JLVLS,NLVLS,JFCTS,
     1            IVFLD,ITRUTH,NTRU
       PRINT 220, NBEGIN,NEND,IVLVL,JLVLS,NLVLS,
     1            JFCTS,IVFLD,ITRUTH,NTRU
 
       if (jfcts .eq. 4 ) then
       READ(12,130,END=900) JMDL,JRUN,JANL,
     1             (IVFCT(J),J=1,JFCTS),NFCTS,NVRFY,IDISK
       PRINT 230,JMDL,JRUN,JANL,(IVFCT(J),J=1,JFCTS),
     1             NFCTS,NVRFY,IDISK
 
C     NAM  84 hr
 
       elseif(jfcts .eq. 7) then
       READ(12,133,END=900) JMDL,JRUN,JANL,
     1             (IVFCT(J),J=1,JFCTS),NFCTS,NVRFY,IDISK
       PRINT 233,JMDL,JRUN,JANL,(IVFCT(J),J=1,JFCTS),
     1             NFCTS,NVRFY,IDISK
 
       elseif(jfcts .eq. 9) then
       READ(12,132,END=900) JMDL,JRUN,JANL,
     1             (IVFCT(J),J=1,JFCTS),NFCTS,NVRFY,IDISK
       PRINT 232,JMDL,JRUN,JANL,(IVFCT(J),J=1,JFCTS),
     1             NFCTS,NVRFY,IDISK
 
       elseif(jfcts .eq. 10) then
       READ(12,134,END=900) JMDL,JRUN,JANL,
     1             (IVFCT(J),J=1,JFCTS),NFCTS,NVRFY,IDISK
       PRINT 234,JMDL,JRUN,JANL,(IVFCT(J),J=1,JFCTS),
     1             NFCTS,NVRFY,IDISK
 
       endif
 
       READ(12,140,END=900) NPTS
       PRINT 240, (NPTS(I),I=1,MXNET)
 
 
        KOUNT = 0
        KBEGIN = mBEGIN
        PRINT *, MXCLY
        DO 20 IJ = 1,62
         INCR = (IJ-1)*12
         KOUNT = KOUNT + 1
         call bumpidt(kbegin,incr,idate(ij))
         if (idate(ij).lt.iconv)    then
           call dateconv(idate(ij),ivt(1,ij),ivt(2,ij),ivt(3,ij),
     1                   ivt(4,ij))
         else
           call datecnv4(idate(ij),ivt(1,ij),ivt(2,ij),ivt(3,ij),
     1                   ivt(4,ij))
         endif
         PRINT 250,IJ,IVT(1,IJ),IVT(2,IJ),IVT(3,IJ),IVT(4,IJ),
     1            IDATE(IJ)
         IF(IDATE(IJ).EQ.MEND) GO TO 22
  20    CONTINUE
  22    CONTINUE
        PRINT 260,IDATE(IJ),MEND,KOUNT
        ICYB = 1
        ICYE = KOUNT
        ICYI = 1
 
C..... READ ARCHIVE DISK AND SUMMARIZE MONTHLY SCORES .....
 
       DO 40 LVL = 1,MXLVL
        IF(.NOT. IVLVL(LVL)) GO TO 40
 
        if (jmdl .eq. 'NAM') then
           CALL READS1eta(LVL,IDATE,jmdl,jfcts,S1SCRe)
           call RORDEReta(S1SCRe)
           CALL S1SCRS(JMDL,NTRU,PLVL(LVL),IVT,IVFCT,NPTS,S1SCRe,
     1              NVRFY,IDISK,jfcts)
        elseif (jmdl .eq. 'NGM') then
           CALL READS1rgnl(LVL,IDATE,jmdl,jfcts,S1SCRr)
           call RORDERrgnl(S1SCRr)
           CALL S1SCRS(JMDL,NTRU,PLVL(LVL),IVT,IVFCT,NPTS,S1SCRr,
     1              NVRFY,IDISK,jfcts)
        elseif (jmdl .eq. 'GFS') then
           call READS1gbl(LVL,IDATE,jmdl,jfcts,S1SCRa)
           CALL S1SCRS(JMDL,NTRU,PLVL(LVL),IVT,IVFCT,NPTS,S1SCRa,
     1              NVRFY,IDISK,jfcts)
        elseif (jmdl .eq. 'GFL') then
           call READS1gbl(LVL,IDATE,jmdl,jfcts,S1SCRm)
           CALL S1SCRS(JMDL,NTRU,PLVL(LVL),IVT,IVFCT,NPTS,S1SCRm,
     1              NVRFY,IDISK,jfcts)
        endif
 
  40   CONTINUE
 
 900    CONTINUE
 
 
       STOP
       END
 
       SUBROUTINE READS1eta(LEVEL,IDATE,jmdl,jfcts,S1SCR)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    READS1eta      PRINT S1 SCORES AND STATISTICS
C   PRGMMR: R. Y. HIRANO     ORG: W/NMC40    DATE: 93-06-23
C
C ABSTRACT: READ ARCHIVE NAM VFCN DATA AND STORES IN ARRAY.
C
C PROGRAM HISTORY LOG:
C   93-06-23  ROBERT Y. HIRANO
C
C USAGE:    CALL READS1rgnl(LEVEL,IDATE,jmdl,jfcts,S1SCR)
C   INPUT ARGUMENT LIST:
C     LEVEL    - PRESSURE LEVEL TO DO (1=MSL,2=500MB,3=250MB)
C     IDATE    - VERIFICATION DATE TIME GROUP (real number not hex)
C
C   OUTPUT ARGUMENT LIST:
C     S1SCR    - S1 SCORE ARRAY. NUM NETS,CYCLE,FORECST,TYPE.
C
C   INPUT FILES:
C     FT50F001 - NAM FCST VS NGM ANALYSIS.
C
C   SUBPROGRAMS CALLED:
C     UNIQUE:    - AFILL, IFILL
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C
C$$$
 
       PARAMETER (MXCYL=62,MXNET=09,MXLVL=3,MXFCT=7)
 
       DIMENSION LABEL(8),IDATE(MXCYL),
     1           S1X(MXNET),S1T(MXNET),S1SCR(MXNET,MXCYL,MXFCT,2)
       character*3 jmdl
       DATA IFILE/ 50/
       data mapid/ 6/
 
 100   FORMAT(6i8,1x,i10,i4)
 110  format(9f7.2)
 
 
       DO 10 L = 1,2
        CALL AFILL(S1SCR(1,1,1,L),9,62,jfcts,0.0)
  10   CONTINUE
 
       REWIND(IFILE)

C .... READ LABEL AND S1 SCORES FROM ARCHIVE DISK FILE ....
C       CHECK FOR CORRECT MAPTYPE AND LEVEL
C       NOTE THAT ARCHIVED DATE IS THE VERIFYING DATE
 
  50   READ(IFILE,100,END=900) (LABEL(LZ),LZ=1,8)
       READ(IFILE,110) S1X
       read(ifile,110) S1T
       IF(LABEL(5) .NE. MAPID) GO TO 50
       if (label(8) .eq. 12) then
            ift=1
         elseif (label(8) .eq. 24) then
            ift=2
         elseif (label(8) .eq. 36) then
            ift=3
         elseif (label(8) .eq. 48) then
            ift=4
         elseif (label(8) .eq. 60) then
            ift=5
         elseif (label(8) .eq. 72) then
            ift=6
         elseif (label(8) .eq. 84) then
            ift=7
       endif
       if ((level .eq. 1 ) .and. (label(3) .eq. -1)) then
          goto 29
        elseif ((level .eq. 1) .and. (label(3) .ne. -1)) then
          goto 50
        elseif ((level .eq. 2) .and. (label(3) .eq. 500)) then
          goto 29
        elseif ((level .eq. 2) .and. (label(3) .ne. 500)) then
          goto 50
        elseif ((level .eq. 3) .and. (label(3) .eq. 250)) then
          goto 29
        elseif ((level .eq. 3) .and. (label(3) .ne. 250)) then
          goto 50
       endif
  29   continue
 
C.....DETERMINE IF V.T. IS IN THE DATE TABLES................
C      IF NOT, GO BACK AND READ ANOTHER CYCLE
 
       KDATE = LABEL(7)
       DO 30 I = 1,62
        IF(KDATE .NE. IDATE(I)) GO TO 30
        IVFT = I
        GO TO 32
  30   CONTINUE
       GO TO 50
 
C.....SAVE S1 SCORES .....
 
  32   CONTINUE
       DO 34 I = 1,9
        S1SCR(I,IVFT,IFT,1) = S1X(I)
        S1SCR(I,IVFT,IFT,2) = S1T(I)
  34   CONTINUE
 
c       PRINT *,LABEL,S1X,S1T
       GO TO 50
 
 900   CONTINUE
 
 
       RETURN
       END
 
       SUBROUTINE RORDEReta(S1SCR)

C$$$  SUBROUTINE DOCUMENTATION BLOCK
C
C SUBPROGRAM: RORDEReta
C   PRGMMR: LILLY            ORG: NP12        DATE: 2009-09-23
C
C ABSTRACT: e
C
C PROGRAM HISTORY LOG:
C
C 2009-09-23 Steven G. Lilly --  Updated sorc codes
C
C USAGE:
C   INPUT FILES:
C     FT05FT001   -
C
C
C   OUTPUT FILES:
C     FT06FT001   -
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

C......RGNAR1 SCORE IN POSITION ONE PLACED IN POSITION SIX
C      IN PLACE OF LLAMR SCORE

       PARAMETER (MXCYL=62,MXNET=09,MXFCT=7)
 
       DIMENSION S1SCR(MXNET,MXCYL,MXFCT,2)
 
       DO 10 K = 1,2
       DO 12 J = 1,MXFCT
       DO 14 I = 1,MXCYL
        S1SCR(6,I,J,K) = S1SCR(1,I,J,K)
        S1SCR(1,I,J,K) = 0.0
  14   CONTINUE
  12   CONTINUE
  10   CONTINUE
 
       RETURN
       END
 
       SUBROUTINE READS1rgnl(LEVEL,IDATE,jmdl,jfcts,S1SCR)

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    READS1rgnl      PRINT S1 SCORES AND STATISTICS
C   PRGMMR: R. Y. HIRANO     ORG: W/NMC40    DATE: 93-06-23
C
C ABSTRACT: READ ARCHIVE NAM and ngm VFCN DATA AND STORES IN ARRAY.
C
C PROGRAM HISTORY LOG:
C   93-06-23  ROBERT Y. HIRANO
C
C USAGE:    CALL READS1rgnl(LEVEL,IDATE,jmdl,jfcts,S1SCR)
C   INPUT ARGUMENT LIST:
C     LEVEL    - PRESSURE LEVEL TO DO (1=MSL,2=500MB,3=250MB)
C     IDATE    - VERIFICATION DATE TIME GROUP (real number not hex)
C
C   OUTPUT ARGUMENT LIST:
C     S1SCR    - S1 SCORE ARRAY. NUM NETS,CYCLE,FORECST,TYPE.
C
C   INPUT FILES:
C     FT50F001 - NAM FCST VS NGM ANALYSIS.
C
C   SUBPROGRAMS CALLED:
C     UNIQUE:    - AFILL, IFILL
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C
C$$$
 
       PARAMETER (MXCYL=62,MXNET=09,MXLVL=3,MXFCT=4)
 
       DIMENSION LABEL(8),IDATE(MXCYL),
     1           S1X(MXNET),S1T(MXNET),S1SCR(MXNET,MXCYL,MXFCT,2)
       character*3 jmdl
       DATA IFILE/ 50/
       data mapid/ 6/
 
 100   FORMAT(6i8,1x,i10,i4)
 110  format(9f7.2)
 
 
       DO 10 L = 1,2
        CALL AFILL(S1SCR(1,1,1,L),9,62,jfcts,0.0)
  10   CONTINUE
 
       REWIND(IFILE)
C..................................................................
C .... READ LABEL AND S1 SCORES FROM ARCHIVE DISK FILE ....
C       CHECK FOR CORRECT MAPTYPE AND LEVEL
C       NOTE THAT ARCHIVED DATE IS THE VERIFYING DATE
C..................................................................
 
  50   READ(IFILE,100,END=900) (LABEL(LZ),LZ=1,8)
       READ(IFILE,110) S1X
       read(ifile,110) S1T
       IF(LABEL(5) .NE. MAPID) GO TO 50
       if (label(8) .eq. 12) then
            ift=1
         elseif (label(8) .eq. 24) then
            ift=2
         elseif (label(8) .eq. 36) then
            ift=3
         elseif (label(8) .eq. 48) then
            ift=4
       endif
       if ((level .eq. 1 ) .and. (label(3) .eq. -1)) then
          goto 29
        elseif ((level .eq. 1) .and. (label(3) .ne. -1)) then
          goto 50
        elseif ((level .eq. 2) .and. (label(3) .eq. 500)) then
          goto 29
        elseif ((level .eq. 2) .and. (label(3) .ne. 500)) then
          goto 50
        elseif ((level .eq. 3) .and. (label(3) .eq. 250)) then
          goto 29
        elseif ((level .eq. 3) .and. (label(3) .ne. 250)) then
          goto 50
       endif
  29   continue
 
C.....DETERMINE IF V.T. IS IN THE DATE TABLES................
C      IF NOT, GO BACK AND READ ANOTHER CYCLE
 
       KDATE = LABEL(7)
       DO 30 I = 1,62
        IF(KDATE .NE. IDATE(I)) GO TO 30
        IVFT = I
        GO TO 32
  30   CONTINUE
       GO TO 50
 
C.....SAVE S1 SCORES .....
 
  32   CONTINUE
       DO 34 I = 1,9
        S1SCR(I,IVFT,IFT,1) = S1X(I)
        S1SCR(I,IVFT,IFT,2) = S1T(I)
  34   CONTINUE
 
c       PRINT *,LABEL,S1X,S1T
       GO TO 50
 
 900   CONTINUE
 
 
       RETURN
       END
 
       SUBROUTINE RORDERrgnl(S1SCR)

C$$$  SUBROUTINE DOCUMENTATION BLOCK
C
C SUBPROGRAM: RORDERrgnl
C   PRGMMR: LILLY            ORG: NP12        DATE: 2009-09-23
C
C ABSTRACT: e
C
C PROGRAM HISTORY LOG:
C
C 2009-09-23 Steven G. Lilly --  Updated sorc codes
C
C USAGE:
C   INPUT FILES:
C     FT05FT001   -
C
C
C   OUTPUT FILES:
C     FT06FT001   -
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

C......RGNAR1 SCORE IN POSITION ONE PLACED IN POSITION SIX
C      IN PLACE OF LLAMR SCORE

       PARAMETER (MXCYL=62,MXNET=09,MXFCT=4)
 
       DIMENSION S1SCR(MXNET,MXCYL,MXFCT,2)
 
       DO 10 K = 1,2
       DO 12 J = 1,MXFCT
       DO 14 I = 1,MXCYL
        S1SCR(6,I,J,K) = S1SCR(1,I,J,K)
        S1SCR(1,I,J,K) = 0.0
  14   CONTINUE
  12   CONTINUE
  10   CONTINUE
 
       RETURN
       END
 
       SUBROUTINE READS1gbl(LEVEL,IDATE,jmdl,jfcts,S1SCR)

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    READS1gbl      PRINT S1 SCORES AND STATISTICS
C   PRGMMR: R. Y. HIRANO     ORG: W/NMC40    DATE: 93-06-23
C
C ABSTRACT: READ ARCHIVE GFS(12) and GFL(24) VFCN DATA AND STORES IN ARRAY.
C
C PROGRAM HISTORY LOG:
C   93-06-23  ROBERT Y. HIRANO
C
C USAGE:    CALL READS1gbl(LEVEL,IDATE,jmdl,jfcts,S1SCR)
C   INPUT ARGUMENT LIST:
C     LEVEL    - PRESSURE LEVEL TO DO (1=MSL,2=500MB,3=250MB)
C     IDATE    - VERIFICATION DATE TIME GROUP (real number not hex)
C
C   OUTPUT ARGUMENT LIST:
C     S1SCR    - S1 SCORE ARRAY. NUM NETS,CYCLE,FORECST,TYPE.
C
C   INPUT FILES:
C     FT50F001 - NWS.WD00.RYH.VANLETA. GFS(12) and GFL(24) FCST VS ssi ANALYSIS.
C
C   SUBPROGRAMS CALLED:
C     UNIQUE:    - AFILL, IFILL
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C
C$$$
 
       PARAMETER   (MXCYL=62,MXNET=14,MXLVL=3,rFCT=4,aFCT=10,mFCT=10)

       DIMENSION LABEL(8),IDATE(MXCYL),
     1           S1X(MXNET),S1T(MXNET),S1SCR(9,MXCYL,jfcts,2),
     2           s1scrg(MXNET,mxcyl,jfcts,2)
       character(3) jmdl
       DATA IFILE/ 50/
       data mapid/ 29/

 100   FORMAT(6i8,1x,i10,i4)
 110   format(14f7.2)

       DO 10 L = 1,2
       CALL AFILL(S1SCR(1,1,1,L),9,62,jfcts,0.0)
        CALL AFILL(S1SCRg(1,1,1,L),14,62,jfcts,0.0)
  10   CONTINUE
 
       REWIND(IFILE)

C .... READ LABEL AND S1 SCORES FROM ARCHIVE DISK FILE ....
C       CHECK FOR CORRECT MAPTYPE AND LEVEL
C       NOTE THAT ARCHIVED DATE IS THE VERIFYING DATE
 
  50   READ(IFILE,100,END=900) (LABEL(LZ),LZ=1,8)
       READ(IFILE,110) S1X
       read(ifile,110) s1t
       IF(LABEL(5) .NE. MAPID) GO TO 50
       if (jmdl .eq. 'GFS') then
       if (label(8) .eq. 12) then
            ift=1
         elseif (label(8) .eq. 24) then
            ift=2
         elseif (label(8) .eq. 36) then
            ift=3
         elseif (label(8) .eq. 48) then
            ift=4
         elseif (label(8) .eq. 60) then
            ift=5
         elseif (label(8) .eq. 72) then
            ift=6
         elseif (label(8) .eq. 84) then
            ift=7
         elseif (label(8) .eq. 96) then
            ift=8
         elseif (label(8) .eq. 108) then
            ift=9
         elseif (label(8) .eq. 120) then
            ift=10
       endif
       endif
       if (jmdl .eq. 'GFL') then
         if (label(8) .eq. 24) then
            ift=1
         elseif (label(8) .eq. 48) then
            ift=2
         elseif (label(8) .eq. 72) then
            ift=3
         elseif (label(8) .eq. 96) then
            ift=4
         elseif (label(8) .eq. 120) then
            ift=5
         elseif (label(8) .eq. 144) then
            ift=6
         elseif (label(8) .eq. 168) then
            ift=7
         elseif (label(8) .eq. 192) then
            ift=8
         elseif (label(8) .eq. 216) then
            ift=9
         elseif (label(8) .eq. 240) then
            ift=10
       endif
       endif
       if ((level .eq. 1 ) .and. (label(3) .eq. -1)) then
          goto 29
        elseif ((level .eq. 1) .and. (label(3) .ne. -1)) then
          goto 50
        elseif ((level .eq. 2) .and. (label(3) .eq. 500)) then
          goto 29
        elseif ((level .eq. 2) .and. (label(3) .ne. 500)) then
          goto 50
        elseif ((level .eq. 3) .and. (label(3) .eq. 250)) then
          goto 29
        elseif ((level .eq. 3) .and. (label(3) .ne. 250)) then
          goto 50
       endif
  29   continue
 
C.....DETERMINE IF V.T. IS IN THE DATE TABLES................
C      IF NOT, GO BACK AND READ ANOTHER CYCLE
 
       KDATE = LABEL(7)
       DO 30 I = 1,62
        IF(KDATE .NE. IDATE(I)) GO TO 30
        IVFT = I
        GO TO 32
  30   CONTINUE
       GO TO 50
 
C.....SAVE S1 SCORES .....
 
  32   CONTINUE
       DO 34 I = 1,14
        S1SCRg(I,IVFT,IFT,1) = S1X(I)
        S1SCRg(I,IVFT,IFT,2) = S1T(I)
  34   CONTINUE
 
c       PRINT *,LABEL,S1X,S1T
       GO TO 50
 
 900   CONTINUE
 
           call RORDERgbl(jfcts,s1scrg,s1scr)
 
       RETURN
       END
 
       SUBROUTINE RORDERgbl(jfcts,s1scrg,S1SCR)

C$$$  SUBROUTINE DOCUMENTATION BLOCK
C
C SUBPROGRAM: RORDERgbl
C   PRGMMR: LILLY            ORG: NP12        DATE: 2009-09-23
C
C ABSTRACT: e
C
C PROGRAM HISTORY LOG:
C
C 2009-09-23 Steven G. Lilly --  Updated sorc codes
C
C USAGE:
C   INPUT FILES:
C     FT05FT001   -
C
C
C   OUTPUT FILES:
C     FT06FT001   -
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

C......RGNAR1 SCORE IN POSITION 14 PLACED IN POSITION SIX
C      IN PLACE OF LLAMR SCORE
 
       PARAMETER   (MXCYL=62,MXNET=09,MXLVL=3)
 
       DIMENSION S1SCR(mxnet,MXCYL,jfcts,2),
     1           s1scrg(14,mxcyl,jfcts,2)
 
       DO 10 K = 1,2
       DO 12 J = 1,jfcts
       DO 14 I = 1,62
        S1SCR(1,I,J,K) = S1SCRg(1,I,J,K)
        S1SCR(2,I,J,K) = S1SCRg(2,I,J,K)
        S1SCR(3,I,J,K) = S1SCRg(3,I,J,K)
        S1SCR(4,I,J,K) = S1SCRg(4,I,J,K)
        S1SCR(5,I,J,K) = S1SCRg(5,I,J,K)
        S1SCR(6,I,J,K) = S1SCRg(14,I,J,K)
        S1SCR(7,I,J,K) = S1SCRg(7,I,J,K)
        S1SCR(8,I,J,K) = S1SCRg(8,I,J,K)
        S1SCR(9,I,J,K) = S1SCRg(9,I,J,K)
  14   CONTINUE
  12   CONTINUE
  10   CONTINUE
 
       RETURN
       END
 
       SUBROUTINE IFILL(IX,I,J,K,IA)

C$$$  SUBROUTINE DOCUMENTATION BLOCK
C
C SUBPROGRAM: IFILL
C   PRGMMR: LILLY            ORG: NP12        DATE: 2009-09-23
C
C ABSTRACT: e
C
C PROGRAM HISTORY LOG:
C
C 2009-09-23 Steven G. Lilly --  Updated sorc codes
C
C USAGE:
C   INPUT FILES:
C     FT05FT001   -
C
C
C   OUTPUT FILES:
C     FT06FT001   -
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

 
C***     FILL ARRAY WITH VALUE = IA
 
       DIMENSION IX(I,J,K)
 
       DO 10 KK = 1,K
       DO 20 JJ = 1,J
       DO 30 II = 1,I
         IX(II,JJ,KK) = IA
  30   CONTINUE
  20   CONTINUE
  10   CONTINUE
 
       RETURN
       END


       SUBROUTINE AFILL(X,I,J,K,A)

C$$$  SUBROUTINE DOCUMENTATION BLOCK
C
C SUBPROGRAM: AFILL
C   PRGMMR: LILLY            ORG: NP12        DATE: 2009-09-23
C
C ABSTRACT: e
C
C PROGRAM HISTORY LOG:
C
C 2009-09-23 Steven G. Lilly --  Updated sorc codes
C
C USAGE:
C   INPUT FILES:
C     FT05FT001   -
C
C
C   OUTPUT FILES:
C     FT06FT001   -
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

 
C     ***     FILL ARRAY WITH VALUE = A
 
       DIMENSION X(I,J,K)
 
       DO 8 KK = 1,K
       DO 10 JJ = 1,J
         DO 12 II = 1,I
           X(II,JJ,KK) = A
  12     CONTINUE
  10   CONTINUE
   8   CONTINUE
 
       RETURN
       END
 
 
       SUBROUTINE S1SCRS(NAME,NTRU,PLEVEL,IVT,IVFCT,NPTS,S1SCR,
     1                   NVRFY,IDISK,jfcts)

C SUBPROGRAM:    S1SCRS
C   PRGMMR: ROBERT HIRANO    ORG: W/NMC40    DATE: 93-06-23
C
C ABSTRACT: SUMMARIZE NAM MODEL S1 SCORES
C
C PROGRAM HISTORY LOG:
C   93-06-23  ROBERT HIRANO
C   99-09-20  C. VLCEK       COMPILE ON IBM RS6000
C
C USAGE:    CALL S1SCRS(NAME,NTRU,PLEVEL,IVT,IVFCT,NPTS,S1SCR,
C                       NVRFY,IDISK,jfcts)
C   INPUT ARGUMENT LIST:
C     NAME     - MODEL NAME
C     NTRU     - VERIFYING FIELD NAME
C     PLEVEL   - PRESSURE LEVEL NAME
C     IVT      - YEAR, MONTH, DAY, HOUR OF EACH CYCLE
C     IVFCT    - REQUESTED FORECAST HOURS
C     NPTS     - NUMBER OF POINTS IN VERIFICATION NETWORK
C     S1SCR    - S1 SCORES FOR ALL CYCLES AND FORECAST HOURS
C     NVRFY    - 1=S1X(ERR/MAX), 2=S1T(ERR/TRU), 3=BOTH
C     IDISK    - COPY TO ARCHIVE DISK (1=YES,0=NO)
C
C   SUBPROGRAM CALLED:
C     UNIQUE:   - AFILL, STATS
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN-90
C   MACHINE:  IBM RS6000
C
C$$$
 
       PARAMETER    (NETX=09, MXCYL=62, JSTTX=10)
       CHARACTER(9) ITRUTH
       CHARACTER(6) PLEVEL,AREA(NETX)
       CHARACTER(5) JSTAT(JSTTX)
       CHARACTER(3) NAME,NTRU,MONTH(12),GNAME(2),GRDN
 
       REAL    S1SCR(NETX,MXCYL,jfcts,2),
     1         X(JSTTX,NETX,JFCTs),W(MXCYL)
       integer ix(jsttx,netx,jfcts)
       INTEGER IVT(4,MXCYL),NPTS(NETX)
       LOGICAL*1 IVFCT(JFCTs)
 
       COMMON /YDATA/ NBEGIN(4),NEND(4),ICYB,ICYE,ICYI,IVFLD,JLVLS,
     1                ITRUTH
 
       DATA AREA/'GBLAR1','59GRID','WEST33','EAST33','49GRID',
     1           'RGNAR1','LL59PT','LLW33 ','LLE33 '/
       DATA GNAME/'MAX','TRU'/
       DATA JSTAT/'  NUM',' MEAN','ABSDV','  VAR',' SDEV',' SKEW',
     1           ' KURT','  MAX','  MIN','RANGE'/
       DATA MONTH/'JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG',
     1            'SEP','OCT','NOV','DEC'/
 
 100   FORMAT(/)
 110   FORMAT(I5,1X,4I3)
 120   FORMAT(i5,1x,4i4,30X,'......  N O T   A V A I L A B L E  .....')
 200   FORMAT(//,15X,'***** S1 SCORE VERIFICATION (S1=ERROR/',
     1        A3,') ***** ',/,6X,'.....MODEL= ',A3,' .....LEVEL= ',A6,
     2        ' .....TRUTH= ',A3,1X,A9,' .....FCST HR= ',I3,' .....',
     3        /,11X,'V.T.',/, '  CYL  YR/MN/DY/HR',  9(2X,A6),/)
 230   FORMAT(i5,1x,4i4,19X, 9F8.2)
 400   FORMAT(/,25X,'..... STATISTICAL ANALYSIS S1 SCORE (S1=',
     1        'ERROR/',A3,') DISTRIBUTION .....',/,
     2        9X,'.... ',A3,' ... VERIFIED AGAINST ',
     3        A3, 1X, A9,'  AT ',A6,'  FOR THE PERIOD  ',I2,'Z',
     4        I3,A3,I4,' - ',I2,'Z',I3,A3,I4,' ....',//,
     5        9X, 'FTHR     AREA',4X,10(3X,A5),5X,'NETPTS',/)
 410   FORMAT(9X,I3,' ....')
 420   FORMAT(18X,A6, 2X,F8.0,F8.2,5F8.3,3F8.2,5X,I5)
 430   FORMAT(/)
 452   format(i5,2x,a3,2x,a6,i2,i5,i3)
 453   FORMAT(3x,18i4)
 455   FORMAT(////,20X,60('*'),//,10X,'IDISK = ',I2,5X,
     1        'WRITE TO ARCHIVE ... MODEL,PLEVEL= ',a3,4X,A6,///)
 460   FORMAT(10X,'??? ERROR WRITING DISK ???   ',
     1        3(A3,3X),////)
 500   FORMAT(10X,'NC,XM,AD,VAR,SD,A3,A4,ZX,ZN,R= ',I3,F8.2,
     1        3F8.3,2F8.4,3F8.2)
 
       ITYPB = 1
       ITYPE = 2
       IF(NVRFY  .EQ.  1) THEN
        ITYPE = 1
       ELSE
        IF(NVRFY  .EQ.  2) THEN
         ITYPB = 2
        ENDIF
       ENDIF

       DO 50 ITYP = ITYPB,ITYPE
        CALL AFILL(X,JSTTX,NETX,JFCTs,.0)
        GRDN = GNAME(ITYP)

C      DO STATISTICAL ANALYSIS OF S1 SCORE DISTRIBUTION

       DO 10 IFT = 1,JFCTS
        DO 12 J = 1,9
         NC = 0
        DO 14 I = 1,62
         IF(S1SCR(J,I,IFT,ITYP)  .EQ.  0.0) GO TO 14
         NC = NC + 1
         W(NC) = S1SCR(J,I,IFT,ITYP)
  14    CONTINUE

        IF(NC .EQ. 0) GO TO 12
         CALL STATS(W,NC,XM,AD,VAR,SD,A3,A4,ZX,ZN,R)
          X(1,J,IFT) = FLOAT(NC)
          X(2,J,IFT) = XM
          X(3,J,IFT) = AD
          X(4,J,IFT) = VAR
          X(5,J,IFT) = SD
          X(6,J,IFT) = A3
          X(7,J,IFT) = A4
          X(8,J,IFT) = ZX
          X(9,J,IFT) = ZN
          X(10,J,IFT) = R
  12    CONTINUE
  10   CONTINUE

C      PRINT S1 SCORE TABLES FOR EACH FORECAST HOUR
C      FIRST AREA IS GBLAR1....NO VFCN FOR THE NAM

       DO 20 IFT = 1,JFCTS
        IF(.NOT. IVFCT(IFT)) GO TO 20
        if (NAME .ne. 'GFL') then
          IFHR = 12*IFT
        elseif (NAME .eq. 'GFL') then
          ifhr = 24*ift
        endif

        PRINT 200,GRDN,NAME,PLEVEL,NTRU,ITRUTH,IFHR,
     1            (AREA(I),I=1,NETX)

       DO 30 I = ICYB,ICYE,ICYI
        IF(IVT(1,I) .EQ. 0) GO TO 30
c        PRINT 110,I,(IVT(J,I),J=1,4)

        IF(S1SCR(2,I,IFT,ITYP)  .LE.  .0) THEN
         PRINT 120,i,(ivt(j,i),j=1,4)
         GO TO 30
        ELSE
          PRINT 230,i,(ivt(j,i),j=1,4),(S1SCR(K,I,IFT,ITYP),K=1,9)
        ENDIF

  30   CONTINUE

  20   CONTINUE

C      PRINT STATISTICAL ANALYSIS TABLES FOR EACH FORECAST HOUR
C      FIRST AREA IS GBLAR1....NO VFCN FOR THE NAM
 
       MONB = NBEGIN(2)
       MONE = NEND(2)
       KC = 0
       KC5 = 1
       DO 40 N = 1,JFCTS
        IF(X(1,2,N) .EQ. 0.) GO TO 40
        KC = KC + 1

        IF(KC  .EQ.  KC5) THEN
         PRINT 400,GRDN,NAME,NTRU,ITRUTH,PLEVEL,NBEGIN(4),NBEGIN(3),
     1             MONTH(MONB),NBEGIN(1),NEND(4),NEND(3),MONTH(MONE),
     2             NEND(1),(JSTAT(I),I=1,JSTTX)
         KC5 = KC5 + 4
        ENDIF

        if (NAME .ne. 'GFL') then
        NFTHR = 12 * N
        elseif (NAME .eq. 'GFL') then
        nfthr = 24 * n
        endif
        PRINT 410,NFTHR
        DO 42 K = 1,NETX
         PRINT 420,AREA(K),(X(I,K,N),I=1,JSTTX),NPTS(K)
  42    CONTINUE
        PRINT 430

  40   CONTINUE

C......WRITE TO ARCHIVE DISK?..............................
C......  S1MAX TO FILE 30, S1TRU TO FILE 40................
c       MODEL = 5
       NF = 30
       IF(ITYP  .EQ.  2) NF = 40
       NUM = X(1,2,1)
       do j=1,jfcts
         do i=1,9
           ix(2,i,j)=IFIX(10*x(2,i,j))
         enddo
       enddo
       IF(IDISK  .EQ.  1) THEN
        write(nf,452,err=90) ityp,name,plevel,nend(2),nend(1),num
        WRITE(nf,453,err=90) ((ix(2,i,j),i=1,9),j=1,jfcts)
       ENDIF
        PRINT 455,IDISK,name,PLEVEL
       GO TO 50
  90   PRINT 460,GNAME(ITYPE),NAME,PLEVEL

  50   CONTINUE

       RETURN
       END
 
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
C   99-09-20  C. VLCEK          COMPILE ON IBM RS6000
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
C   LANGUAGE: FORTRAN 90
C   MACHINE:  IBM RS6000
C
C$$$
 
       DIMENSION X(N)
 
       SX = 0.
       AD = 0.
       SX2 = 0.
       SX3 = 0.
       SX4 = 0.
 
C...... 1. SUM VALUES, CALCULATE MEAN
C       2. SUM DEVIATION FROM MEAN
C            FIRST THRU FOURTH MOMENTS
C            THEN, CALCULATE ABSOLUTE DEVIATION, VARIANCE,
C                  AND STANDARD DEVIATION ....................
 
       DO  1  I = 1,N
         SX = SX + X(I)
   1   CONTINUE
 
       XM = SX / FLOAT(N)
 
       DO  2  I = 1,N
         XMM = X(I) - XM
         X2 = XMM * XMM
         X3 = XMM * X2
         X4 = X2 * X2
 
         AD = AD + ABS(XMM)
         SX2 = SX2 + X2
         SX3 = SX3 + X3
         SX4 = SX4 + X4
 
   2   CONTINUE
 
       AD = AD / FLOAT(N)
       VAR =  SX2 / FLOAT(N - 1)
       SD = SQRT(VAR)
 
C...... 3. MOMENT COEF OF SKEWNESS: SKEWED RIGHT (LONGER TAIL TO
C            RIGHT) IF POSITIVE;   SKEWED LEFT (LONGER TAIL TO
C            LEFT) IF NEGATIVE ...................................
C...... 4. MOMENT COEF OF KURTOSIS WITH RESPECT TO NORMAL:
C            LEPTO (PEAKED) IF POSITIVE; PLATY (FLAT) IF NEGATIVE

       A3 = (SX3/FLOAT(N)) / (VAR*SD)
       A4 = (SX4/FLOAT(N)) / (VAR*VAR)  -  3.

C...... 5. DETERMINE MAXIMUM AND MINIMUM AND THE RANGE ............

       ZX = X(1)
       ZN = ZX

       DO  3  I = 2,N
         IF(X(I) .GT. ZX) ZX = X(I)
         IF(X(I) .LT. ZN) ZN = X(I)
   3   CONTINUE

       R = ZX - ZN
         IF(ZX .LT. 0.) R = ABS(R)

       RETURN
       END

       subroutine dateconv(date1,iy,im,id,ih)
 
C$$$  SUBROUTINE DOCUMENTATION BLOCK
C
C SUBPROGRAM: dateconv
C   PRGMMR: LILLY            ORG: NP12        DATE: 2009-09-23
C
C ABSTRACT: e
C
C PROGRAM HISTORY LOG:
C
C 2009-09-23 Steven G. Lilly --  Updated sorc codes
C
C USAGE:
C   INPUT FILES:
C     FT05FT001   -
C
C
C   OUTPUT FILES:
C     FT06FT001   -
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


c  this subroutine takes an 8 digit integer date word yymmddhh format
c  and breaks it up into 4 integer parts.

       character*8 cdate
       character*2 cyear,cmonth,cday,chour
       integer date1
       write (cdate,900) date1
       cyear=cdate(1:2)
       cmonth=cdate(3:4)
       cday=cdate(5:6)
       chour=cdate(7:8)
       read (cyear,800) iy
       read (cmonth,800) im
       read (cday,800) id
       read (chour,800) ih
 800    format (i2)
 900    format (i8)
       return
       end


       subroutine datecnv4(date1,iy,im,id,ih)

C$$$  SUBROUTINE DOCUMENTATION BLOCK
C
C SUBPROGRAM: datecnv4
C   PRGMMR: LILLY            ORG: NP12        DATE: 2009-09-23
C
C ABSTRACT: e
C
C PROGRAM HISTORY LOG:
C
C 2009-09-23 Steven G. Lilly --  Updated sorc codes
C
C USAGE:
C   INPUT FILES:
C     FT05FT001   -
C
C
C   OUTPUT FILES:
C     FT06FT001   -
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

c  this subroutine takes an 10 digit integer date word yyyymmddhh format
c  and breaks it up into 4 integer parts.

       character*10 cdate
       character*4 cyear
       character*2 cmonth,cday,chour
       integer date1
       write (cdate,900) date1
       cyear=cdate(1:4)
       cmonth=cdate(5:6)
       cday=cdate(7:8)
       chour=cdate(9:10)
       read (cyear,800) iy
       read (cmonth,810) im
       read (cday,810) id
       read (chour,810) ih
 800    format (i4)
 810    format (i2)
 900    format (i10)
       return
       end
 

       SUBROUTINE  BUMPIDT( IDATE, IDT, IBDATE )

C$$$  SUBROUTINE DOCUMENTATION BLOCK
C
C SUBPROGRAM: BUMPIDT
C   PRGMMR: LILLY            ORG: NP12        DATE: 2009-09-23
C
C ABSTRACT: e
C
C PROGRAM HISTORY LOG:
C
C 2009-09-23 Steven G. Lilly --  Updated sorc codes
C
C USAGE:
C   INPUT FILES:
C     FT05FT001   -
C
C
C   OUTPUT FILES:
C     FT06FT001   -
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

C-----------------------------------------------------------------------
C  BUMPS YY, YYMM, YYMMDD, OR YYMMDDHH BY IDT
C-----------------------------------------------------------------------
 
      IF(IDATE.LT.100) THEN
         IY = IDATE
         IF(INCYEAR(IY,IDT).NE.0) GOTO 900
         IBDATE = IY
         RETURN 
      ELSE IF(IDATE.LT.10000) THEN
         IY = MOD(IDATE/100,100)
         IM = MOD(IDATE    ,100)
         IF(INCYEAR(IY,     0).NE.0) GOTO 900
         IF(INCMON (IY,IM,  0).NE.0) GOTO 900
         IF(INCMON (IY,IM,IDT).NE.0) GOTO 900
         IBDATE = IY*100 + IM
         RETURN
      ELSEIF(IDATE.LT.1000000) THEN
         IY = MOD(IDATE/10000,100)
         IM = MOD(IDATE/100  ,100)
         ID = MOD(IDATE      ,100)
         IF(INCYEAR(IY,        0).NE.0) GOTO 900
         IF(INCMON (IY,IM,     0).NE.0) GOTO 900
         IF(INCDAY (IY,IM,ID,  0).NE.0) GOTO 900
         IF(INCDAY (IY,IM,ID,IDT).NE.0) GOTO 900
         IBDATE = IY*10000 + IM*100 + ID
         RETURN
      ELSEIF(IDATE.LT.100000000) THEN
         IY = MOD(IDATE/1000000,100)
         IM = MOD(IDATE/10000  ,100)
         ID = MOD(IDATE/100    ,100)
         IH = MOD(IDATE        ,100)
         IF(INCYEAR(IY,           0).NE.0) GOTO 900
         IF(INCMON (IY,IM,        0).NE.0) GOTO 900
         IF(INCDAY (IY,IM,ID,     0).NE.0) GOTO 900
         IF(INCHOUR(IY,IM,ID,IH,  0).NE.0) GOTO 900
         IF(INCHOUR(IY,IM,ID,IH,IDT).NE.0) GOTO 900
         IBDATE = IY*1000000 + IM*10000 + ID*100 + IH
         RETURN
      ELSE IF(IDATE.LT.10000000000) THEN
         CALL  YYYYDT ( IDATE, IDT, KDATE, IRET )
         IBDATE = KDATE
         RETURN
      ENDIF

 900  CONTINUE
      PRINT *, '**** BAD DATE WORD ****'
      RETURN

      END

      FUNCTION INCYEAR(IY,IDT)
      COMMON /MONTH/ MON(12)
      INTEGER MM(12)
     
      DATA MM /31,28,31,30,31,30,31,31,30,31,30,31/
 
      MON = MM
      INCYEAR = -1
      IF(IY.LT.0 .OR.IY.GT.99) RETURN
      IY = MOD(IY+IDT,100)
      IF(IY.LT.0 .OR.IY.GT.99) RETURN
      IF(MOD(IY,4).EQ.0) MON(2) = 29
      INCYEAR = 0
      RETURN
      END
 
      FUNCTION INCMON(IY,IM,IDT)
 
      INCMON = -1
      IF(IM.LT.1.OR.IM.GT.12) RETURN
      IM = IM+IDT
 
1     IF(IM.LT.1) THEN
         IF(INCYEAR(IY,-1).NE.0) RETURN
         IM = IM+12
         GOTO 1
      ELSEIF(IM.GT.12) THEN
         IF(INCYEAR(IY,1).NE.0) RETURN
         IM = IM-12
         GOTO 1
      ENDIF
      INCMON = 0
      RETURN
      END


      FUNCTION INCDAY(IY,IM,ID,IDT)
      COMMON /MONTH/ MON(12)
 
      INCDAY = -1
      IF(ID.LT.1.OR.ID.GT.MON(IM)) RETURN
      ID = ID+IDT
 
1     IF(ID.LT.1) THEN
         IF(INCMON(IY,IM,-1).NE.0) RETURN
         ID = ID+MON(IM)
         GOTO 1
      ELSEIF(ID.GT.MON(IM)) THEN
         ID = ID-MON(IM)
         IF(INCMON(IY,IM,1).NE.0) RETURN
         GOTO 1
      ENDIF
 
      INCDAY = 0
      RETURN
      END


      FUNCTION INCHOUR(IY,IM,ID,IH,IDT)
 
      INCHOUR = -1
      IF(IH.LT.0.OR.IH.GT.23) RETURN
      IH = IH+IDT
 
1     IF(IH.LT.0) THEN
         IF(INCDAY(IY,IM,ID,-1).NE.0) RETURN
         IH = IH+24
         GOTO 1
      ELSEIF(IH.GE.24) THEN
         IF(INCDAY(IY,IM,ID,1).NE.0) RETURN
         IH = IH-24
         GOTO 1
      ENDIF
 
      INCHOUR = 0
      RETURN
      END


       SUBROUTINE  YYYYDT ( IDATE, IDT, KDATE, IRET )

C$$$  SUBPROGRAM DOCUMENTATION BLOCK                                  
C                                                                     
C SUBPROGRAM:  YYYYDT          BACKDATES YYYYMMDDHH DATE BY IDT HOURS
C   AUTHOR: BOB HOLLERN        ORG: NMC12         DATE: 98-08-19      
C                                                                     
C ABSTRACT: USES THE 4-DIGIT YEAR YYYYMMDDHH DATE WORD AND BACKDATES
C   IDT HOURS FROM THIS DATE TO FORM A NEW 4-DIGIT YEAR DATE WORD.
C
C PROGRAM HISTORY LOG:
C
C   98-08-19  BOB HOLLERN, AUTHOR
C   98-09-24  VLCEK -- COPIED FOR USE WITH MAIN PROGRAM S1SUM.F AND 
C             MODIFIED TO ALWAYS RETURN 4-DIGIT YEAR (ORIGINAL RETURNED
C             2-DIGIT YEAR IF IDT.LE.0)
C                                                                     
C USAGE:    CALL YYYYDT( IDATE, IDT, KDATE, IRET )                    
C
C   INPUT ARGUMENT LIST:                                              
C
C     IDATE    -   DATE WORD OF FORM YYYYMMDDHH WHERE YYYY IS 4-DIGIT
C                  YEAR, MM IS MONTH, DD IS DAY OF MONTH, AND HH IS
C                  HOUR OF DAY
C     IDT      -   VARIABLE SET TO THE NUMBER OF HOURS TO BACKDATE
C                  FROM THE DATE YYYYMMDDHH
C
C   OUTPUT ARGUMENT LIST:                                             
C                                                                     
C     KDATE    -   4-DIGIT YEAR DATE WORD FORMED FROM BACKDATING
C                  IDT HOURS FROM YYYYMMDDHH DATE
C     KRET     -   RETURN CODE. SET TO 1, IF PROBLEMS; OTHERWISE,
C                  SET TO 0.
C                                                                     
C ATTRIBUTES:                                                         
C   LANGUAGE: FORTRAN  90                                             
C   MACHINE: IBM RS6000
C$$$                                                                  

       REAL  RINC(5)

       INTEGER   NDATE(8),  JDATE(8)

       DATA  ICON1  / 1000000 /

       DATA  ICON2  / 10000 /

       DATA  ICON3  / 100 /

110    FORMAT ( 1X, 'JDATE(',I1, ') = ', I4 )

120    FORMAT ( 1X, I8.8 )
 
       IRET = 0 

C      GET 4-DIGIT YEAR FROM IDATE

       IYR = IDATE / ICON1

C      GET MONTH FROM IDATE

       IX = IDATE - (ICON1*IYR)

       IMN = IX / ICON2

C      GET DAY OF MONTH FROM IDATE

       IY = IX - (ICON2*IMN)

       IDAYMN = IY / ICON3

C      GET HOUR FROM IDATE

       IHR = MOD(IY,100)

       DO I = 1,5
         RINC(I) = 0.0
       END DO

       DO I = 1,8
         NDATE(I) = 0
       END DO

C      RUN DATE

       NDATE(1) = IYR
       NDATE(2) = IMN
       NDATE(3) = IDAYMN
       NDATE(5) = IHR

C      BACKDATE BY IDT HOURS

       RINC(2) = IDT
 
       CALL  W3MOVDAT ( RINC, NDATE, JDATE )

       RINC(2) = IDT

C      CREATE RUNDATE

       IY = JDATE(1)
       IM = JDATE(2)
       ID = JDATE(3)
       IH = JDATE(5)

       KDATE = 1000000*IY + 10000*IM + 100*ID + IH

       RETURN
       END
