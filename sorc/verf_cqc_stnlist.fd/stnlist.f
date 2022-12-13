C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C
C MAIN PROGRAM: STNLIST
C   PRGMMR: LILLY            ORG: NP12        DATE: 2009-06-01
C
C ABSTRACT: Compares to determine if a station matches to a station
C           which is already on a pre-define list 
C
C PROGRAM HISTORY LOG:
C
C 1991-12-24  W. Collins -- ORIGINAL AUTHOR
C 2009-06-01 Steve Lilly -- add tmplate for documentation block
C
C USAGE:
C   INPUT FILES:
C
C   OUTPUT FILES:
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

      PROGRAM STNLIST
      CHARACTER*1 CD(2000,62), X, BLANK, Z, CNUM(11)
      CHARACTER*1  BUF(40)
      CHARACTER*2 CMO, CDY, CHR
      CHARACTER*4 PART, ABUF, CYR
      CHARACTER*8 SID, CID(2000), BLANK8
      CHARACTER*10 CDT, CDATE(62), BLANK10
      INTEGER     ICAT(5), JCAT(2000,5), JLV(2000), NUM(2000,2),
     &            ID(2000), IORDER(2000), IDAT(8), JDAT(8)
      REAL        VINC(21,4), RINC(5)
      LOGICAL     START
      EQUIVALENCE (BUF(37), ABUF)
      DATA        X /'X'/, BLANK /' '/, Z /'0'/, BLANK8 /'        '/
      DATA        BLANK10 /'          '/
      DATA        CNUM /' ','0','1','2','3','4','5','6','7','8','9'/

C  INITIALIZE SOME QUANTITIES

      START = .TRUE.
      LAST  = 1
      DO J=1,62
        DO I=1,2000
          CD(I,J) = BLANK
        ENDDO
      ENDDO
      DO I=1,2000
        JLV(I) = 0
      ENDDO
      DO J=1,2
        DO I=1,2000
          NUM(I,J) = 0
        ENDDO
      ENDDO

C  READ DATA

  100 READ(16,499,END=200,ERR=400) CDT,DHR,SID,NLV,PART,(ICAT(I),I=1,5)
      IF(PART.EQ.'MASS') THEN
        READ(16,500,END=200,ERR=400)
     &     ((VINC(J,K),J=1,21),K=1,3)
        IPART = 1
        READ(16,511,END=200,ERR=400) BUF
        IF(ABUF.EQ.'MASS' .OR. ABUF.EQ.'WIND') BACKSPACE 16
      ELSEIF(PART.EQ.'WIND') THEN
        READ(16,510,END=200,ERR=400)
     &     ((VINC(J,K),J=1,21),K=1,4)
        IPART = 2
      ELSEIF(PART.EQ.'    ') THEN
        GOTO 200
      ELSE
        GOTO 100
      ENDIF 
  499 FORMAT(1X,A10,1X,F8.2,1X,A8,1X,I5,1X,A4,1X,5I5)
  500 FORMAT(10X,21F8.0,/,10X,21F8.1,/,10X,21F8.0)
  510 FORMAT(10X,21F8.1,/,10X,21F8.1,/,10X,21F8.1,/,10X,21F8.1)
  511 FORMAT(40A1)

      IF(CDT.EQ.BLANK10 .OR. SID.EQ.BLANK8) GOTO 200

C  SKIP SHIPS

      ICHAR = 0
      DO I=1,8
        DO J=1,11
          IF(SID(I:I).EQ.CNUM(J)) GOTO 5
        ENDDO
        ICHAR = ICHAR + 1
    5   CONTINUE
      ENDDO
      IF(ICHAR.NE.0) GOTO 100

      READ(UNIT=SID,FMT=502) IDENT
  502 FORMAT(I8)

C  SET UP AN ARRAY OF POSSIBLE DATES, STARTING FROM
C  00 UTC ON FIRST DAY OF DATE OF THE FIRST DATA

      IF(START) THEN
        CYR = CDT(1:4)
        CMO = CDT(5:6)
        CDY = '01'
        CHR = '00'
        READ(UNIT=CYR,FMT=5011) IYR
        READ(UNIT=CMO,FMT=506) IMO
        READ(UNIT=CDY,FMT=506) IDY
        READ(UNIT=CHR,FMT=506) IHR
  506   FORMAT(I2)
        WRITE(6,508) CYR,CMO,CDY,CHR
  508   FORMAT(' FIRST YEAR,MONTH,DAY,HOUR = ',3X,A4,3(3X,A2))
        IDAT(1) = IYR
        IDAT(2) = IMO
        IDAT(3) = IDY
        IDAT(4) = 0
        IDAT(5) = IHR
        IDAT(6) = 0
        IDAT(7) = 0
        IDAT(8) = 0
        DO I=1,62
          INCR = (I-1)*12
          RINC(1) = 0
          RINC(2) = INCR
          RINC(3) = 0
          RINC(4) = 0
          RINC(5) = 0
          CALL W3MOVDAT(RINC,IDAT,JDAT)
          IYR = JDAT(1)
          IMO = JDAT(2)
          IDY = JDAT(3)
          IHR = JDAT(5)          
	  WRITE(UNIT=CYR,FMT=5011) IYR
	  WRITE(UNIT=CMO,FMT=501) IMO
	  WRITE(UNIT=CDY,FMT=501) IDY
	  WRITE(UNIT=CHR,FMT=501) IHR
  501     FORMAT(I2)
 5011     FORMAT(I4)
          IF(CYR(1:1).EQ.BLANK) CYR(1:1) = Z
          IF(CMO(1:1).EQ.BLANK) CMO(1:1) = Z
          IF(CDY(1:1).EQ.BLANK) CDY(1:1) = Z
          IF(CHR(1:1).EQ.BLANK) CHR(1:1) = Z
          CDATE(I)(1:4) = CYR
          CDATE(I)(5:6) = CMO
          CDATE(I)(7:8) = CDY
          CDATE(I)(9:10) = CHR
        ENDDO
        IS      = 1
        ID(IS)  = IDENT
        CID(IS) = SID
        JLV(IS) = NLV
        NUM(IS,IPART) = 1
        DO I=1,5
          JCAT(IS,I) = ICAT(I)
        ENDDO
        CD(IS,1) = X
        START = .FALSE.
      ENDIF

C  MATCH THE DATE OF THE DATA WITH CDATE

      DO II=1,62
        I = II
        IF(CDT.EQ.CDATE(I)) GOTO 10
      ENDDO
      
C  NO DATE MATCH

      GOTO 100
   10 CONTINUE
      IF(I.GT.LAST) LAST = I

C  SEE IF STATION MATCHES ONE ALREADY ON LIST

      DO JJ=1,IS
        J = JJ
        IF(SID.EQ.CID(JJ)) GOTO 20
      ENDDO

C  NEW STATION

      IS      = IS + 1
      ID(IS)  = IDENT
      CID(IS) = SID
      J       = IS

C  STATION ALREADY ON LIST

   20 NUM(J,IPART)  = NUM(J,IPART) + 1
      JLV(J)  = JLV(J) + NLV
      DO K=1,5
        JCAT(J,K) = JCAT(J,K) + ICAT(K)
      ENDDO
      CD(J,I) = X
      GOTO 100

C  ERROR IN READING INPUT DATA
C  STOP READING, BUT CONTINUE CALCULATIONS
C  ---------------------------------------

  400 CONTINUE
      WRITE(6,530)
  530 FORMAT(' ERROR IN READING FROM UNIT 16.')

C  END OF STATION LIST

  200 CONTINUE
      WRITE(6,513) IS
  513 FORMAT(' THE NUMBER OF UNIQUE STATIONS IS ',I6)

C  SORT NUM, JLV, JCAT, CD ACCORDING TO ID

      CALL SHELL(ID,IORDER,IS,0)
      CALL SORT(NUM(1,1),IORDER,IS)
      CALL SORT(NUM(1,2),IORDER,IS)
      CALL SORT(JLV,IORDER,IS)
      DO I=1,5
        CALL SORT(JCAT(1,I),IORDER,IS)
      ENDDO
      DO I=1,62
        CALL SORTC1(CD(1,I),IORDER,IS)
      ENDDO

C  COMPLETE COMPUTATION OF AVERAGES

      DO I=1,IS
        IF(NUM(I,1).GT.0) THEN
          JLV(I) = ANINT(FLOAT(JLV(I))/FLOAT(NUM(I,1)))
          DO J=1,2
            JCAT(I,J) = ANINT(FLOAT(JCAT(I,J))/FLOAT(NUM(I,1)))
          ENDDO
        ENDIF
        IF(NUM(I,2).GT.0) THEN
          DO J=3,4
            JCAT(I,J) = ANINT(FLOAT(JCAT(I,J))/FLOAT(NUM(I,2)))
          ENDDO
        ENDIF
        IF(NUM(I,1)+NUM(I,2).GT.0) THEN
          JCAT(I,5) = ANINT(FLOAT(JCAT(I,5))/(FLOAT(NUM(I,1)+NUM(I,2))))
          JLV(I) = JLV(I) - JCAT(I,5)
        ENDIF
      ENDDO

C  PRINT RESULTS

      DO I=1,IS
        IF(MOD(I-1,10).EQ.0) WRITE(6,504)
        WRITE(6,505) ID(I),NUM(I,1),NUM(I,2),JLV(I),(JCAT(I,J),J=1,5),
     &    (CD(I,K),K=1,LAST)
      ENDDO
  504 FORMAT('STATION NUMM NUMW LEVS CAT1 CAT2 CAT3 CAT4 CAT5     01',
     &       '      05        10        15        20        25',
     &       '        30  ')
  505 FORMAT(2X,I5,8(1X,I4),5X,62A1)
      STOP
      END

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    SHELL       SHELL SORT BASED ON V.
C   PRGMMR: W. COLLINS       ORG: W/NMC22    DATE: 12-24-91
C
C ABSTRACT: SHELL SORT, BASED UPON THE VALUES OF V.
C   IV IS THE ORIGINAL INDEX OF EACH ELEMENT OF V.
C
C PROGRAM HISTORY LOG:
C   91-12-24  W. COLLINS
C
C USAGE:    CALL SHELL(V,IV,MAX)
C   INPUT ARGUMENT LIST:
C     V        - VARIABLE
C     MAX      - DIMENSION OF V
C     IREV     = 0 FOR ASCENDING ORDER
C             <> 0 FOR DESCENDING ORDER
C
C   OUTPUT ARGUMENT LIST:
C     IV       - ORIGINAL INDEX OF VARIABLE
C     V        - VARIABLE, SORTED.
C
C ATTRIBUTES:
C   LANGUAGE: VS FORTRAN
C   MACHINE:  , CRAY
C
C$$$
      SUBROUTINE SHELL(V,IV,MAX,IREV)
      INTEGER   V(*), VT
      INTEGER   IV(*)
      DO 10 I=1,MAX
        IV(I) = I
   10 CONTINUE
      IOFSET = MAX/2
   20 CONTINUE
      LIM = MAX - IOFSET
   30 CONTINUE
      ISW = 0
      DO 40 I=1,LIM
        IF(V(I).GT.V(I+IOFSET)) THEN
          VT = V(I)
          V(I) = V(I+IOFSET)
          V(I+IOFSET) = VT
          IVT = IV(I)
          IV(I) = IV(I+IOFSET)
          IV(I+IOFSET) = IVT
          ISW = I
        ENDIF
   40 CONTINUE
      LIM = ISW - IOFSET
      IF(ISW.NE.0) GO TO 30
      IOFSET = IOFSET/2
      IF(IOFSET.GT.0) GO TO 20

      IF(IREV.NE.0) THEN
C        REVERSE SORT ORDER...
         NH = MAX/2
         DO I=1,NH
            ITEMP = IV(I)
            IV(I) = IV(MAX+1-I)
            IV(MAX+1-I) = ITEMP
            TEMP = V(I)
            V(I) = V(MAX+1-I)
            V(MAX+1-I) = TEMP
         ENDDO
      ENDIF
      RETURN
      END

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    SORT        SORT, BASED ON ORDER IN INDX
C   PRGMMR: W. COLLINS       ORG: W/NMC22    DATE: 94-03-17
C
C ABSTRACT:
C   SORT RA ACCORDING TO THE ORDER SPECIFIED BY THE
C   INDICES IN INDX.
C
C PROGRAM HISTORY LOG:
C   94-03-17  W. COLLINS
C
C USAGE:    CALL SORT(RA, INDX, N)
C   INPUT ARGUMENT LIST:
C     RA       - VARIABLE
C     INDX     - ORDER FOR REARRANGEMENT OF RA
C     N        - DIMENSION OF RA
C
C   OUTPUT ARGUMENT LIST:
C     RA       - VARIABLE
C
C ATTRIBUTES:
C   LANGUAGE: VS FORTRAN
C   MACHINE:  , CRAY
C
C$$$
      SUBROUTINE SORT(RA,INDX,N)
 

C     SORT RA ACCORDING TO THE ORDER SPECIFIED BY THE
C     INDICES IN INDX.
 
      DIMENSION RA(*), WKSP(2000)
      INTEGER INDX(*)
      DO J=1,N
         WKSP(J) = RA(J)
      ENDDO
      DO J=1,N
         RA(J) = WKSP(INDX(J))
      ENDDO
      RETURN
      END

      SUBROUTINE SORTC1(RA,INDX,N)

C$$$  SUBROUTINE DOCUMENTATION BLOCK
C
C SUBPROGRAM: SORTC1
C   PRGMMR: LILLY            ORG: NP12        DATE: 2009-09-23
C
C ABSTRACT: Sort RA according to the order specified by the indices in INDX.
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

 
C     SORT RA ACCORDING TO THE ORDER SPECIFIED BY THE
C     INDICES IN INDX.
 
      CHARACTER*1 RA(*), WKSP(2000)
      INTEGER INDX(*)
      DO J=1,N
         WKSP(J) = RA(J)
      ENDDO
      DO J=1,N
         RA(J) = WKSP(INDX(J))
      ENDDO
      RETURN
      END
