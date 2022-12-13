C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C
C MAIN PROGRAM: wmow
C   PRGMMR: LILLY            ORG: NP12        DATE: 2005-06-21
C
C ABSTRACT: For 00Z cycle: gfs vfcn for 850MB, 500MB and 250MB winds
C           for the following areas; 1. Northern Hemisphere, 
C           2. Southern Hemisphaere and 3. Tropicals areas.
C           For 12Z cycle: gfs vfcn for 250MB winds on all areas.
C
C PROGRAM HISTORY LOG:
C
C UNKNOWN      Charles Vlcek -- ORIGINAL AUTHOR
C 2005-06-21 STEVEN G. LILLY -- UPDATING SOURCE CODES
C 2013-12-05 STEVEN G. LILLY -- Fixed variables imod and jmod in
C                               Subroutine getgrib. These variables
C                               were being reset to zero
C
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
C   MACHINE : IBM-SP
C
C$$$

C          DATA SET WMOWMAIN   AT LEVEL 013 AS OF 09/14/94
C
C.....WMO WIND VFCN ......... RMS VEC ERR .......02MAY90.....
C       ADDED 5.0DEG LATLON VFCN 25OCT90....
C
C.....APR94...SET ID(12)=99 REDUCE RETRIEVAL STMTS IN KEYSERS CODE
C.....JUN94...ELIMINATE DAILY STATISTICS (IPDLY=0)
c
c  Modifications by A. Krein ....
c
c..began conversion to cray on 10/15/96
c..conversion completed by     11/01/96
c..bug in rmsve fixed 02/07/97
c
c  Modifications by C. Vlcek ....
c
c..y2k compliant and compiled in f90 Sept. 1998
c..correct format of label record 11/05/1998
c..compile on IBM RS6000, add call to baopenr (in getgrib) 09/16/99
c..
c..
c..eliminate 5.0deg latlon vfcn option
c..run every cycle to generate daily scores
c..for 00Z cycle do:  gfs vfcn for 850, 500, 250mb winds 
c..                   on NH, SH, TROP areas only
c..                   avn vfcn for 250mb winds on all areas and quadrants
c..for 12Z cycle do:  avn vfcn for 250mb winds on all areas and quadrants
c..
c..separate program will generate monthly summary
C
       PARAMETER (MXCYL=1,MXFCT=10,MXLVL=3,MAREA=5,MXRGN=3,
     1            mxmdl=2,NSAVE=6*MAREA,NTOTL=NSAVE*MXRGN)
C
       CHARACTER(15) NAMRGN
       CHARACTER(11) NAREA
Csgl   CHARACTER(9)  JRUN(2)
       CHARACTER(10)  JRUN(2)
       CHARACTER(10) ITRUTH
       CHARACTER(8)  PLVL,PLEVEL
       CHARACTER(3)  JMDL(2),JANL(2),NAME,NTRU
       REAL          WEIGHT(37,MXRGN),SX(NTOTL,MXCYL,MXFCT,2)
       INTEGER       NFCTS(2),NPTS(MAREA,2),
     1               IDIM(2,MAREA,MXRGN),JDIM(2,MAREA,MXRGN)
       integer       idate(mxcyl),ivt(4,mxcyl)
       LOGICAL(1)    IVFCT(mxfct,mxmdl),IVLVL(mxlvl),IVMDL(2)
C
       DATA IDISK/20/
C
       COMMON /XDATA/ IDATE,IDS(4,MXLVL),IVFCT,
     1                IVT,IVLVL,PLVL(MXLVL)
       COMMON /YDATA/ NBEGIN,NEND,ICYB,ICYE,ICYI,IVFLD,IFTINC,
     1                NICR,JLVLS,JFCTS,MDANL,ITRUTH,NAREA(4),
     2                NAMRGN(MXRGN)
C
       DATA  ICON1 / 1000000000 /
C..... . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C
 100  FORMAT(4i4,1X,A5)
 110  FORMAT(I10)
 120  FORMAT(1X,3L1,2I3,1X,2L1,4I3,1X,A9,1X,2I2)
CTEST 120  FORMAT(1X,3L1,2I3,1X,2L1,4I3,1X,A9,2I2)
 125  FORMAT('LEVELS........ MODEL...    FLD         MDL ',
     1       /,' IV        J  N IV  J  N FT IV TRUTH    ANL')
 130  FORMAT(1X,A3,1X,A9,1X,A3,1X,10L1,I3)
 140  FORMAT(I3,3X,3I3,3X,2I3,3X,2I3,3X,I3)
 150  FORMAT( 6(10I4,/), 4(1X,A11),/,3(1X,A15) )
 200  FORMAT(15X,'****** LABELS ******')
 210  FORMAT('  *** VERIFICATION PERIOD',I12,' TO ' ,I12,' *** ',
     1       /,10X,'PRESSURE LEVELS = ',3L1,',  TOTAL = ',
     2       I2,', TO DO= ',I2,' .... MODELS = ',2L1,',  TOTAL= ',
     3       I2,', TO DO= ',I2,/, 15X,'.....TOTAL FCSTS = ',I2,
     4       5X,'..... VERIFYING FLD = ',I2,2X,A9,3X,'(MODEL =',I2,
     5       ')',//)
 220  FORMAT(10X,'.....IDENT  = ',I2,' ... ',4i9,5X,
     1       ' ... ',A8,' .....')
 230  FORMAT(10X,'.....MODEL ',I2,2X,A3,' ... ',A9,
     1       ' RUN ... ',A3,' ANALYSIS ..... ',/,30X,'FCSTS = ',10L1,
     2       ' .. TOTAL OF ',I2,' TIMES .. TO DO = ',I2)
 240   FORMAT(5X,'  VERIFY CYCLES (BEGIN,END,INCREMENT)= ',3I3,
     1        ' ... FCST INCREMENT= ',I3,
     2        ' ... BEGIN RGN= ',I2,'  END RGN= ',I2,/,
     3        ' ... DO AREA = ',I4,'  THRU AREA = ',I3,5X,
     4        ' ... GRID INCREMENT(1=2.5,2=2.5 AND 5.0DEG) ',I3,//)
 241   FORMAT(17X,'AREAS.............',/,17X,4(A11,4X),A15)
 243   FORMAT(8X,'LON',4(7X,2I4),8X,2I4,/,
     1        8X,'LAT',4(7X,2I4),8X,2I4,/)
 244   FORMAT(5X,'PTS(',I1,')',5(11X,I4),/,
     1        5X,'PTS(',I1,')',5(11X,I4))
C
C
 245   FORMAT(10X,'CYCLE=',I3,4X,'YR/MN/DY/HR = ',4I6,8X,
     1       'IDATE = ',i10)
 250   FORMAT(10X,'CYCLE=',I3,4X,'YR/MN/DY/HR = ',4I6,8X,
     1       'IDATE = ',i8)
C
 260   FORMAT(10X,'...........LAST CYCLE DATE = ',i10, 2X,
     1        'REQUESTED END DATE = ',i10,2X,'TOTAL CYCLES= ',I4,///)
 300   FORMAT(5X,'..... COSINE WEIGHTS FOR REGIONS .....')
 310   FORMAT(5X,'REGION= ',I4, ( 13F8.4) )
C
C
 320   FORMAT(///,5X,'..... ',A3,' (M=',I1,') VERIFIED AGNST ',A3,1X,
     1        A9,' (IVFLD=',I1,') ... ',I2,' LEVELS, ',I3,' FCSTS ',
     2        '... CYCLE BEGIN,END,INCREM= ',3I4,' .....',/,25X,
     3        '... IRGN BEGIN AND IRGN END ',2I4,' .....',//)
C
C..... . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C
C
       CALL IFILL(IDS,11,MXLVL,1,0)
       CALL IFILL(NPTS,MAREA,2,1,0)
       CALL AFILL(WEIGHT,37,MXRGN,1,99.9)
       DO I = 1,2
       CALL AFILL(SX(1,1,1,I),NTOTL,MXCYL,MXFCT,999.9)
       END DO
C
C...................................................................
C.....READ IN DATA, PRINT, AND INITIALIZE ARRAYS.....
C...................................................................
C..... . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C
c..input files
c..ft.11  current date
c..ft.12  pds values for winds for each level passed to getgrib
c..ft.13  input variables for models, verification parameter, etc
c..ft.14  boundaries for verification areas.  only used for 250mb avn vfcn 
c..
C
       READ(11,110,END=900) ndate
         nbegin=ndate
         nend=ndate
       PRINT 200
       DO J = 1,MXLVL
       READ(12,100,END=900) (IDS(I,J),I=1,4),PLVL(J)
        PRINT 220,J, (IDS(K,J),K=1,4),PLVL(J)
       END DO
C
       READ(13,120,END=900) IVLVL,JLVLS,NLVLS,IVMDL,JMDLS,NMDLS,
     1                      JFCTS,IVFLD,ITRUTH,MDANL,IPDLY
C
       PRINT 125
       PRINT 120, IVLVL,JLVLS,NLVLS,IVMDL,JMDLS,NMDLS,JFCTS,
     1            IVFLD,ITRUTH,MDANL
       PRINT 210, NBEGIN,NEND,IVLVL,JLVLS,NLVLS,IVMDL,JMDLS,NMDLS,
     1            JFCTS,IVFLD,ITRUTH,MDANL
C
       DO I = 1,JMDLS
       READ(13,130,END=900) JMDL(I),JRUN(I),JANL(I),
     1             (IVFCT(J,I),J=1,JFCTS),NFCTS(I)
       PRINT 230,I,JMDL(I),JRUN(I),JANL(I),(IVFCT(J,I),J=1,JFCTS),
     1             JFCTS,NFCTS(I)
       END DO
C
C
       READ(13,140,END=900) IFTINC,ICYB,ICYE,ICYI,IRGNB,IRGNE,
     1                      MARB,MARE,NICR
       PRINT 240, ICYB,ICYE,ICYI,IFTINC,IRGNB,IRGNE,MARB,MARE,NICR
C
       if (ipdly .eq. 1) then
          READ(14,150,END=900) IDIM,JDIM,NAREA,NAMRGN
          LOOP14: DO J = 1,MXRGN
          PRINT 241,(NAREA(I),I=1,4),NAMRGN(J)
          PRINT 243,((IDIM(1,I1,J),IDIM(2,I1,J)),I1=1,MAREA),
     1              ((JDIM(1,I2,J),JDIM(2,I2,J)),I2=1,MAREA)
          LOOP15: DO K = 1,MAREA
          LOOP16: DO L = 1,NICR
             NPTS(K,L) = ((IDIM(2,K,J) - IDIM(1,K,J))/L  +  1) *
     1                   ((JDIM(2,K,J) - JDIM(1,K,J))/L  +  1)
          END DO LOOP16
          END DO LOOP15
          PRINT 244,(I2,(NPTS(I1,I2),I1=1,MAREA), I2=1,NICR)
          END DO LOOP14
       ENDIF
C
C
C....................................................................
C.....1. SETUP VERIFICATION TIME TABLE.....
C.....2. DEFINE WEIGHT (COSINE LAT....1D AND 1E MAPS)........
C         REGION 3 (TROPICS) IS 20S TO 20 N
C....................................................................
C..... . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C
        CALL IFILL(IDATE,MXCYL,1,1,0)
        CALL IFILL(IVT,4,MXCYL,1,0)
C
        KOUNT = 0
        KBEGIN = nBEGIN
        LOOP20: DO IJ = 1,MXCYL
          INCR = IJ - 1
          KOUNT = KOUNT + 1
         IDATE(IJ)=KBEGIN
C
         IF ( IDATE(IJ) .LT. ICON1 ) THEN
C
C          DATE FORMAT: YYMMDDHH
C
           CALL DATECNV8(IDATE(IJ),IVT(1,IJ),IVT(2,IJ),IVT(3,IJ),
     1                   IVT(4,IJ))
C
           PRINT 250,IJ,IVT(1,IJ),IVT(2,IJ),IVT(3,IJ),IVT(4,IJ),
     1               IDATE(IJ)
C
         ELSE
C
C          DATE FORMAT: YYYYMMDDHH
C  
           CALL DATECNV10( IDATE(IJ), IVT(1,IJ), IVT(2,IJ),
     1                    IVT(3,IJ),  IVT(4,IJ))
           PRINT 245,IJ,IVT(1,IJ),IVT(2,IJ),IVT(3,IJ),IVT(4,IJ),
     1               IDATE(IJ)
C
         END IF
C
          IF(IDATE(IJ).EQ.nEND) EXIT LOOP20
        END DO LOOP20
C
        PRINT 260,IDATE(IJ),nEND,KOUNT
C
C.....
        DO J = 1,37
           XLAT = 2.5*(J-1)/57.296
           JJ = 37 - J + 1
           WEIGHT(J,1) = COS(XLAT)
           WEIGHT(JJ,2) = WEIGHT(J,1)
        END DO
C
        DO J = 1,9
           JJ = J + 28
           WEIGHT(J,3) = WEIGHT(JJ,2)
        END DO
C
        DO J = 10,17
           JJ = J - 8
           WEIGHT(J,3) = WEIGHT(JJ,1)
        END DO
C
       PRINT 300
c
       DO J = 1,3
          PRINT 310,J,(WEIGHT(I,J),I=1,37)
       END DO
C
C
C....................................................................
C      1. DO daily VERIFICATION
C      2. PRINT SUMMARY TABLES
c      3. monthly summary of scores is done in program wsum.f
C...................................................................
C
C
       LOOP30: DO MODEL = 1,JMDLS
       NTRU = JMDL(MDANL)
       NAME = JMDL(MODEL)
       IF(.NOT. IVMDL(MODEL)) CYCLE LOOP30
C
       PRINT 320,NAME,MODEL,NTRU,ITRUTH,IVFLD,JLVLS,JFCTS,
     1           ICYB,ICYE,ICYI,IRGNB,IRGNE
C
C....
       if (name .eq. 'GFS') then
c
          LOOP40: DO LVL = 1,JLVLS
          PLEVEL = PLVL(LVL)
C
          IF(.NOT. IVLVL(LVL)) CYCLE LOOP40
c
          CALL WNDEVL(MODEL,NAME,LVL,WEIGHT,IRGNB,IRGNE,
     1                IDIM,JDIM,SX,IPDLY)
       END DO LOOP40
       elseif (name .eq. 'AVN') then 
          plevel = plvl(3)
          lvl=3
          CALL WNDEVL(MODEL,NAME,LVL,WEIGHT,IRGNB,IRGNE,
     1                IDIM,JDIM,SX,IPDLY)
       ENDIF
C
C.....END OF LEVEL LOOP.....
C
C.....END OF MODEL LOOP.....
       END DO LOOP30
C
C
 900   CONTINUE
C
C
       STOP
       END
c
      SUBROUTINE GETGRIB(run,id,ianl,iver,ifhr,fieldu,fieldv,iret)
c  subprogram getgrib
c  programmer:  Andrew Krein
c
c  Abstract:  Retrieves the requested grib data for a particular
c  valid time.
c
c  Program History Log:
c    96-06-20  Andrew Krein
c    96-10-16  modified for use with wmow.f for gfs/avn only
c    98-08-??  C. Vlcek  compiled on f90, made y2k compliant
c    99-09-16  C. Vlcek  Compiled on IBM RS6000, added baopenr
c    13-12-05  Fixed variables imod and jmod for the 240 forecast hour.
c              These variables were set to zero.
c
c  Usage:  call getgrib(run,id,ianl,iver,ifhr,field,iret)
c    Input Argument List:
c      run:  integer model name (1=GFS, 2=AVN, 3=NGM, 4=ETA)
c      id:   pds numbers from label
c      id1:  pds value 33=u component, 34=v component
c      ianl: non-zero for analysis
c      iver: non-zero for verifying at desired date/time
c      ifhr: forecast hour
c
c    Output Argument List:
c      fieldu:  grid data array for u-component of wind
c      fieldv:  grid data array for v-component of wind
c      iret:   error message from getgb
c
c  Other variables used
c    mxcyl - maximum number of cycles in a monthly verification
c    mxlvl - maximum number of levels to in verification
c    mxfct - maximum number of forecast times to verify
c    ip    - interpolation method (=0 for bilinear)
c    ipopt - interpolation options (=0 for none)
c
c  Subprograms called:
c    getgb,ipolatev
c
c  Variables passed to getgb:
c    idat - logical unit of the unblocked grib data file
c    iinv - logical unit of the unblocked grib index file
c    ipts - integer number of data points to unpack
c    0    - number of messages to skip, =0 to search from beginning
c    jpds - integer (25) pds parameters to search for (-1 for wildcard)
c    jgds - integer (22) gds parameters to search for (-1 for wildcard)
c           only searched for if jpds(3)=255
c
c  Output from getgb
c    ki    - integer number of points unpacked
c    kr    - integer message number unpacked
c    kpdsi - integer (25) unpacked pds parameters
c    kgdsi - integer (22) unpacked gds parameters
c    li    - logical (ki) unpacked bitmap if present
c    hi    - real (ki) unpacked data
c
c  Attributes:
c    Language:  FORTRAN-90
c    Machine:   IBM RS6000
c
c
       parameter (mxmdl=2,mxfct=10,mxlvl=1,
     1            mxcyl=1,mxin=360*181,mxout=145*37)
       character(10) itruth
       character(8) cdat(11), cindex(11)
       character(8) cdt, cdx
       character(6) plvl
       character gdso(42)
       logical(1) ivfct,ivlvl
       logical(1) li(mxin),lo(mxout)
       real ui(mxin),uo(mxout)
       real vi(mxin),vo(mxout)
       real rlat(mxout),rlon(mxout)
       real crot(mxout),srot(mxout)
       real fieldu(145,37),fieldv(145,37)
       integer id(12),jpds(25),jgds(22)
       integer kpdsi(25),kgdsi(22),ibi(mxlvl),ibo(mxlvl)
       integer kpdso(25),kgdso(22)
       integer dat(11),inv(11)
       integer run, ipopt(20)
c
c
       data cdat /'GRIBAD0','GRIBFD1','GRIBFD2','GRIBFD3','GRIBFD4',
     &            'GRIBFD5','GRIBFD6','GRIBFD7','GRIBFD8','GRIBFD9',
     &            'GRIBFDL'/
       data cindex /'GRIBAI0','GRIBFI1','GRIBFI2','GRIBFI3','GRIBFI4',
     &              'GRIBFI5','GRIBFI6','GRIBFI7','GRIBFI8','GRIBFI9',
     &              'GRIBFIL'/
c
c
c  define output locations kdgso(1)<0 ==> rlat and rlon are input args
c  into ipolatev
c
c
       k=1
       jpds=-1
       jpds(6)=id(3)
       jpds(7)=id(4)
       ip=0
       ipopt=0
c
c
c  define data and inventory locations for forecasts valid at ivfdte
c
       IF ((ianl .eq. 0) .and. (iver .eq. 1) .and. (ifhr .ne. 0)) then
          ida=31
          iin=51
          DO i=1,10
             dat(i)=ida
             inv(i)=iin
             ida=ida+1
             iin=iin+1
          END DO
       ENDIF
c
c  define data and inventory locations for analyses valid at ivfdte-ifhr
c
c  set idat and iinv for the current analysis field
c
       IF ((ianl .eq. 1) .and. (iver .eq. 0) .and. (ifhr .eq. 0)) then
         idat=20
         iinv=25
c         print*,'***Analysis ianl,iver,ifhr = ',ianl,iver,ifhr
c         print*,'cdat ',cdat(1)
         call baopenr(idat,cdat(1),ier1)
         call baopenr(iinv,cindex(1),ier2)
         IF (ier1.ne.0.or.ier2.ne.0)    then
           print *, ' *** Cannot open analysis file, quitting ***'
           stop 99
         ENDIF
       ENDIF
c
       LOOP80: DO MX=1,1

       IF (ianl.eq.0.and.iver.eq.1) then
c         print*,'Forecast hour - ifhr,units idat,iinv ',ifhr,idat,iinv

c
C for idat and cdat, when ifhr = 10, imod is set to 11
C reason: imods is reset to zero with each passages.
c

         idat=dat(ifhr)
         iinv=inv(ifhr)
         if (ifhr .lt. 10) then
            imod = mod(iinv,10) + 1
            imods = imod
c            print*,'imod values = ',imod
c            print*,'cdat (',imod,')',cdat(imod)
         elseif(ifhr .eq. 10) then
            imod = 11
c            print*,'cdat (',imod,')',cdat(imod)
         endif 

         call baopenr(idat,cdat(imod),ier1)
         call baopenr(iinv,cindex(imod),ier2)
         IF (ier1.ne.0.or.ier2.ne.0)    then
           print *, ' *** cannot open forecast files ',
     &       idat,' ',cdat(imod),'  ',iinv,' ',cindex(imod),' ***'
           IF (ier1.ne.0)    then
             iret = ier1
           ELSE
             iret = ier2
           ENDIF
           CYCLE LOOP80
         ENDIF
       ENDIF
c
c  get u-component
c
       jpds(5)=id(1)
c
         call getgb(idat,iinv,mxin,0,jpds,jgds,
     1              ki,kr,kpdsi,kgdsi,li(k),ui(k),iret)
c
         IF (iret .ne. 0) then
           print*
           print*,'from getgbu: iret= ',iret
           print*,'idat, iinv= ',idat,iinv
           print*
           CYCLE LOOP80
         ENDIF
c
c  get v-component
c
         jpds(5)=id(2)
c
         call getgb(idat,iinv,mxin,0,jpds,jgds,
     1              ki,kr,kpdsi,kgdsi,li(k),vi(k),iret)
c
         IF (iret .ne. 0) then
           print*
           print*,'from getgbv: iret= ',iret
           print*,'idat, iinv= ',idat,iinv
           print*
           CYCLE LOOP80
         ENDIF
c
         ibi(k)=mod(kpdsi(4)/64,2)
c
c  call makgds for the NH or SH global model on a 145x37 grid (29 or 30)
c
         ig=id(6)
         call makgds(ig,kgdso,gdso,lengds,iret)
c
c  correction for error in hgds parameters returned from makgds for grid 30
c
        IF (ig .eq. 30) then
          kgdso(4)=-90000
          kgdso(5)=0
        ENDIF
c
         IF (iret .ne. 0) then
           print*
           print*,'from makgds: iret= ',iret
           print*,'idat, iinv= ',idat,iinv
           print*
           CYCLE LOOP80
         ENDIF
c
c  interpolate from 1 degree lola to 2.5 degree lola grids
c
       call ipolatev(ip,ipopt,kgdsi,kgdso,mxin,mxout,k,0,li,ui,vi,
     1               ko,rlat,rlon,crot,srot,ibo,lo,uo,vo,iret1)
c
         IF (iret .ne. 0) then
           print*,'from ipolatev: iret= ',iret
           print*,'idat, iinv= ',idat,iinv
           print*
           CYCLE LOOP80
         ENDIF
c
       iz=0
       DO iy=1,37
         DO ix=1,145
           iz=iz+1
c
c  for winds at any isobaric level.  convert m/s to kts.
c
             fieldu(ix,iy)=uo(iz)
             fieldv(ix,iy)=vo(iz)
c
         END DO
       END DO
c
c
       END DO LOOP80
       RETURN
       END
