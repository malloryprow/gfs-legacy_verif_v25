C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C
C MAIN PROGRAM: BIAS
C   PRGMMR: LILLY            ORG: NP12        DATE: 2009-06-01
C
C ABSTRACT: Compute statistics for baseline residuals
C
C PROGRAM HISTORY LOG:
C
C 1991-12-24  W. Collins -- ORIGINAL AUTHOR
C 2009-06-01 Steve Lilly -- add tmplate for documentation block
C 2010-04-07 Steve Lilly -- increase memory space to prevent baseline
C                           bias errors from occurring
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

      PROGRAM BIAS

      dimension    vinc(21,7), cid(1000), icat(5),
     &             n(1000,21,7,4), s(1000,21,7,4),
     &             q(1000,21,7,4), c(1000,21,7,4),
     &             f(1000,21,7,4), mean(1000,21,7,4),
     &             std(1000,21,7,4), skew(1000,21,7,4),
     &             kurt(1000,21,7,4), p(21), date(200),
     &             ii(1000), xn(21,7), pw(1000,4)
      dimension    nb(1000,3), sb(1000,3), qb(1000,3),
     &             cb(1000,3), fb(1000,3), meanb(1000,3), stdb(1000,3),
     &             skewb(1000,3), kurtb(1000,3), bas(1000,1000),
     &             nbas(1000), pi(1000,1000), npi(1000),
     &             zi(1000,1000), nzi(1000)
      real         vinc, s, q, c, f, mean, std,
     &             skew, kurt, p, basres, sb, qb, cb, fb,
     &             meanb, stdb, skewb, kurtb, pi, zis, zi
      integer      icat, n, nlv, idate(5), itimes(4), nb, nbas, npi, nzi
      character*4  part
      character*8  cid, sid, stnmiss, stnout
      character*10 cdt, date
      character*39 stname
      logical timematch, good(1000,1000),test

      data nfin /16/, nfout /26/, bmiss /9999./
      data p /1000.,925.,850.,700.,500.,400.,300.,250.,200.,150.,
     &        100.,70.,50.,30.,20.,10.,7.,5.,3.,2.,1./
      data stnmiss /'99999999'/, itimes /00,06,12,18/, g /9.80665/


      NAMELIST /NAMLST/ TEST
      DATA TEST/.TRUE./

      READ(5,NAMLST,ERR=8,END=8)
    8 PRINT *,' TEST: ',TEST


      n = 0
      s = 0.
      q = 0.
      c = 0.
      f = 0.
      mean = 0.
      std = 0.
      skew = 0.
      nb = 0
      sb = 0.
      qb = 0.
      cb = 0.
      fb = 0.
      meanb = 0.
      stdb = 0.
      skewb = 0.
      ns = 1
      cid(1) = 'ALL'
      nd = 0
      nbas = 0
      nbasmax = 0
      npi = 0

C  Read collected increments and compute statistics.

  100 continue
      read(nfin,500,end=200,err=400) cdt,dhr,sid,nlv,part,
     &  (icat(i),i=1,5)
C     write(62,500) cdt,dhr,sid,nlv,part,(icat(i),i=1,5)
      read(cdt,'(5i2)') idate
      if(part.eq.'MASS') then
        read(nfin,659,end=200,err=400) ((vinc(j,k),j=1,21),k=1,3),
     &    basres,pis,zis
C       write(62,659) ((vinc(j,k),j=1,21),k=1,3),basres
        iv1 = 1
        iv2 = 3
      elseif(part.eq.'WIND') then
        read(nfin,660,end=200,err=400) ((vinc(j,k),j=1,21),k=4,7)

C  Make sure that direction increments are in range (-180,180)

        do j=1,21
          if(vinc(j,7).lt.bmiss) then
            vinc(j,7) = mod(vinc(j,7)+360.,360.)
            if(vinc(j,7).gt.180.) vinc(j,7) = vinc(j,7) - 360.
          endif
        enddo

C       write(62,660) ((vinc(j,k),j=1,21),k=4,7)
        iv1 = 4
        iv2 = 7
      elseif(part.eq.'    ') then
        goto 200
      endif
  500 format(1x,a10,1x,f8.2,1x,a8,1x,i5,1x,a4,1x,5i5)
  659 format(10x,21f8.0,/,10x,21f8.1,/,10x,21f8.0,/,10x,f8.1,
     &  f8.1,f8.0)
  660 format(10x,21f8.1,/,10x,21f8.1,/,10x,21f8.1,/,10x,21f8.1)

C  Collect list of dates.

      do i=1,nd
        if(cdt.eq.date(i)) goto 5
      enddo
      nd = nd + 1
      date(nd) = cdt
    5 continue

C  Check station against list already read in.

      do i=1,ns
        is = i
        if(sid.eq.cid(i)) goto 10
      enddo

C  No match found.  Add this station to the list.

      ns = ns + 1
      is = ns
      cid(is) = sid

C  Match found.  Add to statistics.

   10 continue

      timematch = .false.
      if(idate(5).eq.itimes(1)) it = 1
      if(idate(5).eq.itimes(2)) it = 2
      if(idate(5).eq.itimes(3)) it = 3
      if(idate(5).eq.itimes(4)) it = 4
      if(idate(5).eq.itimes(1) .or.
     &   idate(5).eq.itimes(2) .or.
     &   idate(5).eq.itimes(3) .or.
     &   idate(5).eq.itimes(4)) timematch = .true.

      if(part.eq.'MASS' .and. basres.lt.0.5*bmiss .and. timematch) then
        nb(is,1) = nb(is,1) + 1
        sb(is,1) = sb(is,1) + basres
        qb(is,1) = qb(is,1) + basres**2
        nbas(is) = nbas(is) + 1
        if(nbas(is).gt.nbasmax) nbasmax = nbas(is)
        bas(is,nbas(is)) = basres
        pi(is,nbas(is)) = pis
        zi(is,nbas(is)) = zis
      endif
      if(part.eq.'MASS' .and. pis.lt.0.5*bmiss .and. timematch) then
        nb(is,2) = nb(is,2) + 1
        sb(is,2) = sb(is,2) + pis
        qb(is,2) = qb(is,2) + pis**2
      endif
      if(part.eq.'MASS' .and. zis.lt.0.5*bmiss .and. timematch) then
        nb(is,3) = nb(is,3) + 1
        sb(is,3) = sb(is,3) + zis
        qb(is,3) = qb(is,3) + zis**2
      endif

      do l=1,21
        do iv=iv1,iv2
          if(vinc(l,iv).lt.bmiss) then
            n(is,l,iv,it) = n(is,l,iv,it) + 1
            s(is,l,iv,it) = s(is,l,iv,it) + vinc(l,iv)
            q(is,l,iv,it) = q(is,l,iv,it) + vinc(l,iv)**2
            c(is,l,iv,it) = c(is,l,iv,it) + vinc(l,iv)**3
            f(is,l,iv,it) = f(is,l,iv,it) + vinc(l,iv)**4
            n(1,l,iv,it) = n(1,l,iv,it) + 1
            s(1,l,iv,it) = s(1,l,iv,it) + vinc(l,iv)
            q(1,l,iv,it) = q(1,l,iv,it) + vinc(l,iv)**2
            c(1,l,iv,it) = c(1,l,iv,it) + vinc(l,iv)**3
            f(1,l,iv,it) = f(1,l,iv,it) + vinc(l,iv)**4
          endif
        enddo
      enddo

C  Go back for more data.

      goto 100

  200 continue

C  Sort by block/station number.

      do i=1,ns
        ii(i) = i
      enddo
        call shell(cid,ii,ns,0)
        call sort(nb(1,1),ii,ns)
        call sort(nb(1,2),ii,ns)
        call sort(nb(1,3),ii,ns)
        call sort(nbas,ii,ns)
        call sort(sb(1,1),ii,ns)
        call sort(sb(1,2),ii,ns)
        call sort(sb(1,3),ii,ns)
        call sort(qb(1,1),ii,ns)
        call sort(qb(1,2),ii,ns)
        call sort(qb(1,3),ii,ns)
      do i=1,nbasmax
        call sort(bas(1,i),ii,ns)
        call sort(pi(1,i),ii,ns)
        call sort(zi(1,i),ii,ns)
      enddo
      do it=1,4
        do iv=1,7
          do l=1,21
            call sort(n(1,l,iv,it),ii,ns)
            call sort(s(1,l,iv,it),ii,ns)
            call sort(q(1,l,iv,it),ii,ns)
            call sort(c(1,l,iv,it),ii,ns)
            call sort(f(1,l,iv,it),ii,ns)
          enddo
        enddo
      enddo

C  Compute statistics for baseline residuals.

      do i=1,ns
        if(nb(i,1).lt.1) then
          do j=1,3
            meanb(i,j) = 0.
            stdb(i,j) = 0.
            skewb(i,j) = 0.
            kurtb(i,j) = 0.
          enddo
        else
          do j=1,3
            sum1 = sb(i,j)/nb(i,j)
            sum2 = qb(i,j)/nb(i,j)
            meanb(i,j) = sum1
            arg = sum2 - sum1**2
            if(arg.gt.0.) then
              stdb(i,j) = sqrt(arg)
            else
              stdb(i,j) = 0.
            endif
          enddo
C Assign quality mark

          nb(i,1) = 0
          sb(i,1) = 0.
          qb(i,1) = 0.
          cb(i,1) = 0.
          fb(i,1) = 0.

          do j=1,nbas(i)
            if((abs(bas(i,j)-meanb(i,1)).lt.1.0*stdb(i,1) .or.
     &        abs(bas(i,j)-meanb(i,1)).lt.10.) .and.
     &        nbas(i).ge.1) then
              good(i,j) = .true.
            else
              good(i,j) = .false.
            endif
          enddo
C  Recalculate baseline statistics, using only 'good' obs.

          do j=1,nbas(i)
            if(good(i,j)) then
              nb(i,1) = nb(i,1) + 1
              sb(i,1) = sb(i,1) + bas(i,j)
              qb(i,1) = qb(i,1) + bas(i,j)**2
              cb(i,1) = cb(i,1) + bas(i,j)**3
              fb(i,1) = fb(i,1) + bas(i,j)**4
            endif
          enddo
          if(nb(i,1).lt.1) then
            meanb(i,1) = 0.
            stdb(i,1) = 0.
            skewb(i,1) = 0.
            kurtb(i,1) = 0.
          else
            sum1 = sb(i,1)/nb(i,1)
            sum2 = qb(i,1)/nb(i,1)
            sum3 = cb(i,1)/nb(i,1)
            sum4 = fb(i,1)/nb(i,1)
            meanb(i,1) = sum1
            arg = sum2 - sum1**2
            if(arg.gt.0.) then
              stdb(i,1) = sqrt(arg)
            else
              stdb(i,1) = 0.
            endif
            x3 = sum3 - 3.*sum2*sum1 + 2.*sum1**3
            if(sum2.gt.0.) then
              skewb(i,1) = x3/(sum2)**1.5
            else
              skewb(i,1) = 0.
            endif
            x4 = sum4 - 4.*sum3*sum1 + 6.*sum2*sum1**2 - 3.*sum1**4
            if(stdb(i,1).gt.0.) then
              kurtb(i,1) = (x4/stdb(i,1)**4) - 3.0
            else
              kurtb(i,1) = 0.
            endif
          endif
        endif

C  Compute other statistics

        do it=1,4
          do iv=1,7
            do l=1,21
              if(n(i,l,iv,it).lt.1) then
                mean(i,l,iv,it) = 0.
                std(i,l,iv,it)  = 0.
                skew(i,l,iv,it) = 0.
                kurt(i,l,iv,it) = 0.
                goto 220
              else
                sum1 = s(i,l,iv,it)/n(i,l,iv,it)
                sum2 = q(i,l,iv,it)/n(i,l,iv,it)
                sum3 = c(i,l,iv,it)/n(i,l,iv,it)
                sum4 = f(i,l,iv,it)/n(i,l,iv,it)
              
                mean(i,l,iv,it) = sum1
                arg = sum2 - sum1**2
                if(arg.gt.0.) then
                  std(i,l,iv,it) = sqrt(arg)
                else
                  std(i,l,iv,it) = 0.
                endif
                x3 = sum3 - 3.*sum2*sum1 + 2.*sum1**3
                if(sum2.gt.0.) then
                  skew(i,l,iv,it) = x3/(sum2)**1.5
                else
                  skew(i,l,iv,it) = 0.
                endif
                x4 = sum4 - 4.*sum3*sum1 + 6.*sum2*sum1**2 - 3.*sum1**4
                if(std(i,l,iv,it).gt.0.) then
                  kurt(i,l,iv,it) = (x4/std(i,l,iv,it)**4) - 3.0
                else
                  kurt(i,l,iv,it) = 0.
                endif
              endif
  220         continue
            enddo
          enddo
C  Compute precipitable water (mm)

          pw(i,it) = 0.
          do l=1,20
            q1 = mean(i,l,3,it)
            q2 = mean(i,l+1,3,it)
            pw(i,it) = pw(i,it) + (.0000005)*g*(q1+q2)*(p(l)-p(l+1))
          enddo
        enddo
      enddo

C  Print dates.

      do i=1,nd
      enddo
  504 format(' Number of dates: ',i4,/,' Dates:')
 
C  Print statistics.

      do it=1,4
        do i=1,ns
          call dict(cid(i),stname,xlat,xlon,elv)
          nsumall = 0
          do iv=1,3
            do l=1,21
              nsumall = nsumall + n(i,l,iv,it)
            enddo
          enddo
          if(nsumall.gt.0) then
            do l=1,21
              nsum = n(i,l,1,it) + n(i,l,2,it) + n(i,l,3,it)
              if(nsum.ne.0) then
              endif
            enddo

C  Write grads data

            stnout(1:7) = cid(i)(1:7)
            stnout(8:8) = '\0'
            tim = 0.0
            nlev = 21
            nflag = 0
            do l=1,21
              do iv=1,6
                xn(l,iv) = n(i,l,iv,it)
              enddo
            enddo
            write(61) stnout,xlat,xlon,tim,nlev,nflag
            write(61) (p(l), xn(l,1), mean(i,l,1,it), std(i,l,1,it),
     &        xn(l,2), mean(i,l,2,it), std(i,l,2,it),l=1,21)
          endif
        enddo
      enddo

C  Print dates (again)

  505 format(5x,a10)
      write(60,504) nd
      do i=1,nd
        write(60,505) date(i)
      enddo

C  Print special statistics for baseline errors.

      do i=1,ns
        if(abs(meanb(i,1)).ge.10. .or. abs(meanb(i,3)).ge.10. .or.
     &     abs(meanb(i,2)).ge.3.) then
          call dict(cid(i),stname,xlat,xlon,elv)
          write(60,603) cid(i), xlat, xlon, elv, stname
            write(60,613) nb(i,1),meanb(i,1),stdb(i,1)
            if(i.ne.1) write(60,604) (bas(i,j),good(i,j),j=1,nbas(i))
            write(60,616) nb(i,3),meanb(i,3),stdb(i,3)
            if(i.ne.1) write(60,607) (zi(i,j),j=1,nbas(i))
            write(60,615) nb(i,2),meanb(i,2),stdb(i,2)
            if(i.ne.1) write(60,606) (pi(i,j),j=1,nbas(i))
        endif
      enddo

C  Write final header

      stnout(1:7) = stnmiss
      stnout(8:8) = '\0'
      write(61) stnout,0.,0.,0.,0,0

  501 format(/,1x,'Stn.:',a8,2x,i2,' UTC  Lat.:',f7.2,'  Lon.:',f8.2,2x,
     &       'Ht.:',f6.0,2x,a39)
  502 format(1x,f8.0,'|',7(i4,2f7.1,' |'),i4,2f7.1)
  602 format(1x,'Baseline','|',i4,2f7.1,1x,'|  Surf Press Incr: |',
     &       i4,2f7.1,1x,'|    Sfc Ht Incr:   |',i4,2f7.1,1x,
     &       '|   PW Incr (mm):   |',f8.2,
     &       /,' pressure|       height      |',
     &       '    temperature    |    sp humidity    |',
     &       '    u-component    |    v-component    |',
     &       '      speed        |     direction     |',
     &       /,'-------------------------------',
     &       '------------------------------------------',
     &       '------------------------------------------',
     &       '-----------------------------------')
  603 format('-------------------------------------------------',
     &       '-------------------------------------------------',
     &       /,' Stn.:',a8,'  Lat.:',f7.2,'  Lon.:',f8.2,2x,
     &  'Ht.:',f6.0,2x,a39)
  613 format(' Baseline residual count, mean, std dev: (',i4,2f7.1,')')
  604 format(10(f7.0,l2))

  615 format(' Sfc press residual count, mean, std dev:(',i4,2f7.1,')')
  606 format(10(f7.0,2x))

  616 format(' Sfc ht residual count, mean, std dev:   (',i4,2f7.1,')')
  607 format(10(f7.0,2x))

      stop

  400 write(60,503)
  503 format(' error in reading input data')
      write(60,600) cdt,dhr,sid,nlv,
     &  (icat(i),i=1,5),((vinc(j,k),j=1,21),k=1,7),basres
  600 format(1x,a10,1x,f8.2,1x,a8,1x,i5,1x,a4,1x,5i5,
     &       10x,21f8.1,/,10x,21f8.1,/,10x,21f8.1,/,10x,21f8.1,
     &       /,10x,f8.0)

      end

      subroutine dict(cid,stn,xlat,xlon,elv)

C$$$  SUBROUTINE DOCUMENTATION BLOCK
C
C SUBPROGRAM: DICT
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

      common /stnd/ alat(3000), alon(3000), sid(3000), nstn, zs(3000)
      character*8 sid, cid, blank
      character*29 bigb
      character*39 stname(3000), stn
      character*120 filrec !buffer to test first character
      logical start
      data start /.true./, ndict /17/
      data blank /'        '/, bigb /'                             '/
      save start
      
      if(start) then
        i = 0
   10   i = i+1
        sid(i) = blank
        read(ndict,150,end=20) filrec
  150   format(A)
        if(.not. filrec(1:1) .eq.'!') then
         read(filrec,500,end=20) sid(i)(1:5),stname(i),lat,lon,izs
         alat(i) = .01 * lat
         alon(i) = .01 * lon
         zs(i)   = izs
        endif        
        goto 10
   20   continue
        start = .false.
        nstn = i
      endif
  500 format(10x,a5,a39,i6,i7,i6)

      xlat = 0.
      xlon = 0.
      stn  = bigb

      do i=1,nstn
        if(sid(i).eq.cid) then
          stn  = stname(i)
          xlat = alat(i)
          xlon = alon(i)
          elv  = zs(i)
          goto 30
        endif
      enddo
      write(6,501) cid
  501 format(' No station match to dictionary for ',a8)
   30 continue

      return
      end

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
C USAGE:    CALL SHELL(V,IV,MAX,IREV)
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
C   LANGUAGE: FORTRAN77
C   MACHINE:  P6
C
C$$$

      SUBROUTINE SHELL(V,IV,MAX,IREV)

      CHARACTER*8 V(*), VT, TEMP
      INTEGER IV(*)
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
C   LANGUAGE: FORTRAN77
C   MACHINE:  P6
C
C$$$

      SUBROUTINE SORT(RA,INDX,N)
 
C     SORT RA ACCORDING TO THE ORDER SPECIFIED BY THE
C     INDICES IN INDX.
 
      DIMENSION RA(*), WKSP(899)
      INTEGER INDX(*)
      DO J=1,N
         WKSP(J) = RA(J)
      ENDDO
      DO J=1,N
         RA(J) = WKSP(INDX(J))
      ENDDO
      RETURN
      END
