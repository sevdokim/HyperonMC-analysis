      SUBROUTINE calibr(Esum,Efma,result_ok)
C==============================================================
C     for LGD2 calibration
c     two gammas effective mass                              
C     created  27.02.91  CGA                                   
C                                                             
C     LAST CORRECTION  NLR                            
C                                 
C==============================================================
      COMMON /AMPW2 / ncellb,ncells,IWL2(576),IWLS(64)
      COMMON /LUN   / lunin,lunout,LUNST
      common /one23 / one(60)
      COMMON /SHOWR / NCLUST,nclws,NCELLC(60),XSHW(3,60),
     1                YSHW(3,60),ESHW(3,60),iamp_clus(60),DUM(60)
      common /GAMMAS/ NGAM, XSH(60), YSH(60), ESH(60),DCOS(3,60)
      common /MOMENT/ pi0mass, PLAB(3,60)
      common /KINCUT/ emin2,angmax,efmin,efmax
      common /PRFLAG/ NPREV,LPRINT
      common /CALIB / NSTEP,NSTMAX,ratio(576)
      COMMON /CLLIM / nclus,IXMN(60),IXMX(60),IYMN(60),IYMX(60)
      COMMON /W2GEOM/ XNET2(25),YNET2(25),HALFX2,HALFY2
      COMMON /WSGEOM/ XNETS(9),YNETS(9),HALFXS,HALFYS
      COMMON /LOSSD / LOSSD(70)
      COMMON /TARG  / XTARG,YTARG,ZTARG
      COMMON /ZWALLS/ ZWALL1,ZWALL
      real etot,qqq1,qqq2
      integer iamptot
      common /etotal/ etot,iamptot
      integer nt,icon
      COMMON /NTSTAT/ NT,icon
      common /nttype/ nttype ! ntuple type: big - nttype=1, small - nttype=0
      real effmass,ggangle
      integer numpi0
      logical result_ok
      common /twogmass/ numpi0,effmass(6),ggangle(6)
      common /pi0sel/ npisel,selmass(2),selang(2)
      common/eta0/etamass, etamin, etamax
      common /minihi/ matrix(10,10), b(10), kkk,istep,alfa1(100)

      DIMENSION IGAM(60),ICLU(60),ICL2(2)
      DIMENSION ANGMIN(6),EPI0(6)
cmike
      dimension icl(2)
cmike
      LOGICAL   OK,LPRINT,IFPI0
      real GlobalModifier                                               ! wtf ? ;kondr
C     DATA XLEFT/-331.7/, XRIGHT/95.3/, YDOWN/-215.7/, YUP/211.3/

c      write(*,*)'prishli v calibr'
c      stop
      GlobalModifier=1.1765                                             ! wtf ? ;kondr
      NGAM   = 0
      npisel = 0
      numpi0 = 0
      result_ok = .true.;
      call vzero(effmass,6)
      call vzero(ggangle,6)
      call vzero(selmass,2)
      call vzero(selang,2)
C ------------  Loop over clusters  ---------------------------
      delz = ZWALL - ZTARG 
      call hf1(20142,float(nclust),1.)
      icsm=0
      icbg=0
      DO 100 I = 1,NCLUST

C-        call new_egam(ESHW(1,I))
      IF (ESHW(1,I).LT.EMIN2.OR.ESHW(2,I).LT.-6.) GO TO 100
CSdv2-IF (ESHW(1,I).LT.EMIN2)                     GO TO 100
c
      IF (I.LE.NCLWS.AND.(IXMN(I).EQ.IXMX(I).OR.IYMN(I).EQ.IYMX(I)))
     *                                            GO TO 100
c        if (i.gt.nclws.and.ixmn(i).eq.1) go to 100
        NGAM = NGAM + 1
        one(ngam)=one(I)
c
c        if(csingle.eq.1.)then
c           do ii=1,64
c           iiy=1+(ii-1)/8
c           iix=ii-(iiy-1)*8
c           if(scl(i,ii).eq.1.) then
c              call hf2(5021,float(iix),float(iiy),1.)
c              write(*,*)iix,iiy
c              stop
c           endif
c           icsm=icsm+scl(i,ii)
c           write(*,*)scl(i,ii)
c           enddo
c           do ii=1,576
c           iiy=1+(ii-1)/24
c           iix=ii-(iiy-1)*24
c           if(bcl(i,ii).eq.1.) call hf2(5018,float(iix),float(iiy),1.)
c           icbg=icbg+bcl(i,ii)
c           write(*,*)bcl(i,ii)
c           enddo
c           write(*,*)icsm,icbg
c           stop
c        endif

        tangenx =(XSHW(1,I) - XTARG)/delz
        tangeny =(YSHW(1,I) - YTARG)/delz
        alfx = atan(tangenx)
        alfy = atan(tangeny)
c 
        DELX = 14.*TANH(21.6*ABS(ALFX) - .9) + 10.5
        DELY = 14.*TANH(21.6*ABS(ALFY) - .9) + 10.5
        IF (ALFX.LT.0.) DELX = -DELX
        IF (ALFY.LT.0.) DELY = -DELY

        call new_egam(ESHW(1,I))
CSdv+
C-      ESHW(1,I) = E_cor_pi0(ESHW(1,I)) 
CSdv-	
        ESH(NGAM) = ESHW(1,I)
        XSH(NGAM) = XSHW(1,I) - DELX
        YSH(NGAM) = YSHW(1,I) - DELY
        ICLU(NGAM)= I
CEvd    fill cluster coordinates histos
        call hf2(-30001,xsh(i),ysh(i),1.)
        call hf1(-30005,float(ixmx(i)-ixmn(i)+1),1.)
        call hf1(-30006,float(iymx(i)-iymn(i)+1),1.)
c        if(ixmx(i).ne.ixmn(i)) call hf1(-30003,xsh(i),1.)
c        if(iymx(i).ne.iymn(i)) call hf1(-30004,ysh(i),1.)
        if(ixmx(i).ne.ixmn(i).and.iymx(i).ne.iymn(i)) then 
           call hf2(-30002, xsh(i),ysh(i),1.)
           call hf1(-30003,xsh(i),1.)
           call hf1(-30004,ysh(i),1.)
        endif
100   CONTINUE
      if (nt.eq.0) CALL HF1(20140,FLOAT(NGAM),1.)
c
c      write(*,*)'end loop over clusters, ngam=',ngam
c      stop
c
C---- END OF BIG LOOP -------------------------------------
c
      if (ngam.eq.1) then 
          call hf1(30000,esh(1),1.)
CEvd+
          if(abs(xsh(1)).lt.250.and.abs(ysh(1)).lt.25.0) 
     *         call hf1(-30000,esh(1),1.)
c	  
      do k=1,576
          if(IWL2(k).gt.50.and.IWL2(k).lt.5000) 
     +                           call hf1(6000,float(k),1.)
          enddo
c-   
        do i=2,5
        qqq1=(i-1)*100
        if(esh(1).lt.qqq1) then
          call hf2(30000+i,XSH(1),YSH(1),1.)
        endif
        enddo
        call hf2(30006,XSH(1),YSH(1),1.)
      endif
C             --- The final gamma coordinates and momentums ---             
      CALL COSDIR
      DO 101 J = 1, NGAM
         XSH(J)  =  XSH(J)-XTARG
         YSH(J)  =  YSH(J)-YTARG
      DO 101 I = 1, 3
         PLAB(I,J)= ESH(J)*DCOS(I,J)
  101 CONTINUE
c
      IF (NGAM.LT.2) goto 999
c
c ---  A case with 2 gamma ---------------------------
c
      IF (NGAM.EQ.2) THEN
         IGAM(1) = 1
         IGAM(2) = 2
         CALL EFMASS(2,IGAM,EFMA,Esum,OK)
!          if (.not.ok) result_ok = .false.                               ! ;kondr
         IF (.NOT.OK) GOTO 999
c	 
         CALL ANGLE(1,2,COS2G,ANG2G)
         if (nt.eq.0) then
           CALL HF1(20230+NSTEP,EFMA,1.)
           CALL HF1(20240+NSTEP,ANG2G,1.)
           if(one(1).eq.1.or.one(2).eq.1) call hf1(20232,efma,1.)
           if(one(1).eq.2.or.one(2).eq.2) call hf1(20233,efma,1.)
           if(one(1).eq.3.or.one(2).eq.3) call hf1(20234,efma,1.)
         else
            numpi0 = 1
            effmass(1) = efma
            ggangle(1) = ang2g
         endif
         angmin(1) = 2.*pi0mass/(esh(1)+esh(2))*.7
c--------------calibrovka na eta-meson-----------------
c         if (efma.gt.etamin.and.efma.lt.etamax)then
c            icl2(1)=iclu(1)
c            icl2(2)=iclu(2)
c            call fill_eta(icl2,efma)
c         endif
c--------------calibrovka na pi0-----------------------
         IFPI0 = EFMA.GT.EFMIN.AND.EFMA.LT.EFMAX
c--------------zapolnenie matrici dlia popravki energii-----
         if(ifpi0)then
            call hf1(20312,esum,1.)
CSdv-          if(esum.ge.1000.) then             !  epl54.dat and less
               if(esum.ge.1500.) then 
               call matr_ent(esh,1)
               CALL HF1(20281,EFMA  ,1.)
               CALL HF1(20282,esh(1),1.)
               CALL HF1(20282,esh(2),1.)
            endif
         endif
c         if(((EFMA.gt.pi0mass-(EFMAX-EFMIN).and.EFMA.le.EFMIN).or.
c     &       (EFMA.lt.pi0mass+(EFMAX-EFMIN).and.EFMA.ge.EFMAX)).and.
c     &        esum.ge.1500.) then 
c            call hf1(20312,esum,1.)
c           CALL HF1(20281,EFMA  ,1.)
c           call matr_ent(esh,-1)
c         endif
c-----------------------------------------------------------
cmike
      ICL(1) = ICLU(1)
      ICL(2) = ICLU(2)
      if(esum.gt.1000.)      CALL NEW_CLB(ICL,EFMA)
cmike
         IFPI0 = ifpi0.and.ANG2G.GT.angmin(1).AND.ANG2G.LT.ANGMAX
         IF (.NOT.IFPI0) LOSSD(39) = LOSSD(39) + 1
         IF (.NOT.IFPI0) GOTO 999
            ICL2(1) = ICLU(1)
            ICL2(2) = ICLU(2)
c            call hf1(20312,esum,1.)
c            if(esum.ge.1000.) then
c               call matr_ent(esh)
c               CALL HF1(20281,EFMA,1.)
c            endif
            CALL FILLING(ICL2,EFMA)
            if (nt.eq.0) then
              CALL HF1(20280+NSTEP,EFMA,1.)
            else
              npisel = 1
              selmass(1) = efma
              selang(1)  = ang2g
              if (nttype.eq.0) call hfnt(333)
            endif
      ENDIF
c
c --- A case with 3 gamma ---------------------------
c
      IF (ngam.eq.3) THEN
        do i=1,3
           igam(i)=i
        enddo
        CALL EFMASS(3,IGAM,EFMA,Esum,OK)
        IF (.NOT.OK) GO TO 999
        call hf1(20231,efma,1.)
      ENDIF
c
c --- A case with 4+ gamma --------------------------
c
      if(ngam.gt.3)then
        do i=1,ngam
           igam(i)=i
        enddo
        call efmass(ngam,igam,efma,Esum,ok)
        if(.not.ok) go to 999
      endif
C
  999 if (nttype.eq.1) call hfnt(333)
      if (.not.ok) result_ok = .false.;
c      write(*,*)'calibr done'
cmike
      do i = 1,64
        if(IWLS(i).ne.0) call hf1(-20000-i,   real(IWLS(i)),1.)
      enddo
      do i = 1,576
        if(IWL2(i).ne.0) call hf1(-20000-i-64,real(IWL2(i)),1.)
      enddo
cmike
      RETURN
      END

