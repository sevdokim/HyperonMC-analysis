C
      BLOCK DATA CONST
      COMMON/REJECT/ LINC,KLED,ETLT,ETGT,AMMN,XMMN,XMMX,CHEV,CHEM,CHIE
      COMMON/CONSTS/ BEAM,ANOR,ANORT,DELTA,CELL,DSST,NCNX,NCNY,STCO
      COMMON/CONST2/ MVTD,MVLD,CHLD,DLCH,EMIN,CHQ1,CHQ2,HSTI
      COMMON/GEOMTR/ ZGMS,ZHOD(4),XCHD(4),YCHD(4),XBEM,YBEM,ZTRG,ZFIX
      COMMON/LENGTH/ NSCL,NSC2,IREC,MFIT,ELFT,EGFT,LLPD,LLED,LLEV
      COMMON/ERRORS/ IFIT,DHD0,DHD1,DHD2,DCS0,DCS1,DCS2,DSBM,DSXY,C,D,B
      COMMON/DISPER/ MDTH,DS1S,DSPL,DSPG
      COMMON/RTHRES/ NRGM,ERLS,ERGM(9),MXGM,EMGM,ENGM(7)
      COMMON/EVTYPE/ MC14,MRGM,MLOC,LHOD,MDVT,ENGMT,RGMN,MSM8,ROP,AMP(3)
      COMMON/TARGMS/ IBTG,AM0,SL,R0,T0,ERNT,DSZZ
      COMMON/TYPEVN/ NHYP,MTYP,MYEV,MXHD,MYHD
      COMMON/MEMBS4/ NBCM,MHYP(6,7)
C
C-    GMV(1) = DSBM
C-    GMV(2) = DCS0
C-    GMV(3) = DCS0
C-    GMV(4) = DHD0
C-    GMV(5) = DHD0
C-    GMV(6) = DSZZ*.25 
C
      DATA CELL,ANOR,XBEM,YBEM,DELTA /38.40, 0.001,   0.0,   0.0,  0.0 /
      DATA BEAM,ETLT,ETGT,ELFT,EGFT  / 7.000, 28.5,  44.2, 28.50, 44.2 /!BEAM not used
      DATA NCNX,NCNY,     KLED,IFIT  /    24,   24,           0 ,   16 /
      DATA XMMN,XMMX,AMMN            /  .000,3.800, 22.75              /
      DATA CHEV, C  , D  , B  ,DSBM,DSXY/25.,.0150, .112 , 0.00 ,
     &                                                     0.106,  6.80/
      DATA AM0 ,SL  ,R0  ,T0  ,DSZZ  /  529.,  24.6, 1.35,  190.,  100./
      DATA DCS0,DCS1,DCS2            /3.0E-7,7.3E-8, 1.3E-8            /
      DATA DHD0,DHD1,DHD2            /  25.5,   6.3, 1.06              /
C
      DATA ZHOD,ZFIX,ZTRG / -1030. ,-10880., -1280., -10580., 4341., 0./
C ----------------------------------------------------------------------
      END
C
      SUBROUTINE Cfit34(Esum,Efma)
C     
C    Commons for the Interface with the Hyperon program
C      
      COMMON /ZWALLS/ ZWALL1,ZWALL
      COMMON /GAMMAS/ NGMM, XGAM(60), YGAM(60), EGAM(60), DCOS(3,60)
      COMMON /TARG  / XTARG,YTARG,ZTARG
      DATA    X0,Ec /30., 21./     ! X0 - rad dlina v mm, Ec - v MeV
C.....................................................................      
C
      COMMON /GAMMAG/ NGAM,PBM(5),E(25),X(25),Y(25),CHI(25)
      COMMON /GEOMTR/ ZGMS,ZHOD(4),XCHD(4),YCHD(4),XBEM,YBEM,ZTRG,ZFIX
      COMMON /LENGTH/ NSCL,NSC2,IRIC,MFIT,ELFT,EGFT,LLPD,LLED,LLEV
      COMMON /KINDAT/ XMAS,TEP,AMMS
      COMMON /REJECT/ LINC,KLED,ETLT,ETGT,AMMN,XMMN,XMMX,CHEV,CHEM,CHIE
      COMMON /CONSTS/ BEAM,ANOR,ANORT,DELTA,CELL,DSST,NCNX,NCNY,STCO
      COMMON /TYPEVN/ NHYP,MTYP,MYEV,MXHD,MYHD
      REAL*8 PSM(5),PTR(5),PM(5,25)
      REAL*8 DOT4
      LOGICAL*1 LBEG
      REAL NMAS
      DATA PIMS,PMAS,NMAS / 0.1395673, 0.938272081, 0.939565413 /
      DATA LBEG / .FALSE. /
C
      IF ( LBEG ) GO TO 100
      PBM(1)=.0
      PBM(2)=.0
      PBM(3)= BEAM
      PBM(4)= SQRT(BEAM**2+PIMS*PIMS)
      PBM(5)= PIMS
      LBEG  =.TRUE.
C
C    Hyperon to GAMS Interface
C
  100 IF (NGMM.LE.1) RETURN
C  
      NGAM = NGMM
      ZGMS  = ZWALL - ZTARG
C
      DO 110 Ig = 1, NGAM
      E(Ig) = EGAM(Ig)  *ANOR 
      X(Ig) = DCOS(1,Ig)*ZGMS/DCOS(3,Ig)
      Y(Ig) = DCOS(2,Ig)*ZGMS/DCOS(3,Ig)
  110 CONTINUE
      
c------------------------------
c
c    GAMS kinematic hystograms
c            
      MTYP = 0
      RGAM = NGAM
C-    CALL HF1( 51,RGAM,1.)
C
C --- BEAM HODOSCOPS ---
C     CALL BEAMHOD(MXHD,MYHD,IHOD)
C     CALL HF1(151,XBEM,1.)
C     CALL HF1(152,YBEM,1.)
C
C     CALL TWOONE(ITH)
C     IF ( ITH .NE.0 )   THEN
C       		 MTYP=6
C       		 GO TO 900
C       		 ENDIF
C     CALL KILLGAM
C     CALL HF1( 52,FLOAT(NGAM),1.)
C     IF ( NGAM.LT.2 )   THEN
C                        MTYP=7
C                        GO TO 900
C                        ENDIF
C---------------------------------
C
      PSM(1) = 0.
      PSM(2) = 0.
      PSM(3) = 0.
      PSM(4) = 0.
      DO 350 N= 1,NGAM
      RON     = E(N)/SQRT(X(N)**2 + Y(N)**2 + ZGMS**2)
      PM(1,N) = X(N)*RON
      PM(2,N) = Y(N)*RON
      PM(3,N) = ZGMS*RON
      PM(4,N) = E(N)
C
      DO 320 I= 1,4
  320 PSM(I)  = PSM(I) + PM(I,N)
  350 CONTINUE
      EGMT    = PSM(4)
      XMAS    = SQRT(DABS(DOT4(PSM)))
C
C-    write(*,*) 'NGAM,EGMT,XMAS=',NGAM,EGMT,XMAS,(PBM(k),k=1,4)
C
      DO 360 I= 1,4
  360 PTR(I)  = PBM(I) - PSM(I)
      TEP     = DOT4(PTR(1))
      PTR(4)  = PTR(4) + NMAS
      AMMS    = SQRT(DABS(DOT4(PTR(1))))
C
C-    PP1  = SQRT(PBM(1)**2 + PBM(2)**2 + PBM(3)**2)
C-    PP3  = SQRT(PSM(1)**2 + PSM(2)**2 + PSM(3)**2)
C-    TEP  =-2.*(PP1*PP3 -PP1*PSM(3))
C
      EKIN =(PMAS*PMAS+NMAS*NMAS-TEP)/(2.*NMAS) - PMAS
      ECOR = EGMT + EKIN
C
C-    write(*,*) 'EGMT,TEP,EKIN,ECOR=',EGMT,TEP,EKIN,ECOR
C      
C                     These histograms ID are already used ...
      IHH  = 100*NGAM
      IF (Esum.gt.1000.) THEN
          CALL HF1(IHH+1, Esum, 1.)
          CALL HF1(IHH+2, 1000.*EGMT, 1.)
          CALL HF1(IHH+3, 1000.*EKIN, 1.)
	  CALL HF1(IHH+4, 1000.*ECOR, 1.)
          CALL HF1(IHH+5, Efma,       1.)     !  It is wrong, don' know why ...
          CALL HF1(IHH+6, 1000.*XMAS, 1.)
          CALL HF1(IHH+7,-TEP       , 1.)
	  CALL HF1(IHH+8, 1000.*AMMS, 1.) 
      ENDIF  
C
C-   write(*,*) 'Cfit: NGAM,Esum,EGMT,Efma,XMAS='
C-  +                 ,NGAM,Esum,EGMT,Efma,XMAS
       IF ( NGAM.ge.3.and.NGAM.le.4 ) THEN
C-     IF ( NGAM.eq.3)    THEN
C
C --- Kinematical Fit of 3g & 4g events ---
C      
      IDMR = 1000*NGAM
      CALL KINFIT(NGAM,XMAS,IDMR,CHIH) 
      IF ( CHIH.GT.0.) GO TO 800
      CALL HF1(IDMR+50,XMAS, 1.)      
      END IF                            
C
  800 CONTINUE
      RETURN
C
C-  900 CONTINUE
      CALL HF1( 21, FLOAT(MTYP), 1.)
      RETURN
      END
C
      SUBROUTINE PI0FIT(YM,NMES,XMAS,CHHH,IDMR)
C
C THIS SUB. SUPPOSED TO BE USED FOR FIT 4 or 3 GAMMA EVENTS HYPERON EXPERIMENT
C
      COMMON /GAMMAS/ NGMM, XGAM(60), YGAM(60), EGAM(60), DCOS(3,60) ! used for gamma-4moment calculating
      COMMON/REJECT/ LINC,KLED,ETLT,ETGT,AMMN,XMMN,XMMX,CHEV,CHEM,CHIE
C
      DIMENSION YM(24), YMCY(24),GMVCY(24),GMCY(24),CPAR(12),FPAR(72)
     &      ,YCopy(24,3,3,6),GCopy(24,3,3,6)                         ! YCopy, GCopy - cmike
      COMMON/ERRORG/ GM(24),GMV(24)
      REAL*8 YM,GM,GMV,YMCY,GMCY,GMVCY,FPAR,CPAR
      REAL*8 YCopy,GCopy,InterractionCoordinate,th ! cmike
C
      COMMON/TYPEVN/NHYP,MTYP,MYEV,MXHD,MYHD
      COMMON/FISICI/ISPILL,NEV,IEV,NGAM,NHIP,IFIT
      COMMON/FISICD/F(6),B(24,6),DXRD(6),DYRD(6),
     +       YC(24),CT(6),CT2(15),CHIQ,EB,ET,ECT,S,T,EFMQ,
     +       EFM12Q,EFM34Q,EFM56Q,EFM1234Q,RNCMB,EMQPR(15)
      real*8 csmesh,esmesh,msmesh,di1,di2,rij
      REAL*8   F,B,DXRD,DYRD,YC,CT,CT2,CHIQ,EB,ET,ECT,S,T
      REAL*8   EFMQ,EFM12Q,EFM34Q,EFM56Q,EFM1234Q,RNCMB,EMQPR
      REAL*8   ETYM,YCeq(72)
      REAL*8   MMQ
      REAL     MMS
      EQUIVALENCE (YC(1),YCeq(1)) 
C
      COMMON/FISMAS/ QNMS,QLMS,NEUMQ,ELEN,ELNQ,NEUMS,PRM,PRMQ
      REAL           QNMS,QLMS,NEUMQ,ELEN,ELNQ,NEUMS,PRM,PRMQ,mim
      DATA           QLMS,NHYP  / 1.244748556, 3 / ! NHYP=1 - only pi-pi. NHYP=2 - pi-pi and pi-eta NHYP=3 + pi0gg
C!!-  DATA NEUMS,QNMS / 0.939565413, 0.882783165/
      DATA NEUMS,NEUMQ / 0.939565413, 0.882783165/
      DATA PRM  ,PRMQ / 0.938272081, 0.880354498/
C
      COMMON/EVTFIT/ LBFT,LWAL,LWRT,LNFIT,IBFT(76)
      INTEGER * 2    IBFT
      INTEGER * 4    I4FT(32)
      EQUIVALENCE  ( I4FT,IBFT(13))
C
      COMMON/MEMBH2/ PMCH2(6,7)
      COMMON/MEMBS4/ NBCM,MHYP(6,7)
      COMMON/COMBN4/ NCMB,NHYF,MCMH(6)
      BYTE           K1,K2,K3,K4,MCMC(4),MCMB(4,6)
      EQUIVALENCE   (K1,MCMC(1)),(K2,MCMC(2)),(K3,MCMC(3)),(K4,MCMC(4)),
     +              (MCMH,MCMB ),(MCMC,MCI4)
      DATA NBCM,MCMB  /  6,
     1   1,2,3,4,  1,3,2,4,  2,3,1,4,  3,4,1,2,  2,4,1,3,  1,4,2,3 /
C
      INTEGER*2 NFHP(16),KFTF(16),KHIP(16),KBCM(16)
      integer*2 kcheren                                                 ! kondr
      real probhyp(3)                                                   ! kondr
      real*8 HypProbCriteria3g(2), HypProbCriteria4g(3)                 ! kondr
      real*8 GoodProbCriteria3g(2), GoodProbCriteria4g(3)               ! kondr
      DATA KFTF/ 1, 2,  3, 4,  5, 6,  7, 9, 10,12, 13,14,  8,11, 15,16/
      DATA KHIP/ 1, 2,  3, 4,  5, 6,  7, 8,  9, 9, 11,12, 13,14, 15,16/
      DATA KBCM/ 3, 6,  6, 3,  6, 4,  4, 5,  4, 4,  5, 4,  4, 4,  4, 4/ ! All these for 4-gamma
      DATA NFHP/ 2, 2,  1, 3,  3, 2,  2, 3,  5, 4,  4, 5,  5, 5,  5, 5/ !           for 3-gamma will be exeption
C 
      data HypProbCriteria3g,HypProbCriteria4g                          ! bad probability cuts "less then this" ;kondr
     &              /0.010, 0.060,   0.005, 0.005, 0.005/               ! kondr
      data GoodProbCriteria3g,GoodProbCriteria4g                        ! good probability cuts "more then this" ;kondr 
     &              /0.150, 0.150,   0.150, 0.150, 0.150/               ! kondr
      data probhyp  /-1., -1., -1./                                     ! kondr
      integer mfit_kondr                                                ! temporary container ; kodnr
      integer hi_number_2d                                              ! kondr
      common /pt/ ptcut(10) ! look for ptcut values in BOOK_FIT subroutine
      common/ptransverce/ ptrans
      logical cut_before_fit, second_comb
      real plab(4,4)
      real plab_kondr(4,4)                      ! for my cuts ;kondr
      real*8 dxrd_kondr(6), dyrd_kondr(6)       ! for my cuts ;kondr
      logical*1 cherenki_only                   ! kondr
      LOGICAL LK0S
      common /triggers/c1c2c3(3),c1min,c2min,c3min
c     data c1min,c2min/ 200, 130 /     ! c
      real*8 sstore(18) 
      integer HypModulator ! eq 1 if first hyp, E6 if second
cmike+
      common /TTHICKNESS/ THICKNESS
cmike-
      COMMON /MOMENT/ pi0mass,PL(3,60) ! < need for t calculating
c
c      data c1min,c2min/ 280, 150 /    ! be
c      data c1min,c2min/ 255, 170 /    ! cu3
c      data c1min,c2min/ 250, 150 /    ! cu6
CEvd+
c     event mixing
C     pi0gamma
      integer    imix,ipi0gamma
      parameter (imix=20)
      data ipi0gamma/0/
      real*8 Ppi0pg(4,4,imix),Pgampg(4,4,imix)  ! first index --> number of hypothesa which were considered
      save ipi0gamma,Ppi0pg,Pgampg              ! 1 --> only pi0gamma, 2 --> pi0gamma and etagamma
C     etagamma
      integer ietagamma
      data ietagamma/0/
      real*8 Petaeg(4,4,imix),Pgameg(4,4,imix)!second index --> x,y,z,t
      save ietagamma,Petaeg,Pgameg
C     pi0pi0
      integer ipi0pi0
      data ipi0pi0/0/
      real*8 Ppi01pp(4,4,imix),Ppi02pp(4,4,imix) ! third index --> number of events to use for mixing
      save ipi0pi0,Ppi01pp,Ppi02pp
C     etapi0
      integer ietapi0
      data ietapi0/0/
      real*8 Petaep(4,4,imix),Ppi0ep(4,4,imix)
      save ietapi0,Petaep,Ppi0ep
CEvd-
!kondr++{
      common /kondratyuk/Petaeg1(4,4,imix),Pgameg1(4,4,imix),ietagamma1, ! piece of dark magic ; I had problems with ietagamma1 resetting ; don't touch
     +  ipi0gg1,  Ppi0pgg1(3,4,imix),Pgampgg1(3,4,imix),
     +  ipi0pi01_ch, Ppi01pp1_ch(3,4,imix),Ppi02pp1_ch(3,4,imix)
C
      real*8 ppi0pg1(4,4,imix),pgampg1(4,4,imix)
C      
      integer ipi0gg1
      data    ipi0gg1 /0/
      real*8 Ppi0pgg1,Pgampgg1
C     pi0gamma_ch ; my probability cut for cherenki
      integer ipi0gamma_ch
      data    ipi0gamma_ch/0/
      real*8 Ppi0pg_ch(4,4,imix),Pgampg_ch(4,4,imix)  !first index --> number of hypothesa which were considered
      save ipi0gamma_ch, Ppi0pg_ch,Pgampg_ch           
C     etagamma1 ; my probability cut
      integer ietagamma1
      data    ietagamma1/0/
      real*8 Petaeg1,Pgameg1
C     pi0pi01_ch ; my prbability cut with cherenki
      integer ipi0pi01_ch
      data    ipi0pi01_ch/0/
      real*8 Ppi01pp1_ch, Ppi02pp1_ch
!kondr--}
C -----------------------------------------------------------------------
      NHYP = 3        ! there is some kind of a bug... nhyp didn't change its value, so we change it here; kondr
      probhyp(1)=-1;  probhyp(2)=-1; probhyp(3)=-1;   ! i hate fortran for this...i used data to put values in, so wtf ? ; kondr
      NGAM = NMES
      if (NGAM.lt.3.or.NGAM.gt.4) return
C
Cmike-Pt cuting before fit
C
      cut_before_fit = .true.          ! if 'false', then Pt cuts after fiting
      if(.not.cut_before_fit) goto 700
C      
C-    call hf1(400000+1000*NGAM,ptrans/1000000.,1.)
C-    do i = 1,9
C-       if (ptrans.gt.ptcut(i)*1000000.) icut = i	 ! transverce momentum cuting
C-    enddo
 700  continue
C 
CSdv+      
      IEV  = IEV + 1
C-    write(*,*) 'IEV,NGAM,ptrans=',IEV,NGAM,ptrans/1000000.
C      
      MFIT = 0
      NYFT = 6 + 3*NGAM
      do j = 1,NYFT
       YMCY (j)=YM (j)
       GMVCY(j)=GMV(j)
       GMCY (j)=GM (j)
      enddo
      EFMQCY = EFMQ
C
C ------------------------------------------------
C     
      NHYF = NHYP                     ! 3 hypothesis for 4-gamma events
      if(ngam.eq.3)NHYF = 2           ! 2 hypothesis for 3-gamma events 
C
      DO 800 KHP=1,NHYF               ! NHYF is the number of hypothesis to be considered
      EFMQ = EFMQCY
      IHP  = KFTF(KHP)                ! KFTF contains the order of hypothesa: 1,2,3
      IF    (IHP.LE.0) GO TO 800      ! useless ; kondr
      NF   = NFHP(IHP)                ! NumberOfFreedomDegrees
      NHIP = KHIP(IHP)                ! nhip = mfit = khp = khip(ihp) = khip(kftf(khp)) --- Number of current hypothesis 
      if (NGAM.eq.3)   NF=1           ! Number Of Freedom Degrees = 1 for pi0+g and eta+g
C
      JFIT  =-1                       ! 
      MBCOMB= 0                       ! 
      MBCMB2= 0                       ! 
      CHIP  = 1.E8                    ! 
      XMHP  =-1.                      ! 
C
      DO 499 nhypot = 1,1             ! this cycle was actually removed...
CSdv- DO 499 nhypot = 1,3             ! cyle over the hypothesis with different interaction coordinates:
C                                     !      1 -  target, 2 - cameras, 3 - S4   -- by Mike
C
      KBCMB = KBCM(IHP)               ! number of combination for 4-gamma hypothesis 
      IF (NGAM.eq.3) KBCMB = 3        ! = 3 for 3gamma hypotheses
      DO 498 ICOMB=1,KBCMB            ! combination cycle
C
      MCI4 = MCMH(ICOMB)              ! something from the dark and ancient fortran arts ; seems peaseful and useless, but it's better to never touch it
C
      do  j=1,NYFT
        YM (j)=YMCY (j)
        GMV(j)=GMVCY(j)
        GM (j)=GMCY (j)
      enddo 
Cmike                                 !!! hypothesis with interaction on S4 and on wire cameras!!!!
      if(nhypot.eq.1) then
        th     = THICKNESS
        YM (6) = YMCY(6)
      endif
      if(nhypot.eq.2) then
        th     = 640.
        YM (6) = 3805.+230.+640./2. ! center of wire cameras block position
      endif
      if(nhypot.eq.3) then
        th     = 5.
        YM (6) = 3805.              ! 3805mm - S4 position
      endif
C
      GMV(6) = th**2/12.
      GM (6) = 1./GMV(6)
C
C-    if(IHP.eq.3) then                              !     
C-       InterractionCoordinate = YM(6)              !   This was at first for Ks hypothesis
C-       YM (6) = YM(6)*SQRT(ABS(EFMQ))/0.497672     !   now the IHP = 3 is for pi0gg hypothesis and 
C-       GMV(6) = 10.**2                             !   Ks hypothesis is not concidered here
C-       GM (6) = 1./GMV(6)                          !
C-    endif
C-    write(*,*) 'pi0fit: NGAM,IHP,NF,NHIP,NYFT,ICOMB,YM(6)=',
C-   +  	   NGAM,IHP,NF,NHIP,NYFT,ICOMB,YM(6),sqrt(GMV(6))
C-    do i=1,NGAM
C-    write(*,*) i,YMCY(3+3*I+1),YMCY(3+3*I+2),YMCY(3+3*I+3)*1000.
C-    enddo
C
      ETYM = 0.
      DO  50 I=1,NGAM
        J = MCMC(I)
        I4= 3 + 3*I
        J4= 3 + 3*J
        do  k=1,3
            YM (I4+k)=YMCY (J4+k)
            GM (I4+k)=GMCY (J4+k)
            GMV(I4+k)=GMVCY(J4+k)
        enddo 
C
   50 ETYM = ETYM + YM(I4+3)
C
      do j = 1,NYFT
        YC   (j)     = YM (j)
        YCopy(j,IHP,nhypot,ICOMB) = YM (j)        ! cmike
        GCopy(j,IHP,nhypot,ICOMB) = GMV(j)        ! cmike
      enddo
C
      CALL FITPR(YM,GM,GMV,NF,NYFT)
C
      IF ( JFIT.GE.0 .AND. CHIQ.GT.CHI0) GO TO 105    ! At first JFIT  -1
      CHI0   = CHIQ                                   ! 
      JFIT   = IFIT                                   ! 
C
  105 IF ( IFIT.NE.0) GO TO 498   ! ifit = ID of error or ID of return from FITPR -> (ifit .equiv. ner) ; ! ifit = 0  - everethyng 
                                  ! is all right  ! ifit != 0 - something bad happend in fitpr ; 
				  ! this goto continues the combination cycle ; k

      IF ( MBCOMB.NE.0.AND.CHIQ.GT.CHIP) GO TO 110                      

      CHP2   = CHIP                                                     
      XMP2   = XMHP                                                     
      MBCMB2 = MBCOMB                                                   

      CHIP   = CHIQ                                                     
      XMHP   = SQRT(ABS(EFMQ))                                          
      MBCOMB = ICOMB                                                    
      GO TO 112

  110 IF ( MBCMB2.NE.0.AND.CHIQ.GT.CHP2) GO TO 112                      
      CHP2   = CHIQ                                                    
      XMP2   = SQRT(ABS(EFMQ))                                         
      MBCMB2 = ICOMB 
C    
      
      PMCH2(1,KHP) = CHP2
      PMCH2(2,KHP) = XMP2
      PMCH2(3,KHP) = MBCMB2                                                   
C 
  112 IF ( MFIT.NE.0.AND.CHIQ.GE.FPAR(46)) GO TO 498
C      
      CHIR = CHIQ  
      PCHI = PROB(CHIR,NF)
      IF (probhyp(nhip).lt.PCHI) probhyp(nhip) = PCHI   

C-      IF(NGAM.eq.4) write(*,*)'NHIP=',NHIP,probhyp(1),probhyp(2),CHIR,NF

      IF(NHIP.ne.3.or.
     +  (NHIP.eq.3.and.probhyp(1).lt.0.01.and.probhyp(2).lt.0.01)) THEN
C
         MNEV	= NEV
         MFIT	= NHIP   ! here we save the number of current hypothesis as the best one for now
         RNCMB  = ICOMB
         RKOMB  = ICOMB
         NBhypot= nhypot ! cmike NBhypot - Number of Best Z-Hypotesa from 3
C
         do   j=1,6
         CPAR(j)  = DXRD(j)
         CPAR(j+6)= DYRD(j)
         enddo
C      
         do   j=1,72
         FPAR(j)= YCeq(j)
         enddo
         ZCoord = YCeq(6)
         EffectiveMass = real(sqrt(abs(efmq))*1000.)	 
      ENDIF

  498 CONTINUE        ! End of the combination cycle
C
      CALL HF1( 999, FLOAT(50*(NGAM-3)+10*MFIT+10+MBCOMB), 1.)
      CALL HF1(1000, FLOAT(30*(NGAM-3)+10*MFIT+10)+RKOMB,1.)
C  
  499 CONTINUE        ! End of the Z-hypothesis cycle
C
      IF ( MBCOMB.EQ.0) GO TO 800 
C-    IF (ngam.eq.4.and.khp.eq.3) goto 800  ! we don't want 3rd hypothesis with SA cuts ; kondr
C
  500 CONTINUE
      LNFIT = 12                    ! magic number ! what is this ? ;k
      IF  (MFIT.EQ.0) GO TO 800
C
      CHHH = CHIQ
      IDMR = 1000*NGAM
      IHH  = 100000*KHP+IDMR+100*MFIT+20
C    ------------------------------------
      do  j=1,6
        DXRD(j)=CPAR(j)
        DYRD(j)=CPAR(j+6)
      enddo
C      
      do  j=1,72
        YCeq(j)=FPAR(j)
      enddo
C      
      EFM = SQRT(ABS(EFMQ))
      CHIR= CHIQ              ! no need to use this variable, it's so temporary...used in 2 lines below...!! must be replaced with real(chiq) ; k
      PCHI= PROB(CHIR, NF )
C
      IHM  = 100000 + 1000*NGAM + 100*MFIT + 20
C      
      if ( CHP2.lt.100.) then
                         PCH2=PROB(CHP2,NF)
                         CALL HF1(IHM-5,CHP2,1.0)
		         CALL HF1(IHM-4,PCH2,1.0)
			 CALL HF1(IHM-3,XMP2,1.0)
                if (PCH2.gt.0.06) then
	        CALL HF1(IHM-2,EFM,1.0)
		GO TO 800
		endif
      endif 
C
      CALL HF1(IHM-1, PCHI, 1.)
      CALL HF1(IHM+3, CHIR, 1.)
C
      IF ( PCHI.LT.0.06) THEN
                         CALL HF1(IHM+1, EFM , 1.)
                         GO TO 800
	           ENDIF
      CALL HF1(IHM,PCHI, 1.)
C
Cmike
C      if(ngam.eq.4) then
C      do j = 1,21
C        diff = (YCeq(j)-YCopy(j,MFIT,NBhypot,RNCMB))/
C     &             sqrt(GCopy(j,MFIT,NBhypot,RNCMB))
C        call hf1(HypModulator+IHH+40+j,diff,1.)
C      enddo
C
C-----Pt and missmass calculating and filling
C
      ESUM  = 0.
      PXSUM = 0.
      PYSUM = 0.
      PZSUM = 0.
C
      do ip=1,ngam
        PLAB(1,ip)=FPAR(6+3*ip)*DXRD(ip)
        PLAB(2,ip)=FPAR(6+3*ip)*DYRD(ip)
        PLAB(3,ip)=FPAR(6+3*ip)*dsqrt(dabs(1.0-DXRD(ip)**2-DYRD(ip)**2))
        PLAB(4,ip)=FPAR(6+3*ip)
      enddo           
c
      do ip=1,ngam
C-      ESUM = ESUM+ FPAR(6+3*ip)*1000.
        ESUM = ESUM+ FPAR(6+3*ip)
        pxsum= pxsum+PLAB(1,ip)
        pysum= pysum+PLAB(2,ip)
        pzsum= pzsum+PLAB(3,ip)
      enddo
C      
CEvd+
C event mixing
C pi0gamma
C
C      if(ngam.eq.3.and.mfit.eq.1)then  
C         ipi0gamma=ipi0gamma+1                     ! save current event
C         icurrent =ipi0gamma-(ipi0gamma/imix)*imix+1
C         do iindex=1,4
C            Ppi0pg(khp,iindex,icurrent)=PLAB(iindex,1)+PLAB(iindex,2)
C            Pgampg(khp,iindex,icurrent)=PLAB(iindex,3)
C         enddo
C! do mixing of current event with previous ones
C         if(ipi0gamma.gt.imix) then!do we have enough statistics?
C            do icycl = 1,imix
C               if(icycl.eq.icurrent)goto 44 ! we do not need to mix current event with current event
C               !calc everything we need
C               etot12=Ppi0pg(khp,4,icycl)+Pgampg(khp,4,icurrent)
C               px12  =Ppi0pg(khp,1,icycl)+Pgampg(khp,1,icurrent)
C               py12  =Ppi0pg(khp,2,icycl)+Pgampg(khp,2,icurrent)
C               pz12  =Ppi0pg(khp,3,icycl)+Pgampg(khp,3,icurrent)
C               
C               etot21=Pgampg(khp,4,icycl)+Ppi0pg(khp,4,icurrent)
C               px21  =Pgampg(khp,1,icycl)+Ppi0pg(khp,1,icurrent)
C               py21  =Pgampg(khp,2,icycl)+Ppi0pg(khp,2,icurrent)
C               pz21  =Pgampg(khp,3,icycl)+Ppi0pg(khp,3,icurrent)
C
C               efmas12 = sqrt(etot12**2 - px12**2 - py12**2 - pz12**2)
C               efmas21 = sqrt(etot21**2 - px21**2 - py21**2 - pz21**2)
C               pt12=sqrt(px12**2 + py12**2)
C               pt21=sqrt(px21**2 + py21**2)
C! fill histos
C               ihhh = 100000*khp
C               do icut=1,9  
C                  IH  = IHHH + 10000*icut + 1000*NGAM + 100*MFIT
C                  if(pt12.gt.Ptcut(icut)) then
C                     do j=0,9
C                        Ecut= 1.50 + 0.500*j
C                        if (etot12.gt.Ecut) then
C                           CALL HF1(-(IH+50+j), efmas12 ,1.)
C                           CALL HF1(-(IH+60+j), etot12  ,1.)
C                           CALL HF1(-(IH+70+j), pt12    ,1.)
C                        endif!energy cut (12)
C                     enddo
C                  endif  !ptcut (12)
C                  if(pt21.gt.Ptcut(icut)) then
C                     do j=0,9
C                        Ecut= 1.50 + 0.500*j
C                        if (etot21.gt.Ecut) then
C                           CALL HF1(-(IH+50+j), efmas21 ,1.)
C                           CALL HF1(-(IH+60+j), etot21  ,1.)
C                           CALL HF1(-(IH+70+j), pt21    ,1.)
C                        endif!energy cut (21)
C                     enddo
C                  endif  !ptcut (21)
C
C               enddo
C 44            continue
C            enddo  !end do icycl = 1,imix
C         endif  !end ipi0gamma > imix
C      endif !end pi0gamma
C
C mixing events
C etagamma
C
C      if(ngam.eq.3.and.mfit.eq.2)then
C         ietagamma=ietagamma+1                    !  save current event
C         icurrent=ietagamma-(ietagamma/imix)*imix+1
C         do iindex=1,4
C            Petaeg(khp,iindex,icurrent)=PLAB(iindex,1)+PLAB(iindex,2)
C            Pgameg(khp,iindex,icurrent)=PLAB(iindex,3)
C         enddo
C         !do mixing of current event with previous ones
C         if(ietagamma.gt.imix) then!do we have enough statistics?
C            do icycl = 1,imix
C               if(icycl.eq.icurrent)goto 45 ! we do not need to mix current event with current event
C               !calc everything we need
C               etot12=Petaeg(khp,4,icycl)+Pgameg(khp,4,icurrent)
C               px12  =Petaeg(khp,1,icycl)+Pgameg(khp,1,icurrent)
C               py12  =Petaeg(khp,2,icycl)+Pgameg(khp,2,icurrent)
C               pz12  =Petaeg(khp,3,icycl)+Pgameg(khp,3,icurrent)
C               
C               etot21=Pgameg(khp,4,icycl)+Petaeg(khp,4,icurrent)
C               px21  =Pgameg(khp,1,icycl)+Petaeg(khp,1,icurrent)
C               py21  =Pgameg(khp,2,icycl)+Petaeg(khp,2,icurrent)
C               pz21  =Pgameg(khp,3,icycl)+Petaeg(khp,3,icurrent)
C
C               efmas12 = sqrt(etot12**2 - px12**2 - py12**2 - pz12**2)
C               efmas21 = sqrt(etot21**2 - px21**2 - py21**2 - pz21**2)
C               pt12=sqrt(px12**2 + py12**2)
C               pt21=sqrt(px21**2 + py21**2)
C               
C!fill histos
C               ihhh = 100000*khp
C               do icut=1,9  
C                  IH  = IHHH + 10000*icut + 1000*NGAM + 100*MFIT
C                  if(pt12.gt.Ptcut(icut)) then
C                     do j=0,9
C                        Ecut= 1.50 + 0.500*j
C                        if (etot12.gt.Ecut) then
C                           CALL HF1(-(IH+50+j), efmas12 ,1.)
C                           CALL HF1(-(IH+60+j), etot12  ,1.)
C                           CALL HF1(-(IH+70+j), pt12    ,1.)
C                        endif   !energy cut (12)
C                     enddo
C                  endif  !ptcut (12)
C                  if(pt21.gt.Ptcut(icut)) then
C                     do j=0,9
C                        Ecut= 1.50 + 0.500*j
C                        if (etot21.gt.Ecut) then
C                           CALL HF1(-(IH+50+j), efmas21 ,1.)
C                           CALL HF1(-(IH+60+j), etot21  ,1.)
C                           CALL HF1(-(IH+70+j), pt21    ,1.)
C                        endif   !energy cut (21)
C                     enddo
C                  endif         !ptcut (21)
C               enddo
C 45            continue
C            enddo!end do icycl = 1,imix
C         endif!end ietagamma > imix
C      endif!end etagamma
C
C mixing events
C pi0pi0
C
C      if(ngam.eq.4.and.mfit.eq.1)then 
C         ipi0pi0=ipi0pi0+1                    !  save current event
C         icurrent=ipi0pi0-(ipi0pi0/imix)*imix+1
C         do iindex=1,4
C            Ppi01pp(khp,iindex,icurrent)=PLAB(iindex,1)+PLAB(iindex,2)
C            Ppi02pp(khp,iindex,icurrent)=PLAB(iindex,3)+PLAB(iindex,4)
C         enddo
C         !do mixing of current event with previous ones
C         if(ipi0pi0.gt.imix) then!do we have enough statistics?
C            do icycl = 1,imix
C               if(icycl.eq.icurrent)goto 46 ! we do not need to mix current event with current event
C               !calc everything we need
C               etot12=Ppi01pp(khp,4,icycl)+Ppi02pp(khp,4,icurrent)
C               px12  =Ppi01pp(khp,1,icycl)+Ppi02pp(khp,1,icurrent)
C               py12  =Ppi01pp(khp,2,icycl)+Ppi02pp(khp,2,icurrent)
C               pz12  =Ppi01pp(khp,3,icycl)+Ppi02pp(khp,3,icurrent)
C               
C               etot21=Ppi02pp(khp,4,icycl)+Ppi01pp(khp,4,icurrent)
C               px21  =Ppi02pp(khp,1,icycl)+Ppi01pp(khp,1,icurrent)
C               py21  =Ppi02pp(khp,2,icycl)+Ppi01pp(khp,2,icurrent)
C               pz21  =Ppi02pp(khp,3,icycl)+Ppi01pp(khp,3,icurrent)
C
C               efmas12 = sqrt(etot12**2 - px12**2 - py12**2 - pz12**2)
C               efmas21 = sqrt(etot21**2 - px21**2 - py21**2 - pz21**2)
C               pt12=sqrt(px12**2 + py12**2)
C               pt21=sqrt(px21**2 + py21**2)
C               
C!fill histos
C               ihhh = 100000*khp
C               do icut=1,9  
C                  IH  = IHHH + 10000*icut + 1000*NGAM + 100*MFIT
C                  if(pt12.gt.Ptcut(icut)) then
C                     do j=0,9
C                        Ecut= 1.50 + 0.500*j
C                        if (etot12.gt.Ecut) then
C                           CALL HF1(-(IH+50+j), efmas12 ,1.)
C                           CALL HF1(-(IH+60+j), etot12  ,1.)
C                           CALL HF1(-(IH+70+j), pt12    ,1.)
C                        endif   !energy cut (12)
C                     enddo
C                  endif  !ptcut (12)
C                  if(pt21.gt.Ptcut(icut)) then
C                     do j=0,9
C                        Ecut= 1.50 + 0.500*j
C                        if (etot21.gt.Ecut) then
C                           CALL HF1(-(IH+50+j), efmas21 ,1.)
C                           CALL HF1(-(IH+60+j), etot21  ,1.)
C                           CALL HF1(-(IH+70+j), pt21    ,1.)
C                        endif   !energy cut (21)
C                     enddo
C                  endif         !ptcut (21)
C               enddo
C 46            continue
C            enddo  !end do icycl = 1,imix
C         endif  !end ipi0pi0 > imix
C      endif  !end pi0pi0
C
C     mixing events
C     pi0eta
C
C      if(ngam.eq.4.and.mfit.eq.2)then  ! save current event
C         ietapi0=ietapi0+1
C         icurrent=ietapi0-(ietapi0/imix)*imix+1
C         do iindex=1,4
C            Ppi0ep(khp,iindex,icurrent)=PLAB(iindex,1)+PLAB(iindex,2)
C            Petaep(khp,iindex,icurrent)=PLAB(iindex,3)+PLAB(iindex,4)
C         enddo
C         !do mixing of current event with previous ones
C         if(ietapi0.gt.imix) then!do we have enough statistics?
C            do icycl = 1,imix
C               if(icycl.eq.icurrent)goto 47 ! we do not need to mix current event with current event
C               !calc everything we need
C               etot12=Ppi0ep(khp,4,icycl)+Petaep(khp,4,icurrent)
C               px12  =Ppi0ep(khp,1,icycl)+Petaep(khp,1,icurrent)
C               py12  =Ppi0ep(khp,2,icycl)+Petaep(khp,2,icurrent)
C               pz12  =Ppi0ep(khp,3,icycl)+Petaep(khp,3,icurrent)
C               
C               etot21=Petaep(khp,4,icycl)+Ppi0ep(khp,4,icurrent)
C               px21  =Petaep(khp,1,icycl)+Ppi0ep(khp,1,icurrent)
C               py21  =Petaep(khp,2,icycl)+Ppi0ep(khp,2,icurrent)
C               pz21  =Petaep(khp,3,icycl)+Ppi0ep(khp,3,icurrent)
C
C               efmas12 = sqrt(etot12**2 - px12**2 - py12**2 - pz12**2)
C               efmas21 = sqrt(etot21**2 - px21**2 - py21**2 - pz21**2)
C               pt12=sqrt(px12**2 + py12**2)
C               pt21=sqrt(px21**2 + py21**2)
C               
C               !fill histos
C               ihhh = 100000*khp
C               do icut=1,9  
C                  IH  = IHHH + 10000*icut + 1000*NGAM + 100*MFIT
C                  if(pt12.gt.Ptcut(icut)) then
C                     do j=0,9
C                        Ecut= 1.50 + 0.500*j
C                        if (etot12.gt.Ecut) then
C                           CALL HF1(-(IH+50+j), efmas12 ,1.)
C                           CALL HF1(-(IH+60+j), etot12  ,1.)
C                           CALL HF1(-(IH+70+j), pt12    ,1.)
C                        endif   !energy cut (12)
C                     enddo
C                  endif  !ptcut (12)
C                  if(pt21.gt.Ptcut(icut)) then
C                     do j=0,9
C                        Ecut= 1.50 + 0.500*j
C                        if (etot21.gt.Ecut) then
C                           CALL HF1(-(IH+50+j), efmas21 ,1.)
C                           CALL HF1(-(IH+60+j), etot21  ,1.)
C                           CALL HF1(-(IH+70+j), pt21    ,1.)
C                        endif   !energy cut (21)
C                     enddo
C                  endif         !ptcut (21)
C               enddo
C 47            continue
C            enddo  !end do icycl = 1,imix
C         endif  !end ietapi0 > imix
C      endif  !end etapi0
C
CEvd-
!kondr++{
!     mixing events ! no mixing for SA cut; see before, there is mixing for my cut
!     pi0gg
      if(ngam.eq.4.and.mfit.eq.3)then         !          !save current event
!          ipi0gg=ipi0gg+1
!          icurrent=ipi0gg-(ipi0gg/imix)*imix+1
!          do iindex=1,4
!             Ppi0pgg(khp,iindex,icurrent)=PLAB(iindex,1)+PLAB(iindex,2)
!             Pgampgg(khp,iindex,icurrent)=PLAB(iindex,3)+PLAB(iindex,4)
!          enddo
!          !do mixing of current event with previous ones
!          if(ipi0gg.gt.imix) then!do we have enough statistics?
!             do icycl = 1,imix
!                if(icycl.eq.icurrent)goto 49 ! we do not need to mix current event with current event
!                !calc everything we need
!                etot12=Ppi0pgg(khp,4,icycl)+Pgampgg(khp,4,icurrent)
!                px12  =Ppi0pgg(khp,1,icycl)+Pgampgg(khp,1,icurrent)
!                py12  =Ppi0pgg(khp,2,icycl)+Pgampgg(khp,2,icurrent)
!                pz12  =Ppi0pgg(khp,3,icycl)+Pgampgg(khp,3,icurrent)
!                
!                etot21=Pgampgg(khp,4,icycl)+Ppi0pgg(khp,4,icurrent)
!                px21  =Pgampgg(khp,1,icycl)+Ppi0pgg(khp,1,icurrent)
!                py21  =Pgampgg(khp,2,icycl)+Ppi0pgg(khp,2,icurrent)
!                pz21  =Pgampgg(khp,3,icycl)+Ppi0pgg(khp,3,icurrent)
! 
!                efmas12 = sqrt(etot12**2 - px12**2 - py12**2 - pz12**2)
!                efmas21 = sqrt(etot21**2 - px21**2 - py21**2 - pz21**2)
!                pt12=sqrt(px12**2 + py12**2)
!                pt21=sqrt(px21**2 + py21**2)
!                
!!fill histos
!                ihhh = 100000*khp
!                do icut=1,9  
!                   IH  = IHHH + 10000*icut + 1000*NGAM + 100*MFIT
!                   if(pt12.gt.Ptcut(icut)) then
!                      do j=0,9
!                         Ecut= 1.50 + 0.500*j
!                         if (etot12.gt.Ecut) then
!                            CALL HF1(-(IH+50+j), efmas12 ,1.)
!                            CALL HF1(-(IH+60+j), etot12  ,1.)
!                            CALL HF1(-(IH+70+j), pt12    ,1.)
!                         endif   !energy cut (12)
!                      enddo
!                   endif  !ptcut (12)
!                   if(pt21.gt.Ptcut(icut)) then
!                      do j=0,9
!                         Ecut= 1.50 + 0.500*j
!                         if (etot21.gt.Ecut) then
!                            CALL HF1(-(IH+50+j), efmas21 ,1.)
!                            CALL HF1(-(IH+60+j), etot21  ,1.)
!                            CALL HF1(-(IH+70+j), pt21    ,1.)
!                         endif   !energy cut (21)
!                      enddo
!                   endif         !ptcut (21)
!                enddo
!  49            continue
!             enddo!end do icycl = 1,imix
!          endif!end ipi0gg > imix
      endif!end etapi0
C
C event mixing for pi+ only
C pi0gamma (pi+)
C      kcheren=0
C      if(c1c2c3(2).ge.c2min.and.c1c2c3(1).ge.c1min) kcheren=1           ! pi+ only
C      if(ngam.eq.3.and.mfit.eq.1 .and. kcheren.eq.1) then
C         !save current event
C         ipi0gamma_ch=ipi0gamma_ch+1
C         icurrent=ipi0gamma_ch-(ipi0gamma_ch/imix)*imix+1
C         do iindex=1,4
C            Ppi0pg_ch(khp,iindex,icurrent)=PLAB(iindex,1)+PLAB(iindex,2)
C            Pgampg_ch(khp,iindex,icurrent)=PLAB(iindex,3)
C         enddo
C         !do mixing of current event with previous ones
C         if(ipi0gamma_ch.gt.imix) then!do we have enough statistics?
C            do icycl = 1,imix
C               if(icycl.eq.icurrent)goto 49 ! we do not need to mix current event with current event
C               !calc everything we need
C               etot12=Ppi0pg_ch(khp,4,icycl)+Pgampg_ch(khp,4,icurrent)
C               px12  =Ppi0pg_ch(khp,1,icycl)+Pgampg_ch(khp,1,icurrent)
C               py12  =Ppi0pg_ch(khp,2,icycl)+Pgampg_ch(khp,2,icurrent)
C               pz12  =Ppi0pg_ch(khp,3,icycl)+Pgampg_ch(khp,3,icurrent)
C               
C               etot21=Pgampg_ch(khp,4,icycl)+Ppi0pg_ch(khp,4,icurrent)
C               px21  =Pgampg_ch(khp,1,icycl)+Ppi0pg_ch(khp,1,icurrent)
C               py21  =Pgampg_ch(khp,2,icycl)+Ppi0pg_ch(khp,2,icurrent)
C               pz21  =Pgampg_ch(khp,3,icycl)+Ppi0pg_ch(khp,3,icurrent)
C
C               efmas12 = sqrt(etot12**2 - px12**2 - py12**2 - pz12**2)
C               efmas21 = sqrt(etot21**2 - px21**2 - py21**2 - pz21**2)
C               pt12=sqrt(px12**2 + py12**2)
C               pt21=sqrt(px21**2 + py21**2)
C               !fill histos
C               ihhh = 10000000*kcheren+ 100000*khp
C               do icut=1,9  
C                  IH  = IHHH + 10000*icut + 1000*NGAM + 100*MFIT
C                  if(pt12.gt.Ptcut(icut)) then
C                     do j=0,9
C                        Ecut= 1.50 + 0.500*j
C                        if (etot12.gt.Ecut) then
C                           CALL HF1(-(IH+50+j), efmas12 ,1.)
C                           CALL HF1(-(IH+60+j), etot12  ,1.)
C                           CALL HF1(-(IH+70+j), pt12    ,1.)
C                        endif!energy cut (12)
C                     enddo
C                  endif  !ptcut (12)
C                  if(pt21.gt.Ptcut(icut)) then
C                     do j=0,9
C                        Ecut= 1.50 + 0.500*j
C                        if (etot21.gt.Ecut) then
C                           CALL HF1(-(IH+50+j), efmas21 ,1.)
C                           CALL HF1(-(IH+60+j), etot21  ,1.)
C                           CALL HF1(-(IH+70+j), pt21    ,1.)
C                        endif!energy cut (21)
C                     enddo
C                  endif  !ptcut (21)
C
C               enddo
C 49            continue
C            enddo!end do icycl = 1,imix
C         endif!end ipi0gamma_ch > imix
C      endif!end pi0gamma
C
C mixing events
C pi0pi01 with cherenki
C      if(ngam.eq.4.and.mfit.eq.1.and.kcheren.eq.1)then
C         !save current event
C         ipi0pi01_ch=ipi0pi01_ch+1
C         icurrent=ipi0pi01_ch-(ipi0pi01_ch/imix)*imix+1
C         do iindex=1,4
C          Ppi01pp1_ch(khp,iindex,icurrent)=PLAB(iindex,1)+PLAB(iindex,2)
C          Ppi02pp1_ch(khp,iindex,icurrent)=PLAB(iindex,3)+PLAB(iindex,4)
C         enddo
C         !do mixing of current event with previous ones
C         if(ipi0pi01_ch.gt.imix) then!do we have enough statistics?
C           do icycl = 1,imix
C             if(icycl.eq.icurrent)goto 41 ! we do not need to mix current event with current event
C             !calc everything we need
C             etot12=Ppi01pp1_ch(khp,4,icycl)+Ppi02pp1_ch(khp,4,icurrent)
C             px12  =Ppi01pp1_ch(khp,1,icycl)+Ppi02pp1_ch(khp,1,icurrent)
C             py12  =Ppi01pp1_ch(khp,2,icycl)+Ppi02pp1_ch(khp,2,icurrent)
C             pz12  =Ppi01pp1_ch(khp,3,icycl)+Ppi02pp1_ch(khp,3,icurrent)
C              
C             etot21=Ppi02pp1_ch(khp,4,icycl)+Ppi01pp1_ch(khp,4,icurrent)
C             px21  =Ppi02pp1_ch(khp,1,icycl)+Ppi01pp1_ch(khp,1,icurrent)
C             py21  =Ppi02pp1_ch(khp,2,icycl)+Ppi01pp1_ch(khp,2,icurrent)
C             pz21  =Ppi02pp1_ch(khp,3,icycl)+Ppi01pp1_ch(khp,3,icurrent)
C
C             efmas12 = sqrt(etot12**2 - px12**2 - py12**2 - pz12**2)
C             efmas21 = sqrt(etot21**2 - px21**2 - py21**2 - pz21**2)
C             pt12=sqrt(px12**2 + py12**2)
C             pt21=sqrt(px21**2 + py21**2)
C               
C             !fill histos
C             ihhh = 100000*khp + kcheren*10000000
C             do icut=1,9  
C                IH  = IHHH + 10000*icut + 1000*NGAM + 100*MFIT
C                if(pt12.gt.Ptcut(icut)) then
C                    do j=0,9
C                      Ecut= 1.50 + 0.500*j
C                      if (etot12.gt.Ecut) then
C                          CALL HF1(-(IH+50+j), efmas12 ,1.)
C                          CALL HF1(-(IH+60+j), etot12  ,1.)
C                          CALL HF1(-(IH+70+j), pt12    ,1.)
C                      endif   !energy cut (12)
C                    enddo
C                endif  !ptcut (12)
C                if(pt21.gt.Ptcut(icut)) then
C                    do j=0,9
C                      Ecut= 1.50 + 0.500*j
C                      if (etot21.gt.Ecut) then
C                          CALL HF1(-(IH+50+j), efmas21 ,1.)
C                          CALL HF1(-(IH+60+j), etot21  ,1.)
C                          CALL HF1(-(IH+70+j), pt21    ,1.)
C                      endif   ! energy cut (21)
C                    enddo
C                endif       ! ptcut (21)
C             enddo
C 41          continue
C            enddo        ! end do icycl = 1,imix
C         endif       ! end ipi0pi01_ch > imix
C      endif     ! end pi0pi0
C
C     mixing events ! kondr--}
C                     I should add mixing for pi+, k+, p+ here

      Ptrans_fit = sqrt(pxsum**2+pysum**2)
C
Cmike-----t and missmass calculating
C
CSdv-   pxsum=pxsum*1000.
C-      pysum=pysum*1000.
C-      pzsum=pzsum*1000.
C
C-      esumcopy = ESUM
C-      pxsumcopy= pxsum
C-      pysumcopy= pysum
C-      pzsumcopy= pzsum
C      
C-      call t_and_missmass(5,0,esumcopy,pxsumcopy,pysumcopy,pzsumcopy)
C
C-      if((EffectiveMass.gt.430. .and.EffectiveMass.lt.460. ).or.
C-     &   (EffectiveMass.gt.520. .and.EffectiveMass.lt.550. )) then
C-          call t_and_missmass(3,2,ESUM,PXSUM,PYSUM,PZSUM)
C-      endif
C-      if( EffectiveMass.gt.460. .and.EffectiveMass.lt.520. )
C-     &    call t_and_missmass(3,1,ESUM,PXSUM,PYSUM,PZSUM)
C-      if((EffectiveMass.gt.920. .and.EffectiveMass.lt.1080.).or.
C-     &   (EffectiveMass.gt.1400..and.EffectiveMass.lt.1560.))
C-     &    call t_and_missmass(4,2,ESUM,PXSUM,PYSUM,PZSUM)
C-      if( EffectiveMass.gt.1080..and.EffectiveMass.lt.1400.)
C-     &    call t_and_missmass(4,1,ESUM,PXSUM,PYSUM,PZSUM)PULSE_SERVER
C-      endif
CSdv-        
C      
C-    write(*,*) 'chiq=',NGAM,CHIQ,PCHI
C                                                          ! another temporary variable
C-    IF ( PCHI.LT.0.15) GO TO 750
C-         CALL HF1(HypModulator+IHH+26, EFM, 1.)
C-         CALL HF1(HypModulator+IHH+27, RTYM,1.)
C-       CALL HF1(HypModulator+IHH+31, MMS ,1.)
C
C-    IF (-T .LT.0.25) THEN
C-         CALL HF1(HypModulator+IHH+28, EFM, 1.)
C-         CALL HF1(HypModulator+IHH+29, RTYM,1.)
C-    ENDIF
C  
C-    do i=1,12
C-     sstore(i)=fpar(i+6)
C-    enddo
C      
C-      if(efm.gt.0.75.and.efm.lt.0.85) 
C-     &       call hf1(HypModulator+5049,rtym,1.)
C-      if(efm.gt.0.85.and.efm.lt.0.90) 
C-     &       call hf1(HypModulator+5050,rtym,1.)
C-      if(efm.gt.0.70.and.efm.lt.0.75) 
C-     &       call hf1(HypModulator+5051,rtym,1.)
C-      if(ETYM.gt.1.00)
C-     &       call hf1(HypModulator+IHH+7,real(ZCoord),1.)
C ...........................................................
C
C ---  Energy and Pt cuts:  ---
C     
  750 IHHH = 100000*KHP
C-    do icut=1,9  
C-  	 if(Ptrans_Fit.gt.Ptcut(icut)) then
C-  	   IH  = IHHH + 10000*icut + 1000*NGAM + 100*MFIT
C-  	   do j=0,9
C-  	     Ecut= 1.50 + 0.500*j
C-  	     if (Esum.gt.Ecut) then
C-  	       call HF1(IH+50+j, EFM   ,1.)
C-  	       call HF1(IH+60+j, Esum  ,1.)
C-  	       call HF1(IH+70+j, Ptrans_Fit,1.)
C-  	     endif
C-  	   enddo
C-  	 endif   
C-     enddo
C
! !       write(*,*)'ngam;',ngam,mfit,efm,Ptrans_Fit,etym
!         if ((ngam.eq.3.or.ngam.eq.4).and.mfit.eq.1) then
!         hi_number_2d = 7000 + ngam*100 + mfit*10
! !       write(*,*)'efm;',efm,Ptrans_Fit,etym
!         call hf2(hi_number_2d,    efm, Ptrans_Fit ,1.)
!         call hf2(hi_number_2d + 1,efm, esum       ,1.)
!       endif
C      
C cherenki ++ 
C     ! the same as above, all the histogramms for pi+,k+,p+
C     ! ihhh = 100000*khp						! reset this, just in case
C     if (mfit.eq.1) then
C   	kcheren=0
C   	if(c1c2c3(2).ge.c2min.and.c1c2c3(1).ge.c1min) kcheren=1 	! pi+ only
C   	if(c1c2c3(2).ge.c2min.and.c1c2c3(1).lt.c1min) kcheren=2 	! k+ only
C!  	if(c1c2c3(2).lt.c2min.and.c1c2c3(1).lt.c1min) kcheren=0 	! 3  p+ only  ! we don't use them
C   	if(kcheren.ne.0) then
C   	  do icut=1,9  
C   	    if (Ptrans_Fit.gt.Ptcut(icut)) then
C   	      ih = ihhh + 10000*icut + 1000*ngam + 100*mfit
C   	      do j=0,9
C   		Ecut= 1.50 + 0.500*j
C   		if (Esum.gt.Ecut) then
C   		  call HF1(10000000*kcheren + IH+50+j, EFM   ,1.)
C   		  call HF1(10000000*kcheren + IH+60+j, Esum  ,1.)
C   		  call HF1(10000000*kcheren + IH+70+j, Ptrans_Fit,1.)
C   		endif
C   	      enddo
C   	    endif   
C   	  enddo
C   	endif
C     endif
C- cherenki --
C
C
Cmike-----Pt cuting
C     
C-      if(.not.cut_before_fit) goto 703
C-      do jcut=1,icut
C-      call hf1(Imfit+50+1000*ngam_+jcut*100+j,EffectiveMass,1.)
C-      enddo
C-        call hf1(Imfit+60+1000*ngam_+icut*100+j,RTYM*1000.,1.)
C- 703  continue
C
Cmike-----
C-         if(c1c2c3(2).gt.c2min.and.c1c2c3(1).lt.c1min) then
C-            call hf1(  HypModulator+ihh+30+100+j ,EffectiveMass,1.)
C-            call hf1(-(HypModulator+ihh-20+10 +j),EffectiveMass,1.)
C-         endif
C-         if(c1c2c3(2).lt.c2min.and.c1c2c3(1).lt.c1min) then
C-            call hf1(  HypModulator+ihh+30+150+j ,EffectiveMass,1.)
C-            call hf1(-(HypModulator+ihh-20+20 +j),EffectiveMass,1.)
C-         endif
C-         if(c1c2c3(2).gt.c2min.and.c1c2c3(1).gt.c1min) then
C-            call hf1(  HypModulator+ihh+30+ 50+j ,EffectiveMass,1.)
C-            call hf1(-(HypModulator+ihh-20+    j),EffectiveMass,1.)
C-        endif
C-        if(ngam.eq.4)then
C-            mim=0.
C-            ij=0
C-            do i1=2,4
C-             rij=sstore(i1*3+4)**2+sstore(i1*3+5)**2+fpar(6)**2
C-              di1=1./sqrt(rij)
C-              do i2=1,i1-1
C-                ij=ij+1
C-                rij=sstore(i2*3+4)**2+sstore(i2*3+5)**2+fpar(6)**2
C-                di2=1./sqrt(rij)
C-                 rij=di1*di2
C-                 csmesh=(sstore(i1*3+4)*sstore(i2*3+4)+
C-     &                    sstore(i1*3+5)*sstore(i2*3+5)+fpar(6)**2)*rij
C-                 esmesh=sstore(i1*3+6)*sstore(i2*3+6)
C-                 msmesh=2.*esmesh*(1.-csmesh)
C-                 mim=mim+msmesh
C-              enddo
C-            enddo
C-           mim=sqrt(abs(mim))
C-           call hf1(4350+j,mim*1000.,1.)
C-           endif
C-       endif
C-       if(ngam.eq.4) then
C-         do i=1,6
C-            sstore(i+12)=fpar(i+6)
C-         enddo
C-      endif
C      
  800 CONTINUE                  ! End of the KHP circle
C                               =========================
C  
      efm = sqrt(abs(efmq))     ! important
      KHP = KHP-1               ! 2 for 3-gamma  and  3 for 4-gamma  
C
      LNFIT = 12                ! magic number ! what is this ? ;k
      IF  (MFIT.EQ.0) GO TO 900
C!!-
      do  j=1,6
       DXRD(j)=CPAR(j)  
       DYRD(j)=CPAR(j+6)
      enddo
C!!-      
      do  j=1,72
       YCeq(j)=FPAR(j)
      enddo
C 
      CHHH = CHIQ
      NF   = NGAM-2
      IF (MFIT.eq.3) NF=1
      PCHI = PROB(CHHH, NF)
      IF (PCHI.lt.0.06) GO TO 850
C  ================================
C
      IHM  = 100000 + 1000*NGAM + 100*MFIT + 20
C
C-    write(*,*) 'IHM=',IHM,NGAM,IEV,MFIT 
C
      E12     =     YC( 9)+YC(12)
      CSGJ12  = ABS(YC( 9)-YC(12))/SQRT(ABS(E12*E12-EFM12Q))
C
      E34     =     YC(15)+YC(18)
      CSGJ34  = ABS(YC(15)-YC(18))/SQRT(ABS(E34*E34-EFM34Q))
C
      E123    = E12+YC(15)
      EFM123Q = EFM12Q + EMQPR( 2) + EMQPR( 3)
C
      E124    = E12+YC(18)
      EFM124Q = EFM12Q + EMQPR( 4) + EMQPR( 5)
C
      EFM12   = SQRT(ABS(EFM12Q))
      EFM34   = SQRT(ABS(EFM34Q))
C -----------------------------------------------------------------------------
C
C   CALCULATION COS PI0 IN G.J.S. FOR CASE WITH BEAM PARTICLE EXACTLY ALONG Z !
C
      ZETQ = YC(6)*YC( 6)
      PZ1  = YC(6)*YC( 9)/SQRT( YC( 7)**2 + YC( 8)**2 + ZETQ )
      PZ2  = YC(6)*YC(12)/SQRT( YC(10)**2 + YC(11)**2 + ZETQ )
      PZ3  = YC(6)*YC(15)/SQRT( YC(13)**2 + YC(14)**2 + ZETQ )
      PZ4  = YC(6)*YC(18)/SQRT( YC(16)**2 + YC(17)**2 + ZETQ )
      PZ12 = PZ1  + PZ2
      PZ34 = PZ3  + PZ4
      PZ0  = PZ12 + PZ34
      EFM2 = 2.*EFM
      EC12 =(EFMQ + EFM12Q - EFM34Q)/EFM2
      EC34 =(EFMQ + EFM34Q - EFM12Q)/EFM2
      EFMT = ET+EFM
      PZP1 = PZ12 - PZ0*(E12+EC12)/EFMT
      PZP2 = PZ34 - PZ0*(E34+EC34)/EFMT
      CSGJ1= PZP1/SQRT(ABS(EC12*EC12-EFM12Q))
      CSGJ2= PZP2/SQRT(ABS(EC34*EC34-EFM34Q))
C ----------------------------------------------
C
      RCOMB= RNCMB
      RYC6 = YC(6)
      RT   = T
      RTYM = ETYM
      MMQ  = T+NEUMQ+2.*(EB-ET)*NEUMS
      MMS  = sqrt(abs(MMQ))
C
      second_comb =.false.
      CHP2 = PMCH2(1,MFIT)
      XMP2 = PMCH2(2,MFIT)
      if ( CHP2.lt.100.) then
                         PCH2=PROB(CHP2,NF)
                         CALL HF1(IHM-5,CHP2,1.0)
		         CALL HF1(IHM-4,PCH2,1.0)
			 CALL HF1(IHM-3,XMP2,1.0)
	        if (PCH2.gt.0.06) then
	        CALL HF1(IHM-2,EFM,1.0)
		second_comb =.true.
		GO TO 850
		endif
      endif 
C 
      if (MMS.gt.1.4) go to 900   !!!->  Missing mass cut 
     
C-    write(*,*) 'MMQ=',T,NEUMQ,EB,ET,NEUMS,MMQ,MMS   
C
      CALL HF1(IHM+ 2, EFM    ,1.)
      CALL HF1(IHM+ 4,-RT     ,1.)
      CALL HF1(IHM+ 5, EFM12  ,1.)
      CALL HF1(IHM+ 6, EFM34  ,1.)
      CALL HF1(IHM+ 8, CSGJ12 ,1.)
      CALL HF1(IHM+ 9, CSGJ1  ,1.)
      CALL HF1(IHM+10, CSGJ34 ,1.)
      CALL HF1(IHM+11, CSGJ2  ,1.)
      CALL HF1(IHM+12, MMS    ,1.)
      CALL HF1(IHM+14, EFM1234,1.)
      CALL HF1(IHM+17, RTYM   ,1.)
      CALL HF1(IHM+18, RYC6   ,1.)	
      CALL HF1(IHM+19, RCOMB  ,1.)  
      CALL HF1(IHM+23, PXsum  ,1.) 
      CALL HF1(IHM+24, PYsum  ,1.)	     
      CALL HF1(IHM+25, Ptrans_Fit,1.)
C
      IF ( PCHI.gt.0.15) THEN
           CALL HF1(IHM+26, EFM ,1.)
	   CALL HF1(IHM+27, RTYM,1.)
           IF (-RT.lt.0.3) THEN
	        CALL HF1(IHM+28, EFM ,1.)
	        CALL HF1(IHM+29, RTYM,1.)
	   ENDIF 
      ENDIF
C                                      Sdv main energy dependent EFM histos
      do Je=0,9                     !=======================================
         if(ETYM.gt.1.5+0.5*Je) CALL HF1(IHM+30+Je,EFM ,1.)     
      enddo
C    
Comg-
      IF (NGAM.eq.3.and.ETYM.gt.3.0) THEN
           IF (Efm.gt.0.722.and.Efm.lt.0.842) then
               call hf1(IHM+41, RTYM, 1.)
	       call hf1(IHM+43, Efm , 1.)
	       call hf1(IHM+45,-RT  , 1.)
	       call hf1(IHM+47, MMS , 1.)
	       call hf2(IHM+49, MMS ,-RT, 1.)
C         
	   ELSE if (Efm.gt.0.662.and.Efm.lt.0.902) then  
	       call hf1(IHM+42, RTYM, 1.)
               call hf1(IHM+44, Efm , 1.)
               call hf1(IHM+46,-RT  , 1.)
               call hf1(IHM+48, MMS , 1.)
               call hf2(IHM+50, MMS ,-RT, 1.)	       
          ENDIF
Cf2-      
      ELSE IF (NGAM.eq.4.and.ETYM.gt.3.0) THEN
           IF (Efm.gt.1.100.and.Efm.lt.1.450) then
               call hf1(IHM+41, RTYM, 1.)
	       call hf1(IHM+43, Efm , 1.)
	       call hf1(IHM+45,-RT  , 1.)
	       call hf1(IHM+47, MMS , 1.)
	       call hf2(IHM+49, MMS ,-RT, 1.)
C         
	   ELSE if (Efm.gt.0.925.and.Efm.lt.1.625) then  
	       call hf1(IHM+42, RTYM, 1.)
               call hf1(IHM+44, Efm , 1.)
               call hf1(IHM+46,-RT  , 1.)
               call hf1(IHM+48, MMS , 1.)
               call hf2(IHM+50, MMS ,-RT, 1.)	       
          ENDIF
      ENDIF 
CKs-      
      IF (NGAM.eq.4.and.ETYM.gt.3.0) THEN
         IF (Efm.gt.0.472.and.Efm.lt.0.522) then
             call hf1(IHM+51, RTYM, 1.)
	     call hf1(IHM+53, Efm , 1.)
	     call hf1(IHM+55,-RT  , 1.)
	     call hf1(IHM+57, MMS , 1.)
	     call hf2(IHM+59, MMS ,-RT, 1.)
C       
	 ELSE if (Efm.gt.0.447.and.Efm.lt.0.547) then  
	     call hf1(IHM+52, RTYM, 1.)
             call hf1(IHM+54, Efm , 1.)
             call hf1(IHM+56,-RT  , 1.)
             call hf1(IHM+58, MMS , 1.)
             call hf2(IHM+60, MMS ,-RT, 1.)	     
        ENDIF 
      ENDIF    
C..................................................
C
C Kondratyuk probabilities based on Kfit analysis

 850  if(second_comb) go to 752  !  Second good combination test
C 
      Kfit = 0                                         !  Start of the probability Kfit analysis
C                                                        ========================================
C Ngam=3 Probabilities:
C      
      if (ngam.eq.3) then
         do i = 1, nhyf       ! cycle for all hypothesa
           if (probhyp(i).gt.0) call hf1(5500+ngam*10+i, probhyp(i),1.)     ! let's see our probabilities
         enddo 
	 if (probhyp(1).gt.0.and.probhyp(2).gt.0)
     +       call hf2(5591,probhyp(1),probhyp(2),1.)     ! correlation between 1 and 2 (clean) 
         if (
     +       probhyp(2).gt.GoodProbCriteria3g(2) .and. 
     +       probhyp(1).lt. HypProbCriteria3g(1)) then
                 Kfit = 2; goto 751; endif 
	 if (
     +       probhyp(1).gt.GoodProbCriteria3g(1) .and. 
     +       probhyp(2).lt. HypProbCriteria3g(2)) then
		 Kfit = 1; goto 751; endif
      endif
C
C Ngam=4 Probabilities:
C
      if (ngam.eq.4) then
          do i = 1, nhyf  ! cycle for all hypothesa
            if (probhyp(i).gt.0) call hf1(5500+ngam*10+i, probhyp(i),1.)  ! let's see our probabilities
          enddo
C	  
C-  	  if (probhyp(1).gt.0.and.probhyp(2).gt.0)
C-   +      call hf2(5592,probhyp(1),probhyp(2),1.)		    ! correlation between 1 and 2 (clean)
C-  	  if (probhyp(1).gt.0.and.probhyp(3).gt.0)
C-   +      call hf2(5593,probhyp(1),probhyp(3),1.)		    ! correlation between 1 and 3 (clean)
C-  	  if (probhyp(2).gt.0.and.probhyp(3).gt.0)
C-   +      call hf2(5594,probhyp(2),probhyp(3),1.)		    ! correlation between 2 and 3 (clean)
       if (
     +      probhyp(3).gt.GoodProbCriteria4g(3) .and. 
     +      probhyp(1).lt. HypProbCriteria4g(1) .and. 
     +      probhyp(2).lt. HypProbCriteria4g(2)) then
               Kfit = 3; goto 751; endif 
        if (
     +      probhyp(1).gt.GoodProbCriteria4g(1) .and. 
C3!- +      probhyp(3).lt. HypProbCriteria4g(3) .and. 
     +      probhyp(2).lt. HypProbCriteria4g(2)) then
               Kfit = 1; goto 751; endif			     ! not used ; mind the 752
        if (
     +      probhyp(2).gt.GoodProbCriteria4g(2) .and. 
C3!- +      probhyp(3).lt. HypProbCriteria4g(3) .and. 
     +      probhyp(1).lt. HypProbCriteria4g(1)) then
	        Kfit = 2; goto 751; endif			     ! not used ; mind the 752
      endif
  
      if (Kfit.eq.0)      goto 752  ! NO one succeeded              
C                                   ! End of Kfit analysis 
 751  RTYM = ETYM
C      
      if (ngam.eq.3) then
        if(Kfit.eq.1) call hf2(13112,probhyp(1),probhyp(2),1.)       ! correlation between 1 and 2 (after cut)
        if(Kfit.eq.2) call hf2(13212,probhyp(1),probhyp(2),1.)       ! correlation between 1 and 2 (after cut)
        call hf1(5600+ngam*10+Kfit, probhyp(Kfit),1.)                ! let's see our probabilities after cut
      endif

      if (ngam.eq.4) then
C-      if(Kfit.eq.1) then
C-        call hf2(14112,probhyp(1),probhyp(2),1.)                   ! correlation between 1 and 2 (after cut)
C-        call hf2(14113,probhyp(1),probhyp(3),1.)                   ! correlation between 1 and 3 (after cut)
C-      endif
C-      if(Kfit.eq.2) then
C-        call hf2(14223,probhyp(2),probhyp(3),1.)                   ! correlation between 2 and 3 (after cut)
C-        call hf2(14212,probhyp(1),probhyp(2),1.)                   ! correlation between 2 and 1 (after cut)
C-      endif
C-      if(Kfit.eq.3) then
C-        call hf2(14313,probhyp(1),probhyp(3),1.)                   ! correlation between 3 and 1 (after cut)
C-        call hf2(14323,probhyp(2),probhyp(3),1.)                   ! correlation between 3 and 2 (after cut)
C-      endif
        call hf1(5600+ngam*10+Kfit, probhyp(Kfit),1.)                ! let's see our probabilities after cut
      endif     
      
      do ip=1,ngam
        plab_kondr(1,ip)=YCeq(6+3*ip)*DXRD(ip)
        plab_kondr(2,ip)=YCeq(6+3*ip)*DYRD(ip)
        plab_kondr(3,ip)=YCeq(6+3*ip)*
     +             dsqrt(dabs(1.0-DXRD(ip)**2-DYRD(ip)**2))
        plab_kondr(4,ip)=YCeq(6+3*ip)
      enddo  

      pysum = 0 ; pxsum = 0
      do ip=1,ngam
         pxsum = pxsum+plab_kondr(1,ip)
         pysum = pysum+plab_kondr(2,ip)
      enddo
      Ptrans_fit= sqrt(pxsum**2+pysum**2)
      
C Cherenki ++ 
C the same as below, all the histogramms for pi+,k+,p+
C-    if (fit.eq.1.or.(Kfit.eq.3.and.ngam.eq.4)) then
C-      kcheren=0
C-      if(c1c2c3(2).ge.c2min.and.c1c2c3(1).ge.c1min) kcheren=1         ! pi+ only
C-      if(c1c2c3(2).ge.c2min.and.c1c2c3(1).lt.c1min) kcheren=2         ! k+ only
C-      if(kcheren.ne.0) then
C-        do icut=1,9  
C-          if (Ptrans_Fit.gt.Ptcut(icut)) then
C-            ih = ihhh + 10000*icut + 1000*ngam + 100*Kfit
C-            do j=0,9
C-      	Ecut= 1.50 + 0.500*j
C-      	if (Etym.gt.Ecut) then
C-      	  call HF1(10000000*kcheren + IH+50+j, EFM   ,1.)
C-      	  call HF1(10000000*kcheren + IH+60+j, Rtym  ,1.)
C-      	  call HF1(10000000*kcheren + IH+70+j, Ptrans_Fit,1.)
C-      	endif
C-            enddo
C-          endif   
C-        enddo
C-      endif
C-    endif
C Cherenki --
C
C 
C Kondratyuk main Energy and Pt histo of the base of Kfit analysis
C''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
C!!-  ihhh = 100000*khp
      ihhh = 100000*Kfit
C
C-    write(*,*) 'IH_=',IHHH,NGAM,IEV,MFIT,KFIT 
C-    write(*,*) 
C
      do icut=1,9  
        if(Ptrans_Fit.gt.Ptcut(icut)) then
  	IH  = ihhh + 10000*icut + 1000*ngam + 100*Kfit
	do j=0,9
	  Ecut= 1.50 + 0.500*j
	  if (ETYM.gt.Ecut) then
	      call HF1(IH+50+j, EFM   ,1.)
	      call HF1(IH+60+j, RTYM  ,1.)
	      call HF1(IH+70+j, Ptrans_Fit,1.)
	  endif
	 enddo
        endif   
      enddo
C''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
C mixing events
C pi0pi0
C
      if(ngam.eq.4.and.Kfit.eq.1) then 
         ipi0pi0 =ipi0pi0+1                    !  save current event
         icurrent=ipi0pi0-(ipi0pi0/imix)*imix+1 
         do iindex=1,4
            Ppi01pp(khp,iindex,icurrent)=PLAB(iindex,1)+PLAB(iindex,2)
            Ppi02pp(khp,iindex,icurrent)=PLAB(iindex,3)+PLAB(iindex,4)
         enddo
         !do mixing of current event with previous ones
         if(ipi0pi0.gt.imix) then!do we have enough statistics?
            do icycl = 1,imix
               if(icycl.eq.icurrent)goto 46 ! we do not need to mix current event with current event
               !calc everything we need
               etot12=Ppi01pp(khp,4,icycl)+Ppi02pp(khp,4,icurrent)
               px12  =Ppi01pp(khp,1,icycl)+Ppi02pp(khp,1,icurrent)
               py12  =Ppi01pp(khp,2,icycl)+Ppi02pp(khp,2,icurrent)
               pz12  =Ppi01pp(khp,3,icycl)+Ppi02pp(khp,3,icurrent)
               
               etot21=Ppi02pp(khp,4,icycl)+Ppi01pp(khp,4,icurrent)
               px21  =Ppi02pp(khp,1,icycl)+Ppi01pp(khp,1,icurrent)
               py21  =Ppi02pp(khp,2,icycl)+Ppi01pp(khp,2,icurrent)
               pz21  =Ppi02pp(khp,3,icycl)+Ppi01pp(khp,3,icurrent)

               efmas12 = sqrt(etot12**2 - px12**2 - py12**2 - pz12**2)
               efmas21 = sqrt(etot21**2 - px21**2 - py21**2 - pz21**2)
               pt12=sqrt(px12**2 + py12**2)
               pt21=sqrt(px21**2 + py21**2)
               
!fill histos
C!!-           ihhh = 100000*khp
C---           ihhh = 100000*Kfit
               do icut=1,9  
                  if(pt12.gt.Ptcut(icut)) then
                     do j=0,9
                        Ecut= 1.50 + 0.500*j
                        if (etot12.gt.Ecut) then
                           CALL HF1(-(IH+50+j), efmas12 ,1.)
                           CALL HF1(-(IH+60+j), etot12  ,1.)
                           CALL HF1(-(IH+70+j), pt12    ,1.)
                        endif   !energy cut (12)
                     enddo
                  endif  !ptcut (12)
                  if(pt21.gt.Ptcut(icut)) then
                     do j=0,9
                        Ecut= 1.50 + 0.500*j
                        if (etot21.gt.Ecut) then
                           CALL HF1(-(IH+50+j), efmas21 ,1.)
                           CALL HF1(-(IH+60+j), etot21  ,1.)
                           CALL HF1(-(IH+70+j), pt21    ,1.)
                        endif   !energy cut (21)
                     enddo
                  endif         !ptcut (21)
               enddo
 46            continue
            enddo  !end do icycl = 1,imix
         endif  !end ipi0pi0 > imix
      endif  !end pi0pi0
C
C
C mixing events
C pi0eta
C
      if(ngam.eq.4.and.Kfit.eq.2) then  ! save current event
         ietapi0=ietapi0+1
         icurrent=ietapi0-(ietapi0/imix)*imix+1
         do iindex=1,4
            Ppi0ep(khp,iindex,icurrent)=PLAB(iindex,1)+PLAB(iindex,2)
            Petaep(khp,iindex,icurrent)=PLAB(iindex,3)+PLAB(iindex,4)
         enddo
	 
         !do mixing of current event with previous ones
         if(ietapi0.gt.imix) then!do we have enough statistics?
            do icycl = 1,imix
               if(icycl.eq.icurrent)goto 66 ! we do not need to mix current event with current event
               !calc everything we need
               etot12=Ppi0ep(khp,4,icycl)+Petaep(khp,4,icurrent)
               px12  =Ppi0ep(khp,1,icycl)+Petaep(khp,1,icurrent)
               py12  =Ppi0ep(khp,2,icycl)+Petaep(khp,2,icurrent)
               pz12  =Ppi0ep(khp,3,icycl)+Petaep(khp,3,icurrent)
               
               etot21=Petaep(khp,4,icycl)+Ppi0ep(khp,4,icurrent)
               px21  =Petaep(khp,1,icycl)+Ppi0ep(khp,1,icurrent)
               py21  =Petaep(khp,2,icycl)+Ppi0ep(khp,2,icurrent)
               pz21  =Petaep(khp,3,icycl)+Ppi0ep(khp,3,icurrent)

               efmas12 = sqrt(etot12**2 - px12**2 - py12**2 - pz12**2)
               efmas21 = sqrt(etot21**2 - px21**2 - py21**2 - pz21**2)
               pt12=sqrt(px12**2 + py12**2)
               pt21=sqrt(px21**2 + py21**2)
               
!fill histos
C!!-           ihhh = 100000*khp
C---           ihhh = 100000*KFIT
               do icut=1,9  
                  IH  = IHHH + 10000*icut + 1000*NGAM + 100*KFIT
                  if(pt12.gt.Ptcut(icut)) then
                     do j=0,9
                        Ecut= 1.50 + 0.500*j
                        if (etot12.gt.Ecut) then
                           CALL HF1(-(IH+50+j), efmas12 ,1.)
                           CALL HF1(-(IH+60+j), etot12  ,1.)
                           CALL HF1(-(IH+70+j), pt12    ,1.)
                        endif   !energy cut (12)
                     enddo
                  endif  !ptcut (12)
                  if(pt21.gt.Ptcut(icut)) then
                     do j=0,9
                        Ecut= 1.50 + 0.500*j
                        if (etot21.gt.Ecut) then
                           CALL HF1(-(IH+50+j), efmas21 ,1.)
                           CALL HF1(-(IH+60+j), etot21  ,1.)
                           CALL HF1(-(IH+70+j), pt21    ,1.)
                        endif   !energy cut (21)
                     enddo
                  endif         !ptcut (21)
               enddo
   66        continue
           enddo  !end do icycl = 1,imix
         endif !end ietapi0 > imix
      endif !end etapi0
C 
! mixing events 
! pi0gg1
!
      if(ngam.eq.4.and.Kfit.eq.3) then   ! save current event
         ipi0gg1 =ipi0gg1+1
         icurrent=ipi0gg1-(ipi0gg1/imix)*imix+1
         do iindex=1,4
            Ppi0pgg1(khp,iindex,icurrent)=plab_kondr(iindex,1)
     +                                   +plab_kondr(iindex,2)
            Pgampgg1(khp,iindex,icurrent)=plab_kondr(iindex,3)
     +                                   +plab_kondr(iindex,4)
         enddo
         
! do mixing of current event with previous ones
!
         if(ipi0gg1.gt.imix) then             ! do we have enough statistics?
            do icycl = 1,imix
              if(icycl.eq.icurrent) goto 48   ! we do not need to mix current event with current event
! calc everything we need
              etot12=Ppi0pgg1(khp,4,icycl)+Pgampgg1(khp,4,icurrent)
              px12  =Ppi0pgg1(khp,1,icycl)+Pgampgg1(khp,1,icurrent)
              py12  =Ppi0pgg1(khp,2,icycl)+Pgampgg1(khp,2,icurrent)
              pz12  =Ppi0pgg1(khp,3,icycl)+Pgampgg1(khp,3,icurrent)
              
              etot21=Pgampgg1(khp,4,icycl)+Ppi0pgg1(khp,4,icurrent)
              px21  =Pgampgg1(khp,1,icycl)+Ppi0pgg1(khp,1,icurrent)
              py21  =Pgampgg1(khp,2,icycl)+Ppi0pgg1(khp,2,icurrent)
              pz21  =Pgampgg1(khp,3,icycl)+Ppi0pgg1(khp,3,icurrent)

              efmas12 = sqrt(etot12**2 - px12**2 - py12**2 - pz12**2)
              efmas21 = sqrt(etot21**2 - px21**2 - py21**2 - pz21**2)
              pt12=sqrt(px12**2 + py12**2)
              pt21=sqrt(px21**2 + py21**2)

! fill histos
!             ihhh = 100000*khp                              ! we take this from above
C---          ihhh = 100000*KFIT
              do icut=1,9  
                IH  = ihhh + 10000*icut + 1000*NGAM + 100*KFIT
                if(pt12.gt.Ptcut(icut)) then
                    do j=0,9
                      Ecut= 1.50 + 0.500*j 
                      if (etot12.gt.Ecut) then
                          CALL HF1(-(IH+50+j), efmas12 ,1.)
                          CALL HF1(-(IH+60+j), etot12  ,1.)
                          CALL HF1(-(IH+70+j), pt12    ,1.)
                      endif   !energy cut (12)
                    enddo
                endif  !ptcut (12)
                if(pt21.gt.Ptcut(icut)) then
                    do j=0,9
                      Ecut= 1.50 + 0.500*j
                      if (etot21.gt.Ecut) then
                          CALL HF1(-(IH+50+j), efmas21 ,1.)
                          CALL HF1(-(IH+60+j), etot21  ,1.)
                          CALL HF1(-(IH+70+j), pt21    ,1.)
                      endif   !energy cut (21)
                    enddo
                endif         !ptcut (21)
              enddo
 48           continue
            enddo!end do icycl = 1,imix
         endif!end ipi0gg1 > imix
      endif!end etapi0

! event mixing
! pi0gamma1
!
      if(ngam.eq.3.and.Kfit.eq.1)then      ! save current event
	  ipi0gamma1=ipi0gamma1+1
	  icurrent=ipi0gamma1-(ipi0gamma1/imix)*imix+1
	  do iindex=1,4
	     Ppi0pg1(khp,iindex,icurrent)=plab_kondr(iindex,1)
     +  				 +plab_kondr(iindex,2)
	     Pgampg1(khp,iindex,icurrent)=plab_kondr(iindex,3)
	  enddo

! do mixing of current event with previous ones
	  if(ipi0gamma1.gt.imix) then                             ! do we have enough statistics?
	     do icycl = 1,imix
		if(icycl.eq.icurrent)goto 43                      ! we do not need to mix current event 
                                                                  ! with current event calc everything we need
		etot12=Ppi0pg1(khp,4,icycl)+Pgampg1(khp,4,icurrent)
		px12  =Ppi0pg1(khp,1,icycl)+Pgampg1(khp,1,icurrent)
		py12  =Ppi0pg1(khp,2,icycl)+Pgampg1(khp,2,icurrent)
		pz12  =Ppi0pg1(khp,3,icycl)+Pgampg1(khp,3,icurrent)
		
		etot21=Pgampg1(khp,4,icycl)+Ppi0pg1(khp,4,icurrent)
		px21  =Pgampg1(khp,1,icycl)+Ppi0pg1(khp,1,icurrent)
		py21  =Pgampg1(khp,2,icycl)+Ppi0pg1(khp,2,icurrent)
		pz21  =Pgampg1(khp,3,icycl)+Ppi0pg1(khp,3,icurrent)
 
		efmas12 = sqrt(etot12**2 - px12**2 - py12**2 - pz12**2)
		efmas21 = sqrt(etot21**2 - px21**2 - py21**2 - pz21**2)
		pt12=sqrt(px12**2 + py12**2)
		pt21=sqrt(px21**2 + py21**2)
! fill histos
C!!-		ihhh = 100000*khp
C---		ihhh = 100000*KFIT
		do icut=1,9  
		   IH= IHHH + 10000*icut + 1000*NGAM + 100*KFIT
		   if(pt12.gt.Ptcut(icut)) then
		      do j=0,9
			 Ecut= 1.50 + 0.500*j
			 if (etot12.gt.Ecut) then
			    CALL HF1(-(IH+50+j), efmas12 ,1.)
			    CALL HF1(-(IH+60+j), etot12  ,1.)
			    CALL HF1(-(IH+70+j), pt12	 ,1.)
			 endif !energy cut (12)
		      enddo
		   endif  !ptcut (12)
		   if(pt21.gt.Ptcut(icut)) then
		      do j=0,9
			 Ecut= 1.50 + 0.500*j
			 if (etot21.gt.Ecut) then
			    CALL HF1(-(IH+50+j), efmas21 ,1.)
			    CALL HF1(-(IH+60+j), etot21  ,1.)
			    CALL HF1(-(IH+70+j), pt21	 ,1.)
			 endif!energy cut (21)
		      enddo
		   endif  !ptcut (21)
 
		enddo
  43		continue
	     enddo  !end do icycl = 1,imix
	  endif  !end ipi0gamma1 > imix
      endif  !end pi0gamma1
      
C mixing events
C etagamma1
C
      if(ngam.eq.3.and.Kfit.eq.2) then
C                                        save current event
         ietagamma1=ietagamma1+1
         icurrent=ietagamma1-(ietagamma1/imix)*imix+1
         do iindex=1,4
            Petaeg1(khp,iindex,icurrent)=plab_kondr(iindex,1)
     +                                  +plab_kondr(iindex,2)
            Pgameg1(khp,iindex,icurrent)=plab_kondr(iindex,3)
         enddo

!do mixing of current event with previous ones

         if(ietagamma1.gt.imix) then ! do we have enough statistics?
            do icycl = 1,imix
               if(icycl.eq.icurrent)goto 42 ! we do not need to mix current event with current event
               !calc everything we need               
               etot12=Petaeg1(khp,4,icycl)+Pgameg1(khp,4,icurrent)
               px12  =Petaeg1(khp,1,icycl)+Pgameg1(khp,1,icurrent)
               py12  =Petaeg1(khp,2,icycl)+Pgameg1(khp,2,icurrent)
               pz12  =Petaeg1(khp,3,icycl)+Pgameg1(khp,3,icurrent)
               
               etot21=Pgameg1(khp,4,icycl)+Petaeg1(khp,4,icurrent)
               px21  =Pgameg1(khp,1,icycl)+Petaeg1(khp,1,icurrent)
               py21  =Pgameg1(khp,2,icycl)+Petaeg1(khp,2,icurrent)
               pz21  =Pgameg1(khp,3,icycl)+Petaeg1(khp,3,icurrent)

               efmas12 = sqrt(etot12**2 - px12**2 - py12**2 - pz12**2)
               efmas21 = sqrt(etot21**2 - px21**2 - py21**2 - pz21**2)
               pt12=sqrt(px12**2 + py12**2)
               pt21=sqrt(px21**2 + py21**2)
               
! fill histos
C---           ihhh = 100000*KFIT
               do icut=1,9  
                  IH= IHHH + 10000*icut + 1000*NGAM + 100*KFIT
                  if(pt12.gt.Ptcut(icut)) then
                     do j=0,9
                        Ecut= 1.50 + 0.500*j
                        if (etot12.gt.Ecut) then
                           CALL HF1(-(IH+50+j), efmas12 ,1.)
                           CALL HF1(-(IH+60+j), etot12  ,1.)
                           CALL HF1(-(IH+70+j), pt12    ,1.)
                        endif   !energy cut (12)
                     enddo
                  endif  !ptcut (12)
                  if(pt21.gt.Ptcut(icut)) then
                     do j=0,9
                        Ecut= 1.50 + 0.500*j
                        if (etot21.gt.Ecut) then
                           CALL HF1(-(IH+50+j), efmas21 ,1.)
                           CALL HF1(-(IH+60+j), etot21  ,1.)
                           CALL HF1(-(IH+70+j), pt21    ,1.)
                        endif   !energy cut (21)
                     enddo
                  endif         !ptcut (21)
               enddo
 42            continue
            enddo !end do icycl = 1,imix
         endif !end ietagamma1 > imix
      endif !end etagamma1
    
 752  continue 
 
 900  CONTINUE
      RETURN
      END
C
      SUBROUTINE BOOK_FIT
      logical hexist !cmike
      integer IDVECT(100000)
      common /pt/ ptcut(10)  ! cmike
      data ptcut/0.00,0.10,0.15,0.20,0.25,0.30,0.35,0.40,0.45,0.50/ ! Pt cut levels in GeV
      character *60 a32
      character *400 khist_name
      character *300 base_name
      character *15 part_name
C
      write(*,*) 'BOOK_FIT >'           

! can we delete this ? ; k
C 
C     CALL HBOOK1( 11,'READING   TYPE EVENT     ', 50,    0.,  50.)
C     CALL HBOOK1( 21,'TREATMENT TYPE EVENT     ', 50,    0.,  50.)
C     CALL HBOOK1( 51,'ORIGINAL  MULT. OF PHOTON', 50,    0.,  50.)
C     CALL HBOOK1( 52,'CORRECTED MULT. OF PHOTON', 50,    0.,  50.)
C     CALL HBOOK1( 60,'TARGET DIST.  IBTG=0     ',100, -500., 500.)
C
      DO 20 Ig=2,4 
      IHH = Ig*100
      CALL HBOOK1(IHH+1,'Hyperon Esum	       ',150, 0.,1.5E4,  0.)
      CALL HBOOK1(IHH+2,'Kinfit  Einit         ',500, 0.,1.5E4 , 0.)
      CALL HBOOK1(IHH+3,'Kinfit  Ecorrected    ',500, 0.,1.5E4 , 0.)
      CALL HBOOK1(IHH+4,'Kinfit  Ecor. for T   ',500, 0.,1.5E4 , 0.)
      CALL HBOOK1(IHH+5,'Hyperon Effmas        ',2000,0., 2000., 0.)
      CALL HBOOK1(IHH+6,'Kinfit  Effmas        ',2000,0., 2000., 0.)
      CALL HBOOK1(IHH+7,'Kinfit  T-distribution',1000,0., 1.00 , 0.)
      CALL HBOOK1(IHH+8,'Kinfit  Missing Mass  ', 180,0., 3600., 0.)
   20 CONTINUE
C
C-    CALL HBOOK1(408,'NO PIPI MASS             ',100,    0.,  2.5)
C-    CALL HBOOK1(409,'T - DISTR. 2PI0  CORRECT.',100,    0.,  0.5)
C-    CALL HBOOK1(411,'PAIR MASS                ',100,    0.,  1.0)
C-    CALL HBOOK1(412,'PAIR MASS 34 IF 12=PI0   ',100,    0.,  1.0)
C-    CALL HBOOK1(413,'PAIR MASS 34 IF 12=PI0   ', 54,  .000, .270)
C-    CALL HBOOK1(414,'PAIR MASS 34 IF 12=PI0   ', 60,  .400, .700)
C-    CALL HBOOK1(415,'PAIR MASS 34 IF IBTG=1   ', 54,  .000, .270)
C-    CALL HBOOK1(416,'PAIR MASS 34 IF IBTG=1   ', 60,  .400, .700)
C
C-    CALL HBOOK1(420,'4G  MASS OF   BAD  EVENTS',100, 0., 2.5, 0.)
C-    CALL HBOOK1(421,'2PI MASS OF   BAD  EVENTS',100, 0., 2.5, 0.)
C-    CALL HBOOK1(422,'T - DIST 2PI  BAD  EVENTS',100, 0.,  .5, 0.)
C-    CALL HBOOK1(423,'EPI MASS OF   BAD  EVENTS',100, 0., 2.5, 0.)
C
C-    CALL HBOOK1(430,'4G  MASS OF  GOOD  EVENTS',100, 0.,  2.5, 0.)
C-    CALL HBOOK1(431,'2PI MASS OF  GOOD  EVENTS',100, 0.,  2.5, 0.)
C-    CALL HBOOK1(432,'T - DIST 2PI GOOD  EVENTS',100, 0.,   .5, 0.)
C-    CALL HBOOK1(433,'EPI MASS OF  GOOD  EVENTS',100, 0.,  2.5, 0.)
C-    CALL HBOOK1(441,'4G  MASS OF NO FIT EVENTS',100, 0.,  2.5, 0.)
C
      CALL HBOOK1( 999,'FIT INDEX DISTRIBUT.   ', 100, 0., 100., 0.)
      CALL HBOOK1(1000,'FIT INDEX DISTRIBUT My.', 200, 0., 100., 0.)

C
C-    do mfit =1,2
C-    Imfit   =100000*(mfit+2) 
C-    do ngam_=3,4  ! Photon number
C-       do icp=1,9 ! i trans. moment cut
C-       
C-       do ice=0,9 ! i energy cut
C-       write(a32,222)'Eff mas, Pt^2! gt',ptcut(icp),
C-     &               'GeV^2!, Etot gt',500*(ice+2)
C-       if (ice.le.9) then 
C-       call hbook1(Imfit+50+1000*ngam_+icp*100+ice,a32,1000,0.,2000.,0.)
C-       else 
C-       call hbook1(Imfit+50+1000*ngam_+icp*100+ice,'Etot with Pt cut',
C-     +                                                1000,0.,10000.,0.)
C-       endif
C-       enddo
C	    
C-       write(a32,222)'Etot GeV,  Pt^2! gt',ptcut(icp)
C-       call hbook1(Imfit+60+1000*ngam_+icp*100+ice,
C-     +             'Etot, Pt gt valuse',1000,0.,10.,0.)
C-          enddo
C-      enddo
C-      enddo
C      
C- 222  format(a17,f4.2,a7,i6)
C-
C-     DO 200 IP=3,4
C-      
C-     IH = 1000*IP
C-      write(a32,*) 'Reconstructed Pt'
C-      call hbook1(400000+IH,a32,1000, 0., 2.0, 0.)
C-      write(a32,*) 'Reconstructed Pt'
C-      call hbook1(400000+IH+1,a32,1000, 0., 2.0, 0.)
C-    call hbook1(100000+IH,a32,1000, 0., 2.0, 0.)
C-    call hbook1(200000+IH,a32,1000, 0., 2.0, 0.)
C-    call hbook1(300000+IH,a32,1000, 0., 2.0, 0.)
C-
C-    do 199 ipp=1,3
C-     IH = 1000*IP+100*ipp 
C-     write(a32,*)'Eff mass 2C fit, Etot fit ','(',500*(2),'+)'
C-     call hbook1(	   IH+50,a32, 1000, 0., 2000., 0.)
C-     call hbook1( 100000+IH+50,a32, 1000, 0., 2000., 0.)
C      call hbook1( 200000+IH+50,a32, 1000, 0., 2000., 0.)
C      call hbook1( 300000+IH+50,a32, 1000, 0., 2000., 0.)
C-------summarnaya energiya 0.5+j*0.5---------------------
C-    do 198 j=0,9
C-    do 197 III = 0,200000,100000
C-    write(a32,*)'Eff mass 2C fit, Etot fit ','(',500*(j+2),'+)'
C-    call hbook1((iii+IH+50 +j),a32, 1000, 0., 2000., 0.)
C-    write(a32,*)'Eff mass 2C fit, Etot fit(pi+) ','(',500*(j+2),'+)'
C-    call hbook1( (iii+IH+100+j),a32, 1000, 0., 2000., 0.)
C-    call hbook1(-(iii+IH+100+j),a32, 1000, 0., 2000., 0.)
C-    write(a32,*)'Eff mass 2C fit, Etot fit(K+) ','(',500*(j+2),'+)'
C-    call hbook1( (iii+IH+150+j),a32, 1000, 0., 2000., 0.)
C-    call hbook1(-(iii+IH+10 +j),a32, 1000, 0., 2000., 0.)
C-    write(a32,*)'Eff mass 2C fit, Etot fit(p+) ','(',500*(j+2),'+)'
C-    call hbook1( (iii+IH+200+j),a32, 1000, 0., 2000., 0.)
C-    call hbook1(-(iii+IH+20 +j),a32, 1000, 0., 2000., 0.)
C-    write(a32,*)'Smeshannaya massa iz raznyh sobytiy ',
C-   &			  '(',0.5*(j+2),'+)'
C-    call hbook1((iii+IH+250+j),a32, 1000, 0., 2000., 0.) !!<--gde zapolnenie?
C- 197  ENDDO
C- 198  enddo
C
C
      DO 250 KHP=1,3
C      
      IHHH = 100000*KHP
C      
C     1C and 2C fit: general histograms
C      
      DO 200 Ig=3,4             ! Cycle over the nb of photons     
C      
      NHYP = Ig-1
      DO 200 Ihyp=1,NHYP        ! Cycle over the hypothesis number

      IH = IHHH + 1000*Ig + 100*Ihyp
C
      CALL HBOOK1(IH+15,'CHIQ2           	 ', 200,0.,50.0,0.)
      CALL HBOOK1(IH+16,'PROPABILITY,CHIQ2	 ', 100,0., 1.0,0.)
      CALL HBOOK1(IH+17,'MASS 2C FIT CHIQ2 lt 100.',2000,0.,2.0,0.)
      CALL HBOOK1(IH+18,'MASS 2C FIT PCHI2 gt 0.06',2000,0.,2.0,0.)
C      
      CALL HBOOK1(IH+19,'PROPABILITY,CHIQ	 ', 100,0.,1.0,0.)
      CALL HBOOK1(IH+20,'PROPABILITY,CHIQ lt 11.5', 100,0.,1.0,0.)
      CALL HBOOK1(IH+21,'MASS 2C FIT CHIQ gt 11.5',2000,0.,2.0,0.)
      CALL HBOOK1(IH+22,'MASS 2C FIT CHIQ lt 11.5',2000,0.,2.0,0.)
      CALL HBOOK1(IH+23,'CHIQ 2C FIT DISTRIB.	 ', 100,0.,25.,0.)
      CALL HBOOK1(IH+24,'T - DISTRIB CHIQ lt 11.5', 100,0.,2.0,0.)
      CALL HBOOK1(IH+25,'M12 DISTRIB CHIQ lt 11.5', 100,0.,1.0,0.)
      CALL HBOOK1(IH+26,'M34 DISTRIB CHIQ lt 11.5', 100,0.,1.0,0.)
      CALL HBOOK1(IH+27,'Z coordinate of inter p',1000,3.E3,4.E3,0.)
      CALL HBOOK1(IH+28,'COS GJ  12  CHIQ lt 11.5', 100,0.,1.0,0.)
      CALL HBOOK1(IH+29,'COS GJ(1+2) CHIQ lt 11.5', 100,-1.,1.,0.)
      CALL HBOOK1(IH+30,'COS GJ  34  CHIQ lt 11.5', 100, 0.,1.,0.)
      CALL HBOOK1(IH+31,'COS GJ(3+4) CHIQ lt 11.5', 100,-1.,1.,0.)
      CALL HBOOK1(IH+32,'MisMas CHIQ lt 11.5    ',  250, 0.,5.,0.)
      CALL HBOOK1(IH+33,'MisMas CHIQ lt 6.25    ',  250, 0.,5.,0.)
      call hbook1(IH+34,'Mas pi0g 1C-fit Etot.gt.3',2000,0.,2.,0.)
      call hbook1(IH+35,'Mas 2pi0 2C-fit Etot.gt.3',2000,0.,2.,0.)
      CALL HBOOK1(IH+37,'ETOT 2C FIT CHIQ lt 11.5', 300,0.,15.,0.)     
      CALL HBOOK1(IH+38,'Z - DISTRIB.CHIQ lt 11.5',1000,3400.,3900.,0.)
      CALL HBOOK1(IH+39,'Best COMBINAT. NUMBER   ',  25,0.,25.,0.)
      CALL HBOOK1(IH+40,'Best COMBINAT. CHIQ<11.5',  25,0.,25.,0.)
C
      CALL HBOOK1(IH+43,'P transverce X	',250,-1.25,1.25,0.)
      CALL HBOOK1(IH+44,'P transverce Y	',250,-1.25,1.25,0.)
      CALL HBOOK1(IH+45,'P transverce	',200, 0.0 ,2.00,0.)
      CALL HBOOK1(IH+46,'MASS 2C FIT CHIQ lt 6.25   ',2000,0., 2.,0.)
      CALL HBOOK1(IH+47,'ETOT 2C FIT CHIQ lt 6.25   ', 300,0.,15.,0.)
      CALL HBOOK1(IH+48,'MASS CHIQ lt 6.25 -T lt 0.3',2000,0., 2.,0.)
      CALL HBOOK1(IH+49,'ETOT CHIQ lt 6.25 -T lt 0.3', 300,0.,15.,0.)
C
      do Ie=0,9
      CALL HBOOK1(IH+50+Ie,'MASS 2C-FIT E gt 1000,...',2000,0.,2.0,0.)
      enddo
Comg- 
      if(Ig.eq.3) then
C
      call hbook1(IH+61,'Epi0g in omg, Etot.gt. 3.0',2000, 0.,10.0,0.)
      call hbook1(IH+62,'Epi0g out omg,Etot.gt. 3.0',2000, 0.,10.0,0.)
      call hbook1(IH+63,'Mpi0g in omg, Etot.gt. 3.0',2000, 0., 2.0,0.)
      call hbook1(IH+64,'Mpi0g out omg,Etot.gt. 3.0',2000, 0., 2.0,0.)      
      call hbook1(IH+65,'Tpi0g in omg, Etot.gt. 3.0', 300,-1., 2.0,0.)
      call hbook1(IH+66,'Tpi0g out omg,Etot.gt. 3.0', 300,-1., 2.0,0.)
      call hbook1(IH+67,'MisMs in omg, Etot.gt. 3.0', 180, 0., 3.6,0.)
      call hbook1(IH+68,'MisMs out omg,Etot.gt. 3.0', 180, 0., 3.6,0.)
      call hbook2(IH+69,'T & MisMs in omg, E.gt.3.0', 180, 0., 3.6,
     &  				              150,-.1, 1.4,0.)
      call hbook2(IH+70,'T & MisMs out omg, E.gt.3.0',180, 0., 3.6,
     &  					      150,-.1, 1.4,0.)
      endif
Cf2-
      if(Ig.eq.4) then
C
      call hbook1(IH+61,'E2pi0 in f2, Etot.gt. 3.0',2000, 0.,10.0,0.)
      call hbook1(IH+62,'E2pi0 out f2,Etot.gt. 3.0',2000, 0.,10.0,0.)
      call hbook1(IH+63,'M2pi0 in f2, Etot.gt. 3.0',2000, 0., 2.0,0.)
      call hbook1(IH+64,'M2pi0 out f2,Etot.gt. 3.0',2000, 0., 2.0,0.)	  
      call hbook1(IH+65,'T2pi0 in f2, Etot.gt. 3.0', 300,-1., 2.0,0.)
      call hbook1(IH+66,'T2pi0 out f2,Etot.gt. 3.0', 300,-1., 2.0,0.)
      call hbook1(IH+67,'MisMs in f2, Etot.gt. 3.0', 180, 0., 3.6,0.)
      call hbook1(IH+68,'MisMs out f2,Etot.gt. 3.0', 180, 0., 3.6,0.)
      call hbook2(IH+69,'T & MisMs in f2, E.gt.3.0', 180, 0., 3.6,
     &  				             150,-.1, 1.4,0.)
      call hbook2(IH+70,'T & MisMs out f2, E.gt.3.0',180, 0., 3.6,
     &  					     150,-.1, 1.4,0.)
CK0-     
      call hbook1(IH+71,'E2pi0 in Ks, Etot.gt. 2.5',2000, 0.,10.0,0.)
      call hbook1(IH+72,'E2pi0 out Ks,Etot.gt. 2.5',2000, 0.,10.0,0.)
      call hbook1(IH+73,'M2pi0 in Ks, Etot.gt. 2.5',2000, 0., 2.0,0.)
      call hbook1(IH+74,'M2pi0 out Ks,Etot.gt. 2.5',2000, 0., 2.0,0.)	
      call hbook1(IH+75,'T2pi0 in Ks, Etot.gt. 2.5', 300,-1., 2.0,0.)
      call hbook1(IH+76,'T2pi0 out Ks,Etot.gt. 2.5', 300,-1., 2.0,0.)
      call hbook1(IH+77,'MisMs in Ks, Etot.gt. 2.5', 180, 0., 3.6,0.)
      call hbook1(IH+78,'MisMs out Ks,Etot.gt. 2.5', 180, 0., 3.6,0.)
      call hbook2(IH+79,'T & MisMs in Ks, E.gt.2.5', 180, 0., 3.6,
     &  				             150,-.1, 1.4,0.)
      call hbook2(IH+80,'T & MisMs out Ks,E.gt.2.5' ,180, 0., 3.6,
     &  					     150,-.1, 1.4,0.)
      endif
C

C-    do j = 1,21
C-       CALL HBOOK1(IH+j,'YM(j)-YCopy(j)',2000,-10.,10.,0.)
C-    enddo
C
  200 CONTINUE
C
C-    IHHH = 100000 !can we delete this ?     ; k
C      
      do 201 Igm=3,4                          ! Cycle over number of photons   
C-    if (Igm.eq.3 .and. khp.gt.2) goto 201   ! continue the cycle ; we don't need the 3d hypothesis for the 3gamma events ; kondr
      nhyp = khp                              ! make it clear ; will work faster....a bit ; kondr
      do Ihyp=1,NHYP                          ! Cycle over the hypothesis number

!kondr++{ !probabilities....
      if (khp.eq.1.or.khp.eq.2.or.khp.eq.3) then ! book them once
 2002 format(A,I2)
      write(base_name,2002)'pure probability of hypothesis ',igm*10+ihyp
      call hbook1(5500+igm*10+ihyp, base_name, 200,0.,1.,0.)             ! probabilities
      write(base_name,2002) 'after cut probability of hyp ',igm*10+ihyp
      call hbook1(5600+igm*10+ihyp, base_name, 200,0.,1.,0.)             ! probabilities after cut
      endif
!kondr--} 

      do it=1,9                               ! Transverce momentum cut
!       IF (KHP.eq.1) NHYP=1                  ! commented, see the 2 lines above; kondr
C 
      IH = IHHH + 10000*it + 1000*Igm + 100*Ihyp
C       
      do j=0,9          ! Total energy:  1.0+j*0.5 GeV
!       if (khp.le.2) then                      ! we don't have 3rd hyp for SA cuts; kondr
!commented ; kondr++{
!       CALL HBOOK1(IH+50+j,'MASS FIT Prob gt 0.06, E cut',2000,0., 2.,0.)
!       CALL HBOOK1(IH+60+j,'ETOT FIT Prob gt 0.06, E cut', 300,0.,15.,0.)
!       CALL HBOOK1(IH+70+j,'Pt   FIT Prob gt 0.06, E cut',1000,0., 2.,0.)
CEvd+
!       CALL HBOOK1(-(IH+50+j),'Mix MASS FIT Prob gt 0.06, E cut',2000,0.,
!      +     2.,0.)
!       CALL HBOOK1(-(IH+60+j),'Mix ETOT FIT Prob gt 0.06, E cut', 300,0.,
!      +     15.,0.)
!       CALL HBOOK1(-(IH+70+j),'Mix Pt   FIT Prob gt 0.06, E cut',1000,0.,
!      +     2.,0.)
CEvd-
!commented ; kondr--} ; see below

!kondr++{
!exactly the same histogramms as above but with normal and understandable names ; format desctiption is below
      if (.NOT. (Igm.eq.4 .and. khp.eq.3 .and. ihyp.ne.khp)) then
 1101   format(A,I1,A,I1,A,I1,A,I4,A,F4.2)
        write(base_name,1101) 'Ef.Mass: ', Igm, 'g, hyp ',
     +      Ihyp, '/', KHP, ',E>', j*500,', Pt cut: ', ptcut(it)        ! name
        call hbook1(IH +50 +j,  base_name,  2000,0.,2.,0.)              ! Eff mass
        write(khist_name,*)'Mix:',base_name                             ! mixing
        call hbook1(-(IH +50 +j),khist_name,2000,0.,2.,0.)

        write(base_name,1101) 'E tot: ', Igm, 'g, hyp ',
     +      Ihyp, '/', KHP, ',E>', j*500,', Pt cut: ', ptcut(it)        ! name
        call hbook1(IH +60 +j,base_name,300,0.,15.,0.)                  ! Etot
        write(khist_name,*)'Mix:',base_name                             ! mixing
        call hbook1(-(IH+60+j),khist_name,300,0.,15.,0.)

        write(base_name,1101) 'Pt: ', Igm, 'g, hyp ',
     +      Ihyp, '/', KHP, ',E>', j*500,', Pt cut: ', ptcut(it)        ! name
        call hbook1(IH +70 +j,base_name,  1000,0.,2.,0.)                ! Pt
        write(khist_name,*)'Mix:',base_name                             ! mixing
        call hbook1(-(IH+70+j),khist_name,1000,0.,2.,0.)
      endif !if (.NOT. (Igm.eq.4 .and. khp.eq.3 .and. ihyp.ne.khp)) 

      
!the same histogramms as above for pi+, p+, k+; format description is below
      if ((Ihyp.eq.1.or.(ihyp.eq.3.and.igm.eq.4)).and.khp.eq.ihyp) then
      do kcheren = 1,2   !3  !we don't need protons
       if (kcheren.eq.1) write(part_name,*)' (pi+ only)'
       if (kcheren.eq.2) write(part_name,*)' (k+ only)'
       if (kcheren.eq.3) write(part_name,*)' (p+ only)' !left just in case
 1100  format(A,I1,A,I1,A,I1,A,I4,A,F4.2,A)
       write(base_name,1100) 'Ef.Mass:', Igm, 'g,hyp ',
     +    Ihyp, '/', KHP, ',E>', j*500,', Pt cut: ', ptcut(it),part_name
       call hbook1(10000000*kcheren +IH +50 +j,base_name, 2000,0.,2.,0.)
       if (igm.eq.3.or.(igm.eq.4.and.ihyp.eq.1.and.kcheren.eq.1)) then
       write(khist_name,*)'Mix:',base_name                              ! mixing
       call hbook1(-(10000000*kcheren+IH+50+j),khist_name,2000,0.,2.,0.)
       endif

       write(base_name,1100) 'E tot:', Igm, 'g,hyp ',
     +    Ihyp, '/', KHP, ',E>', j*500,', Pt cut: ', ptcut(it),part_name
       call hbook1(10000000*kcheren +IH +60 +j,base_name, 300,0.,15.,0.)
!      write(khist_name,*)'Mix:',base_name
!      call hbook1(-(10000000*kcheren+IH+60+j),khist_name,300,0.,15.,0.)

       write(base_name,1100) 'Pt:', Igm, 'g,hyp ',
     +    Ihyp, '/', KHP, ',E>', j*500,', Pt cut: ', ptcut(it),part_name
       call hbook1(10000000*kcheren +IH +70 +j,base_name, 1000,0.,2.,0.)
!        write(khist_name,*)'Mix:',base_name
!        call hbook1(-(10000000*kcheren+IH+70+j),khist_name,1000,0.,2.,0.)
      enddo
      endif !if ((Ihyp.eq.1.or.(ihyp.eq.3.and.igm.eq.4)).and.khp.eq.ihyp)
     
!the same histogramms as above for another probability selection; format desctiption is below
      if (Igm.eq.3.and.ihyp.eq.2) then  !we don't need all of them , leave eta+gamma here 
!  1101   format(A,I1,A,I1,A,I1,A,I4,A,I1)
        write(base_name,1101) 'Ef.Mass: ', Igm, 'g,hyp ',
     +        Ihyp, '/', KHP, ',E>', j*500,', Pt cut: ', ptcut(it)      ! name
        call hbook1(  1000000 +IH +50 +j,  base_name,2000,0.,2.,0.)
        write(khist_name,*)'Mix: ',base_name                            ! mixing
        call hbook1(-(1000000 +IH +50 +j),khist_name,2000,0.,2.,0.)

        write(base_name,1101) 'E tot: ', Igm, 'g,hyp ',
     +        Ihyp, '/', KHP, ',E>', j*500,', Pt cut: ', ptcut(it)      ! name
        call hbook1(1000000 +IH +60 +j,base_name, 300,0.,15.,0.)
        write(khist_name,*)'Mix:',base_name                             ! mixing
        call hbook1(-(1000000+IH+60+j),khist_name,300,0.,15.,0.)

        write(base_name,1101) 'Pt: ', Igm, 'g,hyp ',
     +        Ihyp, '/', KHP, ',E>', j*500,', Pt cut: ', ptcut(it)      ! name
        call hbook1(1000000 +IH +70 +j,base_name,   1000,0.,2.,0.)
        write(khist_name,*)'Mix:',base_name                             ! mixing
        call hbook1(-(1000000 +IH +70 +j),khist_name,1000,0.,2.,0.)


!         kcheren = 1
!         if (kcheren.eq.1) write(part_name,*)' (pi+ only)'
!  1100   format(A,I1,A,I1,A,I1,A,I4,A,F4.2,A)
!         write(base_name,1100) 'Ef.Mass:', Igm, 'g,hyp ',Ihyp, '/', 
!      +      KHP, ',E>', j*500,', Pt cut: ', ptcut(it),part_name
!         call hbook1(10000000*kcheren + 1000000+IH+50+j,base_name,
!      +                                                    2000,0.,2.,0.)
!         write(khist_name,*)'Mix:',base_name                             ! mixing
!         call hbook1(-(10000000*kcheren+1000000+IH+50+j),khist_name,
!      +                                                    2000,0.,2.,0.)
! 
! !        write(base_name,1100) 'E tot:', Igm, 'g,hyp ',
! !      +    Ihyp, '/', KHP, ',E>', j*500,', Pt cut: ', ptcut(it),part_name
! !        call hbook1(10000000*kcheren +IH +60 +j,base_name,300,0.,15.,0.)
! !        write(khist_name,*)'Mix:',base_name
! !        call hbook1(-(10000000*kcheren+IH+60+j),khist_name,300,0.,15.,0.)
! ! 
! !        write(base_name,1100) 'Pt:', Igm, 'g,hyp ',
! !      +    Ihyp, '/', KHP, ',E>', j*500,', Pt cut: ', ptcut(it),part_name
! !        call hbook1(10000000*kcheren +IH +70 +j,base_name,1000,0.,2.,0.)
! !        write(khist_name,*)'Mix:',base_name
! !        call hbook1(-(10000000*kcheren+IH+70+j),khist_name,1000,0.,2.,0.)
      endif

!format description:
!typical histogramm name: 20274158, as example, 
!number -> meaning, other possibilities
!2 -> k+only, 1->pi+only, 3->p only (1-3)
!0 -> probability cuts by SA, 1 by me (0-1)
!2 -> number of hypothesis which were considered (1-3)
!7 -> Pt cut by SA (1-9)
!4 -> Number of photons (or gammas) (3-4)
!1 -> Number of hypothesis  (1-3)
!5 -> Ef.mass, 6->Energy, 7->Pt
!8 -> Energy Cut (1-9)

!kondr--}

      enddo  !energy cut cycle
      enddo  !pt cut
      enddo  !hyp cycle
  201 enddo  !ngam cut

  250 continue      

!kondr++{
      call hbook2(5590+1, 'Correlation between 3g-hypothesa (no cut): 
     + 1vs2;1;2' ,200,0.,1.,200,0.,1.,0.)             
C-    call hbook2(5590+2, 'Correlation between 4g-hypothesa (no cut): 
C-   + 1vs2;1;2',200,0.,1.,200,0.,1.,0.)	     
C-    call hbook2(5590+3, 'Correlation between 4g-hypothesa (no cut): 
C-   + 1vs3;1;3',200,0.,1.,200,0.,1.,0.)	     
C-    call hbook2(5590+4, 'Correlation between 4g-hypothesa (no cut): 
C-   + 2vs3;2;3',200,0.,1.,200,0.,1.,0.)	
C
C-    call hbook2(13112, 'Correlation between 3g-hypothesa, after cut
C-   + for 1: 1vs2;1;2' ,200,0.,1.,200,0.,1.,0.)	     
C-    call hbook2(13212, 'Correlation between 3g-hypothesa, after cut
C-   + for 2: 1vs2;1;2' ,200,0.,1.,200,0.,1.,0.)	     
C-    call hbook2(14112, 'Correlation between 4g-hypothesa, after cut
C-   + for 1: 1vs2;1;2' ,200,0.,1.,200,0.,1.,0.)	     
C-    call hbook2(14113, 'Correlation between 4g-hypothesa, after cut
C-   + for 1: 1vs3;1;3' ,200,0.,1.,200,0.,1.,0.)	     
C-    call hbook2(14212, 'Correlation between 4g-hypothesa, after cut
C-   + for 2: 1vs2;1;2' ,200,0.,1.,200,0.,1.,0.)	     
C-    call hbook2(14223, 'Correlation between 4g-hypothesa, after cut
C-   + for 2: 2vs3;2;3' ,200,0.,1.,200,0.,1.,0.)	     
C-    call hbook2(14313, 'Correlation between 4g-hypothesa, after cut
C-   + for 3: 1vs3;1;3' ,200,0.,1.,200,0.,1.,0.)	     
C-    call hbook2(14323, 'Correlation between 4g-hypothesa, after cut
C-   + for 3: 2vs3;2;3' ,200,0.,1.,200,0.,1.,0.)	     

!kondr--}

C
Cmike
c      write(a32,*)'Eff mass 2C fit, Etot fit(pi+)','(',500*(2),'+)'
c      call hbook1(IH+100,a32, 1000, 0., 2000., 0.)
c      write(a32,*)'Eff mass 2C fit, Etot fit(K+) ','(',500*(2),'+)'
c      call hbook1(IH+150,a32, 1000, 0., 2000., 0.)
c      write(a32,*)'Eff mass 2C fit, Etot fit(p+) ','(',500*(2),'+)'
c      call hbook1(IH+200,a32, 1000, 0., 2000., 0.)
c      write(a32,*)'Smeshannaya massa iz raznyh sobytiy ',
c     &                     '(',0.5*(2),'+)'
c      call hbook1(       IH+250+j,a32, 1000, 0., 2000., 0.)
c      call hbook1(100000+IH+250+j,a32, 1000, 0., 2000., 0.)
c      call hbook1(200000+IH+250+j,a32, 1000, 0., 2000., 0.)
c      call hbook1(300000+IH+250+j,a32, 1000, 0., 2000., 0.)
C
C  199 enddo
C  200 CONTINUE
      RETURN
      END
C
      SUBROUTINE PTEVNT(XMCR,ECOR,TEP,TRAN)
      COMMON/EVTDST/ IAD1,IAD2,LNGEV,IADFC,IADGM,IADGS,IADFT,IREC(4096)
      INTEGER*2 IREC
      COMMON /PHYS  / IBEG(3),ITARG(2),IREGO(6),MR(2),ITDC(8),IOH(2),
     +                INH(2),IBADFL,IREGA,IREGF5
      COMMON /TARGMS/ IBTG,AM0,SL,R0,T0,ERNT,DSZZ
      COMMON /GAMMAG/ NGAM,PBM(5),E(25),X(25),Y(25),CHI(25)
      COMMON /GEOMTR/ ZGMS,ZHOD(4),XCHD(4),YCHD(4),XBEM,YBEM,ZTRG,ZFIX
C
      write(*,*)(IREC(I),I=IAD1,IAD1+3)
      write(*,*) NGAM,XMCR,ECOR,TEP,TRAN
C
      write(*,*) XBEM,YBEM,ZTRG,ZGMS
      write(*,*) IBTG,AM0,SL,R0,T0,ERNT,DSZZ
      write(*,*) ITARG
      DO 100 I=1,NGAM
  100 write(*,*) I,X(I),Y(I),E(I),CHI(I)
      RETURN
      END
C
      SUBROUTINE MASFT4(MGAM,PM,TENR,L2PI)
      COMMON/MEMBS4/ NBCM,MHYP(6,7)
      COMMON/COMBN4/ NCMB,NHYF,MCMH(6)
      BYTE           K1,K2,K3,K4,MCMC(4),MCMB(4,6)
      EQUIVALENCE   (K1,MCMC(1)),(K2,MCMC(2)),(K3,MCMC(3)),(K4,MCMC(4)),
     +              (MCMH,MCMB ),(MCMC,MCI4)
C
      DIMENSION PM(5,25),P12(5),PRMS(4,4)
      LOGICAL*1  LPIR(4,4),LETR(4,4),LGPI(4,4),LGET(4,4),LGEP(4,4)
      LOGICAL*1  L2PI,LEPI
C
C      DATA NBCM,MCMB  /  6,
C     1	 1,2,3,4,  1,3,2,4,  1,4,2,3,  3,4,1,2,  2,4,1,3,  2,3,1,4 /
C
      DATA      PIRL,PIRG /         .095 ,  .175  /
      DATA      ETRL,ETRG /         .500 ,  .600  /
      DATA PIMS,PILT,PIGT / .13496, .030 ,  .240  /
      DATA ETMS,ETLT,ETGT / .54880, .400 ,  .700  /
      DATA EPMS,EPLT,EPGT / .95757, .700 , 1.200  /
C
      do i=1,6
      do j=1,7
      MHYP(i,j) = 0
      enddo
      enddo
C
      TENQ  =TENR*TENR
      DO 390 N= 2,MGAM
      DO 390 N2=1,N-1
      DO 380 I =1,4
  380 P12(I)   = PM (I,N) + PM (I,N2)
      QMMS=TENQ*(P12(4)**2- P12(1)*P12(1)-P12(2)*P12(2)-P12(3)*P12(3))
      AM12     = SQRT(ABS(QMMS))
      PRMS(N2,N) = AM12
      LPIR(N2,N) = AM12.GT.PIRL.AND.AM12.LT.PIRG
      LETR(N2,N) = AM12.GT.ETRL.AND.AM12.LT.ETRG
      LGPI(N2,N) = AM12.LT.PILT. OR.AM12.GT.PIGT
      LGET(N2,N) = AM12.LT.ETLT. OR.AM12.GT.ETGT
  390 LGEP(N2,N) = AM12.LT.EPLT. OR.AM12.GT.EPGT
C
      L2PI =.FALSE.
      LEPI =.FALSE.
      DO 450 ICMB=1,NBCM
      MCI4 = MCMH(ICMB)
C
C  ** HYP 1 :  PI-P -> PI0+PI0+N  **
C
      IF  (ICMB.GT. 3) GO TO 392
      IF  (LPIR(K1,K2).AND.LPIR(K3,K4)) L2PI =.TRUE.
      IF  (LGPI(K1,K2).OR .LGPI(K3,K4)) GO TO 391
      MHYP(ICMB,1) = ICMB
C
C  ** HYP 4 :  PI-P -> ETA+ETA+N  **
C
  391 IF  (LGET(K1,K2).OR.LGET(K3,K4))  GO TO 392
      MHYP(ICMB,4) = ICMB
C
C  ** HYP 6 :  PI-P -> PI0+GG +N  **
C
  392 IF  (LGPI(K1,K2))                 GO TO 395
      MHYP(ICMB,6) = ICMB
C
C  ** HYP 2 :  PI-P -> PI0+ETA+N  **
C
      IF  (LGET(K3,K4))                 GO TO 393
      IF  (LPIR(K1,K2).AND.LETR(K3,K4)) LEPI =.TRUE.
      MHYP(ICMB,2) = ICMB
C
C  ** HYP 3 :  PI-P -> PI0+ETP+N  **
C
  393 IF  (LGEP(K3,K4))                GO TO 395
      MHYP(ICMB,3) = ICMB
C
C  ** HYP 7 :  PI-P -> ETA+GG +N  **
C
  395 IF  (LGET(K1,K2))                GO TO 450
      MHYP(ICMB,7) = ICMB
C
C  ** HYP 5 :  PI-P -> ETA+ETP+N  **
C
      IF  (LGEP(K3,K4))                GO TO 450
      MHYP(ICMB,5) = ICMB
  450 CONTINUE
C
      NHYF=NHYP
      IF ( L2PI.OR.LEPI ) NHYF= 2
      RETURN
      END
C
      SUBROUTINE KINFIT(MGAM,XMAS,IDMR,CHIH)
      COMMON /GAMMAG/ NGAM,PBM(5),E(25),X(25),Y(25),CHI(25)
      COMMON /GEOMTR/ ZGMS,ZHOD(4),XCHD(4),YCHD(4),XBEM,YBEM,ZTRG,ZFIX
      COMMON /TARGMS/ IBTG,AM0,SL,R0,T0,ERNT,DSZZ
      COMMON /CONSTS/ BEAM,ANOR,ANORT,DELTA,CELL,DSST,NCNX,NCNY,STCO
      COMMON /TYPEVN/ NHYP,MTYP,MYEV,MXHD,MYHD
      COMMON /ERRORS/ IFIT,DHD0,DHD1,DHD2,DCS0,DCS1,DCS2,DSBM,DSXY,C,D,B
      COMMON /ERRORG/ GM(24),GMV(24)
      REAL*8   YM(24),GM,GMV
C
      iBU = 0
 777  IF(MGAM.NE.3.and.MGAM.NE.4) RETURN
C
      YM(1)  = BEAM
      YM(2)  = 0.
      YM(3)  = 0.
      YM(4)  = 0.
      YM(5)  = 0.
      YM(6)  = ZGMS
C
      GMV(1) = DSBM
      GMV(2) = DCS0
      GMV(3) = DCS0
      GMV(4) = DHD0
      GMV(5) = DHD0
      GMV(6) = DSZZ*.25 
C-      write(*,*) (YM(i) ,i=1,6)
C-      write(*,*) (sqrt(GMV(i)),i=1,6)
C
      DO 710 N=1,MGAM
      LM =3*(N+1)
      YM(LM+1)  = X(N)
      YM(LM+2)  = Y(N)
      YM(LM+3)  = E(N)
C
      SGEN      = C + D/SQRT(E(N)) + B/E(N)
      GMV(LM+1) = DSXY/(E(N)*E(N))
      GMV(LM+2) = GMV(LM+1)
      GMV(LM+3) =(SGEN*E(N))**2
C-      write(*,*) N,YM(LM+1),YM(LM+2),YM(LM+3)
C-      write(*,*) N,sqrt(GMV(LM+1)),sqrt(GMV(LM+2)),sqrt(GMV(LM+3))
  710 CONTINUE
C
      DO 720 I=1,3*(MGAM+2)
        GM(I) = 1.0/GMV(I)
  720 CONTINUE
C
      CHIH = -1.
C-    if(iBU.eq.1) goto 888               ! Sdv: this C-omment prevents  CALL PI0FITbu
      call cherenkovs                     ! fill histos for different cherenkovs counters; kondr
      CALL PI0FIT(YM,MGAM,XMAS,CHIH,IDMR)
      RETURN                              ! Sdv: this operator prevents  CALL PI0FITbu
C      
      iBU = 1
      goto 777
C
 888  CALL PI0FITbu(YM,MGAM,XMAS,CHIH,IDMR)
      RETURN
      END
C
      SUBROUTINE FITPR(YM,GM,GMV,NF,NY)
C         **** PIERAZZINI ****** 16/6/1968.......     ORSAY...
C              CORRECTED         17/7/1983   BY  SADOVSKY.S
C* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C         YM       =   PARAMETRI MISURATI...                         | measured parameters
C         GM       = MATRICE DEI PESI MISURATI.                      | matrix of weights
C         GMV      =  (GM)**-1                                       | 
C         DY(10)   = VETTORE DI CORREZIONE ALLE YM...                | vector of corrections for YM
C         NEV      =   NUMERO   EVENTO.                              | event number
C         NER      = INDICE ERRORE.                                  | ?
C         NSTEP    =  MASSIMO NUMERO DI ITERAZIONI PERMASSE.         | maximum number of iterations
C         ITIP     =   TIPO   EVENTO                                 | event type
C         NGTOT    = NUMERO DI GAMMA  NELL  EVENTO IN CONSIDERAZIONE.| number of fotons?
C         YC(20)   =   PARAMETRI FITTATI.                            | fitting parameters
C        B(20,8)   = MATRICE DELLE DERIVATE  DF/DY                   | MATRIX OF DERIVATIVES DF/DY
C         F(10)    =  EQUAZIONI AI VINCOLI.                          | ?
C              -------       DATA          ------
C        CFRCH     = STEP MINIMO IN CHIQ..                           | minimum step of chiq changing
C        CFRF(NF)  = PRECISIONE SUI VINCOLI DATA IN MEV....          | CONSTRAINTS ON PRECISION (MeV)
C         NSTMP    =  NUMERO  DI STAMPE ......                       | NUMBER OF PRINTS
C* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C
      IMPLICIT  REAL*8 (A-H,O-Z)
      DIMENSION YM(24),DY(24),FL(6),CFRF(6)
      DIMENSION GM(24),GMV(24),GBV(6,6),GBT(24,6)
      COMMON/FISICI/ISPILL,NEV,IEV,NGAM,NHIP,IFIT
      COMMON/FISICD/F(6),B(24,6),DXRD(6),DYRD(6),
     +      YC(24),CT(6),CT2(15),CHIQ,EB,ET,ECT,S,T,EFMQ,
     +            EFM12Q,EFM34Q,EFM56Q,EFM1234Q,RNCMB,EMQPR(15)
      REAL*8 EFMQ,EFM12Q,EFM34Q,EFM56Q,EFM1234Q,RNCMB,EMQPR,YC
      EQUIVALENCE  (IFIT,NER),(NPAS,NEV)
      DATA CFRF,CFRCH  / 6*.0001, .1 /
      DATA NSTEP,NSTMP / 14,       10 /
      LOGICAL LHIP,LBAD
C
      LHIP= NHIP.LT.11
      LBAD=.TRUE.
C  ---------------------
C-    write(*,*) 'IEV,NHIP=',IEV,NHIP

      NER  = 0
      NPAS = 0
      N102 = 0
      CHIQ=-1.E+6
      DO 10 I=1,NY
      DY(I)=YC(I)-YM(I)
      if(dy(i).ne.0.) write(*,*) dy
   10 CONTINUE   
C
C-    CALL DERIVA
      IF (     LHIP) CALL FISICA(1)
C      
CK-   IF (.NOT.LHIP) CALL FISICK(1)
C
      IF ( IEV.LT.NSTMP)  WRITE (*,1000) IEV,NPAS,NER,NGAM,NHIP,CHIQ,
     +EB,ET,ECT,S,T,EFMQ,EFM12Q,EFM34Q,EFM56Q,EFM1234Q,EFM1234Q,YC,F
 1000 FORMAT(/2X,5I5/(6(1X,E12.5)))
C
C  -- CALCULATION:  GBV = B*GMV*BT  --
C
    2 CHPP=CHIQ
      DO 41 I=1,NF
      DO 40 K=1,NY
   40 GBT(K,I)=GMV(K)*B(K,I)
      DO 41 J=1,I
      AUX=0.
      DO 43 K=1,NY
   43 AUX=AUX+B(K,J)*GBT(K,I)
      GBV(J,I)=AUX
   41 GBV(I,J)=AUX
C
C  -- FINDING SOLUTION OF EQUATION:  GBV*FL = F --
C
      CALL MASOL(NF,GBV,NIND,F,FL)
      IF (NIND-2) 45, 91, 91
C
C  -- CORRECTION CALCULATION:  DY=-GMV*BT*FL --
C
   45 CHIQ=0.
      DO 60 I=1,NY
      DDY  =0.
      DO 55 K=1,NF
      if (abs(FL(K)).gt.1.E30) go to 92 
   55 DDY  =DDY  - GBT(I,K)*FL(K)

      AUX  =DY(I)+DDY
      DY(I)=AUX
cmike
      call hf1(4500+I,real(AUX*AUX*GM(I)),1.)
cmike
      CHIQ =CHIQ + AUX*AUX*GM(I)
   60 YC(I)=YM(I)+ AUX
C
      IF(CHIQ.GT.2500.) GO TO 92
   65 NPAS=NPAS+1
      IF(NPAS-NSTEP )  70,70,93
   70 IF (     LHIP ) CALL FISICA(1)
CK-   IF (.NOT.LHIP ) CALL FISICK(1)
      IF ( IEV.LT.NSTMP) WRITE (*,1000) IEV,NPAS,NER,NGAM,NHIP,CHIQ,
     + EB,ET,ECT,S,T,EFMQ,EFM12Q,EFM34Q,EFM56Q,EFM1234Q,EFM1234Q,YC,F
      IF (NER .NE.0 ) RETURN
C
      IF (ABS(CHIQ-CHPP)-CFRCH) 80,80,2
   80 DO 90 I=1,NF
      IF (ABS(F(I)) -  CFRF(I)) 90,90,2
   90 CONTINUE
      IF (EFMQ.LT..0000001)    GO TO 94
      NER=0
      RETURN
C
   91 NER=1
      RETURN
C
   92 NER=2
      RETURN
C
   93 NER=3
      RETURN
C
   94 NER=4
      RETURN
C
C-  102 CONTINUE
      N102 = N102 + 1
      NHIL = 0
      DO 160 I=1,NY
      CHIL = DY(I)*DY(I)*GM(I)
      IF (CHIL.LT.30.) GO TO 160
      NHIL = NHIL+1
      DDY  = 0.
      DO 155 K=1,NF
  155 DDY  = DDY  - GBT(I,K)*FL(K)
C     DDY  = DDY*.75
      DDY  = DDY*.90
      DY(I)= DY(I)- DDY
      YC(I)= YM(I)- DDY
C     CHIQ = CHIQ - CHIL*.9375
      CHIQ = CHIQ - CHIL*.9900
  160 CONTINUE
      IF(NHIL.GT.6.AND.N102.GT.2) GO TO 92
      GO TO 65
      END
C
      SUBROUTINE MASOL(N,H,NIND,RHSV,HINV)
      IMPLICIT  REAL*8(A-H,O-Z)
      DIMENSION RHSV(N),HINV(N)
      DIMENSION H(6,6)
      DO 1 I=1,N
    1 HINV(I)=RHSV(I)
      NIND=1
      I=1
   22 J=I
      AMAS= ABS(H(I,J))
      IND=J+1
      IF(IND-N)69,69,70
   69 DO 8 K=IND,N
      A= ABS(H(I,K))
      IF(A-AMAS)8,8,3
    3 AMAS=A
      J=K
    8 CONTINUE
   70 IF(AMAS*0.0001)7,7,5
    7 NIND=2
      GO TO 21
    5 IF(I-J)9,10,9
    9 DO 11 L=1,N
      A=H(L,I)
      H(L,I)=H(L,J)
   11 H(L,J)=A
      A=HINV(I)
      HINV(I)=HINV(J)
      HINV(J)=A
   10 A=1./H(I,I)
      DO 12 L=1,N
   12 H(L,I) =A*H(L,I)
      HAUX   =A*HINV(I)
      HINV(I)=HAUX
      DO 15 M=1,N
      IF(M-I)16,15,16
   16 A=H(I,M)
      DO 18 L=I,N
   18 H(L,M)=H(L,M)-A*H(L,I)
      AR=A * HAUX
      HINV(M)=HINV(M) - AR
   15 CONTINUE
      IF(I-N)20,21,21
   20 I=I+1
      GO TO 22
   21 RETURN
      END
C
      SUBROUTINE FITPL(YM,GM,GMV,NF,NY)
C         **** PIERAZZINI ****** 16/6/1968.......     ORSAY...
C              CORRECTED         17/7/1983   BY  SADOVSKY.S
C* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C         YM       =   PARAMETRI MISURATI...
C         GM       = MATRICE DEI PESI MISURATI.
C         GMV      =  (GM)**-1
C         DY(10)   = VETTORE DI CORREZIONE ALLE YM...
C         NEV      =   NUMERO   EVENTO.
C         NER      = INDICE ERRORE.
C         NSTEP    =  MASSIMO NUMERO DI ITERAZIONI PERMASSE.
C         ITIP     =   TIPO   EVENTO
C         NGTOT    = NUMERO DI GAMMA  NELL  EVENTO IN CONSIDERAZIONE.
C         YC(20)   =   PARAMETRI FITTATI.
C        B(20,8)   = MATRICE DELLE DERIVATE  DF/DY
C         F(10)    =  EQUAZIONI AI VINCOLI.
C              -------       DATA          ------
C        CFRCH     = STEP MINIMO IN CHIQ..
C        CFRF(NF)  = PRECISIONE SUI VINCOLI DATA IN MEV....
C         NSTMP    =  NUMERO  DI STAMPE ......
C* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C
      IMPLICIT  REAL*8 (A-H,O-Z)
      DIMENSION YM(24),DY(24),FL(6),CFRF(6),GBM(36),GBB(144),GB(144)
      DIMENSION GM(24),GMV(24),GBV(6,6),GBT(24,6),BCY(24,6),BCM(144)
      COMMON/POOLM/ DVY(576)
      COMMON/FISICI/ISPILL,NEV,IEV,NGAM,NHIP,IFIT
      COMMON/FISICD/F(6),B(24,6),DXRD(6),DYRD(6),
     +      YC(24),CT(6),CT2(15),CHIQ,EB,ET,ECT,S,T,EFMQ,
     +            EFM12Q,EFM34Q,EFM56Q,EFM1234Q,RNCMB,EMQPR(15)
      REAL*8 EFMQ,EFM12Q,EFM34Q,EFM56Q,EFM1234Q,RNCMB,EMQPR,YC
      EQUIVALENCE  (IFIT,NER),(NPAS,NEV),(BCY,BCM),(GBT,GBB)
      equivalence  (BCY(1,1),BCYeq(1)),(B(1,1),Beq(1))
      dimension BCYeq(144),Beq(144) 
      DATA CFRF,CFRCH  / 6*.0001, .1 /
      DATA NSTEP,NSTMP / 14,       5 /
      LOGICAL LHIP,LBAD
C
      LHIP= NHIP.LT.11
      LBAD=.TRUE.
C  ---------------------
      NER  = 0
      NPAS = 0
      N102 = 0
      CHIQ=-1.E+6
      DO 10 I=1,NY
   10 DY(I)=YC(I)-YM(I)
C
C-    CALL DERIVA
      IF (     LHIP) CALL FISICA(1)
CK-   IF (.NOT.LHIP) CALL FISICK(1)       
      IF ( IEV.LT.NSTMP)  WRITE (*,*) IEV,NPAS,NER,NGAM,NHIP,CHIQ,
     + EB,ET,ECT,S,T,EFMQ,EFM12Q,EFM34Q,EFM56Q,EFM1234Q,EFM1234Q,YC,F
 1000 FORMAT(/2X,5I5/(6(1X,E12.5)))
C
C  -- CALCULATION:  GBV = B*GMV*BT  --
C
    2 CHPP=CHIQ
      DO 41 I=1,NF
      DO 40 K=1,NY
   40 GBT(K,I)=GMV(K)*B(K,I)
      DO 41 J=1,I
      AUX=0.
      DO 43 K=1,NY
   43 AUX=AUX+B(K,J)*GBT(K,I)
      GBV(J,I)=AUX
   41 GBV(I,J)=AUX
C
C  -- FINDING SOLUTION OF EQUATION:  GBV*FL = F --
C
      CALL MASOL(NF,GBV,NIND,F,FL)
      IF (NIND-2) 45, 91, 91
C
C  -- CORRECTION CALCULATION:  DY=-GMV*BT*FL --
C
   45 CHIQ=0.
      DO 60 I=1,NY
      DDY  =0.
      DO 55 K=1,NF
   55 DDY  =DDY  - GBT(I,K)*FL(K)
      AUX  =DY(I)+DDY
      DY(I)=AUX
      CHIQ =CHIQ + AUX*AUX*GM(I)
C-    AY(I)=AUX
   60 YC(I)=YM(I)+ AUX
C
      IF(CHIQ.GT.2500.) GO TO 92
      do i=1,288
      BCYeq(i) = Beq(i)
      enddo
C-    CALL MOVE(B,BCY,1152)
   65 NPAS=NPAS+1
      IF(NPAS-NSTEP )  70,70,93
   70 IF (     LHIP ) CALL FISICA(1)
CK-   IF (.NOT.LHIP ) CALL FISICK(1)
      IF ( IEV.LT.NSTMP) WRITE (5,1000) IEV,NPAS,NER,NGAM,NHIP,CHIQ,
     + EB,ET,ECT,S,T,EFMQ,EFM12Q,EFM34Q,EFM56Q,EFM1234Q,EFM1234Q,YC,F
      IF (NER .NE.0 ) RETURN
C
      IF (ABS(CHIQ-CHPP)-CFRCH) 80,80,2
   80 DO 81 I=1,NF
      IF (ABS(F(I)) -  CFRF(I)) 81,81,2
   81 CONTINUE
      IF (EFMQ.LT..0000001)    GO TO 94
      NER=0
CPUL- RETURN
      DO 84 I=1,NF
      IK = NY*(I-1)
      DO 82 K=1,NY
   82 GBB(IK+K) = GMV(K)*BCY(K,I)
C
      IJ  = NF*(I-1)
      DO 84 J=1,I
      AUX = 0.
      DO 83 K=1,NY
   83 AUX = AUX+BCY(K,J)*GBB(IK+K)
      JI  = NF*(J-1)
      GBM(IJ+J) = AUX
   84 GBM(JI+I) = AUX
C
      DO 85 K=1,NY
      KI = NF*(K-1)
      DO 85 I=1,NF
   85 GB(KI+I)=GMV(K)*BCY(K,I)
      CALL DSINV(NF,GBM,NF,IFL)
      IF  (IFL.LT.0) GO TO 95
      CALL MXMPY(GBB,GBM,BCM,NY,NF,NF)
      CALL MXMPY(BCM,GB ,DVY,NY,NF,NY)
      RETURN
C
   91 NER=1
      RETURN
C
   92 NER=2
      RETURN
C
   93 NER=3
      RETURN
C
   94 NER=4
      RETURN
C      
   95 NER=5
      RETURN     
C
C-  102 CONTINUE   
      N102 = N102 + 1
      NHIL = 0
      DO 160 I=1,NY
      CHIL = DY(I)*DY(I)*GM(I)
      IF (CHIL.LT.30.) GO TO 160
      NHIL = NHIL+1
      DDY  = 0.
      DO 155 K=1,NF
  155 DDY  = DDY  - GBT(I,K)*FL(K)
C     DDY  = DDY*.75
      DDY  = DDY*.90
      DY(I)= DY(I)- DDY
      YC(I)= YM(I)- DDY
C     CHIQ = CHIQ - CHIL*.9375
      CHIQ = CHIQ - CHIL*.9900
  160 CONTINUE
      IF(NHIL.GT.6.AND.N102.GT.2) GO TO 92
      GO TO 65
      END
C
      SUBROUTINE MXMPY(A,B,C,NY,NF,NX)
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION A(NY,NF),B(NF,NX),C(NY,NX)
C
      DO 10 N=1,NY
      DO 10 M=1,NX
      AB = .0
      DO  5 I=1,NF
    5 AB = AB+A(N,I)*B(I,M)
   10 C(N,M) = AB
      RETURN
      END
C
      SUBROUTINE FISICA(INDEX)
C
C   ROUTINE TO CALCULATE CONSTRAINT FUNCTION AND DERIVATIVES FOR
C   KINEMATICAL FIT OF MANY GAMMA EVENTS IN NICE EXPERIMENT
C
      IMPLICIT REAL*8 (A-H,O-Z)
C
      REAL*8 DER1(6,6),DER2(15,7),DCTDBX(6),DCTDBY(6),
     +       DCTDXV(6) ,DCTDYV(6) ,DCTDZV(6) ,RD(6),DX(6),DY(6),
     +       DT2DXV(15),DT2DYV(15),DT2DZV(15),DT2DXI(15),
     +       DT2DYI(15),DT2DXJ(15),DT2DYJ(15),GP(3,6)
     +      ,E(6),EE(15),DMDX(6),DMDY(6),DMDE(6)
C
      COMMON/FISICI/ISPILL,NEV,IEV,NGAM,NHIP,IFIT
      COMMON/FISICD/F(6),B(24,6),DXRD(6),DYRD(6),
     +      YC(24),CT(6),CT2(15),CHIQ,EB,ET,ECT,S,T,EFMQ,
     +          EFM12Q,EFM34Q,EFM56Q,EFM1234Q,RNCMB,EMQPR(15)
      REAL*8 EFMQ,EFM12Q,EFM34Q,EFM56Q,EFM1234Q,RNCMB,EMQPR,YC
C
      EQUIVALENCE (DX,DCTDBX),(DY,DCTDBY)
      EQUIVALENCE (DER1(1,1),DCTDBX),(DER1(1,2),DCTDBY),(DER1(1,3),
     +    DCTDXV),(DER1(1,4),DCTDYV),(DER1(1,5),DCTDZV),
     +   (DER2(1,1),DT2DXV),(DER2(1,2),DT2DYV),(DER2(1,3),DT2DZV),
     +   (DER2(1,4),DT2DXI),(DER2(1,5),DT2DYI),(DER2(1,6),DT2DXJ),
     +   (DER2(1,7),DCTDYJ)
      EQUIVALENCE (YC(1),P),(YC(2),CBX),(YC(3),CBY),(YC(4),XVRJ),
     +            (YC(5),YVRJ),(YC(6),DZ),(YC(7),GP(1,1))
      COMMON/FISMAS/ QNMS,QLMS,NEUMQ,ELEN,ELNQ,NEUMS,PRM,PRMQ
      REAL       MMQ,QNMS,QLMS,NEUMQ,ELEN,ELNQ,NEUMS,PRM,PRMQ
      dimension    Beq(144)
      equivalence (B(1,1),Beq(1)) 
C
C ------------------------------------------------------------------
C
      DIMENSION SCL(6),EMD(7,3)
C-    DATA EMD/.018219,.018219,.018219,.300153,.300153,.300153,.018219, ! first index - number of hypotesis
C-   +         .018219,.300153,.018219,.300153,.916940,.300153,.300153, ! second index - number of photon pair
C-   +         .018219,.300153,.300153,.916940,.018219,.916940,.916940/ ! EMD = (mass of particle [GeV])^2
C
      DATA EMD/.018219,.300153,.018219,.300153,.300153,.300153,.018219, ! first index - number of hypotesis
     +         .018219,.018219,.300153,.300153,.916940,.300153,.300153, ! second index - number of photon pair
     +         .018219,.018219,.300153,.916940,.018219,.916940,.916940/ ! EMD = (mass of particle [GeV])^2
C
      DATA PIM2 /0.0194826/
C
C-    write(*,*) 'Fisica:',NHIP
C-    write(*,*) 'PRM,NEUMQ=', PRM,NEUMQ

      INDX=INDEX
      IFIT=0
      EB  =SQRT(PIM2+P*P) ! P - beam momentum ! EB = 7.86
C-    S   =PRMQ + PIM2+2.*PRM  *EB            ! S  = 15.68
      S   =NEUMQ+ PIM2+2.*NEUMS*EB 
      DZ2 =DZ*DZ                              ! DZ = YM(6)
      CBZQ=1.-CBX*CBX-CBY*CBY                 ! = 1.
      CBZ =SQRT(ABS(CBZQ))
      CBZDZ=CBZ*DZ ! = DZ
      IJ=0
C
C     CALCULATION OF COS(TT(I)),COS(TT(I,J)) AND DERIVATIVES ON PARAM.
C
      DO 2 I=1,NGAM
      DX(I) = GP(1,I)-XV ! GP(I,J) = YC(6+(j-1)*3+i); 
      DY(I) = GP(2,I)-YV
      DDQ   = DX(I)*DX(I)+DY(I)*DY(I)+DZ2
      RD(I) = 1./SQRT(DDQ)
      CT(I) =(CBX*DX(I)+CBY*DY(I)+CBZDZ)*RD(I)
      DXRD(I)  = DX(I)*RD(I)
      DYRD(I)  = DY(I)*RD(I)
      DCTDXV(I)=-RD(I)*(CBX- DXRD(I)*CT(I))
      DCTDYV(I)=-RD(I)*(CBY- DYRD(I)*CT(I))
      DCTDZV(I)= RD(I)*(CBZ-DZ*RD(I)*CT(I))
C-  4 IF (I.LT.2) GO TO 2
      IF (I.LT.2) GO TO 2  
      DO 1 J=1,I-1
      IJ  = IJ+1
      RIJ = RD(I)*RD(J)
      CT2(IJ)=(DX(I)*DX(J)+DY(I)*DY(J)+DZ2)*RIJ
      DT2DXV(IJ)=CT2(IJ)*(DXRD(I)*RD(I)+DXRD(J)*RD(J))-(DX(I)+DX(J))*RIJ
      DT2DYV(IJ)=CT2(IJ)*(DYRD(I)*RD(I)+DYRD(J)*RD(J))-(DY(I)+DY(J))*RIJ
      DT2DZV(IJ)=-DZ*(CT2(IJ)*(RD(I)*RD(I)+RD(J)*RD(J))-2.*RIJ)
      DT2DXI(IJ)=RD(I)*(DXRD(J)-DXRD(I)*CT2(IJ))
      DT2DYI(IJ)=RD(I)*(DYRD(J)-DYRD(I)*CT2(IJ))
      DT2DXJ(IJ)=RD(J)*(DXRD(I)-DXRD(J)*CT2(IJ))
      DT2DYJ(IJ)=RD(J)*(DYRD(I)-DYRD(J)*CT2(IJ))
    1 CONTINUE
    2 CONTINUE
      ADX=-DZ*CBX/CBZ
      ADY=-DZ*CBY/CBZ
      DO 3 I=1,NGAM
      DCTDBX(I)=(DX(I)+ADX)*RD(I)
    3 DCTDBY(I)=(DY(I)+ADY)*RD(I)
C
C --- EFFECTIVE MASS**2 CALCULATION ---
C
C-    5 CONTINUE
      IJ=0
      EFMQ= 0.
      E(1)= GP(3,1)
      ET  = E(1)
      ECT = E(1)*CT(1)
      DO 110 I=2,NGAM
      E(I)= GP(3,I)
      ET  = ET+E(I)
      ECT = ECT+E(I)*CT(I)
      DO 110 J=1,I-1
      IJ=IJ+1
      EE(IJ)=E(I)*E(J)
      EMQPR(IJ) = 2.*EE(IJ)*(1.-CT2(IJ))
  110 EFMQ      = EFMQ+EMQPR(IJ)
C
C --- KINEMATICAL PARAMETERS ---
C
      T  = EFMQ+PIM2-2.*(EB*ET-P*ECT)
C-    MMQ= T+PRMQ +2.*(EB-ET)*PRM
      MMQ= T+NEUMQ+2.*(EB-ET)*NEUMS
C
C--- CONSTRAINT MMQ=NEUTR.MASS**2 (PRINCIPLE FIT) AND DERIVATIVES EFMQ ---
C
Ch    F(3)=MMQ/NEUMQ-1.
C     F(3)=MMQ/PRMQ-1. 
C
C --- DERIVATIVES OF EFMQ ---
C
      DMDXV=0.
      DMDYV=0.
      DMDZV=0.
      DO 115 I=1,NGAM
      DMDX(I)=0.
      DMDY(I)=0.
  115 DMDE(I)=0.
      IJ=0
      DO 120 I=2,NGAM
      DO 120 J=1,I-1
      IJ=IJ+1
      SCC=2.*(1-CT2(IJ))
      DMDE(I)=DMDE(I)+E(J)*SCC
      DMDE(J)=DMDE(J)+E(I)*SCC
      EEIJ   =2.*EE(IJ)
      DMDXV=DMDXV-EEIJ*DT2DXV(IJ)
      DMDYV=DMDYV-EEIJ*DT2DYV(IJ)
      DMDZV=DMDZV-EEIJ*DT2DZV(IJ)
      DMDX(I)=DMDX(I)-EEIJ*DT2DXI(IJ)
      DMDX(J)=DMDX(J)-EEIJ*DT2DXJ(IJ)
      DMDY(I)=DMDY(I)-EEIJ*DT2DYI(IJ)
      DMDY(J)=DMDY(J)-EEIJ*DT2DYJ(IJ)
  120 CONTINUE
C
C --- DERIVATIVES OF Third CONSTRAINT FUNCTION
C
      do  jk=1,144
      Beq(jk)=0.
      enddo
C
C     For inclusive reaction this constrain don't need
C       
C-      B(1,3)=2.*(P/EB*(PRM  -ET)+ECT)
C       B(1,3)=2.*(P/EB*(NEUMS-ET)+ECT)
C-      P2=2.*P
C-      DO 125 J=2,6
C-      SUM=0.
C-      DO 124 I=1,NGAM
C-  124 SUM=SUM+E(I)*DER1(I,J-1)
C-  125 B(J,3)=P2*SUM
C-      B(4,3)=B(4,3)+DMDXV
C-      B(5,3)=B(5,3)+DMDYV
C-      B(6,3)=B(6,3)+DMDZV
C-      EIN2=2.*(EB+PRM)
C-      EIN2=2.*(EB+NEUMS)
C-      J=7
C-      DO 126 I=1,NGAM
C-      B(J  ,3)=DMDX(I)-P2*E(I)*DCTDXV(I)
C-      B(J+1,3)=DMDY(I)-P2*E(I)*DCTDYV(I)
C-      B(J+2,3)=DMDE(I)-EIN2+P2*CT(I)
C-  126 J=J+3
C-      SCL(3)=1./NEUMQ
C-      ISTOP =6+3*NGAM
C-      DO 130 I=1,ISTOP
C-  130 B(I,3)=B(I,3)*SCL(3)
C
C --- CONSTRAINT ON GAMMA COUPLING:  1+2=PI0 3+4=PI0 5+6=PI0  OR ONE
C     OF PARTICLES:   ETA , ETAPRIM OR 1+2+5=OMEGA 3+4+6=OMEGA
C
      EFM12Q  = EMQPR( 1)
      EFM34Q  = EMQPR( 6)
      EFM1234Q= EFM12Q+EFM34Q + EMQPR(2)+EMQPR(3)+EMQPR(4)+EMQPR(5)
C
      ISTOP  = 2
      if (NGAM.eq.3) then
                     ISTOP = 1
      SCL(1)   = 1./EMD(NHIP,1)
      F(1)   = EFM12Q*SCL(1)-1.
      endif
C      
      if (NGAM.eq.4) then
                     ISTOP = 2
      SCL(1)  = 1./EMD(NHIP,1)
      SCL(2)  = 1./EMD(NHIP,2)
      F(1)  = EFM12Q*SCL(1)-1.
      F(2)  = EFM34Q*SCL(2)-1.  
      endif  
C       
C-    write(*,*) 'Fisica:',NGAM,NHIP,EMD(NHIP,1),EMD(NHIP,2)
C
      JJ=7
      II=1
      IJ=1
      DO 310 I=1,ISTOP
C
      R =-2.*EE(IJ)*SCL(I)
      R1= 2.*(1.-CT2(IJ))*SCL(I)
C
      B(   4,I)=R*DT2DXV(IJ)
      B(   5,I)=R*DT2DYV(IJ)
      B(   6,I)=R*DT2DZV(IJ)
      B(JJ  ,I)=R*DT2DXJ(IJ)
      B(JJ+1,I)=R*DT2DYJ(IJ)
      B(JJ+2,I)=E(II+1)*R1
      B(JJ+3,I)=R*DT2DXI(IJ)
      B(JJ+4,I)=R*DT2DYI(IJ)
      B(JJ+5,I)=E(II)*R1
      JJ = JJ + 6
      II = II + 2
      IF (I.EQ.1) IJ =  6
      IF (I.EQ.2) IJ = 15
  310 CONTINUE 
      RETURN
      END
C
      SUBROUTINE DERIVA
C
C   ROUTINE TO CALCULATE CONSTRAINT FUNCTION AND DERIVATIVES FOR
C   KINEMATICAL FIT OF MANY GAMMA EVENTS IN NICE EXPERIMENT
C
      IMPLICIT REAL*8 (A-H,O-Z)
C
      REAL*8 DER1(6,6),DER2(15,7),DCTDBX(6),DCTDBY(6),
     +       DCTDXV(6) ,DCTDYV(6) ,DCTDZV(6) ,DX(6),DY(6),
     +       DT2DXV(15),DT2DYV(15),DT2DZV(15),DT2DXI(15),
     +       DT2DYI(15),DT2DXJ(15),GP(3,6)
C
      COMMON/FISICI/ISPILL,NEV,IEV,NGAM,NHIP,IFIT
      COMMON/FISICD/F(6),B(24,6),DXRD(6),DYRD(6),
     +      YC(24),CT(6),CT2(15),CHIQ,EB,ET,ECT,S,T,EFMQ,
     +          EFM12Q,EFM34Q,EFM56Q,EFM1234Q,RNCMB,EMQPR(15)
      REAL*8 EFMQ,EFM12Q,EFM34Q,EFM56Q,EFM1234Q,RNCMB,EMQPR,YC
C
      EQUIVALENCE (DX,DCTDBX),(DY,DCTDBY)
      EQUIVALENCE (DER1(1,1),DCTDBX),(DER1(1,2),DCTDBY),(DER1(1,3),
     +    DCTDXV),(DER1(1,4),DCTDYV),(DER1(1,5),DCTDZV),
     +   (DER2(1,1),DT2DXV),(DER2(1,2),DT2DYV),(DER2(1,3),DT2DZV),
     +   (DER2(1,4),DT2DXI),(DER2(1,5),DT2DYI),(DER2(1,6),DT2DXJ),
     +   (DER2(1,7),DCTDYJ)
      EQUIVALENCE (YC(1),P),(YC(2),CBX),(YC(3),CBY),(YC(4),XVRJ),
     +            (YC(5),YVRJ),(YC(6),DZ),(YC(7),GP(1,1))
      COMMON/FISMAS/ QNMS,QLMS,NEUMQ,ELEN,ELNQ,NEUMS,PRM,PRMQ
      REAL           QNMS,QLMS,NEUMQ,ELEN,ELNQ,NEUMS,PRM,PRMQ
C
C ------------------------------------------------------------------
      REAL*8 FSV(6),BSV(24,6)
C
      DO 20 I=1,24
      YC(I)  = YC(I) + .01
      CALL FISICA(1)
      DO 5 N=1,5
    5 FSV(N) = F(N)
C
      YC (I) = YC(I) - .02
      DO 10 N=1,6
      CALL FISICA(1)
   10 BSV(I,N)=(FSV(N)-F(N))/.02
   20 YC (I) = YC(I) + .01
      CALL FISICA(1)
C
      DO 30 N=1,5
      DO 30 I=1,24
   30 write(*,*) N,I,B(I,N),BSV(I,N)
      RETURN
      END
C
      SUBROUTINE PAIRFS
C
C *** CALCULATION OF COS(TT(I,J)) AND ALL PAIR MASSES(I,J)
C
      IMPLICIT REAL*8 (A-H,O-Z)
C
      REAL*8 RD(6),DX(6),DY(6),E(6),GP(3,6)
C
      COMMON/FISIC/ISPILL,NEV,IEV,NGAM,NHIP,IFIT
      COMMON/FISICD/F(6),B(24,6),DXRD(6),DYRD(6),
     +      YC(24),CT(6),CT2(15),CHIQ,EB,ET,ECT,S,T,EFMQ,
     +         EFM12Q,EFM34Q,EFM56Q,EFM1234Q,RNCMB,EMQPR(15)
      REAL*8   F,B,DXRD,DYRD,YC,CT,CT2,CHIQ,EB,ET,ECT,S,T
      REAL*8   EFMQ,EFM12Q,EFM34Q,EFM56Q,EFM1234Q,RNCMB,EMQPR
C
      EQUIVALENCE (YC(1),P),(YC(2),CBX),(YC(3),CBY),(YC(4),XVRJ),
     +            (YC(5),YVRJ),(YC(6),DZ),(YC(7),GP(1,1))
      COMMON/FISMAS/ QNMS,QLMS,NEUMQ,ELEN,ELNQ,NEUMS,PRM,PRMQ
      REAL           QNMS,QLMS,NEUMQ,ELEN,ELNQ,NEUMS,PRM,PRMQ
C
      IJ=0
      DZ2  =DZ*DZ
      EFMQ =0.
      DO 5I=1,NGAM
      DX(I)=GP(1,I)-XV
      DY(I)=GP(2,I)-YV
      E (I)=GP(3,I)
      RD(I)=1./SQRT((DX(I)**2 + DY(I)**2)+DZ2)
      IF (I.LT.2) GO TO 5
      DO 3 J=1,I-1
      IJ=IJ+1
      CT2  (IJ)=(DX(I)*DX(J)+DY(I)*DY(J)+DZ2)*RD(I)*RD(J)
      EMQPR(IJ)= 2.*E(I)*E(J)*(1.-CT2(IJ))
    3 EFMQ     = EFMQ + EMQPR(IJ)
    5 CONTINUE
C
      EFM12Q  =EMQPR( 1)
      EFM34Q  =EMQPR( 6)
      EFM56Q  =EMQPR(15)
      EFM1234Q=EMQPR( 1) + EMQPR(2) + EMQPR(5) + EMQPR(6)
      RETURN
      END
C
      FUNCTION DOT4(P)
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION P(5)
      DOT4 = P(4)**2 - P(1)**2 - P(2)**2 - P(3)**2
      RETURN
      END
C
      SUBROUTINE TWOONE(ITHREE)
      COMMON /GAMMAG/ NG,PL(5),E(25),X(25),Y(25),CHIG(25)
      COMMON /CONSTS/ BEAM,ANOR,ANORT,DELTA,CELL,DSST,NCNX,NCNY,STCO
      DIMENSION XXX(200),ESV(50),XSV(50),YSV(50),CSV(50),IUN(50)
      EQUIVALENCE (ESV(1),XXX(  1)) , (XSV(1),XXX( 51)),
     +            (YSV(1),XXX(101)) , (CSV(1),XXX(151))
      ITHREE = 0
      NGAMMA = 0
      do  i=1,NG
      ESV(i) = 0.
      XSV(i) = 0.
      YSV(i) = 0.
      CSV(i) = 0.
      IUN(i) = 0
      enddo
C      
      DO 1 J1=1,NG
      IF(IUN(J1).NE.0)             GO TO 1
      DO 2 J2=J1+1,NG
      IF(J2.GT.NG)                 GO TO 2
      IF(IUN(J2).NE.0)             GO TO 2
      IF(J1.EQ.J2)                 GO TO 2
      III      = ICLOS(J1,J2)
      IF(III.EQ.0)                 GO TO 2
C
      DO 3 J3=J1+1,NG
      IF(J3.GT.NG)                 GO TO 3
      IF(IUN(J3).NE.0)             GO TO 3
      IF(J3.EQ.J1)                 GO TO 3
      IF(J3.EQ.J2)                 GO TO 3
      II1     = ICLOS(J1,J3)
      IF(II1.NE.0)                 ITHREE=1
      IF(II1.NE.0)                 RETURN
      II2     = ICLOS(J2,J3)
      IF(II2.NE.0)                 ITHREE=1
      IF(II2.NE.0)                 RETURN
    3 CONTINUE
      NGAMMA  = NGAMMA+1
      ESV(NGAMMA) =  E(J1)+E(J2)
      XSV(NGAMMA) = (E(J1)*X(J1)+E(J2)*X(J2))/ESV(NGAMMA)
      YSV(NGAMMA) = (E(J1)*Y(J1)+E(J2)*Y(J2))/ESV(NGAMMA)
      CSV(NGAMMA) =-(CHIG(J1)+CHIG(J2))/2.
      IUN(J2)  = 1
      RSQ      = XSV(NGAMMA)**2 + YSV(NGAMMA)**2
      RGG      = SQRT(RSQ)
                                   GO TO 1
    2 CONTINUE
      NGAMMA     = NGAMMA+1
      ESV(NGAMMA)= E(J1)
      XSV(NGAMMA)= X(J1)
      YSV(NGAMMA)= Y(J1)
      CSV(NGAMMA)= CHIG(J1)
      RSQ = X(J1)**2 + Y(J1)**2
      RGG = SQRT(RSQ)
    1 CONTINUE
      NG  = NGAMMA
      do i=1,NG
      E(i)   = ESV(i) 
      X(i)   = XSV(i) 
      Y(i)   = YSV(i) 
      CHIG(i)= CSV(i)
      enddo
      RETURN
      END
C
      FUNCTION ICLOS(N1,N2)
      COMMON /GEOMTR/ ZGMS,ZHOD(4),XCHD(4),YCHD(4),XBEM,YBEM,ZTRG,ZFIX
      COMMON /GAMMAG/ NGAM,PBM(5),E(25),X(25),Y(25),CHIG(25)
      LOGICAL LBEG
      DATA CUTR/40./, CUTM/.070/, LBEG/.FALSE./
C
      IF (LBEG) GO TO 1
      CUTRSQ =  CUTR**2
      CUTMSQ =  CUTM**2
      ZGMSSQ =  ZGMS**2
      LBEG   = .TRUE.
C
    1 ICLOS  = 0
      IF   (CHIG(N1).NE.CHIG(N2))              RETURN
      RSQ =(X(N1)-X(N2))**2 + (Y(N1)-Y(N2))**2
      IF   (RSQ.GT.CUTRSQ)                     RETURN
      AMSQ= E(N1)*E(N2)*RSQ/ZGMSSQ
      IF   (AMSQ.GT.CUTMSQ)                    RETURN
      ICLOS=1
      RETURN
      END
C
      SUBROUTINE KILLGAM
      COMMON /GAMMAG/ NGAM,PBM(5),E(25),X(25),Y(25),D(25)
C
      NK  = 0
      DO 1 N=1,NGAM
      K     = ICNTR(N)
      IF (K.NE.0) GO TO 1
      NK   = NK+1
      E(NK)= E(N)
      X(NK)= X(N)
      Y(NK)= Y(N)
      D(NK)= D(N)
    1 CONTINUE
      NGAM = NK
      RETURN
      END
C
      FUNCTION ICNTR(IG)
      COMMON /GAMMAG/ NGAM,PBM(5),E(25),X(25),Y(25),CHIG(25)
      COMMON /CONSTS/ BEAM,ANOR,ANORT,DELTA,CELL,DSST,NCNX,NCNY,STCO
      DATA RGAP,RCNTR,R0    / 26.,  40., 200. /
      DATA ETH ,E0   ,ENOKL /.400, .700, 2.000/
C
      ICNTR = 0
      IF (E(IG).LT.ETH)           GO TO 1
      R     =  X(IG)**2+Y(IG)**2
      R     =  SQRT(R)
      IF (R .LT.RGAP)             GO TO 1
      IF (E(IG).GT.ENOKL) RETURN
      IF (R .LT.RCNRT)            GO TO 1
      IF (E(IG).GT.E0)    RETURN
      IF (R    .GT.R0)    RETURN
      SESQ =(E0 - ETH)**2
      SRSQ =(R0 - RCNTR)**2
      FF   =(E(IG)-E0)**2/SESQ + (R-R0)**2/SRSQ
      IF (FF.LT.1.)       RETURN
    1 ICNTR= 1
      RETURN
      END
C
      SUBROUTINE XYGM(XG,YG)
      COMMON /GAMMAG/ NGAM,PBM(5),E(25),X(25),Y(25),CHI(25)
C
      XP = 0.
      YP = 0.
      EP = 0.
      DO 10 I=1,NGAM
      XP = XP + X(I)*E(I)
      YP = YP + Y(I)*E(I)
   10 EP = EP + E(I)
      XG = XP/EP
      YG = YP/EP
      RETURN
      END
C
      SUBROUTINE PTCOREL
      COMMON /CHANAL/ TFILE,BFILE,LFILE,MFILE,ITP,IBK,LIS,IHM,MHM
      CHARACTER*50    TFILE,BFILE,LFILE,MFILE
      COMMON /CORREL/ MSMX(4),EVGX(4),EV1X(4),EV2X(4),DS1X(4),DS2X(4),
     +                MSMY(4),EVGY(4),EV1Y(4),EV2Y(4),DS1Y(4),DS2Y(4),
     +DSGX(4),DSGY(4),CR1X(4),CR2X(4),CRLX(4),CR1Y(4),CR2Y(4),CRLY(4)
      REAL*8 EVGX,DSGX,EV1X,EV2X,DS1X,DS2X,CR1X,CR2X,CRLX,
     +       EVGY,DSGY,EV1Y,EV2Y,DS1Y,DS2Y,CR1Y,CR2Y,CRLY
C
      OPEN(UNIT=11,STATUS='UNKNOWN',FORM='FORMATTED')
      DO 10 I1=1,4
      I2 =  I1+1
      IF (I2.GT.4) I2=1
C
      WRITE(11,*) 'X',I1,MSMX(I1)
      WRITE(11,*)                 EVGX(I1),EV1X(I1),EV2X(I1),
     2                            DSGX(I1),DS1X(I1),DS2X(I1),
     3                            CR1X(I1),CR2X(I1),CRLX(I1)
      WRITE(11,*) 'Y',I1,MSMY(I1)
      WRITE(11,*)                 EVGY(I1),EV1Y(I1),EV2Y(I1),
     2                            DSGY(I1),DS1Y(I1),DS2Y(I1),
     3                            CR1Y(I1),CR2Y(I1),CRLY(I1)
C
      DGPX = DSGX(I1)-EVGX(I1)**2
      D1PX = DS1X(I1)-EV1X(I1)**2
      D2PX = DS2X(I1)-EV2X(I1)**2
      C1RX =(CR1X(I1)-EV1X(I1)*EVGX(I1))/SQRT(D1PX*DGPX)
      C2RX =(CR2X(I1)-EV2X(I1)*EVGX(I1))/SQRT(D2PX*DGPX)
      C12X =(CRLX(I1)-EV1X(I1)*EV2X(I1))/SQRT(D1PX*D2PX)
C
      DGPY = DSGY(I1)-EVGY(I1)**2
      D1PY = DS1Y(I1)-EV1Y(I1)**2
      D2PY = DS2Y(I1)-EV2Y(I1)**2
      C1RY =(CR1Y(I1)-EV1Y(I1)*EVGY(I1))/SQRT(D1PY*DGPY)
      C2RY =(CR2Y(I1)-EV2Y(I1)*EVGY(I1))/SQRT(D2PY*DGPY)
      C12Y =(CRLY(I1)-EV1Y(I1)*EV2Y(I1))/SQRT(D1PY*D2PY)
C
      write(*,*) 'X ',I1,MSMX(I1)
      write(*,*) EVGX(I1),EV1X(I1),EV2X(I1),
     &           DGPX,D1PX,D2PX,C1RX,C2RX,C12X
      write(*,*) 'Y ',I1,MSMY(I1)
      write(*,*) EVGY(I1),EV1Y(I1),EV2Y(I1),
     &           DGPY,D1PY,D2PY,C1RY,C2RY,C12Y
C
      WRITE(LIS,*) 'X',I1 ,MSMX(I1)
      WRITE(LIS,*)EVGX(I1),EV1X(I1),EV2X(I1),
     +                              DGPX,D1PX,D2PX,C1RX,C2RX,C12X
      WRITE(LIS,*) 'Y',I1 ,MSMY(I1)
      WRITE(LIS,*)EVGY(I1),EV1Y(I1),EV2Y(I1),
     +                              DGPY,D1PY,D2PY,C1RY,C2RY,C12Y
   10 CONTINUE
      CLOSE (UNIT=11)
      RETURN
      END
C
      SUBROUTINE TEST4G(MGAM,PM,TENR)
      COMMON/TARGMS/ IBTG,AM0,SL,R0,T0,ERNT,DSZZ
      COMMON/COMBN4/ NCMB,NHYF,MCMH(6)
      BYTE           K1,K2,K3,K4,MCMC(4),MCMB(4,6)
      EQUIVALENCE   (K1,MCMC(1)),(K2,MCMC(2)),(K3,MCMC(3)),(K4,MCMC(4)),
     +              (MCMH,MCMB ),(MCMC,MCI4)
C
      DIMENSION PM(5,25),P12(5),PRMS(4,4)
      LOGICAL*1 LPIR(4,4)
C-    LOGICAL*1  LE2P,LETR(4,4),LGET(4,4),LGEP(4,4),LGPI(4,4)
C
C-    DATA NBCM,MCMB  /  6,
C-   1     1,2,3,4,  1,3,2,4,  1,4,2,3,  3,4,1,2,  2,4,1,3,  2,3,1,4 /
C
      DATA      PIRL,PIRG /         .095 ,  .175  /
      DATA      ETRL,ETRG /         .500 ,  .600  /
      DATA PIMS,PILT,PIGT / .13496, .030 ,  .240  /
      DATA ETMS,ETLT,ETGT / .54880, .400 ,  .700  /
      DATA EPMS,EPLT,EPGT / .95757, .700 , 1.200  /
C
      TENQ  =TENR*TENR
      DO 390 N= 2,MGAM
      DO 390 N2=1,N-1
      DO 380 I =1,4
  380 P12(I)   = PM (I,N) + PM (I,N2)
      QMMS=TENQ*(P12(4)**2- P12(1)*P12(1)-P12(2)*P12(2)-P12(3)*P12(3))
      AM12     = SQRT(ABS(QMMS))
      PRMS(N2,N) = AM12
  390 LPIR(N2,N) = AM12.GT.PIRL.AND.AM12.LT.PIRG
C
      DO 450 ICMB=1,NBCM
      MCI4 = MCMH(ICMB)
C
C-                      CALL HF1(411, PRMS(K1,K2), 1.)
      IF ( LPIR(K1,K2)) THEN
                        CALL HF1(412, PRMS(K3,K4), 1.)
                        CALL HF1(413, PRMS(K3,K4), 1.)
                        CALL HF1(414, PRMS(K3,K4), 1.)
                        IF(IBTG.EQ.1) THEN
                                      CALL HF1(415, PRMS(K3,K4), 1.)
                                      CALL HF1(416, PRMS(K3,K4), 1.)
                                      ENDIF
                        ENDIF
  450 CONTINUE
      RETURN
      END
