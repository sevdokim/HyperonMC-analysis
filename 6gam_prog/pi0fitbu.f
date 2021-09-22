      SUBROUTINE PI0FITbu(YM,NMES,XMAS,CHHH,IDMR)
C
C   THIS SUBROUTINE USING IN FITING 3&4 gamma EVENTS WHITH ONLY pi-pi HYPOTHISYS.
C
      COMMON/REJECT/ LINC,KLED,ETLT,ETGT,AMMN,XMMN,XMMX,CHEV,CHEM,CHIE
C
      DIMENSION YM(24),YMCY(24),GMVCY(24),GMCY(24),FPAR(72)
     &      ,YCopy(24,6),GCopy(24,6) ! YCopy, GCopy - cmike
      COMMON/ERRORG/ GM(24),GMV(24)
      REAL*8 YM,GM,GMV,YMCY,GMCY,GMVCY,FPAR
      REAL*8 YCopy,GCopy ! cmike
C
      COMMON/TYPEVN/NHYP,MTYP,MYEV,MXHD,MYHD
      COMMON/FISICI/ISPILL,NEV,IEV,NGAM,NHIP,IFIT
      COMMON/FISICD/F(6),B(24,6),DXRD(6),DYRD(6),
     +      YC(24),CT(6),CT2(15),CHIQ,EB,ET,ECT,S,T,EFMQ,
     +         EFM12Q,EFM34Q,EFM56Q,EFM1234Q,RNCMB,EMQPR(15)
      real*8 csmesh,esmesh,msmesh,di1,di2,rij
      REAL*8   F,B,DXRD,DYRD,YC,CT,CT2,CHIQ,EB,ET,ECT,S,T
      REAL*8   EFMQ,EFM12Q,EFM34Q,EFM56Q,EFM1234Q,RNCMB,EMQPR
      REAL*8   ETYM,YCeq(72)
      equivalence (YC(1),YCeq(1)) 
C
      COMMON/FISMAS/ QNMS,QLMS,NEUMQ,ELEN,ELNQ,NEUMS,PRM,PRMQ
      REAL    QNMS,QLMS,NEUMQ,ELEN,ELNQ,PRM,mim
C
      COMMON/EVTFIT/ LBFT,LWAL,LWRT,LNFIT,IBFT(76)
      INTEGER * 2    IBFT
      INTEGER * 4    I4FT(32)
      EQUIVALENCE  ( I4FT,IBFT(13))
C
      COMMON/MEMBS4/ NBCM,MHYP(6,7)
      COMMON/COMBN4/ NCMB,NHYF,MCMH(6)
      BYTE           K1,K2,K3,K4,MCMC(4),MCMB(4,6)
      EQUIVALENCE   (K1,MCMC(1)),(K2,MCMC(2)),(K3,MCMC(3)),(K4,MCMC(4)),
     +              (MCMC,MCI4)
      DATA MCMB/1,2,3,4, 1,3,2,4, 2,3,1,4, 3,4,1,2, 2,4,1,3, 2,3,1,4 /
C
      INTEGER*2 NFHP(16),KFTF(16),KHIP(16),KBCM(16)
      DATA KFTF/ 1, 2,  3, 4,  5, 6,  7, 9, 10,12, 13,14,  8,11, 15,16/
      DATA KHIP/ 1, 2,  3, 4,  5, 6,  7, 8,  9, 9, 11,12, 13,14, 15,16/
      DATA KBCM/ 3, 6,  6, 3,  6, 4,  4, 5,  4, 4,  5, 4,  4, 4,  4, 4/
      DATA NFHP/ 2, 2,  3, 3,  3, 2,  2, 3,  5, 4,  4, 5,  5, 5,  5, 5/
      LOGICAL LK0S
      common /triggers/c1c2c3(3)
      data c1min,c2min/ 200, 130 /    ! c
      real*8 sstore(18)
      data c1min,c2min/ 280, 150 /    ! be
      data c1min,c2min/ 255, 170 /    ! cu3
      data c1min,c2min/ 250, 150 /    ! cu6
      integer j
C -----------------------------------------------------------------------
C 
C-    write(*,*) 'pi0fitbu', NEV,IEV,NGAM
C
      NHYP = 2
      CHIQ = 0.
      NGAM = NMES
      if (NGAM.lt.3.or.NGAM.gt.4) return
      MFIT = 0
      NYFT = 6 + 3*NGAM
      do j = 1,NYFT
       YMCY (j)=YM (j)
       GMCY (j)=GM (j)
       GMVCY(j)=GMV(j)
      enddo
C -------------------------------------
C     
      NHYF = NHYP                     !
      DO 500 KHP=1,NHYF               !
      IHP  = KFTF(KHP)                !  The only 4g -> pi0pi0 at first 
      IF    (IHP.LE.0) GO TO 500      !      and  4g -> pi0Eta at second 
      NF   = NFHP(IHP)                !
      NHIP = KHIP(IHP)  
C      
      if (NGAM.eq.3) then
                     NF=1
	             NHYF=1
            else 
            if (NGAM.eq.4.and.IHP.eq.2.and.XMAS.lt.0.5) go to 500  
      endif
C	    
C-    if (NGAM.eq.4) 
C-   +write(*,*) 'pi0fit: IEV', IEV,NGAM,KHP,IHP,NF,NHIP,KBCM(IHP) 
C
      JFIT  =-1
      MBCOMB= 0
      MBCMB2= 0
      CHIP  = 1.E8
      XMHP  =-1.
C
      LK0S  = NHIP.GE.11
      NEUMQ = QNMS
      IF (LK0S) NEUMQ = QLMS
C
      DO 499 ICOMB=1,KBCM(IHP)
      MCI4 = MCMH(ICOMB)
C
      do  j=1,NYFT
      YM (j)=YMCY (j)
      GM (j)=GMCY (j)
      GMV(j)=GMVCY(j)
      enddo 
C
C-    write(*,*) 'pi0fit: NGAM,IHP,NF,NHIP,NYFT,ICOMB=',
C-   +                    NGAM,IHP,NF,NHIP,NYFT,ICOMB 
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
CK-   IF ( .NOT.LK0S ) GO TO 100
CK-   CALL PAIRFS
CK-   EFM1234 = SQRT(ABS(EFM1234Q))
CK-   YM (5)  = YM(6)*2.*EFM1234
CK-   IF (Z4KS(KAMB).GT.1.) GO TO 95
CK-   YM( 5)  = YM (6)
CK-   GMV(5)  = GMV(6)*4.
CK-   GM (5)  = 1./GMV(5)
CK-   GO TO 100
CK-95 YM (5)  = YM (6)/Z4KS(KAMB)
CK-   GMV(5)  =  1.0E9
CK-   GM (5)  =  1./GMV(5)
C
C-  100 continue

      do j = 1,NYFT
      YC(j)= YM(j)
      YCopy(j,ICOMB) = YM (j)! cmike
      GCopy(j,ICOMB) = GMV(j)! cmike
      enddo
C 
C
      CALL FITPR(YM,GM,GMV,NF,NYFT)
C     write(*,*) JFIT,KHP,CHIQ,CHI0
C
      IF ( JFIT.GE.0 .AND. CHIQ.GT.CHI0 ) GO TO 105
      CHI0   = CHIQ
      JFIT   = IFIT
C
  105 IF ( IFIT  .NE.0 ) GO TO 499
      IF ( MBCOMB.NE.0.AND.CHIQ.GT.CHIP ) GO TO 110
      CHP2   = CHIP
      XMP2   = XMHP
      MBCMB2 = MBCOMB
C
      CHIP   = CHIQ
      XMHP   = SQRT(ABS(EFMQ))
      MBCOMB = ICOMB
      GO TO 112
C
  110 IF ( MBCMB2.NE.0.AND.CHIQ.GT.CHP2 ) GO TO 112
      CHP2   = CHIQ
      XMP2   = SQRT(ABS(EFMQ))
      MBCMB2 = ICOMB
C
  112 IF ( MFIT.NE. 0.AND.CHIQ.GE.FPAR(46)) GO TO 499
C
      MNEV   = NEV
      MFIT   = NHIP
      RNCMB  = ICOMB
      RKOMB  = ICOMB
      do   j=1,72
      FPAR(j)=YCeq(j)
      enddo
C=============================================================================
C
  499 CONTINUE
      CALL HF1( 999, FLOAT(10*(MGAM+NHIP+JFIT)), 1.)
CSdv+
C-    IF (NGAM.eq.4.and.IHP.eq.1.and.CHIQ.lt.11.5) go to 501 
CSdv-      
      IF ( MBCOMB.EQ.0) GO TO 500
C
  500 CONTINUE
c      write(*,*) "ngam =", ngam
C
  501 LNFIT = 12
      IF  (MFIT.EQ.0) RETURN
C
      CHHH = CHIQ
      do  j=1,72
      YCeq(j)=FPAR(j)
      enddo
C
      CALL HF1(IDMR,XMAS,1.)
C      
      IHH = IDMR + 100*MFIT + 20
      EFM = SQRT(ABS(EFMQ))
      CHIR= CHIQ
      PCHI= PROB(CHIR, NF )
C
C
                         CALL HF1( IHH-1, PCHI, 1.)
                         CALL HF1( IHH+3, CHIR, 1.)
C-    IF ( CHIQ.GT.11.5) CALL HF1( IHH+1, EFM , 1.)
C-    IF ( CHIQ.GT.11.5) GO TO 609
      IF ( PCHI.LT.0.10) CALL HF1( IHH+1, EFM , 1.)
      IF ( PCHI.LT.0.10) GO TO 609
                         CALL HF1( IHH  , PCHI, 1.)
Cmike
      if(ngam.eq.4) then
      do j = 1,24
        diff = (YCeq(j)-YCopy(j,int(RNCMB)))/sqrt(GCopy(j,int(RNCMB)))
        call hf1(IHH+40+j,diff,1.)
      enddo
      endif
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
C   CALCULATION COS PI0 IN G.J.S. FOR CASE WITH BEAM PARTICLE EXECTLY ALONG Z !
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
      RCOMB= RNCMB
      RYC6 = YC(6)
      RT   = T
      RTYM = ETYM
      CALL HF1( IHH+ 2, EFM    ,1.)
      CALL HF1( IHH+ 4,-RT     ,1.)
      CALL HF1( IHH+ 5, EFM12  ,1.)
      CALL HF1( IHH+ 6, EFM34  ,1.)
      CALL HF1( IHH+ 8, CSGJ12 ,1.)
      CALL HF1( IHH+ 9, CSGJ1  ,1.)
      CALL HF1( IHH+10, CSGJ34 ,1.)
      CALL HF1( IHH+11, CSGJ2  ,1.)
      CALL HF1( IHH+14, EFM1234,1.)
      CALL HF1( IHH+17, RTYM,   1.)
      CALL HF1( IHH+18, RYC6   ,1.)
      CALL HF1( IHH+19, RCOMB  ,1.)
      CALL HF1( IHH+20, RKOMB  ,1.)
      do i=1,12
         sstore(i)=fpar(i+6)
      enddo
C
C            Energy cuts: 
C     
      if(PCHI.gt.0.1) THEN
      if(efm.gt.0.75.and.efm.lt.0.85) call hf1(5049,rtym,1.)
      if(efm.gt.0.85.and.efm.lt.0.90) call hf1(5050,rtym,1.)
      if(efm.gt.0.70.and.efm.lt.0.75) call hf1(5051,rtym,1.)
         do j =0,9
            Ecut =1.00 + 0.500*j
            if ( ETYM.gt.Ecut) then 
            call HF1(  IHH+30   + j, EFM*1000., 1.) 
         if(c1c2c3(2).gt.c2min.and.c1c2c3(1).lt.c1min) then
c           call hf1(  ihh+30+100+j ,efm*1000.,1.)
            call hf1(-(ihh-20+10 +j),efm*1000.,1.)
         endif
         if(c1c2c3(2).lt.c2min.and.c1c2c3(1).lt.c1min) then
c           call hf1(  ihh+30+150+j ,efm*1000.,1.)
            call hf1(-(ihh-20+20 +j),efm*1000.,1.)
         endif
         if(c1c2c3(2).gt.c2min.and.c1c2c3(1).gt.c1min) then
c           call hf1(  ihh+30+50+j ,efm*1000.,1.)
            call hf1(-(ihh-20+   j),efm*1000.,1.)
         endif
         if(ngam.eq.4)then
            mim=0.
            ij=0
            do i1=2,4
               rij=sstore(i1*3+4)**2+sstore(i1*3+5)**2+fpar(6)**2
               di1=1./sqrt(rij)
               do i2=1,i1-1
                  ij=ij+1
                  rij=sstore(i2*3+4)**2+sstore(i2*3+5)**2+fpar(6)**2
                  di2=1./sqrt(rij)
                  rij=di1*di2
                  csmesh=(sstore(i1*3+4)*sstore(i2*3+4)+
     &                    sstore(i1*3+5)*sstore(i2*3+5)+fpar(6)**2)*rij
                  esmesh=sstore(i1*3+6)*sstore(i2*3+6)
                  msmesh=2.*esmesh*(1.-csmesh)
                  mim=mim+msmesh
               enddo
            enddo
            mim=sqrt(abs(mim))
            call hf1(4350+j,mim*1000.,1.)
c            write(*,*)'2'
         endif
            endif
         enddo
      ENDIF
      if(ngam.eq.4) then
         do i=1,6
            sstore(i+12)=fpar(i+6)
         enddo
      endif
C      
      RTYM = ETYM
      IF ( CHIQ.GT.6.25) GO TO 609
                         CALL HF1(IHH+26, EFM, 1.)
                         CALL HF1(IHH+27, RTYM,1.)
      IF (  -T .LT.0.30) THEN
                         CALL HF1(IHH+28, EFM, 1.)
                         CALL HF1(IHH+29, RTYM,1.)
                         ENDIF
C
  609 CONTINUE
c      write(*,*)'pi0fit 7'
      RETURN
      END
