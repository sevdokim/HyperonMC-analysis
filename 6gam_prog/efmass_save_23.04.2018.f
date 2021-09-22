      SUBROUTINE EFMASS (NPART,IPART,EFMAS,ESUM,OK)
C     **********************************************
C     CALCULATES EFFECTIVE MASS FOR NPART PARTICLES
C     WITH NUMBERS IPART
C     Author:  Nadia Russakovich
c     Updated:
c*****************************************************
      COMMON /ZWALLS/ ZWALL1,ZWALL !cmike
      COMMON /GAMMAS/ NGAM, XGAM(60), YGAM(60), EGAM(60), DCOS(3,60)
      COMMON /LOSSD / LOSSD(70)
      COMMON /MOMENT/ pi0mass,PLAB(3,60)
      common /targandbeam/ beammomentum, targetmass
      common /correlation/ chislitel, znamenatel1, znamenatel2
c      real msrednee
c      common /correlation/ rsrednee,msrednee,nfotonov
      real qqq1,qqq2,etot,energy_2gamma,mm,cosalfa,cos1,cos2,sqrt_efm
      integer I1,I2,iamptot,a,qqq,partts,i,j
      common /etotal/ etot,iamptot
      real myenergy
!       real mixmass
      logical exists
      character*3 mixstatus
      double precision Pc(5),Pccms(5),Pa(5),Pb(5),Pbcms(5)
      real proton_mass, neutron_mass, repulse_energy                    ! ;kondr
!       integer hi_number_2d                                              ! ;kondr

      integer nafnaf_energy, wall_proc,mclus_bad,ef
      common /bad_due_something/ nafnaf_energy, wall_proc,mclus_bad,ef
C
      data proton_mass, neutron_mass /938.272, 939.565/                 ! ;kondr
      data mmin /100.0/, mmax /170.0/          !   All
      data targetmass,beammomentum /939.57,7000.0/
!      data AmKplus,Ampiplus /139.57,493.677/   ! mistake  ;kondr
      data AmKplus,Ampiplus /493.677,139.57/
      common /myen/ myenergy
      DIMENSION IPART(60)
      dimension partts(20)
      LOGICAL OK
      real assimetry1, assimetry2 
      common /triggers/c1c2c3(3), c1min,c2min,c3min
c      data c1min,c2min/ 200, 130 /    ! c
c      data c1min,c2min/ 280, 150 /    ! be
c      data c1min,c2min/ 255, 170 /    ! cu3
c      data c1min,c2min/ 250, 150 /    ! cu6
      common/ptransverce/ ptrans
      common/ptc/ ptcut
      common /t_missmass/ t,missmass
      common /T_Clock/ tclock,iclock,eta
      real missmass,t
      logical eta
cmike
      logical ktf ! <-- kakaya-to fignya na gistogramme -13002
cmike
!kondr++{ !2gamma event mixing    ; (cmix-2gg 2 gamma)
      integer imix
      parameter (imix=100)
      integer ikplus, ipplus, ipiplus                                   !iterators for every beam particle
      integer counter, tempcounter                                      !iterators
      integer energy_slice                                              ! == qqq2 ? for filling sliced histos
      real*8 P1g_temp(4), P2g_temp(4), efm_temp                         !temporary containers for 1st and 2nd gamma 4-momentum ; efm to put in the histogramm
      real*8 P1g_mix(4, imix, 3), P2g_mix(4, imix, 3)                   !containers for mixed events [component(4), number(10), type(k+,p+,pi+)]
      save P1g_temp, P2g_temp, P1g_mix, P2g_mix, ikplus, ipplus, ipiplus
      data ikplus, ipplus, ipiplus /0, 0, 0/
!kondr--}
c
c     write(*,*) 'EFMASS CALLED!'
      ESUM  = 0.
      PXSUM = 0.
      PYSUM = 0.
      PZSUM = 0.
      OK = .FALSE.

!kondr++{ !2gamma event mixing: temporary momentum storage      ; (cmix-2gg 2 gamma)
      if(npart.eq.2)then
C            \/  1st gamma \/                  \/  2nd gamma \/
         P1g_temp(4) = EGAM(ipart(1))  ; P2g_temp(4) = EGAM(ipart(2))
         P1g_temp(1) = PLAB(1,ipart(1)); P2g_temp(1) = PLAB(1,ipart(2))
         P1g_temp(2) = PLAB(2,ipart(1)); P2g_temp(2) = PLAB(2,ipart(2))
         P1g_temp(3) = PLAB(3,ipart(1)); P2g_temp(3) = PLAB(3,ipart(2))
      endif
!kondr--}

      DO 10 I = 1, NPART
         IP = IPART(I)
        ESUM  = ESUM  + EGAM(IP)
        PXSUM = PXSUM + PLAB(1,IP)
        PYSUM = PYSUM + PLAB(2,IP)
        PZSUM = PZSUM + PLAB(3,IP)
        CALL hf1(5033,egam(ip),1.)
  10  CONTINUE
CEvd+
C     Beam 4-momentum in lab
      if((c1c2c3(1).gt.c1min).and.(c1c2c3(2).gt.c2min))then
         Pa(5)=Ampiplus
      else
         Pa(5)=AmKplus
      endif
      Pa(1)=0.
      Pa(2)=0.
      Pa(3)=beammomentum
      Pa(4)=dsqrt(Pa(3)**2+Pa(2)**2+Pa(1)**2+Pa(5)**2)
C     Target 4-momenrum in lab
      Pb(5)=targetmass
      Pb(1)=0.
      Pb(2)=0.
      Pb(3)=0.
      Pb(4)=Pb(5)
C     C-particle in reaction a+b -> c+X (lab)
      Pc(1)=pxsum
      Pc(2)=pysum
      Pc(3)=pzsum
      Pc(4)=esum
      Pc(5)=dsqrt(Pc(4)**2-Pc(1)**2-Pc(2)**2-Pc(3)**2)
C     s invariant variable
      s=(Pa(4)+Pb(5))**2-Pa(1)**2-Pa(2)**2-Pa(3)**2
C     Target in CMS
      Pbcms(5)=targetmass
      Pbcms(4)=(s+Pb(5)**2-Pa(5)**2)/(2.*sqrt(s))
      Pbcms(1)=0.
      Pbcms(2)=0.
      Pbcms(3)=-1.*dsqrt(Pbcms(4)**2-Pbcms(5)**2)
C     to calc Xf we need use 4-momentum of C in CMS
      call arturs(Pc,Pbcms,Pccms)
C
      AmXmin=939.565
      EffMass=dsqrt(Pc(4)**2-Pc(1)**2-Pc(2)**2-Pc(3)**2)
      Xfwrong=2.*Pccms(3)/sqrt(s)
      Esmcms=Pccms(4)
      pt=dsqrt(Pccms(1)**2+Pccms(2)**2)
      Emax=(s+Pc(5)**2-AmXmin**2)/(2*sqrt(s))
      Amt=sqrt(pt**2+EffMass**2)
      Plmax=dsqrt(Emax**2-Pc(5)**2-pt**2)
      W=(Xf**2+(Amt/Plmax)**2)**(0.5)/pt
      A=Emax+Plmax
      Xf=Pccms(3)/Plmax
      Xplus=(Pccms(3)+Pccms(4))/A
      T=(Pc(4)-Pa(4))**2-(Pc(1)-Pa(1))**2-(Pc(2)-Pa(2))**2-
     *     (Pc(3)-Pa(3))**2
!kondr++{
      repulse_energy = ((neutron_mass**2 + proton_mass**2 - T)
     &       /(2 * neutron_mass)) - proton_mass
CSdv- if((repulse_energy + esum).gt.8000.) return                       ! for fun ;kondr
      if((repulse_energy + esum).gt.8500.) return                       ! for fun ;kondr
      call hf1(5054,repulse_energy       , 1.)                          ! cut for the max energy in lgd2, it can't exceed the beam energy. 
      call hf1(5055,repulse_energy + esum, 1.)                          ! we set 8000 instead of 7000 because of the noise in lgd2 which increases the real energy.
!kondr--} 
      T=-T/1.E+06                                                       ! why ? MeV->eV ? ;kondr
      sqrMM=(Pa(4)+Pb(4)-Pc(4))**2-(Pa(1)+Pb(1)-Pc(1))**2-
     *     (Pa(2)+Pb(2)-Pc(2))**2-(Pa(3)+Pb(3)-Pc(3))**2
      sqrMM=sqrMM/1.E+06
      Xparticle=pc(1)*zwall1/pc(3)
      Yparticle=pc(2)*zwall1/pc(3)
C
CEvd-
      call hf1(5032,esum,1.)
      call hf1(5034,float(npart),1.)
      EFM = ESUM**2 - PXSUM**2 - PYSUM**2 - PZSUM**2
      IF (EFM.LE.0.) THEN
          LOSSD(26) = LOSSD(26) + 1
          ef=ef+1
          return
      END IF
      Ptrans = PXSUM**2 + PYSUM**2

      call hf1(-6000,sqrt(Ptrans),1.)
      if(ngam.eq.2) then
        call hf2(-6010,sqrt(Ptrans),sqrt(efm) ,1.)
      endif
! !kondr++{ ! 2d - histogramm Meff vs Pt
!       hi_number_2d = 7000 + ngam * 100
!       call hf2(hi_number_2d,     sqrt(efm), sqrt(Ptrans) ,1.)
!       call hf2(hi_number_2d + 1, sqrt(efm), esum         ,1.)
! !kondr--}   ! was commented bcs files grow in size...
CEvd+
      AmPi0=134.977
      Wpi0=13.
      AmEta=547.862
      Weta=35.
      if(ngam.eq.2) then
C     Energy cut
      x1=xgam(1)
      y1=ygam(1)
c      e1=egam(1)
      x2=xgam(2)
      y2=ygam(2)
c      e2=egam(2)
C     
C     Energy histos                                     !  No histos
C-      if(esum.gt.7000.) call hf1(-70050,EffMass,1.)
C-      if(esum.gt.7500.) call hf1(-70051,EffMass,1.)
C-      if(esum.gt.8000.) call hf1(-70052,EffMass,1.)
C-      if(esum.gt.8500.) call hf1(-70053,EffMass,1.)
C-      if(EffMass.gt.(AmPi0-3.*Wpi0).and.EffMass.lt.(AmPi0+3.*Wpi0))
C-     *        then
C-         call hf1(-71020,esum,1.)
C-         if(esum.gt.7000.) then
C-            call hf2(-70030,x1,y1,1.)
C-            call hf2(-70030,x2,y2,1.)
C-            call hf2(-70040,xparticle,yparticle,1.)
C-         endif
C-
C-      else
C-         if(EffMass.gt.(AmPi0-6.*Wpi0).and.EffMass.lt.(AmPi0+6.*Wpi0))
C-     *     then
C-            call hf1(-71320,esum,1.)
C-            if(esum.gt.7000.) then
C-               call hf2(-70330,x1,y1,1.)
C-               call hf2(-70330,x2,y2,1.)
C-               call hf2(-70340,xparticle,yparticle,1.)
C-            endif
C-         endif
C-      endif 
C-      if(EffMass.gt.(AmEta-2.*Weta).and.EffMass.lt.(AmEta+2.*Weta))
C-     *        then
C-         call hf1(-71120,esum,1.)
C-         if(esum.gt.7000.) then
C-            call hf2(-70130,x1,y1,1.)
C-            call hf2(-70130,x2,y2,1.)
C-            call hf2(-70140,xparticle,yparticle,1.)
C-         endif
C-
C-      else
C-         if(EffMass.gt.(AmEta-4.*Weta).and.EffMass.lt.(AmEta+4.*Weta))
C-     *        then
C-            call hf1(-71420,esum,1.)
C-            if(esum.gt.7000.) then
C-               call hf2(-70430,x1,y1,1.)
C-               call hf2(-70430,x2,y2,1.)
C-               call hf2(-70440,xparticle,yparticle,1.)
C-            endif
C-
C-         endif
C-      endif
C
C
C      Energy cut 
         do j=0,9
         if(esum.gt.(1000.+500*j))then
C     Pi0
         if(EffMass.gt.(AmPi0-3.*Wpi0).and.EffMass.lt.(AmPi0+3.*Wpi0))
     *        then
            call hf2(-70010-j,x1,y1,1.)
            call hf2(-70010-j,x2,y2,1.)
            call hf2(-70020-j,xparticle,yparticle,1.)
C
            call hf1(-71060-j,    pt,1.)
            call hf1(-71070-j,    Xf,1.)
            call hf1(-71080-j, Xplus,1.)
            call hf1(-71090-j,Xfwrong,1.)
            call hf2(-71050-j,pt**2,xf,1.)
            call hf2(-71040-j,t,sqrMM,1.)
C     
            call hf1(-73060-j,    pt, W)
            call hf1(-73070-j,    Xf, W)
            call hf1(-73080-j, Xplus, W)
C     
            call hf1(-72060-j,    pt,Esmcms)
            call hf1(-72070-j,    Xf,Esmcms)
            call hf1(-72090-j,Xfwrong,Esmcms)
            call hf1(-72080-j, Xplus,Esmcms)
            call hf1(-72030-j,pt**2,Esmcms)
            call hf2(-72050-j,pt**2,xf,Esmcms)
C     
            call hf1(-74080-j, Xplus,Xplus)
            call hf1(-71010-j,     T,   1.)
            call hf1(-72010-j,     T,Esmcms)
         else
C     
           if(EffMass.gt.(AmPi0-6.*Wpi0).and.EffMass.lt.(AmPi0+6.*Wpi0))
     *           then
               call hf2(-70310-j,x1,y1,1.)
               call hf2(-70310-j,x2,y2,1.)
               call hf2(-70320-j,xparticle,yparticle,1.)
C
               call hf1(-71360-j,    pt,1.)
               call hf1(-71370-j,    Xf,1.)
               call hf1(-71380-j, Xplus,1.)
               call hf1(-71390-j,Xfwrong,1.)
               call hf2(-71350-j,pt**2,xf,1.)
               call hf2(-71340-j,t,sqrMM,1.)
C     
               call hf1(-73360-j,    pt, W)
               call hf1(-73370-j,    Xf, W)
               call hf1(-73380-j, Xplus, W)
C     
               call hf1(-72360-j,    pt,Esmcms)
               call hf1(-72370-j,    Xf,Esmcms)
               call hf1(-72390-j,Xfwrong,Esmcms)
               call hf1(-72380-j, Xplus,Esmcms)
               call hf1(-72330-j,pt**2,Esmcms)
               call hf2(-72350-j,pt**2,xf,Esmcms)
C     
               call hf1(-74380-j, Xplus,Xplus)
               call hf1(-71310-j,     T,   1.)
               call hf1(-72310-j,     T,Esmcms)
            endif
         endif
C
C     Eta
         if(EffMass.gt.(AmEta-2.*Weta).and.EffMass.lt.(AmEta+2.*Weta))
     *        then
            call hf2(-70110-j,x1,y1,1.)
            call hf2(-70110-j,x2,y2,1.)
            call hf2(-70120-j,xparticle,yparticle,1.)
C
            call hf1(-71160-j,    pt,1.)
            call hf1(-71170-j,    Xf,1.)
            call hf1(-71180-j, Xplus,1.)
            call hf1(-71190-j,Xfwrong,1.)
            call hf2(-71150-j,pt**2,xf,1.)
            call hf2(-71140-j,t,sqrMM,1.)
C     
            call hf1(-73160-j,    pt, W)
            call hf1(-73170-j,    Xf, W)
            call hf1(-73180-j, Xplus, W)
C     
            call hf1(-72160-j,    pt,Esmcms)
            call hf1(-72170-j,    Xf,Esmcms)
            call hf1(-72190-j,Xfwrong,Esmcms)
            call hf1(-72180-j, Xplus,Esmcms)
            call hf1(-72130-j,pt**2,Esmcms)
            call hf2(-72150-j,pt**2,xf,Esmcms)
C     
            call hf1(-74180-j, Xplus,Xplus)
            call hf1(-71110-j,     T,   1.)
            call hf1(-72110-j,     T,Esmcms)
C     
         else
C     
           if(EffMass.gt.(AmEta-4.*Weta).and.EffMass.lt.(AmEta+4.*Weta))
     *           then
               call hf2(-70410-j,x1,y1,1.)
               call hf2(-70410-j,x2,y2,1.)
               call hf2(-70420-j,xparticle,yparticle,1.)
C
               call hf1(-71460-j,    pt,1.)
               call hf1(-71470-j,    Xf,1.)
               call hf1(-71480-j, Xplus,1.)
               call hf1(-71490-j,Xfwrong,1.)
               call hf2(-71450-j,pt**2,xf,1.)
               call hf2(-71440-j,t,sqrMM,1.)
C     
               call hf1(-73460-j,    pt, W)
               call hf1(-73470-j,    Xf, W)
               call hf1(-7348, Xplus, W)
C     
               call hf1(-72460-j,    pt,Esmcms)
               call hf1(-72470-j,    Xf,Esmcms)
               call hf1(-72490-j,Xfwrong,Esmcms)
               call hf1(-72480-j, Xplus,Esmcms)
               call hf1(-72430-j,pt**2,Esmcms)
               call hf2(-72450-j,pt**2,xf,Esmcms)
C     
               call hf1(-74480-j, Xplus,Xplus)
               call hf1(-71410-j,     T,   1.)
               call hf1(-72410-j,     T,Esmcms)
C     
            endif
         endif
      endif
      enddo
      endif
      if(ngam.eq.2.and.sqrt(efm).gt.mmin.and.sqrt(efm).lt.mmax) then
        call hf1(-6001,sqrt(Ptrans),1.)
        call hf1(-6003,         Xf, 1.)
        call hf2(-6011,sqrt(Ptrans),sqrt(efm),1.)
      endif
      if(ngam.eq.2.and.sqrt(efm).gt.450..and.sqrt(efm).lt.650.) then
        call hf1(-6002,sqrt(Ptrans),1.)
        call hf1(-6004,         Xf, 1.)   
        call hf2(-6012,sqrt(Ptrans),sqrt(efm),1.)
      endif
      ptcut_ = ptcut**2
      if(Ptrans.lt.ptcut_) return
! commented ;kondr
! we don't need THIS mixing
! ! ! cmike Writing of event for event mixing analysis
! ! !       if (ngam.eq.2) then
! ! !         inquire(file='LastEvent.dat',exist=exists)
! ! !         if(exists) then
! ! !           open(unit=1,file='LastEvent.dat',status='old')
! ! !           read(1,*) egamlast,pla1last,pla2last,pla3last
! ! !           close(1)
! ! !           mixmass = sqrt((egamlast+egam(2))**2 -
! ! !      &                   (pla1last+plab(1,2))**2-
! ! !      &                   (pla2last+plab(2,2))**2-
! ! !      &                   (pla3last+plab(3,2))**2)
! ! !           do iecut = 0,9
! ! !             recut = real(iecut)*500.+1000.
! ! !             if((egamlast+egam(2)).gt.recut) then
! ! !               call hf1(-30120-iecut,mixmass,1.)
! ! !             endif
! ! !           enddo
! ! !         mixstatus='old'
! ! !         else
! ! !         mixstatus='new'
! ! !         endif
! ! !         open(unit=1,file='LastEvent.dat',status=mixstatus)
! ! !         write(1,*) egam(1),plab(1,1),plab(2,1),plab(3,1)
! ! !         close(1)
! ! !       endif
! ! ! cmike      
cmike
      x1=pxsum*zwall1/pzsum
      y1=pysum*zwall1/pzsum
      call hf2(5011,x1,y1,1.)
      if(ngam.eq.2.and.esum.gt.1000.) then
        call hf2(-13001,sqrt(1-dcos(3,1)**2),sqrt(efm),1.)
        call hf2(-13001,sqrt(1-dcos(3,2)**2),sqrt(efm),1.)
        call hf2(-13002,log(EGAM(1)),sqrt(efm),1.) 
        call hf2(-13002,log(EGAM(2)),sqrt(efm),1.)
        ktf =         log(EGAM(1)).lt.7..and.log(EGAM(2)).lt.7.
        ktf = ktf.and.log(EGAM(1)).gt.6..and.log(EGAM(2)).gt.6.
        ktf = ktf.and.sqrt(efm).lt.(50.*log(EGAM(1))-275.)
        ktf = ktf.and.sqrt(efm).lt.(50.*log(EGAM(2))-275.)
        if(ktf) then
          x1=pxsum*zwall1/pzsum
          y1=pysum*zwall1/pzsum
          call hf2(-13005,x1,y1,1.)
        endif
        call hf1(-5003,egam(1),1.)
        call hf1(-5003,egam(2),1.)
c        call hf2(-13003,EGAM(1),sqrt(efm),1.)
c        call hf2(-13003,EGAM(1),sqrt(efm),1.)
      endif
cmike
c      
c------------effektivnye massy 20401-20408-------------
c      call hf1(20400+ngam-1,sqrt(efm),1.)
c------------raspredelenie po srezam po energii dlia 2x gamma------------
c      write(*,*)'2gamma'
      if (NPART.eq.2) then
        I1=IPART(1)
        I2=IPART(2)
      energy_2gamma=EGAM(I1)+EGAM(I2)
c-------------Ugol vyleta pi0 i eta--------------------------------------
      sqrt_efm=sqrt(efm)
cmike-------t and missing mass culculation for pi0 and eta mesons
      call t_and_missmass(0,0,ESUM,PXSUM,PYSUM,PZSUM)
      if((sqrt_efm.gt.65. .and.sqrt_efm.lt.100.).or.
     &   (sqrt_efm.gt.170..and.sqrt_efm.lt.205.)) then
         call t_and_missmass(1,2,ESUM,PXSUM,PYSUM,PZSUM)
         if(ESUM.gt.6334.)
     &        call t_and_missmass(1,4,ESUM,PXSUM,PYSUM,PZSUM)
      endif
      if( sqrt_efm.gt.100..and.sqrt_efm.lt.170.)then
         call t_and_missmass(1,1,ESUM,PXSUM,PYSUM,PZSUM)
         if(ESUM.gt.6334.)
     &        call t_and_missmass(1,3,ESUM,PXSUM,PYSUM,PZSUM)
      endif
      if((sqrt_efm.gt.420..and.sqrt_efm.lt.480.).or.
     &   (sqrt_efm.gt.600..and.sqrt_efm.lt.660.))then
         call t_and_missmass(2,2,ESUM,PXSUM,PYSUM,PZSUM)
         if(ESUM.gt.6334.)
     &        call t_and_missmass(2,4,ESUM,PXSUM,PYSUM,PZSUM)
      endif
      if( sqrt_efm.gt.480..and.sqrt_efm.lt.600.) then
         call t_and_missmass(2,1,ESUM,PXSUM,PYSUM,PZSUM)
         tclock=t
         eta=.true.
         if(ESUM.gt.6334.)
     &        call t_and_missmass(2,3,ESUM,PXSUM,PYSUM,PZSUM)
      endif
cmike

      if (sqrt_efm.gt.450..and.sqrt_efm.lt.650.) then !---coordinates of gammas by eta-meson
        do nfotona=1,2
         x1=xgam(nfotona)
         y1=ygam(nfotona)
         r=sqrt(x1*x1+y1*y1)
         call hf2(-5002,x1,y1,1.)
         call hf2(-5005,r,sqrt_efm,1.)
        enddo
      endif
      if (sqrt_efm.gt.mmin.and.sqrt_efm.lt.mmax) then
        do nfotona=1,2
         r=sqrt((xgam(nfotona))**2+ygam(nfotona)**2)
         call hf2(-5004,r,sqrt_efm,1.)
        enddo
         x1=plab(1,1)*zwall1/plab(3,1)
         y1=plab(2,1)*zwall1/plab(3,1)
         call hf2(5001,x1,y1,1.)
         x1=plab(1,2)*zwall1/plab(3,2)
         y1=plab(2,2)*zwall1/plab(3,2)
         call hf2(5001,x1,y1,1.)
         x1=pxsum*zwall1/pzsum
         y1=pysum*zwall1/pzsum
         call hf2(5011,x1,y1,1.)
         call hf1(5012,x1,1.)
         call hf1(5013,y1,1.)
        do qqq=1,9
          qqq1=(qqq-1)*500+1500
          if (esum.gt.qqq1) then
            a=31090+qqq
            tgalfa=sqrt(pxsum*pxsum+pysum*pysum)/pzsum
            alfa=atan(tgalfa)
            call hf1(a,alfa,1.)
          endif
        enddo
      endif
      if (sqrt_efm.gt.500.and.sqrt_efm.lt.600) then
        do qqq=1,9
          qqq1=(qqq-1)*500+1500
          if (esum.gt.qqq1) then
            a=31190+qqq
            tgalfa=sqrt(pxsum*pxsum+pysum*pysum)/pzsum
            alfa=atan(tgalfa)
            call hf1(a,alfa,1.)
          endif
        enddo
      endif
c---------------------------------------------------------------
      do qqq=1,7
        qqq1=(qqq-1)*500+1000
        if (energy_2gamma.gt.qqq1) then
          qqq1=energy_2gamma/(EGAM(I1)+EGAM(I2))
          a=20600+qqq
          call hf1(a,qqq1,1.)
          sqrt_efm=sqrt(EFM)
          a=20500+qqq
          call hf2(20700,sqrt_efm,qqq1,1.)
          qqq1=qqq1*sqrt_efm
          call hf2(20701,sqrt_efm,qqq1,1.)
          call hf1(20702,qqq1,1.)
        endif
      enddo
      endif
      goto 300      ! no, really ? wtf ? ;kondr
c      
! \/  this code is to be never executed  \/
c------------raspredelenie po srezam po energii------------
      do qqq=1,8
        qqq1=(qqq-1)*500+1000
        sqrt_efm=sqrt(EFM)
        if(esum.gt.qqq1)then
          a=20500+qqq+(ngam-2)*10
          call hf1(a,sqrt_efm,1.)
        endif
      enddo
      write(*,*)'3-4 gamma'
c--------------kombinatorika dlia 3x,4x gamma-----------------
      if(ngam.ge.3)then
        do i=1,(ngam-1)
          do j=(i+1),ngam
            call minimas(ipart,i,j,mm)
            do qqq=1,8
              qqq1=(qqq-1)*500+1000
              if(esum.gt.qqq1)then
                a=30100+qqq+(ngam-3)*10
                call hf1(20600+(ngam-2)*10+qqq,
     &               abs((egam(i)-egam(j))/(egam(i)+egam(j))),1.)
              endif
            enddo
!commented ;kondr
!no histos are filled, no variables are changed. useless... ; anyway this is inside "goto 300" , 20 lines above
!             if(mm.gt.mmin.and.mm.lt.mmax)then
!               if(ngam.ge.4)then
!                 do i1=1,10 
!                   partts(i1)=0
!                 enddo
!                 do i1=1,(i-1) 
!                   partts(i1)=i1
!                 enddo
!                 do i1=(i+1),(j-1)
!                   partts(i1-1)=i1
!                 enddo
!                 do i1=(j+1),ngam
!                    partts(i1-2)=i1
!                 enddo
! c
!                 do i1=1,(ngam-3)
!                   do i2=(i1+1),(ngam-2)
!                     call minimas(iparts,partts(i1),partts(i2),mm)
! !                     do qqq=1,8
! !                       qqq1=(qqq-1)*500+1000
! !                       if(esum.gt.qqq1)then
! !                         a=30400+qqq+(ngam-3)*10
! ! c                        call hf1(a,mm,1.)
! !                       endif
! !                     enddo
! !                     if(mm.gt.mmin.and.mm.lt.mmax)then
! !                       do qqq=1,8
! !                         qqq1=(qqq-1)*500+1000
! !                         if(esum.gt.qqq1)then
! !                           a=30410+qqq+(ngam-4)*10
! ! c                          call hf1(a,sqrt(efm),1.)
! !                         endif
! !                       enddo
! !                     endif
!                   enddo
!                 enddo
!               endif
! !               do qqq=1,9
! !                 qqq1=(qqq-1)*500+1000
! !                 if(esum.gt.qqq1)then
! ! c                  call hf1(30200+(ngam-3)*10+qqq-1,sqrt(efm),1.)
! !                 endif
! !               enddo
!             endif
          enddo
        enddo
      endif
c----------------------------------------------------------
C
  300 continue
c 
c--------gistogramirovanie vsego podriad-------------------
      do qqq=1,10
c------------energiya--------------------------------------
        qqq1=(qqq-1)*1000
        if(esum.gt.qqq1) then
          call hf1(30000+(ngam-1)*100+qqq-1,esum,1.)
          do i=1,ngam
            call hf1(30010+(ngam-1)*100 +qqq-1,egam(i),1.)
          enddo
        endif
c------------efmass----------------------------------------
csdv-  qqq1=(qqq-1)*500+1500  etc.
c
CEvd+   additional slices over energy
        qqq2=(qqq-1)*500+6000
        if(esum.gt.qqq2) then
           call hf1(30030+(ngam-1)*100+qqq-1,sqrt(efm),1.)
        endif
CEvd-
        qqq1=(qqq-1)*500+1000
        if(esum.gt.qqq1) then
          call hf1(30020+(ngam-1)*100+qqq-1,sqrt(efm),1.)
          if(esum.lt.(qqq1+500).and.ngam.eq.2) then
             call hf1(32119+qqq,sqrt(efm),1.)
          endif
          if(ngam.eq.2.or.ngam.eq.3) then
c-------tol'ko K+
            if(c1c2c3(2).gt.c2min.and.c1c2c3(1).lt.c1min) then
               call hf1(31510+qqq-1+(ngam-2)*30,sqrt(efm),1.)
            endif
c-------tol'ko p+
            if(c1c2c3(2).lt.c2min.and.c1c2c3(1).lt.c1min) then
               call hf1(31520+qqq-1+(ngam-2)*30,sqrt(efm),1.)
            endif
c-------tol'ko pi+
            if(c1c2c3(2).gt.c2min.and.c1c2c3(1).gt.c1min) then
               call hf1(31500+qqq-1+(ngam-2)*30,sqrt(efm),1.)
            endif
          endif
        endif
      enddo


!kondr++{ .................. 2 gamma event mixing    ; (cmix-2gg 2 gamma)
!      write(*,*)'х1 ngam = ',ngam
      if (ngam.eq.2) then
!      write(*,*)'х2 ngam = ',ngam
c-------tol'ko K+
         if(c1c2c3(2).gt.c2min.and.c1c2c3(1).lt.c1min) then
            ikplus = ikplus + 1
            icurrent = ikplus-(ikplus/imix)*imix+1                      ! -> icurrent = ikplus % imix + 1
            do tempcounter=1,4
               P1g_mix(tempcounter,icurrent,1) = P1g_temp(tempcounter)
               P2g_mix(tempcounter,icurrent,1) = P2g_temp(tempcounter)
            enddo
            if (ikplus.lt.imix)       goto 611
            do icycl=1,imix      !mix cycle
               if (icycl.eq.icurrent) goto 601
               esum = P1g_mix(4,icurrent,1) + P2g_mix(4,icycl,1)        !1 is for k+; 2 for p+, 3 for pi+
               efm = esum**2 - 
     &                (P1g_mix(1,icurrent,1) + P2g_mix(1,icycl,1))**2 -
     &                (P1g_mix(2,icurrent,1) + P2g_mix(2,icycl,1))**2 -
     &                (P1g_mix(3,icurrent,1) + P2g_mix(3,icycl,1))**2
               do counter=1,10   !energy slices cycle
                  energy_slice=(counter-1)*500+1000
                  if(esum.gt.energy_slice) 
     &            call hf1(-(31510+counter-1),sqrt(efm),1.)
               enddo
  601       enddo
  611    endif
c-------tol'ko p+
         if(c1c2c3(2).lt.c2min.and.c1c2c3(1).lt.c1min) then
            ipplus = ipplus + 1
            icurrent = ipplus-(ipplus/imix)*imix+1                      ! -> icurrent = ipplus % imix + 1
            do tempcounter=1,4
               P1g_mix(tempcounter,icurrent,2) = P1g_temp(tempcounter)
               P2g_mix(tempcounter,icurrent,2) = P2g_temp(tempcounter)
            enddo
            if (ipplus.lt.imix)       goto 612
            do icycl=1,imix      !mix cycle
               if (icycl.eq.icurrent) goto 602
               esum = P1g_mix(4,icurrent,2) + P2g_mix(4,icycl,2)        !1 is for k+; 2 for p+, 3 for pi+
               efm = esum**2 - 
     &                (P1g_mix(1,icurrent,2) + P2g_mix(1,icycl,2))**2 -
     &                (P1g_mix(2,icurrent,2) + P2g_mix(2,icycl,2))**2 -
     &                (P1g_mix(3,icurrent,2) + P2g_mix(3,icycl,2))**2
               do counter=1,10   !energy slices cycle
                  energy_slice=(counter-1)*500+1000
                  if(esum.gt.energy_slice) 
     &            call hf1(-(31520+counter-1),sqrt(efm),1.)
               enddo
  602       enddo
  612    endif
c-------tol'ko pi+
         if(c1c2c3(2).gt.c2min.and.c1c2c3(1).gt.c1min) then
            ipiplus = ipiplus + 1
            icurrent = ipiplus-(ipiplus/imix)*imix+1                    ! -> icurrent = ikplus % imix + 1
            do tempcounter=1,4
               P1g_mix(tempcounter,icurrent,3) = P1g_temp(tempcounter)
               P2g_mix(tempcounter,icurrent,3) = P2g_temp(tempcounter)
            enddo
            if (ipiplus.lt.imix)      goto 613
            do icycl=1,imix      !mix cycle
               if (icycl.eq.icurrent) goto 603
               esum = P1g_mix(4,icurrent,3) + P2g_mix(4,icycl,3)        !1 is for k+; 2 for p+, 3 for pi+
               efm = esum**2 - 
     &                (P1g_mix(1,icurrent,3) + P2g_mix(1,icycl,3))**2 -
     &                (P1g_mix(2,icurrent,3) + P2g_mix(2,icycl,3))**2 -
     &                (P1g_mix(3,icurrent,3) + P2g_mix(3,icycl,3))**2
               do counter=1,10   !energy slices cycle
                  energy_slice=(counter-1)*500+1000
                  if(esum.gt.energy_slice) 
     &            call hf1(-(31500+counter-1),sqrt(efm),1.)
               enddo
  603       enddo
  613    endif
      endif
!kondr--} .................. event mixing






      do i=1,(ngam-1)
        do j=(i+1),ngam
c-------------kombinatorika--------------------------------
          call minimas(ipart,i,j,mm)
          assimetry1=abs((egam(i)-egam(j))/(egam(i)+egam(j)))
c--------ugol vyleta chego-to v SCM osnovnoy chasticy------
          if(ngam.gt.2) then
            call cos_in_f2(esum,pxsum,pysum,pzsum,sqrt(efm),i,j,cosalfa)
            cos1=cosalfa
          endif
c----------------------------------------------------------
          do qqq=1,10
            qqq1=(qqq-1)*500+1000
            if(esum.gt.qqq1) then
CEvd+  combine gammas for gamma>2 events (it makes no sense for 2gamma events)
               if(ngam.gt.2)then
                  call hf1(30030+(ngam-1)*100+qqq-1,mm,1.)
               endif
               if(ngam.eq.3) then ! 3 gamma events
                  qqq2 = sqrt(efm)
                  if(qqq2.gt.650.0.and.qqq2.lt.900.0) ! omega(782) mass window
     +            call hf1(-(30030+(ngam-1)*100+qqq-1),mm,1.)     
               endif
CEvd-
               call hf1(30040+(ngam-1)*100+qqq-1,assimetry1,1.)
               if(sqrt(efm).gt.110..and.sqrt(efm).lt.160.) then
                  call hf1(32040+(ngam-1)*100+qqq-1,assimetry1,1.)
               endif
            endif
            qqq1=(qqq-1)*1000
            if(esum.gt.qqq1) then
              call hf1(30080+(ngam-1)*100+qqq-1,esum,1.)
            endif
          enddo
cmike
          if(ngam.eq.3.and.esum.gt.1000.) then
            call hf2(-13003,log(egam(i)),mm,1.)
            call hf2(-13003,log(egam(j)),mm,1.)
          endif
cmike
c          write(*,*)'ef3'
c---------------efmass with pi0----------------------------
          if(mm.gt.mmin.and.mm.lt.mmax)then
            do qqq=1,10
              qqq1=(qqq-1)*500+1000
              if(esum.gt.qqq1) then
                call hf1(30050+(ngam-1)*100+qqq-1,sqrt(efm),1.)
                if(ngam.eq.3) then
c-------tol'ko K+
            if(c1c2c3(2).gt.c2min.and.c1c2c3(1).lt.c1min) then
               call hf1(31570+qqq-1,sqrt(efm),1.)
            endif
c-------tol'ko p+
            if(c1c2c3(2).lt.c2min.and.c1c2c3(1).lt.c1min) then
               call hf1(31580+qqq-1,sqrt(efm),1.)
            endif
c-------tol'ko pi+
            if(c1c2c3(2).gt.c2min.and.c1c2c3(1).gt.c1min) then
               call hf1(31560+qqq-1,sqrt(efm),1.)
            endif                   
                endif
              endif
            enddo
c------kombinatorika dopolneniya k pi0---------------------
            if(ngam.ge.4)then
              do i1=1,10 
                partts(i1)=0
              enddo
              do i1=1,(i-1) 
                partts(i1)=i1
              enddo
              do i1=(i+1),(j-1)
                partts(i1-1)=i1
              enddo
              do i1=(j+1),ngam
                 partts(i1-2)=i1
              enddo
              do i1=1,(ngam-3)
                do i2=(i1+1),(ngam-2)
                  call minimas(iparts,partts(i1),partts(i2),mm)
                  assimetry2=abs((egam(partts(i1))-egam(partts(i2)))/
     &                          (egam(partts(i1))+egam(partts(i2))))
                  call cos_in_f2(esum,pxsum,pysum,pzsum,sqrt(efm),
     &                           partts(i1),partts(i2),cosalfa)
                  cos2=cosalfa
                  do qqq=1,10
                    qqq1=(qqq-1)*500+1000
                    if(esum.gt.qqq1)then
                      call hf1(30060+(ngam-1)*100+qqq-1,mm,1.)
                    endif
cmike---------------addition to pi0 from f2
                    if(ngam.eq.4.and.sqrt(efm).gt.1050.) then
                      call hf1(-30360-qqq+1,mm,1.)
                    endif
cmike
cmike
                    if(ngam.eq.4.and.esum.gt.1000) then
                      call hf2(-13004,log(egam(partts(i1))),mm,1.)
                      call hf2(-13004,log(egam(partts(i2))),mm,1.)
                    endif
cmike
                  enddo
c----------massa pi0+pi0-----------------------------------
                  if(mm.gt.mmin.and.mm.lt.mmax)then
                    do qqq=1,10
                      qqq1=(qqq-1)*500+1000
                      if(esum.gt.qqq1)then      
                        call hf1(30070+(ngam-1)*100+qqq-1,sqrt(efm),1.)
                        call hf1(30090+(ngam-1)*100+qqq-1,assimetry2,1.)
                        call hf1(30090+(ngam-1)*100+qqq-1,assimetry1,1.)
                        if(ngam.eq.4.and.sqrt(efm).gt.1150
     &                              .and.sqrt(efm).lt.1400)then
                          call hf1(31000+qqq-1,cos1,1.)
                          call hf1(31000+qqq-1,cos2,1.)
                        endif
                      endif
                    enddo
                  endif
                enddo
              enddo
            endif
          endif
c          write(*,*)'ef4'
        enddo
      enddo
c----------------------------------------------------------
      if(sqrt(efm).gt.110..and.sqrt(efm).lt.160.)call hf1(5043,esum,1.)
      if(sqrt(efm).gt.160..and.sqrt(efm).lt.185.)call hf1(5044,esum,1.)
      if(sqrt(efm).gt. 85..and.sqrt(efm).lt.110.)call hf1(5045,esum,1.)
      if(sqrt(efm).gt.498..and.sqrt(efm).lt.598.)call hf1(5046,esum,1.)
      if(sqrt(efm).gt.598..and.sqrt(efm).lt.643.)call hf1(5047,esum,1.)
      if(sqrt(efm).gt.448..and.sqrt(efm).lt.498.)call hf1(5048,esum,1.)
c      write(*,*)'gistogramming done'
      IF (EFM.LE.0.) THEN
        LOSSD(26) = LOSSD(26) + 1
        ef=ef+1
        RETURN
      END IF
c      EFMAS = SQRT(EFM)-0.0111*myenergy+44.4
      EFMAS = SQRT(EFM)
      efm=efm/1000
      call hf1(20311,efm,1.)
      if(npart.eq.2)then
        call hf1(20300,esum,1.)
        x=(egam(ipart(1))-egam(ipart(2)))/
     &       (egam(ipart(1))+egam(ipart(2)))
        call hf1(20301,x,1.)
        x=abs(x)
        call hf1(20309,x,1.)
      endif
      if(npart.eq.2.and.efmas.gt.100.and.efmas.lt.177)then
        call hf1(20303,esum,1.)
        do i=1,2
          IP = IPART(I)
          call hf1(20306,egam(ip),1.)
        enddo
        x=(egam(ipart(1))-egam(ipart(2)))/
     &       (egam(ipart(1))+egam(ipart(2)))
        call hf1(20304,x,1.)
        x=abs(x)
        call hf1(20310,x,1.)
      endif

      call hf1(20302,efmas,1.)
c      if(efmas.ge.176.and.efmas.le.180)then
c        write(*,*)'chetyreh-impulsy chastic'
c        do i=1,npart
c          ip=ipart(i)
c          write(*,*)plab(1,ip),plab(1,ip),plab(1,ip),egam(ip)
c        enddo
c      endif
      OK = .TRUE.
C
c      write(*,*)'efmass - done'
      RETURN
      END

      subroutine minimas(ipart,np1,np2,mm)
CEvd  Just a comment:
C     This subroutine supposed to calculate mass 
C     of gammas with indices np1 and np2 
C     ipart is not used here
      COMMON /GAMMAS/ NGAM, XGAM(60), YGAM(60), EGAM(60), DCOS(3,60)
      COMMON /MOMENT/ pi0mass,PLAB(3,60)
      DIMENSION IPART(60)
      real es,px,py,pz,m1,mm
c      write(*,*)'sdfh',np1,np2,ipart(np1),ipart(np2),ngam
      es=egam(np1)+egam(np2)
      px=plab(1,np1)+plab(1,np2)
      py=plab(2,np1)+plab(2,np2)
      pz=plab(3,np1)+plab(3,np2)
c      write(*,*)'mm0.1',es,px,py,pz
      m1=es*es-px*px-pz*pz-py*py
c      write(*,*)'mm1',m1
      if(m1.lt.0) then
         mm=10000
         return
      endif
      mm=sqrt(m1)
      return
      end

      subroutine cos_in_f2(ef,xf,yf,zf,f2_mas,np1,np2,cosalfa)
      COMMON /GAMMAS/ NGAM, XGAM(60), YGAM(60), EGAM(60), DCOS(3,60)
      COMMON /MOMENT/ pi0mass,PLAB(3,60)
      real es,px,py,pz,cosalfa
      real ep,pxp,pyp,pzp,modp
c-------chastica prost tak-------------------------------
      es=egam(np1)+egam(np2)
      px=plab(1,np1)+plab(1,np2)
      py=plab(2,np1)+plab(2,np2)
      pz=plab(3,np1)+plab(3,np2)
c-------chastica v SCM f2--------------------------------
c      write(*,*)f2_mas,ef
      ep=(ef*es-(xf*px+yf*py+zf*pz))/f2_mas
      pxp=px-xf*((es+ep)/(ef+f2_mas))
      pyp=py-yf*((es+ep)/(ef+f2_mas))
      pzp=pz-zf*((es+ep)/(ef+f2_mas))
      modp=sqrt(pxp*pxp+pyp*pyp+pzp*pzp)
c      write(*,*)modp
      cosalfa=pzp/modp
      return
      end

      subroutine t_and_missmass(npeak,key,ESUM,PXSUM,PYSUM,PZSUM)
        integer npart,npeak,key
        real ESUM,PXSUM,PYSUM,PZSUM
        real momentbeam,energy,momentx,momenty,momentz
        real t,missmass,mpart(2)
        data mpart/139.57,493.677/
CEvd+
        common /t_missmass/ t,missmass
        common /triggers/c1c2c3(3), c1min,c2min,c3min
C
        if((c1c2c3(1).gt.c1min).and.(c1c2c3(2).gt.c2min))then ! pi+ beam
          npart=1
        else
          npart=2 ! K+ beam
        endif
        momentbeam = 7000.
        energy  = sqrt(momentbeam**2 + mpart(npart)**2) - ESUM
        momentx = PXSUM
        momenty = PYSUM
        momentz = momentbeam-PZSUM
        t = energy**2 - momentx**2 - momenty**2 - momentz**2
        t = t/1000.
        call hf1(-60001-npart*1000-npeak*100-key*10,t,1.)
        energy = energy + 939.57!<-- neutron mass
        missmass=sqrt(energy**2 - momentx**2 - momenty**2 - momentz**2)
        call hf1(-60002-npart*1000-npeak*100-key*10,missmass,1.)
        return
      end
C
      SUBROUTINE ARTURS(PC,P0,PL)
C...........................................................................
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION PC(5),P0(5),PL(5)
C
      AM0 =  P0(5)
      EL4 = (PC(4)*P0(4) + PC(3)*P0(3) + PC(2)*P0(2) + PC(1)*P0(1))/AM0
      DO 10 I = 1,3
   10 PL(I) = PC(I) + P0(I)*(EL4+PC(4))/(P0(4)+AM0)
      PL(4) = EL4
      RETURN
      END
