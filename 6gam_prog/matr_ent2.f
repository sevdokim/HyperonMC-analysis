      function rpl(X,n)
      real X
      double precision XX
      XX = X 
      rpl = (Dlog(XX))**(n-1)
      return
      end
C      
      function r10(X,n)
      real X
      double precision XX
      XX = X 
      r10 = (Dlog10(XX))**(n-1)
      return
      end
C      
      real function xpl(X,n)         ! Sdv 19.06.09
      real X                         ! X**n monons 
      double precision XX    
c
      XX  = X
      if(n.le.6)then
         xpl = XX**n
      else
C-    xpl=XX/(XX+n-6)                !  epl58.dat E>1500
      xpl=XX/(XX+(n-6)*0.5)          !  epl60.dat E>1500
      endif
      RETURN
      END

      subroutine matr_ent(egam,i)
c    ----------------------------------------
      implicit none
      common /minihi/ matrix(10,10),b(10),alfa1(100),kkk,istep
      real matrix,b,alfa1
      COMMON /MOMENT/ pi0mass, PLAB(3,60)
cmike
      common/chisquare/ chi
      real coefa,coefb,coeft
cmike
      real chi
      integer m, l, kkk,istep,i
      real c12, eegam, cosalfa, alfa, mult, pi0mass,plab
      real egam, coef, wvec, mtmp, sigmaq
      real mypi,rpl,r10,xpl,eg_in_pi0
      real reg1,reg2
      dimension egam(60), eegam(2), wvec(10)
C
      mult = 1000.
C10-  mult = 1.
      pi0mass = 134.9766 
C      
      call angle(1, 2,  cosalfa, alfa)
      c12 = sqrt(2.*(1.-cosalfa))
      call hf1(6002, c12, 1.)
C      
      eegam(1) = egam(1)/mult
      eegam(2) = egam(2)/mult
      mypi = c12*sqrt(eegam(1)*eegam(2))*mult
      call hf1(7001,mypi,1.)
      coef = (c12**2)/(2*eegam(1)*eegam(2))
c
      wvec(1) = eegam(1) + eegam(2)
      do m = 2, kkk
c10-    wvec(m) = eegam(2)*r10(eegam(1),m) + eegam(1)*r10(eegam(2),m)
	wvec(m) = eegam(2)*rpl(eegam(1),m) + eegam(1)*rpl(eegam(2),m)
c-      wvec(m) = eegam(2)*xpl(eegam(1),m) + eegam(1)*xpl(eegam(2),m)
      enddo
c
c-    reg1 = eg_in_pi0(egam(1))            ! correction for sqrt or Eg probability
c-    reg2 = eg_in_pi0(egam(2))            ! correction for sqrt or Eg probability
c-    if (reg1.le.0.0.or.reg2.le.0.0) return
c      
      sigmaq = 0.0025*(c12*c12*(eegam(1)+eegam(2))+0.014)
c-    sigmaq = sigmaq*sqrt(reg1*reg2)      ! correction for sqrt of Eg probability
c-    sigmaq = sigmaq*reg1*reg2            ! correction for Eg probability
c
      call hf1(-1,sqrt(sigmaq),1.)
      chi = chi + (mypi - pi0mass)**2/(sigmaq*mult*mult)
      call hf1(-2,(mypi - pi0mass)**2/(sigmaq*mult*mult),1.)
      call hf1(-3,(mypi - pi0mass)/(sqrt(sigmaq)*mult)  ,1.)
c
      call hf1(20283,1./sigmaq,1.)
      
      do m = 1, kkk
        do l = m, kkk
          mtmp=matrix(m,l)
          matrix(m,l)= matrix(m,l) + i*coef*wvec(m)*wvec(l)/sigmaq
          matrix(l,m)= matrix(m,l)
        enddo
        b(m) = b(m) + i*wvec(m)*c12*
     &  ((pi0mass/(mult*sqrt(eegam(1)*eegam(2))))-c12)/sigmaq
      enddo
      return
      end

      subroutine new_egam(energy)
      implicit none
      common /minihi/ matrix(10,10),b(10),alfa1(100),kkk,istep
      real matrix,b,alfa1
      integer i, kkk,istep,j
      real mult, en, energy,fpl,xpl,rpl,r10
C
      mult = 1000.
C10-  mult = 1.
      energy=energy/mult
      do j=1,istep
         en = energy + alfa1((j-1)*10+1)
         do i = 2, kkk
C10-        en = en + alfa1((j-1)*10+i)*r10(energy,i)
	    en = en + alfa1((j-1)*10+i)*rpl(energy,i)
c-          en = en + alfa1((j-1)*10+i)*xpl(energy,i)
         enddo
         energy = en
      enddo
      energy=energy*mult
      return
      end
C
      function eg_in_pi0(X)
C
C     Sdv 14.11.2019: this is probability parametrization of gamma energy 
C                     in 2g decay of pi0 at energy Epi0 > 1500 MeV
C ........................................................................         
      common/parpar/par(10)
      data par / 58958., -0.00088060, 6.5486, 13570., 4244.4,
     +          -6624.5,    24773.,  -4380.7, 7631.9,    0.0/
C
      if(X.lt.1275.) then
        	    eg_in_pi0 = 21663. - 1.2405*X
         else
         eg_in_pi0=par(1)*exp(par(2)*X)
     +           +(par(3)*exp(-(X-par(4))**2/(2.*par(5)**2))
     +           + par(6)*exp(-(X-par(7))**2/(2.*par(8)**2)))
     *           * abs(par(9)-X)
         endif
      eg_in_pi0=eg_in_pi0/3800.
      return
      end
C      
      function E_cor_pi0(ESHW)
C
C     Sdv 17.11.2018  --> not used now:
C     This is attemp to correct further the energy scale by using fit 
C     of energy dependence of pi0 mass on the full Hyperon statistics 
C      
      real ESHW
C      
C-    write(*,*) 'ESHW before:',ESHW 
C-    ESHW = ESHW-0.11395*tan((ESHW-4000.)/2580.)/135.0
      ESHW = ESHW-1.00000*tan((ESHW-4000.)/2580.)/135.0
C-    write(*,*) 'ESHW after :',ESHW 
C-    write(*,*)
C      
      E_cor_pi0 = ESHW
      return
      end
      
