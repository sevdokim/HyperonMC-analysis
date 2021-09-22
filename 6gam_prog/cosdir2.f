
      SUBROUTINE COSDIR
C=======================================================================
C     CALCULATES DIRECTIVE COSINES OF GAMMA-QUANTA
C     Author Nadia Russakovich
c     Updated:
c=========================================================================
c     Lines commented by SA caused mass shift in 2g-spectrum (at least).     ; kondr
c     This version is taken from the old analysis programm, without editing  ; kondr

      COMMON /ZWALLS/ ZWALL1,ZWALL
      COMMON /GAMMAS/ NGAM, XGAM(60), YGAM(60), EGAM(60), DCOS(3,60)
      COMMON /TARG  / XTARG,YTARG,ZTARG
      data x0,Ec/27.3,13.69/          ! x0 - rad dlina v mm, Ec - v MeV     
      REAL*8 AX, AY, RB, DZ
      real*8 tantheta,dx,dy,tm,rnew,cphi,sphi,costheta,rold
      real*8 find_SinAlpha,CosAlphaX,SinAlphaX,CosAlphaY,SinAlphaY
      real*8 CosAlpha,SinAlpha
      real nyu
C
      nyu=0.717
CSdv- nyu=0.62            !  Original
CSdv- nyu=0.45            !  22.01.2020
CSdv- nyu=0.365           !  05.02.2020 -- best for exact 2 solution
CSdv- nyu=0.25            !  22.01.2020
CSdv- nyu=0.435           !  03.02.2020 -- estimated as the best for original
C
      DO 10 IG = 1, NGAM
        dx=DBLE(XGAM(IG) - XTARG)
        dy=DBLE(YGAM(IG) - YTARG)
        RB=DBLE(ZWALL	 - ZTARG)	
	tm=nyu*x0*(alog(Egam(IG)/Ec)+ 0.5)
C       				     Exact2 solution of the equation
CExac2-	rold=dsqrt(dx**2+dy**2)
CExac2-	cphi=dx/rold
CExac2-	sphi=dy/rold
C
CExac2-	SinAlpha = find_SinAlpha(tm,RB,rold)
CExac2-	CosAlpha = Dsqrt(1.d0-SinAlpha**2)
C
CExac2-	DCOS (1,IG) = SNGL(SinAlpha*cphi)
CExac2-	DCOS (2,IG) = SNGL(SinAlpha*sphi)
CExac2-	DCOS (3,IG) = SNGL(CosAlpha)
CExac2-	go to 10
C
CExact- SinAlphaX = find_SinAlpha(tm,RB, dX)
CExact- SinAlphaY = find_SinAlpha(tm,RB, dY)
CExact- CosAlphaX = Dsqrt(1.d0-SinAlphaX**2)
CExact- CosAlphaY = Dsqrt(1.d0-SinAlphaY**2)
C	
CExact- AX = SinAlphaX/CosAlphaX
CExact- AY = SinAlphaY/CosAlphaY
C
C------------------igra vokrug utochneniya koordinat-----
C
        AX = dx/RB							  !  Original
        AY = dy/RB							  !  Original
        AX = dble(ax-nyu*x0*(alog(Egam(IG)/Ec)+ 0.5)*ax/rb)		  !  Original
        AY = dble(ay-nyu*x0*(alog(Egam(IG)/Ec)+ 0.5)*ay/rb)		  !  Original 
C
        DZ = 1.D00/DSQRT (1.D00 + AX*AX + AY*AY)
C----------------------------------------------------------------------------
C
CSdv- Vse eto, kak pokazal pryamoj test razreshenie ne uluchshaet  12.01.2016  
C
C-        rold=dsqrt(dx**2+dy**2)
C-        cphi=dx/rold
C-        sphi=dy/rold
C
C-        tantheta= rold/rb
C-        tm      = alog(egam(ig)/Ec)+0.5
C-        tantheta= tantheta/(1+nyu*tm*dz*x0/rb)
C-        dz   = dsqrt(1./(1.+tantheta**2))
C        
C-        rnew = rb*tantheta
C-        dx   = rnew*cphi
C-        dy   = rnew*sphi
C-        ax   = dx/rb
C-        ay   = dy/rb
C.............................................................

	DCOS (1,IG) = SNGL(AX*DZ)
	DCOS (2,IG) = SNGL(AY*DZ)
	DCOS (3,IG) = SNGL(DZ)
   10 CONTINUE
C
      if (NGAM.ne.2) return
C
C     Opening angle for pi0 --> 2g process to test MC Z_target 
C      
      COS12= DCOS(1,1)*DCOS(1,2)+DCOS(2,1)*DCOS(2,2)+DCOS(3,1)*DCOS(3,2)
      SIN12= SQRT((1.-COS12)/2.)
      call hf1(20141,SIN12,1.)
      RETURN
      END
C
      function find_SinAlpha(tmd, z0d, xd)
      implicit double precision(A-H, O-Z)
      complex*16 dews, tm, z0, x, cfind_SinAlpha
      complex*16 i/(0.0d0,-1.0d0)/
      data sq3rt2 /1.587401051968199474751706d0 /! 2**(2./3.)
      data sqrt3  /1.732050807568877293527446d0 /! sqrt(3.)
      save sq3rt2,sqrt3    
C         
      tm = dcmplx(tmd,0.0d0)
      z0 = dcmplx(z0d,0.0d0)
      x  = dcmplx( xd,0.0d0)
C
      dews = (18*tm**2*x - 2*x**3 - 9*tm*x*z0 + 
     -     cdSqrt((18*tm**2*x - 2*x**3 - 9*tm*x*z0)**2 + 
     -     4*(-x**2 - 3*tm*(tm + z0))**3))**0.33333333333333333333d0
      if(xd.gt.0) then
         cfind_SinAlpha = x + (1.0d0-i*sqrt3)*(x**2+3.0d0*tm*(tm+z0))/
     -     (sq3rt2*dews)+(1.0d0+i*sqrt3)*dews/sq3rt2**2
      else
         cfind_SinAlpha = x + (1.0d0+i*sqrt3)*(x**2+3.0d0*tm*(tm+z0))/
     -     (sq3rt2*dews)+(1.0d0-i*sqrt3)*dews/sq3rt2**2
      endif
      cfind_SinAlpha=cfind_SinAlpha/(3.d0*tmd)
c
      find_SinAlpha=dreal(cfind_SinAlpha)
      if(dabs(xd).lt.0.001) find_SinAlpha = xd/(z0d+tmd)
      return
      end

