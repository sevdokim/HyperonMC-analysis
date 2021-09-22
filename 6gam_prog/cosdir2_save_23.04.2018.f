
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
      real nyu

      nyu=0.62
      DO 10 IG = 1, NGAM
        dx=DBLE(XGAM(IG) - XTARG)
        dy=DBLE(YGAM(IG) - YTARG)
        RB = DBLE(ZWALL  - ZTARG) 
    
        AX = dx/RB
        AY = dy/RB
        DZ = 1.D00/DSQRT (1.D00 + AX*AX + AY*AY)

c------------------igra vokrug utochneniya koordinat-----
c        AX = dble(ax-x0*alog(Egam(IG)/Ec)*ax/rb)
c        AY = dble(ay-x0*alog(Egam(IG)/Ec)*ay/rb)
c-------------------------------------------------------
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
C-    rnew = rb*tantheta
C-        dx   = rnew*cphi
C-        dy   = rnew*sphi
C-        ax   = dx/rb
C-        ay   = dy/rb
C....................................

        DCOS (1,IG) = SNGL(AX*DZ)
        DCOS (2,IG) = SNGL(AY*DZ)
        DCOS (3,IG) = SNGL(DZ)
   10   CONTINUE
C
      RETURN
      END


