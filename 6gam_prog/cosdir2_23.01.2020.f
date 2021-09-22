
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
Cbad- data x0,Ec/25.3,13.69/          ! x0 - rad dlina v mm, Ec - v MeV
Cbad- data x0,Ec/29.3,13.69/          ! x0 - rad dlina v mm, Ec - v MeV
      
      REAL*8 AX, AY, RB, DZ
      real*8 tantheta,dx,dy,tm,rnew,cphi,sphi,costheta,rold
      real nyu

CSdv- nyu=0.62
CSdv- nyu=0.45            !  22.01.2020
      nyu=0.35            !  23.01.2020
      DO 10 IG = 1, NGAM
        dx=DBLE(XGAM(IG) - XTARG)
        dy=DBLE(YGAM(IG) - YTARG)
        RB = DBLE(ZWALL  - ZTARG) 
    
        AX = dx/RB
        AY = dy/RB
        DZ = 1.D00/DSQRT (1.D00 + AX*AX + AY*AY)
C Sdv+
C------------------igra vokrug utochneniya koordinat-----
        AX = dble(ax-nyu*x0*(alog(Egam(IG)/Ec)+ 0.5)*ax/rb)
        AY = dble(ay-nyu*x0*(alog(Egam(IG)/Ec)+ 0.5)*ay/rb)
C
C-      AX = dble(ax+x0*(alog(Egam(IG)/Ec)+ 0.5)*ax/rb)   !  
C-      AY = dble(ay+x0*(alog(Egam(IG)/Ec)+ 0.5)*ay/rb)   !	 
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
      if (NGAM.ne.2) return
C
C     Opening angle for pi0 --> 2g process to test MC Z_target 
C      
      COS12= DCOS(1,1)*DCOS(1,2)+DCOS(2,1)*DCOS(2,2)+DCOS(3,1)*DCOS(3,2)
C-    RQ1  = DCOS(1,1)*DCOS(1,1)+DCOS(2,1)*DCOS(2,1)+DCOS(3,1)*DCOS(3,1)
C-    RQ2  = DCOS(1,2)*DCOS(1,2)+DCOS(2,2)*DCOS(2,2)+DCOS(3,2)*DCOS(3,2)
C-    write(*,*) 'COS12,RQ1,RQ2=', COS12,RQ1,RQ2 
C
      SIN12 = SQRT((1.-COS12)/2.)
      call hf1(20141,SIN12,1.)
      RETURN
      END


