      program main
      implicit double precision(A-H, O-Z)
      IMPLICIT INTEGER(I-N)
      data tm, x, z0,one / 160.,200.,3700.,1./
      write(*,*) 'enter tm = '
      READ*,tm
      write(*,*) 'enter z0 = '
      READ*,z0
      write(*,*) 'enter x = '
      READ*,x
      write(*,*) 'alpha = ', find_SinAlpha(tm, z0,x)
      write(*,*) 'alpha = ', find_SinAlpha(tm, z0,-x)
      write(*,*) 'alpha = ', find_SinAlpha(tm, z0,x)
      stop
      end
C
      function find_SinAlpha(tmd, z0d, xd)
      implicit double precision(A-H, O-Z)
      complex*16 dews, tm, z0, x, cfind_SinAlpha
      complex*16 i/(0.0d0,-1.0d0)/
      data sq3rt2 /1.587401051968199474751706d0 /! 2**(2./3.)
      data sqrt3  /1.732050807568877293527446d0 /! sqrt(3.)
      save sq3rt2,sqrt3
      tm = dcmplx(tmd,0.0d0)
      z0 = dcmplx(z0d,0.0d0)
      x  = dcmplx( xd,0.0d0)
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
c     write(*,*) cfind_SinAlpha
      find_SinAlpha=dreal(cfind_SinAlpha)
CSdv- if(dabs(xd).lt.0.0001) find_SinAlpha = xd/(z0d+tmd)
      if(dabs(xd).lt.1.0) then
                          write(*,*)
                          write(*,*) 'find_SinAlpha_t=',find_SinAlpha
			  find_SinAlpha = xd/(z0d+tmd)
			  write(*,*) 'find_SinAlpha_a=',find_SinAlpha
			  write(*,*)
			  endif
      return
      end
