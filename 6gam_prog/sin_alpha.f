      function sin_alpha(tm, z0, x)
      implicit double precision(A-H, O-Z)
      IMPLICIT INTEGER(I-N)
C      
      DEWS = (-tm**6 + 3*tm**4*(x**2 + z0**2) + (x**2 + z0**2)**3 -
     +                 3*tm**2*(x**4 - 16*x**2*z0**2 + z0**4) +
     +                 6*Dsqrt(3.d0)*tm*z0*Dsqrt(x**2*
     +        -tm**6 + 3*tm**4*(x**2 + z0**2) + (x**2 + z0**2)**3 -
     + 3*tm**2*(x**4 - 7*x**2*z0**2 + z0**4))))**0.3333333333333333d0
      SQDEWS = DSqrt(DEWS + 3*x**2 - 2*(-tm**2 + x**2 + z0**2) +
     +      (-tm**2 + x**2 + z0**2)**2/DEWS)
      sin_alpha = (-(DSqrt(3.d0)*SQDEWS) + 3*x + DSqrt(3.d0)*
     +  DSqrt(-DEWS + 6*x**2 + (6*DSqrt(3.d0)*x*(tm**2 + z0**2))/
     + SQDEWS - 4*(-tm**2 + x**2 + z0**2) - (-tm**2 + x**2 + z0**2)**2
     + /DEWS))/(6*tm)
      return
      end 
