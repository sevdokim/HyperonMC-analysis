C ..
      SUBROUTINE WRCOEF ( OK )
C=================================================================
C     SAVE OF  CALIBRATION COEFITIENTS                           =
C     FOR DECEMBER SESSION.                                      =
C                                                                =
C     12.05.91   CGA                                             =
C     LAST CORRECTION  28.10.91  NLR                             =
C=================================================================

      PARAMETER ( NXWAL = 24, NYWAL = 24, NCELW = NXWAL*NYWAL )

      common /LUN   / LU,LUNOU,LUNST
      common /CLB2  / CLB2(NCELW),CLBS(64)
      common /CALIB / NSTEP,NSTMAX,NITER(NCELW)
      common /coefs/ coefs_s(64),coefs_b(576),w_s(64),w_b(576)
      common /weight2/ w2big(576),w2small(64)
      COMMON /CELFIL/ ICFIL(576),ISFIL(64)
      LOGICAL OK

      real sigb(576),sigs(64),d,w,sigb2(576),sigs2(64)
C ..

      j=0
      do i=1,576
         sigb(i)=0.
         if(icfil(i).ne.0) then
            d=w2big(i)/float(icfil(i))
            w=w_b(i)/float(icfil(i))
            if(w.ne.0.) then
               sigb2(i)=sqrt(abs(d-w**2))/w
               sigb(i)=sqrt(abs(d-w**2))/(w*sqrt(float(icfil(i))))
               j=j+1
            endif
         endif
      enddo
c      write(*,*)'jjjjjjj',j
      j=0
      do i=1,64
         sigs(i)=0.
         if(isfil(i).ne.0) then
            d=w2small(i)/float(isfil(i))
            w=w_s(i)/float(isfil(i))
            if(w.ne.0.) then
               sigs2(i)=sqrt(abs(d-w**2))/w
c               write(*,*)sqrt(abs(d-w**2)),d,w**2,sigs2(i)
               sigs(i)=sqrt(abs(d-w**2))/(w*sqrt(float(isfil(i))))
               j=j+1
            endif
         endif
      enddo
c      write(*,*)'jjjjjjj',j

      open(72,FILE = 'delta.dat',form='formatted',STATUS = 'unknown')
      write(72,*)'delta for calibr coefs, delta=-1+mpi0t*weight/mpi0r'
      write(72,1001)coefs_b,coefs_s
      write(72,*)'weights, W=(weight1+weight2+...)'
      write(72,1002)w_b,w_s
      write(72,*)'D, D=(weight1**2+weight2**2+...)'
      write(72,1002)w2big,w2small
      write(72,*)'statistika, N:'
      write(72,1003)icfil,isfil
      write(72,*)'sigma, sigma=sqrt(D-W**2)/W'
      write(72,1001)sigb2,sigs2
      write(72,*)'sigma/sqrt(N)'
      write(72,1002)sigb,sigs
      close(72)
      NSTEP  = NSTEP + 1
      WRITE(LUNOU,1010) NSTEP,NSTMAX
      WRITE(13,1000) CLB2,CLBS
      close(13)
      OK =.TRUE.
      IF (NSTEP.LT.NSTMAX) OK = .FALSE.
      RETURN
C ..
1000  FORMAT(48(12F7.3/),8(8f7.3/))    
1001  format(24(24F8.5/),8(8f8.5/))
1002  format(24(24F8.2/),8(8f8.2/))
1003  format(24(24I8/),8(8I8/))
1010  FORMAT(//,'  **************************************** '/,
     +          '  *   NUMBER     OF ITERATION  =',I5,'    *'/,
     +          '  *   MAX.NUMBER OF ITERATION  =',I5,'    *'/,
     +          '  **************************************** '/)

      END

