      subroutine new_clb(icl,efm)
      implicit none
      COMMON /CLLIM / nclus,IXD(60),IXU(60),IYD(60),IYU(60)
      COMMON /W2GEOM/ XNET2(25),YNET2(25),HALFX2,HALFY2
      COMMON /WSGEOM/ XNETS(9),YNETS(9),HALFXS,HALFYS
      COMMON /SHOWR / NCLUST,nclws,NCELLC(60),XSHW(3,60),
     1                YSHW(3,60),ESHW(3,60),iamp_clus(60),DUM(60)
      COMMON /NUM/ iCHECK,nums(8,8),num2(24,24),
     &             cX(640),cY(640)
      COMMON /CLB2  / CLB2(24,24),CLBS(8,8) ! <-- calibration coefficients
      real CLB2,CLBS,mass, sigma,cX,cY,N
      integer nclus,IXD,IXU,IYD,IYU,NCLUST,nclws,NCELLC,iamp_clus
      real XNET2,YNET2,HALFX2,HALFY2
      real XNETS,YNETS,HALFXS,HALFYS
      real XSHW,YSHW,ESHW,DUM,efm
      integer icl,II,NXS,NYS,NX2,NY2,i,j,jj,nums,num2,iCHECK
      real stXs,stYs,stX2,stY2
      logical center, gm, nm,bm, exists
      dimension icl(2),mass(640),sigma(640),N(640)
c-----Number for every cell
      IF(iCHECK.eq.0) THEN
      jj = 0
      do i = 1,8
        do j = 1,8
          jj = jj + 1
          nums(j,i) = jj 
          cX(jj) = j
          cY(jj) = i
        enddo
      enddo
      do i = 1,24
        do j = 1,24
          jj = jj + 1
          num2(j,i) = jj
          cX(jj) = j
          cY(jj) = i
        enddo
      enddo
      iCHECK = 1
    
      inquire(file='mass.dat',exist=exists)
      IF (exists) THEN
      open(270,file='mass.dat',form='formatted',
     &               STATUS = 'unknown')
      do i = 1,640
        read(270,*) mass(i),sigma(i),N(i)
c gm - "good mass", nm - "not so bad mass", bm - "bad mass"
        gm = mass(i).ge.125..and.mass(i).le.145.
        nm = mass(i).gt.0..and.mass(i).lt.125..or.
     &       mass(i).gt.145.
        bm = mass(i).eq.0.
      if(i.lt.64) then
        call hf2(-12001,10.+cX(i)/2.,10.+cY(i)/2.,mass(i))
       if(gm) call hf2(-12002,10.+cX(i)/2.,10.+cY(i)/2.,mass(i))
       if(nm) call hf2(-12003,10.+cX(i)/2.,10.+cY(i)/2.,mass(i))
       if(bm) call hf2(-12004,10.+cX(i)/2.,10.+cY(i)/2.,1.)
      else
        call hf2(-12001,cX(i)    ,cY(i)    ,mass(i))
        call hf2(-12001,cX(i)-0.5,cY(i)    ,mass(i))
        call hf2(-12001,cX(i)    ,cY(i)-0.5,mass(i))
        call hf2(-12001,cX(i)-0.5,cY(i)-0.5,mass(i))

       if(gm) call hf2(-12002,cX(i)    ,cY(i)    ,mass(i))
       if(gm) call hf2(-12002,cX(i)-0.5,cY(i)    ,mass(i))
       if(gm) call hf2(-12002,cX(i)    ,cY(i)-0.5,mass(i))
       if(gm) call hf2(-12002,cX(i)-0.5,cY(i)-0.5,mass(i))

       if(nm) call hf2(-12003,cX(i)    ,cY(i)    ,mass(i))
       if(nm) call hf2(-12003,cX(i)-0.5,cY(i)    ,mass(i))
       if(nm) call hf2(-12003,cX(i)    ,cY(i)-0.5,mass(i))
       if(nm) call hf2(-12003,cX(i)-0.5,cY(i)-0.5,mass(i))

       if(bm) call hf2(-12004,cX(i)    ,cY(i)    ,1.)
       if(bm) call hf2(-12004,cX(i)-0.5,cY(i)    ,1.)
       if(bm) call hf2(-12004,cX(i)    ,cY(i)-0.5,1.)
       if(bm) call hf2(-12004,cX(i)-0.5,cY(i)-0.5,1.)
      endif
        call hf1(-11001,mass(i) ,1.)
        call hf1(-11002,sigma(i),1.)
        call hf1(-11003,N(i)    ,1.)    
      enddo
      close(270)
      ENDIF
      ENDIF

C-----Coordinate of claster center 
      stXs = XNETS(2) - XNETS(1)
      stYs = YNETS(2) - YNETS(1)
      stX2 = XNET2(2) - XNET2(1)
      stY2 = YNET2(2) - YNET2(1)
      DO II = 1,2
C-----center = .true. if cell is in center of LGD2
      center = .false.

      IF(XSHW(1,ICL(II)).le.(XNET2(1 )+stX2*0.3).or.
     &   XSHW(1,ICL(II)).ge.(XNET2(25)-stX2*0.3).or.
     &   YSHW(1,ICL(II)).le.(YNET2(1 )+stY2*0.3).or.
     &   YSHW(1,ICL(II)).ge.(YNET2(25)-stY2*0.3)) go to 1

        if(XSHW(1,ICL(II)).ge.XNETS(1).and.
     &     XSHW(1,ICL(II)).le.XNETS(9).and.
     &     YSHW(1,ICL(II)).ge.YNETS(1).and.
     &     YSHW(1,ICL(II)).le.YNETS(9)) then
          NXS =int((XSHW(1,ICL(II))-XNETS(1))/stXs+1)
          NYS =int((YSHW(1,ICL(II))-YNETS(1))/stYs+1)
          center = .true.
        else
          NX2 =int((XSHW(1,ICL(II))-XNET2(1))/stX2+1)
          NY2 =int((YSHW(1,ICL(II))-YNET2(1))/stY2+1)
        endif

C-----Histogram filling
      IF(center) THEN
        call hf1(-10000-nums(NXS,NYS),efm,1.)
      ELSE
        call hf1(-10000-num2(NX2,NY2),efm,1.)
      ENDIF
        call hf1(-11000,efm,1.)
        call hf2(-12000,XSHW(1,ICL(II)),YSHW(1,ICL(II)),1.)
        call hf1(-11004,XSHW(1,ICL(II)),1.)
        call hf1(-11005,YSHW(1,ICL(II)),1.)
      ENDDO

 1    continue
      return
      end
