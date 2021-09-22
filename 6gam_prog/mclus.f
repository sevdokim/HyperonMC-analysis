      subroutine mclus (ncell,ixmin,ixmax,iymin,iymax,ix1par,
     *                  ien,nxwall,nywall,ifwl,ok)
c--------------------------------------------------------------------------
c  This routine search for new cluster in the current event
c
c  Creation:      O4.O3.88      Chernov N.I.
c  edition:       O8.09.90      NLR
c  Last eddition  16.08.07      Sdv
c--------------------------------------------------------------------------
c
      dimension ien(NXWALL,NYWALL)
C     LOGICAL XLONG
      COMMON /LOSSD / LOSSD(70)
      dimension bad1(26),bad2(26)
      data N26,bad1 / 0, 26*0/, bad2 /26*0/
Csdv- data bad1/1, 1, 1, 1, 2, 2, 5, 6, 6, 6,19,21,23,24, 7, 7,12,12,
C    &         13,14,15,15,15,16,18,18/
C     data bad2/1, 5, 8, 9, 1, 8, 6, 1, 7, 8,19, 3,22, 9, 6,10, 3, 5,
Csdv-&          4, 3, 3, 7,19,16, 2, 7/
      logical ok
Csdv-
c      if (nxwall.eq.24) then
c      do i=1,24 
c      do j=1,24
c      ien(i,j) = 0
c      do k=1,N26
c      if (i.eq.bad1(k).and.j.eq.bad2(k)) ien(i,j)=1
c      enddo
c      enddo
c      enddo
c      do j=1,24
c      write (*,'(24i3)') (ien(i,j),i=1,24)
c      enddo
c      stop
c      endif
Csdv-
      ncell=0
      ix1=ix1par
c   --------------------- find first cell of a cluster -----------
c      do iy=1,nxwall
c      write(*,'(8i5)')(ien(j,iy),j=1,8)
c      enddo
c      stop

      IFWL=0
      do  2  ix=ix1,NXWALL
      do  1  iy=1,NYWALL
         if (ien(ix,iy).ne.0) goto 3
   1  continue
   2  continue
c                     --- no clusters left ---    
      goto 20
c   --------------------- begin to extract the cluster ------
   3  ncell=1
      ix1par=ix
      ien(ix,iy)=-ien(ix,iy)
      IFWL=IFWL-ien(ix,iy)
c
      jxmin=ix
      jxmax=ix
      jymin=iy
      jymax=iy
c
   4  ix1=max0(jxmin-1,1)
      ix2=min0(jxmax+1,NXWALL)
      iy1=max0(jymin-1,1)
      iy2=min0(jymax+1,NYWALL)
c
      kods=0
c   -------------- look through a neighbourhood of the cluster -----
c
      do  12  iy=iy1,iy2
      do  10  ix=ix1,ix2
c
      if (ien(ix,iy).le.0) goto 10
c
      if(ix.gt.1) then
        if(nxwall.eq.24)then
          do i=1,N26
            if(bad1(i).eq.(ix-1).and.bad2(i).eq.iy) then
              ok=.false.
              goto 20
            endif
          enddo
        endif
        if (ien(ix-1,iy).lt.0)      goto 6
      endif
      if(ix.lt.nxwall) then
        if(nxwall.eq.24)then
          do i=1,N26
            if(bad1(i).eq.(ix+1).and.bad2(i).eq.iy) then
              ok=.false.
              goto 20
            endif
          enddo
        endif
        if (ien(ix+1,iy).lt.0)      goto 6
      endif
      if(iy.gt.1) then
        if(nxwall.eq.24)then
          do i=1,N26
            if(bad1(i).eq.ix.and.bad2(i).eq.(iy-1)) then
              ok=.false.
              goto 20
            endif
          enddo
        endif
        if (ien(ix,iy-1).lt.0)      goto 8
      endif
      if(iy.lt.nxwall) then
        if(nxwall.eq.24)then
          do i=1,N26
            if(bad1(i).eq.ix.and.bad2(i).eq.(iy+1)) then
              ok=.false.
              goto 20
            endif
          enddo
        endif
        if (ien(ix,iy+1).lt.0)      goto 8
      endif
c
       goto 10
c   -------------- new element! adjoin it to the cluster ---------
   6  kods=1
      ncell=ncell+1
      ien(ix,iy)=-ien(ix,iy)
      IFWL=IFWL-ien(ix,iy)
c
      if (jxmin.gt.ix) jxmin=ix
      if (jxmax.lt.ix) jxmax=ix
c     write(*,*)'imeetsia vyhod po shesterke'
c
      goto 10
c
   8  kods=1
      ncell=ncell+1
      ien(ix,iy)=-ien(ix,iy)
      IFWL=IFFL-ien(ix,iy)
c
      if (jymin.gt.iy) jymin=iy
      if (jymax.lt.iy) jymax=iy
c     write(*,*)'imeetsia vyhod po vosmerke'
c
  10  continue
  12  continue
c
      if (kods.ne.0) goto 4
c  --------------------------- all elements have been found -------
      ixmin=jxmin
      ixmax=jxmax
      iymin=jymin
      iymax=jymax
C
 20   return
      end
