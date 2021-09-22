c-------------------------------------------------------------------------
c-             Procedury raspokovki geometrii
c-     perevodiat zhelezniy adres v koordinaty detektora
c-------------------------------------------------------------------------
      subroutine hat_geometry(ibuf)
c-  schityvanie nomerov stanciy. vyzyvat' iz procedury hat_process
      dimension ibuf(*)             ! buffer of data
      integer stancii(2,20)
      common/stancii/stancii
      integer AdrtoXY(2,3000),XxYloc(24,32),x,y,centr,irej,adr
      common/AdrtoXY/AdrtoXY
      integer ich,imodul,nx,ny,icfactor

c     chtenie tablicy podkliucheniya
      write(*,*)'<---------------------------->'
      write(*,*)'chtenie tablicy podkliucheniya'
      open(12,file='h_s_new.dat',status='old',form='formatted')
      do i=1,5
         read(12,*)
      enddo
      do i=1,7
        stancii(1,i)=ibuf(i+15)
        stancii(2,i)=ibuf(i+15)
        do j=0,95
           read(12,1000)adr,ich,centr,imodul
           adr=adr+stancii(1,i)
           AdrtoXY(1,adr)=ich
           AdrtoXY(2,adr)=centr
c           nx=ich
c           icfactor=((24*centr)+(8*(1-centr)))
c           ny=(nx-1)/icfactor
c           nx=nx-ny*icfactor
c           ny=ny+1
c           write(*,*)ich,nx,ny
c           call hf2(5019-centr*3,float(nx),float(ny),1.)
        enddo
      enddo
      close(12)
   11 b=0               ! sortirovka nomerov stanciy po vozrastaniyu
      do j=1,6
        if(stancii(2,j).gt.stancii(2,j+1)) then
          ID=stancii(2,j)
          stancii(2,j)=stancii(2,j+1)
          stancii(2,j+1)=ID
          b=1
        endif
      enddo
      if(b.eq.1)go to 11
 1000 format(I5,I5,I4,I6)
      end
