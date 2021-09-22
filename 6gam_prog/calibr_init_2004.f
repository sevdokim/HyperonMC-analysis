      subroutine calibr_init
c=====================================================================
c     Initialisation of the LGD2 calibration code
c     Author  Nadia Russakovich 13.12.03
c     Updated: 15.12.03 NR
c=====================================================================
c
      common /cfread / space(1000)
c-- lost events
      integer lossd
      COMMON /LOSSD /  LOSSD(70)
      integer lunin,lunout,lunst
      COMMON /LUN   /  LUNIN,LUNOUT,LUNST
        COMMON /CLB2  / CLB2(576),CLBS(64)
        COMMON /CLB2O / CLB2O(576),CLBSO(64)
        COMMON /IW2CUT/  IWBCUT(576),IWSCUT(64)
c--  number of run to proceed
        COMMON /NRUN  / NRUN
c-- cell boundaries and half sizes 
        COMMON /W2GEOM/ XNET2(25),YNET2(25),HALFX2,HALFY2
        COMMON /WSGEOM/ XNETS(9),YNETS(9),HALFXS,HALFYS
c-- cells number in X,Y and total, left and down LGD2 positions, big cell sizes
        COMMON /W2SET / NXW2,NYW2,NCELW2,XW2L,YW2D,DXCW2,DYCW2
c-- small cells number in X,Y and total, left and down positions, cell sizes
        COMMON /WSSET / NXWS,NYWS,NCELWS,XWSL,YWSD,DXCWS,DYCWS
        COMMON /ZWALLS/ ZWALL1,ZWALL2,ENORM
      common /minihi/ matrix(10,10),b(10),alfa1(100),kkk,istep
      real matrix
      integer addr,ibig(900),kkk
      integer cellnum(900)
      common /address/ addr,cellnum,ibig
      integer nchannel
      common /nchann/ nchannel
      real a,clbtmp(192),alfa1,alfa2
       COMMON /TARG  / XTARG,YTARG,ZTARG
cmike
c      COMMON /BADCB /  NCBDB,ICBADB(35)
c      common /bb / ncbdb2,ib2(10)
c      common /bbb/ ncbdb3,ib3(15)
cmike
cmike
       common /chisquare/ chi
cmike
c-- Local variables
       character*80 runname,calcoef, coefnew,lgdcut
cmike
       chi = 0.
cmike
        OPEN (4,FILE='calibr.cards',STATUS='OLD')

        call ffinit(1000)
        call ffset ('LINP',4)
        call user_cards
        call ffgo

cmike
c        NCBDB = NCBDB + ncbdb2 + ncbdb3
c        do i = 11,20
c          ICBADB(i) = ib2(i-10)
c        enddo
c        do i = 21,35
c          ICBADB(i) = ib3(i-20)
c        enddo
cmike

c-- cell boundaries and half sizes calculation
        DO 10 I = 1,NXW2+1
 10        XNET2(I) = XW2L + DXCW2*(I - 1)
        DO 11 I = 1,NYW2+1
 11        YNET2(I) = YW2D + DYCW2*(I - 1)
        HALFX2 = DXCW2/2.
        HALFY2 = DYCW2/2.
        DO 12 I = 1,NXWS+1
 12        XNETS(I) = XWSL + DXCWS*(I - 1)
        DO 13 I = 1,NYWS+1
 13        YNETS(I) = YWSD + DYCWS*(I - 1)
        HALFXS = DXCWS/2.
        HALFYS = DYCWS/2.

      lunin = 11
      lunout = 80
!       open(lunout,file='wall2_calibr.log',status='unknown',
!      *     form='formatted')
!       close(lunout)
CSdv-      open(21,file='hard_soft.dat',status='old',form='formatted')

      OPEN (LUNIN,FILE='file_names.dat',STATUS='OLD',FORM='FORMATTED')
      read (lunin,*) calcoef
      open (12,file=calcoef,status='OLD',form='FORMATTED')      ! koefy ot
c      open (12,file='lgd_coeff_m',status='OLD',form='FORMATTED') ! viktorova
      read (lunin,*) coefnew
      open (13,file=coefnew,status='UNKNOWN',form='FORMATTED')
      read (lunin,*) lgdcut
      open (14,file=lgdcut,status='OLD',form='FORMATTED')

      READ (12,5) CLB2,CLBS     ! koefy ot
      do i=1,576                !
         clb2(i)=clb2(i)*ENORM  ! 
      enddo                     ! Popravka energii na ENORM iz  
      do i=1,64                 ! 
         clbs(i)=clbs(i)*ENORM  ! 
      enddo                     !
C                          !
        READ (14,15) IWBCUT,IWSCUT
        DO 20 I = 1,576
20        CLB2O(I) = CLB2(I)
        DO 30 I = 1,64
30        CLBSO(I) = CLBS(I)
        close(12)
        close(14)

       call array_init
CSdv-             do i = 1,nchannel
CSdv-              read (21,*) addr,cellnum(addr-1919),ibig(addr-1919)
CSdv-             enddo
CSdv-             close (21)  

      call booking
CSdv-      ztarg=zwall22-zwall1
CSdv-      zwall1=zwall12
CSdv-      zwall2=zwall22
       zwall2=zwall1
c
c      write(*,5) CLB2,CLBS
c------ read koefs for complex energy correction--------------
      open(27,file='e_cor_matrix.dat',form='formatted',status='old')
      read(27,*) kkk
      read(27,*) istep
      read(27,1) matrix
      read(27,2) b
      read(27,3)alfa1
      close(27)
      do i=1,10
         b(i)=0.
         do j=1,10
            matrix(i,j)=0.
         enddo
      enddo
5     FORMAT (48(12F7.3/),8(8f7.3/))
15    FORMAT(24(24I3/),8(8I3/))
 1    format(10(10E23.10/))
 2    format(10E23.10/)
 3    format(10(10E23.10/))
      return 
      end
