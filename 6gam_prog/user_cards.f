c
      subroutine USER_CARDS
c=======================================================================
c     routine for reading data cards by FFREAD package
c     Author N.Russakovich  17.11.03
c     Updated: 
c========================================================================
c
c-- bad big cells
cmike
      COMMON /BADCB /  NCBDB,ICBADB(20)
c      COMMON /BADCB /  NCBDB,ICBADB(35)
cmike
Cevd+ fortran can read only 80 characters from 1 string from file so to add more bad cells we need to add new line in calibr.cards and new common block
      COMMON /BADCB2 /  NCBDB2,ICBADB2(20)
      COMMON /BADCB3 /  NCBDB3,ICBADB3(20)  ! ;kondr
      COMMON /BADCB4 /  NCBDB4,ICBADB4(20)  ! ;kondr
c-- bad small cells
      COMMON /BADS  /  NCBDS,ICBADS(20)
c-- minimal total energy and amplitude in LGD2
      COMMON /NEVMAX/  NEVMAX
      real etotl
      integer amptotl
      COMMON /elim  /  ETOTL,amptotl
c-- kinematical cuts: shower energy, angles, eff.mass
      real emin2,angmax,efmin,efmax
      COMMON /KINCUT/ EMIN2,angmax,efmin,efmax
c-- lun numbers for input, output and histogram storing
      integer lunin,lunout,lunst
      COMMON /LUN   /  LUNIN,LUNOUT,LUNST
c-- particle momenta and masses
      real plab,pi0mass
      COMMON /MOMENT/pi0mass, PLAB(3,60)
c-- minimal hits number in cell
      integer minst
      COMMON /MINST / MINST,TWOSIG
      integer nfproc,nprint(7)
c-- number of runs to proceed
      COMMON /NEOF  / nfproc
c-- control print keys
      COMMON /NPRNT / NPRINT
c-- control print period nprev and flag lprint
      integer nprev
      logical lprint
      COMMON /PRFLAG/ NPREV,LPRINT
c---NT=1 - NTUPLE, NT=0 - HISTOGRAMMS, ICON=0 - NEW, ICON=1 - CONTINUE
      integer nt,icon
      COMMON /NTSTAT/ NT,icon
c-- Ntuple type big (nttype=1) small (nttype=0)
      common /nttype/ nttype
c-- target position
      COMMON /TARG  / XTARG,YTARG,ZTARG
c-- cells number in X,Y and total, left and down LGD2 positions, big cell sizes
      COMMON /W2SET / NXW2,NYW2,NCELW2,XW2L,YW2D,DXCW2,DYCW2
c-- small cells number in X,Y and total, left and down positions, cell sizes
      COMMON /WSSET / NXWS,NYWS,NCELWS,XWSL,YWSD,DXCWS,DYCWS
c-- z positions of LGD1 (not used) and LGD2
      COMMON /ZWALLS/ ZWALL1,ZWALL2
c-        COMMON /ZWALLS2/ ZWALL12,ZWALL22
c-- Target thickness
c        common /TTHICKNESS/ THICKNESS
cmike
      common/ptc/ ptcut
Cevd+ modified cherenkovs 
      integer c1c2c3_adr(4)
      common/triggers/ c1c2c3(3), c1min, c2min, c3min, c1c2c3_adr      
C     c1c23c(1,2,3) -- cherenkovs amplitudes; c1min,c2min,c3min -- cherenkovs cuts
C     c1c2c3_adr(1) -- isOldAddress?; c1c2c3_adr(2,3,4) -- cherenkovs addresses
c     data c1c2c3_/21,22,23/ ! new addresses from 2012-11
c     data c1c2c3_data/32,33,34/ ! novie adresa (posle runa 9366)//till the end of 2012-04
c     data c1c2c3_data/2688,2738,2740/ ! starie adresa (do runa 9366)
Cevd+ pileup protection
      integer pileup_adr(3),pileup_cut(2) ! 1st is (M4 in the past) protection
c     pileup_adr(1) -- isActivePileupProtection?; pileup_adr(2,3) -- pileup addresses
c     pileup_cut(1,2) -- pileup cuts
      common /pileup/ pileup_adr,pileup_cut
cmike
c
      common/eta0/etamass, etamin, etamax
c	real ZWAL12(2)
C	
C.................................................................................
Cevd+ default settings
      pileup_adr(1) = 0 ! no pileup protection
      c1c2c3_adr(1) = 1 ! new address format
      c1c2c3_adr(2) = 32 ! c1 address
      c1c2c3_adr(3) = 33 ! c2 address
      c1c2c3_adr(4) = 34 ! c3 address
C
        call ffkey('NEVE',nevmax,1,'I')
        call ffkey('NTTY',nttype,1,'I')
        call ffkey('LUNN',lunin,3,'I')
        call ffkey('HISB',nt,2,'I')
        call ffkey('NEOF',nfproc,1,'I')
        call ffkey('KPRN',nprint,7,'I')
        call ffkey('KFLG',nprev,1,'I')
        call ffkey('BCEL',nxw2,7,'M')
        call ffkey('SCEL',nxws,7,'M')
c       call ffkey('ZCAL',zwall12,2,'R')     ! cmike 18.05.2011
C       call ffkey('ZCAL',zwal12 ,2,'R')     !  Sdv+ 28.05.2009
        call ffkey('BADB',ncbdb,21,'I')
Cevd+
        call ffkey('BAD2',ncbdb2,21,'I')
        call ffkey('BAD3',ncbdb3,21,'I')     ! ;kondr
        call ffkey('BAD4',ncbdb4,21,'I')     ! ;kondr

Cevd-
        call ffkey('BADS',ncbds,21,'I')
        call ffkey('ETOT',etotl,2,'M')
        call ffkey('MASS',pi0mass,1,'R')
        call ffkey('KCUT',emin2,4,'R')
        call ffkey('TARG',xtarg,3,'R')
        call ffkey('STAT',minst,2,'M')
        call ffkey('ETA0',etamass,3,'R')
        call ffkey('PTCU',ptcut,1,'R')
        call ffkey('CHCU',c1min,3,'I')

Cevd+
        call ffkey('CHAD',c1c2c3_adr,4,'I')
        call ffkey('PLUP',pileup_adr,5,'I')
CSdv+
        if(zwall1.eq.0.)then
           write(*,*)'no Distance in file list!!!'
           stop
        endif

        return
        end
