      subroutine ntup_book
c=====================================================================
c     CWN Ntuple booking for calibration
c     Author Nadia Russakovich 13.12.03
c     Updated:
c=====================================================================
c
        parameter(npawc=2 000 000)
      COMMON /PAWC  /  HMEM(npawc)

      integer hard_addr(24,24),hard_addr_small(8,8)
      integer pedest(24,24),pedest_small(8,8)
      integer sigma(24,24),sigma_small(8,8)
      common /caddress/ pedest,pedest_small,
     *                  sigma,sigma_small,hard_addr,hard_addr_small

      integer iwl2_big(24,24), iwl2_small(8,8) !,inumb(24)
      common /ampw2/ ncellb,ncells,iwl2_big,iwl2_small

      integer nsignal,nsign1,nsign2,ampl_cable(66,16)
      common /amplcabl/ nsignal,nsign1,nsign2,ampl_cable

      real entot
      integer iamptot
      common /etotal/ entot,iamptot

      integer nctot,ener2,eners
      COMMON /W2COPY/ nctot,ener2(24,24),eners(8,8)
      real effmass,angle
      integer numpi0
      common /twogmass/ numpi0,effmass(6),angle(6)
      common /pi0sel/ npisel,pi0mass(2),pi0ang(2)
      COMMON /efmean/ EFMb(24,24),EFMSM(8,8)

      COMMON /SHOWR / NCLUST,nclws,NCELLC(60),XSHW(3,60),
     1                YSHW(3,60),ESHW(3,60),iamp_clus(60),DUM(60)
        COMMON /CLLIM / nclus,IXMN(60),IXMX(60),IYMN(60),IYMX(60)

      call hbnt(333,'hyperon',' ')
c      call hbname(333,'etotal',entot,'entot:R,amptot:I')

      call hbname(333,'ampw2',ncellb,'ncellb:I,ncells:I,amp2(24,24):I,
     *                amps(8,8):I')
c      call hbname(333,'amplcabl',nsignal,'nsign,nsign1,nsign2,
c     *                 ampcabl:I')
c      call hbname(333,'w2copy',nctot,'nctot,ener2(24,24):I,
c     *                eners(8,8):I')
c      call hbname(333,'twogmass',numpi0,'numpi0[0,6],effmass(numpi0):R,
c     *     angle(numpi0):R')
      call hbname(333,'pi0sel',npisel,'npisel[0,2],
     *            pi0mass(npisel):R,pi0ang(npisel):R')
      call hbname(333,'efmean',efmb,'efmb(24,24):R,efms(8,8):R')
c      call hbname(333,'showr',nclust,'nclust[0,30],nclws[0,30],
c     *  ncellc(nclust),xshw(3,nclust),yshw(3,nclust),eshw(3,nclust),
c     *  ampsh(nclust)')
      call hbname(333,'cllim',nclus,'nclus[0,30],ixmn(nclus):I,
     *   ixmx(nclus):I,iymn(nclus):I,iymx(nclus):I')
                

      return
      end
