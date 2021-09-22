c ----------------------------------------------------------
c     subroutine readeventgz(charName,lenName,ibuf0, 
c    &  			   Ihead,icounters,L,ibuf)     
      subroutine readeventgz(Ihead,ibuf,L,ok)
c     character *1 charName(*)      ! name of data, real dimension 
                                    ! of name is lenName
      dimension Ihead(*)            ! header, real dimension is 6
      dimension ibuf(*)             ! buffer of data	
c     dimension ibuf0(*)            ! buffer of midas data
c     dimension icounters(*)        ! icounter(1) - number of counters
                                    ! icounter(2) - counter N0 1 
c ----------------------------------------------------------
c
c read event, called from C++ (read zip-file type of *.gz)
c ------------------ 
c this subr. alsao zeroed initail value of counters of events and runs,
c read configuration file recons.conf and call hat_process 
c that initite all (hbook, etc) 
c 
c Ihead(1) - run number extracted from file name
c Ihead(5) - pedestal label extracted from file name (begin with blank)
c
c input  : L - length of data in array ibuf
c output : L=-1 - finish run
c          L=-2 - finish work
c ---------------------------------------------------------
c 
      logical ok
      integer addr,ibig(900)
      integer cellnum(900)
      common /address/ addr,cellnum,ibig
      integer nchannel
      common /nchann/ nchannel
      integer wrong_addr(10,2),double_addr
      common /wraddr/ double_addr,wrong_addr

      save izer,iev
      data izer/1/,iev/0/
      real a10,a20

      integer nafnaf_energy, wall_proc,mclus_bad,ef
      common /bad_something/ nafnaf_energy,wall_proc,mclus_bad,ef,efmm
      
      if(izer.eq.1)then
         izer = 0
         nchannel = 624 
c      write(*,*)'calibr_init,ihed=',Ihead(1),Ihead(2),Ihead(3)
         call calibr_init
      endif
c
c      do i=1,6
c         write(*,*)(i-1),') ',Ihead(i)
c      enddo
c      write(*,*)L
c      do i=1,50
c         write(*,*)(i-1),') ',ibuf(i)
c      enddo
c      stop
c      write(*,*)ibuf(1),ibuf(2),ibuf(3)
c      write(*,*)'event_decode,ihed=',Ihead(1)

      iev =iev+1
c                                             ==> iwl2,iwls      
      call event_decode(ibuf,ok,Ihead)
c    ----------------------------------            
c      
Cprnt-      write(*,*)'to ampl_energy, iev=',iev,ok
      if (.not.ok) goto 999
C                                             ==> IENER2,IENERS
      call ampl_energy(ok,Eout,Ihead)
c    ---------------------------------      
      
      if (.not.ok) goto 999
      call hf1(5035,Eout,1.)
c                                             ==> XSHW,YSHW,ESHW
      call wall2_proc(ok,Itype)
c    ---------------------------
      
CSdv- if(Itype.ne.0)write(*,*)'wall2_proc,iev=',iev,Itype     
 
      if (.not.ok) then 
         wall_proc=wall_proc+1
CSdv-	 
C-       call hf1(5038+Itype,Eout,1.)
         goto 999
      endif
      call hf1(5036,Eout,1.) 
c      
c     write(*,*)'calibr'
c    
      Efma=0.  
      call calibr(Esum,Efma,ok)
c    ---------------------------       
c      
      if (.not.ok) goto 999                                             ! ;kondr
c
C     Kinematic fit of 3&4 gamma events  
C
c     write(*,*)'kinematic fit'
c
      call hf1(5037,Esum,1.)
      call hf1(5038,Eout-Esum,1.)
      if (Efma.gt.0.) call hf1(5039,Efma,1.)
c
      call Cfit34(Esum,Efma)
c    ------------------------
c      
c     write(*,*)'kinematic fit - done'
c
 999  return
      end

      subroutine event_decode (ibuf,ok,Ihead)
c========================================================================
c     reads and decode events run December 2004
c     only LGD2
c     Author Nadia Russakovich
c     Updated: 12.12.04  NLR
c========================================================================


Cmike parameter(npawc=1100000)
      parameter(npawc=210000000)
      COMMON /PAWC  /  HMEM(npawc)

C---NT=1 - NTUPLE, NT=0 - HISTOGRAMMS, ICON=0 - NEW, ICON=1 - CONTINUE
        integer nt,icon
      COMMON /NTSTAT/ NT,icon

      dimension ibuf(*)
      dimension Ihead(*)            ! header, real dimension is 6

      integer inumb(24)
      common /AMPW2/ ncellb,ncells,iwl2(24,24),iwls(8,8)
      integer ifullb,ifulls
      COMMON /IFULLB/  IFULLB(24,24),IFULLS(8,8)

      integer addr,ibig(900)
      integer cellnum(900)
      common /address/ addr,cellnum,ibig

      integer hard_addr(24,24),hard_addr_small(8,8)
      integer pedest(24,24),pedest_small(8,8)
      integer sigma(24,24),sigma_small(8,8)
      common /caddress/ hard_addr,hard_addr_small,pedest,pedest_small,
     *                  sigma,sigma_small

      integer ixchan,iychan,nstart,nevent_read
      real*4 Amp
      logical ok,leof
      integer vector(32)
      logical pileup(2)

      integer linin,lunout,lunst,nevmax
      COMMON /LUN   /  LUNIN,LUNOUT,LUNST
      COMMON /NREC  /  nevent_read
      COMMON /FILEEN/  LEOF
      COMMON /NEVMAX/  NEVMAX
      common /ifd/ ifd
      
      integer amplhard
      common /hardamp/ nctot,amplhard(900)

      integer nchcrate, nchtot,nadccr,lenevent,identev
      integer runtype

      data nstart /1/
      integer nchannel
      common /nchann/ nchannel
      integer wrong_addr(10,2),double_addr
      common /wraddr/ double_addr,wrong_addr
      integer centr,irej,adr,x,y
      integer AdrtoXY(2,3000)
      common/AdrtoXY/AdrtoXY
      integer stancii(2,20)
      common/stancii/stancii

      integer nafnaf_energy, wall_proc,mclus_bad,ef
      common /bad_something/ nafnaf_energy,wall_proc,mclus_bad,ef,efmm
      integer c1c2c3_adr(4) 
      common /triggers/ c1c2c3(3), c1min, c2min, c3min, c1c2c3_adr
C     cherenkovs addresses are read from calibr.cards
C     c1c23c(1,2,3) -- cherenkovs amplitudes; c1min,c2min,c3min -- cherenkovs cuts
C     c1c2c3_adr(1) -- isOldAddress?; c1c2c3_adr(2,3,4) -- cherenkovs addresses
c     data c1c2c3_/21,22,23/ ! new addresses from 2012-11
c     data c1c2c3_data/32,33,34/ ! novie adresa (posle runa 9366)//till the end of 2012-04
c     data c1c2c3_data/2688,2738,2740/ ! starie adresa (do runa 9366)
      integer pileup_adr(3),pileup_cut(2) ! 1st is (M4 in the past) protection
c     pileup_adr(1) -- isActivePileupProtection?; pileup_adr(2,3) -- pileup addresses
c     pileup_cut(1,2) -- pileup cuts
      common /pileup/ pileup_adr,pileup_cut

c      data pileup_adr /36,37/  ! 2nd is (M4 in future protection)
c      data pileup_cut /10,400/
      
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
C      write(*,*)'BADB:', ncbdb, icbadb
C      write(*,*)'PLUP:', pileup_adr, pileup_cut
C      write(*,*)'CHAD:', c1c2c3_adr
C      write(*,*)'CHCU:', c1min,c2min,c3min
C      stop 
        
      ok = .true.
c
      call vzero(iwl2,576)
      call vzero(iwls,64)
      call vzero(amplhard,900)

      ncellb = 0
      ncells = 0
c      write(*,*)'0.5'
       lenevent = ibuf(4)/4
c      write(*,*)'0.6',lenevent
       identev = ibits(ibuf(1),0,16)
c       
c      write(*,*)'0.7'
c       if (identev.ne.1.and.identev.ne.10) then
c          write (*,*) 'Record identificator is wrong',identev
c          ok = .false.
c          return
c       endif
c      write(*,*)'0.6',identev
c      do i=1,20
c         write(*,*)i,' ibuf',ibuf(i)
c      enddo
c      stop
c      write(*,*)'ihead',Ihead(1)
c
       if (identev.eq.10.and.ibuf(6).eq.Ihead(1)) then
          nafnaf_energy=0
          wall_proc=0
          mclus_bad=0
	  efmm=0
          ef=0
c      write(*,*)'1'
      call hat_geometry(ibuf)
c      write(*,*)'Shapka so stanciyami prochitana'
c      do i=1,900
c      write(*,*)AdrtoXY(1,i),AdrtoXY(2,i),AdrtoXY(3,i)
c      enddo

c       if (nstart.eq.1) then
C
C --- Run header --- 
C
c         nstart = 0
          write(lunout,*) '     Run number',ibuf(6)
          runtype = ibits(ibuf(1),16,4)
          write(lunout,*) 'runtype =',runtype
c
c          do i = 1,32
c            vector(i) = ibits(ibuf(1),32-i,1)
c          enddo
c          write(lunout,*) 'first word =',vector 
          if(runtype.eq.1) write(lunout,*) '      physical'
          if(runtype.eq.2) write(lunout,*) '      pedestal'
          if(runtype.eq.4) write(lunout,*) '      pulser'
          if(runtype.eq.8) write(lunout,*) '      monitor'
          write(lunout,*) 'Event number in run',ibuf(7)
          nchtot = 0
          l1 = 11
          lencr = ibuf(l1)
          nadccr = ibuf(l1+3)
          iaddr = l1+4+nadccr+1
          nchcrate = ibuf(iaddr)
          nchtot = nchtot+nchcrate

          do i = 1,nchcrate
            irej=0
            ihadr = ibuf(iaddr+1+4*(i-1))
c      write(*,*)'adres:',ihadr
            if (ihadr.lt.stancii(2,1).or.ihadr.gt.stancii(2,7)+95) then
c               write(lunout,*) ' wrong ihadr=',ihadr
               go to 10
            endif
c      call geometry(ihadr,nx,ny,centr,irej)
      nx=AdrtoXY(1,ihadr)
      centr=AdrtoXY(2,ihadr)
      icfactor=((24*centr)+(8*(1-centr)))
      ny=(nx-1)/icfactor
c      ny=nx/icfactor
      nx=nx-ny*icfactor
      ny=ny+1
      if(nx.eq.0)goto 10
      if(centr.eq.1)then
            hard_addr(nx,ny) = ihadr
            pedest(nx,ny) = ibuf(iaddr+2+4*(i-1))
            sigma(nx,ny) = ibuf(iaddr+3+4*(i-1))
         else
            hard_addr_small(nx,ny) = ihadr
            pedest_small(nx,ny) = ibuf(iaddr+2+4*(i-1))
            sigma_small(nx,ny) = ibuf(iaddr+3+4*(i-1))
         endif

 10        continue
        enddo
        return
      endif

      if (identev.ne.10.or.ibuf(6).ne.Ihead(1)) then
c     
c--   trigger event
c     
         nevent_read = nevent_read + 1
	 
c     lenevent = ibuf(4)/4  !
         l1=6               !    Tak kak v midase dlia dannyh ubrana
c     l1=1                  !    shapka run'a.
!    No dlia posledney versii nuzhen imenno l1=6
c     lenevent =            !
         if(ibuf(l1+1).eq.17.and.ibuf(l1+2).eq.69)then
            call counters_process(ibuf,l1)
            l1=l1+ibuf(l1)
         endif
         if((ibuf(l1+1).eq.10.and.ibuf(l1+2).eq.301).or.
     &        (ibuf(l1+1).eq.0.and.ibuf(l1+2).eq.301))then
            call cameras_process(ibuf)
            l1=l1+ibuf(l1)
         endif
         if(ibuf(l1+1).ne.0.or.ibuf(l1+1).eq.17) then
            if(ibuf(l1+2).ne.71) then
               write(*,*)'no LGD2!!',l1,ibuf(l1+1),ibuf(l1+2)
               do i=1,25
                  write(*,*)i,' ibuf',ibuf(i)
               enddo
	       ok=.false.
	      return 
CSdv-         stop
            endif
         endif
         lencrate = ibuf(l1)
c     lencrate = ibuf(l1)-34   !  predpolozhenie, chto k nam
c     !  prosachivayutsia pereschetki
         numcrate = ibuf(l1+1)
         npair = (lencrate - 3)/2
         iaddr = l1+3           !  +4 - nachalo strukturnogo bufera
c     iaddr = l1+4    !
         call hf1(5004,float(npair),1.)
         a10=0.
         a20=0.
         bad_in_run=0
C7-	 
Cprnt-	 write(*,*) 'iaddr,npair=',iaddr,npair
C
C-	 call filter71( npair, ibuf(iaddr), ierr )
C-	 if (ierr.ne.0) then
C-		    ok=.false.
C-		    return
C-		endif
C7-	 
         do i = 1,npair
            irej=0
            ihadr = ibuf(iaddr+2*(i-1))
C	    
C-	    write(*,*)'i,ihadr,Iampl=',i,ihadr,ibuf(iaddr+2*i-1)
            call hf1(5005,float(ihadr),1.)
C	    
            if (ihadr.lt.stancii(2,1).or.ihadr.gt.stancii(2,7)+95) then
               bad_in_run=bad_in_run+1
C-	       write(*,*) 'bad_in_run=',bad_in_run
               go to 20
            endif
            iampl= ibuf(iaddr+2*i-1)
	    
C-	    write(*,*) 'From buffer: i=   ',i,ihadr,iampl 
	    
            call hf1(5006,float(iampl),1.)
            if (iampl.gt.10) then
               a10=a10+iampl
               if(iampl.gt.20) then
                  a20=a20+iampl
               endif
            endif
CSdv-         do in=0,95
C-               if(ihadr-stancii(1,7).eq.i)then
C-                  call hf1(5200+i,float(iampl),1.)
C-               endif
C-            enddo
            do ic=1,3
      if((ihadr-stancii(1,7)*c1c2c3_adr(1)).eq.c1c2c3_adr(ic+1)) then     ! novie adresa (posle runa 9366)
c     if(ihadr.eq.c1c2c3_data(ic))then ! starie adresa(do runa 9366)
                  c1c2c3(ic)=iampl
c     write(*,*) 'ihadr = ', ihadr
c     write(*,*) 'stancii(1,7)=',stancii(1,7)
c-    write(*,*) 'ampl in cherenkov=', iampl
                  call hf1(5100+ic,float(iampl),1.)
                  goto 20         !  Cherenkov counters are ommitted from the LG2 amplitudes
               endif
            enddo
C
C     cherenkovs selection
C     if(c1c2c3(1).gt.130.) ok=.false.
C     if(c1c2c3(3).gt.100.) ok=.false.
C
C     evd+ pileup discrimination:
            if(pileup_adr(1).gt.0) then ! pileup protection active
            if((ihadr-stancii(1,7)).eq.pileup_adr(2)) then ! past (M4 in strob) protection
               call hf1(5105  ,float(iampl),1.)
               if(iampl.gt.pileup_cut(1)) then
                  pileup(1)=.true.
                  ok = .false.
Cprnt-		  write(*,*)'pileup(1)=',ok
                  return
               endif
            endif
            if((ihadr-stancii(1,7)).eq.pileup_adr(3)) then ! future (M4 in strob) protection
               call hf1(5106  ,float(iampl),1.)
               if(iampl.gt.pileup_cut(2)) then  
                  pileup(2)=.true.
                  ok = .false.
Cprnt-		  write(*,*)'pileup(2)=',ok
                  return
               endif
            endif
            endif ! pileup protection active
C
C-	    if(.not.(pileup(1).or.pileup(2)))ok=.false. ! pile-up event selection, but it is already active 
C     evd-
            nx=AdrtoXY(1,ihadr)
            centr=AdrtoXY(2,ihadr)
            icfactor=((24*centr)+(8*(1-centr)))
            ny=(nx-1)/icfactor
c     ny=nx/icfactor
            nx=nx-ny*icfactor
            ny=ny+1
            if(nx.eq.0)goto 20
            if(centr.eq.1)then
c     write(*,*)iampl	  
            if(iampl.gt.2) then              !  only iampl>2 are going in cluster
               iwl2(nx,ny) = iampl  
               ifullb(nx,ny) = ifullb(nx,ny)+1
               ncellb = ncellb + 1
               if(iampl.gt.10) then
                  call hf2(5002,float(nx),float(ny),1.)
                  if (iampl.lt.500) then
                     call hf2(5022,float(nx),float(ny),1.)
                     call hf1(5009,float(ihadr),1.)
                  endif
                  if (iampl.lt.1500) then
                     call hf2(5052,float(nx),float(ny),1.)
                     call hf1(5059,float(ihadr),1.)
                  endif
                  if (iampl.ge.1500) then
                     call hf2(5062,float(nx),float(ny),1.)
                     call hf1(5069,float(ihadr),1.)
                  endif
               endif
	    endif
            else
	    if(iampl.gt.2) then              !  only iampl>2 are going in cluster
               iwls(nx,ny) = iampl
               ifulls(nx,ny) = ifulls(nx,ny)+1
               ncells = ncells + 1
               if(iampl.gt.10) then
                  call hf2(5003,float(nx),float(ny),1.)
                  if (iampl.lt.500) then
                     call hf2(5023,float(nx),float(ny),1.)
                     call hf1(5009,float(ihadr),1.)
                  endif
                  if (iampl.lt.1500) then
                     call hf2(5053,float(nx),float(ny),1.)
                     call hf1(5059,float(ihadr),1.)
                  endif
                  if (iampl.ge.1500) then
                     call hf2(5063,float(nx),float(ny),1.)
                     call hf1(5069,float(ihadr),1.)
                  endif
               endif
            endif
	    endif
 20         continue
         enddo
C
CSdv-    
Cprnt-   write(*,*) 'nevent_read=',nevent_read,ncellb,ncells
C-       write(*,'(24I6)') ((iwl2(nx,ny),nx=1,24),ny=1,24) 
C-       write(*,' (8I6)') ((iwls(nx,ny),nx=1,8),ny=1,8)   
C
         if(a10.gt.0) call hf1(5007,a10,1.)
         if(a20.gt.0) call hf1(5008,a20,1.)

         nctot = ncellb+ncells
         call hf1(700,float(nctot),1.)
	 
       endif ! end of trigger event
c
c      call wlprni2(iwl2,24,24,'big cells amplitude total')
c      call wlprni2(iwls,8,8,'small cells amplitude total')
c
c      if (nt.eq.1) then
c         call hfntb(11,'ampw2')
c         call hfntb(11,'amplcable')
c         call hfnt(11)
c      endif
c      write(*,*)'bad in run:',bad_in_run
 30   return
      end
