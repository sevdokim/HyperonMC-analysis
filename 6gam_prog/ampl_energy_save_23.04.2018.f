      SUBROUTINE  ampl_energy  ( OK, Eout,Ihead )
c==========================================================================
c     Amlitude - energy convertion for LGD2
c     Author  Nadia Russakovich  
c     Updated:  20.04.04  NR
c========================================================================= 
      dimension Ihead(*)            ! header, real dimension is 6
      logical ok,reject
        integer nt,icon
      COMMON /NTSTAT/ NT,icon
      integer ncellb,ncells,iwl2,iwls
      COMMON /AMPW2 /  ncellb,ncells,IWL2(576),IWLS(64)
      integer ncbdb,icbadb,ncbds,icbads
      COMMON /BADCB /  NCBDB,ICBADB(20)
      COMMON /BADCB2 /  NCBDB2,ICBADB2(20)
      COMMON /BADCB3 /  NCBDB3,ICBADB3(20)  ! ;kondr
      COMMON /BADCB4 /  NCBDB4,ICBADB4(20)  ! ;kondr
      COMMON /BADS  /  NCBDS,ICBADS(20)
      real clb2,clbs
      COMMON /CLB2  /  CLB2(576),CLBS(64)
      integer iener2,ieners
      COMMON /ENERW2/  IENER2(576),IENERS(64)
      integer nctot,ienr2,ienrs
      COMMON /W2COPY/ nctot,IENR2(24,24),IENRS(8,8)
C      real Gl
      integer amptotl
      COMMON /elim  /  ETOTL,amptotl
      real etot
      integer iamptot
      common /etotal/ etot,iamptot
      real myenergy
      common /myen/ myenergy
      integer ifullb,ifulls
      COMMON /IFULLB/  IFULLB(576),IFULLS(64)
      integer iwbcut,iwscut
      COMMON /IW2CUT/  IWBCUT(576),IWSCUT(64)
      integer lossd
      COMMON /LOSSD /  LOSSD(70)
      LOGICAL LPRINT
      COMMON /PRFLAG/  NPREV,LPRINT
      integer nxw2,nyw2,ncelw2,nxws,nyws,ncelws
      real xw2l,yw2d,xwsl,ywsd,dxcw2,dycw2,dxcws,dycws
      common /W2SET /  NXW2,NYW2,NCELW2,XW2L,YW2D,DXCW2,DYCW2
      COMMON /WSSET /  NXWS,NYWS,NCELWS,XWSL,YWSD,DXCWS,DYCWS

      integer nafnaf_energy, wall_proc,mclus_bad,ef
      common /bad_due_something/ nafnaf_energy, wall_proc,mclus_bad,ef
      common /energy_flow/ aver_map1(640),aver_map2(640),
     &                      hit_map1(640),hit_map2(640)
c
c      do i=1,24 
c      do k=1,24
c      if(IWL2(k+(i-1)*24).lt.100) IWL2(k+(i-1)*24) = 0
c      enddo
c	write(*,'(24i4)')(IWL2(j+(i-1)*24),j=1,24)
c      enddo
c      write(*,*)
c      do i=1,8 
c         write(*,'(8i4)')(iwbcut(j+(i-1)*8),j=1,8)
c      enddo
c      stop
c
        ETOT = 0.
        iamptot = 0
        ncellb = 0
        ncells = 0
        OK = .FALSE.
        reject=.false.
        nafnaf_energy=nafnaf_energy+1
cevd	
c$$$	write(*,*) 'Sdv-Sdv NCBDB', NCBDB, (ICBADB(j),j=1,NCBDB)
c$$$	write(*,*) 'Sdv-Sdv NCBDB2', NCBDB2, (ICBADB2(j),j=1,NCBDB2)
c$$$        write(*,*) 'Sdv-Sdv small', NCBDS, (ICBADS(j),j=1,NCBDS)
        DO 1 I = 1,NCBDB
          ICB = ICBADB(I)
Csdv-          IWL2(ICB) = 10000
          IWL2(ICB) = 0 
  1     CONTINUE
        DO 2 I = 1,NCBDS
          ICB = ICBADS(I)
Csdv-          IWLS(ICB) = 10000
          IWLS(ICB) = 0
  2     CONTINUE
        DO 3 I = 1,NCBDB2
           ICB = ICBADB2(I)
Csdv-          IWL2(ICB) = 10000
           IWL2(ICB) = 0 
 3      CONTINUE
!kondr++{
        do I = 1,NCBDB3
           ICB = ICBADB3(I)
           IWL2(ICB) = 0 
        enddo
        do I = 1,NCBDB4
           ICB = ICBADB4(I)
           IWL2(ICB) = 0 
        enddo
!kondr--}

        DO 30 I = 1,NCELW2
          IF (IWL2(I).LE.IWBCUT(I)) IWL2(I) = 0
          IF (IWL2(I).GT.0) then
             IFULLB(I) = IFULLB(I) + 1
             ncellb = ncellb + 1
          endif
Csdv-
C         Correction for calib coeff   
C
          IENER2(I) = CLB2(I)*IWL2(I)
Cevd+
CSdv-     if(iener2(i).gt.8000) reject=.true.     ! event rejection - this is bad
          if(iener2(i).gt.8500) reject=.true.     ! event rejection
          nx=(i-1)/24+1
          ny=(i-1)-(nx-1)*24+1
          call hf2(-75100,float(nx),float(ny),float(IENER2(I)))
          call hf1(-75300,float(IENER2(I)),1.)

Cevd
          if(iener2(i).gt.5) call hf1(5031,float(iener2(i)),1.)
          ETOT = ETOT + FLOAT(IENER2(I))
C-        write(*,*) 'ETOT=',ETOT,CLB2(I),IWL2(I)
          iamptot = iamptot + iwl2(i)
c-------popytka poiska chastey s uplyvshey energiey---------------
          if(10.eq.20)then
c          if(iener2(i).gt.0) then
             if(Ihead(1).lt.4200)then
                hit_map1(i)=hit_map1(i)+1
                aver_map1(i)=aver_map1(i)*(hit_map1(i)-1)/hit_map1(i)+
     &                       iener2(i)/hit_map1(i)
             else
                hit_map2(i)=hit_map2(i)+1
                aver_map2(i)=aver_map2(i)*(hit_map2(i)-1)/hit_map2(i)+
     &                       iener2(i)/hit_map2(i)
             endif
          endif
  30    CONTINUE
        DO 40 I = 1,NCELWS
          IF (IWLS(I).LE.IWSCUT(I)) IWLS(I) = 0
c          IF (IWLS(I).GT.700) IWLS(I) = 0
          IF (IWLS(I).GT.0) then
             IFULLS(I) = IFULLS(I) + 1
             ncells = ncells + 1
          endif
          IENERS(I) = CLBS(I)*IWLS(I)
Cevd+
CSdv-     if(ieners(i).gt.8000) reject=.true.     ! event rejection  - this is bad
	  if(ieners(i).gt.8500) reject=.true.     ! event rejection
          nx=(i-1)/8+1
          ny=(i-1)-(nx-1)*8+1
          call hf2(-75200,float(nx),float(ny),float(IENERS(I)))
          call hf1(-75400,float(IENERS(I)),1.)
Cevd          
c-----popytka poiska chastey s uplyvshim napriazheniem----------
          if(10.eq.20)then            ! LOL
c          if(ieners(i).gt.0) then
             ii=i+576
             if(Ihead(1).lt.4200)then
                hit_map1(ii)=hit_map1(ii)+1
                aver_map1(ii)=aver_map1(ii)*(hit_map1(ii)-1)/
     &                   hit_map1(ii)+ieners(i)/hit_map1(ii)
             else
                hit_map2(ii)=hit_map2(ii)+1
                aver_map2(ii)=aver_map2(ii)*(hit_map2(ii)-1)/
     &                   hit_map2(ii)+ieners(i)/hit_map2(ii)
             endif
          endif
          if(ieners(i).gt.5) call hf1(5031,float(ieners(i)),1.)
          ETOT = ETOT + FLOAT(IENERS(I))
c-	  write(*,*) ETOT
          iamptot = iamptot + iwls(i)
   40   CONTINUE
        if (nt.eq.0) then
	   CALL HF1(20120,float(iamptot),1.)
           CALL HF1(20121,ETOT,1.)
        endif
        nctot = ncellb + ncells
        myenergy=etot
        call hf1(5035,etot,1.)
        IF (ETOT.LT.ETOTL) RETURN
        if (iamptot.lt.amptotl) return
C
C-       write(*,*) etot,ETOTL,(clb2(i),i=1,4),(IENERS(I),i=1,4)
Cevd-       if (ETOT.gt.10000.) return 
Cevd+
       if(reject)return  ! event rejection
Cevd
       OK  =.TRUE.
       Eout= etot
       call hf2(-70060 ,float(nctot),etot,1.)
C       
        nafnaf_energy=nafnaf_energy-1
c       IF (.NOT.LPRINT) RETURN
c        CALL BWLPRNI(IENER2,NXW2,NYW2,'I H E P  WALL ENERGIES')
c        CALL WLPRNTI(IENERS,NXWS,NYWS,XWSL,YWSD,DXCWS,DYCWS,
c     1   'S M A L L  WALL ENERGIES')
C ..
      RETURN
      END


