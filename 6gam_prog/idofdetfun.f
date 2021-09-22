!       subroutine idofdetfun(typ,IGgroup_in,IDdet_in,ID)                       ! commented ; kondr
      subroutine idofdetfun(typ,IGgroup_in,IDdet_in,ID)                         ! ; kondr      
      integer typ                                                               ! ; kondr , mysterious problems here, no need for char. replaced with int    
!       character *4 typ                                                        ! commented ; kondr
c---
c define: 
c    1)  program identifcator IDdet   of detector      (if typ='IDdet')     == 1 int
c    2)  program identifcator IGgroup of group IGgroup (if typ='IGgroup')   == 2 int 
c    3)  program type         ITdet   of detector      (if typ='ITdet')     == 3 int 
c    4)  program type         ITgroup of group         (if typ='ITgroup')   == 4 int
c
c   where IDdet  =1,2 .. IDdet_max ...  = LGD2, .. (but 69 = LE96, 301= MWPC)
c         IGgroup=1,2 .. IGgroup_max  
c         ITdet  =1,2 .. 6 = ECAL,HCAL,CPV,MWPC,COUNTERS,TUBE 
c         ITgroup=1,2 .. 5 = ADC, ZDC, TDC,REGI,COUNTERS 
c 
c Output ID=-3 means invalid identificator, in this case you must initiate
c              stop operation with possible dump of data
c        ID=-1 means unknown identificator, in this case
c              the detector with this edentificaor will be ignored
c---
c input: typ = IGgroup_in, IDdet_in - input identificator of device group and detector
c output: ID
c---
      parameter (i8=4)
!       character *4 a8(i8)                                                     ! commented ; kondr
      dimension a8(i8)                                                          ! ; kondr
      integer a8                                                                ! ; kondr
c      data a8/'IDdet', 'IGgroup', 'ITdet','ITgroup'/
!       data a8/'IDde', 'IGgr', 'ITde','ITgr'/                                  ! commented ; kondr
      data a8/1, 2, 3, 4/                                                       ! ; kondr      
      parameter (Ndd=6,Ndg=5,   Ndt=5,Ngt=5)
      dimension iaa(3*Ndd+3*Ndg+3*Ndt+3*Ngt)

c---------------------------------------------------------------
c IDdet as function of IDgroup_in and IDdet_in 
      dimension idd(3*Ndd)
      equivalence (iaa,idd)
c              IDdet  IGgroup_in  IDdet_in
      data idd/1,   17,           71, ! LGD2 (Ndd=ciclo strok)
     *         1,   0,            71,  
     *        69,   17,           69, ! LE69
     *       301,   10,          301, ! MWPC  
     *       301,    0,          301, ! MWPC               
     *        -1,    0,            0/ ! unknown detector
c IGgroup as function of IDgroup_in and IDdet_in 
      dimension idg(3*Ndg)
      equivalence (iaa(3*Ndd+1),idg(1))      
c              IGgroup  IGgroup_in  IDdet_in
      data idg/1,    17,           71, ! LGD2 (Ndg - chislo strok)
     *         1,     0,           71,    
     *         1,    17,           69, ! LE69
     *         1,    10,          301, ! MWPC   
     *         1,     0,          301/ ! MWPC  
c ITdet as function of IDgroup_in and IDdet_in 
      dimension idt(3*Ndt)
      equivalence (iaa(3*Ndd+3*Ndg+1),idt(1))
c              IGgroup IGgroup_in  IDdet_in
      data idt/1,    17,           71, ! LGD2 (Ndt -chislo strok)
     *         1,     0,           71,  
     *         5,    17,           69, ! LE69
     *         4,    10,          301, ! MWPC
     *         4,     0,          301/ ! MWPC    

c ITgroup as function of IDgroup_in and IDdet_in 
      dimension igt(3*Ngt)
      equivalence (iaa(3*Ndd+3*Ndg+3*Ngt+1),igt(1))
c              ITgroup IGgroup_in   IDdet_in
      data igt/1,     17,          71, ! LGD2 (Ngt - chislo strok)
     *         1,      0,          71,   
     *         1,     17,          69, ! LE96
     *         1,     10,         301, ! MWPC
     *         1,      0,         301/ ! MWPC            
c-----------------------------------------
      dimension n_array(i8)
      save n_array
      data n_array/Ndd,Ndg,Ndt,Ngt/
c-----------------------------------------
c control validity of the 1st argumet= typ
      it=0
      L1=1
      do i=1,i8
      if(a8(i).eq.typ)then
        it=i
        goto 10
      endif
      L1=L1+n_array(i)*3
      enddo ! i
      write(*,'(a45,a4)')'idofdetfun: invalid 1st argument, typ=',typ       
      write(*,*)' valid list of this argument are:'
      do i=1,i8
      write(*,'(1x,a4)')a8(i)
      enddo      
      stop
      
c--------
c control validity of the 2nd and 3rd arguments            
   10 do i=1,n_array(it)
      if(IGgroup_in.eq.iaa(L1+1))then
      if(IDdet_in.  eq.iaa(L1+2))then
      ID=iaa(L1)
      return 
      endif
      endif
      L1=L1+3
      enddo
      write(*,*)'idofdetfun: ** error **'
      write(*,'(1x,a19,a4)')' 1st argument: typ=',typ
      write(*,*)' not recognized arguments IGgroup_in,IDdet_in=',
     *                                     IGgroup_in,IDdet_in
c      stop
      ID=-3
      return
      end      
