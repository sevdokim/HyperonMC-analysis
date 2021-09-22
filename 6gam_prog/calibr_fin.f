      subroutine finishall
c=====================================================================
c     called from recons_main.c at the end of job
c====================================================================
cmis+>
      common /minihi/matrix(10,10),b(10),alfa1(100),kkk,istep
      real matrix, b, alfa1     ! matrixx, bx
      integer kkk,istep
      open(27,file='e_cor_matrix_new.dat',form='formatted',
     &               STATUS = 'unknown')
c
      write(27,*)kkk
      write(27,*)istep
      write(27,1)matrix
      write(27,2)b
      write(27,3)alfa1
      close(27)
 1    format(10(10E23.10/))
 2    format(10   E23.10/)
 3    format(10(10E23.10/))
c
c     dimension matrixx(k,k), bx(k)
c     integer i, j, k
c     do i = 1, k
c        do j = 1, k
c           matrixx(i,j) = matrix(i,j)
c        enddo
c        bx(i) = b(i)
c     enddo
c     write(27,*) matrixx
c     write(27,*) bx
cmis->
      write(*,*)'-------------starting to finish all----------------'
      call correc
      call wrcoef (ok)
      call calibr_fin

      return
      end

      SUBROUTINE  calibr_fin
C=======================================================================
C     print results and close files                       
C     Author: Nadia Russakovich  21.06.90
C     Updated:     20.04.04	NLR				       
C=======================================================================
C ..
      COMMON /W2SET /  NXW2,NYW2,NCELW2,XWL2,YWD2,DXC2,DYC2
      COMMON /WSSET /  NXWS,NYWS,NCELWS,XWLS,YWDS,DXCS,DYCS
      integer ifullb,ifulls
      COMMON /IFULLB/  IFULLB(24,24),IFULLS(8,8)
      COMMON /LOSSD /  LOSSD(70)
      COMMON /NREC  /  NREC(10)
      COMMON /LUN   /  LU,LUNOUT,LUNST
      COMMON /CLB2  /  CLB2(576),CLBS(64)
      COMMON /CLB2O /  COLD2(576),COLDS(64)
      COMMON /CELFIL/  ICFIL(24,24),ISFIL(8,8)
      COMMON /W2FIL /  WFIL(24,24,50)
      COMMON /WSFIL /  WSFIL(8,8,50),IDEFS(8,8)
      COMMON /CALIB /  NSTEP,NSTMAX,RATIO(576)
      COMMON /DEFMAS/  IDEF(24,24)
      COMMON /EFMEAN/  EFMb(24,24),EFMsm(8,8)
      COMMON /MASFSM/ BIGS(8,8),SMALS(8,8),RATIOS(64)
      common /energy_flow/ aver_map1(640),aver_map2(640),
     &                      hit_map1(640),hit_map2(640)
        integer nt,icon
      COMMON /NTSTAT/ NT,icon
      DIMENSION IDCL2(576),IDCLS(64) !,IRB(576),IRS(64)
      character*40 lostxt

      integer nafnaf_energy, wall_proc,mclus_bad,ef
      common /bad_something/ nafnaf_energy,wall_proc,mclus_bad,ef,efmm
C
c	WRITE(LUNOUT,1000)
c	WRITE(LUNOUT,15)  (NREC(I),I=1,2)
C ..
c      goto 60
      do i =1,576
         if(aver_map1(i).gt.hit_map1(1)/6)
     & call hf2(5017,float(i-((i-1)/24)*24),float(i/24)+1,aver_map1(i))
      enddo
      do i=577,640
         ii=i-576
         if(aver_map2(i).gt.hit_map2(1)/6)
     & call hf2(5020,float(ii-((ii-1)/8)*8),float(ii/8)+1,aver_map2(i))
      enddo
c 60   continue
      write(*,*)'bad events due to nafnaf energy:',nafnaf_energy
      write(*,*)'bad events due to wall proc:',wall_proc
      write(*,*)'bad events due to mclus:',mclus_bad
      write(*,*)'bad events due to efmass<0:',ef
      write(*,*)'bad events due to Mismass<0:',efmm

        L9 = 1
        OPEN (L9,FILE = 'lossc.txt',form='formatted',STATUS = 'OLD')
c        WRITE(LUNOUT,25)
        DO 10 I = 1,70
          READ (L9,25) LOSTXT
c          IF (LOSSD(I).GT.0) WRITE(LUNOUT,45) LOSTXT,I,LOSSD(I)
10      CONTINUE
        CLOSE (L9)

       DO 20 I = 1,64
           IF (CLBS(I).LE.0.) GO TO 20
           IDCLS(I) = IFIX((CLBS(I)-COLDS(I))/CLBS(I)*100.)
c	  IRS(I) = IFIX(RATIOS(I)*100.)
20      CONTINUE
        DO 30 I = 1,576
           IF (CLb2(I).LE.0.) GO TO 30
           IDCL2(I) = IFIX((CLB2(I)-COLD2(I))/CLB2(I)*100.)
c	  IRB(I) = IFIX(RATIO(I)*100.)
30      CONTINUE
C ..
c      call wlprni2(ifullb,24,24,'big cells hits statistics')
c      call wlprni2(ifulls,8,8,'small cells hits statistics')
c      call wlprni2(icfil,24,24,'big cells hits statistics')
c      call wlprni2(isfil,8,8,'small cells hits statistics')
c      CALL  B W L P R N R(CLB2,NXW2,NYW2,
c     *			  'NEW COEFFICIENTS FOR WALL2')
c      CALL  W L P R N I 2(IDCL2,NXW2,NYW2,
c     *			  'DEVIATION FROM OLD COEF.(%)')
c      CALL  W L P R N I 2(IDEF,NXW2,NYW2,'MASS DIFFERENCES')
c      CALL  W L P R N I 2(IRB,NXW2,NYW2,'RATIO') ! i bylo tak

c      CALL  B W L P R N R(CLBS,NXWS,NYWS,
c     *			  'NEW COEFFICIENTS FOR WALLS')
c      CALL  W L P R N I 2(IDCLS,NXWS,NYWS,
c     *			  'DEVIATION FROM OLD COEF.(%)')
c      CALL  W L P R N I 2(IDEFS,NXWS,NYWS,'MASS DIFFERENCES')
c      CALL  W L P R N I 2(IRS,NXWS,NYWS,'RATIO') ! i bylo tak

C ..
       WRITE (LUNOUT,1005)
       DO 40 IY = 1,NYW2
       DO 40 IX = 1,NXW2
c	 WRITE(LUNOUT,1010) IX,IY,EFMb(IX,IY)
c	 WRITE(LUNOUT,1020) (IFIX(WFIL(IX,IY,I)),I=1,25)
40     CONTINUE
       DO 50 IY = 1,NYWS
       DO 50 IX = 1,NXWS
c	 WRITE(LUNOUT,1010) IX,IY,EFMsm(IX,IY)
c	 WRITE(LUNOUT,1020) (IFIX(WSFIL(IX,IY,I)),I=1,25)
50     CONTINUE
       
       if (nt.gt.0) then
          call hrout(333,icycle,' ')
          write(*,*) 'icycle',icycle
       else
         call hrout(0,icycle,' ')
       endif
          write(*,*) 'icycle',icycle

       call hrend('hyperon')
      close (lunst)
      RETURN
C
C15    FORMAT (//2X,'DECOD STATISTICS:',
C     1         /5X,'EVENTS NUMBER =    ',I6,
C     2         /5X,'proceeded runs number =',I6)
c     3		/5X,'SPILL END EVENTS =  ',I6,
c     4		/5X,'EVENTS NUMBER =     ',I6,
c     5		/5X,'REPERS NUMBER =     ',I6,
c     6		/5X,'RUN BEG/END NUMBER =',I6,
c     7		/5X,'NEVENT COUNTER =    ',I6,
c     8		/5X,'GOOD EVENTS NUMBER =',I6)
25    FORMAT (A40)
C45    FORMAT (2X,A40,5X,'LOSSD ( ',I2,' ) = ',I6)
C1000  FORMAT(//2X,'*** RESULT PRINT ',4X,95('*'))
1005  FORMAT(//10X,'*****  EF. MASS  HISTOGRAMMS *****')
C1010  FORMAT(2X,'IX =',I3,'   IY =',I3,'    EFMEAN =',F6.1)
C1020  FORMAT(25I4,/25I4)
C ..
      END

