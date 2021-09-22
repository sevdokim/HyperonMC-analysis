        SUBROUTINE  wall2_proc (OK,Itype)
C=======================================================================
C       SUBROUTINE FOR IHEP WALL PROCEEDING
C       CREATED          30.05.90  NLR
C       CORRECTION       16.02.93  NLR
C       LAST CORRECTION  16.02.93  NLR
C=======================================================================
        COMMON /SHOWR / NCLUST,nclws,NCELLC(60),XSHW(3,60),
     1                  YSHW(3,60),ESHW(3,60),iamp_clus(60),DUM(60)
        COMMON /PRFLAG/ NPREV,LPRINT
        COMMON /LUN   / LU,LUN,LUNST
        common /one23 / one(60)
        common /KINCUT/ emin2,angmax,efmin,efmax
        COMMON /W2SET / NXW2,NYW2,NCELW2,XW2L,YW2D,DXCW1,DYCW2
        COMMON /WSSET / NXWS,NYWS,NCELWS,XWSL,YWSD,DXCWS,DYCWS
        COMMON /W2GEOM/ XNET(25),YNET(25),HALFX,HALFY
        COMMON /WSGEOM/ XNETS(9),YNETS(9),HALFXS,HALFYS
        COMMON /ENERW2/ IEN2(24,24),IENS(8,8)
        COMMON /W2COPY/ nctot,IENR2(24,24),IENRS(8,8)
        COMMON /AMPW2 / ncellb,ncells,IWL2(24,24),IWLS(8,8)
        COMMON /CLLIM / nclus,IXMN(60),IXMX(60),IYMN(60),IYMX(60)

        COMMON /LOSSD / LOSSD(70)
        COMMON /NFILW2/ NFILWB(24,24),NFILWS(8,8)
        COMMON /NTSTAT/ NT,icon
c       common/clusters/ scl(100,64),bcl(100,576),csingle
        LOGICAL OK,SWALL,LPRINT
        DATA NCMAX /60/,NCMAXT/60/
C
        common /energy_flow/ aver_map1(640),aver_map2(640),
     &                        hit_map1(640),hit_map2(640)
        integer ib(24,24), is(8,8), nc, i_aver_map2
C-
C     write(*,*) 'W2SET=',NXW2,NYW2,NCELW2,XW2L,YW2D,DXCW1,DYCW2
C     write(*,*) 'WSSET=',NXWS,NYWS,NCELWS,XWSL,YWSD,DXCWS,DYCWS
C-
      nc=0
      Itype = 0 
      csingle=0.
c
c      do i=1,100
c         do j=1,64
c            scl(i,j)=0.
c         enddo
c         do j=1,576
c            bcl(i,j)=0.
c         enddo
c      enddo
C 151  FORMAT (24(24i3/))
C 152  format (8(8i4/))
C
C -----   SMALL WALL SUMMARY   -------------------------------
C
        IEN2(11,11) = IENS(1,1)+IENS(1,2)+IENS(2,1)+IENS(2,2)
        IEN2(11,12) = IENS(1,3)+IENS(1,4)+IENS(2,3)+IENS(2,4)
        IEN2(11,13) = IENS(1,5)+IENS(1,6)+IENS(2,5)+IENS(2,6)
        IEN2(11,14) = IENS(1,7)+IENS(1,8)+IENS(2,7)+IENS(2,8)
        IEN2(12,11) = IENS(3,1)+IENS(3,2)+IENS(4,1)+IENS(4,2)
        IEN2(12,12) = IENS(3,3)+IENS(3,4)+IENS(4,3)+IENS(4,4)
        IEN2(12,13) = IENS(3,5)+IENS(3,6)+IENS(4,5)+IENS(4,6)
        IEN2(12,14) = IENS(3,7)+IENS(3,8)+IENS(4,7)+IENS(4,8)
        IEN2(13,11) = IENS(5,1)+IENS(5,2)+IENS(6,1)+IENS(6,2)
        IEN2(13,12) = IENS(5,3)+IENS(5,4)+IENS(6,3)+IENS(6,4)
        IEN2(13,13) = IENS(5,5)+IENS(5,6)+IENS(6,5)+IENS(6,6)
        IEN2(13,14) = IENS(5,7)+IENS(5,8)+IENS(6,7)+IENS(6,8)
        IEN2(14,11) = IENS(7,1)+IENS(7,2)+IENS(8,1)+IENS(8,2)
        IEN2(14,12) = IENS(7,3)+IENS(7,4)+IENS(8,3)+IENS(8,4)
        IEN2(14,13) = IENS(7,5)+IENS(7,6)+IENS(8,5)+IENS(8,6)
        IEN2(14,14) = IENS(7,7)+IENS(7,8)+IENS(8,7)+IENS(8,8)
C
        do i=1,24
           do j=1,24
              ib(i,j)=ien2(i,j)
           enddo
        enddo
C
CSdv-   write(*,*)'starting ien2'
CSdv-   write(*,'(24I6)') ien2
C
       DO 11 I = 1,NXWS
       DO 11 J = 1,NYWS
        IENRS(I,J) = IENS(I,J)
        is(i,j)=iens(i,j)
11     CONTINUE
C
CSdv-   write(*,*)'starting ienS'
CSdv-   write(*,'(8I6)') is
C
C------------SMALL WALL CLUSTER SEARCHING ----------------------------
C
        IX1PAR = 1
        ICLBEG = 1
        ICLN = 0
        IFWL = 0
        DO 20 ICL = ICLBEG,NCMAXT
        ICLN = ICLN + 1
        IF (ICLN.GT.NCMAX) GO TO 30
C
        CALL mclus(NCELL,IXMIN,IXMAX,IYMIN,IYMAX,IX1PAR,
     *                   IENS,NXWS,NYWS,IFWL,ok)
Csdv-
c        write(*,*)'iens afetr cluster seurch'
c        write(*,152)iens
c
        if (.NOT.ok) Itype = 1
        IF (NCELL.EQ.0) GO TO 31
          DO 13 IX = IXMIN,IXMAX
          DO 13 IY = IYMIN,IYMAX
            IF (IWLS(IX,IY).NE.10000) GO TO 13
            LOSSD(59) = LOSSD(59) + 1
            GO TO 14
   13     CONTINUE
c          nc=nc+1
       if(ncell.eq.1)then
          x=float(ixmin)
          y=float(iymin)
          call hf2(5019,x,y,1.)
          i_aver_map2 = x+(y-1)*8
          aver_map2(i_aver_map2)=aver_map2(i_aver_map2)+1
          if(hit_map2(1).lt.aver_map2(i_aver_map2))
     &                      hit_map2(1)=aver_map2(i_aver_map2)
       endif
       call hf1(5014,float(ncell),1.)
C
C-------- CLUSTER IS IN SMALL WALL ----------------------------------
C
        SWALL = IXMIN.GT.1.AND.IXMAX.LT.8
        SWALL = SWALL.AND.IYMIN.GT.1.AND.IYMAX.LT.8
        IF (SWALL) GO TO 8

C----------- CLUSTER IS NEAR THE BIG GLASS -------------------------
C
        IF (IXMIN.NE.1) GO TO 2
         DO 1 IY = IYMIN,IYMAX
          IYB = (IY+1)/2 + 10
          IF (IENS(IXMIN,IY).LT.0.AND.IEN2(10,IYB).GT.0) GO TO 17
 1       CONTINUE
 2      IF (IXMAX.NE.8) GO TO 4
         DO 3 IY = IYMIN,IYMAX
          IYB = (IY+1)/2 + 10
          IF (IENS(IXMAX,IY).LT.0.AND.IEN2(15,IYB).GT.0) GO TO 17
 3       CONTINUE
 4      IF (IYMIN.NE.1) GO TO 6
         DO 5 IX = IXMIN,IXMAX
          IXB = (IX+1)/2 + 10
          IF (IENS(IX,IYMIN).LT.0.AND.IEN2(IXB,10).GT.0) GO TO 17
 5       CONTINUE
 6      IF (IYMAX.NE.8) GO TO 8
         DO 7 IX = IXMIN,IXMAX
          IXB = (IX+1)/2 + 10
          IF (IENS(IX,IYMAX).LT.0.AND.IEN2(IXB,15).GT.0) GO TO 17
 7       CONTINUE
 8       CONTINUE
c
         IF (IXMAX-IXMIN.LE.1.AND.IYMAX-IYMIN.LE.1)
     *      CALL hf1(20016,FLOAT(NCELL),1.)
         IF (IXMAX-IXMIN.EQ.2.AND.IYMAX-IYMIN.LE.1)
     *      CALL hf1(20016,FLOAT(10+NCELL),1.)
         IF (IXMAX-IXMIN.LE.1.AND.IYMAX-IYMIN.EQ.2)
     *      CALL hf1(20016,FLOAT(10+NCELL),1.)
         IF (IXMAX-IXMIN.EQ.2.AND.IYMAX-IYMIN.EQ.2)
     *      CALL hf1(20016,FLOAT(20+NCELL),1.)
         IF (IXMAX-IXMIN.GT.2.OR.IYMAX-IYMIN.GT.2)
     *      CALL hf1(20016,FLOAT(30+NCELL),1.)
C
C------------- SMALL WALL SHOWER RECONSTRUCTION ----------------------
C
c        write(*,*)'iens before wsclus'
c        write(*,152)iens
c
        CALL WSCLUS (IXMIN,IXMAX,IYMIN,IYMAX,
     *         ESHW1,XSHW1,YSHW1,ESHW2,OK)
        if(ok) nc=nc+1
Csdv-
c        write(*,*)'iens after wsclus'
c        write(*,152)iens
c
        if (.NOT.OK) Itype = 2
        IF (.NOT.OK) GO TO 14
C
        IXMN(ICLN) = IXMIN
        IXMX(ICLN) = IXMAX
        IYMN(ICLN) = IYMIN
        IYMX(ICLN) = IYMAX
        ESHW(1,ICLN) = ESHW1
        XSHW(1,ICLN) = XSHW1
        YSHW(1,ICLN) = YSHW1
        NCELLC(ICLN) = NCELL
        ESHW(2,ICLN) = ESHW2
C!!!-	
C-	write(*,*) 'ESHW(2,ICLN)=',ICLN,ESHW2
        iamp = 0
c
c        do ii=1,64
c           iiy=1+(ii-1)/8
c           iix=ii-(iiy-1)*8
c           if(iens(iix,iiy).lt.0)scl(icln,ii)=1.
c        enddo
c        do ii=1,576
c           iiy=1+(ii-1)/24
c           iix=ii-(iiy-1)*24
c           if(ien2(iix,iiy).lt.0)bcl(icln,ii)=1.
c        enddo
c
        IF (ESHW1.GT.EMIN2) THEN
          DO 9 IX = IXMIN,IXMAX
          DO 9 IY = IYMIN,IYMAX
            IF(IENRS(IX,IY).GT.0) NFILWS(IX,IY) = NFILWS(IX,IY) + 1
            iamp = iamp + iwls(ix,iy)
 9        CONTINUE
        END IF
        iamp_clus(icln) = iamp
        if(swall)then
        one(icln)=1
        else
        one(icln)=3
        endif
        GO TO 15
 14     ICLN = ICLN - 1
        OK = .TRUE.
C
C--------- KILLING BIG WALL CELLS, CONTAINING SMALL WALL CLUSTER ----------
C
 15      CONTINUE
        DO 16 IX = IXMIN,IXMAX
        DO 16 IY = IYMIN,IYMAX
          IF (IENS(IX,IY).GE.0) GO TO 16
          IX1 = (IX+1)/2 + 10
          IY1 = (IY+1)/2 + 10
          IEN2(IX1,IY1) = IEN2(IX1,IY1) + IENS(IX,IY)
 16     CONTINUE
        GO TO 18
C---------------------------- MIXED CLUSTER --------------------------
 17      ICLN = ICLN - 1
C
C------------- KILLING SMALL WALL CLUSTER ----------------------------
C
 18      CONTINUE
       do  19  IY = IYMIN,IYMAX
       do  19  IX = IXMIN,IXMAX
 19    IF (IENS(IX,IY).LT.0) IENS(IX,IY)=0
C
  20   CONTINUE
C
  30   LOSSD(7) = LOSSD(7) + 1
  31   CONTINUE
       NCLUST = ICLN - 1
C
C-----------BIG WALL PROCEEDING -------------------------------------
C
        DO 40 I = 1,NXW2
        DO 40 J = 1,NYW2
         IENR2(I,J) = IEN2(I,J)
40      CONTINUE
        IX1PAR = 1
        NCLWS  = NCLUST
        ICLBEG = NCLUST + 1
        ICL2 = NCLUST
        IFWL = 0
C
C---------CLUSTERS SEARCHING IN SUMMED WALL -----------------------
C
        DO 70 ICL = ICLBEG,NCMAXT
        ICL2 = ICL2 + 1
	
Cw-     write(*,*) 'ICL2=',ICL2
	
        IF (ICL2.GT.NCMAX) THEN
	                   Itype = 5
	                   GO TO 80
		    ENDIF
	
        CALL mclus(NCELL,IXMIN,IXMAX,IYMIN,IYMAX,IX1PAR,
     *                    IEN2,NXW2,NYW2,IFWL,ok)
c        write(*,*)'------------------------------------------------'
c        write(*,151)ien2
c  
Csdv-
        if (.not.ok) Itype = 3
        if (.not.ok) goto 999
        IF (NCELL.EQ.0) GO TO 81
	
C       nc=nc+1
        IF (IXMAX-IXMIN.LE.1.AND.IYMAX-IYMIN.LE.1)
     *      CALL hf1(20015,FLOAT(NCELL),1.)
        IF (IXMAX-IXMIN.EQ.2.AND.IYMAX-IYMIN.LE.1)
     *      CALL hf1(20015,FLOAT(10+NCELL),1.)
        IF (IXMAX-IXMIN.LE.1.AND.IYMAX-IYMIN.EQ.2)
     *      CALL hf1(20015,FLOAT(10+NCELL),1.)
        IF (IXMAX-IXMIN.EQ.2.AND.IYMAX-IYMIN.EQ.2)
     *      CALL hf1(20015,FLOAT(20+NCELL),1.)
        IF (IXMAX-IXMIN.GT.2.OR.IYMAX-IYMIN.GT.2)
     *      CALL hf1(20015,FLOAT(30+NCELL),1.)
C
       if(ncell.eq.1)then
          x=float(ixmin)
          y=float(iymin)
          call hf2(5016,x,y,1.)
          i_aver_map2 = x+(y-1)*24
          aver_map1(i_aver_map2)=aver_map1(i_aver_map2)+1
          if(hit_map1(1).lt.aver_map1(i_aver_map2))
     &                      hit_map1(1)=aver_map1(i_aver_map2)
          if(x.eq.2.and.y.eq.3)then
             csingle=1.
          endif
c
c            write(*,151)ib
c            stop         
c             do ix=1,24
c                do iy=1,24
c                   if(ib(ix,iy).gt.50)then
c                      ic=ic+1
c                      call hf2(5018,float(ix),float(iy),1.)
c                   endif
c                enddo
c             enddo
c             do ix=1,8
c                do iy=1,8
c                   if(is(ix,iy).gt.0)then
c                      call hf2(5021,float(ix),float(iy),1.)
c                   endif
c                enddo
c             enddo
c             write(*,*)'icount=',ic
c          endif
       endif
       call hf1(5014,float(ncell),1.)
C
C-------- SHOWER RECONSTRUCTION IN BIG WALL -------------------------
C
          DO 41 IX = IXMIN,IXMAX
          DO 41 IY = IYMIN,IYMAX
            IF (IWL2(IX,IY).NE.10000) GO TO 41
            LOSSD(59) = LOSSD(59) + 1
            GO TO 53
41        CONTINUE
        CALL WBCLUS (IXMIN,IXMAX,IYMIN,IYMAX,ESHW1,ESHW2,
     *       XSHW1,XSHW2,YSHW1,YSHW2,OK)
        if (ok) nc=nc+1
Csdv-
        IF (.NOT.OK) THEN
	             Itype = 4
	             GO TO 53
	     ENDIF
C
        IXMN(ICL2) = IXMIN
        IXMX(ICL2) = IXMAX
        IYMN(ICL2) = IYMIN
        IYMX(ICL2) = IYMAX
        ESHW(1,ICL2) = ESHW1
        XSHW(1,ICL2) = XSHW1
        YSHW(1,ICL2) = YSHW1
        ESHW(2,ICL2) = ESHW2
        XSHW(2,ICL2) = XSHW2
        YSHW(2,ICL2) = YSHW2
        NCELLC(ICL2) = NCELL
C!!!-	
C-	write(*,*) 'ESHW(2,ICL2)=',ICL2,ESHW2,XSHW2,YSHW2
c
c        do ii=1,576
c           iiy=1+(ii-1)/24
c           iix=ii-(iiy-1)*24
c           if(ien2(iix,iiy).lt.0)then
c              bcl(icl2,ii)=1.
c           endif
c        enddo 
c
        iamp = 0
        IF (ESHW1.GT.EMIN2) THEN
          DO 50 IX = IXMIN,IXMAX
          DO 50 IY = IYMIN,IYMAX
           IF(IENR2(IX,IY).GT.0) NFILWB(IX,IY) = NFILWB(IX,IY) + 1
            iamp = iamp + iwl2(ix,iy)
  50      CONTINUE
        END IF
        one(icl2)=2
        iamp_clus(icl2) = iamp
        GO TO 54
C
C---------- KILLING BIG WALL CLUSTER ----------------------------------
C
  53    ICL2 = ICL2 - 1
        OK = .TRUE.
C
  54    CONTINUE
          DO  55  IY = IYMIN,IYMAX
          DO  55  IX = IXMIN,IXMAX
  55      IF (IEN2(IX,IY).LT.0) IEN2(IX,IY)=0
C
  70    CONTINUE
C
  80   LOSSD(7) = LOSSD(7) + 1
  81   CONTINUE
       NCLUST = ICL2 - 1
       nclus = nclust
c
c       write(*,'(10i4)')(one(i),i=1,10)
c       write(*,*)'------------at the end----------------------'
c       write(*,152)iens
c       write(*,151)ien2
c 
       call hf1(20142,float(nc),1.)
 999   RETURN
        END

