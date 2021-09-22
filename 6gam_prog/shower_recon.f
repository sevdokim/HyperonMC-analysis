
        SUBROUTINE  WBCLUS(IXMIN,IXMAX,IYMIN,IYMAX,ESHW1,ESHW2,
     *     XSHW1,XSHW2,YSHW1,YSHW2,OK)
C====================================================================
C       SUBROUTINE FOR SHOWER RECONSTRUCTION IN IHEP WALL
C       CREATED  02.06.90   NLR
C       LAST CORRECTION   13.09.93   NLR
C====================================================================
        COMMON /W2SET / NX,NY,NCELW2,XW2L,YW2D,DXC,DYC
        COMMON /W2GEOM/ XNET(25),YNET(25),HX,HY
        COMMON /ENERW2/ IEN2(24,24),IENS(8,8)

        COMMON /PRFLAG/ NPREV,LPRINT
        COMMON /LUN   / LU,LUN,LUNST
        COMMON /LOSSD / LOSSD(70)

        DIMENSION ICEN(25,25),ESUMX(6),ESUMY(6) !,ESUM(3),ESUM2(3)
        LOGICAL OK,LPRINT
        DATA ECUT/.05/

C 151    FORMAT (24(24i3/))
c        write(*,151)ien2
c
          ESHW2 = -1.
          OK = .TRUE.
          CALL VZERO (ICEN,625)
        DO 10 IX = IXMIN,IXMAX
        DO 10 IY = IYMIN,IYMAX
          IF (IEN2(IX,IY).LT.0) ICEN(IX,IY) = - IEN2(IX,IY)
   10   CONTINUE
        IF (IXMAX-IXMIN.GT.5.OR.IYMAX-IYMIN.GT.5) GO TO 900
C     -------------------------------------------------------
        CALL VZERO(ESUMX,6)
        CALL VZERO(ESUMY,6)
        ESHW1 = 0.
        DO 20 IX = IXMIN,IXMAX
          IEX = IX - IXMIN + 1
          DO 20 IY = IYMIN,IYMAX
            IEY = IY - IYMIN + 1
            IF (ICEN(IX,IY).LE.0) GO TO 20
            ESUMY(IEX) = ESUMY(IEX) + ICEN(IX,IY)
            ESUMX(IEY) = ESUMX(IEY) + ICEN(IX,IY)
            ESHW1 = ESHW1 + ICEN(IX,IY)
   20   CONTINUE
C
C---------------SINGLE CLUSTER, 1,2 CELL AND 2x2 CARE-----------
C
C--------------3x1   3x2  AND 3x3 CLUSTERS -------------------------
C
        IDX = IXMAX - IXMIN
        IDY = IYMAX - IYMIN
        IF (IDX.LE.2.AND.IDY.LE.2) THEN
          IEXC = 0
          IEYC = 0
          IEXM = 0
          IEYM = 0
          EMAXX = 0
          EMAXY = 0
          EMINX = 100000.
          EMINY = 100000.
          DO 30 I = 1,3
            IF (ESUMY(I).GT.EMAXY) THEN
              IEXC = I
              EMAXY = ESUMY(I)
            END IF
            IF (ESUMX(I).GT.EMAXX) THEN
              IEYC = I
              EMAXX = ESUMX(I)
            END IF
            IF (ESUMY(I).LT.EMINY) THEN
              IEXM = I
              EMINY = ESUMY(I)
            END IF
            IF (ESUMX(I).LT.EMINX) THEN
              IEYM = I
              EMINX = ESUMX(I)
            END IF
   30     CONTINUE
          IF (IEXC.EQ.1) THEN
            RDIF = -ESUMY(2) - ESUMY(3)
            XSHW1 = XNET(IXMIN) + SHP(RDIF,ESHW1,DXC,OK)
            IF (IDX.EQ.2) ESHW2 = -3.
          END IF
          IF (IEXC.EQ.2) THEN
            RDIF = ESUMY(1) - ESUMY(3)
            XSHW1 = XNET(IXMIN+1) + SHP(RDIF,ESHW1,DXC,OK)
          END IF
          IF (IEXC.EQ.3) THEN
            RDIF = ESUMY(1) + ESUMY(2)
            XSHW1 = XNET(IXMAX) + SHP(RDIF,ESHW1,DXC,OK)
            ESHW2 = -5.
          END IF
          IF (ESUMY(IEXM).GT.ECUT*ESHW1.AND.IDX.EQ.2) ESHW2 = -6
          IF (IEYC.EQ.1) THEN
            RDIF = -ESUMX(2) - ESUMX(3)
            YSHW1 = YNET(IYMIN) + SHP(RDIF,ESHW1,DYC,OK)
            IF (IDY.EQ.2) ESHW2 = -3.
          END IF
          IF (IEYC.EQ.2) THEN
            RDIF = ESUMX(1) - ESUMX(3)
            YSHW1 = YNET(IYMIN+1) + SHP(RDIF,ESHW1,DYC,OK)
          END IF
          IF (IEYC.EQ.3) THEN
            RDIF = ESUMX(1) + ESUMX(2)
            YSHW1 = YNET(IYMAX) + SHP(RDIF,ESHW1,DYC,OK)
            ESHW2 = -5.
          END IF
          IF (ESUMX(IEYM).GT.ECUT*ESHW1.AND.IDY.EQ.2) ESHW2 = -6
          GO TO 910
        END IF
C
C ---------------- BIG CLUSTER -----------------------------------
C
        EMAX = 0.
        DO 150 IX = IXMIN,IXMAX
        DO 150 IY = IYMIN,IYMAX
          IF (ICEN(IX,IY).LT.EMAX) GO TO 150
          EMAX = ICEN(IX,IY)
          IXM = IX
          IYM = IY
150     CONTINUE
        XSHW1 = XNET(IXM) + HX
        YSHW1 = YNET(IYM) + HY
        ESHW2 = -10.
C!!!!!- ESHW2 = -4.
        LOSSD(9) = LOSSD(9) + 1
        GO TO 910
C
C--------------- BAD CLUSTER -----------------------------------------
C
900     CONTINUE
        LOSSD(8) = LOSSD(8) + 1
        OK = .FALSE.
C
910     CONTINUE
          IF (.NOT.LPRINT) RETURN
           WRITE(LUN,1030) IXMIN,IXMAX,IYMIN,IYMAX
          DO 920 IY = IYMIN,IYMAX
           IYI = IYMAX - IY + IYMIN
           WRITE(LUN,1020) (ICEN(IX,IYI),IX=IXMIN,IXMAX)
920       CONTINUE
         IF (.NOT.OK) RETURN
          WRITE(LUN,1010) XNET(IXMIN),YNET(IYMIN)
          WRITE(LUN,1040) ESHW1,XSHW1,YSHW1
          WRITE(LUN,1050) ESHW2,XSHW2,YSHW2
          RETURN

1010    FORMAT(/2X,'XLEFT =',F6.0,3X,'YDOWN =',F6.0)
1020    FORMAT(10X,8I5)
1030    FORMAT(2X,'IXMIN =',I3,3X,'IXMAX =',I3,
     *        /2X,'IYMIN =',I3,3X,'IYMAX =',I3)
1040    FORMAT (2X,'ESHW1 =',F6.0,3X,'XSHW1 =',F7.1,
     *          3X,'YSHW1 =',F7.1)
1050    FORMAT (2X,'ESHW2 =',F6.0,3X,'XSHW2 =',F7.1,
     *          3X,'YSHW2 =',F7.1)
        END
c
        SUBROUTINE  WSCLUS(IXMIN,IXMAX,IYMIN,IYMAX,
     *            ESHW1,XSHW1,YSHW1,ESHW2,OK)
C====================================================================
C       FOR SHOWER RECONSTRUCTION IN SMALL GLASSES
C       CREATED  08.09.90   NLR
C       LAST CORRECTION  16.02.93  NLR
C====================================================================
        COMMON /WSSET / NX,NY,NCELWS,XWSL,YWSD,DXC,DYC
        COMMON /WSGEOM/ XNET(9),YNET(9),HX,HY
        COMMON /ENERW2/ IEN2(24,24),IENS(8,8)
        COMMON /PRFLAG/ NPREV,LPRINT
        COMMON /LUN   / LU,LUN,LUNST
        COMMON /LOSSD / LOSSD(70)
        COMMON /ALFAW2/ ALFA

        LOGICAL LPRINT,OK,OKvrt
        DIMENSION ICEN(8,8),ESUMX(8),ESUMY(8),Ivertx(3,15)
        DATA ECUT/.05/
C
        OK = .TRUE.
        CALL VZERO(ICEN,64)
        CALL VZERO(ESUMX,8)
        CALL VZERO(ESUMY,8)
        ESHW1 = 0.
        ESHW2 = -1.
C
        DO 10 IX = IXMIN,IXMAX
          IEX = IX - IXMIN + 1
          DO 10 IY = IYMIN,IYMAX
            IEY = IY - IYMIN + 1
            IF (IENS(IX,IY).GE.0) GO TO 10
            ICEN(IX,IY) = -IENS(IX,IY)
            ESUMY(IEX) = ESUMY(IEX) + ICEN(IX,IY)
            ESUMX(IEY) = ESUMX(IEY) + ICEN(IX,IY)
            ESHW1 = ESHW1 + ICEN(IX,IY)
10      CONTINUE
C
C ------- LESS THAN 5x5 CLUSTERS --------------------------------------
C
        IDX = IXMAX - IXMIN
        IDY = IYMAX - IYMIN
C	
C-        IF (IDX.gt.4.or.IDY.gt.4) then
C-	    write(*,*)'IDX,IDY=',IDX,IDY
C-	    write(*,'(8I7)') ((ICEN(IX,IY),IX=1,IDX),IY=1,IDY)
C-	    endif
C
        IF (IDX.LE.4.AND.IDY.LE.4) THEN
          IEXC = 0
          IEYC = 0
          IEXM = 0
          IEYM = 0
          EMAXX = 0
          EMAXY = 0
          EMINX = 100000.
          EMINY = 100000.
          DO 20 I = 1,5
            IF (ESUMY(I).GT.EMAXY) THEN
              IEXC = I
              EMAXY = ESUMY(I)
            END IF
            IF (ESUMX(I).GT.EMAXX) THEN
              IEYC = I
              EMAXX = ESUMX(I)
            END IF
            IF (ESUMY(I).LT.EMINY) THEN
              IEXM = I
              EMINY = ESUMY(I)
            END IF
            IF (ESUMX(I).LT.EMINX) THEN
              IEYM = I
              EMINX = ESUMX(I)
            END IF
20        CONTINUE
          ALFA = 0.
          IF (IEXC.EQ.1) THEN
CSdv-       RDIF = ESUMY(1) - ESHW1
            RDIF =-ESUMY(2) - ESUMY(3) - ESUMY(4) - ESUMY(5)
            DX = SHP(RDIF,ESHW1,DXC,OK)
            XSHW1 = XNET(IXMIN) + DX
            IF (IDX.GE.2) ESHW2 = -3.
C	    
C-	    write(*,*) 'IECX=',IEXC,RDIF
            GO TO 30
          END IF
          IF (IEXC.EQ.2) THEN
            RDIF = ESUMY(1) - ESUMY(3) - ESUMY(4) - ESUMY(5)
            DX = SHP(RDIF,ESHW1,DXC,OK)
            XSHW1 = XNET(IXMIN+1) + DX
            GO TO 30
          END IF
          IF (IEXC.EQ.3) THEN
            RDIF = ESUMY(1) + ESUMY(2) - ESUMY(4) - ESUMY(5)
            DX = SHP(RDIF,ESHW1,DXC,OK)
            XSHW1 = XNET(IXMIN+2) + DX
            IF (IDX.EQ.2) ESHW2 = -5.
            GO TO 30
          END IF
          IF (IEXC.EQ.4) THEN
            RDIF = ESUMY(1) + ESUMY(2) + ESUMY(3) - ESUMY(5)
            DX = SHP(RDIF,ESHW1,DXC,OK)
            XSHW1 = XNET(IXMIN+3) + DX
            IF (IDX.EQ.3) ESHW2 = -5.
            GO TO 30
          END IF
          IF (IEXC.EQ.5) THEN
CSdv-       RDIF = ESHW1 - ESUMY(5)
            RDIF = ESUMY(1) + ESUMY(2) + ESUMY(3) + ESUMY(4)
            DX = SHP(RDIF,ESHW1,DXC,OK)
            XSHW1 = XNET(IXMAX) + DX
            ESHW2 = -5.
C	    
C	    write(*,*) 'IECX=',IEXC,RDIF
          END IF
30        CONTINUE
          IF (IDX.EQ.4.AND.ESUMY(IEXM).GT.ECUT*ESHW1) ESHW2 = -6.
          IF (IEYC.EQ.1) THEN
CSdv-       RDIF = ESUMX(1) - ESHW1
            RDIF =-ESUMX(2) - ESUMX(3) - ESUMX(4) - ESUMX(5)
            DY = SHP(RDIF,ESHW1,DYC,OK)
            YSHW1 = YNET(IYMIN) + DY
C	    
C-	    write(*,*) 'IECY=',IEYC,RDIF
	    IF (IDY.GE.2) ESHW2 = -3.   
            GO TO 40
          END IF
          IF (IEYC.EQ.2) THEN
            RDIF = ESUMX(1) - ESUMX(3) - ESUMX(4) - ESUMX(5)
            DY = SHP(RDIF,ESHW1,DYC,OK)
            YSHW1 = YNET(IYMIN+1) + DY
            GO TO 40
          END IF
          IF (IEYC.EQ.3) THEN
            RDIF = ESUMX(1) + ESUMX(2) - ESUMX(4) - ESUMX(5)
            DY = SHP(RDIF,ESHW1,DYC,OK)
            YSHW1 = YNET(IYMIN+2) + DY
            IF (IDY.EQ.2) ESHW2 = -5.
            GO TO 40
          END IF
          IF (IEYC.EQ.4) THEN
            RDIF = ESUMX(1) + ESUMX(2) + ESUMX(3) - ESUMX(5)
            DY = SHP(RDIF,ESHW1,DYC,OK)
            YSHW1 = YNET(IYMIN+3) + DY
            IF (IDY.EQ.3) ESHW2 = -5.
            GO TO 40
          END IF
          IF (IEYC.EQ.5) THEN
CSdv-       RDIF = ESHW1 - ESUMX(5)
            RDIF = ESUMX(1) + ESUMX(2) + ESUMX(3) + ESUMX(4)
C-	    
            DY = SHP(RDIF,ESHW1,DYC,OK)
            YSHW1 = YNET(IYMAX) + DY
            ESHW2 = -5.
C
C-	    write(*,*) 'IECY=',IEYC,RDIF
          END IF
40        CONTINUE
          IF (IDY.EQ.4.AND.ESUMX(IEYM).GT.ECUT*ESHW1) ESHW2 = -6.
          GO TO 1010
        END IF
C
C ---------------- BIG CLUSTER ---------------------------------------
C
        EMAX = 0.
        DO 130 IX = IXMIN,IXMAX
        DO 130 IY = IYMIN,IYMAX
C-	   write(*,*)  '===>',IX,IY,ICEN(IX,IY)
	   IF (ICEN(IX,IY).LT.EMAX) GO TO 130
           EMAX= ICEN(IX,IY)
           IXM = IX
           IYM = IY
130     CONTINUE
        XSHW1 = XNET(IXM) + HX
        YSHW1 = YNET(IYM) + HY
        ESHW2 = -10.
C!!!!!- ESHW2 = -4.
        LOSSD(11) = LOSSD(11) + 1
	OK = .FALSE.
        GO TO 1010

C1000  CONTINUE
       LOSSD(10) = LOSSD(10) + 1
1010   CONTINUE
C
C      Vertex analysis by Sdv
CSdv-
       call vertex_S(ICEN,IXMIN,IXMAX,IYMIN,IYMAX,Nvertx,Ivertx,OKvrt) 
       if(Nvertx.gt.1) OK=.false.     
       OK = OK.and.OKvrt
C
       IF (.NOT.LPRINT) RETURN
       WRITE(*,*) IXMIN,IXMAX,IYMIN,IYMAX,'  ESHW2=',ESHW2
       DO IY=IYMIN,IYMAX
       write(*,'(8I7)') (ICEN(IX,IY),IX=IXMIN,IXMAX)
       ENDDO
C-
C-     IF (OK) WRITE(*,125) XSHW1,YSHW1,ESHW1,ESHW2
C
105    FORMAT(5X,'IXMIN =',I2,3X,'IXMAX =',I2,3X,'IYMIN =',I2,
     *        3X,'IYMAX=',I2)
115    FORMAT(10X,8I5)
125    FORMAT(5X,'XSH1 =',F8.2,'  YSH1 = ',F8.2,'   ESH1 =',F7.0,
     *                                          '   ESH2 =',F7.0)
       RETURN
       END
C
      subroutine vertex_S(ICEN,IXMIN,IXMAX,IYMIN,IYMAX,Nvertx,Ivertx,OK)
      dimension ICEN(8,8),IaXY(676),Ivertx(3,15)
      logical OK
C
C-    write(*,'(8I8)') ICEN
C
      Nx = IXMAX-IXMIN+3
      Ny = IYMAX-IYMIN+3
      CALL VZERO(IaXY,Nx*Ny)
C
      do iy=IYMIN,IYMAX
      ixy=Nx*(iy-IYMIN+1)
C
      do ix=IXMIN,IXMAX
      i =ix-IXMIN+2
C
      if(ICEN(ix,iy).gt.0) IaXY(ixy+i) = ICEN(ix,iy)
      enddo
      enddo
C      
      call vert_XY(IaXY,Nx,Ny,Nvertx,Ivertx,OK)
C-    write(*,*) 'Nvertx=',Nvertx
C-    write(*,'(3I6)/') ((Ivertx(i,j),i=1,3),j=1,Nvertx)
C
      return
      end
C
      subroutine vert_XY(IaXY,Nx,Ny,Nvrt,Ivrt,OK)
      dimension ICEN(8,8),IaXY(Nx,Ny),Ivrt(3,15)
      data IaMax / 180 /
      logical OK
C
C-    write(*,*) 'nx,ny=', nx,ny 
C-    do j=1,Ny
C-    write(*,*) (IaXY(i,j),i=1,nx)
C-    enddo 
C-    write(*,*)
C
      Nvrt= 0
      OK  =.true.
      do 10 j=2,Ny-1
      do 10 i=2,Nx-1
      Iam = IaXY(i,j)
      if(Iam.lt.IaXY(i-1,j).or.Iam.lt.IaXY(i+1,j)) go to 10
      if(Iam.lt.IaXY(i,j-1).or.Iam.lt.IaXY(i,j+1)) go to 10
      Nvrt= Nvrt+1
      if(Nvrt.gt.15) then
     		     OK  =.false.
     		     Nvrt= Nvrt-1
     		     return
     	      else
     	      Ivrt(1,Nvrt) = i
     	      Ivrt(2,Nvrt) = j
     	      Ivrt(3,Nvrt) = Iam
C
              if(Iam.lt.IaMax) then
	      Nvrt= Nvrt-1
	      endif
     	 endif
   10  continue 
       return
       end
C
        FUNCTION EOUT(ESUM,ILIM)
C       
C       NOT used in codes	
C	
        DIMENSION ESUM(6)
C
        IF (ESUM(1).GT.ESUM(2).AND.ESUM(2).GE.ESUM(3)) GO TO 10
        IF (ESUM(3).GT.ESUM(2).AND.ESUM(2).GE.ESUM(1)) GO TO 20
        IF (ESUM(2).GE.ESUM(1).AND.ESUM(1).GE.ESUM(3)) GO TO 10
        IF (ESUM(2).GE.ESUM(3).AND.ESUM(1).LT.ESUM(3)) GO TO 20
        IF (ESUM(2).LT.ESUM(1).AND.ESUM(2).LT.ESUM(3)) THEN
            IF (ESUM(1).GE.ESUM(3)) GO TO 10
            IF (ESUM(1).LT.ESUM(3)) GO TO 20
        END IF
10      EOUT = ESUM(2) + ESUM(3)
        ILIM = 1
        RETURN
20      EOUT = ESUM(3)
        ILIM = 2
        RETURN
        END
c
        FUNCTION SHP(DEOUT,ETOT,DX,OK)
c===================================================================
c     Formulae for shower coordinate in wall2
c     Author:  N.Russakovich
c===================================================================
        COMMON /ALFAW2/ ALFA
        LOGICAL OK
        DATA A1/2.91545/,A2/-.186/
C
        IF (DEOUT.GT.ETOT.OR.ETOT.EQ.0.) THEN
          SHP = 0.
          OK = .FALSE.
          RETURN
        END IF
        OK = .TRUE.
        REOUT = DEOUT/ETOT
        IF (ABS(REOUT).LT.0.001) THEN
          SHP = DX/2.
          RETURN
        END IF
        TG = TAN(A1*REOUT)              
        DISCR = SQRT(4. + TG*TG*(A2*A2*DX*DX - 4.))
        SHP = ((A2*DX*TG - 2.) + DISCR)/2./A2/TG
C
        RETURN
        END
