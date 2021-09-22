C ..
      SUBROUTINE filling (ICL,EFM)
C===============================================================
C     CALLED by CALIBR2. Stores effective masses in every cell               
C     ICL - clusters' number for gammas                        
C     EFM - eff.mass for two gammas                            
C                                                              
C     Author:  CGA 22.05.91                                            
C     Updated  20.04.04  NLR                          
C===============================================================

      PARAMETER ( NXWALL = 24, NYWALL = 24, NCELW = NXWALL*NYWALL )

      COMMON /W2FIL / WFIL(NXWALL,NYWALL,50)
      COMMON /WSFIL / WSFIL(8,8,50),NITERS(8,8)
      COMMON /SHOWR / NCLUST,nclws,NCELLC(60),XSHW(3,60),
     1                YSHW(3,60),ESHW(3,60),iamp_clus(60),DUM(60)
      COMMON /CLLIM / nclus,IXD(60),IXU(60),IYD(60),IYU(60)
      COMMON /W2COPY/ nctot,IEN2(24,24),IENS(8,8)
      COMMON /AMPW2 / ncellb,ncells,IWL2(24,24),IWLS(8,8)
      COMMON /CELFIL/ ICFIL(NXWALL,NYWALL),ISFIL(8,8)
      COMMON /EFMFIL/ EFMN2(24,24),EFMNS(8,8)
      common /weight2/ w2big(24,24),w2small(8,8)
      COMMON /NMFIL / WM2FIL(24,24),WMSFIL(8,8)
      COMMON /EFMean/ EFMb(24,24),EFMSM(8,8)
        integer nt,icon
      COMMON /NTSTAT/ NT,icon
      COMMON /MOMENT/pi0mass, PLAB(3,60)
      common /eta0  /etamass, etamin, etamax

      DIMENSION  ICL(2)
      DATA AMPI0 /135./
C ..
      call vzero(efmb,576)
      call vzero(efmsm,64)
            N  = INT(EFM/10.) + 1
C
      DO 100 I = 1,2

         IC  = ICL(I)
         IXB = IXD(IC)
         IYB = IYD(IC)
         IXE = IXU(IC)
         IYE = IYU(IC)

C --- IN CENTRAL PART OF SHD-2 --------------

          IF (IC.LE.NCLWS) THEN
c        write(*,*)'filling: begin of central part',IXB,IXE,IYB,IYE

            DO 10 IX = IXB,IXE
            DO 10 IY = IYB,IYE
              IF (IWLS(IX,IY).LT.5) GO TO 10
              ISFIL(IX,IY)  = ISFIL(IX,IY)  + 1
              WEIGHT = FLOAT(IENS(IX,IY))/ESHW(1,IC)            
              if (efm.lt.pi0mass) weight=weight*0.9552
              if (n.le.50) WSFIL(IX,IY,N) = WSFIL(IX,IY,N) + WEIGHT
              w2small(ix,iy)=w2small(ix,iy)+weight**2
              EFMNS(IX,IY) = EFMNS(IX,IY) + EFM*WEIGHT
              WMSFIL(IX,IY) = WMSFIL(IX,IY) + WEIGHT
              if(wmsfil(ix,iy).eq.0)goto 10
              EFMSM(IX,IY) = EFMNS(IX,IY)/WMSFIL(IX,IY)
              if (nt.eq.0) then
                id = (iy-1)*8 + ix+1000
c                call hf1(id,efmsm(ix,iy),1.)
              endif
10         CONTINUE
           GO TO 100
         END IF

C --- HIT IN BIG CELLS ----------------------
c                write(*,*)'filling: begin of side part'

       DO 30 IX = IXB,IXE
       DO 30 IY = IYB,IYE
         IF (IWL2(IX,IY).LT.5) GO TO 30
         IF (IX.GT.10.AND.IX.LT.15.AND.IY.GT.10.AND.IY.LT.15) THEN
           IX2 = (IX - 10)*2
           IX1 = IX2 - 1
           IY2 = (IY - 10)*2
           IY1 = IY2 - 1
           DO 40 IS = IX1,IX2
           DO 40 JS = IY1,IY2
             IF (IWLS(IX,IY).LT.5) GO TO 40
             ISFIL(IS,JS)  = ISFIL(IS,JS)  + 1
             WEIGHT = FLOAT(IENS(IS,JS))/ESHW(1,IC)
             if (efm.lt.pi0mass) weight=weight*0.9552
             if (n.le.50) WSFIL(IS,JS,N) = WSFIL(IS,JS,N) + WEIGHT
             w2small(ix,iy)=w2small(ix,iy)+weight**2
             EFMNS(IS,JS) = EFMNS(IS,JS) + EFM*WEIGHT
             WMSFIL(IS,JS) = WMSFIL(IS,JS) + WEIGHT
              if(wmsfil(ix,iy).eq.0)goto 40
             EFMSM(IX,IY) = EFMNS(IX,IY)/WMSFIL(IX,IY)
             if (nt.eq.0) then
              id = (iy-1)*8 + ix+1000
c              call hf1(id,efmsm(ix,iy),1.)
             endif
 40        CONTINUE
           GO TO 30
         END IF

         ICFIL(IX,IY)  = ICFIL(IX,IY)  + 1
         WEIGHT = FLOAT(IEN2(IX,IY))/ESHW(1,IC)
         w2big(ix,iy)=w2big(ix,iy)+weight**2
         if (efm.lt.pi0mass) weight=weight*0.9552
         if (n.le.50) WFIL(IX,IY,N) = WFIL(IX,IY,N) + WEIGHT
         EFMN2(IX,IY) = EFMN2(IX,IY) + EFM*WEIGHT
         WM2FIL(IX,IY) = WM2FIL(IX,IY) + WEIGHT
              if(wm2fil(ix,iy).eq.0)goto 30
         EFMb(IX,IY) = EFMN2(IX,IY)/WM2FIL(IX,IY)
         if (nt.eq.0) then
              id = (iy-1)*24 + ix
c              call hf1(id,efmb(ix,iy),1.)
         endif
30     CONTINUE     
c        write(*,*)'filling: side part'
100    CONTINUE

       RETURN
       END

      subroutine fill_eta(icl, efm)
      PARAMETER ( NXWALL = 24, NYWALL = 24, NCELW = NXWALL*NYWALL )
      dimension icl(2)
      COMMON /CLLIM / nclus,IXD(60),IXU(60),IYD(60),IYU(60)
      COMMON /SHOWR / NCLUST,nclws,NCELLC(60),XSHW(3,60),
     1                YSHW(3,60),ESHW(3,60),iamp_clus(60),DUM(60)
      COMMON /AMPW2 / ncellb,ncells,IWL2(24,24),IWLS(8,8)
      COMMON /CELeta/ ICeta(NXWALL,NYWALL),ISeta(8,8)
      COMMON /EFMeta/ EFeta2(24,24),EFetaS(8,8)
      COMMON /NMeta / WM2eta(24,24),WMSeta(8,8)
      COMMON /W2COPY/ nctot,IEN2(24,24),IENS(8,8)
      COMMON /EFMeanEta/ EFMbEta(24,24),EFMSMEta(8,8)
      common /eta0  /etamass, etamin, etamax


      DO 100 I = 1,2

         IC  = ICL(I)
         IXB = IXD(IC)
         IYB = IYD(IC)
         IXE = IXU(IC)
         IYE = IYU(IC)

         IF (IC.LE.NCLWS) THEN
c        write(*,*)'filling: begin of central part',IXB,IXE,IYB,IYE

            DO 10 IX = IXB,IXE
            DO 10 IY = IYB,IYE
              IF (IWLS(IX,IY).LT.5) GO TO 10
              ISeta(IX,IY)  = ISeta(IX,IY)  + 1
              WEIGHT = FLOAT(IENS(IX,IY))/ESHW(1,IC)            
              if (efm.lt.etamass) weight=weight*0.9252
c              if (n.le.50) WSFIL(IX,IY,N) = WSFIL(IX,IY,N) + WEIGHT
              EFetaS(IX,IY) = EFetaS(IX,IY) + EFM*WEIGHT
              WMSeta(IX,IY) = WMSeta(IX,IY) + WEIGHT
              if(wmseta(ix,iy).eq.0)goto 10
              EFMSMEta(IX,IY) = EFetaS(IX,IY)/WMSeta(IX,IY)
10         CONTINUE
           GO TO 100
         ENDIF

C --- HIT IN BIG CELLS ----------------------
c                write(*,*)'filling: begin of side part'

       DO 30 IX = IXB,IXE
       DO 30 IY = IYB,IYE
         IF (IWL2(IX,IY).LT.5) GO TO 30
         IF (IX.GT.10.AND.IX.LT.15.AND.IY.GT.10.AND.IY.LT.15) THEN
           IX2 = (IX - 10)*2
           IX1 = IX2 - 1
           IY2 = (IY - 10)*2
           IY1 = IY2 - 1
           DO 40 IS = IX1,IX2
           DO 40 JS = IY1,IY2
             IF (IWLS(IX,IY).LT.5) GO TO 40
             ISeta(IS,JS)  = ISeta(IS,JS)  + 1
             WEIGHT = FLOAT(IENS(IS,JS))/ESHW(1,IC)
             if (efm.lt.etamass) weight=weight*0.9252
c             if (n.le.50) WSFIL(IS,JS,N) = WSFIL(IS,JS,N) + WEIGHT
             EFetaS(IS,JS) = EFetaS(IS,JS) + EFM*WEIGHT
             WMSeta(IS,JS) = WMSeta(IS,JS) + WEIGHT
              if(wmseta(ix,iy).eq.0)goto 40
             EFMSMEta(IX,IY) = EFetaS(IX,IY)/WMSeta(IX,IY)
 40        CONTINUE
           GO TO 30
         END IF

         ICeta(IX,IY)  = ICeta(IX,IY)  + 1
         WEIGHT = FLOAT(IEN2(IX,IY))/ESHW(1,IC)
         if (efm.lt.etamass) weight=weight*0.9252
c         if (n.le.50) WFIL(IX,IY,N) = WFIL(IX,IY,N) + WEIGHT
         EFeta2(IX,IY) = EFeta2(IX,IY) + EFM*WEIGHT
         WM2eta(IX,IY) = WM2eta(IX,IY) + WEIGHT
              if(wm2eta(ix,iy).eq.0)goto 30
         EFMbEta(IX,IY) = EFeta2(IX,IY)/WM2eta(IX,IY)
30     CONTINUE     
100    CONTINUE

       RETURN
       END
