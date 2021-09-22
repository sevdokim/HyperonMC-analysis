C..
      SUBROUTINE CORREC
C==============================================================
C     to correct SHD-2 calibration coefisients.               =
C                                                             =
C     5.05.91 CGA                                             =
C     LAST CORRECTION  15.12.03  NLR                          =
C==============================================================
      PARAMETER (NXWALL =24, NYWALL =24, NCELLW =NXWALL*NYWALL)

      COMMON /CLB2  / CLB2(NXWALL,NYWALL),CLBS(8,8)
      COMMON /CALIB / nstep,NSTMAX,RATIO(NXWALL,NYWALL)
      COMMON /W2FIL / WFIL(NXWALL,NYWALL,50)
      COMMON /WSFIL / WSFIL(8,8,50),IDEFS(8,8)
      COMMON /CELFIL/ ICFIL(NXWALL,NYWALL),ISFIL(8,8)
      COMMON /LUN   / LU,LUN,LUNST
      COMMON /IFCOR / IFCOR2(24,24),IFCORS(8,8)
      COMMON /MINST / MINST,TWOSIG
      COMMON /EFMFIL/ EFMN2(NXWALL,NYWALL),EFMNS(8,8)
      COMMON /NMFIL / WM2FIL(24,24),WMSFIL(8,8)
      COMMON /MASFIL/ BIGM(24,24),SMALM(24,24)
      COMMON /MASFSM/ BIGS(8,8),SMALS(8,8),RATIOS(8,8)
      COMMON /DEFMAS/ IDEF(24,24)
      COMMON /EFMean/ EFMb(24,24),EFMSM(8,8)
      COMMON /CELeta/ ICeta(NXWALL,NYWALL),ISeta(8,8)
      COMMON /EFMeta/ EFeta2(24,24),EFetaS(8,8)
      COMMON /NMeta / WM2eta(24,24),WMSeta(8,8)
      COMMON /EFMeanEta/ EFMbEta(24,24),EFMSMEta(8,8)
      common/eta0/etamass, etamin, etamax
      COMMON /MOMENT/pi0mass, PLAB(3,60)
      common /coefs/ coefs_s(8,8),coefs_b(24,24),w_s(8,8),w_b(24,24)

C --- HALFB IS 1/2 PART OF BIN  ---

      DATA HALFB /   5.0/,COEFF/1./
      DATA AMPI0 / 135.0/
      data qalfa / 0.3/

C ..
C --- FOR  BIG  CELLS ----------------------------------
C ..

      DO  100 IX = 1, NXWALL
      DO   90 IY = 1, NYWALL
        IF (ICFIL(IX,IY).LE.MINST) GO TO 90

c       RATIO(IX,IY) = (BIGM(IX,IY) - SMALM(IX,IY))/
c    *          (BIGM(IX,IY) + SMALM(IX,IY))
c       DELTA = TWOSIG*(1. - SQRT(1. - ABS(RATIO(IX,IY))))
c    *          *(1. + ABS(RATIO(IX,IY)))**2
c       IF (BIGM(IX,IY).GT.SMALM(IX,IY)) DELTA = -DELTA
        IF (WM2FIL(IX,IY).EQ.0) GO TO 90
        EFMb(IX,IY) = EFMN2(IX,IY)/WM2FIL(IX,IY)
        IDEF(IX,IY) = IFIX(AMPI0 - EFMb(IX,IY))
        IF(IFCOR2(IX,IY).EQ.1) GO TO 90
c       CLB2(IX,IY)  = CLB2(IX,IY)*(1. + DELTA)
        coefs_b(ix,iy)=pi0mass/EFMb(IX,IY)-1.
        w_b(ix,iy)=EFMN2(IX,IY)
        CLB2(IX,IY) = CLB2(IX,IY)*(1+qalfa*(pi0mass/EFMb(IX,IY)-1.))

 90    CONTINUE
100    CONTINUE
C ..
C --- FOR SMALL CELLS ----------------------------------
C ..
       DO  200 IX = 1, 8
       DO  190 IY = 1, 8
c       RATIOS(IX,IY) = (BIGS(IX,IY) - SMALS(IX,IY))/
c    *          (BIGS(IX,IY) + SMALS(IX,IY))
c       DELTA = TWOSIG*(1. - SQRT(1. - ABS(RATIOS(IX,IY))))
c    *          *(1. + ABS(RATIOS(IX,IY)))**2
c       IF (BIGS(IX,IY).GT.SMALS(IX,IY)) DELTA = -DELTA
        IF (ISFIL(IX,IY).LE.MINST) GO TO 190
        IF (WMSFIL(IX,IY).EQ.0) GO TO 190
        EFMSM(IX,IY) = EFMNS(IX,IY)/WMSFIL(IX,IY)
        IDEFS(IX,IY) = IFIX(AMPI0 - EFMSM(IX,IY))
        IF(IFCORS(IX,IY).EQ.1) GO TO 190
c       CLBS(IX,IY)  = CLBS(IX,IY)*(1. + DELTA)
        coefs_s(ix,iy)=pi0mass/EFMSM(IX,IY)-1.
        w_s(ix,iy)=EFMNS(IX,IY)
        CLBS(IX,IY) = CLBS(IX,IY)*(1+qalfa*(pi0mass/EFMSM(IX,IY)-1.))
190    CONTINUE
200    CONTINUE
       return
c-------------------------------------------------------
c-------------korrektirovka na eta-meson----------------
c-------------------------------------------------------
C ..
C --- FOR  BIG  CELLS ----------------------------------
C ..

      DO  101 IX = 1, NXWALL
      DO   91 IY = 1, NYWALL
        IF (ICeta(IX,IY).LE.MINST) GO TO 91
        IF (WM2eta(IX,IY).EQ.0) GO TO 91
        EFMbEta(IX,IY) = EFeta2(IX,IY)/WM2eta(IX,IY)
        IDEF(IX,IY) = IFIX(etamass - EFMbEta(IX,IY))
        IF(IFCOR2(IX,IY).EQ.1) GO TO 91
        CLB2(IX,IY) = CLB2(IX,IY)*etamass/EFMbEta(IX,IY)

 91   CONTINUE
 101  CONTINUE
C ..
C --- FOR SMALL CELLS ----------------------------------
C ..
       DO  201 IX = 1, 8
       DO  191 IY = 1, 8
        IF (ISeta(IX,IY).LE.MINST) GO TO 191
        IF (WMSeta(IX,IY).EQ.0) GO TO 191
        EFMSMEta(IX,IY) = EFetaS(IX,IY)/WMSeta(IX,IY)
        IDEFS(IX,IY) = IFIX(AMPI0 - EFMSMEta(IX,IY))
        IF(IFCORS(IX,IY).EQ.1) GO TO 191
        CLBS(IX,IY) = CLBS(IX,IY)*etamass/EFMSMEta(IX,IY)
 191  CONTINUE
 201  CONTINUE

      RETURN
      END
