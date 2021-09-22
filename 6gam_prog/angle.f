      SUBROUTINE ANGLE (IP1,IP2,COSTETA,TETA)
C*******************************************************************
C     Calculates angle between two vectors
c     Author  N.Russakovich
c******************************************************************
      COMMON /GAMMAS/ NGAM, XGAM(60), YGAM(60), EGAM(60), DCOS(3,60)
      REAL*8 ALFAX1,ALFAX2,ALFAY1,ALFAY2,ALFAZ1,ALFAZ2,ALFA,DTETA
C
      ALFAX1 = DBLE(DCOS(1,IP1))
      ALFAX2 = DBLE(DCOS(1,IP2))
      ALFAY1 = DBLE(DCOS(2,IP1))
      ALFAY2 = DBLE(DCOS(2,IP2))
      ALFAZ1 = DBLE(DCOS(3,IP1))
      ALFAZ2 = DBLE(DCOS(3,IP2))
C
      ALFA = ALFAX1*ALFAX2 + ALFAY1*ALFAY2 + ALFAZ1*ALFAZ2
      IF (ALFA.GT.1.D00) ALFA = 1.D00
      COSTETA = SNGL(ALFA)
      DTETA = DACOS(ALFA)
      TETA = SNGL(DTETA)
C
      RETURN
      END
      
