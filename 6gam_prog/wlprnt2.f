        SUBROUTINE  WLPRNI2 (IDIM,NX,NY,MESS)
        COMMON /LUN   / LU,LUN,LUN1
        DIMENSION IDIM(NX,NY)
        CHARACTER* (*) MESS
C
        WRITE (LUN,'(5X,A/)') MESS
        WRITE(LUN,1000) (I, I=1,NX)
C
        DO 20 J = 1,NY
          JINV = NY - J + 1
          WRITE(LUN,1010) JINV,(IDIM(I,JINV), I=1,NX)
20      CONTINUE
        RETURN
C
1000    FORMAT(4X,24I5/)
1010    FORMAT(I3,'+',24I5)
        END
C
        SUBROUTINE  BWLPRNR (RDIM,NX,NY,MESS)
        COMMON /LUN   / LU,LUN,LUN1
        DIMENSION RDIM(NX,NY)
        CHARACTER* (*) MESS
C
        WRITE (LUN,'(5X,A/)') MESS
        WRITE(LUN,1000) (I, I=1,NX)
C
        DO 20 J = 1,NY
          JINV = NY - J + 1
          WRITE(LUN,1010) JINV,(RDIM(I,JINV), I=1,NX)
20      CONTINUE
        RETURN
C
1000    FORMAT(4X,24I5/)
1010    FORMAT(I3,'+',24F5.1)
        END
