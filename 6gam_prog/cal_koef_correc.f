      subroutine cal_koef_correc()
      implicit none
      COMMON /KOEF/ matr(576,576)
      COMMON /MOMENT/ pi0mass
c      double precision beta
      real alfa, A, pi0mass, c12, matr, ClSum1, ClSum2, M
      integer l, k, LL, NN, i, j, n
      logical Din, Dinn
      dimension alfa(576), A(576)
      data matr/331776*0./
c LL - number of cells in cluster C2
c NN - number of cells in cluster C1      
c zdes' - inicializaciya alfa, A, c12, M, 
      data alfa/576*1./, A/576*1./, c12/1./, M/1./
c
      LL = 3
      NN = 3
c
      do l=1,LL
        ClSum2 = ClSum2 + alfa(l)*A(l)
      enddo
      do k=1,NN
        ClSum1 = ClSum1 + alfa(k)*A(k)
      enddo

      do i = 1,576
        do j = 1,576
c       zdes' vstavit' otbor sobytij, v kotoryh zasvechena i-ya yachejka
c       i v etom zhe clastere yachejka "n" (Din = .true.)
          Din = .true.
c Dinn - sobytiya v kotoryh i-ya i n-ya yachejki nahodyatsya v raznyh klasterah
          Dinn = .false.
          if (Din.and.Dinn) stop"Din = Dinn, ERROR"
          n = j + 576*(i-1)
          if (Din)  matr(i,j)=matr(i,j)+(c12**2/(2*M))*alfa(n)*A(n)*
     &                                       ClSum2**2*alfa(i)*A(i)
          if (Dinn) matr(i,j)=matr(i,j)+(c12**2/(2*M))*alfa(n)*A(n)*
     &                                   ClSum2*ClSum1*alfa(i)*A(i) 
        enddo
      enddo
      
      return
      end
