C --------------------------
      subroutine cherenkovs
C -------------------------
C    *******************************************************************
C    * Treats the Cherencov 1,2 and 3 signals decoded in               *
C    * event_decode_2005.f                                             *
C    * Histogramms booked in hist_book_2004.f                          *
C    * C1 - detect pions                                               *
C    * C2 - detect pions and kaons                                     *
C    * C3 - detect pions                                               *
C    *                                                                 *
C    * Author : D.I. Patalakha                                         *
C    * Created on :                9 March 2011                        *
C    * Last modification made on : 9 March 2011                        *
C    * Modified by Kondratyuk    : 22 March 2016                       *
C    ******************************************************************* 
! almost all histogramms are sane and working, I commented with "!" ones which will never work. See hb_cherenkovs.f for more info
!       common /triggers/c1c2c3(3)                    !
      common /triggers/c1c2c3(3),c1min,c2min,c3min    ! ; kondr

C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
C            write(*,1001) (iw, c1c2c3(iw),iw=1,3)
C 1001       format(1x,3(" Cherencov ",I2," c1c2c3=",F8.2)/)
C
C--- C1(Pi+) signals....
!       if (c1c2c3(1) .gt. 0.) then
! C-- Cherencov signal for Pi+ (All 3 counters give signals)  ! is done below
! !          if (c1c2c3(2) .gt. 0. .and. 
! !      +       c1c2c3(3) .gt. 0. )  then
! !             call hf1(-5191,c1c2c3(1),1.)            !C1(Pi+)
! !             call hf1(-5192,c1c2c3(2),1.)            !C2(K+)
! !             call hf1(-5193,c1c2c3(3),1.)            !C3(Pi+)
! !             call hf2( 5912,c1c2c3(2), c1c2c3(1),1.) ! Corr C1(Pi+) vs C2(K+)
! !             call hf2( 5913,c1c2c3(3), c1c2c3(1),1.) ! Corr C1(Pi+) vs C3(Pi+)
! !             call hf2( 5923,c1c2c3(2), c1c2c3(3),1.) ! Corr C3(Pi+) vs C2(K+)
! !          endif
! 
!          if (c1c2c3(2) .gt. 0.) then
!             call hf2( 5112,c1c2c3(2), c1c2c3(1),1.) ! Corr C1(Pi+) vs C2(K+)
! !             call hf1(-5101,c1c2c3(1)-c1c2c3(2),1.)
! !             if (c1c2c3(3) .gt. 120.)
! !      +        call hf1(-5104,c1c2c3(1)-c1c2c3(2),1.)
!          else
!             call hf1(-5111,c1c2c3(1),1.)            !C1(Pi+) if no C2(K+)
!          endif
! 
!          if (c1c2c3(3) .gt. 0.) then
!             call hf2( 5113,c1c2c3(3), c1c2c3(1),1.) ! Corr C1(Pi+) vs C3(Pi+)
! !             call hf1(-5102,c1c2c3(1)-c1c2c3(3),1.)
! !             if (c1c2c3(3) .gt. 120.)
! !      +        call hf1(-5105,c1c2c3(1)-c1c2c3(3),1.)
!          else
!             call hf1(-5112,c1c2c3(1),1.)            !C1(Pi+) if no C3(Pi+)
!          endif
! 
!          if (c1c2c3(2) .lt. 0. .or.  c1c2c3(3) .lt. 0.)
!      +      call hf1(-5113,c1c2c3(1),1.)            !C1(Pi+) if no c2(Pi+) or C3(Pi+)
! !          if (c1c2c3(2) .lt. 0. .and. c1c2c3(3) .lt. 0.) 
! !      +      call hf1(-5114,c1c2c3(1),1.)            !C1(Pi+) if no c2(Pi+) and C3(Pi+)
!       endif

C--- C2(K+) signals....
! they are never < 0... commented ; kondr
!       if (c1c2c3(2) .gt. 0.) then
! 
!          if (c1c2c3(1) .lt. 0.)      call hf1(-5121,c1c2c3(2),1.)       !C2(K+) if no C1(Pi+)
!          if (c1c2c3(3) .lt. 0.)      call hf1(-5122,c1c2c3(2),1.)       !C2(K+) if no C3(Pi+)
!          if (c1c2c3(1) .lt. 0. .or.
!      +       c1c2c3(3) .lt. 0.)      call hf1(-5123,c1c2c3(2),1.)       !C2(K+) if no C1(Pi+) or no C3(Pi+)
!          if (c1c2c3(1) .lt. 0. .and.
!      +       c1c2c3(3) .lt. 0.)      call hf1(-5124,c1c2c3(2),1.)       !C2(K+) if no C1(Pi+) and no C3(Pi+)
!       endif

C--- C3(Pi+) signals....
!       if (c1c2c3(3) .gt. 0.) then
! 
! !          if (c1c2c3(1) .lt. 0.)     
! !      +      call hf1(-5131,c1c2c3(3),1.)        !C3(Pi+) if no C1(Pi+)
! !          if (c1c2c3(2) .lt. 0.)     then
! !             call hf1(-5132,c1c2c3(3),1.)        !C3(Pi+) if no C2(K+)
! !          else
!             call hf2( 5123,c1c2c3(2), c1c2c3(3),1.) ! Corr C3(Pi+) vs C2(K+)
! !             call hf1(-5103,c1c2c3(2)-c1c2c3(3),1.)
! !             if (c1c2c3(3).gt.120.)
! !      +          call hf1(-5106,c1c2c3(2)-c1c2c3(3),1.)
! !          endif
! 
! !          if (c1c2c3(1) .lt. 0. .or. c1c2c3(2) .lt. 0.)     
! !      +      call hf1(-5133,c1c2c3(3),1.)        !C3(Pi+) if no C1(Pi+) or no C2(K+)
! 
! !          if (c1c2c3(1) .lt. 0. .and. c1c2c3(2) .lt. 0.)
! !      +      call hf1(-5134,c1c2c3(3),1.)        !C3(Pi+) if no C1(Pi+) and no C2(K+)
!       endif
C    *******************************************************************
!       if (c1c2c3(3) .lt. 0. ) call hf1(-5111,c1c2c3(1),1.)
!       if (c1c2c3(1) .lt. 0. ) call hf1(-5133,c1c2c3(3),1.)
!       if (c1c2c3(1) .lt. 0. .or. c1c2c3(3) .lt. 0.)
!      +                        call hf1(-5122,c1c2c3(2),1.)
!       if (c1c2c3(1) .lt. 0. .and. c1c2c3(3) .lt. 0.)
!      +                        call hf1(-5123,c1c2c3(2),1.)

C---- 7Pi : 2011-04->  C1(pi) > 250. and C3(pi) > 120. C2(K) > 250.
C---- 7Pi : 2010-11->  C1(pi) > 300. and C3(pi) > 150. C2(K) > 150.
C---- 5Pi : 2010-11->  C1(pi) > 250. and C3(pi) > 150. C2(K) > 100.
!          if (c1c2c3(1) .gt. 0. .and. 
!      +       c1c2c3(3) .gt. 0. .and.
!      +       c1c2c3(2) .gt. 0.)                                   then
             call hf2( 5991,c1c2c3(2), c1c2c3(1),1.) ! Corr C1(Pi+) vs C2(K+)
             call hf2( 5992,c1c2c3(3), c1c2c3(1),1.) ! Corr C1(Pi+) vs C3(Pi+)
             call hf2( 5993,c1c2c3(2), c1c2c3(3),1.) ! Corr C3(Pi+) vs C2(K+)
!              call hf1(-5991,c1c2c3(1),1.)
!              call hf1(-5992,c1c2c3(2),1.)
!              call hf1(-5993,c1c2c3(3),1.)
!          endif
C---- 7K : 2011-04 -> C1(pi) < 250. and C3(pi) < 120. C2(K) > 250.
C---- 7K : 2010-11 -> C1(pi) < 300. and C3(pi) < 150. C2(K) > 0.
C---- 5K : 2010-11->  C1(pi) < 250. and C3(pi) < 150. C2(K) > 0.
!          if (c1c2c3(1) .le. 0. .and. 
!      +       c1c2c3(3) .le. 0. .and.
!      +       c1c2c3(2) .gt. 0.)                                     then
!              call hf2( 5994,c1c2c3(2), c1c2c3(1),1.) ! Corr C1(Pi+) vs C2(K+)
!              call hf2( 5995,c1c2c3(3), c1c2c3(1),1.) ! Corr C1(Pi+) vs C3(Pi+)
!              call hf2( 5996,c1c2c3(2), c1c2c3(3),1.) ! Corr C3(Pi+) vs C2(K+)
!              call hf1(-5994,c1c2c3(1),1.)
!              call hf1(-5995,c1c2c3(2),1.)
!              call hf1(-5996,c1c2c3(3),1.)
!          endif
C      endif
C    *************************************************************
       if (c1c2c3(1) .gt. 0. .and. c1c2c3(3).gt.0.)          then
                  call hf1(5870,c1c2c3(1),1.)
                  call hf1(5880,c1c2c3(3),1.)
             if (c1c2c3(2) .lt. 30.)                        then
                  call hf1(5871,c1c2c3(1),1.)
                  call hf1(5881,c1c2c3(3),1.)
             endif
             if (c1c2c3(2) .lt. 60.)                        then
                  call hf1(5872,c1c2c3(1),1.)
                  call hf1(5882,c1c2c3(3),1.)
             endif
             if (c1c2c3(2) .lt. 90.)                        then
                  call hf1(5873,c1c2c3(1),1.)
                  call hf1(5883,c1c2c3(3),1.)
             endif
             if (c1c2c3(2) .lt. 120.)                       then
                  call hf1(5874,c1c2c3(1),1.)
                  call hf1(5884,c1c2c3(3),1.)
             endif
             if (c1c2c3(2) .lt. 150.)                       then
                  call hf1(5875,c1c2c3(1),1.)
                  call hf1(5885,c1c2c3(3),1.)
             endif
        endif


      if (c1c2c3(1).gt.c1min) call hf1(5800, 1.,1.)
      if (c1c2c3(2).gt.c2min) call hf1(5800, 2.,1.)
      if (c1c2c3(3).gt.c3min) call hf1(5800, 3.,1.)

      if (c1c2c3(1).gt.0) call hf1(5800, 5.,1.)
      if (c1c2c3(2).gt.0) call hf1(5800, 6.,1.)
      if (c1c2c3(3).gt.0) call hf1(5800, 7.,1.)
      

      call hf1(5801,c1c2c3(1),1.)
      call hf1(5802,c1c2c3(2),1.)
      call hf1(5803,c1c2c3(3),1.)

      if(c1c2c3(2).ge.c2min.and.c1c2c3(1).ge.c1min)call hf1(5810,1.,1.) !pi+ only
      if(c1c2c3(2).ge.c2min.and.c1c2c3(1).lt.c1min)call hf1(5810,2.,1.) !k+ only
      if(c1c2c3(2).lt.c2min.and.c1c2c3(1).lt.c1min)call hf1(5810,3.,1.) !p+ only  ! we don't use them
      
      do i=1,20
        if(c1c2c3(1).ge.i*15) call hf1(5820+i,c1c2c3(2),1.) 
        if(c1c2c3(2).ge.i*15) call hf1(5720+i,c1c2c3(1),1.) 
        if(c1c2c3(1).le.i*15) call hf1(5620+i,c1c2c3(2),1.) 
      enddo

!       if(c1c2c3(2).ge.c2min.and.c1c2c3(1).ge.c1min)call hf1(5810,1.,1.) !pi+ only
!       if(c1c2c3(2).ge.c2min.and.c1c2c3(1).lt.c1min)call hf1(5810,2.,1.) !k+ only
!       if(c1c2c3(2).lt.c2min.and.c1c2c3(1).lt.c1min)call hf1(5810,3.,1.) !p+ only  ! we don't use them
! !kondr++{
!        write (*,*)'п1; ',c1c2c3(1),c1c2c3(2),c1c2c3(3),
!      +  c1min,c2min,c3min
!         call hf1(5820,1,c1min,1.)
!         call hf1(5820,1,c2min,2.)
!         call hf1(5820,1,c3min,3.)
! !kondr--}
      return
      end
