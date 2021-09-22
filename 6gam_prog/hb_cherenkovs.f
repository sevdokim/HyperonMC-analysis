C     ************************
      SUBROUTINE hb_cherenkovs
C     ************************
C    *******************************************************************
C    * Books the histogramms for Cherencov 1,2 and 3                   *
C    * called by event_decode_2005                                     *
C    * C1 - detect pions                                               *
C    * C2 - detect pions and kaons                                     *
C    * C3 - detect pions                                               *
C    *                                                                 *
C    * Author : D.I. Patalakha                                         *
C    * Created on :                09 March 2011                       *
C    * Last modification made on : 15 March 2011                       *
C    * Modified by Kondratyuk on : 23 March 2016                       *
C    *******************************************************************
! I removed (commented) some histograms from here, because they are of no use for us. See Cherenkovs.f for more info
      parameter(npawc=2 000 000)
      character *60 h_name
      COMMON /PAWC  /  HMEM(npawc)
      common/triggers/ c1c2c3(3), c1min, c2min, c3min, c1c2c3_adr      

C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
!           call hbook1(5101,
!      +    'C1([p]);ADC counts;Events;',4000,0.,4000.,0.)
!           call hbook1(5102,
!      +    'C2(K);ADC counts;Events;',4000,0.,4000.,0.)
!           call hbook1(5103,
!      +    'C3([p]);ADC counts;Events;',4000,0.,4000.,0.)
!           call hbook1(-5191,
!      +    'C1([p]) if C2(K) and C3([p]) too;ADC counts;Events;',
!      +    4000,0.,4000.,0.)
!           call hbook1(-5192,
!      +    'C2(K) if C1([p]) and C3([p]) too;ADC counts;Events;',
!      +    4000,0.,4000.,0.)
!           call hbook1(-5193,
!      +    'C3([p]) if C1([p])and C2(K) too;ADC counts;Events;',
!      +    4000,0.,4000.,0.)
!           call hbook1(-5101,'C1([p])-C2(K);ADC counts;Events;',
!      +    4000,-4000.,4000.,0.)
!           call hbook1(-5102,'C1([p])-C3([p]);ADC counts;Events;',
!      +    4000,-4000.,4000.,0.)
!           call hbook1(-5103,'C2(K)-C3([p]);ADC counts;Events;',
!      +    4000,-4000.,4000.,0.)
!           call hbook1(-5104,
!      +    'C1([p])-C2(K) if C2(K) GT 120.;ADC counts;Events;',
!      +    4000,-4000.,4000.,0.)
!           call hbook1(-5105,
!      +    'C1([p])-C3([p]) if C2(K)GT 120.;ADC counts;Events;',
!      +    4000,-4000.,4000.,0.)
!           call hbook1(-5106,
!      +    'C2(K)-C3([p]) if C2(K) GT 120.;ADC counts;Events;',
!      +    4000,-4000.,4000.,0.)
! C-------------------------------------Start Noice C1, C2, C3 -----------------
! CC-------- C1(Pi)
!           call hbook1(-5111,
!      +    'C1([p]) if No C2(K);ADC counts;Events;',
!      +                4000,0.,4000.,0.)
!           call hbook1(-5112,
!      +    'C1([p]) if No C3([p]);ADC counts;Events;',
!      +                4000,0.,4000.,0.)
!           call hbook1(-5113,
!      +    'C1([p]) if No C2(K) or C3([p]);ADC counts;Events;',
!      +                4000,0.,4000.,0.)
!           call hbook1(-5114,
!      +    'C1([p]) if No C2(K) and C3([p]);ADC counts;Events;',
!      +                4000,0.,4000.,0.)
! CC-------- C2(Pi+)
!           call hbook1(-5121,
!      +    'C2(K) if No C1([p]);ADC counts;Events;',
!      +                4000,0.,4000.,0.)
!           call hbook1(-5122,
!      +    'C2(K) if No C3([p]);ADC counts;Events;',
!      +                4000,0.,4000.,0.)
!           call hbook1(-5123,
!      +    'C2(K) if No C1([p]) or C3([p]);ADC counts;Events;',
!      +                4000,0.,4000.,0.)
!           call hbook1(-5124,
!      +    'C2(K) if No C1([p]) and C3([p]);ADC counts;Events;',
!      +                4000,0.,4000.,0.)
! CC-------- C3(Pi+)
!           call hbook1(-5131,
!      +    'C3([p]) if No C1([p]);ADC counts;Events;',
!      +                4000,0.,4000.,0.)
!           call hbook1(-5132,
!      +    'C3([p]) if No C2(K);ADC counts;Events;',
!      +                4000,0.,4000.,0.)
!           call hbook1(-5133,
!      +    'C3([p]) if No C1([p]) or C2(K);ADC counts;Events;',
!      +                4000,0.,4000.,0.)
!           call hbook1(-5134,
!      +    'C3([p]) if No C1([p]) and C2(K);ADC counts;Events;',
!      +                4000,0.,4000.,0.)
! C-------------------------------------- End Noice C1, C2, C3 -----------------
! C------------------------------ Start Correlation C1, C2, C3 -----------------
!           call hbook2(5112,
!      +    'C1([p]) vs C2(K);C2 counts;C1 counts;',
!      +                200,0.,4000.,200,0.,4000.,0.)
!           call hbook2(5113,
!      +    'C1([p]) vs C3([p]);C3 counts;C1 counts;',
!      +                200,0.,4000.,200,0.,4000.,0.)
!           call hbook2(5123,
!      +    'C3([p]) vs C2(K);C2 counts;C3 counts;',
!      +                200,0.,4000.,200,0.,4000.,0.)
! C-------------------------------- End Correlation C1, C2, C3 -----------------
! C------------------------------ Start Correlation C1, C2, C3 -----------------
!           call hbook2(5912,
!      +    'C1([p]) vs C2(K) IF C1+C2+C3;C2 counts;C1 counts;',
!      +                200,0.,4000.,200,0.,4000.,0.)
!           call hbook2(5913,
!      +    'C1([p]) vs C3([p]) IF C1+C2+C3;C3 counts;C1 counts;',
!      +                200,0.,4000.,200,0.,4000.,0.)
!           call hbook2(5923,
!      +    'C3([p]) vs C2(K) IF C1+C2+C3;C2 counts;C3 counts;',
!      +                200,0.,4000.,200,0.,4000.,0.)
C-------------------------------- End Correlation C1, C2, C3 -----------------
C------------------------------ Start Cuts for Pi and K on  C1, C2, C3 -------
C---- Pi : C1(pi) > 250. and C3(pi) > 120. C2(K) > 250.
          call hbook2(5991,
     +    'C1([p]) vs C2(K) IF [p]-signal;C2 counts;C1 counts;',
     +                200,0.,4000.,200,0.,4000.,0.)
          call hbook2(5992,
     +    'C1([p]) vs C3([p]) IF [p]-signal;C3 counts;C1 counts;',
     +                200,0.,4000.,200,0.,4000.,0.)
          call hbook2(5993,
     +    'C3([p]) vs C2(K) IF [p]-signal;C2 counts;C3 counts;',
     +                200,0.,4000.,200,0.,4000.,0.)
!           call hbook1(-5991,
!      +    'C1([p]) if [p]-signal;ADC counts;Events;',4000,0.,4000.,0.)
!           call hbook1(-5992,
!      +    'C2(K) if [p]-signal;ADC counts;Events;',4000,0.,4000.,0.)
!           call hbook1(-5993,
!      +    'C3([p]) if [p]-signal;ADC counts;Events;',4000,0.,4000.,0.)
! C---- K  : C1(pi) < 250. and C3(pi) < 120. C2(K) > 250.
!           call hbook2(5994,
!      +    'C1([p]) vs C2(K) IF K-signal;C2 counts;C1 counts;',
!      +                200,0.,4000.,200,0.,4000.,0.)
!           call hbook2(5995,
!      +    'C1([p]) vs C3([p]) IF K-signal;C3 counts;C1 counts;',
!      +                200,0.,4000.,200,0.,4000.,0.)
!           call hbook2(5996,
!      +    'C3([p]) vs C2(K) IF K-signal;C2 counts;C3 counts;',
!      +                200,0.,4000.,200,0.,4000.,0.)
!           call hbook1(-5994,
!      +    'C1([p]) if K-signal;ADC counts;Events;',4000,0.,4000.,0.)
!           call hbook1(-5995,
!      +    'C2(K) if K-signal;ADC counts;Events;',4000,0.,4000.,0.)
!           call hbook1(-5996,
!      +    'C3([p]) if K-signal;ADC counts;Events;',4000,0.,4000.,0.)
C------------------------------ End Cuts for Pi and K on  C1, C2, C3 ---------
C------------------------------ Start Cuts for K on  C2, C3 ------------------------
          call hbook1(5870,
     +    'C1([p]) if no cut on [p]-signal in C2(K);'//
     +    'ADC counts;Events;', 4000,0.,4000.,0.)
          call hbook1(5880,
     +    'C3([p]) if no cut on [p]-signal in C2(K);'//
     +    'ADC counts;Events;', 4000,0.,4000.,0.)
CC
          call hbook1(5871,
     +    'C1([p]) if cut on [p]-signal in C2(K) "G# 200.;'//
     +    'ADC counts;Events;', 4000,0.,4000.,0.)
          call hbook1(5881,
     +    'C3([p]) if cut on [p]-signal in C2(K) "G# 200.;'//
     +    'ADC counts;Events;', 4000,0.,4000.,0.)
CC
          call hbook1(5872,
     +    'C1([p]) if cut on [p]-signal in C2(K) "G# 225.;'//
     +    'ADC counts;Events;', 4000,0.,4000.,0.)
          call hbook1(5882,
     +    'C3([p]) if cut on [p]-signal in C2(K) "G# 225.;'//
     +    'ADC counts;Events;', 4000,0.,4000.,0.)
CC
         call hbook1(5873,
     +    'C1([p]) if cut on [p]-signal in C2(K) "G# 250.;'//
     +    'ADC counts;Events;', 4000,0.,4000.,0.)
         call hbook1(5883,
     +    'C3([p]) if cut on [p]-signal in C2(K) "G# 250.;'//
     +    'ADC counts;Events;', 4000,0.,4000.,0.)
CC
         call hbook1(5874,
     +    'C1([p]) if cut on [p]-signal in C2(K) "G# 275.;'//
     +    'ADC counts;Events;', 4000,0.,4000.,0.)
         call hbook1(5884,
     +    'C3([p]) if cut on [p]-signal in C2(K) "G# 275.;'//
     +    'ADC counts;Events;', 4000,0.,4000.,0.)
CC
         call hbook1(5875,
     +    'C1([p]) if cut on [p]-signal in C2(K) "G# 300.;'//
     +    'ADC counts;Events;', 4000,0.,4000.,0.)
         call hbook1(5885,
     +    'C3([p]) if cut on [p]-signal in C2(K) "G# 300.;'//
     +    'ADC counts;Events;', 4000,0.,4000.,0.)
C------------------------------ Start Cuts for K on  C2, C3 ------------------------
!kondr++{
      call hbook1(5800,'c1,c2,c3 > threshhold & c1,c2,c3 > 0',10,0,10,0)
      call hbook1(5801,'c1 all', 2000,0,2000,0)
      call hbook1(5802,'c2 all', 2000,0,2000,0)
      call hbook1(5803,'c3 all', 2000,0,2000,0)

      call hbook1(5810,'pi+, k+, p+',5,0,5,0)
      write (h_name,'(3(A,F4.1))')'thresholds: c1m = ', c1min,
     + ', c2m = ', c2min,', c3m = ', c3min
      call hbook1(5820,h_name,5,0,5,0) 

      do i=1,20
        write (h_name,*)'c2 when c1 > ', i*15
        call hbook1(5820+i,h_name,2000,0,2000,0) 
        write (h_name,*)'c1 when c2 > ', i*15
        call hbook1(5720+i,h_name,2000,0,2000,0) 
        write (h_name,*)'c2 when c1 < ', i*15
        call hbook1(5620+i,h_name,2000,0,2000,0) 
      enddo
      
      call hf1(5820, 1.1, c1min)  ! write down the thresholds
      call hf1(5820, 2.2, c2min)
      call hf1(5820, 3.3, c3min)
!kondr--}

        RETURN
        END
