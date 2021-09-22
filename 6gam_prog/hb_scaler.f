C     ********************
      SUBROUTINE hb_scaler
C     ********************
C    *******************************************************************
C    * Books the histogramms for HYPERON scalers LE-69                 *
C    * called by event_decode_2005                                     *
C    * Hyperon logbook 2008 14.11.2009                                 *
C    * Счета в run                                                     *
C    *  ===========                                                    *
C    *     0 - N spill                                                 *
C    *     1 - Number of triggers in run                               *
C    *     2 - Number of pi                                            *
C    *     3 - Number of K                                             *
C    *     4 ---------------------------                               *
C    *     5 ---------------------------                               *
C    *     6 ---------------------------                               *
C    *     7 ---------------------------                               *
C    * Счета между принятыми триггерами                                *
C    * ================================                                *
C    *     8 - Beam (M4 ^ Phys Phys ворота)                            *
C    *     9 - K+pi (M4^C2   ^ Phys ворота)                            *
C    *     10 - pi  (M4^C1   ^ Phys ворота)                            *
C    *     11 - N Phys. trig.                                          *
C    * Счета в Spill                                                   *
C    * =============                                                   *
C    *     12 - M4 ^ Phys Phys ворота                                  *
C    *     13 - Phys. trig. ^ Phys Phys ворота                         *
C    *     14 - Clock 10 MHz                                           *
C    *     15 - Number of trigger accepted in spill                    *
C    *                                                                 *
C    * Author : D.I. Patalakha                                         *
C    * Created on :                9 March 2011                        *
C    * Last modified on :          13 Apr  2012 by S.Evdokimov         *
C    *******************************************************************
C-------- Start to book scaler histogramms
C......................fired wire position
      call HBOOK1(-151,
     *            'Beam vs Spill;Spill;Beam;',
     *            101,-0.5,100.5,0.)
      call HBOOK1(-152,
     *            'M4 vs Spill;Spill;M4;',
     *            101,-0.5,100.5,0.)
      call HBOOK1(-153,
     *            'Triggers vs Spill;Spill;Triggers;',
     *            101,-0.5,100.5,0.)
      call HBOOK1(-161,
     *            'N([p]^+!)/M4 vs Spill;Spill;N([p]^+!)/M4, "Y#;',
     *            101,-0.5,100.5,0.)
      call HBOOK1(-162,
     *            'Triggers/M4 vs Spill;Spill;Triggers/M4, "Y#;',
     *            101,-0.5,100.5,0.)
      call HBOOK1(-163,
     *            'M4/Beam vs Spill;Spill;M4/Beam, "Y#;',
     *            101,-0.5,100.5,0.)
      call HBOOK1(-251,
     *            'K^+!/[p]^+! in Beam;'//
     *            'K^+!/[p]^+! in Beam;'//
     *            'Events;',
     *            1000,0.,10,0.)
      call HBOOK1(-253,
     *            'K^+!/[p]^+! in Beam/M4 vs Spill;'//
     *            'Spill;'//
     *            'K^+!/[p]^+! in Beam/M4, "Y#;',
     *            101,-0.5,100.5,0.)
      call HBOOK1(-254,
     *            'K^+!/[p]^+! in Beam/M4 vs Spill;'//
     *            'Spill;'//
     *            'K^+!/[p]^+! in Beam/M4, "Y#;',
     *            101,-0.5,100.5,0.)
      call HBOOK1(-255,
     *            'K^+!/Beam in event;'//
     *            'K^+!/Beam, "Y#;'//
     *            'Events;',
     *            1000,0.,100.,0.)
      call HBOOK1(-256,
     *            '[p]^+!/Beam  in event;'//
     *            '[p]^+!/Beam, "Y#;'//
     *            'Events;',
     *            1000,0.,1000.,0.)
CEvd+
      call HBOOK1(-250,
     *            'total counters;'//
     *            'N beam m4, N pi, N K, N phys tr, N phys tr acc;'//
     *            'Counts;',
     *            5,0.,5.,0.)
      call HBOOK1(-252,
     *            'total counters;'//
     *            'm4 by spill, m4 by event, 
     *            phys.trig. by spill, phys.trig. by event;'//
     *            'Counts;',
     *            4,0.,4.,0.)
      call HBOOK1(-257,
     *            'total counters;'//
     *            'event and spill counters;'//
     *            'Counts;',16,0.,16.,0.)
      call HBOOK1(-1115,'counter 15 = clock 10MHz',
     *     20000,0.,100000000.,0.)
      call HBOOK1(-1113,'counter 13 = M4',
     *     10000,0.,10000.,0.)
      call HBOOK1(-1114,'counter 14 = phys.trig',
     *     10000,0.,10000.,0.)
      call HBOOK1(-1116,'counter 16 = trig. acc.',
     *     100000000,0.,100000000.,0.)

C-------- End to book scaler histogramms
      RETURN
      END
