C**********************************
      subroutine track_scalers_prnt
C**********************************
C***** track parametrization is x = Ax + Bx*z
C                               y = Ay + By*z
      real ax, bx, ay, by
      COMMON /mwpc_track/ ax, bx, ay, by 
CEvd+
      integer*8 hy_sca,
     *        events_in_run, n_beam_in_run, n_pi_plus_in_run,
     *        n_phys_trig_into_gate_in_run,
     *        n_phys_trig_into_gate_accepted_in_run,
     *        m4_into_gate_in_run,
     *        n_k_pi_3_event, n_k_pi_4_event,
     *        n_k_pi_3_spill, n_k_pi_4_spill,
     *        n_k_pi_3_run,   n_k_pi_4_run,
     *        n_phys_trig_into_gate_tot,
     *        n_phys_trig_into_gate_accepted_tot,
     *        n_beam_tot_m4_into_gate, n_beam_tot_pi_plus,
     *        n_beam_tot_k
      COMMON /scalers_17/ hy_sca(18),
     *        events_in_run, n_beam_in_run, n_pi_plus_in_run,
     *        n_phys_trig_into_gate_in_run,
     *        n_phys_trig_into_gate_accepted_in_run,
     *        m4_into_gate_in_run,
     *        n_k_pi_3_event, n_k_pi_4_event,
     *        n_k_pi_3_spill, n_k_pi_4_spill,
     *        n_k_pi_3_run,   n_k_pi_4_run,
     *        n_phys_trig_into_gate_tot,
     *        n_phys_trig_into_gate_accepted_tot,
     *        n_beam_tot_m4_into_gate, n_beam_tot_pi_plus,
     *        n_beam_tot_k
CEvd-

C12345678901234567890123456789012345678901234567890123456789012345678901
      call mwpc_track_prnt
      call hypero_sca_prnt
      RETURN
      END
C*******************************
      subroutine mwpc_track_prnt
C*******************************
      real ax, bx, ay, by
      COMMON /mwpc_track/ ax, bx, ay, by 
C12345678901234567890123456789012345678901234567890123456789012345678901
      write(*,1000) ax, bx, ay, by
 1000 FORMAT(1x,"Beam : Ax=", F10.5," Bx=", F10.5,
     *                " Ay=", F10.5," By=", F10.5)
      RETURN
      END
C*******************************
      subroutine hypero_sca_prnt
C*******************************
C12345678901234567890123456789012345678901234567890123456789012345678901
      integer*8 hy_sca,
     *        events_in_run, n_beam_in_run, n_pi_plus_in_run,
     *        n_phys_trig_into_gate_in_run,
     *        n_phys_trig_into_gate_accepted_in_run,
     *        m4_into_gate_in_run,
     *        n_k_pi_3_event, n_k_pi_4_event,
     *        n_k_pi_3_spill, n_k_pi_4_spill,
     *        n_k_pi_3_run,   n_k_pi_4_run,
     *        n_phys_trig_into_gate_tot,
     *        n_phys_trig_into_gate_accepted_tot,
     *        n_beam_tot_m4_into_gate, n_beam_tot_pi_plus,
     *        n_beam_tot_k
      COMMON /scalers_17/ hy_sca(18),
     *        events_in_run, n_beam_in_run, n_pi_plus_in_run,
     *        n_phys_trig_into_gate_in_run,
     *        n_phys_trig_into_gate_accepted_in_run,
     *        m4_into_gate_in_run,
     *        n_k_pi_3_event, n_k_pi_4_event,
     *        n_k_pi_3_spill, n_k_pi_4_spill,
     *        n_k_pi_3_run,   n_k_pi_4_run,
     *        n_phys_trig_into_gate_tot,
     *        n_phys_trig_into_gate_accepted_tot,
     *        n_beam_tot_m4_into_gate, n_beam_tot_pi_plus,
     *        n_beam_tot_k

C    Hyperon logbook 2008 14.11.2009
C    Счета в run
C    ===========
C    0 - N spill
C    1 - Number of triggers in run
C    2 - Number of pi
C    3 - Number of K
C    4 ---------------------------
C    5 ---------------------------
C    6 ---------------------------
C    7 ---------------------------
C    Счет между принятыми тригерами
C    ==============================
C    8 - Beam (M4 ^ Phys ворота)
C    9 -      (N phys. trig. ^ Phys ворота)
C    10 - M4 (Pi+)
C    11 - N Phys. trig. accepted
C    Счет в Spill
C    ============
C    12 - M4 ^ Phys ворота
C    13 - Phys. trig. ^ Phys ворота
C    14 - Clock 10 MHz
C    15 - Number of trigger accepted in spill
      character * 53 S_NAME(17) 
      data S_NAME/
     *     "number of scalers","Spill number",
     *     "Number of any type Triggers in run",
     *     "Number of pi",
     *     "Number of K ",
     *   4*"Unused",
     *     "Beam particles after previous trigger",
     *     "Physical triggers into gate after previous trigger",
     *     "M4(Pi+) after previous trigger",
     *     "Phys. trig. into gate accepted after previous trigger",
     *     "M4 into gate in spill",
     *     "Physical triggers into gate in spill",
     *     "event MISS clock in spill",
     *     "Event number in spill"/
      write(*,1000) (iw, S_NAME(iw), hy_sca(iw),iw=1,hy_sca(1)+1)
 1000 FORMAT(1x,I2,".->",A,"=",I10/)
      write(*,1001)    n_k_pi_3_event
 1001 FORMAT(1x," 3.-> n_k_pi_3_event =",I10)
      write(*,1002)    n_k_pi_4_event
 1002 FORMAT(1x," 4.-> n_k_pi_4_event =",I10)
      write(*,1003)    100*n_k_pi_3_event/hy_sca(9)
 1003 FORMAT(1x," Ratio(n_k_pi_3_event/beam) =",F10.3,"%")
      write(*,1004)    100*n_k_pi_4_event/hy_sca(9)
 1004 FORMAT(1x," Ratio(n_k_pi_4_event/beam) =",F10.3,"%")

      RETURN
      END
C**************************************
      subroutine hyp_sca_sp_prnt()
C**************************************
C12345678901234567890123456789012345678901234567890123456789012345678901
      integer*8 hy_sca,
     *        events_in_run, n_beam_in_run, n_pi_plus_in_run,
     *        n_phys_trig_into_gate_in_run,
     *        n_phys_trig_into_gate_accepted_in_run,
     *        m4_into_gate_in_run,
     *        n_k_pi_3_event, n_k_pi_4_event,
     *        n_k_pi_3_spill, n_k_pi_4_spill,
     *        n_k_pi_3_run,   n_k_pi_4_run,
     *        n_phys_trig_into_gate_tot,
     *        n_phys_trig_into_gate_accepted_tot,
     *        n_beam_tot_m4_into_gate, n_beam_tot_pi_plus,
     *        n_beam_tot_k
      COMMON /scalers_17/ hy_sca(18),
     *        events_in_run, n_beam_in_run, n_pi_plus_in_run,
     *        n_phys_trig_into_gate_in_run,
     *        n_phys_trig_into_gate_accepted_in_run,
     *        m4_into_gate_in_run,
     *        n_k_pi_3_event, n_k_pi_4_event,
     *        n_k_pi_3_spill, n_k_pi_4_spill,
     *        n_k_pi_3_run,   n_k_pi_4_run,
     *        n_phys_trig_into_gate_tot,
     *        n_phys_trig_into_gate_accepted_tot,
     *        n_beam_tot_m4_into_gate, n_beam_tot_pi_plus,
     *        n_beam_tot_k

      WRITE(*,*) "=================== hyp_sca_sp_prnt ============"
      WRITE(*,*) "================================================"
      WRITE(*,*) " *** Printout of scaler for spill =",hy_sca(2)
      WRITE(*,1000)  events_in_run
 1000 Format(1x,"events_in_run                             =",I10)
      WRITE(*,1001)  n_beam_in_run
 1001 Format(1x,"n_beam_in_run (9)                         =",I10)
      WRITE(*,1002)  n_pi_plus_in_run
 1002 Format(1x,"n_pi_plus_in_run (11)                     =",I10)
      WRITE(*,1003)  n_phys_trig_into_gate_in_run
 1003 Format(1x,"n_phys_trig_into_gate_in_run (10)         =",I10)
      WRITE(*,1004)  n_phys_trig_into_gate_accepted_in_run
 1004 Format(1x,"n_phys_trig_into_gate_accepted_in_run (12)=",I10)
      WRITE(*,1005)   m4_into_gate_in_run
 1005 Format(1x,"m4_into_gate_in_run (13)                  =",I10)
      WRITE(*,*) "================================================"
      WRITE(*,1006)
     * (100.*n_pi_plus_in_run/m4_into_gate_in_run) 
 1006 Format(1x,"(N_Pi+/m4_into_gate)_in_run  = ",F8.3,"%")
      WRITE(*,1007)
     * (100.*events_in_run/m4_into_gate_in_run) 
 1007 Format(1x,"(events/m4_into_gate)_in_run = ",F8.3,"%")
      WRITE(*,*) "================================================"
      RETURN
      END
