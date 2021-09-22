#include <iostream>
#include <cstdlib>
#include <iomanip>
#include "T_Clock.h"
using namespace std;
#include <cfortran.h>
#ifdef __linux__
#define f2cFortran
#endif
#include <hbook.h>
int scaler12,scaler13;
void print_scalers_for_event( int *);
void print_scalers_for_spill(int );
#ifdef __cplusplus
extern "C" {
#endif
  struct block_1 { long hy_sca[17],
      events_in_spill, 
      n_beam_in_spill, 
      n_phys_trig_into_gate_in_spill,
      n_phys_trig_into_gate_accepted_in_spill,
      n_k_event, n_pi_event,
      n_k_spill, n_pi_spill,
      n_phys_trig_into_gate_tot,
      n_phys_trig_into_gate_accepted_tot,
      n_beam_tot_into_gate, n_beam_tot_pi_plus,
      n_beam_tot_k; };
  struct block_1 scalers_16_;
  void hyp_sca_sp_prnt_();
#ifdef __cplusplus
}
#endif
//int events_in_run=0, n_beam_in_run=0, n_pi_plus_in_run=0,
//  n_phys_trig_into_gate_in_run=0,
//  n_phys_trig_into_gate_accepted_in_run=0,
//  m4_into_gate_in_run=0;
/*==================================================*/
void hyperon_scalers( int * pdata, int spill_old_miss)
/*==================================================*/
/*
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
*/
{  int * p_sc  = pdata;
  
  T_CLOCK.ICLOCK=*(p_sc+14);// event number in MC and clock in exp
  
  if (spill_old_miss != *(p_sc+1) && spill_old_miss > 0) {
    //    cout << " hyperon_scalers: End of spill = " << spill_old_miss 
    //	 << endl;
    //print_scalers_for_event( pdata);
    //print_scalers_for_spill(spill_old_miss);
    //    hyp_sca_sp_prnt_();
    HF1(-151, 
	(float) spill_old_miss,
        (float) scalers_16_.n_beam_in_spill);
    HF1(-153, 
	(float) spill_old_miss,
        (float) scalers_16_.events_in_spill); 
    if(scalers_16_.n_beam_in_spill!=0){
      HF1(-161, 
	  (float) spill_old_miss,
	  (100.*scalers_16_.n_pi_spill/scalers_16_.n_beam_in_spill));
      HF1(-162, 
	  (float) spill_old_miss,
	  (100.*scalers_16_.events_in_spill/scalers_16_.n_beam_in_spill));
      /*    cout << "100.*scalers_16_.n_k_pi_3_spill/scalers_16_.m4_into_gate_in_spill="
	    <<  100.*scalers_16_.n_k_pi_3_spill/scalers_16_.m4_into_gate_in_spill
	    << endl;
	    cout << "scalers_16_.n_k_pi_3_spill 253 ="
	    <<  scalers_16_.n_k_pi_3_spill
	    << endl;
	    cout << "scalers_16_.m4_into_gate_in_spill 253 ="
	    <<  scalers_16_.m4_into_gate_in_spill
	    << endl;
      */
      HF1(-253, 
	  (float) spill_old_miss,
	  (100.*scalers_16_.n_k_spill/scalers_16_.n_beam_in_spill));
      HF1(-254, 
	  (float) spill_old_miss,
	  (100.*scalers_16_.n_pi_spill/scalers_16_.n_beam_in_spill));
    
      HF1(-257,13.,float(*(p_sc+13)));
      HF1(-257,14.,float(*(p_sc+14)));
      HF1(-257,15.,float(*(p_sc+15)));
      HF1(-257,16.,float(*(p_sc+16)));
    }

    scalers_16_.events_in_spill                         = 0;
    scalers_16_.n_beam_in_spill                         = 0;
    scalers_16_.n_phys_trig_into_gate_in_spill          = 0;
    scalers_16_.n_phys_trig_into_gate_accepted_in_spill = 0;
    scalers_16_.n_k_spill                               = 0;
    scalers_16_.n_pi_spill                              = 0;
    //    cout << endl << "Press RETURN to continue" << endl;
    //    cin.get();
  }
  else {
    if (spill_old_miss < 0) {
       cout << "It is the first enter in this programm" << endl;
       cout << " Start spill value (-1)=" << spill_old_miss
	   << " New spill =" <<  *(p_sc+1) 
	   << endl;
      scalers_16_.events_in_spill                         = 0;
      scalers_16_.n_beam_in_spill                         = 0;
      scalers_16_.n_phys_trig_into_gate_in_spill          = 0;
      scalers_16_.n_phys_trig_into_gate_accepted_in_spill = 0;
      scalers_16_.n_k_spill                               = 0;
      scalers_16_.n_pi_spill                              = 0;
    }
  }
  scalers_16_.events_in_spill+=1;
  scalers_16_.n_beam_in_spill+=*(p_sc + 9);
  scalers_16_.n_pi_event=*(p_sc +11);
  scalers_16_.n_pi_spill+=*(p_sc +11);
  scalers_16_.n_k_event=(*(p_sc +10)-*(p_sc +11));
  scalers_16_.n_k_spill+=(*(p_sc +10)-*(p_sc +11));
  /******** trigger efficiencies **********************************/
  scalers_16_.n_phys_trig_into_gate_in_spill+=*(p_sc +12);
  scalers_16_.n_phys_trig_into_gate_accepted_in_spill+=1;
  /******** total scalers *****************************************/
  scalers_16_.n_phys_trig_into_gate_accepted_tot+=1;
  scalers_16_.n_phys_trig_into_gate_tot+=*(p_sc +12);
  scalers_16_.n_beam_tot_pi_plus+=*(p_sc +11);
  scalers_16_.n_beam_tot_k+=(*(p_sc +10)-*(p_sc +11));
  scalers_16_.n_beam_tot_into_gate+=*(p_sc + 9);
  //cout<<"spill_number"<<oct<<*(p_sc+1)<<endl;
  //cout<<"spill_number"<<hex<<*(p_sc+1)<<endl;
  //Evd-
  if(scalers_16_.n_pi_event!=0){
    HF1(-251, (float) scalers_16_.n_k_event/scalers_16_.n_pi_event, 1.);
  }
  if(*(p_sc+9) > 0){
    /*  if(scalers_16_.n_k_spill > 0){
    cout << "scalers_16_.n_k_spill 255 ="
	 <<  scalers_16_.n_k_spill
	 << endl;
        cout << endl << "Press RETURN to continue" << endl;
        cin.get();
  }
*/
    HF1(-255,     100*scalers_16_.n_k_event/(*(p_sc + 9)), 1.);
    HF1(-256,     100*scalers_16_.n_k_event/(*(p_sc + 9)), 1.);
  }
  
  HF1(-257, 9.,float(*(p_sc+ 9)));
  HF1(-257,10.,float(*(p_sc+10)));
  HF1(-257,11.,float(*(p_sc+11)));
  HF1(-257,12.,float(*(p_sc+12)));


  HF1(-1113,float(*(p_sc+13)),1.);
  HF1(-1114,float(*(p_sc+14)),1.);

  HF1(-1115,float(*(p_sc+15)),1.);
  HF1(-1116,float(*(p_sc+16)),1.);
  
  

  
  //print_scalers_for_event( pdata);
  /*  if (spill_old_miss == 10*(spill_old_miss/10)) {
      cout << " spill old value =" << spill_old_miss << endl;
      cout << setw(25) << "n_beam_in_spill -> (" << setw(2) << 9 << ")=" 
      << setw(10) << *(p_sc+ 9) << endl;
      cout << setw(25) <<  "n_beam_in_spill -> (" << setw(2) << 9 << ")=" 
      << setw(10) << *(p_sc+13) << endl;
      }*/
  
}
/*=======================================*/
void print_scalers_for_event( int * pdata)
/*=======================================*/
{
  int * p_sc  = pdata;

  int len=*(p_sc)+1;
  /* Hyperon logbook 2008 14.11.2009
     Счета в run
     ===========
     0 - N spill
     1 - Number of triggers in run
     2 - Number of pi
     3 - Number of K
     4 ---------------------------
     5 ---------------------------
     6 ---------------------------
     7 ---------------------------
     Счет между принятыми тригерами
     ==============================
     8 - Beam (M4 ^ Phys ворота)
     9 - (N phys. trig. ^ Phys ворота)
     10 - M4 (Pi+)
     11 - N Phys. trig. accepted
     Счет в Spill
     ============
     12 - M4 ^ Phys ворота
     13 - Phys. trig. ^ Phys ворота
     14 - Clock 10 MHz
     15 - Number of trigger accepted in spill
  */
  string S_NAME[17]={"number of scalers","Spill number",
		     "Number of any type Triggers in run",
                     "Number of pi","Number of  K","Unused",
                     "Unused","Unused","Unused",
                     "Beam particles after previous trigger",
                     "Physical triggers into gate after previous trigger",
		     "M4(Pi+) after previous trigger",
                     "Phys. trig. into gate accepted after previous trigger",
		     "M4 into gate in spill",
                     "Physical triggers into gate in spill",
		     "event MISS clock in spill",
                     "Event number in spill"};
  /************************************************************************/
  cout << "hyperon_scaler : array length of scalers = " << len << endl;
  for ( int i = 0; i < len; i++ ) {
    
    cout << setw(53) << S_NAME[i] 
         << " -> (" << setw(2) << i << ")=" 
         << setw(10) << *(p_sc+i) << endl;
    //    if (3*(i/3) == i )
    //      cout << endl;
  }
  cout << setw(53) << "scalers_16_.n_k_event " << "-> ( 3)="
         << setw(10) << scalers_16_.n_k_event << endl;
  cout << setw(53) << "scalers_16_.n_pi_event " << "-> ( 3)="
         << setw(10) << scalers_16_.n_pi_event << endl;
  cout << setw(53) << "scalers_16_.n_k_event/beam ="
       << fixed << setprecision(3) 
       << 100*scalers_16_.n_k_event/(*(p_sc+9)) << "%" << endl;
 cout << setw(53) << "scalers_16_.n_pi_event/beam ="
       << fixed << setprecision(3) 
       << 100*scalers_16_.n_pi_event/(*(p_sc+9)) << "%" << endl;
  // PAUSE after print of scalers 
  cout << endl << "Press RETURN to continue" << endl;
  cin.get();

  return;
}
/*====================================*/
void print_scalers_for_spill(int spill)
/*====================================*/ 
{
  cout << "================================================"   << endl; 
  cout <<"  *** Printout of scaler for spill =" << setw(10) << spill << endl;
  cout << setw(43) << "events_in_spill =" 
       << setw( 8) << scalers_16_.events_in_spill                << endl;
  cout << setw(43) << "n_beam_in_spill (9)=" 
       << setw( 8) << scalers_16_.n_beam_in_spill                << endl;
  cout << setw(43) << "n_pi_plus_in_spill (11)="
       << setw( 8) << scalers_16_.n_pi_spill             << endl;
  cout << setw(43) << "n_phys_trig_into_gate_in_spill (10)="
       << setw( 8) << scalers_16_.n_phys_trig_into_gate_in_spill          << endl;
  cout << setw(43) << "n_phys_trig_into_gate_accepted_in_spill (12)="
       << setw( 8) << scalers_16_.n_phys_trig_into_gate_accepted_in_spill << endl;
  cout << setw(43) << "n_beam_in_spill (13)="
       << setw( 8) <<  scalers_16_.n_beam_in_spill                  << endl;
  cout << "================================================" << endl; 
  cout <<setw(43)
       << "(N_Pi+/n_beam)_in_spill = "
       << fixed << setprecision(3) 
       << 100.*scalers_16_.n_pi_spill/scalers_16_.n_beam_in_spill 
       << "%" << endl;
  cout <<setw(43)
       << "(events/n_beam)_in_spill = "
       << fixed << setprecision(3) 
       << 100.*scalers_16_.events_in_spill/scalers_16_.n_beam_in_spill 
       << "%" << endl;
        cout <<setw(43)
       << "(n_k_spill/n_beam)_in_spill = "
       << fixed << setprecision(3) 
       << 100.*scalers_16_.n_k_spill/scalers_16_.n_beam_in_spill 
       << "%" << endl;
  cout <<setw(43)
       << "(n_pi_spill/n_beam)_in_spill = "
       << fixed << setprecision(3) 
       << 100.*scalers_16_.n_pi_spill/scalers_16_.n_beam_in_spill 
       << "%" << endl;
  cout << "================================================" << endl;
  cout << "M4 spill counter = "<<scaler12<<endl;
  cout << "Phys. trig. in gate spill counter"<<scaler13<<endl;
  
  hyp_sca_sp_prnt_();
  cout << endl << "Press RETURN to continue" << endl;
  cin.get();
}
//===========================================================================
