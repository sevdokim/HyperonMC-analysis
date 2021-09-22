//
// 6 Jul 2012 by S.Evdokimov
// C structure interact with fortran commom block T_Clock
//
#ifndef ROOT_T_CLOCK
#define ROOT_T_CLOCK

#ifndef __CFORTRAN_LOADED
#include "cfortran.h"
#endif

extern "C" {
//        common /T_CLOCK/ tclock,iclock,eta
          typedef struct{
            float   TCLOCK;
	    int     ICLOCK;
	    bool    ETA;
          } T_Clock ;  

#define T_CLOCK COMMON_BLOCK(T_CLOCK,t_clock)    // old(input) cc 
        COMMON_BLOCK_DEF(T_Clock,T_CLOCK);
};

#endif
