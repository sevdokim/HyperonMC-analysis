//
// May 28, 09 by PAI from version of SAS
// C structure interact with fortran commom block
//
//#ifndef ROOT_ECommon
//#define ROOT_ECommon
#ifndef ROOT_ZWALLS
#define ROOT_ZWALLS

#ifndef __CFORTRAN_LOADED
#include "cfortran.h"
#endif

extern "C" {
// 	  COMMON /ZWALLS/ ZWALL1,ZWALL 
          typedef struct{
	    float   ZWALL1;
	    float   ZWALL;
	    float   ENORM;
	  } Zwalls ;  

#define ZWALLS COMMON_BLOCK(ZWALLS,zwalls)    // old(input) cc 
        COMMON_BLOCK_DEF(Zwalls,ZWALLS);
};

#endif
