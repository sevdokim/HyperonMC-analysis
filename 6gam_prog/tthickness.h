//
// March 23, 11 by M.Stolpovsky
// C structure interact with fortran commom block tthickness
//
//#ifndef ROOT_ECommon
//#define ROOT_ECommon
#ifndef ROOT_TTHICKNESS
#define ROOT_TTHICKNESS

#ifndef __CFORTRAN_LOADED
#include "cfortran.h"
#endif

extern "C" {
// 	  common /TTHICKNESS/ THICKNESS
          typedef struct{
	    float   THICKNESS;
	  } Tthickness ;  

#define TTHICKNESS COMMON_BLOCK(TTHICKNESS,tthickness)    // old(input) cc 
        COMMON_BLOCK_DEF(Tthickness,TTHICKNESS);
};

#endif
