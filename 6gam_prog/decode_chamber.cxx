/* in  DIP************************************************************ */
#include "queue.h"
/* end DIP ***************************************************************** */
void decode3XY(unsigned short indata );
void decode1G(unsigned short indata );
void decode2G(unsigned short indata );
void tracks(unsigned short *, int);
int planeflag[8] = {0,0,0,0,0,0,0,0},i;
extern float centerp[8][96]; // centerp[j][k] is k-th cluster coordinate in 
                             //j-th plane
extern int widthp[8][96]; // widthp[j][k] is k-th cluster width in j-th plane
extern int skipt[3];     // flags for chamber selection for alignment purposes
extern float xshift[3]; // shift for x-origin
extern float yshift[3]; // shift for y-origin 
extern float zposit[3]; // z coordinates 
// almost constant
extern float wspace[3]; // wire spacing
extern float sigmax[3]; // space resolution
extern float sigmay[3];
extern int widthp[8][96]; // widthp[j][k] is k-th cluster width in j-th plane
extern int np[3]; // np[i] is number of combinatorial points in i-th chamber
extern float sum[2][100]; // sum[i][l] is test sum in i-th chamber for l-th combination of wires 
extern float x[3][100]; // x[i][l] is x in ith chamber of lth wires' combination
extern float y[3][100]; // y[i][l] is y in ith chamber of lth wires' combination
extern float z[3][100]; // z[i][l] is z in ith chamber of lth wires' combination
// track parametrization is x = ax + bx*z
//                          y = ay + by*z
extern float axt[20], bxt[20], ayt[20], byt[20]; // tracks parameters
extern float chi2xt[20], chi2yt[20];             // Chi-square/N.D.F
extern int nt;                                   // number of found tracks
extern float XYZ_LGD2[3];
int camac_spill_old = -1;
queue   MWPC_beam_queue;
queue *p_MWPC_beam_queue=&MWPC_beam_queue;
/*==========================================*/
void chamber_treatment( int * pdata, int len)
/*==========================================*/
{
  int a, n, c;
  unsigned short code, cdata;
  unsigned short * pev  = (unsigned short *) pdata;
  unsigned short * pev1  = pev;int len1=len;
  int camac_clock_number_bcd, camac_clock,camac_power, 
    camac_spill_number_bcd, camac_spill,
    camac_event_number_bcd, camac_event;
  for (int i=0; i<8; i++) planeflag[i] = 0;
  HF1(-1000, 9., 1.);
  /************************************************************************/
  camac_spill_number_bcd = *(pdata+9);
  camac_event_number_bcd = *(pdata+10);
  camac_clock_number_bcd = *(pdata+11);
  camac_spill = 0;
  camac_event = 0;
  camac_clock = 0;
  camac_power = 1;
  for ( int i=0; i < 8; i++ ) {
    camac_spill += 
      ((camac_spill_number_bcd) & 0x0000000F)*camac_power;
    camac_spill_number_bcd = (camac_spill_number_bcd>>4);
    camac_event += 
      ((camac_event_number_bcd) & 0x0000000F)*camac_power;
    camac_event_number_bcd = (camac_event_number_bcd>>4);
    camac_clock += 
      ((camac_clock_number_bcd) & 0x0000000F)*camac_power;
    camac_clock_number_bcd = (camac_clock_number_bcd>>4);
    camac_power *= 10;
  }
  HF1(-3000, (float) camac_clock, 1);
  /************************************************************************/
  code=0;len=len-12;pev +=24;
  while(len > 0)
    {if ( *pev != code ) {
	code=*pev; pev+=2; len--; continue;
      }// skip 1st word for group 
      code  = *pev++;
      cdata = *pev++;
      a =  code & 0xF;
      c = (code & 0xE00) >> 9;
      n = (code & 0x1F0) >> 4;
      if ( cdata ) {
	if(     c==1 && n==14) decode3XY(cdata); // 3XY
	else if(c==1 && n== 8) decode1G (cdata); // 1G
	else if(c==1 && n==11) decode2G (cdata); // 2G
	//      code=0;
	len--;
      }
    }
  //////////////////////////////////////////////////////////////////////////////
  tracks(pev1, len1);
  /////////////////////////////////////////////////////////////////////////////
  for ( int i = 0; i < 8; i++ ) {
    HF1(-(i+1)*100-3, planeflag[i], 1.);
    if ( i > 5 ) continue;
    for( int j = 0; j < planeflag[i]; j++ ) {
      HF1 ( -( i+1 )*100 - 1, centerp[i][j], 1. );
      HF1 ( -( i+1 )*100 - 2, widthp [i][j], 1. );
    }
  }           

  for ( int i = 0; i < 3; i++ ) {
    if ( np[i] != 1 ) continue;
    if( nt == 1 )
      for ( int j = 0; j < nt; j++ ) {
	for ( int L=0; L < np[i]; L++ ) {
	  HF1 ( -1801-2*i,   axt[j] + z[i][L]*bxt[j] -
		x[i][L]*wspace[i] - xshift[i], 1. );
	  HF1 ( -1802-2*i, ayt[j] + z[i][L]*byt[j] -
		y[i][L]*wspace[i] - yshift[i], 1. );
	}
      }
    for ( int j=0; j < np[i]; j++ ) {
      if ( i != 2 ) HF1 ( -1501 - i, sum[i][j], 1. );
      HF2(-1601-i, x[i][j], y[i][j], 1.);
    } 
  }

  if ( nt == 1 )  {
    // track parametrization is x = ax + bx*z
    //                          y = ay + by*z
    if ( camac_spill != camac_spill_old ) { 
      camac_spill_old = camac_spill;
      p_MWPC_beam_queue->Put_in_queue(camac_spill, 2, 10000 );
    }
    HF2 ( -1701, axt[0], ayt[0], 1. );
    HF2 ( -1702, bxt[0], byt[0], 1. );
    HF1 ( -1703, chi2xt[0],      1. );
    HF1 ( -1704, chi2yt[0],      1. );
    float X_LGD2 = axt[0] + bxt[0]*XYZ_LGD2[2];
    float Y_LGD2 = ayt[0] + byt[0]*XYZ_LGD2[2];
    HF2 ( -1850, X_LGD2, Y_LGD2, 1. );
    if ( chi2xt[0] < 7. && chi2yt[0] < 7. )
      HF2 ( -1851, X_LGD2, Y_LGD2, 1. );
    p_MWPC_beam_queue->Set_MWPC( camac_event, camac_clock, 
				 axt[0], bxt[0], ayt[0], byt[0] );
  }
  return;
}
/*==================================*/
void decode3XY(unsigned short indata)
/*==================================*/
{
  unsigned short center = indata & 0x00FF;
  unsigned short width  = (indata & 0xF000)>>12;
  int min = 0, max = 0, i = 0;

  //printf("  indata=%i center=%i width=%i \n",indata,center,width);
  if( width > 0 )
    {
      if ( center <  96)
	{
	  planeflag[6]++;
	  if (planeflag[6] == 1)
	    {
	      HF1(-1000, 6., 1.);
	    }
	  min = center - width/2;
	  max = min + (int)width;
	  //    printf("  min=%i max=%i  \n",min,max);
	  for (i=min; i<max; i++) HF1(-700, i, 1);
	  HF1(-701,(int)center,1);
	  HF1(-702,(int)width,1);
	}
      else if (center >= 128)
	{
	  center -= 128;
	  if( center <  96)
	    {
	      planeflag[7]++;
	      if (planeflag[7] == 1)
		{
		  HF1(-1000, 7., 1.);
		}
	      min = center - width/2;
	      max = min + (int)width;
	      for (i=min; i<max; i++) HF1(-800, i, 1);
	      HF1(-801,(int)center,1);
	      HF1(-802,(int)width,1);
	    }
	}
    }
}
void decode1G(unsigned short indata)
{
  unsigned short center = indata & 0x00FF;
  unsigned short width  = (indata & 0xF000)>>12;
  int min = 0, max = 0, i = 0;

  // printf(" 1G indata=%i center=%i width=%i \n",indata,center,width);
  if( width > 0 )
    {
      if(center <  96)      
	{
	  min = center - width/2;
	  max = min + (int)width;
	  for (i=min; i<max; i++) 
	    {
	      if( i >= 0 && i < 32)
		{
		  planeflag[2]++;
		  if (planeflag[2] == 1)
		    {
		      HF1(-1000, 2., 1.);
		    }
		  HF1(-300, 2*i+1, 1);
		} 
	      if( i >= 32 && i < 64)
		{
		  planeflag[0]++;
		  if (planeflag[0] == 1)
		    {
		      HF1(-1000, 0., 1.);
		    }
		  HF1(-100, 2*(i-32)+1, 1);
		}
	      if( i >= 64 && i < 96)
		{
		  planeflag[1]++;
		  if (planeflag[1] == 1)
		    {
		      HF1(-1000, 1., 1.);
		    }
		  HF1(-200, 2*(i-64)+1, 1);
		}
	    }
	}
      else if (center >= 128)
	{
	  center -= 128;
	  if(center <  96)
	    {  
	      min = center - width/2;
	      max = min + (int)width;
	      for (i=min; i<max; i++) 
		{
		  if( i >= 0 && i < 32)
		    {
		      planeflag[1]++;
		      if (planeflag[1] == 1)
			{
			  HF1(-1000, 1., 1.);
			}
		      HF1(-200, 2*i, 1);
		    } 
		  if( i >= 32 && i < 64)
		    {
		      planeflag[0]++;
		      if (planeflag[0] == 1)
			{
			  HF1(-1000, 0., 1.);
			}
		      HF1(-100, 2*(i-32), 1);
		    }
		  if( i >= 64 && i < 96)
		    {
		      planeflag[2]++;
		      if (planeflag[2] == 1)
			{
			  HF1(-1000, 2., 1.);
			}
		      HF1(-300, 2*(i-64), 1);
		    }
		}
	    }  
	} 
    }
}

void decode2G(unsigned short indata)
{
  unsigned short center = indata & 0x00FF;
  unsigned short width  = (indata & 0xF000)>>12;
  int min = 0, max = 0, i = 0;
 
  //printf("  indata=%i center=%i width=%i \n",indata,center,width);
  if( width > 0 )
    {
      if(center <  96)      
	{
	  min = center - width/2;
	  max = min + (int)width;
	  for (i=min; i<max; i++) 
	    {
	      if( i >= 0 && i < 32)
		{
		  planeflag[5]++;
		  if (planeflag[5] == 1)
		    {
		      HF1(-1000, 5., 1.);
		    }
		  HF1(-600,2*i+1, 1);
		} 
	      if( i >= 32 && i < 64)
		{
		  planeflag[4]++;
		  if (planeflag[4] == 1)
		    {
		      HF1(-1000, 4., 1.);
		    }
		  HF1(-500, 2*(i-32)+1, 1);
		}
	      if( i >= 64 && i < 96)
		{
		  planeflag[3]++;
		  if (planeflag[3] == 1)
		    {
		      HF1(-1000, 3., 1.);
		    }
		  HF1(-400, 2*(i-64)+1, 1);
		}
	    }
	}
      else if (center >= 128)
	{
	  center -= 128;
	  if(center <  96)
	    {  
	      min = center - width/2;
	      max = min + (int)width;
	      for (i=min; i<max; i++) 
		{
		  if( i >= 0 && i < 32)
		    {
		      planeflag[3]++;
		      if (planeflag[3] == 1)
			{
			  HF1(-1000, 3., 1.);
			}
		      HF1(-400, 2*i, 1);
		    } 
		  if( i >= 32 && i < 64)
		    {
		      planeflag[4]++;
		      if (planeflag[4] == 1)
			{
			  HF1(-1000, 4., 1.);
			}
		      HF1(-500, 2*(i-32), 1);
		    }
		  if( i >= 64 && i < 96)
		    {
		      planeflag[5]++;
		      if (planeflag[5] == 1)
			{
			  HF1(-1000, 5., 1.);
			}
		      HF1(-600, 2*(i-64), 1);
		    }
		}
	    }  
	} 
    }
}
//===========================================================================
