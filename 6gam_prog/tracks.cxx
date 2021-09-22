#include <math.h>
//double sqrt(double);
#include <iostream>
#include <cstdlib> /* used for EXIT_SUCCESS */
using namespace std;
void clusters( int );
void decode3XYr( unsigned short );
void decode2Gr( unsigned short );
void decode1Gr( unsigned short );

// MWPC chambers are 1G (1U, 1V, 1X), 2G (2U, 2V, 2Y), 3XY (3X, 3Y)
// chamber index is:  0                1                 2
// plane index   is:      0   1   2        3   4   5         6   7

// output arrays after track finding and fitting

extern int planeflag[8]; // number of clusters in j-th plane 
float centerp[8][96]; // centerp[j][k] is k-th cluster coordinate in j-th plane
int widthp[8][96]; // widthp[j][k] is k-th cluster width in j-th plane
int wirep[8][96]; // wirep[j][k] = 1 if k-th wire in j-th plane was hitted
int np[3]; // np[i] is number of combinatorial points in i-th chamber
float sum[3][100]; // sum[i][l] is test sum in i-th chamber for l-th combination of wires 
float x[3][100]; // x[i][l] is x-coordinate in i-th chamber for l-th combination of wires
float y[3][100]; // y[i][l] is x-coordinate in i-th chamber for l-th combination of wires
float z[3][100]; // z[i][l] is x-coordinate in i-th chamber for l-th combination of wires
float xp[3], yp[3], zp[3]; // points selected for fit
// track parametrization is x = ax + bx*z
//                          y = ay + by*z
float axt[20], bxt[20], ayt[20], byt[20]; // arrays of tracks parameters
float chi2xt[20], chi2yt[20]; // Chi-square/N.D.F
int nt;  // number of found tracks

// input parameters for track finding and fitting
int npmin = 2;    // minimal number of space points for track fitting (3 or 2)
#define FALSE      0
int take2plane = FALSE; // allow to use 2 planes only in 3-planes chambers
int widthmax = 5;    // maximal allowed cluster width (if great - skip cluster)
float chi2xmax = 100000000.; // maximal allowed chi-square/N.D.F.
float chi2ymax = 100000000.;
int skipt[3] = {0, 0, 0}; // flags for chamber selection for alignment purposes
float xshift[3] = {0., -32.5, 0.};       // shift for x-origin
float yshift[3] = {1.5, -24.0, 1.0};       // shift for y-origin 
//float zposit[3] = {2500., 4300., 7300.}; // z-coordinate 
float zposit[3] = {2778.68, 5148.68,7638.68}; // z-coordinate 
float XYZ_LGD2[3]={0.0, 0.0, 20550.0};
// local variables
int nwires[8] = {64, 64, 64, 64, 64, 64, 96, 96};
float wspace[3] = {2., 2., 2.}; // wire spacing
float sigmax[3]; // space resolution (calculated from wire spacing)
float sigmay[3];
static float u, v, x1, y1c;
static float sumn, sumz, sumz2, sumzx, sumzy, sumx, sumy, det;
static float ax, bx, ay, by, chi2x, chi2y;
static float cosa = 0.707107, sina = -0.707107;

void tracks(unsigned short *pev, int len)
{
  unsigned short code = 0, cdata;
  float delta;
  int a, n, c, npmin1;
  int ncl0, ncl1, ncl2, ncl3, ncl4, ncl5, npo0, npo1, npo2, skip[3];

  nt = 0;

  for ( int i = 0; i < 3; i++ ) {
    np[i] = 0;
    sigmax[i] = wspace[i]/sqrt((double)12.);
    sigmay[i] = wspace[i]/sqrt((double)12.);
  }
	
  for ( int i = 0; i < 8; i++ ) {
    planeflag[i] = 0;
    for ( int j = 0; j < 96; j++ ) {
      centerp[i][j] = 0.;
      widthp[i][j] = 0;
      wirep[i][j] = 0;
    }
  }
	
  while ( len > 0 )
    {
      if ( *pev != code ) { 
	code = *pev; pev += 2; len--; continue;
      }  // skip 1st word for group 
      code  = *pev++;
      cdata = *pev++;
      a =   code & 0xF;
      n = ( code & 0x1F0 ) >> 4;
      c = ( code & 0xE00 ) >> 9;
      if ( cdata ) {
	if      ( c == 1 && n == 14 ) decode3XYr(cdata); // 3XY
	else if ( c == 1 && n ==  8 ) decode1Gr(cdata);  // 1G
	else if ( c == 1 && n == 11 ) decode2Gr(cdata);  // 2G
      }
      len--;
    }
  for ( int i = 0; i < 6; i++ ) if ( planeflag[i] ) clusters(i);

//  for ( int i = 0; i < 8; i++ ) if ( planeflag[i] != 1) return;

  npmin1 = npmin;
  for ( int i = 0; i < 3; i++ ) { skip[i] = skipt[i]; if (skip[i]) npmin1 = 2; }

  if ( take2plane || ( npmin < 3 ) ) {
    for ( int i = 0; i < 3; i++ ) skip[i] = 0;
  }

  ncl0 = planeflag[0];
  if ( take2plane && ( ncl0 == 0 ) ) ncl0 = 1;
  for ( int i = 0; i < ncl0; i++ ){
    if ( planeflag[0] > 0 && widthp[0][i] > widthmax ) continue;
    ncl1 = planeflag[1];
    if ( take2plane && ( ncl1 == 0 ) ) ncl1 = 1;
    for ( int j = 0; j < ncl1; j++) {
      if ( planeflag[1] > 0 && widthp[1][i] > widthmax ) continue;
      ncl2 = planeflag[2];
      if ( take2plane && ( ncl2 == 0 ) ) ncl2 = 1;
      for (int k = 0; k < ncl2; k++ ) {
	if ( planeflag[2] > 0 && widthp[2][i] > widthmax ) continue;
	if ( planeflag[0]*planeflag[1]*planeflag[2] ) {
	  n = np[0];
	  if ( n >= 100 ) continue;
	  u = centerp[0][i] - (float)nwires[0]/2. - 0.5;
	  v = centerp[1][j] - (float)nwires[1]/2. - 0.5;
	  v = -v;
	  x1 = u*cosa - v*sina;
	  y1c = u*sina + v*cosa;
	  x[0][n] = centerp[2][k] - (float)nwires[2]/2. - 0.5;
	  y[0][n] = y1c;
	  sum[0][n] = x[0][n] - x1;
	  x[0][n] = (x[0][n] + x1)/2.;
	  z[0][n] = zposit[0];
	  np[0]++;
	}
	else if ( take2plane*planeflag[0]*planeflag[1] ) {
	  n = np[0];
	  if ( n >= 100 ) continue;
	  u = centerp[0][i] - (float)nwires[0]/2. - 0.5;
	  v = centerp[1][j] - (float)nwires[1]/2. - 0.5;
	  v = -v;
	  x1 = u*cosa - v*sina;
	  y1c = u*sina + v*cosa;
	  x[0][n] = x1;
	  y[0][n] = y1c;
	  sum[0][n] = 1000000.;
	  z[0][n] = zposit[0];
	  np[0]++;
	}
      }
    }
  }
  ncl3 = planeflag[3];
  if ( take2plane && ( ncl3 == 0 ) ) ncl3 = 1;
  for ( int i = 0; i < ncl3; i++ ) {
    if ( planeflag[3] > 0 && widthp[3][i] > widthmax ) continue;
    ncl4 = planeflag[4];
    if ( take2plane && ( ncl4 == 0 ) ) ncl4 = 1;
    for ( int j = 0; j < ncl4; j++) {
      if ( planeflag[4] > 0 && widthp[4][i] > widthmax ) continue;
      ncl5 = planeflag[5];
      if ( take2plane && ( ncl5 == 0 ) ) ncl5 = 1;
      for ( int k = 0; k < ncl5; k++ ) {
	if ( planeflag[5] > 0 && widthp[5][i] > widthmax ) continue;
	if ( planeflag[3]*planeflag[4]*planeflag[5] ) {
	  n = np[1];
	  if ( n >= 100 ) continue;
	  u = centerp[3][i] - (float)nwires[3]/2. - 0.5;
	  v = centerp[4][j] - (float)nwires[4]/2. - 0.5;
	  u = -u;
	  v = -v;
	  x1 = u*cosa - v*sina;
	  y1c = u*sina + v*cosa;
	  x[1][n] = x1;
	  y[1][n] = centerp[5][k] - (float)nwires[5]/2. - 0.5;
	  y[1][n] = -y[1][n];
	  sum[1][n] = y[1][n] - y1c;
	  y[1][n] = (y[1][n] + y1c)/2.;
	  z[1][n] = zposit[1];
	  np[1]++;
	}
	else if ( take2plane*planeflag[3]*planeflag[4] ) {
	  n = np[1];
	  if ( n >= 100 ) continue;
	  u = centerp[3][i] - (float)nwires[3]/2. - 0.5;
	  v = centerp[4][j] - (float)nwires[4]/2. - 0.5;
	  u = -u;
	  v = -v;
	  x1 = u*cosa - v*sina;
	  y1c = u*sina + v*cosa;
	  x[1][n] = x1;
	  y[1][n] = y1c;
	  y[1][n] = -y[1][n];
	  sum[1][n] = 1000000.;
	  z[1][n] = zposit[1];
	  np[1]++;
	}
      }
    }
  }
  for ( int i = 0; i < planeflag[6]; i++ ) {
    for ( int j = 0; j < planeflag[7]; j++ ) {
      n = np[2];
      if ( n >= 100 ) continue;
      x[2][n] = centerp[6][i] - (float)nwires[6]/2. - 0.5;
      y[2][n] = centerp[7][j] - (float)nwires[7]/2. - 0.5;
      z[2][n] = zposit[2];
      np[2]++;
    }
  }
  //track fit
  npo0 = np[0];
  if ( npmin < 3  && npo0 == 0 ) npo0 = 1;
  for ( int i = 0; i < npo0; i++ ) {
    if ( nt >= 20 ) continue;
    if ( np[0] ) {
      xp[0] = x[0][i]*wspace[0] + xshift[0];
      yp[0] = y[0][i]*wspace[0] + yshift[0];
      zp[0] = z[0][i];
    }
    npo1 = np[1];
    if ( npmin < 3  && npo1 == 0 ) npo1 = 1;
    for ( int j = 0; j < npo1; j++ ) {
      if ( nt >= 20 ) continue;
      if ( np[1] ) {
	xp[1] = x[1][j]*wspace[1] + xshift[1];
	yp[1] = y[1][j]*wspace[1] + yshift[1];
	zp[1] = z[1][j];
      }
      npo2 = np[2];
      if ( npmin < 3  && npo2 == 0 ) npo2 = 1;
      for ( int k = 0; k < npo2; k++ ) {
	if ( nt >= 20 ) continue;
	if ( np[2] ) {
	  xp[2] = x[2][k]*wspace[2] + xshift[2];
	  yp[2] = y[2][k]*wspace[2] + yshift[2];
	  zp[2] = z[2][k];
	}
	sumn = 0;
	sumz = 0;
	sumz2 = 0;
	sumzx = 0;
	sumzy = 0;
	sumx = 0;
	sumy = 0;
	for ( int n = 0; n < 3; n++ ) {
	  if ( !np[n] ) continue;
	  if ( skip[n] ) continue;
	  sumn++;
	  sumz  += zp[n];
	  sumz2 += zp[n]*zp[n];
	  sumzx += zp[n]*xp[n];
	  sumzy += zp[n]*yp[n];
	  sumx  += xp[n];
	  sumy  += yp[n];
	}
	if ( sumn < npmin1 ) continue;
	det = sumn*sumz2 - sumz*sumz;
	if ( det <= 0. ) continue;
	ax = (sumz2*sumx - sumz*sumzx)/det;
	bx = (sumn*sumzx - sumz*sumx)/det;
	ay = (sumz2*sumy - sumz*sumzy)/det;
	by = (sumn*sumzy - sumz*sumy)/det;
	chi2x = 0;
	chi2y = 0;
	if ( sumn > 2. ) {
	  for (int n = 0; n < 3; n++ ) {
	    if ( !np[n] ) continue;
	    if ( skip[n] ) continue;
	    delta  = xp[n] - ax - bx*zp[n];
	    chi2x += delta*delta/(sigmax[n]*sigmax[n]);
	    delta  = yp[n] - ay - by*zp[n];
	    chi2y += delta*delta/(sigmay[n]*sigmay[n]);
	  }
	  chi2x /= (sumn-2);
	  chi2y /= (sumn-2);
	  if ( chi2x > chi2xmax ) continue;
	  if ( chi2y > chi2ymax ) continue;
	}
	axt[nt] = ax;
	bxt[nt] = bx;
	ayt[nt] = ay;
	byt[nt] = by;
	chi2xt[nt] = chi2x;
	chi2yt[nt] = chi2y;
	nt++;
      }
    }
  }
}
void decode3XYr( unsigned short indata)
{
  unsigned short center = indata & 0x00FF;
  unsigned short width  = (indata & 0xF000)>>12;
  int min = 0, max = 0;
 
  if( width > 0 )
    {
      if ( center <  96 ) {
	centerp[6][planeflag[6]] = (float)(2*center - width/2 + width - 1)/2.;
	widthp[6][planeflag[6]] = width;
	planeflag[6]++;
	min = center - width/2;
	max = min + (int)width;
	for (int i =min; i<max; i++) wirep[6][i] = 1;
      }
      else if ( center >= 128 ) {
	center -= 128;
	if ( center <  96 ) {
	  centerp[7][planeflag[7]] = (float)(2*center - width/2 + width - 1)/2.;
	  widthp[7][planeflag[7]] = width;
	  planeflag[7]++;
	  min = center - width/2;
	  max = min + (int)width;
	  for (int i =min; i<max; i++) wirep[7][i] = 1;
	}
      }
    }
}

void decode1Gr( unsigned short indata)
{
  unsigned short center = indata & 0x00FF;
  unsigned short width  = (indata & 0xF000)>>12;
  int min = 0, max = 0;
 
  if ( width > 0 ) {
    if ( center <  96 ) {
      min = center - width/2;
      max = min + (int)width;
      for ( int i = min; i < max; i++ ) {
	if ( i >= 0 && i < 32 ) {
	  planeflag[2]++;
	  wirep[2][2*i + 1] = 1;
	} 
	if ( i >= 32 && i < 64 ) {
	  planeflag[0]++;
	  wirep[0][2*(i - 32) + 1] = 1;
	}
	if ( i >= 64 && i < 96) {
	  planeflag[1]++;
	  wirep[1][2*(i - 64) + 1] = 1;
	}
      }
    }
    else if ( center >= 128 ) {
      center -= 128;
      if ( center <  96 ) {  
	min = center - width/2;
	max = min + (int)width;
	for (int i = min; i < max; i++ ) {
	  if ( i >= 0 && i < 32 ) {
	    planeflag[1]++;
	    wirep[1][2*i] = 1;
	  } 
	  if ( i >= 32 && i < 64 ) {
	    planeflag[0]++;
	    wirep[0][2*(i - 32)] = 1;
	  }
	  if ( i >= 64 && i < 96 ) {
	    planeflag[2]++;
	    wirep[2][2*(i - 64)] = 1;
	  }
	}
      }  
    } 
  }
}

void decode2Gr( unsigned short indata)
{
  unsigned short center = indata & 0x00FF;
  unsigned short width  = (indata & 0xF000)>>12;
  int min = 0, max = 0;
 
  if ( width > 0 ) {
    if ( center <  96 ) {
      min = center - width/2;
      max = min + (int)width;
      for ( int i = min; i < max; i++ ) {
	if ( i >= 0 && i < 32 ) {
	  planeflag[5]++;
	  wirep[5][2*i + 1] = 1;
	} 
	if ( i >= 32 && i < 64 ) {
	  planeflag[4]++;
	  wirep[4][2*(i - 32) + 1] = 1;
	}
	if ( i >= 64 && i < 96 ) {
	  planeflag[3]++;
	  wirep[3][2*(i - 64) + 1] = 1;
	}
      }
    }
    else if ( center >= 128 ) {
      center -= 128;
      if ( center <  96 ) {  
	min = center - width/2;
	max = min + (int)width;
	for ( int i = min; i < max; i++ ) {
	  if ( i >= 0 && i < 32 ) {
	    planeflag[3]++;
	    wirep[3][2*i] = 1;
	  } 
	  if ( i >= 32 && i < 64 ) {
	    planeflag[4]++;
	    wirep[4][2*(i - 32)] = 1;
	  }
	  if ( i >= 64 && i < 96 ) {
	    planeflag[5]++;
	    wirep[5][2*(i - 64)] = 1;
	  }
	}
      }  
    } 
  }
}
void clusters(int pl)
{
  int w = 0, start = 0;
  planeflag[pl] = 0;
  for ( int i = 0; i < nwires[pl]; i++ ) {
    if ( wirep[pl][i] ) {
      if ( !w ) start = i;
      w++;
      if ( i == ( nwires[pl]-1 ) ) {
	widthp[pl][planeflag[pl]] = w;
	centerp[pl][planeflag[pl]] = (float)start + (float)(w-1)/2.;
	planeflag[pl]++;
	w = 0;
      }
    }
    else if ( w ) {
      widthp[pl][planeflag[pl]] = w;
      centerp[pl][planeflag[pl]] = (float)start + (float)(w-1)/2.;
      planeflag[pl]++;
      w = 0;
    }
  }
}
