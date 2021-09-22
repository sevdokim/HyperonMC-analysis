 // Dec 5, 2008 - connect fortran and C++
#ifndef ROOT_GammasCB
#define ROOT_GammasCB

#ifndef __CFORTRAN_LOADED
#include "cfortran.h"
#endif

extern "C" {
//      COMMON /NREC  /  nevent_read // Jan 29,2009
  struct Nrec {
    int nevent_read; // number precessed event
  };
#define NREC COMMON_BLOCK(NREC,nrec)
  COMMON_BLOCK_DEF(Nrec,NREC);

// Calibration coefficients staff - look to calibr_init_2004.f 
// COMMON /CLB2  / CLB2(576),CLBS(64)
// COMMON /CLB2O / CLB2O(576),CLBSO(64) 
    struct CalibCoefs {
      float clbb[576]; // big   cell
      float clbs[64];  // small cell
    };

#define CLB2 COMMON_BLOCK(CLB2,clb2)    // old(input) cc 
  COMMON_BLOCK_DEF(CalibCoefs,CLB2);
#define CLB2O COMMON_BLOCK(CLB2O,clb2o) // new(output) cc 
  COMMON_BLOCK_DEF(CalibCoefs,CLB2O);

  // ======================================

    struct GammasCommon {
      int     ngam;
      float   xsh[30];
      float   ysh[30];
      float   esh[30];
      float   dcos[30];
      int     iclu[30]; // index of cluster: start from one: May 26,09;
    };

#define GAMMAS COMMON_BLOCK(GAMMAS,gammas)
  COMMON_BLOCK_DEF(GammasCommon,GAMMAS);

  // What we should do in case of sharing cluster in big and small cells?
  struct  Shower { // look to calibr2, filling2.f and COMMON /CLLIM / nclus,IXD(30),IXU(30),IYD(30),IYU(30
    int   nclust;  // sum number of clusters
    int   nclws;   // number of cluster in smnall cells
    int   ncellc[30];  // how much cell in shower ?
    float xshw[30][3]; // (3,30) -> [30][3]
    float yshw[30][3];
    float eshw[30][3]; // ??
    int   iamp_clus[30];
    float dum[30];
  };
#define SHOWR COMMON_BLOCK(SHOWR,showr)
  COMMON_BLOCK_DEF(Shower,SHOWR);
  // ======================================
  // Q: fortran <-> C++ swap indexes
  // Look to the Pythia:
  /* COMMON/PYJETS/N,NPAD,K(4000,5),P(4000,5),V(4000,5)
     struct Pyjets_t {
     int    N;
     int    NPAD;
     int    K[5][4000];
     double P[5][4000];
     double V[5][4000];
     };
   */
  //  ione(col,row) => Apr 28,2009
  //  =============
  // First fortran index - first(row or x):    second (column or y)
  // First C++ index     - first(column or y): second (row or x)
  // Transition from one index to two index
  // wall2_proc.f:   COMMON /AMPW2 / ncellb,ncells,IWL2(24,24),IWLS(8,8)
  // calibr2.f:      COMMON /AMPW2 / ncellb,ncells,IWL2(576),IWLS(64)
  /* Look to the program t.f
  ione(ix,iy) = 24*(ix-1) + iy
 common/one/ione(576) -> ione(24,24) : ione(ix,iy)
  ione(column, row)
 id = column + 24*row: row=id/24; column=id%24: (c ctyle)
 First grow second index at two dimensional array
    --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
24| 553 554 555 556 557 558 559 560 561 562 563 564 565 566 567 568 569 570 571 572 573 574 575 576
23| 529 530 531 532 533 534 535 536 537 538 539 540 541 542 543 544 545 546 547 548 549 550 551 552
22| 505 506 507 508 509 510 511 512 513 514 515 516 517 518 519 520 521 522 523 524 525 526 527 528
21| 481 482 483 484 485 486 487 488 489 490 491 492 493 494 495 496 497 498 499 500 501 502 503 504
20| 457 458 459 460 461 462 463 464 465 466 467 468 469 470 471 472 473 474 475 476 477 478 479 480
19| 433 434 435 436 437 438 439 440 441 442 443 444 445 446 447 448 449 450 451 452 453 454 455 456
18| 409 410 411 412 413 414 415 416 417 418 419 420 421 422 423 424 425 426 427 428 429 430 431 432
17| 385 386 387 388 389 390 391 392 393 394 395 396 397 398 399 400 401 402 403 404 405 406 407 408
16| 361 362 363 364 365 366 367 368 369 370 371 372 373 374 375 376 377 378 379 380 381 382 383 384
15| 337 338 339 340 341 342 343 344 345 346 347 348 349 350 351 352 353 354 355 356 357 358 359 360
14| 313 314 315 316 317 318 319 320 321 322 323 324 325 326 327 328 329 330 331 332 333 334 335 336
13| 289 290 291 292 293 294 295 296 297 298 299 300 301 302 303 304 305 306 307 308 309 310 311 312
12| 265 266 267 268 269 270 271 272 273 274 275 276 277 278 279 280 281 282 283 284 285 286 287 288
11| 241 242 243 244 245 246 247 248 249 250 251 252 253 254 255 256 257 258 259 260 261 262 263 264
10| 217 218 219 220 221 222 223 224 225 226 227 228 229 230 231 232 233 234 235 236 237 238 239 240
 9| 193 194 195 196 197 198 199 200 201 202 203 204 205 206 207 208 209 210 211 212 213 214 215 216
 8| 169 170 171 172 173 174 175 176 177 178 179 180 181 182 183 184 185 186 187 188 189 190 191 192
 7| 145 146 147 148 149 150 151 152 153 154 155 156 157 158 159 160 161 162 163 164 165 166 167 168
 6| 121 122 123 124 125 126 127 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 144
 5|  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120
 4|  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96
 3|  49  50  51  52  53  54  55  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72
 2|  25  26  27  28  29  30  31  32  33  34  35  36  37  38  39  40  41  42  43  44  45  46  47  48
 1|   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18  19  20  21  22  23  24
    --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
      1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18  19  20  21  22  23  24
  */
  struct  Ampw2 {         // look to event_decode_2005.f
    int ncellb;           // number of big cells with signal
    int ncells;           // number of small cells with signal
    int iwl2[24][24];     // iwl2_big(24,24) -> (24-row, 24-col)  
    int iwls[8][8];       // iwl2_small(8,8)
  };

#define AMPW2 COMMON_BLOCK(AMPW2,ampw2)
  COMMON_BLOCK_DEF(Ampw2,AMPW2);

  // Jan 11, 2009
  // event_decode_2005.f:      common /triggers/c1c2c3(3)
  struct  Triggers {
    float c1c2c3[3];
  };
#define TRIGGERS COMMON_BLOCK(TRIGGERS,triggers)
  COMMON_BLOCK_DEF(Triggers,TRIGGERS);
  
}
/*
C includeCommonMonitors.inc
C Common block for fortran  - Nov 10, 2008
      integer nrun   ! run number
      integer ng     ! number of gamma
      real*8 mgg     ! mass of (gamma,gamma)
      real*8 egg     ! energy of gg
      real*8 mpi, m4 ! Monitors between accepted triggers

      COMMON/MONITORS/ nrun, ng, egg, mgg, mpi, m4
*/
      struct Monitors {
        int nrun;
        int ng;
        double mgg;
        double egg;
        double mpi, m4;
      };

#define MONITORS COMMON_BLOCK(MONITORS,monitors)
   COMMON_BLOCK_DEF(Monitors,MONITORS);

  //
  // Geometry staff - Jan 23,2009
  //
  //  cell boundaries and half sizes 
  //      COMMON /W2GEOM/ XNET2(25),YNET2(25),HALFX2,HALFY2
// =================================================================
       struct WallGeomBig {
	 float xnet2[25];
	 float ynet2[25];
         float halfx2;
         float halfy2;
       };
#define W2GEOM COMMON_BLOCK(W2GEOM,w2geom)
   COMMON_BLOCK_DEF(WallGeomBig,W2GEOM);

// =================================================================
  //      COMMON /WSGEOM/ XNETS(9),YNETS(9),HALFXS,HALFYS
       struct WallGeomSmall {
	 float xnets[9];
	 float ynets[9];
         float halfx2;
         float halfy2;
       };
#define WSGEOM COMMON_BLOCK(WSGEOM,wsgeom)
   COMMON_BLOCK_DEF(WallGeomSmall,WSGEOM);
// =================================================================
//  cells number in X,Y and total, left and down LGD2 positions, big cell sizes
//     COMMON /W2SET / NXW2,NYW2,NCELW2,XW2L,YW2D,DXCW2,DYCW2
//  small cells number in X,Y and total, left and down positions, cell sizes
//     COMMON /WSSET / NXWS,NYWS,NCELWS,XWSL,YWSD,DXCWS,DYCWS

       struct WallSet {
         int   nxw, nyw, ncelw;
         float xwl, ywd, dxcw, dycw;
       };
#define W2SET COMMON_BLOCK(W2SET,w2set)
  COMMON_BLOCK_DEF(WallSet, W2SET);

#define WSSET COMMON_BLOCK(WSSET,wsset)
  COMMON_BLOCK_DEF(WallSet, WSSET);
// =================================================================

//COMMON /elim  /  etotl, amptotl
       struct Elim { // Sum summary cuts; just for sum case 
         float etotl;
         int   amptotl;
       };
#define ELIM COMMON_BLOCK(ELIM,elim)
  COMMON_BLOCK_DEF(Elim, ELIM);

// Bad channels - May 19, 09 
// bad big cells
// COMMON /BADCB /  NCBDB,ICBADB(20)
       struct Badcb {
         int ncbdb;
         int icbadb[20];
       };
#define BADCB COMMON_BLOCK(BADCB, badcb)
  COMMON_BLOCK_DEF(Badcb, BADCB);

#endif
