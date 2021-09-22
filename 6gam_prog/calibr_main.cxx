/*DIP program 
  created       on 09 Mar 2011 by D.Patalakha
  last modified on 09 Jul 2012 by S.Evdokimov
*/
/* 
********************************************************************
Usage:
------
reconhyp             : read file-names from default file - file_list.dat
reconhyp -f file.dat : read file-names from user file    - file.dat 
reconhyp file.raw    : read data       from file         - file.raw 

********************************************************************
*/

//#include "system.h"
#include <zlib.h>
#include <stdio.h>
//int main( int, char *argv[] );
//int main( int, char *);

// DEFINE COMMON BLOCK ZWALLS
#include "string_text.h"
#include "zwalls.h" // C structure interact with fortran commom block
#include "tthickness.h" // C structure interact with fortran commom block tthickness
#include "T_Clock.h"

/* DIP in ****************************************************************** */
#include "queue.h"
/* DIP end ***************************************************************** */

/* ***************************************************************** */        



const int L160=121;   // max length of text string
int   Ihead[15],*pIhead=Ihead; // array Ihead containes run-parameter data

/************************************************************************/

//extern "C" vo readeventgz_(char *,int*,int *, int *, int *,  int *,int *);
extern "C" void readeventgz_(int *, int *, int *, bool* );
extern "C" void idofdetfun_(int* ,int *,int *,int *); // commented; kondr
// extern "C" void idofdetfun_(char *,int *,int *,int *); // commented; kondr
extern "C" void finishall_(void);
//void reconhyp_dump_ibuf(int *,int , int );

/* DIP in ****************************************************************** */
extern float XYZ_LGD2[3];// z coordinates LGD2
extern int camac_spill_old;
bool ok;
void chamber_treatment( int * pdata, int len);
extern queue   MWPC_beam_queue;
extern queue *p_MWPC_beam_queue;
extern "C" 
#ifdef __cplusplus
extern "C" {
#endif
  struct block { float Ax1, Bx1, Ay1, By1; };
  struct block mwpc_track_;
  //Evd+
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
  //Evd-
  //  void track_scalers_prnt_();
  //  void mwpc_track_prnt_();
  //  void hypero_sca_prnt_();
  //  void hyp_sca_sp_prnt_();
#ifdef __cplusplus
}
#endif
void hyperon_scalers(int *, int);
/* DIP end ***************************************************************** */
/*****************************************************************************/

int main ( int argc, char *argv[] )
{
  //Evd+
  int time=0;//event time
  FILE* fTexp=fopen("TExp.dat","w");
  fprintf(fTexp,"/ Event ID    T_measured(GeV)\n");
  //Evd-
  const int nbuf_max=4000016,// max length of input buffer
    nEvbuf_max=30000,   // max length of output data_buffer
    ncounters=16,       // number of counters (MISS module LE69)
    Imidas_dim=4,       // size of midas header 
    debu=0; // debug print =0,1,2 	   

// char IDde_char[ ]={'I','D','d','e','\0'};
//    char *IDde_char={"IDde"}; // commented; kondr
   int *IDde_char = new int(1); // kondr, see idofdetfun function

  const int begin_event_word=037777777777 /*, end_spill_word =025252525252 */ ; 
	    
  static int ipri=1,ipri_1=1,ipri_2=0,ipri_3=0,ipri4=0,ipri44=0,   ipri_shivka=0,
    ipri_mid1=0, ipri2=0, ipri69=0, ipri69_1=0,  ipri69no=0, ipri71=0,ipri712=0,
    ipriMWPC=0,  ipri_empty=0,ipriID=0,ih_pri=0, ipri_rejb=0,ipri_AK74=0, 
    ipri_overb=0,ipri_overe=0,ipri_no_run_head=0;

  int Nrun_number=0,irun_type=0,nrec=0,evi=0,evi_loc=0,evit=0,iBL=0,ityp10=0,
    nbuf=0,overtailb_counter=0, overtaile_counter=0,L=0,L0_out=0,L_out=0,
    pattern_unit=0,open_file_flag=1,ierr=0,RunHead=0,RunHead_first=0,
    iform=-1,iform_prev=-2,idata_type=-1,idata_type_prev=-2,i71=0, i301=0,

    i,j,ii,jj,iDir=0,IGg=0,IDb=0,i69=0, L2e_outgen=0,
    event_for_print=0,event_for_printi=0,
    ipriznak_siedeno_poslednee_slovo=0,L2e_address_max=0,
                           
    icounters[ncounters+1]={0},  // buffer for counters of module LE69             
    Ibuf0[5]={0},                // buffer for midas header     
      *pout=NULL,*p=NULL,  // buffer pointers
  Evbuf[nEvbuf_max+Imidas_dim+10],*p2=Evbuf; // event output buffer
  p = new int[2000000];   // ;kondr -- make it static, don't create each cycle. 2Mb memory total.
  // to transform input data to output format
  int distance;     
  int thickness;
  int enorm_int;   
  

  char cc,cc1,cc2,cc3,*pIfn = NULL,		
  FileNames_default[L160]="file_list.dat",         // default data_list
  FileName[L160],FileName_gz[L160],                // current data file name
  DirName[L160],
                                          
  *pFileNames_default=FileNames_default,
  *pFileName=FileName,*pFileName_gz=FileName_gz,
  *pDirName=DirName;

  //FILE *frun;
  gzFile frun;
  //gzFile *frun;
  /* DIP in ************************************************************* */
  int spill_miss_old                             = -1;
  scalers_16_.n_phys_trig_into_gate_accepted_tot =  0;
  scalers_16_.n_phys_trig_into_gate_tot          =  0;
  scalers_16_.n_beam_tot_into_gate            =  0;
  scalers_16_.n_beam_tot_pi_plus                 =  0;
  scalers_16_.n_beam_tot_k                 =  0;
  /* DIP end ************************************************************ */

  ZWALLS.ZWALL1=0.;
  TTHICKNESS.THICKNESS=0.;
  printf("reconhyp: number of arguments in command string argc=%i\n",argc);
  if(argc>=4){printf("        error, too much params\n"); exit(1);}
            
  //--------------------------------------------------------------
  //+++++		
  printf(":::   Start Processing   :::\n");
  //	reconsinit_(); 
  //      Ihead[6]=0; Ihead[7]=0; Ihead[8]=0; Ihead[9]=0; // not used header part

  // begin_cicle_through_data_files:
  // --------------------------------       
  // argc=1 - read list of data_files from default file
  // argc=2 - read data from user file
  // argc=3 = read list of data_files from user file 	
  
 a10: 
  if(argc==1)                      // example: recons
  {
    pIfn=pFileNames_default;
    if(ipri)  printf("read list of data from default file: %s \n",pIfn);
  }
        
  if(argc==2)                     // example: recons Run_76.dat.gz
  {
    pIfn=pFileName;
    if(ipri)
    {
      ii=ucopy_string(argv[1],pFileName,0,L160);
      if(ii<=0) { fprintf(stderr,"command error\n"); exit(1); }
      goto a20; 
    }
    else
      goto ErrExit;
  } 
      // examle: recons -f list_of_data.dat
  if(argc==3)
  {
    if(ipri)
    {
      pIfn=argv[2];
      printf("read list of data from user file: %s \n",pIfn);
    }
    pIfn=argv[2];
  } 
 
  ipri=0;
  ii=ucopy_string_from_file(pIfn,pFileName,L160); // read name of data
  if(ii<=0)goto ErrExit; // all data_file has already processed

  cc=*(pFileName);	
  //	if(cc=='#')goto a10;               //  comments
  irun_type=0; ipri_2=0;
  if(cc==' ')
  {
    irun_type=deletblank_string(pFileName,L160);
    irun_type=1;
    ipri_2=1;
  } // set run_type=PED by force  

  pIfn=pFileName;               // initially PIfn = pFileName

  //Sdv+	
  cc1=get_symbol_from_string(pIfn,0);
  cc2=get_symbol_from_string(pIfn,1);
  cc3=get_symbol_from_string(pIfn,2);
  
  if(cc1=='/'&&cc2==' '&&cc3=='D')
  {
    distance=integer_from_string_after_symbol(pIfn,':');
    thickness=integer_from_string_after_symbol(pIfn,';');//read target thickness
    enorm_int=integer_from_string_after_symbol(pIfn,'.');
    printf("Distance Z: %i \n",distance);
    printf("Thickness of target: %i \n",thickness);
    if(distance==enorm_int) 
    {
      ZWALLS.ENORM = 1.0000;
    }
    else  
    {
      printf("Energy normal. constant:  %i \n",enorm_int);
      ZWALLS.ENORM = float(enorm_int)/10000.;
    }  
    printf("Energy normalisation: %f   \n",ZWALLS.ENORM);
    printf("=============================================== \n");  
    printf("                                                \n");
    ZWALLS.ZWALL1=float(distance);
    TTHICKNESS.THICKNESS=float(thickness);
    goto a10;
  }
  //Sdv-
    
  ii=length_of_string(pIfn,'#');// delet comments
  if(ii<=0)goto a10;
  
  cc=get_symbol_from_string(pIfn,-2); // check if is it dir name
  if(cc=='/')
  {
    iDir=1;
    printf("%s\n",pIfn);
    ii=ucopy_string(pIfn,pDirName,0,L160);
    goto a10;
  }

  // form full name with including of full pass name
  if(iDir)
  {
    ucopy_string(pDirName,pFileName_gz,0,L160);
    ucopy_string(pIfn,pFileName_gz,3,L160);
    ucopy_string(pFileName_gz,pIfn,0,L160);
  }

 a20: 
    ipri=0; evi=0; ierr=0; nrec=0; /*RunHead_first=0;*/ ipri_no_run_head=0; 
    //    ipri69=0; ipri69_1=0; ipri69no=0; ipri44=0;
    i69=0; overtailb_counter=0; overtaile_counter=0;
    printf("----------------------------\n");        
    printf("data_file: %s\n",pIfn);
    if(irun_type==1)
    {
      if(ipri_2)  printf("     *** ped run by force\n");
      else        printf("     *** ped run\n");
      ipri_2=0;
    }
    //      pIfn=pFileName_gz;
    ucopy_string(pIfn,pFileName_gz,0,L160);
    ii=delet_to_symbol(pFileName_gz,'/',0,1); // select base name
    Nrun_number=integer_from_string_after_symbol(pFileName_gz,'/');	
    printf("    Nrun_number(from file name)=%i\n",Nrun_number); 

    //   end of preparation
    // --------------------

    ii=length_of_string(pIfn,' '); // length of file_name

    // open data_file
    // --------------
    if(open_file_flag)
    {
      frun=gzopen(pIfn,"r");
      if(frun==NULL)
      {
        printf(" Can't open data file: %s\n",pIfn);
        goto RECONS_finish_run;
      }
      open_file_flag=0;
    }
    icounters[0]=0; // ochisti chislo  schetchikov

      // read header of recors  
 READ_RECORD: 
  ierr=0; pattern_unit=0; icounters[0]=0; i69=0; iBL=1;
  jj=4;   // length to read in bytes
  if(Imidas_dim>0)jj=Imidas_dim*4;
  jj = gzread(frun,Ibuf0,jj);   
  time=Ibuf0[2];
  if((debu>1)||(ipri_mid1<20))
  {
    ipri_mid1++;
    printf("\nreconhyp: Record=%i, length of input header(bytes)=%i\n",nrec+1,jj);
  }
  if(jj<=0)  // EOF
  {
    if(jj<0) ierr=1; 
    L=-2; 
    goto RECONS_finish_run;
  }

  if(Imidas_dim>0)
  {
    L=Ibuf0[3];     // following length in bytes s razdel. slovom
    nbuf=L/4+Imidas_dim;
  } // total length in words
  else 
  {
    nbuf=Ibuf0[0]; 
    L=(nbuf-1)*4; 
  }
  if(nbuf>nbuf_max)
  {
    printf("reconhyp: rec=%i - too long record, len=%i > nbuf_max=%i\n",
    nrec,nbuf,nbuf_max);
    printf(" increase parameter nbuf_max in reconhyp.cxx\n");
    exit(1);	
    // L=-2; goto RECONS_finish_run; // finish run
    // goto READ_RECORD;       // next event       
  }
  // read all data
  
  i=nbuf; if(i<=0) i=1;

//Sdv-
  for (int __i; __i < 2000000; __i++) p[__i] = 0;       // ;kondr - zero it
//for (int __i; __i < 1000000; __i++) p[__i] = 0;       // ;kondr - zero it
  // copy midas header into input and output buffers 
  for(i=0;i<=Imidas_dim; i++)
  {(*(p+i))=Ibuf0[i]; Evbuf[i]=Ibuf0[i];}

  i=Imidas_dim;  
  jj=0; 
  
  if(L>0)jj=gzread(frun,p+i,L); // read total record
  if(jj<=0)    // EOF 
  {if(jj<0)ierr=2; L=-2; goto RECONS_finish_run;} 
  if(debu>1)printf("    read main part of record, gzread=%i\n",jj);
  nrec++;      // counter of records
  
  // dump of record begining
  if(ipri_3<20)
  {
    int i2;
    ipri_3++; i2=nbuf; if(i2>40)i2=40; 
    printf("reconhyp: dump of record\n");
    // for(int i=0; i<=i2; i++)printf("  i=%i) %i\n",i,*(p+i));
  }

  if(debu>1||nrec<5) printf("total length of data nbuf=%i\n",nbuf);
  if(nbuf<(Imidas_dim+4))
  {
    if(ipri_empty<20)
    {
      ipri_empty++; printf("reconhyp: rec=%i - empty record, len=%i\n",nrec,nbuf); 
      goto READ_RECORD;
    }
  } // nbuf< Imidas_dim+4 
  
  RunHead=0; i=Ibuf0[0]&0177777; IDb=-1; i71=0; i301=0;
  ityp10=0;

  if ((Ibuf0[0] & 0xFFFF) == 0x8000) goto READ_RECORD; // Standart MIDAS begin of run; ASh
  if ((Ibuf0[0] & 0xFFFF) == 0x8001) goto READ_RECORD; // Standart MIDAS end of run; ASh
      
      // process of RUN_HEAD
  if(i==10)
  {
    RunHead=1; ipri71=0; ipriMWPC=0; evi=0; nrec=1;  // hatrun record
    if(!irun_type){i=(Ibuf0[0])>>16; 
    irun_type=4;
//                          if(i&01)irun_type=4;   // phys run
    if(i&02)irun_type=1; // ped run
    //     		            if(i&03)irun_type=2;   // pul run
    if(i&04)irun_type=3;}// led run
    printf("RunHeader detected, irun_type=%i\n",irun_type);
    RunHead_first=1; ityp10=1;
    if(Nrun_number!=(*(p+5))) // Nrun from file name != Nrun
    {
      printf(" error in coding of Nrun=%i\n",*(p+5));
      L=-2; 
      goto RECONS_after;
    } // finish run by force	  
  }

  else // process of data, i !=10
  {
    int L1b,L2b,ip1,ip10,ipri0;     // data record
    ip1=0; ip10=0;
    if(RunHead_first==0)
    {
      if(!ipri_no_run_head) 
      {
        printf("First run in file_list.dat doesn't have run_header record!!!\n");
        printf("Move down first run file in your file_list.dat\n");
        return -1;
      }
      ipri_no_run_head=1;
      goto READ_RECORD;

    }              
    evi++; evit++; // event counters (inside of current run and totally)
    iform=1;
    idata_type=0; // MISS or SUMMA ADC with old format
    if(iform>0){idata_type=i; if(idata_type==0)idata_type=1;}
    if(idata_type==32770) idata_type=1;//evd 
    ipri0=0;

    if(ip10==0)
    {
      ip10=1;
      if(ipri4<20){ipri4++; ipri0=1;}
      if((iform_prev!=iform)||(idata_type_prev!=idata_type))
      {iform_prev=iform; idata_type_prev=idata_type; ipri0=1;}

      if(ipri0>0)  
      {
        if(ipri4>=20){if(ipri44>2)ipri0=0;}
        if(ipri0>0)
        {
          if(ipri4>=20) ipri44++; printf("reconhyp: rec=%i, ev=%i, iform=%i, idata_type=%i, Ibuf0[0]&0177777=%i\n", nrec,evit,iform,idata_type,i);
          if(iform>1)           { printf("reconhyp: invalid format code=%i, ev=%i\n",iform,evi);     exit(1); }
          if(idata_type>3)      { printf("reconhyp: invalid data_type=%i, ev=%i\n",idata_type,evi);  exit(1); }
        }
      } // ipri0
    }  // ip10
  
    //  idata_type = 0 - ADC from MISS or SUMMA with old format of LE51 (2002) after event builder
    //		 1 - ADC from MISS with new format LE74 (2004), no event builder
    //		 2   - only data from MWPC (SUMMA) prepared, no event builder
    //		 3   - data from LGD2 (MISS) + MWPC (SUMMA) after event builder

    L2b=Imidas_dim+1;

    // ASh+ 2016 data format
    if(*(p+6) == 0x5353494d) // == MISS, the first bank in MIDAS event
    {
      nbuf = *(p+8)/4; 
      p += 9; 
    }
    // ASh-
    do
    {
      L1b=L2b; IGg=*(p+L1b+1); IDb=*(p+L1b+2); // group and detector identificators

      // prinuditelnoe vystavlenie priznaka LGD2
      if(idata_type==1||idata_type==3)
      { if(((IGg==0)||(IGg==17))&&(IDb==0)) {  IDb=71; *(p+L1b+2)=IDb; } }

      if(ipriID<10)  // print group and detector identificators
      {
        ipriID++; ip1++;
        if(ip1==1) { printf("reconhyp, rec=%i, ev=%i : IGgroup=%i, IDdet=%i\n",nrec,evi,IGg,IDb); }
        else       { printf("                          IGgroup=%i, IDdet=%i\n",IGg,IDb);          }
      } // iprID<10

//       cout << "YK1: idofdetfun\n" << *IDde_char << endl;; 
      idofdetfun_(IDde_char,&IGg,&IDb,&i); // set program detector identificators through input identificators 
      i=1;//evd

      // avariinyi dump of record
      if(i==(-3)) // not valid detector identificator
      {
        int i;
        printf("reconhyp: rec=%i, ev=%i, iform=%i, idata_type=%i\n", nrec,evit,iform,idata_type);
        printf("reconhyp: dump of record\n");
        for(i=0; i<=20; i++)  printf("  i=%i) %i\n      i=%i) %i\n",i,*(p+i),L1b+i,*(p+L1b+i)); exit(1);
      }
      if(i>0)            // for valid detector identificators
      {
        if(i==1)   i71=1;  // LGD2    
        if(i==301) i301=i; // MWPC
      }
      L2b=L1b+(*(p+L1b));
    } while(L2b<nbuf);  

    //   printf(" I fini control of ID\n"); // debug

    if(i71)
    {
      if(!ipri71)
      {
        ipri71=1; 
        printf("reconhyp: found LGD2 in input data stream, idata_type=%i\n",
        idata_type);
      }
      if((ipri712<4)&&(ipri4>=20))
      {
        printf("\n idata_type=%i\n",idata_type);
        printf(" control dump of input data: nbuf=%i\n",nbuf);
        ipri712++; 
        //    for(int i=0; i<=20; i++)printf("  i=%i) %i\n",i,*(p+i));
      }
    }

    if(i301)
    {
      if(!ipriMWPC)
      {
        ipriMWPC=1; printf("reconhyp: found MWPC in input data stream, idata_type=%i\n",
        idata_type);
      }
      chamber_treatment(p, nbuf);
    }
  } // i=10  
 
  if((i71==0)&&(RunHead==0))goto READ_RECORD; // no LGD2 in input stream
  // fill information extracted from file_name and from header of record
  // -------------------------------------------------------------------
  // definition of patter_unit
  pattern_unit=0; if(Imidas_dim>0)pattern_unit=Ibuf0[0];

  for(i=1;i<15;i++)Ihead[i]=0;
  Ihead[0]=Nrun_number; // (from file name)
  Ihead[1]=evi;         // event number (in current run)   
  Ihead[2]=pattern_unit; // PatternUnit
  //Ihead[3]=0;   // sampling    
  //Ihead[4]=0;   // presampling 
  Ihead[5]=irun_type; // (0-ALL, 1-PED, 2-PULL, 3-LED, 4-PHYS)
  //Ihead[6]=0;   // run from event builder  
  //Ihead[7]=0;   // not used
  //Ihead[8]=evi; // event identificator (event number from event builder)
  Ihead[9]=evit;// total event number 
  //Ihead[10]- CHANNELS
  //Ihead[11]- adcFreqMHz

  evi_loc=0;    
  if(RunHead||(idata_type==0)||(idata_type==3)) // this is hat of run or data after event builder 
  {
    L_out=nbuf-Imidas_dim-1;    // total length without midas header and without razdel. slova 
    goto RECONS;
  }

      // ***************** EVENT BUILDER *********************************

      // transform format of LE74 (2004) to standard old format LE51 (2002)  
      //-------------------------------------------------------------------

  {
    int lenb,lene,lene2,lene23, L1b,L2b, L1e, L2e, 
    L1e_out,L2e_out, ERmis,iEV,iBL_prev_max,evi_mis,time_69,
    event_from_main_block_found,AK74;
           
	// input data are in format LE74 from 2004 and later -------
	// convert format of LE74 to standard format of LE51
	// ---------------------------------------------------------
	if(debu>1)printf("HHHHHHHHHHH  EVENT BUILDER: rec=%i ev=%i HHHHHHHHHHHHHH\n",
			 nrec,evi); 
	L2b=Imidas_dim+1;  // initial address of data in input array
	iBL=0;             // counter of blocks
	event_from_main_block_found=0;
	  
	// scan through block containing data from different crates or detectors
	//      to find the first valid block (main block) 

      BLOCK_CICLE: if(event_from_main_block_found)goto READ_RECORD; // main block already was found 
	L1b=L2b; if((L1b+6)>=nbuf)goto READ_RECORD;
	i=iBL+1; L_out=0; 
	lenb=*(p+L1b); // length of block
	L2b+=lenb;     // address of next data block
	if(lenb<=6)goto BLOCK_CICLE; // too short block
	IGg=*(p+L1b+1); IDb=*(p+L1b+2);
// 	cout << "YK2: idofdetfun\n"; 
	idofdetfun_(IDde_char,&IGg,&IDb,&i);
	//	    printf(" IDdet=%i\n"); // debug 
	if(i!=1)goto BLOCK_CICLE; // select the data from LGD2
	ih_pri=0;	  
	if(iBL)ih_pri=debu;
	if((debu>=1)&&(iBL==0))ih_pri=debu;	  
	if(ih_pri>1)printf("----- BLOCK_CICLE: rec,BL=%i %i) L1b,lenb=%i %i\n",	  
			   nrec,i,L1b,lenb); 					  
	iBL=i; L=0;          	  
	if(L2b>nbuf)
	  {if(ipri_overb<20)
	      {   ipri_overb++;
		printf(" %i) rec,BL=%i %i, L1b,lenb=%i %i ** overblock ** nbuf=%i\n",
		       ++overtailb_counter,nrec,iBL,L1b,lenb,nbuf);}
	    L2b=nbuf; lenb=L2b-L1b;
	    if(lenb<=6)goto READ_RECORD;} 
		     	       	    	      
	if(debu>1)
	  printf("     rec,BL=%i %i) lenb=%i, IGgroup,IDdet,reject=%i %i %i\n",
		 nrec,iBL,lenb,*(p+L1b+1),*(p+L1b+2),*(p+L1b+3));	 
		       		              	                             
	if((*(p+L1b+3))!=0) // reject code was setting
	  {if(ipri_rejb<10)
	      {ipri_rejb++;
	        printf("reconhyp: rec,BL=%i %i) ** block with setting reject code **\n",
		       nrec,iBL);}
	    goto BLOCK_CICLE; // reject group of data
	  }              

	IDb=*(p+L1b+2);    // identificator of detector
                   	       
	//--------- transfer format LE-74 (2004) --> format LE-51 (2002-2004) ------------------  
	L2e=L1b+4;            // initial address of data structure of controller LE74
	// 4 data header words: length of group, ID group,ID detector, reject code
	ipri_1=1;             // flag to print of invalid start word of block      
	iEV=0; 
	iBL_prev_max=iBL;
	event_from_main_block_found=0;
  
      EVENT_cicle_inside_buffer_LE74: 
	i69=0;          
	L1e=L2e; // adres tekuchego sobytii vo vhodnyh dannyh LE74
	if((L1e+3)>=L2b)goto BLOCK_CICLE;
	L2e_out=Imidas_dim+1; // nachalo zapisi pereformatirovannyh dannyh
	iEV++;
	evi_mis=-1; time_69=-1;
	icounters[0]=0;      // chislo schetchikov
	L_out=0;             // polnaia dlina dannyh bez midas i bez razdel. slova
	L0_out=0;            // to je samoe dlia tekusjei gruppy dannyh
	L1e_out=Imidas_dim+1;// adres vyhodnyh dannyh (za shapkoi MIDAS)
	if(debu>1)printf(" >>>  BL, ev(local)=%i %i) EVENT_cicle: L1e=%i\n",iBL,iEV,L1e); 

	//-------------------------------------------------------------------------------------
      BEGIN_EVENT_WORD: i=*(p+L1e);	
	if(i!=begin_event_word)
	  {if(ipri_1){printf(" rec,BL,ev=%i %i %i) wrong begin_event_word=%#o, L1e=%i\n",nrec,iBL,evi,i,L1e); 
	      ipri_1=0;
	      printf("   w0-w1...-w4= %#o %#o %#o %#o %#o\n",
		     i,*(p+L1e+1),*(p+L1e+2),*(p+L1e+3),*(p+L1e+4));} 
		     
	    if((L1e+1)<L2b){L1e++; L2e=L1e; goto BEGIN_EVENT_WORD;}
	    goto BLOCK_CICLE;
	  } // i!=begin_event_word)
    
	//i=(Ibuf0[0])&04; 
	//if(i){if(ipri_r){printf("end_spill record detected\n"); ipri_r=0;} 
	//  //                     goto READ_RECORD;
	//                         goto BEGIN_EVENT_WORD;
	//}      

	// printf(" begin word found, L2e,L2e_out=%i %i\n",L2e,L2e_out); // debug
	ipri_1=1; evi_mis=-1; time_69=-1;
	ERmis=*(p+L1e+2); AK74=ERmis; i=ERmis;
	lene=ERmis&03777; lene2=lene; // dlina sobytia v strukture LE-74
	ERmis=ERmis&034000; ERmis=(ERmis>>11);  
	AK74=AK74&01740000; AK74=(AK74>>14);
	if((lene>3)&&(ERmis==0))evi_mis=i&0177777; // event number from LE74 in spill
  
	if(ipri_AK74<20)
	  {printf("%i) Auton.Contr. N0=%i, L1b,L2b=%i %i, L1e,lene=%i %i, Err=%i, rec,ev=%i %i\n",
		  ++ipri_AK74,AK74,L1b,L2b,L1e,lene,ERmis,nrec,evi);} 

	if((lene<3)||(ERmis!=0))printf(" rec,BL=%i %i), **rej. ev=%i**, N(AK74)=%i, ERmis=%i, lene=%i\n",
				       nrec,iBL,evi,AK74,ERmis,lene);
	if(lene<3)goto BLOCK_CICLE;  
	L2e=L1e+lene;             // address of next event in structire LE74
	if((lene==3)||(ERmis!=0))goto EVENT_cicle_inside_buffer_LE74;// reject event      
    
	if((L2e>nbuf_max)||(L2b>nbuf_max))
	  {printf("too long event in LE-74 structure, len=%i, rec,ev=%i %i\n",lene,nrec,evi); 
	    //    L=-2; delete[] p1;goto RECONS_finish_run;
	    //     goto BLOCK_CICLE;
	    exit(1);}
         
   
	// L2e_address_max=L2b;
	L2e_address_max=L2b+1; // v strukture zapisi dannyh est osibka - poslednee
	//                              slovo v poslednem sobytii ne zapisyvaetsia. Kak
	//                              pravilo eto poslednee slovo est priznak bloka LE69.
	if(L2e>L2e_address_max)
	  {overtaile_counter++;
	    if(ipri_overe<20)
	      {   ipri_overe++;
		printf(" event overtail: %i) rec,BL,ev=%i %i %i, L1b,L2b=%i %i, L1e,lene=%i %i\n",
		       overtaile_counter,nrec,iBL,evi, L1b,L2b, L1e,lene);} 
	    //    L=-2; delete[] p1;goto RECONS_finish_run;
	    //    goto BLOCK_CICLE;
	    //    exit(1);
	    L2e=L2b;
	    lene=L2e-L1e; 
	    //     if(lene<=6)goto BLOCK_CICLE;
	    goto BLOCK_CICLE; // nepolnoe sobytie brakuetsia 
	  } 
     
	ipriznak_siedeno_poslednee_slovo=0;
	if(L2e>L2b)ipriznak_siedeno_poslednee_slovo=1; // takoe sobytie eshe mojno obrabotat 
                
	// obrabotka chetchikov LE69
	//--------------------------
	// seek counter module LE69  
	{//static int i34=34,i16=(34-2)/2;
	  int i,i1,i2,i16,i34,L1,ipp;
	  i16=ncounters; // =16  
	  i34=i16*2+2;   // =34
	  icounters[0]=0;
  
	  if(lene<(i34+3))
	    {if(ipriznak_siedeno_poslednee_slovo)goto BLOCK_CICLE;// nepolnoe sobytie, brak 
	    }
	  else // lene>=i34+3
	    {int ic;
	      ic=ipriznak_siedeno_poslednee_slovo;
	      if(ic==0){if(((*(p+L2e-1))&0177777)==0xA822)ic=1;} // priznak LE69
	      if(ic) 
		{lene2=lene2-i34; // dlina dannyh ADC (za vychetom chetchikov)
		  icounters[0]=ncounters; 
		  ic=ipri69;
		  if(ipriznak_siedeno_poslednee_slovo)ic=ipri69_1;

		  // fill array of counter data
		  icounters[0]=i16;
		  L1=L2e-i34; 
		  for(i=1; i<=i16;i++)
		    {i1=*(p+L1); i2=*(p+L1+1); L1+=2;
		      icounters[i]=((i1&077777)|((i2&077777)<<15));} // pobitovoe slojenie
	 
		  ipp=0;
		  if(ipriznak_siedeno_poslednee_slovo){
		    for(i=3; i<=6;i++)if(icounters[i]!=0)ipp=1;
		  } // eto ne schetchiki
		  if(ipp)goto BLOCK_CICLE; // nepolnoe sobytie, break  
		  /* Evd-
		  here was scalers processing
		  */
		  // print counter data
		  ic=ipri69; if(ipriznak_siedeno_poslednee_slovo)ic=ipri69_1;
		  if(ic<20||debu)
		    {ic++;
		      printf("\nI found block LE-69, ev,len=%i %i, siedeno_slovo=%i\n",
			     evi,lene,ipriznak_siedeno_poslednee_slovo); 
		      printf("%i) dump of counters\n",ic);
		      //	for(i=1; i<=i16;i++)printf(" counter %i) %i\n",i,icounters[i]);
	
		      if(ipriznak_siedeno_poslednee_slovo){ipri69_1=ic;}
		      else                                {ipri69=ic;}
		    }// ic<20     
   	    
		  // fill group of counters LE69 in output stream
		  // shapka dannyh gruppy schetchikov
		  L1e_out=L2e_out;  
		  L2e_outgen=L2e_out;
		  if((L2e_out+i16+3)>nEvbuf_max)goto Evbuf_overflow; // overflow output buffer
		  L0_out=3; L_out+=3;   // polnaia dlina dannyh chetchikov
		  *(p2+L2e_out+1)=AK74;   // ID controllera/gruppy  
		  *(p2+L2e_out+2)=69;     // ID detectora schetchikov 
		  L2e_out+=3;
		  for(i=1; i<=i16;i++){*(p2+L2e_out)=icounters[i]; L2e_out++;}
		  L_out+=i16; L0_out+=i16;
		  *(p2+L1e_out)=L0_out; L0_out=0; // length of group   
		  i69=1; time_69=icounters[16];  // flag that module LE69 found
   
		} // ic=(*(p+L2e-1))&0177777)==0xA822 - priznak chetchikov LE69
	    } // else lene >=(i34+3)
	  //printf(" konec obrabotki scetchikov: L1e_out,L2e_out,L_out=%i %i %i\n", // debug
	  //         L1e_out,L2e_out,L_out); // debug
	}   // konec obrabotki chetchikov 
  
	// obrabotka ADC LE91
	//------------------------------
	// shapka dannyh gruppy ADC LE91
	L1e_out=L2e_out;
	L2e_outgen=L2e_out;
	lene23=lene2-3; //  -3 - eto 3 slujebnyh slova zagolovka LE-74
	ii=2*lene23+3;  // polnaia dlina sformirovannyh vyhodnyh dannyh dlia ADC LE91     
	if((L2e_out+ii)>nEvbuf_max)goto Evbuf_overflow; // overflow output buffer  
	L0_out=ii;      // dlina dannyh dannogo detektora (ADC LE91)
	L_out+=ii;      // polnaia dlina bez midas i bez radel. slova
     
	*(p2+L2e_out)=ii;
	*(p2+L2e_out+1)=AK74;    // ID controllera/gruppy  
	*(p2+L2e_out+2)=IDb;     // ID detectora         
	//     printf("I do block LE91: L2e,L1e_out=%i %i\n",L2e,L2e_out); // debug
        L2e_out+=3;
     
	// dump shapki LE74
        if((ipri2<10)||(debu>1))
	  {printf("\n shapka sobytiia LE-74: rec,BL,ev=%i %i %i\n",nrec,iBL,evi); 	  
	    for(i=L1e;i<(L1e+3);i++)
	      printf("%i) %#o %i\n",i,*(p+i),*(p+i)); 
	    printf("           **** dump of output data  ****\n");
	  }
	
	jj=0;
	for(i=0;i<lene23;i++)       
	  {int A,iA,L1e3i;
	    L1e3i=L1e+3+i;     
	    A=*(p+L1e3i); iA=A;
	    iA=(iA>>16); iA=iA&07777; A=A&0177777;			    
	    *(p2+L2e_out)=iA; *(p2+L2e_out+1)=A; 

	    // dump of prepared output data
	    if((ipri2<10)||(debu>1))
	      {if(jj<20)
		  {   jj++;	  
		    //          printf("%i) %#o -> L2e_out) %i %i %i\n",
		    //	         L1e3i,*(p+L1e3i),L2e_out,iA,A);
		  }}                  
	    L2e_out+=2;     
	  } // i  (end of processing of output data for the main found block)
	//printf(" konec obrabotki ADC: L1e_out,L2e_out=%i %i, lene23,Lout=%i %i\n", // debug
	//         L1e_out,L2e_out,lene23,L_out); // debug
          
	L0_out=0; // length of group   
	if(ipri2<10)ipri2++;
      
	event_from_main_block_found=1;
	if(L2b>=nbuf)goto EVI_LOC;   // no the 2nd data block (no need shivka)
	if(ipri_shivka<20){ipri_shivka++; printf(" more than one data block - shivka\n");}
       
	// ====================================================================
	//              SHIVKA
	// scan of 2ry blocks (after main block) containing data from different detectors
	// criteria "shivki" = "the same event number"
	// all operation practically the same as for scan of the main block 
	//     when the first valid block (main block) to be found
           
	{ int iBL0, L1b0,L2b0,
	    i,L,iEV,lenb,IGg,IDb,L1e,L2e,ipri_1,evi_mis2;
      
	  iBL0=iBL;          // save of current block bumber (main block)
          L1b0=L1b; L2b0=L2b;// save addresses of block	            

	  // start block scan after the main already found block
	BLOCK_CICLE2: i=iBL+1; L1b=L2b; lenb=0; 
          if((L1b+6)>=nbuf)goto BLOCK_CICLE2_after;
          lenb=*(p+L1b); // length of block
	  L2b+=lenb;     // address of next data block
	  if(lenb<=6)goto BLOCK_CICLE2; // finish scan of 2ry blocks
	  IGg=*(p+L1b+1); IDb=*(p+L1b+2);
// 	  cout << "YK3: idofdetfun\n"; 
	  idofdetfun_(IDde_char,&IGg,&IDb,&i); 
	  if(i!=1)goto BLOCK_CICLE2; // selection of LGD2
	  ih_pri=0;
	  L=0;
	  if(iBL)ih_pri=debu;
	  if((debu>=1)&&(iBL==0))ih_pri=debu;	  
	  if((ih_pri>1)&&(iBL>iBL_prev_max))
	    printf("----- BLOCK_CICLE2: rec,BL=%i %i) L1b,lenb=%i %i\n",
		   nrec,i,L1b,lenb); 
	  iBL=i;           	  
	  L2b=L1b+lenb;     // address of next data block

	  if(L2b>nbuf)
	    {
	      //		        if(iBL>iBL_prev_max)
	      //		        {overtailb_counter++;		     
	      //		         printf(" %i) rec,BL=%i %i, L1b,lenb=%i %i ** overblock ** nbuf=%i\n",
	      //	                          overtailb_counter,nrec,iBL,L1b,lenb,nbuf);}
	      L2b=nbuf; lenb=L2b-L1b;
	      if(lenb<=6)goto BLOCK_CICLE2_after;}	       	  
	      
          if((debu>1)&&(iBL>iBL_prev_max))
	    printf("     rec,BL=%i %i) lenb=%i, IGgroup,IDdet,reject=%i %i %i\n",
		   nrec,iBL,lenb,*(p+L1b+1),*(p+L1b+2),*(p+L1b+3));	 
		       		              	                             
	  if((*(p+L1b+3))!=0) // reject code was setting
	    {if((ipri_rejb<10)&&(iBL>iBL_prev_max))
		{ ipri_rejb++;
		  printf("reconhyp: rec,BL=%i %i) ** block with setting reject code **\n",
			 nrec,iBL);}
	      goto BLOCK_CICLE2; // reject group of data
	    }              

	  IDb=*(p+L1b+2);    // identificator of detector
                   	       
	  //--------- transfer format LE-74 (2004) --> format LE-51 (2002-2004) ------------------  
	  L2e=L1b+4;            // initial address of data structure of controller LE74
	  // 4 data header words: length of group, ID group,ID detector, reject code
	  ipri_1=1;             // flag to print of invalid start word of block      
	  iEV=0;               
  
	EVENT_cicle2_inside_buffer_LE74: 
	  //    i69=0;         
	  L1e=L2e; // adres tekuchego sobytii vo vhodnyh dannyh LE74
	  if((L1e+3)>=L2b)goto BLOCK_CICLE2;
	  iEV++;
	  evi_mis2=-2;
	  icounters[0]=0;   // chislo schetchikov
	  //     L_out=0;            // polnaia dlina dannyh bez midas 
	  L0_out=0;        // dlina tekushei gruppy dannyh (vmeste s shapkoi dannyh)
	  //    L1e_out=Imidas_dim+1;// adres vyhodnyh dannyh (za shapkoi MIDAS)
	  //    if(debu>1)printf(" >>>  BL,ev(in block)=%i %i) EVENT_cicle2: L1e=%i %i\n",iBL,iEV,L1e); 

	  //-------------------------------------------------------------------------------------
	BEGIN2_EVENT_WORD: i=*(p+L1e);	
	  if(i!=begin_event_word)
	    {if(ipri_1)
		{
		  if(iBL>iBL_prev_max)
		    {printf(" rec,BL,ev=%i %i %i) wrong begin_event_word=%#o, L1e=%i\n",
			    nrec,iBL,evi,i,L1e);         
		      printf("   w0-w1...-w4= %#o %#o %#o %#o %#o\n",
			     i,*(p+L1e+1),*(p+L1e+2),*(p+L1e+3),*(p+L1e+4));} 
		}
	      ipri_1=0;		     
	      if((L1e+1)<L2b){L1e++; L2e=L1e; goto BEGIN2_EVENT_WORD;}
	      goto BLOCK_CICLE2;
	    } // i!=begin_event_word
    
	  //i=(Ibuf0[0])&04; 
	  //if(i){if(ipri_r){printf("end_spill record detected\n"); ipri_r=0;} 
	  //  //                     goto READ_RECORD;
	  //                         goto BEGIN2_EVENT_WORD;
	  //}      

	  ipri_1=1; evi_mis2=-2;
	  ERmis=*(p+L1e+2); AK74=ERmis; i=ERmis;
	  lene=ERmis&03777; lene2=lene; // dlina sobytia v strukture LE-74
	  ERmis=ERmis&034000; ERmis=(ERmis>>11);  
	  AK74=AK74&01740000; AK74=(AK74>>14);
	  if((ERmis==0)&&(lene>3))
	    {evi_mis2=i&177777; // event number from LE74 (in spill)
	      if(evi_mis2>evi_mis)goto BLOCK_CICLE2;}    
 
	  i=1; if((iBL<=iBL_prev_max)&&((ERmis!=0)||(lene<=3)))i=0;
	  if(i)
	    {
	      if(ipri_AK74<20)
		printf("%i) Auton.Contr. N0=%i, L1b,L2b=%i %i, L1e,lene=%i %i, Err=%i, rec,ev=%i %i\n",
		       ++ipri_AK74,AK74,L1b,L2b,L1e,lene,ERmis,nrec,evi); 

	      if((lene<3)||(ERmis!=0))printf(" rec,BL=%i %i), **rej. ev=%i**, N(AK74)=%i, ERmis=%i, lene=%i\n",
					     nrec,iBL,evi,AK74,ERmis,lene);
	    } // i				  
	  if(lene<3)goto BLOCK_CICLE2;  

	  //L2E_2:
	  L2e=L1e+lene;             // address of next event in structire LE74
	  if((lene==3)||(ERmis!=0))goto EVENT_cicle2_inside_buffer_LE74;// reject event       

	  if(evi_mis2<=0)goto EVENT_cicle2_inside_buffer_LE74;
	  if(evi_mis2!=evi_mis)goto EVENT_cicle2_inside_buffer_LE74;
    
	  if((L2e>nbuf_max)||(L2b>nbuf_max))
	    {printf("too long event in LE-74 structure, len=%i, rec,ev=%i %i\n",lene,nrec,evi); 
	      //    L=-2; delete[] p1;goto RECONS_finish_run;
	      //     goto BLOCK_CICLE2;
	      exit(1);}
         
	  if(L2e>L2b)
	    {overtaile_counter++;
	      if(overtaile_counter<20)
		printf(" event overtail2: %i) rec,BL2,ev=%i %i %i, L1b,L2b=%i %i, L1e,lene=%i %i\n",
		       overtaile_counter,nrec,iBL,evi, L1b,L2b, L1e,lene); 

	      //    L=-2; delete[] p1;goto RECONS_finish_run;
	      //    goto BLOCK_CICLE2;
	      //    exit(1);
	      L2e=L2b;
	      lene=L2e-L1e; if(lene<=6)goto BLOCK_CICLE2;} 
                      
	  // obrabotka chetchikov LE69
	  //--------------------------
	  // seek counter module LE69  
	  {//static int i34=34,i16=(34-2)/2;
	    int i,i1,i2,i16,i34,L1,ipp;
	    i16=ncounters; // =16  
	    i34=i16*2+2;   // =34
	    icounters[0]=0;
  
	    if(lene<(i34+3))
	      {if(ipriznak_siedeno_poslednee_slovo)goto BLOCK_CICLE;// nepolnoe sobytie, brak 
	      }
	    else // lene>=i34+3
	      {int ic;
		ic=ipriznak_siedeno_poslednee_slovo;
		if(ic==0){if(((*(p+L2e-1))&0177777)==0xA822)ic=1;} // priznak LE69
		if(ic)
		  {lene2=lene2-i34; // dlina dannyh ADC (za vychetom chetchikov)
		    icounters[0]=ncounters; 
		    ic=ipri69;
		    if(ipriznak_siedeno_poslednee_slovo)ic=ipri69_1;     
     
		    // fill array of counter data
		    icounters[0]=i16;
		    L1=L2e-i34; 
		    for(i=1; i<=i16;i++)
		      {i1=*(p+L1); i2=*(p+L1+1); L1+=2;
			icounters[i]=((i1&077777)|((i2&077777)<<15));} // pobitovoe slojenie     

		    ipp=0;
		    if(ipriznak_siedeno_poslednee_slovo){for(i=3; i<=6;i++)if(icounters[i]!=0)ipp=1;} // eto ne schetchiki
		    if(ipp)goto BLOCK_CICLE2; // nepolnoe sobytie, brak  
     
		    // print counter data
		    ic=ipri69; if(ipriznak_siedeno_poslednee_slovo)ic=ipri69_1;
		    if(ic<20||debu)
		      {ic++;       
			printf("  I found block LE-69, ev,len=%i %i, siedeno_slovo=%i\n",
			       evi,lene,ipriznak_siedeno_poslednee_slovo);
			printf("\n        %i) dump of counters\n",ic); 
			for(i=1; i<=i16;i++)printf(" counter %i) %i\n",i,icounters[i]);
	
			if(ipriznak_siedeno_poslednee_slovo){ipri69_1=ic;}
			else                                {ipri69=ic;}
		      }// ic<20    
    
		    // fill group of counters LE69 in output stream
		    // shapka dannyh gruppy schetchikov
		    L1e_out=L2e_out;
		    L2e_outgen=L2e_out;  
		    if((L2e_out+i16+3)>nEvbuf_max)goto Evbuf_overflow; // overflow output buffer
		    L0_out=3; L_out+=3;   // polnaia dlina dannyh chetchikov
		    *(p2+L2e_out+1)=AK74;   // ID controllera/gruppy  
		    *(p2+L2e_out+2)=102;    // ID schetchikov 102 
		    L2e_out+=3;
		    for(i=1; i<=i16;i++){*(p2+L2e_out)=icounters[i]; L2e_out++;}
		    L_out+=i16; L0_out+=i16;
		    *(p2+L1e_out)=L0_out; L0_out=0; // length of group   
		    if(i69==0){i69=1; time_69=icounters[16];} 
		    // flag that module LE69 found
   
		  } // (*(p+L2e-1))&0177777)==0xA822 - priznak chetchikov LE69
	      } // lene >=(i34+3)
	  }   // konec obrabotki chetchikov 
  
	  // obrabotka ADC LE91
	  //------------------------------
	  // shapka dannyh gruppy ADC LE91
	  L1e_out=L2e_out;
	  L2e_outgen=L2e_out;
	  lene23=lene2-3; //  -3 - eto 3 slujebnyh slova zagolovka LE-74
	  ii=2*lene23+3;  // polnaia dlina sformirovannyh vyhodnyh dannyh dlia ADC LE91     
	  if((L2e_out+ii)>nEvbuf_max)goto Evbuf_overflow; // overflow output buffer  
	  L0_out=ii;      // dlina dannyh dannogo detektora (ADC LE91)
	  L_out+=ii;      // polnaia dlina bez midas i bez radel. slova
     
	  *(p2+L2e_out)=ii;
	  *(p2+L2e_out+1)=AK74;    // ID controllera/gruppy  
	  *(p2+L2e_out+2)=IDb;     // ID detectora         
	  L2e_out+=3;

	  // dump shapki LE74
	  if((ipri2<10)||(debu>1))
	    {printf("\n shapka sobytiia LE-74: rec,BL,ev=%i %i %i\n",nrec,iBL,evi); 	  
	      for(i=L1e;i<(L1e+3);i++)
		printf("%i) %#o %i\n",i,*(p+i),*(p+i)); 
	      printf("           **** dump of output data  ****\n");
	    }
	
	  jj=0;
	  for(i=0;i<lene23;i++)       
	    {int A,iA,L1e3i;
	      L1e3i=L1e+3+i;     
	      A=*(p+L1e3i); iA=A;
	      iA=(iA>>16); iA=iA&07777; A=A&0177777;			    
	      *(p2+L2e_out)=iA; *(p2+L2e_out+1)=A; 

	      // dump of prepared output data
	      if((ipri2<10)||(debu>1))
		{if(jj<20)
		    {   jj++;	  
		      //          printf("%i) %#o -> L2e_out) %i %i %i\n",
		      //	         L1e3i,*(p+L1e3i),L2e_out,iA,A);
		    }} 
                 
	      L2e_out+=2;     
	    } // i  (end of processing of output in current block data)
          
	  L0_out=0;    
	  if(ipri2<10)ipri2++;

	  goto BLOCK_CICLE2;  // next block dlia shivki
	BLOCK_CICLE2_after: 
	  if(iBL>iBL_prev_max)iBL_prev_max=iBL;
	  iBL=iBL0; // restore of current block number (main block)
	  L1b=L1b0; L2b=L2b0;  // restore addresses of main block	    

	} // =========== END of shivki sobytii ========================q

      EVI_LOC: evi_loc++;  
	//  L_out++; // total length without midas header + razdel. slovo
	if(evi_loc>1)
	  {evi++; evit++;
	    Ihead[1]=evi; 
	    Ihead[8]=evi;  
	    Ihead[9]=evit;}
      }    
      // ***********  END of EVENT BUILDER ************************     
   
      // vyzov programmy obrabotki sobytiia, L_out - dlina bez midas header i bez razdel slova
      //-----------------------------------
 RECONS:  
  L_out++;  // dobavili razdelitelnoe slovo v dlinu dannyh
  if(RunHead||(idata_type==0)||(idata_type==3))  pout=p; // output array = input array
  else                                           pout=p2;// output array = transformed format array
  *(pout+Imidas_dim)=L_out; // total length from this word (without midas header) 
  L=L_out;
  if(L>5)
	{
	  if(Imidas_dim>0)
    {
      i=Imidas_dim-1; *(pout+i)=4*L_out; // following length in bytes
      i=(Ibuf0[0]&037703777777);         // obnuli nomer formata zapisi dannyh
      j=(i&0177777); 
      if(j==0) i=(i|01);                 // tip dannyh 0 -> 1 (dannye LE91 + LE69) 
      *(pout)=i;
      pattern_unit=i;                    // correct pattern unit
      Ihead[2]=i;
    }
    // print number of processing events
    event_for_print++;
    {
      int i,ima;
      i=0; ima=20000; 
      if(event_for_print<=ima)
      {
        ima=1;
        if(event_for_print>10)    ima=10;
        if(event_for_print>100)   ima=100;
        if(event_for_print>500)   ima=500;
        if(event_for_print>2500)  ima=1000;
        if(event_for_print>5000)  ima=2000;
        if(event_for_print>10000) ima=10000;
        if(event_for_print>10000) ima=20000;
        if((event_for_print/ima*ima)==event_for_print) ima=0;
      }
      else
      {
        event_for_printi++; 
        if(event_for_printi==ima) { event_for_printi=0; ima=0; }
      }
      if(ima==0) 
      {
        cout << setw(51) << "********** reconhyp working ********* : event = " 
             << setw(10) << event_for_print << endl;
          /* DIP in ***************************************************** */
        cout << setw(51) << "n_phys_trig_into_gate  = "
             << setw(10) <<  scalers_16_.n_phys_trig_into_gate_tot << endl;
        cout << setw(51) << "n_beam_tot_into_gate = "
             << setw(10) << scalers_16_.n_beam_tot_into_gate << endl;
        if(scalers_16_.n_phys_trig_into_gate_tot > 0)
          cout << setw(51) << "n_phys_trig_into_gate/n_beam_tot_into_gate = "
               << fixed << setprecision(3) 
               << 100.*scalers_16_.n_phys_trig_into_gate_tot/scalers_16_.n_beam_tot_into_gate << "%" << endl;
          cout << setw(51) << "n_phys_trig_into_gate_accepted_tot = "
               << setw(10) <<  scalers_16_.n_phys_trig_into_gate_accepted_tot << endl;
          cout << setw(51) << "n_beam_tot_pi_plus                 = "
               << setw(10) << scalers_16_.n_beam_tot_pi_plus << endl;
        if(scalers_16_.n_beam_tot_pi_plus > 0)
          cout << setw(51) << "n_phys_trig_into_gate/n_beam_tot_pi_plus = "
               << fixed << setprecision(3) 
               << 100.*scalers_16_.n_phys_trig_into_gate_accepted_tot/scalers_16_.n_beam_tot_pi_plus<< "%" << endl;
      }
      /* DIP end ****************************************************** */
      //        readeventgz_(pIfn,&ii,Ibuf0,pIhead,icounters,&L_out,pout);
      *(pout+Imidas_dim)     = L_out;   // sleduiuishaia dlina vmeste s etim slovom
      *(pout+Imidas_dim-1)   = 4*L_out;
      L_out += Imidas_dim;              // total length with midas header + razdel. slovo	   
      {
        if(RunHead||(idata_type==0)||(idata_type==3))i69=1;
        if(!i69)
        {
          if(ipri69no<5)
          {
            ipri69no++;
            printf("reconhyp: rec,BL,ev=%i %i %i) ** no LE69 **\n",nrec,iBL,evi);
          }
        }
      }

      readeventgz_(pIhead,pout,&L_out,&ok); L=L_out;
      // ok == false: event rejection due to some reason (example: pile-up discriminator > 10 ADC counts)
      //  if(ok) 
      { // Scalers processing
        /* DIP in Scalers ***************************************** */
        hyperon_scalers(icounters, spill_miss_old);
        //Evd+
        //Fill Texp.dat 
        if(T_CLOCK.ETA)
        {
          fprintf(fTexp,"%d\t%f\n",time,T_CLOCK.TCLOCK);
          T_CLOCK.ETA=false;
        }
        for (int i_sc=0; i_sc <= icounters[0];i_sc++)
        {
          scalers_16_.hy_sca[i_sc]=icounters[i_sc];
          /*  //cout<< "icounters["<<i_sc<<"] = "<< icounters[i_sc]<<endl; //debug*/
        }
        //     hypero_sca_prnt_();
        /* DIP in ************************************************* */
        HF1(-2000, (float)icounters[15], 1.);//int clock_miss)
        if (spill_miss_old != icounters[1] ) 
        {
          //*******************************
          //Start of new spill of LGD2 data 
          //*******************************
          while ( p_MWPC_beam_queue->Get_CAMAC_spill() > 0 && 
            p_MWPC_beam_queue->Get_CAMAC_spill() < icounters[1] ) 
          {
            p_MWPC_beam_queue->Remove_from_queue();
          }
    //		  if ( p_MWPC_beam_queue->Get_CAMAC_spill() < 0 ) {
    //		    cout << " **** Match_MWPC_to_LGD2 : No MWPC data in spill "
    //			 << setw(3) << icounters[1] << " **** " << endl;
    //		  }                                                               //commented ; kondr
          spill_miss_old   = icounters[1];
        }
        /************************************************************/
        float Ax, Bx, Ay, By; 
        if ( p_MWPC_beam_queue->Match_MWPC_to_LGD2 ( icounters[16], icounters[15],Ax, Bx, Ay, By ) >= 0 )
        {
          float X_LGD2 = Ax + Bx*XYZ_LGD2[2];
          float Y_LGD2 = Ay + By*XYZ_LGD2[2];
          HF2 ( -1852, X_LGD2, Y_LGD2, 1. );
          mwpc_track_.Ax1 = Ax;
          mwpc_track_.Bx1 = Bx;
          mwpc_track_.Ay1 = Ay;
          mwpc_track_.By1 = By;
          //       track_scalers_prnt_();
          //       track_scalers_prnt_();
        }
        /* DIP end ************************************************ */
      }
      //	    if(ima==1)printf("       I return, L=%i\n",L);  // debug
    }
  }  // L>4
      // return value L= -2 finish run
      //              L= -3 finish work		  
      //---------------------------------------------------------
 RECONS_after: 
  if(L>0) L=0;
  if(L<(-1))
  {
    if(L==(-3)) printf(" End of processing by force-flag=%i\n",L);
    else        printf(" End of Run by force-flag=%i\n",L);
    goto RECONS_finish_run;
  }

  if(RunHead)L=0;
  if(RunHead||(idata_type==0)||(idata_type==3)) goto READ_RECORD;
  goto EVENT_cicle_inside_buffer_LE74;  

      //----------------
 RECONS_finish_run:
  if(ierr)  printf(" >>>   Read file err. of data_file, ierr= %d\n", ierr);
  if(!open_file_flag)
  {
    gzclose(frun); open_file_flag=1;
    printf("   close data_file, nrec/ev=%i %i\n",nrec,evi);
  }
    printf("     number of oveblocks      =%i\n",overtailb_counter);
    printf("     number of event overtails=%i\n",overtaile_counter);
    /* DIP in ************************************************************* */
    //cout << "End of Run" << endl;
    while ( p_MWPC_beam_queue->Remove_from_queue() > 0 );         // all right, this is cool, but ... took a minute or so to comprehend
    spill_miss_old  = -1;
    camac_spill_old = -1;
    /* DIP end ************************************************************ */
    if(L==(-3))goto ErrExit;  // finish work
    if(argc!=2)goto a10; 	  // goto next run

    //------------------------------------------------------
 ErrExit: 
    printf(">>>   Finish, nev(tot)=%i   <<<\n",evit);
    //  if(fEvbuf){delete [] p2; fEvbuf=0;}
    printf("reconhyp.cxx: I call finishall()\n");
    /*Evd+*/
    HF1(-250,0.5,float(scalers_16_.n_beam_tot_into_gate));//1st bin - n_beam
    HF1(-250,1.5,float(scalers_16_.n_beam_tot_pi_plus));//2nd bin - n_pi_plus
    HF1(-250,2.5,float(scalers_16_.n_beam_tot_k));//3nd bin - n_k
    HF1(-250,3.5,float(scalers_16_.n_phys_trig_into_gate_tot));//4rd bin - n_phys_trig
    HF1(-250,4.5,float(scalers_16_.n_phys_trig_into_gate_accepted_tot));//5rd bin - n_phys_trig_accepted
    /*Evd-*/
    finishall_();
    /* DIP in ************************************************************* */
    cout << setw(51) << "********* End of reconhyp work ********* : event = " 
         << setw(10) << event_for_print << endl;
    cout << setw(51) << "n_phys_trig_into_gate_accepted_tot = "
         << setw(10) <<  scalers_16_.n_phys_trig_into_gate_accepted_tot << endl;
    cout << setw(51) << "n_beam_tot_into_gate              = " 
         << setw(10) << scalers_16_.n_beam_tot_into_gate << endl;
    /*Evd+*/
    cout << setw(51) << "n_beam_tot_pi_plus                   = "
         << setw(10) << scalers_16_.n_beam_tot_pi_plus << endl;
    cout << setw(51) << "n_beam_tot_K_plus                    = "
         << setw(10) << scalers_16_.n_beam_tot_k << endl;
    /*Evd-*/
    if(scalers_16_.n_phys_trig_into_gate_accepted_tot > 0)
    cout << setw(51) << "n_phys_trig_into_gate_accepted_tot/n_beam_tot_into_gate = "
         << fixed << setprecision(3) 
         << 100.*scalers_16_.n_phys_trig_into_gate_accepted_tot/scalers_16_.n_beam_tot_into_gate << "%" << endl;
    /* DIP end ************************************************************ */
    exit(0);
    //-----------
 Evbuf_overflow:
    L_out+=(Imidas_dim+1);
    printf("reconhyp:  total output event length=%i\n",L_out);
    printf("L2e_out=%i\n",L2e_outgen);
    printf("  overflowed array Evbuf[%i]\n",nEvbuf_max);
    printf("  increase parameter nEvbuf_max in reconhyp.cxx\n");
    fclose(fTexp);
    exit(1);
}
