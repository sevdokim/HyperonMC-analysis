// #include "system.h"
#include "stdlib.h"
#include "stdio.h"
#include "string_text.h" 
/************************************************************************/
     
int ucopy_string_from_file(char *L1,char *L2,int L160)
//---
// read one character string from file that name is given by pointer L1. 
// Pointer to extracted string is given by L2. Length of string is 
// defined by varaible L160.
// End_line label='\0' is positioned. 
// output value of function ucopy_string_from_file=number of symbols
//                                                 -1 means END_FILE
// function uses internal pointer FILE=*fp_file_names 
//---
{     static FILE *fp_file_names;
      char c,cbl=' ';
      int in,icomments,ic,ibls,i162,i163,iempty,icin,inc;
//    int isy;
      static int ifirst=1,iend_file=0;
      const int debug=0;  // 0 - no debug print	,1,2 - debug printing	

      i162=L160-2; i163=L160-3; iempty=-1; inc=0;
      	
      if(ifirst)  // open file with file_names
	{ifirst=0; iend_file=0;
        fprintf(stderr,"ucopy_string_from_file: I open %s \n",L1);
        if((fp_file_names=fopen(L1,"r"))==NULL ){perror(L1); return -1;}    
        }
a1: if(iend_file){ifirst=1; iend_file=0; return iempty;}
	
                icomments=0; ic=0; in=0; icin=0; ibls=0; 
//		isy=0;
                while(ic<L160){*(L2+ic)='\0'; ic++;} // set end_line
		
		if(debug>1)
		{fprintf(stderr," in,icomments= %d %d \n", in,icomments);
		 fprintf(stderr," after blank: %s \n",L2); 
		} 
		 
// read one symbol
L52:            ic=1; 
//              isy++;
                if((c=getc(fp_file_names))==EOF)iend_file=1; 
                if(debug>1)fprintf(stderr," in,iend= %d %d\n",in,iend_file);       
		if(iend_file)goto L58;
					       
		ic=0;
		if(c==' ')ic=1;
		if((c=='\015')||(c=='\012')||(c=='\n'))ic=2;// curriage return
		if(c=='\0')ic=2;
		if((c=='#'))icomments=1;   // comments
                if((ic!=1)&&(ic!=2)&&(icomments==0))inc=1;
   if((debug>1)&&(ic!=2))fprintf(stderr," ** c(char)->%c!!! c(i)->%d! \n",c,c);
   if(debug>1)fprintf(stderr,"1) in,ic,icomments %d %d %d \n",in,ic,icomments);
		if(ic==2){if(icomments){icomments=0; if(in==0)goto L52;}
		          icomments=0; goto L58;
			  } 
		if(icomments)goto L52;       
		                    
//                                           fill out_string
		if(ic==1)                 // blank
		  {
//		  if(isy==i162)goto L58;
//		  if(in==0)goto L52;
		  ibls++;
		  if(ibls>=2)goto L52;
		  }
		  else
		  ibls=0;
		           
		*(L2+in)=c;                
		in++; icin=in;
		if(debug>1)
		{fprintf(stderr,"accept) in,ic,icomments %d %d %d \n",in,ic,icomments);
                fprintf(stderr,"        out_string(in)=%c \n",c); 
                fprintf(stderr,"        out_string    =%s \n",L2); 
                }
		if(in==i162)goto L58;
//		if(isy==i162)goto L58;
		goto L52;

// end of string
L58:		if((in==0)||(inc==0))
                  {if(icomments)goto a1;
		  iend_file=1; // empty string=END_FILE
		  iempty=0;
		  goto a1;
		  }
		  
                if(debug)
		{if(debug>1)
		 fprintf(stderr,"       string end line=%c!!! \n",*(L2+L160-1));                
                 printf(" I fill string: %s\n",L2);
		}
		if(debug>1)
		{fprintf(stderr,"         in= %d \n",in);
		 fprintf(stderr,"  codes 015 012 n 0 %c %c %c %c \n",'\015','\012','\n','\0'); 
		 fprintf(stderr,"  codes 015 012 n 0 %d %d %d %d \n",'\015','\012','\n','\0'); 
	        }	  

                i163=0; ic=0; inc=0;// i163 - here end_line
		while(ic>=0){c=*(L2+ic); i163=ic; ic++; if(c=='\0')ic=-1;}
                i162=i163-1; 
		if(i162<0){icin=0; goto Exi0;}
                icin=i162;
                while(ic>=0){c=*(L2+ic); i162=ic; ic--; if(c!=cbl)ic=-1;}
                i162++; icin=i162;
		if(i162<i163){*(L2+i162)=cbl; i162++;}
		icin=i162;
		while(i162<i163){*(L2+i162)='\0'; i162++;}
                   
Exi0:		return icin; 
}

int ucopy_string1_from_file(char *L1,char *L2,int L160)
//---
// read one character string from file that name is given by pointer L1
// and close file 
// Pointer to extracted string is given by L2. Length of string is 
// defined by varaible L160.
// End_line label='\0' is positioned. 
// output value of function ucopy_string1_from_file=number of symbols
//                                                  -1 - means END_FILE
//
// function uses external work pointer FILE=*fp_file_names 
//---
{     char c,cbl=' ';
      int in,icomments,ic,icin,ibls,i162,i163,iempty,inc;
//    int isy;
      static int ifirst=1,iend_file=0;
      const int debug=0;  // 0 - no debug print	,1,2 - debug printing	

      FILE *fp_file_names;  // pointer na potok of data list

      i162=L160-2; i163=L160-3; iempty=-1; inc=0;
      ifirst=1;
      	
      if(ifirst)  // open file with file_names
	{ifirst=0; iend_file=0; 
        if(debug)fprintf(stderr,"ucopy_string1_from_file: I open %s \n",L1);
        if((fp_file_names=fopen(L1,"r"))==NULL ){perror(L1); return -1;}    
        }
a1: if(iend_file){ifirst=1; iend_file=0; return iempty;}
	
                icomments=0; ic=0; in=0; icin=0; ibls=0; 
//		isy=0;
                while(ic<L160){*(L2+ic)='\0'; ic++;} // set end_line
		
		if(debug>1)
		{fprintf(stderr," in,icomments= %d %d \n", in,icomments);
		 fprintf(stderr," after blank: %s \n",L2); 
		} 
		 
// read one symbol
L52:            ic=1; 
//              isy++;
                if((c=getc(fp_file_names))==EOF)iend_file=1; 
                if(debug>1)fprintf(stderr,"     in,iend= %d %d\n",in,iend_file);       
		if(iend_file)goto L58;
			       
		ic=0;
		if(c==' ')ic=1;
		if((c=='\015')||(c=='\012')||(c=='\n'))ic=2;// curriage return
		if(c=='\0')ic=2;
		if((c=='#'))icomments=1;   // comments
		if((ic!=1)&&(ic!=2)&&(icomments==0))inc=1;
   if((debug>1)&&(ic!=2))fprintf(stderr," ** c(char)->%c!!! c(i)->%d! \n",c,c);
   if(debug>1)fprintf(stderr,"1) in,ic,icomments %d %d %d \n",in,ic,icomments);
		if(ic==2){if(icomments){icomments=0; if(in==0)goto L52;}
		          icomments=0; goto L58;
			  }  
		if(icomments)goto L52;    
		                    
//                                           fill out_string
		if(ic==1)                 // blank
		  {
//		  if(isy==i162)goto L58;
//		  if(in==0)goto L52;
		  ibls++;
		  if(ibls>=2)goto L52;
		  }
		  else
		  ibls=0;
		           
		*(L2+in)=c;                
		in++; icin=in;
		if(debug>1)
		{fprintf(stderr,"accept) in,ic,icomments %d %d %d \n",in,ic,icomments);
                fprintf(stderr,"        out_string(in)=%c \n",c); 
                fprintf(stderr,"        out_string    =%s \n",L2); 
                }
		if(in==i162)goto L58;
//		if(isy==i162)goto L58;
		goto L52;

// end of string
L58:		if((in==0)||(inc==0))
                  {if(icomments)goto a1;
		  iend_file=1; // empty string=END_FILE
		  iempty=0;
		  goto a1;
		  }
		  
                if(debug)
		{if(debug>1)
		 fprintf(stderr,"       string end line=%c!!! \n",*(L2+L160-1));                
                 printf("ucopy_string1_from_file, I fill string: %s\n",L2);
		}
		if(debug>1)
		{fprintf(stderr,"         in= %d \n",in);
		 fprintf(stderr,"  codes 015 012 n 0 %c %c %c %c \n",'\015','\012','\n','\0'); 
		 fprintf(stderr,"  codes 015 012 n 0 %d %d %d %d \n",'\015','\012','\n','\0'); 
	        }	  

                i163=0; ic=0; inc=0;// i163 - here end_line
		while(ic>=0){c=*(L2+ic); i163=ic; ic++; if(c=='\0')ic=-1;}
                i162=i163-1; 
		if(i162<0){icin=0; goto Exi0;}
                ic=i162;
                while(ic>=0){c=*(L2+ic); i162=ic; ic--; if(c!=cbl)ic=-1;}
                i162++;
		if(i162<i163){*(L2+i162)=cbl; i162++;}
		icin=i162;
		while(i162<i163){*(L2+i162)='\0'; i162++;}
                   
Exi0:		fclose(fp_file_names);
                return icin; 
}

int ucopy_string(char *L1, char *L2,int i0,int L160)
//---
// copy character string  L1 -> L2
// Length of tring is defined by variable L160
// The last end_line='\0' is set
//
// first used element on L2 is 0 if i0=0
// first used element on L2 is after the last symbol if i0=1
// first used element on L2 is after the last meaning symbol plus blank if i0=2
// first used element on L2 is after the last meaning symbol
//        
// output value of function ucopy_string=1 means empty string
//---
{     char c,cbl=' ',*L;
      int in,ic,Li,ibls,i1,i162,i163,icin=0;
//    int isy;       
      const int debug=0;  // 0 - no debug print, 1,2 - debug printing
                
		if(debug>0)
		{L=L1; if(i0!=0)L=L2;
		 fprintf(stderr,"ucopy_string: i0,Len=%d %d; %s\n",i0,L160,L);
		 if(i0>0)
		 fprintf(stderr,"                            %s\n",L1); 
		}
		
// i162 is possible maximal address of location of the last meaning symbol in string *L2
// i1 is real location of the last meaning symbol in string *L2
                i162=L160-2; i163=L160-3; 		

                Li=0; i1=-1; in=0; icin=0; ibls=0;
//      	isy=0;              
                if(i0==0)
	          {ic=0;
		  while(ic<L160){*(L2+ic)='\0'; ic++;}  // fill all string *L2 by end_line
		  }else // find i1 last meaning symbol of string *L2
		  {ic=0; i1=i162;
		   while(ic<=i162){c=*(L2+ic);  
		                  if(c=='\0'){i1=ic-1; icin=ic; ic=L160;}
				  ic++;				 
			         }
                   ic=i1;
                   while(ic>=0)   {c=*(L2+ic); ic--;    // delete all end blanks in string *L2 
		                  if(c==cbl)i1=ic;
				  else
				  ic=-1;				 
			         }				 				                    
                   if((i1<i162)&&(i0!=3))
		                 {ic=i1+1; c=*(L2+ic); // but the last end blank take into account
				  if((c!='\0')&&(c==cbl))i1=ic;
                                 }
				 
                   if((i1<i162)&&(i0==2))              // add blank in the end of string *L2 by force
		                 {                       
		                 if(i1<0){i1=0; *(L2+i1)=cbl;}
		                 c=*(L2+i1); 
				 if(c!=cbl){i1++; *(L2+i1)=cbl;}				 				 				 
				 }
		  }

                icin=i1+1;		  
// read one symbol
L52:            ic=1; 
//              isy++;
                c=*(L1+Li); Li++;
	        if(debug>1)fprintf(stderr," ** c(char)->%c!!! c(i)->%d! \n",c,c);					       
		ic=0;
		if(c==cbl)ic=1;
		if((c=='\015')||(c=='\012')||(c=='\n'))ic=2;// curriage return
                if(c=='\0')ic=2;
		if((Li>=(L160-1)))ic=2;
              if(debug>1)fprintf(stderr,"1) in,ic %d %d \n",in,ic);
                if(ic==2)goto L58;   
		                    
//                                           fill output string
		if(ic==1)                 // blank
		  {
//		   if(isy==i162)goto L58;
//		   if(in==0)goto L52;
		   ibls++;
		   if(ibls>=2)goto L52;
		  }
		  else
		  {ibls=0;}
		           
                in++;
		*(L2+in+i1)=c; icin=in+i1+1;               		
		if(debug>1)
		{fprintf(stderr,"accept) in,ic %d %d \n",in,ic);
                 fprintf(stderr,"        out_string(in)=%c \n",c); 
                 fprintf(stderr,"        out_string=%s \n",L2); 
                }
		 if(((in+i1)==i162))goto L58;
//		 if(isy==i162)goto L58;
		 goto L52;

// end of string
L58:            if(icin==0)return 0; // empty string		  
                if(debug)
		{if(debug>1)
		 fprintf(stderr,"       string end line=%c!!! \n",*(L2+L160-1));                
                 if(i0)fprintf(stderr,"             I fill string: %s\n",L2);
		}
	        if(debug>1)
		{fprintf(stderr,"         in= %d \n",in);		
		 fprintf(stderr,"  codes 015 012 n 0 %c %c %c %c \n",'\015','\012','\n','\0'); 
		 fprintf(stderr,"  codes 015 012 n 0 %d %d %d %d \n",'\015','\012','\n','\0'); 
	        }	  

                i163=0; ic=0; // i163 - here end_line
		while(ic>=0){c=*(L2+ic); i163=ic; ic++; if(c=='\0')ic=-1;}
                i162=i163-1; 
		if(i162<0){icin=0; goto Exi0;}
                ic=i162;
                while(ic>=0){c=*(L2+ic); i162=ic; ic--; if(c!=cbl)ic=-1;}
                i162++;
		if(i162<i163){*(L2+i162)=cbl; i162++;}
		icin=i162;
		while(i162<i163){*(L2+i162)='\0'; i162++;}
                   
Exi0:           return icin;
 
}

int deletgz_string(char *L1, char *L2)
//---
// replace symbols *L2 (like ".gz") to blanks in character string *L1
// It should be set end-line='\0' in the end
//        
// output value of function deletgz_string=1 if symbols ".gz" was found
//---
{     char c,c0,cbl=' ',end_line='\0';
      int ic,k=0,n,iend1,iend2,i162,i162_2;

//		  c0='z';
	          c0=*(L2);
		  if(c0==end_line)return 0;
		  
// 2nd string (last not blank is i162_2)
		  i162=0; ic=0;
		  while(ic>=0){c=*(L2+ic); i162=ic; ic++; if(c==end_line)ic=-1;}
		  iend2=i162;
		  i162--;
                  if(i162<=0)return 0;

		  ic=i162;
		  while(ic>=0){c=*(L2+ic); i162=ic; ic--; if(c!=cbl)ic=-1;} 
		  i162_2=i162;
		  			  
// 1st string (last not blank is i162)		  		  		  
                  i162=0; ic=0;
		  while(ic>=0){c=*(L1+ic); i162=ic; ic++; if(c==end_line)ic=-1;}
                  iend1=i162;
                  i162--;
		  
		  ic=i162;
		  while(ic>=0){c=*(L1+ic); i162=ic; ic--; if(c!=cbl)ic=-1;} 		
                 
// check if end of 1st string = 2nd string
		  ic=i162; n=0;
		  while(ic>=0){c=*(L1+ic); k=ic; ic--; 
		               c0=*(L2+i162_2); i162_2--; if(i162_2<0)ic=-1;        
			       if(c==c0)
			        {n++; *(L1+k)=cbl;}
		               else
			        {n=0; ic=-1;}
		              }
			      
                  if(i162_2>=0)n=0;
		  if(n==0)return 0;
		  
		  ic=k;
		  while(ic>=0){c=*(L1+ic); k=ic; ic--; if(c!=cbl)ic=-1;}
		  k++;
		  
		  if(k<iend1) 
		  {*(L1+k)=cbl;
		  k++;                  
                  if(k<iend1)
		  {ic=k;
		  while(ic>=0){*(L1+ic)=end_line; ic++; if(ic>=iend1)ic=-1;}		  	      
                  }}
               
		  return 1; 	       			       
}			       

int delet_to_symbol(char *L1,char cd,int nd,int id)
//---
//  delet string *L1 to symbol cd
//  nd - number of symbols cd ( nd=0 - seak the last symbol cd)
//  id =0,1 (0 - cd is not deleted, 1 - cd is also deleted)   
//---
{     char c,end_line='\0';
      int ic,i1,i2,i11;
      const int debug=0; // 0 - no debug print 		
                    
	ic=0; 
	while(ic>=0)
	{c=*(L1+ic); i2=ic; ic++; if(c==end_line)ic=-1; 
	} // end of string
	i2--;  // i2 is the last meaning symbol
	if(i2<0)return 0;
	
	i1=-1; i11=0; ic=0;
	while(ic>=0)
	{c=*(L1+ic); 
	 if(c==cd){i1=ic; i11++; if(nd>0){if(i11>=nd)ic=-2;}}	
	 ic++; if(ic>i2)ic=-1; 
	} // end of string
	if(i1<0)return 0;
	if(id!=0)i1++;
	if(i1>i2)return 0;
	
	ic=i1; i11=0;
	while(ic>=0) // delete part of string
	{*(L1+i11)=*(L1+ic); i11++;   	
	 ic++; if(ic>i2)ic=-1; 
	} 
		
	ic=i2-i1+1; 
	while(ic>=0)
	{*(L1+i11)=end_line;    	
	 ic++; if(ic>i2)ic=-1; 
	}
	
	if(debug)fprintf(stderr,"delet_to_string: %s\n",L1);

	return 0;
}

int blank_string(char *L1,int i0,int L160)
//---
//  fill string by blanks from position i0=0,1, ... L160-2
//---
{     char c,cbl=' ';
      int ic,i1,i162;
      
      i162=L160-2; 		
                 
      i1=i0; 
      if(i1<0)
	{ic=0; i1=0;
	while(ic<=i162)
	{c=*(L1+ic); i1=ic; ic++;
	 if(c=='\0')ic=L160; 
	}
        }

      if(i1>=0)
      {
      if(i1<=i162)
        {ic=i1;
	while(ic<=i162){*(L1+ic)=cbl; ic++;}
        }
      }
      return i1;
}

int ndata_in_string(char *L1)
//---
//  define number of data in string  
//---
{     char c,cbl=' ',cbl2=9;
      int ic,ibeg_dat,ndat,icbl=0; 		
                       
	ndat=0; ibeg_dat=1;
	ic=0;
	while(ic>=0)
	{c=*(L1+ic); 	 
         icbl=0;
	 ic++;
	 if(c=='\0'){ic=-1; goto end1;} // end of string
         if((c==cbl)||(c==cbl2))icbl=1;	 
	 if(icbl){ibeg_dat=1; goto end1;}
	 if(ibeg_dat){ndat++; ibeg_dat=0;}
end1:  continue;  
        }	 
      
      return ndat;
}

int length_of_string(char *L1, char a)
//---
//  define length of string  if a is comment symbol (a=/= ' ')
//  and comments are replaced by blanks
//---
{     char c,cbl=' ',end_line='\0';
      int ic,L,ido; 		
                       
	ic=0; ido=0;
	while(ido==0)
	{c=*(L1+ic); ic++; 	 
	 if(c==end_line)ido=1;       // end of string
	 else       
	 {if(c!=cbl){if(c==a)ido=2;}} // comments 	 
        }
	L=ic-2;	 
		
	if(ido==2) // replace comments by symbol blank
	{ic=L+1;
	 while(ic>=0)
	     {*(L1+ic)=cbl; ic++;
	      c=*(L1+ic);
	      if(c==end_line)ic=-1;
	     }
        }
      
        if(L>=0)
	{ic=L;
	while(ic>=0)
	{c=*(L1+ic); L=ic; ic--; 	 
	 if(c!=cbl)ic=-1; // not blank
        }}        
       
       ic=L+1;
       if(L<0)ic=0;
            
      return ic;
}

int integer_from_string(char *L1)
//---
//  extract integer from string  
//---
{     char c,cbl=' ',f[]="0123456789";
      int ic,ic2,i1=0,i2=0,i22,iff,k,is,m; 		
                    
	ic=0; i1=0; i2=0;
	while(ic>=0)
	{c=*(L1+ic); i2=ic; ic++; if(c=='\0')ic=-1; } // end of string
	i2--;
	if(i2<0)return 0;
	ic=i2;
	while(ic>=0)
	{c=*(L1+ic); i2=ic; ic--; if(c!=cbl)ic=-1; } 
	 
        i22=i2;
	i1=-1; i2=-1; ic=0;
	while(ic>=0)
	{c=*(L1+ic); ic++; if(ic>i22)ic=-1;
	 iff=0; ic2=0;
	  while(ic2<=9){if(f[ic2++]==c){iff=1; ic2=10;}} 
	  if(iff){if(i1<0)i1=ic-1; i2=ic-1;}
	  else
	  {if(i1>=0)ic=-1;}
	}   

// integer is located between i1 - i2
         if(i2<0)return 0;         
	 is=0; m=1;
	 while(i2>=i1)
	 {c=*(L1+i2); i2--;
	 ic=0; 
	 while(ic<=9){k=ic; ic++; if(f[k]==c){is=is+k*m; ic=10;}} 
         m=m*10;
         }
         return is;
}

int integer_from_string_after_symbol(char *L1, char a)
//---
//  extract integer from string  
//---
{     char c,cbl=' ',f[]="0123456789";
      int ic,ic2,i1,i2,iff,k,is,m,i11,i22; 		
                    
	i11=0; i22=0; ic=0; 
	while(ic>=0)
	{c=*(L1+ic); i2=ic; ic++; if(c=='\0')ic=-1;  
	 if(c==a)i11=i2+1;
	} // end of string
	i2--;
	if(i2<0)return 0;
	
	ic=i2;
	while(ic>=i11)
	{c=*(L1+ic); i2=ic; ic--; if(c!=cbl)ic=-1; } // ignore the last blanks 
        i22=i2;
	
	i1=i11; ic=i1;
	while(ic>=i22)
	{c=*(L1+ic); i1=ic; ic++; if(c!=cbl)ic=-1; } // ignore the 1st blanks 
        i11=i1;	
	
	ic=i11;
        while(ic<=i22)     // find the 1st figure
	{c=*(L1+ic); 
	  iff=0; ic2=0;
	  while(ic2<=9){if(f[ic2++]==c){iff=1; ic2=10;}}
	  if(iff){i11=ic; ic=i22;}
	  ic++;
	}  

        if(i11>i22)return 0;
	
        i2=i22;
	ic=i11;
        while(ic<=i2)      // find the last figure
	{c=*(L1+ic); 
	  iff=0; ic2=0;
	  while(ic2<=9){if(f[ic2++]==c){iff=1; ic2=10;}} 
	  if(!iff){i22=ic-1; ic=i2;}
	  ic++;
	}  
        
	if(i11>i22)return 0;
	
	i1=i11; i2=i22;		

// integer is located between i1 - i2
	 
	 is=0; m=1;
	 while(i2>=i1)
	 {c=*(L1+i2); i2--;
	 ic=0; 
	 while(ic<=9){k=ic; ic++; if(f[k]==c){is=is+k*m; ic=10;}} 
         m=m*10;
         }
         return is;
}


char get_symbol_from_string(char *L1, int id)
//---
//  fine symbol in string
//  id>=0 symbol number id
//  id=-1 - 1st meaning symbol
//  id=-2 - last meaning symbol
//---
{     int ic;
      char c,c0;
      const char cbl=' ',end_line='\0';
      const int id1=-1; 		
                       	       
	c0=cbl;
	
	ic=0;
	while(ic>=0)
	{c=*(L1+ic); 	 
	 if(c==end_line)
	    ic=-2;
	 else
	    {if((ic==id)&&(id>=0)){c0=c; ic=-2;}
	     if(c!=cbl){c0=c; if(id==id1)ic=-2;}
	    }
	 ic++; 
	}
	
	return c0;
}

int deletblank_string(char *L1,int Lmax)
//---
// delete blanks in string 
// It should be set end-line='\0' in the end
//        
// output value gives number of not blank symbols
//---
{     char c,cbl=' ',end_line='\0';
      int ic,ic2;

 ic=0; ic2=0;
 while(ic>=0){c=*(L1+ic); if(c==end_line)ic=-1; 
	     if(ic>=0){if(ic>=Lmax){printf("deletblank_string: too long line\n"); exit(1);}
	              if(c!=cbl){*(L1+ic2)=c; if(ic!=ic2){*(L1+ic)=cbl;} ic2++;} ic++;} 
	     } 	     
if(ic2>0){if(ic2<Lmax){*(L1+ic2)=end_line;}}
return ic2;
}
