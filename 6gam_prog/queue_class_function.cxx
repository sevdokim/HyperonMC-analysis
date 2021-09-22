#include "queue.h"
///////////// Function definition for class queue ////////////////////////////
/*************/
queue::queue()
/*************/
{
  //      cout << " +++++++++++++++++++++++++++" << endl;
  //      cout << " queue::queue : Constructor " << endl;
  //      cout << " +++++++++++++++++++++++++++" << endl;
  head = tail = next = NULL;
  n_in_queue =0;
}
/**************/
queue::~queue()
/**************/
{
  //  cout << " ----------------------------------------" << endl;
  //  cout << " queue::queue : Destructor of class queue" << endl;
  //  cout << " ----------------------------------------" << endl;
}
/*********************/
void queue::NewArray()
/*********************/
{
  //    cout << " ************************************************************" 
  //         << endl;
  //    cout << " queue::NewArray(" << m_Row << "," << m_Col 
  //         <<"). Spill_number = " << setw(4) << Spill_number
  //         << " Length_spill=" << setw(4) << Length_spill << endl;
  //    cout << " ************************************************************" 
  //         << endl;

  m_MWPC_data = new type_i*[m_Row];
  for( uint i = 0; i < m_Row; ++i )
    m_MWPC_data[i] = new type_i[m_Col];
  m_MWPC_beam = new type_f*[2*m_Row];
  for( uint i = 0; i < 2*m_Row; ++i )
    m_MWPC_beam[i] = new type_f[m_Col];

  //  Print();

}
/**********************/
void queue::ZeroArray()
/**********************/
{ 
  //  cout << " *************************************************************" 
  //       << endl;
  //  cout << " queue::ZeroArray(" << m_Row << "," << m_Col 
  //       <<"). Spill_number = " << setw(4) << Spill_number
  //       << " Length_spill=" << setw(4) << Length_spill << endl;
  //  cout << " *************************************************************" 
  //       << endl;

  for (   uint i = 0; i < m_Row; ++i )
    for ( uint j = 0; j < m_Col; ++j ) {
      m_MWPC_data[i][j] = type_i();
    }
  for (   uint i = 0; i < 2*m_Row; ++i )
    for ( uint j = 0; j < m_Col; ++j ) {
      m_MWPC_beam[i][j] = type_f();
    }
}
/******************/
void queue::Print()
/******************/
{
  cout << " queue::Print : Массив MWPC_data " 
       << m_Row << "(row) x " << m_Col << "(col) :\n";
  if ( Length_spill >= 0 ) {
    for (  uint i = 0; i < m_Row; ++i ) {
      cout << setw(5) << "Row " << setw(8) << i << " : ";
      for ( int j = 0; j <= Length_spill; ++j ) {
	cout << setw(8) << m_MWPC_data[i][j];
	if( j +1 - 6*((j+1)/6) == 0 ) {
	  cout << endl;
	  cout << setw(5) << "Row " << setw(8) << i << " : " ;
	}
      }
      cout << endl;
    }
    cout << " queue::Print : Массив MWPC_beam " 
	 << m_Row << "(row) x " << m_Col << "(col) :\n";
    for(  uint i = 0; i < 2*m_Row; ++i ){
      cout << setw(5) << "Row " << setw(8) << i << " : " ;
      for( int j = 0; j <= Length_spill; ++j ) {
	cout << std::setw(8) << m_MWPC_beam[i][j];
	if( j +1 - 6*((j+1)/6) == 0 ) {
	  cout << endl;
	  cout << setw(5) << "Row " << setw(8) << i << " : " ;
	}
      }
      cout << endl;
    }
  }
  else {
    cout << " queue::Print : Mассивы MWPC_data и MWPC_beam незаполнены" 
	 << endl;
  }
}
/*******************************************************/
void queue::Put_in_queue(int Spill, uint row, uint col )
/*******************************************************/
{
  queue *item;

  //  cout << "********************************************************" <<endl;
  //  cout << " queue::Put in queue for Spill " << setw(4) << Spill
  //       << ", row=" << setw(4) << row << ", col=" << setw(4) << col << endl;
  //  cout << "********************************************************" <<endl;

  if(!head) {
    //    cout << " *** Put in queue: Список ещё пуст" << endl;
  }
  item = new queue;
  if(!item) {
    cout << "Ошибка выделения памяти queue" << endl;
    exit(1);
  }
  ////////////////////////////////////////////////////////////////////
  n_in_queue++;
  if ( !head ) head = item;       //pointer to first element of queue
  if (  tail ) tail->next = item; //pointer to next element of queue
  tail = item;              //pointer to last element of queue
  ////////////////////////////////////////////////////////////////////
  item->Spill_number=Spill;
  item->Length_spill=-1;
  item->m_Row = row;
  item->m_Col = col;
  item->NewArray (); // создаем массивы MWPC_data и MWPC_beam
  item->ZeroArray(); // обнуляем
  //    cout << "********************************************" << endl;
  //    cout << " queue::Put_in_queue for Spill " 
  //         << setw(4) <<  Spill << " finished" << endl;
  //    cout << "********************************************" << endl;
}
/*****************************/
int queue::Remove_from_queue()
/****************************/
{
  queue *p;
  int Spill;

  if(!head) {
    //            cout << "queque::Remove_from_queue: Список ещё пуст" << endl;
    return 0;
  }
  //    cout << " queue::Remove_from_queue : Queue comprise " 
  //     << n_in_queue << " members " << endl;
  //Remove queue element from queue begin
  Spill = head->Spill_number;
  //    cout << " queue::Removed_from_queue : Spill " << setw(4) << Spill 
  //     << " is removed from queue" << endl;
  n_in_queue--;
  p = head;
  head = head->next;
  for ( uint row = 0; row <    p->m_Row;  row++ )
    delete[] p->m_MWPC_data[row];
  for ( uint row = 0; row < (2*p->m_Row); row++ )
    delete[] p->m_MWPC_beam[row];
  delete p;
  if (n_in_queue == 0 ) {
    head = tail = next = NULL;
  }
  return Spill;
}
/****************************/
void queue::Show( int Spill )
/****************************/
{
  queue *p;

  cout << " ************************************" << endl;
  cout << " queue::Show : Started for Spill " << setw(4) << Spill << endl;
  cout << " ************************************" << endl;

  p = head;
  if(!p) {
    cout << " queue::Show : Список пуст" << endl;
    return;
  }
  //Show all queue elements
  else {
    cout << " queue::Remove_from_queue : *** Queue comprise " 
	 << n_in_queue << " members *** " << endl;
    int n_queue=1;
    do {
      cout << " Queue member number is " << setw(3) << n_queue
	   <<" : Spill number " << setw(5) << p->Spill_number
	   << " Data Length " << setw(5) << p->Length_spill << endl;
      if(p->Spill_number==Spill) {
	cout << " queue::Show   : Spill " << Spill 
	     << " is found in queue." << endl;
	p->Print();
	break;
      }
      n_queue++;
      p = p->next;
    }
    while(p);
    cout << " *************************************" << endl;
    cout << " queue::Show : Finished for Spill " << setw(4) << Spill << endl;
    cout << " *************************************" << endl;
    if(!p){
      cout << " !!!! queue::Show : Spill " << Spill << " not found in queue  " 
	   << endl;
    }
  }
}
////////////////////////////////////////////////////
/*********************************************************************/
void queue::Set_MWPC( type_i &event, type_i &clock, 
	              type_f &Ax, type_f &Bx, type_f &Ay, type_f &By )
/********************************************************************/
{
  if(!head) {
    cout << " queue::Set_MWPC : Список пуст" << endl;
    return;
  }
  tail->Length_spill++;
  tail->m_MWPC_data[0][tail->Length_spill] = event;
  tail->m_MWPC_data[1][tail->Length_spill] = clock;
  tail->m_MWPC_beam[0][tail->Length_spill] = Ax;
  tail->m_MWPC_beam[1][tail->Length_spill] = Bx;
  tail->m_MWPC_beam[2][tail->Length_spill] = Ay;
  tail->m_MWPC_beam[3][tail->Length_spill] = By;
}
/***************************/
int queue::Get_CAMAC_spill()
/***************************/
{
  if(!head) {
    //    cout << " queue::Get_CAMAC_spill : Список пуст" << endl;
    return -1;
  }
  else return head->Spill_number;
}
/******************************************************/
int queue::Match_MWPC_to_LGD2 ( int &event, int &clock,
				type_f &Ax, type_f &Bx, 
				type_f &Ay, type_f &By )
/******************************************************/
{
  int d_clock_min, col_min, col_beam;
  int d_clock_min_previous, d_clock_min_previous_n,
    col_min_previous, col_min_previous_n;
  int d_clock_min_abs;
  int d_clock, d_clock_abs;
  //   cout << " ***********************************************" << endl;
  //  cout << " queue::Match_MWPC_to_LGD2 : Started Spill =" 
  //      << setw(4) << head->Spill_number << endl;
  // cout << " ***********************************************" << endl;
  if(head) {
    HF1(-2001, (float)clock, 1);
    d_clock_min_abs      = 1000000000;
    d_clock              = 0; 
    d_clock_abs          = 0;
    d_clock_min          = -1000000000;
    d_clock_min_previous = d_clock_min;
    col_min              = head->Length_spill -1;
    col_min_previous     = col_min;
    if ( head->Length_spill >= 0 ) {
      for ( int col = 0; col <= head->Length_spill; ++col ) {
	d_clock = head->m_MWPC_data[1][col] - clock;
	if ( d_clock < 0 ) d_clock_abs = -d_clock;
	else               d_clock_abs =  d_clock;
	if ( d_clock_min_abs   >   d_clock_abs ) {
	  d_clock_min_abs        = d_clock_abs;
	  d_clock_min_previous_n = d_clock_min_previous;
	  d_clock_min_previous   = d_clock_min;
	  col_min_previous_n     = col_min_previous;
	  col_min_previous       = col_min;
	  d_clock_min            = d_clock;
	  col_min                = col;
	}
      }
      if (   head->m_MWPC_data[0][col_min]            != event &&
	     head->m_MWPC_data[0][col_min_previous]   != event &&
	     head->m_MWPC_data[0][col_min_previous_n] != event ) {
	HF1( -2006, (float)d_clock_min, 1. );
	HF1( -2007, (float)(head->m_MWPC_data[0][col_min] - event),1.);
	col_beam = -2;
      } //if
      else {
	if ( head->m_MWPC_data[0][col_min]          == event ) {
	  col_beam = col_min;
	  HF1( -2002, (float)d_clock_min, 1. );
	  HF1( -2003, (float)( head->m_MWPC_data[0][col_min] - event ), 1. );
	}
	else {
	  if ( head->m_MWPC_data[0][col_min_previous] == event ) {
	    col_beam = col_min_previous;
	    HF1( -2004, (float)d_clock_min_previous, 1. );
	    HF1( -2005, 
		 (float)(head->m_MWPC_data[0][col_min_previous] - event),1.);
	    HF2( -2011, 
                 (float)d_clock_min, (float)d_clock_min_previous, 1. );
	  }
	  else {
	    if ( head->m_MWPC_data[0][col_min_previous_n] == event ) {
	      col_beam = col_min_previous_n;
	      HF1( -2004, (float)d_clock_min_previous_n, 1. );
	      HF1( -2005, 
		   (float)(head->m_MWPC_data[0][col_min_previous_n] - event),1.);
	      HF2( -2011, 
		   (float)d_clock_min, (float)d_clock_min_previous_n, 1. );
	    }
	  }
	}
      }
    }
  }
  else {
    //       cout << " queue::Match_MWPC_to_LGD2 : Нет данных MWPC"
    //    << endl;
    if(!head) return -1;
    else col_beam = head->Length_spill;
  }
  if ( col_beam >= 0 ) { 
    HF1( -3001, (float)head->m_MWPC_data[1][col_beam], 1);
    Ax    = head->m_MWPC_beam[0][col_beam];
    Bx    = head->m_MWPC_beam[1][col_beam];
    Ay    = head->m_MWPC_beam[2][col_beam];
    By    = head->m_MWPC_beam[3][col_beam];
  }
  else {
    Ax = 0;
    Bx = 0;
    Ay = 0;
    By = 0;
  }
  return col_beam;
}
