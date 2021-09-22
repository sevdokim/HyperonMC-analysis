#include <iostream>
#include <cstdlib>
#include <iomanip>
#include <clocale>
#include <cctype>
using namespace std;
#include <cfortran.h>
#ifdef __linux__
#define f2cFortran
#endif
#include <hbook.h>
///////////////// Class definition //////////////////////////////////////////
// Base class list
class queue {
  // protected:
  typedef int          type_i;
  typedef float        type_f;
  typedef unsigned int uint;
  queue *head; // queue begin
  queue *next; // next queue element 
  queue *tail; // last queue element
  type_i n_in_queue;
  type_i Spill_number,  Length_spill;
  uint m_Row, m_Col;
  type_i **m_MWPC_data;
  type_f **m_MWPC_beam;
public:
  //  queue (uint row_data, uint col_data);//Constructor
  queue ();//Constructor
  queue ( const queue & ar );// Copy constructor
  ~queue ();
  /////////////////////////////////////////////////////////////////////////
  void NewArray ();// выделение памяти для массива MWPC_data
  void ZeroArray ();// обнуление элементов массива MWPC_data
  void Print ();// Печати текущего состояния массива: void print(...);
  void Set_MWPC ( type_i &event, type_i &clock, 
		 type_f &Ax, type_f &Bx, type_f &Ay, type_f &By );
  int Get_CAMAC_spill ();
  ////////////////////////////////////////////////////////////////////////////
  void Put_in_queue ( int Spill, uint row, uint col );
  int  Remove_from_queue ();
  void Show ( int Spill );
  int Match_MWPC_to_LGD2 ( int &event, int &clock,
			   type_f &Ax, type_f &Bx, 
			   type_f &Ay, type_f &By );
};
void Queue_pause();
