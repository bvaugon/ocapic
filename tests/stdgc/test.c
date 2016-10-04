/*************************************************************************/
/*                                                                       */
/*                                OCaPIC                                 */
/*                                                                       */
/*                             Benoit Vaugon                             */
/*                                                                       */
/*    This file is distributed under the terms of the CeCILL license.    */
/*    See file ../../LICENSE-en.                                         */
/*                                                                       */
/*************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include "mem.h"
#include "gc.h"
#include "printer.h"

int main(){
  unsigned int i, b1, b2, b3, b4, b5;
  b1 = alloc_bloc(2, 11);
  b2 = alloc_bloc(1, 22);
  b3 = alloc_bloc(1, 33);
  b4 = alloc_bloc(1, 44);
  b5 = alloc_bloc(1, 55);
  set_field(b1, 0, b2);
  set_field(b1, 1, b4);
  set_field(b2, 0, b3);
  set_field(b3, 0, val_of_int(78));
  set_field(b4, 0, b5);
  set_field(b5, 0, b2);

  for(i = 0 ; i < ROOTS_NB ; i ++) set_root(i, b1);

  while(1){
    //    print_mem();
    print_graph();
    alloc_bloc(3, 99);
    sleep(1);
  }
  return 0;
}
