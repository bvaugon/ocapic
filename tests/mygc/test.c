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
#include <time.h>
#include <unistd.h>
#include "gc.h"

int main(){
  unsigned short i;
  unsigned short bloc, size, tag, b1, b2, b3, b4, b5;
  srand(time(NULL));
  for(i = 0 ; i < 8 ; i ++) set_root(i, 2);

  b1 = alloc(2, 0x11);
  b2 = alloc(1, 0x22);
  b3 = alloc(1, 0x33);
  b4 = alloc(1, 0x44);
  b5 = alloc(1, 0x55);
  set_field(b1, 0, b2);
  set_field(b1, 1, b4);
  set_field(b2, 0, b3);
  set_field(b3, 0, b4);
  set_field(b4, 0, b5);
  set_field(b5, 0, b2);
  print();

  while(1){
    size = rand() % 5 + 1;
    tag = rand() % 32;
    bloc = alloc(size, tag);
    for(i = 0 ; i < size ; i ++)
      set_field(bloc, i, 0x7777);
    print();
  }
  return 0;
}
