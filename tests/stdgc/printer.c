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
#include "printer.h"
#include "mem.h"

void print_mem(void){
  int i;
  for(i = 0 ; i < MEMORY_SIZE >> 2 ; i ++){
    printf("%08x", memory[i]);
    if(i % 8 == 7)
      printf("\n");
    else
      printf(" ");
  }
  if(i % 8 != 0) printf("\n");
  printf("\n");
}

void print_graph(void){
  int i;
  unsigned int ptr = current_seg_anch;
  while(ptr < current_pos){
    unsigned int addr = ptr + 4;
    unsigned int size = get_size(addr);
    unsigned char tag = get_tag(addr);
    printf("@%-3x: size=%-3d tag=%-3d [|", addr, size, tag);
    for(i = 0 ; i < size ; i ++){
      unsigned int field = get_field(addr, i);
      if(is_int(field)){
        printf(" %d ", int_value(field));
      }else{
        printf(" @%x ", field);
      }
    }
    printf("|]\n");
    ptr += (size + 1) * 4;
  }
  printf("Roots: [|");
  for(i = 0 ; i < ROOTS_NB ; i ++){
    unsigned int root = get_root(i);
    if(is_int(root)){
      printf(" %d ", int_value(root));
    }else{
      printf(" @%x ", root);
    }
  }
  printf("|]\n\n");
}
