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

#include <stdlib.h>
#include <stdio.h>
#include "mem.h"

#define COPIED_TAG 245

unsigned int unsafe_alloc_bloc(unsigned int bloc_size, unsigned char tag){
  unsigned int bloc_addr = current_pos + 4;
  set_size(bloc_addr, bloc_size);
  set_tag(bloc_addr, tag);
  current_pos += (bloc_size + 1) * 4;
  return bloc_addr;
}

void switch_segments(void){
  unsigned int tmp = current_seg_anch;
  current_seg_anch = other_seg_anch;
  other_seg_anch = tmp;
  current_pos = current_seg_anch;
}

unsigned int copy(unsigned int src){
  unsigned int i;
  unsigned int size = get_size(src);
  unsigned int tag = get_tag(src);
  unsigned int dest = unsafe_alloc_bloc(size, tag);
  for(i = 0 ; i < size ; i ++){
    set_field(dest, i, get_field(src, i));
  }
  return dest;
}

void gc(){
  unsigned int i;
  unsigned int ptr = other_seg_anch;
  switch_segments();
  for(i = 0 ; i < ROOTS_NB ; i ++){
    unsigned int root = get_root(i);
    if(!is_int(root)){
      if(get_tag(root) == COPIED_TAG){
        set_root(i, get_field(root, 0));
      }else{
        unsigned int root_copy = copy(root);
        set_root(i, root_copy);
        set_field(root, 0, root_copy);
        set_tag(root, COPIED_TAG);
        while(ptr < current_pos){
          int j;
          unsigned int node = ptr + 4;
          unsigned int size = get_size(node);
          for(j = 0 ; j < size ; j ++){
            unsigned int field = get_field(node, j);
            if(!is_int(field)){
              if(get_tag(field) == COPIED_TAG){
                set_field(node, j, get_field(field, 0));
              }else{
                unsigned int field_copy = copy(field);
                set_field(node, j, field_copy);
                set_field(field, 0, field_copy);
                set_tag(field, COPIED_TAG);
              }
            }
          }
          ptr += (size + 1) * 4;
        }
      }
    }
  }
}

unsigned int alloc_bloc(unsigned int bloc_size, unsigned char tag){
  unsigned int free_space_size = SEGMENT_SIZE + current_seg_anch - current_pos;
  unsigned int wanted_size = (bloc_size + 1) * 4;
  if(free_space_size >= wanted_size){
    return unsafe_alloc_bloc(bloc_size, tag);
  }else{
    gc();
    free_space_size = SEGMENT_SIZE + current_seg_anch - current_pos;
    if(free_space_size >= wanted_size){
      return unsafe_alloc_bloc(bloc_size, tag);
    }else{
      fprintf(stderr, "Allocation failed, full memory\n");
      exit(1);
    }
  }
}
