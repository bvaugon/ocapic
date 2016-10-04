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
#include "mem.h"

unsigned int memory[MEMORY_SIZE >> 2];
unsigned int current_seg_anch = SEG1_ANCH;
unsigned int other_seg_anch = SEG2_ANCH;
unsigned int current_pos = SEG1_ANCH;

unsigned char get_tag(unsigned int bloc_addr){
#ifdef DEBUG
  if(bloc_addr % 4 != 0 || bloc_addr == 0 || bloc_addr >= MEMORY_SIZE){
    fprintf(stderr,"ERROR: invalid address: get_tag(0x%x)\n",bloc_addr);
    exit(1);
  }
#endif
  return memory[(bloc_addr >> 2) - 1] & 0xFF;
}

void set_tag(unsigned int bloc_addr, unsigned char tag){
#ifdef DEBUG
  if(bloc_addr % 4 != 0 || bloc_addr == 0 || bloc_addr >= MEMORY_SIZE){
    fprintf(stderr,"ERROR: invalid address: set_tag(0x%x, %d)\n",bloc_addr,tag);
    exit(1);
  }
#endif
  memory[(bloc_addr >> 2) - 1] =
    ((memory[(bloc_addr >> 2) - 1] >> 8) << 8) | tag;
}

unsigned int get_size(unsigned int bloc_addr){
#ifdef DEBUG
  if(bloc_addr % 4 != 0 || bloc_addr == 0 || bloc_addr >= MEMORY_SIZE){
    fprintf(stderr,"ERROR: invalid address: get_size(0x%x)\n",bloc_addr);
    exit(1);
  }
#endif
  return memory[(bloc_addr >> 2) - 1] >> 8;
}

void set_size(unsigned int bloc_addr, unsigned int bloc_size){
#ifdef DEBUG
  if(bloc_addr % 4 != 0 || bloc_addr == 0 || bloc_addr >= MEMORY_SIZE){
    fprintf(stderr,"ERROR: invalid address: set_size(0x%x, %d)\n",
            bloc_addr, bloc_size);
    exit(1);
  }
  if(bloc_size == 0 || bloc_addr + bloc_size * 4 > MEMORY_SIZE){
    fprintf(stderr,"ERROR: invalid size: set_size(0x%x, %d)\n",
            bloc_addr, bloc_size);
    exit(1);
  }
#endif
  memory[(bloc_addr >> 2) - 1] =
    (memory[(bloc_addr >> 2) - 1] & 0xFF) | (bloc_size << 8);
}

unsigned int get_field(unsigned int bloc_addr, unsigned int ind){
#ifdef DEBUG
  if(bloc_addr % 4 != 0 || bloc_addr == 0 || bloc_addr >= MEMORY_SIZE){
    fprintf(stderr,"ERROR: invalid address: get_field(0x%x, %d)\n",
            bloc_addr, ind);
    exit(1);
  }
  if(ind >= get_size(bloc_addr)){
    fprintf(stderr,"ERROR: invalid index: get_field(0x%x, %d)\n",
            bloc_addr, ind);
    exit(1);
  }
#endif
  return memory[(bloc_addr >> 2) + ind];
}

void set_field(unsigned int bloc_addr, unsigned int ind, unsigned int value){
#ifdef DEBUG
  if(bloc_addr % 4 != 0 || bloc_addr == 0 || bloc_addr >= MEMORY_SIZE){
    fprintf(stderr,"ERROR: invalid address: set_field(0x%x, %d)\n",
            bloc_addr, ind);
    exit(1);
  }
  if(ind >= get_size(bloc_addr)){
    fprintf(stderr,
            "ERROR: invalid index (bloc size = %d): set_field(0x%x, %d)\n",
            get_size(bloc_addr), bloc_addr, ind);
    exit(1);
  }
#endif
  memory[(bloc_addr >> 2) + ind] = value;
}

unsigned int get_root(unsigned int ind){
#ifdef DEBUG
  if(ind >= ROOTS_NB){
    fprintf(stderr,"ERROR: invlid index: get_root(%d)\n",ind);
    exit(1);
  }
#endif
  return memory[ROOTS_ANCH / 4 + ind];
}

void set_root(unsigned int ind, unsigned int value){
#ifdef DEBUG
  if(ind >= ROOTS_NB){
    fprintf(stderr,"ERROR: invlid index: set_root(%d, %d)\n",ind,value);
    exit(1);
  }
#endif
  memory[ROOTS_ANCH / 4 + ind] = value;
}

int is_int(unsigned int value){
  return value & 0x1;
}

int int_value(unsigned int value){
#ifdef DEBUG
  if(!is_int(value)){
    fprintf(stderr,"ERROR: not an int value: int_value(%d)\n",value);
    exit(1);
  }
#endif
  return value >> 1;
}

unsigned int val_of_int(int i){
  return i << 1 | 1;
}
