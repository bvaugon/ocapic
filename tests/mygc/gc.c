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

#define MEM_SIZE 80
#define PART_SIZE 32
#define PART1_ANCH 0
#define PART2_ANCH 32
#define ROOTS_NB 8
#define ROOTS_ANCH 64
#define COPIED_TAG 245
#define CLOSURE_TAG 247
#define NO_PREV 0

unsigned short curr_part_anch = PART1_ANCH;
unsigned short other_part_anch = PART2_ANCH;
unsigned short next_allocation = PART1_ANCH + 2;
unsigned char memory[MEM_SIZE];

void print(void){
  int i;
  for(i = 0 ; i < MEM_SIZE ; i ++){
    printf("%02x", memory[i]);
    if(i % 16 == 15)
      printf("\n");
    else if(i % 2 == 1)
      printf(" ");
  }
  printf("\n");
  if(i % 16 != 0) printf("\n");
  //  sleep(1);
}

#define isint(val) ((val) & 1)
#define isptr(val) (!isint(val))

#define read_short(addr) (memory[addr]+(memory[(addr)+1]<<8))
#define write_short(addr,v) memory[addr]=(v)&255;memory[(addr)+1]=(v)>>8;

#define get_root(ind) (read_short(ROOTS_ANCH + (ind)*2))
#define get_tag(addr_bloc) (memory[(addr_bloc) - 1])
#define set_tag(addr_bloc,tag) memory[(addr_bloc) - 1] = tag;
#define get_size(addr_bloc) (memory[(addr_bloc) - 2])
#define set_size(addr_bloc,size) memory[(addr_bloc) - 2] = size;
#define get_field(addr_bloc,ind) (read_short((addr_bloc)+(ind)*2))
#define set_field(addr_bloc,ind,v) write_short((addr_bloc)+(ind)*2,v)

unsigned short unsafe_alloc(unsigned char bloc_size){
  unsigned short res = next_allocation;
  next_allocation += bloc_size * 2 + 2;
  return res;
}

void copy(unsigned short from, unsigned short to){
  unsigned char i;
  unsigned char size = get_size(from);
  set_size(to, size);
  set_tag(to, get_tag(from));
  for(i = 0 ; i < size ; i ++){
    set_field(to, i, (get_field(from, i)));
  }
}

unsigned short ind_root;
unsigned short prec_bloc;
unsigned short curr_bloc;
unsigned short copy_bloc;
unsigned short copy_ptr;
unsigned short tmp;

void gc(){
  tmp = other_part_anch;
  other_part_anch = curr_part_anch;
  curr_part_anch = tmp;
  next_allocation = curr_part_anch + 2;
  ind_root = ROOTS_NB;

 loop_roots:
  ind_root --;
  if(ind_root == 0xffff) return;
  if(isptr(get_root(ind_root))){
    curr_bloc = get_root(ind_root);
    prec_bloc = NO_PREV;
    copy_ptr = ROOTS_ANCH + ind_root * 2;
    goto traitement_bloc;
  }else{
    goto loop_roots;
  }

 traitement_bloc:
  if(get_tag(curr_bloc) == COPIED_TAG){
    write_short(copy_ptr, get_field(curr_bloc, 0));
    goto go_back;
  }else{
    copy_bloc = unsafe_alloc(get_size(curr_bloc));
    copy(curr_bloc, copy_bloc);
    write_short(copy_ptr, copy_bloc);
    set_tag(curr_bloc, COPIED_TAG);
    set_field(curr_bloc, 0, copy_bloc);
    if(get_size(curr_bloc) == 1){
      if(isptr(get_field(copy_bloc, 0)) && get_tag(copy_bloc) != CLOSURE_TAG){
        copy_ptr = copy_bloc;
        curr_bloc = get_field(copy_bloc, 0);
        goto traitement_bloc;
      }else{
        goto go_back;
      }
    }else{
      set_field(curr_bloc, 1, prec_bloc);
      prec_bloc = curr_bloc;
      goto begin_loop_bloc;
    }
  }

 go_back:
  if(prec_bloc == NO_PREV){
    goto loop_roots;
  }else{
    curr_bloc = prec_bloc;
    copy_bloc = get_field(curr_bloc, 0);
  }

 loop_bloc:
  if(get_size(curr_bloc) == 0 ||
     (get_size(curr_bloc) == 1 && get_tag(curr_bloc) == CLOSURE_TAG)){
    prec_bloc = get_field(curr_bloc, 1);
    goto go_back;
  }

 begin_loop_bloc:
  tmp = get_size(curr_bloc) - 1;
  set_size(curr_bloc, tmp);
  if(isptr(get_field(copy_bloc, tmp))){
    copy_ptr = copy_bloc + tmp*2;
    curr_bloc = get_field(copy_bloc, tmp);
    goto traitement_bloc;
  }else{
    goto loop_bloc;
  }
}

unsigned short alloc(unsigned char size, unsigned char tag){
  if(next_allocation + size * 2 > curr_part_anch + PART_SIZE){
    gc();
    if(next_allocation + size * 2 > curr_part_anch + PART_SIZE){
      printf("Plus de mémoire\n");
      exit(1);
    }
  }
  unsigned short res = next_allocation;
  next_allocation += size * 2 + 2;
  set_size(res, size);
  set_tag(res, tag);
  return res;
}

unsigned short (get_field)(unsigned short bloc, unsigned char ind){
  return get_field(bloc,ind);
}

void (set_field)(unsigned short bloc, unsigned char ind, unsigned short val){
  set_field(bloc,ind,val);
}

unsigned short (get_size)(unsigned short bloc){
  return get_size(bloc);
}

void set_root(unsigned short ind, unsigned short val){
  write_short(ROOTS_ANCH+ind*2,val);
}
