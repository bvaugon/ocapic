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

#ifndef _MEM
#define _MEM

#define DEBUG
#define SEGMENT_SIZE 128
#define SEG1_ANCH 0
#define SEG2_ANCH SEGMENT_SIZE
#define ROOTS_NB 1
#define ROOTS_ANCH (SEGMENT_SIZE*2)
#define MEMORY_SIZE (ROOTS_ANCH+ROOTS_NB*4)

extern unsigned int memory[];
extern unsigned int current_seg_anch;
extern unsigned int other_seg_anch;
extern unsigned int current_pos;

unsigned char get_tag(unsigned int bloc_addr);

void set_tag(unsigned int bloc_addr, unsigned char tag);

unsigned int get_size(unsigned int bloc_addr);

void set_size(unsigned int bloc_addr, unsigned int size);

unsigned int get_field(unsigned int bloc_addr, unsigned int ind);

void set_field(unsigned int bloc_addr, unsigned int ind, unsigned int value);

int is_int(unsigned int value);

int int_value(unsigned int value);

unsigned int val_of_int(int i);

unsigned int get_root(unsigned int ind);

void set_root(unsigned int ind, unsigned int value);

#endif
