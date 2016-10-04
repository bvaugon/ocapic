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

#ifndef GC
#define GC

unsigned short alloc(unsigned char size, unsigned char tag);

unsigned short get_field(unsigned short bloc, unsigned char ind);

void set_field(unsigned short bloc, unsigned char ind, unsigned short val);

unsigned short get_size(unsigned short bloc);

void set_root(unsigned short ind, unsigned short val);

void print(void);

#endif
