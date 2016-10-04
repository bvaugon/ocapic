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
#include <caml/mlvalues.h>

value init_aff(){
  return Val_unit;
}

value print_int(value n){
  printf("0x%04X\n", (unsigned int) (Long_val(n) & 0x7FFF));
  return Val_unit;
}

value print_string(value s){
  printf("%s\n", String_val(s));
  return Val_unit;
}
