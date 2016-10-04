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

#include <caml/mlvalues.h>

/* Fonction interne au simulateur */
/* Ecriture dans un registre special du PIC */
value caml_pic_write_reg(value vreg, value vval);

/* Version C de la fonction mask_portb */
value mask_portb(value v1, value v2){
  caml_pic_write_reg(Val_long(0x01), Val_long(Long_val(v1) ^ Long_val (v2)));
  return Val_unit;
}
