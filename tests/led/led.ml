(*************************************************************************)
(*                                                                       *)
(*                                OCaPIC                                 *)
(*                                                                       *)
(*                             Benoit Vaugon                             *)
(*                                                                       *)
(*    This file is distributed under the terms of the CeCILL license.    *)
(*    See file ../../LICENSE-en.                                         *)
(*                                                                       *)
(*************************************************************************)

open Pic;;

write_reg TRISB 0x00;

while true do
  set_bit RB0;
  Sys.sleep 500;
  clear_bit RB0;
  Sys.sleep 500;
done
