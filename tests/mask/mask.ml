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

external mask_portb : int -> int -> unit = "mask_portb";;

write_reg TRISB 0x00;; (* PORTB en sortie *)
write_reg PORTB 0x00;; (* PORTB <- 0 *)

for i = 1 to 100 do
  mask_portb i 0xF0;   (* Execution du code externe *)
  Sys.sleep 1000;      (* Attend 1s *)
done
