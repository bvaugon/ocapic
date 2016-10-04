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

set_bit IRCF1;;
set_bit IRCF0;;
set_bit PLLEN;;

module Disp = Lcd.Connect (
  struct
    let bus_size = Lcd.Eight
    let e  = Pic.LATD0
    let rs = Pic.LATD2
    let rw = Pic.LATD1
    let bus = Pic.PORTC
  end
);;
open Disp;;

init ();;
config ();;

Printf.fprintf output "%Lu\n" Int64.max_int;;
moveto 2 (-8);;
Printf.fprintf output "%Lu\n" Int64.max_int;;

Sys.sleep 4000;;

let check n =
  moveto 1 0;
  Printf.fprintf output "%-5d -> %u    " n n;
  Sys.sleep 1000;
;;

for i = min_int to min_int + 20 do check i done;;
for i = -20 to 20 do check i done;;
for i = max_int - 20 to max_int do check i done;;

Sys.sleep 4000;;

clear ();;
home ();;
Gc.run ();;
Printf.fprintf output "s=%d n=%d" (Gc.heap_occupation ())
  (Gc.running_number ());;
