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

module Disp = Lcd.Connect (
  struct
    let bus_size = Lcd.Four
    let e  = LATD0
    let rs = LATD2
    let rw = LATD1
    let bus = PORTB
  end
);;

Disp.init ();;
Disp.config ();;

Disp.print_int Eeprom.size;;
Disp.print_char ' ';;
if Eeprom.size <> 0 then
  let n = Eeprom.read 0 in
  Disp.print_int n;
  try Eeprom.write 0 (succ n); Eeprom.refresh ();
  with
    | Failure s | Invalid_argument s -> Disp.print_string s;
    | _ -> Disp.print_string "Eexception...";
;;

