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
Disp.print_string "Hello world";;
