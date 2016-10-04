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
Disp.register_bitmap '\000'
  0b10001_01010 0b10001_00100_10001 0b01010_10001_00000;;
Disp.register_bitmap '\001'
  0b01000_00100 0b00100_00010_00110 0b01001_10001_00000;;
Disp.register_bitmap '\002'
  0b00100_01110 0b01110_01110_11111 0b00000_00100_00000;;
Disp.register_bitmap '\003'
  0b10001_01010 0b10001_00100_10001 0b01010_10001_00000;;
Disp.print_string "  Hello world\n    \000 \001 \002 \003";;
