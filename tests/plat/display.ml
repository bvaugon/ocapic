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
    let e  = LATA0
    let rs = LATA1
    let rw = LATA3
    let bus = PORTB
  end
);;
open Disp;;

init ();;
config ();;

let write_string_at l c s =
  moveto l c;
  print_string s;
;;

let write_string s =
  clear ();
  home ();
  print_string s;
;;
