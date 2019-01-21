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
open Lcd;;
open Bytes;;

set_bit IRCF1;;
set_bit IRCF0;;
set_bit PLLEN;;

let disp = connect ~bus_size:Lcd.Four ~e:LATD0 ~rs:LATD2 ~rw:LATD1 ~bus:PORTB;;

let _ =
  disp.init ();
  disp.config ();

  let b = Bytes.create 11 in
  Bytes.set b 0 'H';
  Bytes.blit_string "ello warld" 0 b 1 10;
  Bytes.set b 7 (Bytes.get b 4);
  disp.print_string (Bytes.to_string b);

  while true do () done
