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

set_bit IRCF1;;
set_bit IRCF0;;
set_bit PLLEN;;

let disp = connect ~bus_size:Lcd.Four ~e:LATD0 ~rs:LATD2 ~rw:LATD1 ~bus:PORTB;;

disp.init ();;
disp.config ();;
disp.print_string "Hello world";;

let read () =
  write_reg ADCON2 0b10111110;
  write_reg ADCON1 0b00001110;
  write_reg ADCON0 0b00000011;
  while test_bit GO_NOT_DONE do () done;
  (read_reg ADRESH lsl 8) lor read_reg ADRES
;;

while true do
  disp.moveto 2 0;
  disp.print_string "        ";
  disp.moveto 2 0;
  disp.print_int (read ());
  Sys.sleep 100;
done
