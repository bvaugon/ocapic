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
    let bus = Pic.PORTB
  end
);;
open Disp;;

init ();;
config ();;

let rec receive () =
  if test_bit RCIF then (
    read_reg RCREG;
  ) else
    receive ()
;;

let transmit x =
  while not (test_bit TRMT) do () done;
  write_reg TXREG x;
;;

write_reg SPBRG 34;;
write_reg TXSTA 0b00100100;;
write_reg RCSTA 0b10010000;;

print_string "Hello !";;
moveto 2 0;;

while true do
  let i = receive () in
  let c = char_of_int i in
  print_char c;
  transmit (i+1);
done
